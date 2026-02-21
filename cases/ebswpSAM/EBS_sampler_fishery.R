#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))))
  }

  this <- tryCatch(normalizePath(sys.frames()[[1]]$ofile), error = function(e) NA_character_)
  if (!is.na(this) && nzchar(this)) {
    return(dirname(this))
  }

  normalizePath(getwd())
}

as_bool <- function(x, default = FALSE) {
  if (is.null(x) || is.na(x) || !nzchar(x)) {
    return(default)
  }
  tolower(x) %in% c("1", "true", "t", "yes", "y")
}

parse_cli_args <- function(args) {
  out <- list()
  if (length(args) == 0) {
    return(out)
  }

  for (arg in args) {
    parts <- strsplit(arg, "=", fixed = TRUE)[[1]]
    if (length(parts) != 2) {
      next
    }
    key <- parts[[1]]
    val <- parts[[2]]
    out[[key]] <- val
  }

  out
}

discover_control_years <- function(fishery_dir) {
  files <- list.files(fishery_dir, pattern = "^sam[0-9]{4}\\.dat$", full.names = FALSE)
  years <- as.integer(sub("^sam([0-9]{4})\\.dat$", "\\1", files))
  sort(years[!is.na(years)])
}

write_bs_setup <- function(fishery_dir, nbs, levels) {
  stopifnot(length(levels) == 4)
  path <- file.path(fishery_dir, "bs_setup.dat")
  lines <- c(as.integer(nbs), as.numeric(levels))
  writeLines(as.character(lines), con = path)
  path
}

run_sampler_year <- function(year, fishery_dir, sam_bin, io = FALSE, verbose = TRUE) {
  ctl <- sprintf("sam%d.dat", year)
  ctl_path <- file.path(fishery_dir, ctl)
  if (!file.exists(ctl_path)) {
    stop(sprintf("Control file not found: %s", ctl_path))
  }

  args <- c(if (io) c("-nox", "-io"), "-ind", ctl)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(fishery_dir)

  out <- system2(sam_bin, args = args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (is.null(status)) {
    status <- 0
  }

  if (verbose) {
    message(sprintf("Year %d: sam status %d", year, status))
  }

  if (status != 0) {
    msg <- paste(out, collapse = "\n")
    stop(sprintf("sam failed for year %d\n%s", year, msg))
  }

  invisible(TRUE)
}

read_estimates <- function(year, fishery_dir) {
  path <- file.path(fishery_dir, "results", sprintf("Est_%d.dat", year))
  if (!file.exists(path)) {
    return(NULL)
  }

  dt <- data.table::fread(path)
  req <- c("year", "type", "stratum", "sex", "age", "value")
  if (!all(req %in% names(dt))) {
    stop(sprintf("Estimate file missing expected columns: %s", path))
  }

  dt[, year := as.integer(year)]
  dt[]
}

summarize_estimates <- function(est_dt) {
  if (is.null(est_dt) || nrow(est_dt) == 0) {
    return(data.table::data.table())
  }

  total <- est_dt[type == "N" & stratum == 99 & sex == 99,
    .(
      total_n = sum(value, na.rm = TRUE),
      mean_age = ifelse(sum(value, na.rm = TRUE) > 0,
        sum(as.numeric(age) * value, na.rm = TRUE) / sum(value, na.rm = TRUE),
        NA_real_
      )
    ),
    by = .(year)
  ]

  male <- est_dt[type == "N" & stratum == 99 & sex == 1,
    .(male_n = sum(value, na.rm = TRUE)),
    by = .(year)
  ]

  female <- est_dt[type == "N" & stratum == 99 & sex == 2,
    .(female_n = sum(value, na.rm = TRUE)),
    by = .(year)
  ]

  out <- merge(total, male, by = "year", all = TRUE)
  out <- merge(out, female, by = "year", all = TRUE)
  data.table::setorder(out, year)
  out[]
}

run_ebswp_fishery <- function(
  start_year = NULL,
  end_year = NULL,
  run_sampler = TRUE,
  nbs = 1L,
  levels = c(1, 1, 1, 1),
  io = FALSE,
  fishery_dir = NULL,
  sam_bin = NULL,
  summary_csv = NULL,
  verbose = TRUE
) {
  script_dir <- get_script_dir()

  if (is.null(fishery_dir)) {
    fishery_dir <- file.path(script_dir, "data", "fishery")
  }
  fishery_dir <- normalizePath(fishery_dir)

  if (is.null(sam_bin)) {
    sam_bin <- normalizePath(file.path(script_dir, "..", "..", "src", "sam"), mustWork = FALSE)
  }

  all_years <- discover_control_years(fishery_dir)
  if (length(all_years) == 0) {
    stop(sprintf("No control files found in %s", fishery_dir))
  }

  if (is.null(start_year)) {
    start_year <- min(all_years)
  }
  if (is.null(end_year)) {
    end_year <- max(all_years)
  }

  years <- all_years[all_years >= as.integer(start_year) & all_years <= as.integer(end_year)]
  if (length(years) == 0) {
    stop("No matching years after applying start_year/end_year")
  }

  if (is.null(summary_csv)) {
    summary_csv <- file.path(fishery_dir, "results", "ebswp_est_summary.csv")
  }

  if (run_sampler) {
    if (!file.exists(sam_bin)) {
      stop(sprintf("sam binary not found: %s", sam_bin))
    }

    dir.create(file.path(fishery_dir, "results"), recursive = TRUE, showWarnings = FALSE)
    write_bs_setup(fishery_dir, nbs = as.integer(nbs), levels = as.numeric(levels))

    for (yr in years) {
      run_sampler_year(yr, fishery_dir = fishery_dir, sam_bin = sam_bin, io = io, verbose = verbose)
    }
  }

  est_list <- lapply(years, read_estimates, fishery_dir = fishery_dir)
  est_list <- Filter(Negate(is.null), est_list)

  if (length(est_list) == 0) {
    warning("No Est_<year>.dat files were found; summary CSV was not written.")
    return(invisible(list(years = years, summary_csv = NA_character_)))
  }

  est <- data.table::rbindlist(est_list, use.names = TRUE, fill = TRUE)
  summary_dt <- summarize_estimates(est)

  dir.create(dirname(summary_csv), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(summary_dt, summary_csv)

  if (verbose) {
    message(sprintf("Wrote summary: %s", summary_csv))
  }

  invisible(list(
    years = years,
    summary_csv = summary_csv,
    n_rows = nrow(summary_dt)
  ))
}

if (sys.nframe() == 0) {
  args <- parse_cli_args(commandArgs(trailingOnly = TRUE))

  levels <- c(1, 1, 1, 1)
  if (!is.null(args$levels)) {
    levels <- as.numeric(strsplit(args$levels, ",", fixed = TRUE)[[1]])
    if (length(levels) != 4) {
      stop("levels must be a comma-separated list of four values")
    }
  }

  run_ebswp_fishery(
    start_year = if (!is.null(args$start_year)) as.integer(args$start_year) else NULL,
    end_year = if (!is.null(args$end_year)) as.integer(args$end_year) else NULL,
    run_sampler = as_bool(args$run_sampler, default = TRUE),
    nbs = if (!is.null(args$nbs)) as.integer(args$nbs) else 1L,
    levels = levels,
    io = as_bool(args$io, default = FALSE),
    fishery_dir = args$fishery_dir,
    sam_bin = args$sam_bin,
    summary_csv = args$summary_csv,
    verbose = as_bool(args$verbose, default = TRUE)
  )
}
