#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

#' Resolve the script directory
#'
#' Finds the current script directory using `--file` when available, falling
#' back to frame metadata and then `getwd()`.
#'
#' @return Absolute path to the script directory as a character scalar.
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

#' Parse common truthy/falsey strings to logical
#'
#' @param x Character-like scalar from CLI args.
#' @param default Logical value returned when `x` is missing/empty.
#'
#' @return Logical scalar.
as_bool <- function(x, default = FALSE) {
  if (is.null(x) || is.na(x) || !nzchar(x)) {
    return(default)
  }
  tolower(x) %in% c("1", "true", "t", "yes", "y")
}

#' Parse key-value CLI arguments
#'
#' @param args Character vector like `c("key=value", "other=1")`.
#'
#' @return Named list of parsed key-value strings.
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

#' Discover available sampler control years
#'
#' @param fishery_dir Directory containing `samYYYY.dat` files.
#'
#' @return Integer vector of discovered years in ascending order.
discover_control_years <- function(fishery_dir) {
  files <- list.files(fishery_dir, pattern = "^sam[0-9]{4}\\.dat$", full.names = FALSE)
  years <- as.integer(sub("^sam([0-9]{4})\\.dat$", "\\1", files))
  sort(years[!is.na(years)])
}

#' Write `bs_setup.dat` bootstrap controls
#'
#' @param case_dir Case directory where `bs_setup.dat` will be written.
#' @param nbs Number of bootstrap replicates.
#' @param levels Numeric vector of four sampling-level controls.
#'
#' @return Path to the written `bs_setup.dat` file.
write_bs_setup <- function(case_dir, nbs, levels) {
  stopifnot(length(levels) == 4)
  path <- file.path(case_dir, "bs_setup.dat")
  lines <- c(as.integer(nbs), as.numeric(levels))
  writeLines(as.character(lines), con = path)
  path
}

#' Run ADMB sampler for one control year
#'
#' @param year Integer year matching a `samYYYY.dat` file.
#' @param case_dir Case directory used as working directory for ADMB.
#' @param fishery_dir Directory containing `samYYYY.dat` control files.
#' @param sam_bin Path to ADMB `sam` executable.
#' @param io Logical; if `TRUE`, pass `-nox -io`.
#' @param verbose Logical; emit per-year status messages.
#'
#' @return Invisibly returns `TRUE` on success.
run_sampler_year <- function(year, case_dir, fishery_dir, sam_bin, io = FALSE, verbose = TRUE) {
  ctl <- sprintf("sam%d.dat", year)
  ctl_path <- file.path(fishery_dir, ctl)
  if (!file.exists(ctl_path)) {
    stop(sprintf("Control file not found: %s", ctl_path))
  }
  est_path <- file.path(case_dir, "results", sprintf("Est_%d.dat", year))

  data_dir <- normalizePath(file.path(case_dir, "data"), mustWork = FALSE)
  ctl_arg <- if (normalizePath(fishery_dir) == data_dir) {
    file.path("data", ctl)
  } else {
    ctl_path
  }

  args <- c(if (io) c("-nox", "-io"), "-ind", ctl_arg)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(case_dir)

  out <- suppressWarnings(system2(sam_bin, args = args, stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (is.null(status)) {
    status <- 0
  }

  if (verbose) {
    message(sprintf("Year %d: sam status %d", year, status))
  }

  if (status != 0) {
    wrote_est <- file.exists(est_path)
    if (wrote_est) {
      if (verbose) {
        message(sprintf("Year %d: accepting non-zero sam status because %s was written", year, est_path))
      }
      return(invisible(TRUE))
    }

    msg <- paste(out, collapse = "\n")
    stop(sprintf("sam failed for year %d\n%s", year, msg))
  }

  invisible(TRUE)
}

#' Read one `Est_<year>.dat` output file
#'
#' @param year Integer year to read.
#' @param case_dir Case directory containing `results/`.
#'
#' @return `data.table` with estimate rows, or `NULL` if file is missing.
read_estimates <- function(year, case_dir) {
  path <- file.path(case_dir, "results", sprintf("Est_%d.dat", year))
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

#' Summarize annual combined estimates
#'
#' Builds yearly totals and mean age from `type == "N"` at combined
#' `stratum == 99`, including sex-specific totals.
#'
#' @param est_dt `data.table` of stacked `Est_<year>.dat` rows.
#'
#' @return `data.table` with one summary row per year.
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

#' Run consolidated ebswpSAM fishery workflow
#'
#' Executes ADMB runs over a year range (optional), then reads
#' `results/Est_<year>.dat` files and writes a rolled-up summary CSV.
#'
#' @param start_year Optional integer start year; defaults to minimum control year.
#' @param end_year Optional integer end year; defaults to maximum control year.
#' @param run_sampler Logical; if `TRUE`, run ADMB before summarizing.
#' @param nbs Integer bootstrap count for `bs_setup.dat`.
#' @param levels Numeric length-4 vector for sampling controls in `bs_setup.dat`.
#' @param io Logical; if `TRUE`, pass `-nox -io` to ADMB `sam`.
#' @param fishery_dir Optional directory containing `samYYYY.dat`; defaults to `data` next to this script.
#' @param sam_bin Optional path to ADMB `sam`; defaults to `../../src/sam` from this script.
#' @param summary_csv Optional output path for summary CSV.
#' @param verbose Logical; if `TRUE`, print progress messages.
#'
#' @return Invisibly returns a list with `years`, `summary_csv`, and `n_rows`.
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
  case_dir <- get_script_dir()

  if (is.null(fishery_dir)) {
    fishery_dir <- file.path(case_dir, "data")
  }
  fishery_dir <- normalizePath(fishery_dir)

  if (is.null(sam_bin)) {
    sam_bin <- normalizePath(file.path(case_dir, "..", "..", "src", "sam"), mustWork = FALSE)
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
    summary_csv <- file.path(case_dir, "results", "ebswp_est_summary.csv")
  }

  if (run_sampler) {
    if (!file.exists(sam_bin)) {
      stop(sprintf("sam binary not found: %s", sam_bin))
    }

    dir.create(file.path(case_dir, "results"), recursive = TRUE, showWarnings = FALSE)
    write_bs_setup(case_dir, nbs = as.integer(nbs), levels = as.numeric(levels))

    for (yr in years) {
      run_sampler_year(
        yr,
        case_dir = case_dir,
        fishery_dir = fishery_dir,
        sam_bin = sam_bin,
        io = io,
        verbose = verbose
      )
    }
  }

  est_list <- lapply(years, read_estimates, case_dir = case_dir)
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
