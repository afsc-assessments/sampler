# High-performance refactor of sam.tpl into R (data.table)
# Input formats preserved: sam*.dat, age*.dat, len*.dat, bs_setup.dat
# Outputs: tidy parquet tables (point estimates + bootstrap separately)

#' Run the sampler refactor
#'
#' @param control_file Path to sam*.dat control file
#' @param bs_file Path to bs_setup.dat
#' @param out_dir Output directory for parquet and log files
#' @param out_prefix Output filename prefix
#' @return list with point, bootstrap, and log path
#' @export
#'

suppressPackageStartupMessages({
  library(data.table)
  library(utils)
})

globalVariables(c(".", ".N", "N", ":=", "strata", "sex", "len", "age", "freq", "prop",
                  "p", "catch_len", "catch_n", "catch_at_age", "wt_at_age", "wt",
                  "bootstrap", "haul"))

if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("Package 'arrow' is required for Parquet output.")
}

read_control <- function(path) {
  x <- scan(path, what = "character", quiet = TRUE)
  nstrata <- as.integer(x[10])
  list(
    year = as.integer(x[1]),
    agefile = x[2],
    lenfile = x[3],
    na = as.integer(x[4]),
    nl = as.integer(x[5]),
    a1 = as.integer(x[6]),
    a2 = as.integer(x[7]),
    l1 = as.integer(x[8]),
    l2 = as.integer(x[9]),
    nstrata = nstrata,
    strata_catch = as.numeric(x[11:(10 + nstrata)]),
    outfile = x[11 + nstrata]
  )
}

read_bs_setup <- function(path = "bs_setup.dat") {
  x <- as.numeric(scan(path, what = "character", quiet = TRUE))
  list(
    nbs = as.integer(x[1]),
    sam_level_age_tows = x[2],
    sam_level_ages = x[3],
    sam_level_lf_tows = x[4],
    sam_level_lfreqs = x[5]
  )
}

read_age <- function(path) {
  dt <- data.table::fread(path, col.names = c("strata","haul","sex","age","wt","len"))
  dt[, haul := as.integer(haul)]
  dt
}

read_len <- function(path) {
  dt <- data.table::fread(path, col.names = c("strata","haul","sex","len","freq"))
  dt[, haul := as.integer(haul)]
  dt
}

compute_lf <- function(len_dt) {
  lf <- len_dt[, .(freq = sum(freq)), by = .(strata, sex, len)]
  lf[, prop := freq / sum(freq), by = .(strata, sex)]
  lf
}

compute_wl <- function(age_dt) {
  age_dt[, .(wt = mean(wt, na.rm = TRUE)), by = .(strata, sex, len)]
}

compute_alk <- function(age_dt) {
  alk <- age_dt[, .N, by = .(strata, sex, len, age)]
  alk[, p := N / sum(N), by = .(strata, sex, len)]
  alk[, .(strata, sex, len, age, p)]
}

compute_caa <- function(lf_dt, alk_dt, strata_catch) {
  sc <- data.table::data.table(strata = seq_along(strata_catch), strata_catch = strata_catch)
  lf2 <- merge(lf_dt, sc, by = "strata", all.x = TRUE)
  lf2[, catch_len := prop * strata_catch]
  dt <- merge(lf2[, .(strata, sex, len, catch_len)], alk_dt, by = c("strata","sex","len"), allow.cartesian = TRUE)
  dt[, catch_n := catch_len * p]
  dt[, .(catch_at_age = sum(catch_n)), by = .(strata, sex, age)]
}

compute_wta <- function(age_dt) {
  age_dt[, .(wt_at_age = mean(wt, na.rm = TRUE)), by = .(strata, sex, age)]
}

as_tidy <- function(year, lf, alk, caa, wta, wl) {
  out <- data.table::rbindlist(list(
    lf[, .(year, strata, sex, age = NA_integer_, len, metric = "lf_freq", value = freq)],
    lf[, .(year, strata, sex, age = NA_integer_, len, metric = "lf_prop", value = prop)],
    alk[, .(year, strata, sex, age, len, metric = "alk_p", value = p)],
    caa[, .(year, strata, sex, age, len = NA_integer_, metric = "catch_at_age", value = catch_at_age)],
    wta[, .(year, strata, sex, age, len = NA_integer_, metric = "wt_at_age", value = wt_at_age)],
    wl[, .(year, strata, sex, age = NA_integer_, len, metric = "wt_at_len", value = wt)]
  ), use.names = TRUE, fill = TRUE)
  out[]
}

resample_by_haul <- function(dt, level_tows = 1, level_rows = 1) {
  if (nrow(dt) == 0) return(dt)
  out <- dt[, {
    h <- unique(haul)
    n_h <- length(h)
    n_take <- max(1L, as.integer(floor(n_h / level_tows)))
    h_s <- sample(h, size = n_take, replace = TRUE)
    sub <- dt[haul %in% h_s]
    n_r <- nrow(sub)
    n_take_r <- max(1L, as.integer(floor(n_r / level_rows)))
    sub[sample.int(n_r, n_take_r, replace = TRUE)]
  }, by = .(strata)]
  out[, haul := as.integer(haul)]
  out
}

bootstrap_sampler <- function(age_dt, len_dt, control, bs, seed = 123) {
  set.seed(seed)
  nbs <- bs$nbs
  if (is.na(nbs) || nbs < 1) return(NULL)

  res <- vector("list", nbs)
  for (b in seq_len(nbs)) {
    age_b <- resample_by_haul(age_dt, bs$sam_level_age_tows, bs$sam_level_ages)
    len_b <- resample_by_haul(len_dt, bs$sam_level_lf_tows, bs$sam_level_lfreqs)

    lf_b <- compute_lf(len_b)
    alk_b <- compute_alk(age_b)
    caa_b <- compute_caa(lf_b, alk_b, control$strata_catch)
    wta_b <- compute_wta(age_b)
    wl_b <- compute_wl(age_b)

    tidy_b <- as_tidy(control$year, lf_b, alk_b, caa_b, wta_b, wl_b)
    tidy_b[, bootstrap := b]
    res[[b]] <- tidy_b
  }
  data.table::rbindlist(res, use.names = TRUE, fill = TRUE)
}

run_sampler_refactor <- function(control_file,
                                 bs_file = "bs_setup.dat",
                                 out_dir = "results",
                                 out_prefix = "sampler") {
  t0 <- proc.time()
  control <- read_control(control_file)
  bs <- read_bs_setup(bs_file)

  age_dt <- read_age(control$agefile)
  len_dt <- read_len(control$lenfile)

  t1 <- proc.time()
  lf <- compute_lf(len_dt)
  alk <- compute_alk(age_dt)
  caa <- compute_caa(lf, alk, control$strata_catch)
  wta <- compute_wta(age_dt)
  wl <- compute_wl(age_dt)
  t2 <- proc.time()

  tidy <- as_tidy(control$year, lf, alk, caa, wta, wl)

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  arrow::write_parquet(tidy, file.path(out_dir, paste0(out_prefix, "_tidy.parquet")))

  boot <- bootstrap_sampler(age_dt, len_dt, control, bs)
  if (!is.null(boot)) {
    arrow::write_parquet(boot, file.path(out_dir, paste0(out_prefix, "_bootstrap_", control$year, ".parquet")))
  }
  t3 <- proc.time()

  log_path <- file.path(out_dir, paste0(out_prefix, "_refactor.log"))
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  cat(
    sprintf("\n--- %s ---\n", ts),
    sprintf("control=%s\n", control_file),
    sprintf("bs=%s\n", bs_file),
    sprintf("year=%s\n", control$year),
    sprintf("read_sec=%.3f\n", (t1 - t0)["elapsed"]),
    sprintf("compute_sec=%.3f\n", (t2 - t1)["elapsed"]),
    sprintf("write_sec=%.3f\n", (t3 - t2)["elapsed"]),
    sprintf("total_sec=%.3f\n", (t3 - t0)["elapsed"]),
    "sessionInfo:\n",
    paste(capture.output(sessionInfo()), collapse = "\n"),
    "\n",
    file = log_path,
    append = TRUE
  )

  list(point = tidy, bootstrap = boot, log = log_path)
}
