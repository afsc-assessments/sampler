#!/usr/bin/env Rscript

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))))
  }
  normalizePath(getwd())
}

script_dir <- get_script_dir()
source(file.path(script_dir, "EBS_sampler_fishery.R"))

args <- parse_cli_args(commandArgs(trailingOnly = TRUE))

levels <- c(1, 1, 1, 1)
if (!is.null(args$levels)) {
  levels <- as.numeric(strsplit(args$levels, ",", fixed = TRUE)[[1]])
}

run_ebswp_fishery(
  start_year = if (!is.null(args$start_year)) as.integer(args$start_year) else 1991L,
  end_year = if (!is.null(args$end_year)) as.integer(args$end_year) else 2022L,
  run_sampler = as_bool(args$run_sampler, default = TRUE),
  nbs = if (!is.null(args$nbs)) as.integer(args$nbs) else 1L,
  levels = levels,
  io = as_bool(args$io, default = FALSE),
  fishery_dir = args$fishery_dir,
  control_pattern = if (!is.null(args$control_pattern)) args$control_pattern else "^sam[0-9]{4}\\.dat$",
  sam_bin = args$sam_bin,
  summary_csv = args$summary_csv,
  verbose = as_bool(args$verbose, default = TRUE)
)
