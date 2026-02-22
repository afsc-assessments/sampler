#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))))
  }
  normalizePath(getwd())
}

parse_cli_args <- function(args) {
  out <- list()
  if (length(args) == 0) {
    return(out)
  }

  for (arg in args) {
    kv <- strsplit(arg, "=", fixed = TRUE)[[1]]
    if (length(kv) != 2) {
      next
    }
    out[[kv[[1]]]] <- kv[[2]]
  }

  out
}

read_estimates <- function(results_dir) {
  files <- list.files(results_dir, pattern = "^Est_[0-9]{4}\\.dat$", full.names = TRUE)
  if (length(files) == 0) {
    stop(sprintf("No Est_YYYY.dat files found in %s", results_dir))
  }

  dt <- rbindlist(lapply(files, fread), use.names = TRUE, fill = TRUE)
  req <- c("year", "type", "stratum", "sex", "age", "value")
  if (!all(req %in% names(dt))) {
    stop("Estimate files are missing one or more required columns: year,type,stratum,sex,age,value")
  }

  dt[, year := as.integer(year)]
  dt[, stratum := as.integer(stratum)]
  dt[, sex := as.integer(sex)]
  dt[, age := as.integer(age)]
  dt[, value := as.numeric(value)]
  dt[]
}

plot_total_numbers <- function(n_total, out_dir, width, height, dpi) {
  p <- ggplot(n_total, aes(x = year, y = total_n))
  if (length(unique(n_total$year)) > 1) {
    p <- p + geom_line(linewidth = 0.8, color = "#1f77b4")
  }
  p <- p +
    geom_point(size = 2, color = "#1f77b4") +
    labs(
      title = "Combined Numbers-at-Age Total",
      x = "Year",
      y = "Total numbers (stratum=99, sex=99)"
    ) +
    theme_bw()

  ggsave(file.path(out_dir, "total_numbers_by_year.png"), p, width = width, height = height, dpi = dpi)
}

plot_mean_age <- function(mean_age, out_dir, width, height, dpi) {
  p <- ggplot(mean_age, aes(x = year, y = mean_age))
  if (length(unique(mean_age$year)) > 1) {
    p <- p + geom_line(linewidth = 0.8, color = "#d62728")
  }
  p <- p +
    geom_point(size = 2, color = "#d62728") +
    labs(
      title = "Combined Mean Age",
      x = "Year",
      y = "Mean age (weighted by N)"
    ) +
    theme_bw()

  ggsave(file.path(out_dir, "mean_age_by_year.png"), p, width = width, height = height, dpi = dpi)
}

plot_age_heatmap <- function(age_prop, out_dir, width, height, dpi) {
  p <- ggplot(age_prop, aes(x = year, y = age, fill = p)) +
    geom_tile() +
    scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
    labs(
      title = "Combined Age Composition",
      x = "Year",
      y = "Age",
      fill = "Proportion"
    ) +
    theme_bw()

  ggsave(file.path(out_dir, "age_composition_heatmap.png"), p, width = width, height = height, dpi = dpi)
}

plot_sex_totals <- function(sex_totals, out_dir, width, height, dpi) {
  sex_totals[, sex_label := fifelse(sex == 1L, "Male", "Female")]
  p <- ggplot(sex_totals, aes(x = year, y = total_n, color = sex_label))
  if (length(unique(sex_totals$year)) > 1) {
    p <- p + geom_line(linewidth = 0.8)
  }
  p <- p +
    geom_point(size = 2) +
    labs(
      title = "Combined Numbers by Sex",
      x = "Year",
      y = "Total numbers (stratum=99)",
      color = "Sex"
    ) +
    theme_bw()

  ggsave(file.path(out_dir, "sex_totals_by_year.png"), p, width = width, height = height, dpi = dpi)
}

plot_weight_at_age <- function(wt_age, out_dir, width, height, dpi) {
  p <- ggplot(wt_age, aes(x = age, y = avg_wt, group = year, color = as.factor(year))) +
    geom_line(linewidth = 0.7) +
    labs(
      title = "Average Weight-at-Age (Combined)",
      x = "Age",
      y = "Average weight",
      color = "Year"
    ) +
    theme_bw()

  ggsave(file.path(out_dir, "weight_at_age_by_year.png"), p, width = width, height = height, dpi = dpi)
}

run_plot_results <- function(
  results_dir = NULL,
  out_dir = NULL,
  start_year = NULL,
  end_year = NULL,
  width = 10,
  height = 6,
  dpi = 150
) {
  script_dir <- get_script_dir()
  if (is.null(results_dir)) {
    results_dir <- file.path(script_dir, "results")
  }
  if (is.null(out_dir)) {
    out_dir <- file.path(results_dir, "plots")
  }

  results_dir <- normalizePath(results_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  est <- read_estimates(results_dir)
  if (!is.null(start_year)) {
    est <- est[year >= as.integer(start_year)]
  }
  if (!is.null(end_year)) {
    est <- est[year <= as.integer(end_year)]
  }
  if (nrow(est) == 0) {
    stop("No rows remain after year filtering")
  }

  n_combined <- est[type == "N" & stratum == 99L & sex == 99L]
  if (nrow(n_combined) == 0) {
    stop("No combined N rows found (type='N', stratum=99, sex=99)")
  }

  n_total <- n_combined[, .(total_n = sum(value, na.rm = TRUE)), by = year][order(year)]
  mean_age <- n_combined[
    ,
    .(
      mean_age = ifelse(sum(value, na.rm = TRUE) > 0,
        sum(age * value, na.rm = TRUE) / sum(value, na.rm = TRUE),
        NA_real_
      )
    ),
    by = year
  ][order(year)]
  age_prop <- n_combined[, .(n = sum(value, na.rm = TRUE)), by = .(year, age)]
  age_prop[, p := ifelse(sum(n, na.rm = TRUE) > 0, n / sum(n, na.rm = TRUE), NA_real_), by = year]
  age_prop <- age_prop[order(year, age)]

  sex_totals <- est[type == "N" & stratum == 99L & sex %in% c(1L, 2L),
    .(total_n = sum(value, na.rm = TRUE)),
    by = .(year, sex)
  ][order(year, sex)]

  wt_age <- est[type == "avg_wt_age" & stratum == 99L & sex == 99L,
    .(avg_wt = mean(value, na.rm = TRUE)),
    by = .(year, age)
  ][order(year, age)]

  plot_total_numbers(n_total, out_dir, width, height, dpi)
  plot_mean_age(mean_age, out_dir, width, height, dpi)
  plot_age_heatmap(age_prop, out_dir, width, height, dpi)
  if (nrow(sex_totals) > 0) {
    plot_sex_totals(sex_totals, out_dir, width, height, dpi)
  }
  if (nrow(wt_age) > 0) {
    plot_weight_at_age(wt_age, out_dir, width, height, dpi)
  }

  summary_dt <- merge(n_total, mean_age, by = "year", all = TRUE)
  if (nrow(sex_totals) > 0) {
    male <- sex_totals[sex == 1L, .(year, male_n = total_n)]
    female <- sex_totals[sex == 2L, .(year, female_n = total_n)]
    summary_dt <- merge(summary_dt, male, by = "year", all = TRUE)
    summary_dt <- merge(summary_dt, female, by = "year", all = TRUE)
  }
  fwrite(summary_dt[order(year)], file.path(out_dir, "plot_summary.csv"))

  message(sprintf("Wrote plots to %s", out_dir))
  invisible(list(
    results_dir = results_dir,
    out_dir = out_dir,
    years = sort(unique(est$year)),
    n_rows = nrow(est)
  ))
}

if (sys.nframe() == 0) {
  args <- parse_cli_args(commandArgs(trailingOnly = TRUE))

  run_plot_results(
    results_dir = args$results_dir,
    out_dir = args$out_dir,
    start_year = if (!is.null(args$start_year)) as.integer(args$start_year) else NULL,
    end_year = if (!is.null(args$end_year)) as.integer(args$end_year) else NULL,
    width = if (!is.null(args$width)) as.numeric(args$width) else 10,
    height = if (!is.null(args$height)) as.numeric(args$height) else 6,
    dpi = if (!is.null(args$dpi)) as.integer(args$dpi) else 150
  )
}
