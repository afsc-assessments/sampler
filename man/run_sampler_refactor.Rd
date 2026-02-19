\name{run_sampler_refactor}
\alias{run_sampler_refactor}
\title{Run the sampler refactor}
\usage{
run_sampler_refactor(control_file, bs_file = "bs_setup.dat", out_dir = "results", out_prefix = "sampler")
}
\arguments{
\item{control_file}{Path to sam*.dat control file}
\item{bs_file}{Path to bs_setup.dat}
\item{out_dir}{Output directory for parquet and log files}
\item{out_prefix}{Output filename prefix}
}
\value{
List with point, bootstrap, and log path.
}
\description{
Run the sampler refactor and write parquet outputs and a timing log.
}
