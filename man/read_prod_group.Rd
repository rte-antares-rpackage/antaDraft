% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_prod_group.R
\name{read_prod_group}
\alias{read_prod_group}
\title{production data per groups of production}
\usage{
read_prod_group(production_dir = NULL, production_file = NULL)
}
\arguments{
\item{production_dir}{datasets directory of data energy productions
by groups.}

\item{production_file}{production file to be used}
}
\description{
import csv data representing production per groups The data
is read from an entsoe repository.
}
\examples{
production_dir <- system.file(package = "antaDraft", "data_sample",
  "prod_sample_20160129/B02")
global_options <- getOption("global_options")
prod_by_types <- read_prod_group(production_dir = production_dir,
  production_file = global_options$thermal_production_per_country)
}
