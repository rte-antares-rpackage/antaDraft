% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/augment_shifted.R
\name{augment_shifted}
\alias{augment_shifted}
\title{add a shifted value}
\usage{
augment_shifted(x, col, hour_shift = -1, summary_colname = "summary")
}
\arguments{
\item{x}{dataset}

\item{col}{column name of the measure to be aggregated and lagged}

\item{hour_shift}{lag as a number of hours}

\item{summary_colname}{column name containing results from \code{\link{augment_process_summary}}}
}
\description{
add a shifted value to an existing dataset.
}
\examples{
load_dir <- system.file(package = "antaDraft",
  "data_sample/load_sample_2017")

load_data <- anta_load(data_dir = load_dir )
load_data <- augment_validation(data = load_data)
head(load_data)

aggregated_db <- agg_data(load_data)
aggregated_db <- augment_validation(aggregated_db)
aggregated_db <- data_correct_with_rules(aggregated_db)
aggregated_db <- augment_process_summary(aggregated_db)
aggregated_db <- augment_holiday(aggregated_db)
aggregated_db <- augment_seasons_id(aggregated_db)
aggregated_db <- augment_daylight(aggregated_db)
aggregated_db <- augment_daily(aggregated_db, col = "CTY", decay = 1)
aggregated_db <- augment_shifted(aggregated_db, col = "CTY", hour_shift = -1)
head(aggregated_db)
}
