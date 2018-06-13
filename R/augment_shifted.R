#Copyright © 2018 RTE Réseau de transport d’électricité

#' @export
#' @title add a shifted value
#' @description add a shifted value to an existing dataset.
#' @param x dataset
#' @param col column name of the measure to be aggregated and lagged
#' @param hour_shift lag as a number of hours
#' @param summary_colname column name containing results from \code{\link{augment_process_summary}}
#' @examples
#' load_dir <- system.file(package = "antaDraft",
#'   "data_sample/load_sample_2017")
#'
#' load_data <- anta_load(data_dir = load_dir )
#' load_data <- augment_validation(data = load_data)
#' head(load_data)
#'
#' aggregated_db <- agg_data(load_data)
#' aggregated_db <- augment_validation(aggregated_db)
#' aggregated_db <- data_correct_with_rules(aggregated_db)
#' aggregated_db <- augment_process_summary(aggregated_db)
#' aggregated_db <- augment_holiday(aggregated_db)
#' aggregated_db <- augment_seasons_id(aggregated_db)
#' aggregated_db <- augment_daylight(aggregated_db)
#' aggregated_db <- augment_daily(aggregated_db, col = "CTY", decay = 1)
#' aggregated_db <- augment_shifted(aggregated_db, col = "CTY", hour_shift = -1)
#' head(aggregated_db)
augment_shifted <- function(x, col, hour_shift = -1, summary_colname = "summary"){

  meta <- capture_df_meta(x)

  new_name <- paste0("HOUR_SHIFT_", col, "_",
                     ifelse(sign(hour_shift)<0, "PLUS", "MINUS"), "_", abs(hour_shift) )

  x <- as.data.table(x[, setdiff(names(x), new_name ) ])
  x <- setorderv(x, c(meta$countryvar, meta$timevar, meta$id.vars ) )

  CTY_H1 <- x[, c(meta$countryvar, meta$timevar, meta$id.vars, col, summary_colname), with=FALSE]
  CTY_H1[[col]] <- ifelse( CTY_H1[[summary_colname]] %in% "invalid", NA_real_ , CTY_H1[[col]])
  CTY_H1[[summary_colname]] <- NULL
  CTY_H1 <- CTY_H1[, DateTime := DateTime + (-hour_shift * 60*60 ), by = "country" ]
  names( CTY_H1 )[3] <- new_name

  x <- merge(x, CTY_H1, all.x = TRUE, all.y = FALSE, by = c(meta$countryvar, meta$timevar, meta$id.vars ) )

  meta <- add_df_meta(meta, "shift_columns", unique( c(meta$shift_columns, new_name ) ) )

  restore_df_meta(x, meta = meta )
}



