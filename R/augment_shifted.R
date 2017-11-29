#' @export
#' @title add a shifted value
#' @description add a shifted value to an existing dataset.
#' @param x dataset
#' @param col column name of the measure to be aggregated and lagged
#' @param hour_decay lag as a number of hours
#' @param summary_colname column name containing results from \code{\link{augment_process_summary}}
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
#'
#' load_data <- anta_load_read(data_dir = load_dir )
#' load_data <- augment_validation(data = load_data)
#' head(load_data)
#'
#' aggregated_db <- aggregate_with_rules(load_data)
#' aggregated_db <- augment_validation(aggregated_db)
#' aggregated_db <- data_correct_with_rules(aggregated_db)
#' aggregated_db <- augment_process_summary(aggregated_db)
#' aggregated_db <- augment_holiday(aggregated_db)
#' aggregated_db <- augment_seasons_id(aggregated_db)
#' aggregated_db <- augment_daylight(aggregated_db)
#' aggregated_db <- augment_daily(aggregated_db, col = "CTY", decay = 1)
#' aggregated_db <- augment_shifted(aggregated_db, col = "CTY", hour_decay = -1)
#' head(aggregated_db)
augment_shifted <- function(x, col, hour_decay = -1, summary_colname = "summary"){

  id.vars <- attr(x, "id.vars")
  ts_key <- attr( x, "timevar")
  validators <- attr( x, "validators")
  x_class <- class(x)

  x <- as.data.table(x)
  x <- setorderv(x, id.vars )

  CTY_H1 <- x[, c(id.vars, col, summary_colname), with=FALSE]
  CTY_H1[[col]] <- ifelse( CTY_H1[[summary_colname]] %in% "invalid", NA_real_ , CTY_H1[[col]])
  CTY_H1[[summary_colname]] <- NULL
  CTY_H1 <- CTY_H1[, DateTime := DateTime + (hour_decay * 60*60 ), by = "country" ]
  names( CTY_H1 )[3] <- paste0(col, "_HOUR_DECAY_", ifelse(sign(hour_decay)<0, "MINUS", "PLUS"), "_", abs(hour_decay) )

  x <- merge(x, CTY_H1, all.x = TRUE, all.y = FALSE, by = id.vars)
  x <- as.data.frame(x)
  class(x) <- x_class
  attr(x, "validators") <- validators
  attr( x, "id.vars") <- id.vars
  attr( x, "timevar") <- ts_key

  x
}



