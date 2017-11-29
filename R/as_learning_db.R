#' @title prepare an aggregated load dataset to be model
#' @description add columns to an aggregated load dataset
#' so that a model can be run.
#' @param dat aggregated load dataset
#' @param hour_decay integer specifying to shift data Y from \code{hour_decay}
#' and add the resulting column as variable
#' @param keep columns to keep from input dataset
#' @export
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
#' as_learning_db(aggregated_db)
as_learning_db <- function( dat, hour_decay = -1, keep = "summary" ){

  dat <- drop_validators(dat)

  existing_vars <- names(dat)

  dat2 <- augment_holiday(dat)
  dat2 <- augment_seasons_id(dat2)
  dat2 <- augment_daylight(dat2)
  dat2 <- augment_daily(dat2, col = "CTY", decay = 1)
  dat2 <- augment_daily(dat2, col = "CTY", decay = 2)
  dat2 <- augment_shifted(dat2, col = "CTY", hour_decay = hour_decay)

  added_vars <- setdiff(names(dat2), existing_vars)
  as.data.frame( dat2 )[,c(attr( dat, "id.vars"), "CTY", keep, added_vars )]
}

drop_validators <- function(x){
  id.vars <- attr( x, "id.vars")
  ts_key <- attr( x, "timevar")

  x <- x[, !names(x) %in% attr(x, "validators")]
  x <- as.data.frame(x)

  attr(x, "validators") <- character(0)
  attr( x, "id.vars") <- id.vars
  attr( x, "timevar") <- ts_key
  x
}

