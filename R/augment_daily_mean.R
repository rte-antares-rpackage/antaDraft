#' @export
#' @title add holiday column
#' @description add holiday column
#' @param x dataset
#' @param col column name of the measure to be aggregated and lagged
#' @param country_id column name of the country to be used in aggregations
#' @param decay lag as a number of days
#' @importFrom data.table .SD
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
#' aggregated_db <- augment_daily(aggregated_db, col = "CTY", decay = 2)
#' head(aggregated_db)
augment_daily <- function(x, col, country_id = "country", decay = 1){

  id.vars <- attr(x, "id.vars")
  ts_key <- attr( x, "timevar")
  validators <- attr( x, "validators")
  x_class <- class(x)

  x$day_date <- as.Date(format(x[[ts_key]], "%Y-%m-%d") )

  zz <- as.data.table(x)
  zz <- zz[, list(
    MIN = min(.SD[[1]], na.rm = TRUE),
    AVG = mean(.SD[[1]], na.rm = TRUE),
    MAX = max(.SD[[1]], na.rm = TRUE)
  ) , by = c(country_id, "day_date"), .SDcols = col ]

  names(zz)[names(zz) %in% "MIN"] <- paste0("MIN_", col, "_", decay)
  names(zz)[names(zz) %in% "AVG"] <- paste0("AVG_", col, "_", decay)
  names(zz)[names(zz) %in% "MAX"] <- paste0("MAX_", col, "_", decay)

  zz$day_date <- zz$day_date + decay

  x <- as.data.table(x)
  x <- merge(x, zz, all.x=TRUE, by = c(country_id, "day_date"))
  x$day_date <- NULL
  x <- as.data.frame(x)
  class(x) <- x_class
  attr(x, "validators") <- validators
  attr( x, "id.vars") <- id.vars
  attr( x, "timevar") <- ts_key

  x
}



