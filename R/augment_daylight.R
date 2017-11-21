#' @export
#' @title add day light columns
#' @description add day light columns
#' @param x dataset
#' @param ts_key column name specifying the datetime column
#' @param loc_id column name specifying the location column (country)
#' @importFrom suncalc getSunlightTimes
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
#' head(aggregated_db)
augment_daylight <- function(x, ts_key = "DateTime", loc_id = "country"){

  id.vars <- attr(x, "id.vars")
  timevar <- attr( x, "timevar")
  validators <- attr( x, "validators")
  x_class <- class(x)


  stopifnot( inherits(x[[ts_key]], "POSIXct") )

  country_coordinates <- NULL
  data("country_coordinates", envir = environment() )

  by_key <- c(loc_id);names(by_key) <- c("country")
  data <- merge(as.data.table(x), country_coordinates, all=FALSE, by = by_key)
  data <- as.data.frame(data)
  data <- data[, c(ts_key, loc_id, "lon", "lat" ) ]
  data[[ts_key]] <- as.Date(format(data[[ts_key]], "%Y-%m-%d")  )
  names(data) <- c("date", loc_id, "lon", "lat")
  data <- unique(data)

  daylight <- getSunlightTimes(
    data = data, keep = c("sunset", "sunrise"), tz = "CET")
  daylight$date <- as.Date(substr(daylight$date, 1, 10)  )

  x$dummy_date <- as.Date(format(x[[ts_key]], "%Y-%m-%d")  )


  x <- merge(as.data.table(x), daylight, all.x=TRUE, by.x = c(loc_id, "dummy_date"), by.y = c(loc_id, "date" ) )
  x <- as.data.frame(x)
  x$light_time <- as.integer(difftime(x$sunset, x$sunrise, units = "mins" ))

  x$dummy_date <- NULL
  x$lon <- NULL
  x$lat <- NULL
  x$sunset <- NULL
  x$sunrise <- NULL

  class(x) <- x_class
  attr(x, "validators") <- validators
  attr( x, "id.vars") <- id.vars
  attr( x, "timevar") <- timevar

  x
}

