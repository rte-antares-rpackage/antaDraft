#' @export
#' @title add day light columns
#' @description add day light columns
#' @param x dataset
#' @param ts_key column name specifying the datetime column
#' @param loc_id column name specifying the location column (country)
#' @importFrom suncalc getSunlightTimes
#' @importFrom dplyr inner_join
#' @examples
#'
#' if( dir.exists( Sys.getenv("LOAD_DIR") ) ){
#'   load_data <- anta_load_read( data_dir = Sys.getenv("LOAD_DIR") )
#'   load_data <- augment_validation(data = load_data)
#' }
#'
#' if( require(magrittr) )
#'   agg_db <- aggregate_with_rules(load_data) %>%
#'     augment_validation() %>%
#'     data_correct_with_rules() %>%
#'     augment_process_summary() %>%
#'     augment_holiday() %>%
#'     augment_seasons_id() %>%
#'     augment_daylight()
augment_daylight <- function(x, ts_key = "DateTime", loc_id = "country"){

  id.vars <- attr(x, "id.vars")
  timevar <- attr( x, "timevar")
  validators <- attr( x, "validators")
  x_class <- class(x)


  stopifnot( inherits(x[[ts_key]], "POSIXct") )

  country_coordinates <- NULL
  data("country_coordinates", envir = environment() )

  by_key <- c(loc_id);names(by_key) <- c("country")
  data <- inner_join(x, country_coordinates, by = by_key)
  data <- data[, c(ts_key, loc_id, "lon", "lat" ) ]
  data[[ts_key]] <- as.Date(format(data[[ts_key]], "%Y-%m-%d")  )
  names(data) <- c("date", loc_id, "lon", "lat")
  data <- distinct(data)

  daylight <- getSunlightTimes(
    data = data, keep = c("sunset", "sunrise"), tz = "CET")
  daylight$date <- as.Date(substr(daylight$date, 1, 10)  )

  x$dummy_date <- as.Date(format(x[[ts_key]], "%Y-%m-%d")  )

  by_key <- c(loc_id, "date" )
  names(by_key) <- c(loc_id, "dummy_date")

  x <- left_join(x, daylight, by = by_key)

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

