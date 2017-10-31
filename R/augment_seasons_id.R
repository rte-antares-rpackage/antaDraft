#' @export
#' @title add weekly season id columns
#' @description add weekly season id columns
#' @param x dataset
#' @param ts_key column name specifying the datetime column
#' @importFrom lubridate isoyear isoweek hour wday
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
#'     augment_seasons_id()
augment_seasons_id <- function(x, ts_key = "DateTime"){

  x$year.iso <- isoyear(x[[ts_key]])
  x$week.iso <- isoweek(x[[ts_key]])
  x$hour.iso <- hour(x[[ts_key]])
  x$day.iso <- wday(x[[ts_key]])

  x
}
