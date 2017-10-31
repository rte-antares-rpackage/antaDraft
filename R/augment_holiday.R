#' @export
#' @title add holiday column
#' @description add holiday column
#' @param x dataset
#' @param country_id column name specifying the country column
#' @importFrom tidyr replace_na
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
#'     augment_holiday()
augment_holiday <- function(x, country_id = "country"){

  holidays <- NULL
  data("holidays", envir = environment() )

  id.vars <- attr(x, "id.vars")
  ts_key <- attr( x, "timevar")
  validators <- attr( x, "validators")
  x_class <- class(x)

  x[["Date_"]] <- as.Date(format(x[[ts_key]], "%Y-%m-%d")  )
  data <- x[, c("Date_", country_id) ]
  data <- distinct(data)
  key_ <- c("Date", "country")
  names(key_) <- c("Date_", country_id)
  data <- left_join(data, holidays, by = key_)
  data <- replace_na(data = data, replace = list(is_off = FALSE, likely_off = FALSE))
  x <- left_join(x, data, by = c("Date_", country_id) )
  x$Date_ <- NULL

  x <- as.data.frame(x)
  class(x) <- x_class
  attr(x, "validators") <- validators
  attr( x, "id.vars") <- id.vars
  attr( x, "timevar") <- ts_key

  x
}
