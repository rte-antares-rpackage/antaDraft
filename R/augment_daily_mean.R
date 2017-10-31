#' @export
#' @title add holiday column
#' @description add holiday column
#' @param x dataset
#' @param col column name of the measure to be aggregated and lagged
#' @param country_id column name of the country to be used in aggregations
#' @param decay lag as a number of days
#' @importFrom dplyr group_by_at
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
#'     augment_daylight() %>%
#'     augment_daily(col = "CTY", decay = 1) %>%
#'     augment_daily(col = "CTY", decay = 2)
augment_daily <- function(x, col, country_id = "country", decay = 1){

  id.vars <- attr(x, "id.vars")
  ts_key <- attr( x, "timevar")
  validators <- attr( x, "validators")
  x_class <- class(x)

  x$day_date <- as.Date(format(x[[ts_key]], "%Y-%m-%d") )

  zz <- group_by_at(x, .vars = c(country_id, "day_date") )
  zz <- summarise_mean_var(zz, col, paste0("_D", decay))
  zz$day_date <- zz$day_date + decay

  x <- left_join(x, zz, by = c(country_id, "day_date") )
  x$day_date <- NULL


  x <- as.data.frame(x)
  class(x) <- x_class
  attr(x, "validators") <- validators
  attr( x, "id.vars") <- id.vars
  attr( x, "timevar") <- ts_key

  x
}

#' @importFrom rlang sym syms
summarise_mean_var <- function(df, colname, suffix) {
  mean_name <- paste0("AVG_", colname, suffix)
  max_name <- paste0("MAX_", colname, suffix)
  min_name <- paste0("MIN_", colname, suffix)
  summarise(df,
            !!min_name := min(!!!sym( colname ), na.rm = TRUE) ,
            !!mean_name := mean(!!!sym( colname ), na.rm = TRUE) ,
            !!max_name := max(!!!sym( colname ), na.rm = TRUE)  )
}
