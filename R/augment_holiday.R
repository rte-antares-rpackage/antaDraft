#' @export
#' @title add holiday column
#' @description add holiday column
#' @param x dataset
#' @param country_id column name specifying the country column
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
#' head(aggregated_db)
augment_holiday <- function(x, country_id = "country"){

  holidays <- NULL
  data("holidays", envir = environment() )

  id.vars <- attr(x, "id.vars")
  ts_key <- attr( x, "timevar")
  validators <- attr( x, "validators")
  x_class <- class(x)

  x[["Date_"]] <- as.Date(format(x[[ts_key]], "%Y-%m-%d")  )
  data <- x[, c("Date_", country_id) ]
  data <- unique(data)

  data <- merge(as.data.table(data), holidays, all.x=TRUE, by.x = c("Date_", country_id), by.y = c("Date", "country") )
  data$is_off[is.na(data$is_off)] <- FALSE
  data$likely_off[is.na(data$likely_off)] <- FALSE
  x <- merge(as.data.table(x), data, all.x=TRUE, by = c("Date_", country_id))
  x$Date_ <- NULL

  x <- as.data.frame(x)
  class(x) <- x_class
  attr(x, "validators") <- validators
  attr( x, "id.vars") <- id.vars
  attr( x, "timevar") <- ts_key

  x
}
