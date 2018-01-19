#' @export
#' @title add holiday column
#' @description add holiday column
#' @param x dataset
#' @param country_id column name specifying the country column
#' @examples
#' load_dir <- system.file(package = "antaDraft",
#'   "data_sample/load_sample_2017")
#'
#' load_data <- anta_load_read(data_dir = load_dir )
#' load_data <- augment_validation(data = load_data)
#' head(load_data)
#'
#' aggregated_db <- agg_data(load_data)
#' aggregated_db <- augment_validation(aggregated_db)
#' aggregated_db <- data_correct_with_rules(aggregated_db)
#' aggregated_db <- augment_process_summary(aggregated_db)
#' aggregated_db <- augment_holiday(aggregated_db)
#' head(aggregated_db)
augment_holiday <- function(x, country_id = "country"){

  holidays <- NULL
  data("holidays", envir = environment() )
  new_names <- c("is_off", "likely_off" )
  meta <- capture_df_meta(x)

  x[["Date_"]] <- as.Date(format(x[[meta$timevar]], "%Y-%m-%d")  )
  data <- x[, c("Date_", country_id) ]
  data <- unique(data)

  data <- merge(as.data.table(data), holidays, all.x=TRUE, by.x = c("Date_", country_id), by.y = c("Date", "country") )
  data$is_off[is.na(data$is_off)] <- FALSE
  data$likely_off[is.na(data$likely_off)] <- FALSE

  x <- as.data.table(x[, setdiff(names(x), new_names ) ])
  x <- merge(x, data, all.x=TRUE, by = c("Date_", country_id))
  x$Date_ <- NULL

  meta <- add_df_meta(meta, "holiday_columns", unique( c(meta$holiday_columns, new_names ) ) )

  restore_df_meta(x, meta = meta )
}
