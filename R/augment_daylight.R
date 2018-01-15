#' @export
#' @title add day light columns
#' @description add day light columns
#' @param x dataset
#' @param loc_id column name specifying the location column (country)
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
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
#' aggregated_db <- augment_seasons_id(aggregated_db)
#' aggregated_db <- augment_daylight(aggregated_db)
#' head(aggregated_db)
#' @importFrom utils data
augment_daylight <- function(x, loc_id = "country"){

  meta <- capture_df_meta(x)

  stopifnot( inherits(x[[meta$timevar]], "POSIXct") )

  daylight <- NULL
  data("daylight", envir = environment() )

  x$dummy_date <- as.Date(format(x[[meta$timevar]], "%Y-%m-%d")  )

  x <- merge(as.data.table(x), daylight, all.x=TRUE, by.x = c(loc_id, "dummy_date"), by.y = c("country", "date" ) )
  x <- as.data.frame(x)
  x$light_time <- as.integer(difftime(x$sunset, x$sunrise, units = "mins" ))

  x$dummy_date <- NULL
  x$sunset <- NULL
  x$sunrise <- NULL

  meta <- add_df_meta(meta, "daylight_columns", c("light_time" ) )

  restore_df_meta(x, meta = meta )
}

