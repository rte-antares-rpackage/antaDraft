#' @export
#' @title add weekly season id columns
#' @description add weekly season id columns
#' @param x dataset
#' @param ts_key column name specifying the datetime column
#' @importFrom lubridate isoyear isoweek hour wday
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
#' head(aggregated_db)
augment_seasons_id <- function(x, ts_key = "DateTime"){
  meta <- capture_df_meta(x)

  x$year.iso <- isoyear(x[[ts_key]])
  x$week.iso <- isoweek(x[[ts_key]])
  x$hour.iso <- hour(x[[ts_key]])
  x$day.iso <- wday(x[[ts_key]])

  meta <- add_df_meta(meta, "season_columns", c("year.iso", "week.iso", "hour.iso", "day.iso" ) )

  restore_df_meta(x, meta = meta )
}

