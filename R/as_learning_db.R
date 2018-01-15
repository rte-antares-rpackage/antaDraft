#' @title prepare an aggregated load dataset to be model
#' @description add columns to an aggregated load dataset
#' so that a model can be run.
#' @param data aggregated load dataset
#' @param target column to manipulate when shifting or summarizing
#' @export
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
#' newdb <- as_learning_db(aggregated_db)
#' head(newdb)
as_learning_db <- function( data, target = "CTY" ){

  hour_shift <- c(-1, 1)
  daily_summary <- c(-1, 1)

  data <- augment_holiday(data)
  data <- augment_seasons_id(data)
  data <- augment_daylight(data)
  for( hs in hour_shift)
    data <- augment_shifted(data, col = target, hour_shift = hs)
  for( ds in daily_summary)
    data <- augment_daily(data, col = target, decay = ds)

  meta <- capture_df_meta(data)
  meta <- add_df_meta(meta, "target", target )
  meta <- add_df_meta(meta, "hour_shift", hour_shift )
  meta <- add_df_meta(meta, "daily_shift", daily_summary )
  meta <- add_df_meta(meta, "models", list() )

  restore_df_meta(data, meta = meta, new_class = "learning_data" )
}

