#Copyright © 2018 RTE Réseau de transport d’électricité

#' @export
#' @title add daily average value
#' @description add daily average values to an existing dataset
#' @param x dataset
#' @param col column name of the measure to be aggregated and lagged
#' @param country_id column name of the country to be used in aggregations
#' @param decay lag as a number of days
#' @importFrom data.table .SD
#' @examples
#' load_dir <- system.file(package = "antaDraft",
#'   "data_sample/load_sample_2017")
#'
#' load_data <- anta_load(data_dir = load_dir )
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
#' aggregated_db <- augment_daily(aggregated_db, col = "CTY", decay = 1)
#' aggregated_db <- augment_daily(aggregated_db, col = "CTY", decay = 2)
#' head(aggregated_db)
augment_daily <- function(x, col, country_id = "country", decay = 1){

  meta <- capture_df_meta(x)

  x$day_date <- as.Date(format(x[[meta$timevar]], "%Y-%m-%d") )

  zz <- as.data.table(x[ is.finite(x[[col]]),])
  zz <- zz[, list(
    MIN = min(.SD[[1]], na.rm = TRUE),
    AVG = mean(.SD[[1]], na.rm = TRUE),
    MAX = max(.SD[[1]], na.rm = TRUE)
  ) , by = c(country_id, "day_date"), .SDcols = col ]

  new_names <- paste0("DAILY_", c("MIN_", "AVG_", "MAX_"), col, "_",
         ifelse(sign(decay)<0, "PLUS", "MINUS"), "_", abs(decay) )

  names(zz)[names(zz) %in% "MIN"] <- new_names[1]
  names(zz)[names(zz) %in% "AVG"] <- new_names[2]
  names(zz)[names(zz) %in% "MAX"] <- new_names[3]

  zz$day_date <- zz$day_date + -decay


  x <- as.data.table(x[, setdiff(names(x), new_names ) ])
  x <- merge(x, zz, all.x=TRUE, by = c(country_id, "day_date"))
  x$day_date <- NULL

  meta <- add_df_meta(meta, "daily_summary", unique( c(meta$daily_summary, new_names ) ) )

  restore_df_meta(x, meta = meta )
}



