#Copyright © 2018 RTE Réseau de transport d’électricité

#' @export
#' @title add summary columns
#' @description add validation column
#' @param data dataset
#' @param colname name of the column to create that will contain
#' processed informations (i.e. original, invalid, corrected, ...)
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
#' head(aggregated_db)
augment_process_summary <- function( data, colname = "summary" ){

  load_options <- getOption("load_options")
  meta <- capture_df_meta(data)

  rules <- yaml.load_file(load_options$correct)

  replace_var <- rule_names(rules)

  invalid <- data[, meta$validators, drop = FALSE]
  invalid <- !apply(invalid, 1, all)
  corrected <- data[, replace_var, drop = FALSE]
  corrected <- apply(corrected, 1, any)

  coldata <- rep("original", nrow(data))
  coldata[invalid] <- "invalid"
  coldata[corrected] <- "corrected"
  data[[colname]] <- coldata

  meta <- add_df_meta(meta, "colname_process_summary", colname )
  restore_df_meta(data, meta = meta, new_class = "summarized" )
}
