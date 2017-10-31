#' @export
#' @title add summary columns
#' @description add validation column
#' @param data dataset
#' @param colname name of the column to create that will contain
#' processed informations (i.e. original, invalid, corrected, ...)
#' @examples
#' if( dir.exists( Sys.getenv("LOAD_DIR") ) ){
#'   load_data <- anta_load_read( data_dir = Sys.getenv("LOAD_DIR") )
#'   load_data <- augment_validation(data = load_data)
#'   agg_db <- aggregate_with_rules(load_data)
#'   agg_db <- augment_validation(agg_db)
#'   agg_db <- data_correct_with_rules(agg_db)
#'   agg_db <- augment_process_summary(agg_db)
#' }
augment_process_summary <- function( data, colname = "summary" ){

  corrections_rules <- system.file(package = package_name, "agg_correct.yml")
  rules <- yaml.load_file(corrections_rules)
  replace_var <- rule_names(rules)

  invalid <- data[, attr(data, "validators"), drop = FALSE]
  invalid <- !apply(invalid, 1, all)
  corrected <- data[, replace_var, drop = FALSE]
  corrected <- apply(corrected, 1, any)

  coldata <- rep("original", nrow(data))
  coldata[invalid] <- "invalid"
  coldata[corrected] <- "corrected"
  # coldata <- factor(coldata, levels = c("original", "invalid", "corrected", "rf_predicted") )
  data[[colname]] <- coldata

  data

}
