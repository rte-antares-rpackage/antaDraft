#' @export
#' @title correct datasets
#' @description correct data based on condition expressed in a yaml file.
#' @param data dataset
#' @param corrections_rules yaml file containing correction rules
#' @examples
#' data(load_example)
#' cty_rules <- system.file(package = "entsoe",
#'   "templates/cty_rules.yaml")
#' val_rules <- system.file(package = "entsoe",
#'   "templates/raw/validate.yml")
#' fp_rules <- system.file(package = "entsoe",
#'   "templates/raw/false_positives.yml")
#' correct_rules <- system.file(package = "entsoe",
#'   "templates/aggregated/correct.yml")
#' raw_db <- augment_rules(load_example, file_rules = cty_rules)
#' raw_db <- augment_validation(data = raw_db, val_rules = val_rules,
#'   fp_rules = fp_rules)
#'
#' val_rules <- system.file(package = "entsoe",
#'   "templates/aggregated/validation.yml")
#' fp_rules <- system.file(package = "entsoe",
#'   "templates/aggregated/false_positives.yml")
#' agg_data <- aggregate_with_rules(raw_db)
#' agg_data <- augment_validation(agg_data, val_rules = val_rules,
#'   fp_rules = fp_rules)
#' data_correct(agg_data, corrections_rules = correct_rules)
data_correct <- function( data, corrections_rules = NULL ){

  stopifnot( !is.null(corrections_rules), file.exists(corrections_rules) )

  init_classes <- class(data)

  markers_exprs_ <- mark_correct_exprs(corrections_rules)
  correct_exprs_ <- correct_exprs(corrections_rules)
  data <- within(data, eval(markers_exprs_))
  data <- within(data, eval(correct_exprs_))

  class(data) <- c(init_classes, "corrected" )

  data

}

