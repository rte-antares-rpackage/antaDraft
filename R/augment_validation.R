#' @importFrom validate validator confront values voptions
#' @importFrom tibble as_tibble
#' @export
#' @title evaluate validation rules for each line of a dataset
#' @description add logical columns to a dataset. Each column is a test to
#' perform against dataset.
#' @param data dataset
#' @param val_rules yaml file containing validation rules
#' @param fp_rules yaml file containing false positive rules to drop from results
#' @examples
#' data(load_example)
#' cty_rules <- system.file(package = "entsoe", "templates/cty_rules.yaml")
#' val_rules <- system.file(package = "entsoe", "templates/raw/validate.yml")
#' fp_rules <- system.file(package = "entsoe", "templates/raw/false_positives.yml")
#' raw_db <- augment_rules(load_example, file_rules = cty_rules)
#' raw_db <- augment_validation(data = raw_db, val_rules = val_rules,
#'   fp_rules = fp_rules)
augment_validation <- function( data, val_rules = NULL, fp_rules = NULL ){

  stopifnot( !is.null(val_rules), file.exists(val_rules) )
  stopifnot( !is.null(fp_rules), file.exists(fp_rules) )

  init_classes <- class(data)

  id.vars <- attr( data, "id.vars")
  timevar <- attr( data, "timevar")

  v <- validator(.file = val_rules )
  voptions(v, raise='all', na.value = FALSE)

  all_res <- values( confront(data, v) )

  data <- cbind(data, as_tibble(all_res) )

  fp_expr_ <- fp_expr(fp_rules)

  data <- within(data, eval(fp_expr_))

  attr(data, "validators") <- names(v)
  class(data) <- c(init_classes, "controled" )

  data$VALIDATED <- apply(data[, attr(data, "validators") ], MARGIN = 1, FUN = all)

  attr( data, "id.vars") <- id.vars
  attr( data, "timevar") <- timevar

  data

}


