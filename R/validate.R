#' @importFrom validate validator confront values voptions
#' @importFrom tibble as_tibble
#' @export
#' @title evaluate validation rules for each line of a dataset
#' @description add logical columns to a dataset. Each column is a test to
#' perform against dataset.
#' @param data dataset
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
#'
#' load_data <- anta_load_read(data_dir = load_dir )
#' load_data <- augment_validation(data = load_data)
#' head(load_data)
#'
#' aggregated_db <- aggregate_with_rules(load_data)
#' aggregated_db <- augment_validation(aggregated_db)
#' head(aggregated_db)
augment_validation <- function( data ){

  if( inherits(data, "raw_level") ){
    val_rules <- system.file(package = package_name, "config", "load", "raw_validate.yml")
    fp_rules <- system.file(package = package_name, "config", "load", "raw_fp.yml")
  } else if( inherits(data, "aggregated") ){
    val_rules <- system.file(package = package_name, "config", "load", "agg_validate.yml")
    fp_rules <- system.file(package = package_name, "config", "load", "agg_fp.yml")
  }


  init_classes <- class(data)
  id.vars <- attr( data, "id.vars")
  timevar <- attr( data, "timevar")


  v <- validator(.file = val_rules )
  voptions(v, raise='all', na.value = FALSE)
  confront_ <- values( confront(data, v) )
  data <- cbind(data[, setdiff(names(data), colnames(confront_))], confront_ )

  fp_expr_ <- fp_expr(fp_rules)

  data <- within(data, eval(fp_expr_))

  attr(data, "validators") <- names(v)
  class(data) <- c(init_classes, "controled" )

  attr( data, "id.vars") <- id.vars
  attr( data, "timevar") <- timevar

  data

}

