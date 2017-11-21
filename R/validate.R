#' @importFrom validate validator confront values voptions
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

  load_options <- getOption("load_options")

  if( inherits(data, "raw_level") ){
    val_rules <- load_options$validate$raw$validate
    fp_rules <- load_options$validate$raw$false_pos
  } else if( inherits(data, "aggregated") ){
    val_rules <- load_options$validate$agg$validate
    fp_rules <- load_options$validate$agg$false_pos
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

