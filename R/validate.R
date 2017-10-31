#' @importFrom validate validator confront values voptions
#' @importFrom tibble as_tibble
#' @export
#' @title evaluate validation rules for each line of a dataset
#' @description add logical columns to a dataset. Each column is a test to
#' perform against dataset.
#' @param data dataset
#' @examples
#' if( dir.exists( Sys.getenv("LOAD_DIR") ) ){
#'   load_data <- anta_load_read( data_dir = Sys.getenv("LOAD_DIR") )
#'   load_data <- augment_validation(data = load_data)
#' }
augment_validation <- function( data ){

  if( inherits(data, "raw_level") ){
    val_rules <- system.file(package = package_name, "raw_validate.yml")
    fp_rules <- system.file(package = package_name, "raw_fp.yml")
  } else if( inherits(data, "aggregated") ){
    val_rules <- system.file(package = package_name, "agg_validate.yml")
    fp_rules <- system.file(package = package_name, "agg_fp.yml")
  }


  init_classes <- class(data)
  id.vars <- attr( data, "id.vars")
  timevar <- attr( data, "timevar")


  v <- validator(.file = val_rules )
  voptions(v, raise='all', na.value = FALSE)
  data <- cbind(data, values( confront(data, v) ) )

  fp_expr_ <- fp_expr(fp_rules)

  data <- within(data, eval(fp_expr_))

  attr(data, "validators") <- names(v)
  class(data) <- c(init_classes, "controled" )

  attr( data, "id.vars") <- id.vars
  attr( data, "timevar") <- timevar

  data

}

