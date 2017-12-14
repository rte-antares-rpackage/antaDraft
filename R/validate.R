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
#'
#' data_neg_val <- structure(list(country = c("LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG",
#' "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG",
#' "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG",
#' "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG",
#' "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG", "LUXEMBOURG",
#' "LUXEMBOURG"), DateTime = structure(c(1502582400, 1502586000,
#' 1502589600, 1502593200, 1502596800, 1502600400, 1502604000, 1502607600,
#' 1502611200, 1502614800, 1502618400, 1502622000, 1502625600, 1502629200,
#' 1502632800, 1502636400, 1502640000, 1502643600, 1502647200, 1502650800,
#' 1502654400, 1502658000, 1502661600, 1502665200), class = c("POSIXct",
#' "POSIXt")), BZN = c(346.000000000003, 352.000000000003, 354.999999999998,
#' 330.000000000002, 345.000000000003, 363.999999999998, 367, 374,
#' 389.999999999997, 373, 351, 342, 339, 344, 367, 386, -36032.64,
#' 398.999999999998, 414.000000000005, 412.000000000005, 381.999999999998,
#' 351.999999999998, 336.000000000002, 332.999999999995), CTA = c(346,
#' 352, 355, 330, 345, 364, 367, 374, 390, 373, 351, 342, 339, 344,
#' 367, 386, 401, 399, 414, 412, 382, 352, 336, 333), CTY = c(346,
#' 352, 355, 330, 345, 364, 367, 374, 390, 373, 351, 342, 339, 344,
#' 367, 386, 401, 399, 414, 412, 382, 352, 336, 333)), .Names = c("country",
#' "DateTime", "BZN", "CTA", "CTY"), row.names = 179664:179687, class = c("data.frame",
#' "aggregated"))
#'
#' aggregated_db <- augment_validation(data_neg_val)
#' aggregated_db
#'
augment_validation <- function( data ){
  UseMethod("augment_validation")

  load_options <- getOption("load_options")

  if( inherits(data, "raw_level") ){
    val_rules <- load_options$validate$raw$validate
    fp_rules <- load_options$validate$raw$false_pos
  } else if( inherits(data, "aggregated") ){
    val_rules <- load_options$validate$agg$validate
    fp_rules <- load_options$validate$agg$false_pos
  }

  meta <- capture_df_meta(data)


  v <- validator(.file = val_rules )
  voptions(v, raise='all', na.value = FALSE)
  confront_ <- values( confront(as.data.frame(data), v) )
  data <- cbind(data[, setdiff(names(data), colnames(confront_))], confront_ )

  fp_expr_ <- fp_expr(fp_rules)

  data <- within(data, eval(fp_expr_))

  meta <- add_df_meta(meta, "validators", names(v))
  restore_df_meta(data, meta = meta, new_class = "controled" )
}

#' @export
#' @rdname augment_validation
augment_validation.raw_level <- function( data ){

  load_options <- getOption("load_options")
  val_rules <- load_options$validate$raw$validate
  fp_rules <- load_options$validate$raw$false_pos

  meta <- capture_df_meta(data)

  v <- validator(.file = val_rules )
  voptions(v, raise='all', na.value = FALSE)
  confront_ <- values( confront(as.data.frame(data), v) )
  data <- cbind(data[, setdiff(names(data), colnames(confront_))], confront_ )

  fp_expr_ <- fp_expr(fp_rules)

  data <- within(data, eval(fp_expr_))

  meta <- add_df_meta(meta, "validators", names(v))
  restore_df_meta(data, meta = meta, new_class = "controled" )
}

#' @export
#' @rdname augment_validation
augment_validation.aggregated <- function( data ){

  load_options <- getOption("load_options")

  val_rules <- load_options$validate$agg$validate
  fp_rules <- load_options$validate$agg$false_pos

  meta <- capture_df_meta(data)


  v <- validator(.file = val_rules )
  voptions(v, raise='all', na.value = FALSE)
  confront_ <- values( confront(as.data.frame(data), v) )
  data <- cbind(data[, setdiff(names(data), colnames(confront_))], confront_ )

  fp_expr_ <- fp_expr(fp_rules)

  data <- within(data, eval(fp_expr_))

  meta <- add_df_meta(meta, "validators", names(v))
  restore_df_meta(data, meta = meta, new_class = "controled" )
}

#' @export
#' @rdname augment_validation
augment_validation.raw_channel_prod <- function( data ){

  load_options <- getOption("prod_options")

  val_rules <- load_options$validate$raw$validate
  fp_rules <- load_options$validate$raw$false_pos

  meta <- capture_df_meta(data)


  v <- validator(.file = val_rules )
  voptions(v, raise='all', na.value = FALSE)
  confront_ <- values( confront(as.data.frame(data), v) )
  data <- cbind(data[, setdiff(names(data), colnames(confront_))], confront_ )

  fp_expr_ <- fp_expr(fp_rules)

  data <- within(data, eval(fp_expr_))

  meta <- add_df_meta(meta, "validators", names(v))
  restore_df_meta(data, meta = meta, new_class = "controled" )
}

