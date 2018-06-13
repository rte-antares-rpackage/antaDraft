#Copyright © 2018 RTE Réseau de transport d’électricité

#' @importFrom yaml yaml.load_file
#' @export
#' @title import load data from an entsoe repository
#' @description import csv data representing load data
#' from an entsoe repository.
#' @param data_dir datasets directory
#' @importFrom lubridate minute
#' @importFrom data.table fread rbindlist CJ data.table
#' @examples
#' load_dir <- system.file(package = "antaDraft",
#'   "data_sample/load_sample_2017")
#' load_data <- anta_load(data_dir = load_dir )
anta_load <- function( data_dir = NULL ){
  stopifnot(dir.exists(data_dir))

  id_vars <- c("DateTime", "AreaTypeCode", "AreaName", "MapCode")
  time_vars <- "DateTime"
  submission_time_var <- "SubmissionTS"
  data <- entsoe_dir_reader(dir = data_dir, datetime_col = time_vars,
                            submissin_col = submission_time_var,
                            drops = c("year", "month", "day"),
                            id_vars = id_vars)

  data <- ref_join_class(x = data,
                         classobj = "raw_load",
                         date_time = time_vars)

  data <- as.data.frame(data)
  class(data) <- c(class(data), "load_raw" )
  attr( data, "id.vars") <- c("MapCode", "AreaTypeCode")
  attr( data, "timevar") <- "DateTime"
  attr( data, "countryvar") <- "country"
  attr( data, "label") <- "Actual Total Load [6.1]"

  data
}


#' @export
#' @rdname agg_data
#' @importFrom data.table as.data.table dcast setorderv
agg_data.load_raw <- function(x, ...){

  meta <- new_df_meta()
  dimensions <- get_ctry_rules(add_complex = TRUE )
  measures <- unique(dimensions[["AreaTypeCode"]])

  x <- x[ apply( x[, attr(x, "validators"), drop = FALSE], 1, all ) , , drop = FALSE]
  x <- as.data.table(x)
  out <- x[, list(TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE) ),
           by=c("country", "AreaTypeCode", "DateTime")]

  add_db <- cyclic_dataset(out, y = "TotalLoadValue", group_col = "country", measures_col = "AreaTypeCode" )

  out <- rbind(out, add_db)

  out <- out[, list(TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE) ),
             by=c("country", "AreaTypeCode", "DateTime")]

  out <- dcast(out, country + DateTime ~ AreaTypeCode,
               value.var = "TotalLoadValue",
               fun.aggregate = sum, na.rm = FALSE)

  out <- ref_join_class(x = out, classobj = "on_ctry_dates", date_time = "DateTime")

  meta <- add_df_meta(meta, "id.vars", character(0))
  meta <- add_df_meta(meta, "timevar", "DateTime")
  meta <- add_df_meta(meta, "measures", measures )
  meta <- add_df_meta(meta, "countryvar", "country" )
  restore_df_meta(out, meta = meta, new_class = "aggregated" )
}

#' @export
#' @rdname augment_validation
augment_validation.load_raw <- function( data ){
  load_options <- getOption("load_options")
  val_rules <- load_options$validate$raw$validate
  fp_rules <- load_options$validate$raw$false_pos
  add_validation_columns( val_rules_file = val_rules,
                          falsepos_rules_file = fp_rules,
                          data = data )
}

#' @export
#' @rdname augment_validation
augment_validation.aggregated <- function( data ){

  load_options <- getOption("load_options")

  val_rules <- load_options$validate$agg$validate
  fp_rules <- load_options$validate$agg$false_pos

  add_validation_columns( val_rules_file = val_rules,
                          falsepos_rules_file = fp_rules,
                          data = data )
}



