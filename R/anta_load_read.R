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
#' load_data <- anta_load_read(data_dir = load_dir )
anta_load_read <- function( data_dir = NULL ){
  stopifnot(dir.exists(data_dir))

  id_vars <- c("DateTime", "AreaTypeCode", "AreaName", "MapCode")
  time_vars <- "DateTime"
  submission_time_var <- "SubmissionTS"
  data <- entsoe_dir_reader(dir = data_dir, datetime_col = time_vars,
                            submissin_col = submission_time_var,
                            drops = c("year", "month", "day"),
                            id_vars = id_vars,
                            ct_format = "%Y-%m-%d %H:%M:%S")

  data <- ref_join_class(x = data,
                         classobj = "std_data",
                         date_time = time_vars)

  data <- as.data.frame(data)
  class(data) <- c(class(data), "raw_level" )
  attr( data, "id.vars") <- c("country", "MapCode", "AreaTypeCode", "DateTime")
  attr( data, "timevar") <- "DateTime"
  attr( data, "countryvar") <- "country"

  data
}

