#' @importFrom data.table year setnames
#' @export
#' @title production data per types of production
#' @description import csv data representing production per types. The data
#' is read from an entsoe repository.
#' @param production_dir datasets directory of data energy productions
#' by types.
#' @param capacity_dir datasets directory of data energy capacities
#' by types.
#' @examples
#' production_dir <- system.file(package = "antaDraft", "data_sample",
#'   "prod_sample_20160129/B01")
#' capacity_dir <- system.file(package = "antaDraft", "data_sample",
#'   "prod_sample_20160129/B06")
#' prod_by_types <- anta_prod_type(production_dir, capacity_dir)
anta_prod_type <- function(production_dir = NULL, capacity_dir = NULL){

  stopifnot(dir.exists(production_dir))

  id_vars <- c("DateTime", "AreaTypeCode", "AreaName", "MapCode", "ProductionType_Name")
  time_vars <- "DateTime"
  submission_time_var <- "SubmissionTS"
  data <- entsoe_dir_reader(dir = production_dir, datetime_col = time_vars,
                    submissin_col = submission_time_var,
                    drops = c("year", "month", "day"),
                    id_vars = id_vars,
                    ct_format = "%Y-%m-%d %H:%M:%S")

  setnames(data, "ProductionType_Name", "production_type")
  setnames(data, "ActualConsumption","consumption")
  setnames(data, "ActualGenerationOutput","generation_output")
  data$observed <- TRUE

  data <- ref_join_class(x = data, classobj= "prod_type", date_time = time_vars)

  data$observed[is.na(data$observed)] <- FALSE

  capacity_channel <- anta_capacity_channel(data_dir = capacity_dir)

  data <- merge( x = capacity_channel, y = data,
                 by = c("DateTime", "MapCode", "AreaTypeCode",
                        "production_type", "country"),
                 all.x = TRUE, all.y = TRUE )

  data <- as.data.frame(data)

  class(data) <- c(class(data), "raw_prod_type" )
  attr( data, "id.vars") <- c("country", "MapCode", "AreaTypeCode", "DateTime", "AreaName", "production_type")
  attr( data, "timevar") <- "DateTime"
  attr( data, "countryvar") <- "country"

  data
}

anta_capacity_channel <- function( data_dir = NULL){
  stopifnot(dir.exists(data_dir))

  id_vars <- c("DateTime", "AreaTypeCode", "MapCode", "ProductionType_Name")
  time_vars <- "DateTime"
  submission_time_var <- "SubmissionTS"
  drops <- c("year", "month", "day", "AreaName")
  data <- entsoe_dir_reader(dir = data_dir, datetime_col = time_vars,
                            submissin_col = submission_time_var,
                            drops = drops,
                            id_vars = id_vars,
                            ct_format = "%Y-%m-%d %H:%M:%S")

  setnames(data, "AggregatedInstalledCapacity","installed_capacity")
  setnames(data, "ProductionType_Name", "production_type")
  data <- data[installed_capacity>0]

  data <- ref_join_class(x = data, classobj = "prod_capacity_type", date_time = time_vars)

  data
}


