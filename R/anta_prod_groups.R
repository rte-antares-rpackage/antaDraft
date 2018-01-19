#' @export
#' @title import production data per groups from an entsoe repository
#' @description import csv data representing production per groups data
#' from an entsoe repository.
#' @param production_dir datasets directory of production by type files
#' @examples
#' production_dir <- system.file(package = "antaDraft", "data_sample",
#'   "prod_sample_20160129/B02")
#' raw_groups <- anta_prod_group(production_dir)
anta_prod_group <- function(production_dir = NULL){

  stopifnot(dir.exists(production_dir))

  id_vars <- c("DateTime", "AreaTypeCode", "AreaName", "MapCode", "PowerSystemResourceName")
  time_vars <- "DateTime"
  submission_time_var <- "SubmissionTS"
  data <- entsoe_dir_reader(dir = production_dir, datetime_col = time_vars,
                            submissin_col = submission_time_var,
                            drops = c("year", "month", "day"),
                            id_vars = id_vars,
                            ct_format = "%Y-%m-%d %H:%M:%S")
  setnames(data, "ActualConsumption","consumption")
  setnames(data, "ActualGenerationOutput","generation_output")
  setnames(data, "InstalledGenCapacity","installed_capacity")
  setnames(data, "ProductionTypeName", "production_type")
  data$observed <- TRUE

  data <- ref_join_class(x = data, classobj= "prod_type", date_time = time_vars)

  data$observed[is.na(data$observed)] <- FALSE

  data <- as.data.frame(data)

  class(data) <- c(class(data), "raw_group_prod" )
  attr( data, "id.vars") <- c("country", "MapCode", "AreaTypeCode", "DateTime", "AreaName", "production_type", "PowerSystemResourceName")
  attr( data, "timevar") <- "DateTime"
  attr( data, "countryvar") <- "country"

  data
}

