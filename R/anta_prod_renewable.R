#' @export
#' @title renewable production data per types of production
#' @description import csv data representing production per types. The data
#' is read from an entsoe repository.
#' @param production_dir datasets directory of data energy productions
#' by types.
#' @param capacity_dir datasets directory of data energy capacities
#' by types.
#' @examples
#' production_dir <- system.file(package = "antaDraft", "data_sample",
#'   "prod_sample_20160129")
#' raw_type_renewable <- anta_prod_renewable(file.path(production_dir, "B01"),
#'   file.path(production_dir, "B06") )
anta_prod_renewable <- function(production_dir = NULL, capacity_dir = NULL){

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

  global_options <- getOption("global_options")
  data <- ref_join_class(x = data, classobj = "on_ctry_dates_prod_type",
                          date_time = time_vars, prod_file_yaml = global_options$renewable_production_per_country)
  data$observed[is.na(data$observed)] <- FALSE

  capacity_channel <- anta_prod_capacity(data_dir = capacity_dir, join_class = "renewable_prod_type")
  data <- merge( x = capacity_channel, y = data,
                 by = c("DateTime", "MapCode", "AreaTypeCode",
                        "production_type", "country"),
                 all.x = TRUE, all.y = TRUE )

  data <- as.data.frame(data)

  class(data) <- c(class(data), "raw_prod_renewable_type" )
  attr( data, "id.vars") <- c("country", "MapCode", "AreaTypeCode", "DateTime", "AreaName", "production_type")
  attr( data, "timevar") <- "DateTime"
  attr( data, "countryvar") <- "country"

  data
}

