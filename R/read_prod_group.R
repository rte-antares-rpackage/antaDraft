#' @importFrom data.table year setnames setDF setDT
#' @export
#' @title production data per groups of production
#' @description import csv data representing production per groups The data
#' is read from an entsoe repository.
#' @param production_dir datasets directory of data energy productions
#' by groups.
#' @param production_file production file to be used
#' @examples
#' production_dir <- system.file(package = "antaDraft", "data_sample",
#'   "prod_sample_20160129/B02")
#' global_options <- getOption("global_options")
#' prod_by_types <- read_prod_group(production_dir = production_dir,
#'   production_file = global_options$thermal_production_per_country)
read_prod_group <- function(production_dir = NULL, production_file = NULL){
  stopifnot(dir.exists(production_dir), file.exists(production_file))

  id_vars <- c("DateTime", "AreaTypeCode", "AreaName", "MapCode",
               "PowerSystemResourceName", "ProductionTypeName")
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
  setnames(data, "PowerSystemResourceName", "group_name")

  data$observed <- TRUE

  data <- ref_join_class(x = data, classobj= "on_ctry_dates_prod_group",
                         date_time = time_vars, production_file)

  data$observed[is.na(data$observed)] <- FALSE

  setDF(data)
  class(data) <- c(class(data), "prod_by_group" )
  attr( data, "id.vars") <- c("MapCode", "production_type", "group_name")
  attr( data, "timevar") <- "DateTime"
  attr( data, "countryvar") <- "country"
  attr( data, "production_file") <- production_file
  attr( data, "label") <- "Installed Generation Capacity [14.1]"

  data
}



#' @export
#' @rdname augment_validation
augment_validation.prod_by_group <- function( data ){

  load_options <- getOption("prod_options")

  val_rules <- load_options$validate$groupes_raw$validate
  fp_rules <- load_options$validate$groupes_raw$false_pos

  add_validation_columns( val_rules_file = val_rules,
                          falsepos_rules_file = fp_rules,
                          data = data )
}

