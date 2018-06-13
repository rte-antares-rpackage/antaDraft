#Copyright © 2018 RTE Réseau de transport d’électricité

#' @importFrom data.table year setnames setDF setDT
#' @export
#' @title production data per types of production
#' @description import csv data representing production per types. The data
#' is read from an entsoe repository.
#' @param production_dir datasets directory of data energy productions
#' by types.
#' @param capacity_dir datasets directory of data energy capacities
#' by types.
#' @param production_file YAML production file to be used. This file
#' should contain for each country a list of production types.
#' @examples
#' production_dir <- system.file(package = "antaDraft", "data_sample",
#'   "prod_sample_20160129/B01")
#' capacity_dir <- system.file(package = "antaDraft", "data_sample",
#'   "prod_sample_20160129/B06")
#' global_options <- getOption("global_options")
#' prod_by_types <- read_prod_type(production_dir = production_dir,
#'   capacity_dir = capacity_dir,
#'   production_file = global_options$thermal_production_per_country)
#'
#' # Existing lists of production types -----
#' system.file(package = "antaDraft", "config/global/thermal_production.yml")
#' system.file(package = "antaDraft", "config/global/hps_production.yml")
#' system.file(package = "antaDraft", "config/global/thermal_production.yml")
read_prod_type <- function(production_dir = NULL, capacity_dir = NULL, production_file = NULL){
  stopifnot(dir.exists(production_dir), dir.exists(capacity_dir), file.exists(production_file))

  id_vars <- c("DateTime", "AreaTypeCode", "AreaName", "MapCode", "ProductionType_Name")
  time_vars <- "DateTime"
  submission_time_var <- "SubmissionTS"
  data <- entsoe_dir_reader(dir = production_dir, datetime_col = time_vars,
                            submissin_col = submission_time_var,
                            drops = c("year", "month", "day"),
                            id_vars = id_vars)

  setnames(data, "ProductionType_Name", "production_type")
  setnames(data, "ActualConsumption","consumption")
  setnames(data, "ActualGenerationOutput","generation_output")
  data$observed <- TRUE

  data <- ref_join_class(x = data, classobj= "on_ctry_dates_prod_type",
                         date_time = time_vars, production_file)

  data$observed[is.na(data$observed)] <- FALSE

  capacity_channel <- read_prod_capacity(data_dir = capacity_dir, join_class= "on_ctry_dates_prod_type", production_file )

  capacity_channel$year_date <- year(capacity_channel$DateTime)
  capacity_channel$DateTime <- NULL

  data$year_date <- year(data$DateTime)
  data <- merge( x = data, y = capacity_channel,
                 by = c("year_date", "MapCode", "AreaTypeCode",
                        "production_type"),
                 all.x = TRUE, all.y = FALSE )
  data$year_date <- NULL

  setDF(data)

  class(data) <- c(class(data), "prod_by_type" )
  attr( data, "id.vars") <- c("MapCode", "AreaTypeCode", "production_type")
  attr( data, "timevar") <- "DateTime"
  attr( data, "countryvar") <- "country"
  attr( data, "production_file") <- production_file
  attr( data, "label") <- "Installed Generation Capacity [14.1]"

  data
}

#' @export
#' @rdname augment_validation
augment_validation.prod_by_type <- function( data ){

  data_options <- getOption("prod_options")

  val_rules <- data_options$validate$raw$validate
  fp_rules <- data_options$validate$raw$false_pos

  add_validation_columns( val_rules_file = val_rules,
                          falsepos_rules_file = fp_rules,
                          data = data )
}



#' @export
#' @rdname agg_data
agg_data.prod_by_type <- function(x, ...){

  meta <- new_df_meta()
  dimensions <- get_ctry_rules(add_complex = TRUE )
  measures <- unique(dimensions[["AreaTypeCode"]])

  out <- as.data.table(x)

  #if we have no PSP then we can add prod and consumption together
  if(!("Hydro Pumped Storage" %in% unique(out$production_type))){

    out$y <- out$generation_output + out$consumption
  }else{
    #if we have PSP, we must compute psp correctly
    out[production_type=="Hydro Pumped Storage", y:=generation_output-consumption]
    out[production_type!="Hydro Pumped Storage", y:=generation_output+consumption]
  }

  out <- out[, list(y = sum(y, na.rm = FALSE) ), by=c("country", "AreaTypeCode", "production_type", "DateTime")]

  add_db <- cyclic_dataset(out, y = "y",
                           gp_col = c("DateTime", "production_type"),
                           group_col = c("country"),
                           measures_col = "AreaTypeCode" )

  out <- rbind(out, add_db)

  out <- out[, list(y = sum(y, na.rm = FALSE) ),
             by=c("country", "AreaTypeCode", "production_type", "DateTime")]

  out <- dcast(out, country + DateTime + production_type ~ AreaTypeCode,
               value.var = "y",
               fun.aggregate = sum, na.rm = FALSE)
  out <- ref_join_class(x = out, classobj = "agg_ctry_dates_prod_type", date_time = "DateTime",
                        prod_file_yaml = attr(x, "production_file") )

  meta <- add_df_meta(meta, "id.vars", c("production_type"))
  meta <- add_df_meta(meta, "timevar", c("DateTime"))
  meta <- add_df_meta(meta, "measures", measures )
  meta <- add_df_meta(meta, "countryvar", "country" )
  meta <- add_df_meta(meta, "production_file", attr(x, "production_file") )
  restore_df_meta(out, meta = meta, new_class = "aggprod_by_type" )
}

#' @export
#' @rdname augment_validation
augment_validation.aggprod_by_type <- function( data ){

  data_options <- getOption("prod_options")

  val_rules <- data_options$validate$agg$validate
  fp_rules <- data_options$validate$agg$false_pos

  add_validation_columns( val_rules_file = val_rules,
                          falsepos_rules_file = fp_rules,
                          data = data )
}

