#Copyright © 2016 RTE Réseau de transport d’électricité

#' @importFrom utils unzip
#' @importFrom antaresEditObject createArea readIniFile writeIni
#' @importFrom antaresRead setSimulationPath getAreas
#' @export
#' @title create an empty antares project
#' @description create an empty antares project to be then
#' enriched with data.
#' @param path folder to be created
#' @examples
#' \dontrun{
#' library(antaresEditObject)
#' library(magrittr)
#'
#' load_dir <- "/.../load_20180115"
#' load_data <- anta_load(data_dir = load_dir ) %>% agg_data()
#'
#' production_dir <- "/.../prod_20180115/B01"
#' capacity_dir <- "/.../prod_20180115/B06"
#'
#' global_options <- getOption("global_options")
#' p_renewable_file <- global_options$renewable_production_per_country
#'
#' PTT <- read_prod_type(
#'   production_dir = production_dir,
#'   capacity_dir = capacity_dir,
#'   production_file = p_renewable_file) %>%
#'   agg_data()
#'
#' start_time <- fasttime::fastPOSIXct("2017-01-01 00:00:00", tz = "GMT")
#' end_time <- fasttime::fastPOSIXct("2017-01-31 23:00:00", tz = "GMT")
#'
#' init_antares_project("demo_proj")
#' add_load_to_project(load_data,  start_time = start_time,  end_time = end_time )
#' add_wind_to_project(PTT, start_time = start_time, end_time = end_time )
#' add_solar_to_project(PTT, start_time = start_time, end_time = end_time )
#' add_ror_to_project(PTT, start_time = start_time, end_time = end_time )
#' add_hwr_to_project(PTT, start_time = start_time, end_time = end_time )
#' }
init_antares_project <- function(path){
  f_template <- system.file(package = package_name, "etude_vide_package.zip")

  unzip(zipfile = f_template, exdir = path )

  study.antares <- file.path(path, "study.antares")
  study.antares_meta <- readIniFile(study.antares)
  study.antares_meta$antares$caption <- basename(path)
  writeIni(study.antares_meta, pathIni = study.antares, overwrite = TRUE)

  desktop.ini <- file.path(path, "Desktop.ini")
  desktop.ini_meta <- readIniFile(desktop.ini)
  desktop.ini_meta$.shellclassinfo$infotip <- gsub("etude_vide_package", basename(path), desktop.ini_meta$.shellclassinfo$infotip)
  writeIni(desktop.ini_meta, pathIni = desktop.ini, overwrite = TRUE)

  setSimulationPath(path = path)
}

create_area_if_necessary <- function( ctry ){
  if( !casefold(ctry, upper = FALSE) %in% getAreas() ){
    message("createArea for ", ctry)
    createArea(ctry, overwrite = TRUE)
  }
  invisible()
}

do_write_files_option1 <- function(data, iter_on_column = "country", y_column = "CTY",
                           data_path, file_mask ){
  list_index <- unique(data[[iter_on_column]])
  for( ctry in list_index ){

    create_area_if_necessary(ctry)

    curr_data <- data[data[[iter_on_column]] %in% ctry, y_column, drop = FALSE]

    if( any( is.na(curr_data[[y_column]]) ) ){
      warning(ctry, " has NA values")
    }

    if( nrow(curr_data) > 8760 )
      stop("can not write more than 8760 rows", call. = FALSE)
    else if( nrow(curr_data) < 1 ) next
    else if( nrow(curr_data) < 8760 ){
      curr_data <- rbind(curr_data,
                         data.frame(CTY = rep(0, 8760 - nrow(curr_data) ))
      )
    }

    filename <- sprintf(file_mask, casefold(ctry, upper = FALSE) )
    filename <- file.path(getOption("antares")$studyPath, data_path, filename)
    fwrite(curr_data, file = filename, sep = "\t", col.names = FALSE, dateTimeAs = "write.csv")
  }
}
do_write_files_option2 <- function(data, iter_on_column = "country", y_column = "CTY",
                           data_path, file_mask ){
  list_index <- unique(data[[iter_on_column]])
  for( ctry in list_index ){

    create_area_if_necessary(ctry)

    curr_data <- data[data[[iter_on_column]] %in% ctry, y_column, drop = FALSE]

    if( any( is.na(curr_data[[y_column]]) ) ){
      warning(ctry, " has NA values")
    }

    if( nrow(curr_data) > 12 )
      stop("can not write more than 12 rows", call. = FALSE)
    else if( nrow(curr_data) < 1 ) next
    else if( nrow(curr_data) < 12 ){
      curr_data <- rbind(curr_data,
                         data.frame(CTY = rep(0, 12 - nrow(curr_data) ))
      )
    }

    filename <- sprintf(file_mask, casefold(ctry, upper = FALSE) )
    filename <- file.path(getOption("antares")$studyPath, data_path, filename)
    fwrite(curr_data, file = filename, sep = "\t", col.names = FALSE, dateTimeAs = "write.csv")
  }
}

do_write_files_psp <- function(data, iter_on_column = "country", y_column = "CTY",
                           data_path, file_mask ){
  list_index <- unique(data[[iter_on_column]])
  for( ctry in list_index ){

    create_area_if_necessary(ctry)

    curr_data <- data[data[[iter_on_column]] %in% ctry, y_column, drop = FALSE]

    if( any( is.na(curr_data[[y_column]]) ) ){
      warning(ctry, " has NA values")
    }

    if( nrow(curr_data) > 8760 )
      stop("can not write more than 8760 rows", call. = FALSE)
    else if( nrow(curr_data) < 1 ) next
    else if( nrow(curr_data) < 8760 ){
      curr_data <- rbind(curr_data,
                         data.frame(CTY = rep(0, 8760 - nrow(curr_data) ))
      )
    }
    proto_ <- rep(0, 8760)
    curr_data <- data.frame(X1 = proto_, X2 = proto_, X3 = proto_, X4 = proto_, X5 = proto_,
               X6 = proto_, CTY = curr_data$CTY, X7 = proto_ )
    filename <- sprintf(file_mask, casefold(ctry, upper = FALSE) )
    filename <- file.path(getOption("antares")$studyPath, data_path, filename)
    fwrite(curr_data, file = filename, sep = "\t", col.names = FALSE, dateTimeAs = "write.csv")
  }
}

#' @export
#' @importFrom data.table fwrite
#' @param data dataset
#' @param start_time,end_time time limits (posixct)
#' @rdname init_antares_project
add_load_to_project <- function(data, start_time, end_time){
  data <- data[data$DateTime >= start_time & data$DateTime <= end_time,]
  do_write_files_option1(data = data, iter_on_column = "country", y_column = "CTY", data_path = "input/load/series", file_mask = "load_%s.txt")
  invisible()
}


#' @export
#' @rdname init_antares_project
#' @section add_wind_to_project:
#' The function will aggregate on and off shore wind productions and generate an hourly TS.
add_wind_to_project <- function(data, start_time, end_time){
  newdata <- data[data$DateTime >= start_time & data$DateTime <= end_time,]
  setDT(newdata)
  newdata <- newdata[grepl("^wind", production_type, ignore.case = TRUE),]
  newdata <- newdata[, list(CTY = sum(CTY, na.rm = TRUE) ), by=c("country", "DateTime")]
  setDF(newdata)
  do_write_files_option1(data = newdata, iter_on_column = "country", y_column = "CTY", data_path = "input/wind/series", file_mask = "wind_%s.txt")
  invisible()
}

#' @export
#' @rdname init_antares_project
#' @section add_solar_to_project:
#' The function will aggregate solar productions and generate an hourly TS.
add_solar_to_project <- function(data, start_time, end_time){
  newdata <- data[data$DateTime >= start_time & data$DateTime <= end_time,]
  setDT(newdata)
  newdata <- newdata[grepl("^solar", production_type, ignore.case = TRUE),]
  newdata <- newdata[, list(CTY = sum(CTY, na.rm = TRUE) ), by=c("country", "DateTime")]
  setDF(newdata)

  do_write_files_option1(data = newdata, iter_on_column = "country", y_column = "CTY", data_path = "input/solar/series", file_mask = "solar_%s.txt")

  invisible()
}


#' @export
#' @rdname init_antares_project
#' @param productions production types to be summed (default to
#' c("Hydro Pumped Storage", "Hydro Water Reservoir",
#' "Hydro Run-of-river and poundage"))
#' @section add_ror_to_project:
#' The function will aggregate hydro productions type (that can specified with
#' argument \code{productions}) and generate an hourly TS.
add_ror_to_project <- function(data, start_time, end_time,
                               productions = c("Hydro Pumped Storage",
                                               "Hydro Water Reservoir",
                                               "Hydro Run-of-river and poundage") ){
  newdata <- data[data$DateTime >= start_time & data$DateTime <= end_time,]
  newdata <- newdata[newdata$production_type %in% productions,]
  setDT(newdata)
  newdata <- newdata[, list(CTY = sum(CTY, na.rm = TRUE) ),
           by=c("country", "DateTime")]
  setDF(newdata)

  do_write_files_option1(data = newdata, iter_on_column = "country", y_column = "CTY", data_path = "input/hydro/series", file_mask = "%s/ror.txt")

  invisible()
}


#' @export
#' @rdname init_antares_project
#' @section add_hwr_to_project:
#' The function will aggregate Hydro Water Reservoir production
#' and generate a monthly TS.
add_hwr_to_project <- function(data, start_time, end_time){
  newdata <- data[data$DateTime >= start_time & data$DateTime <= end_time,]
  newdata <- newdata[newdata$production_type %in% "Hydro Water Reservoir",]
  newdata$DateTimeMonth <- data.table::month(newdata$DateTime)
  newdata$DateTimeYear <- data.table::year(newdata$DateTime)
  setDT(newdata)
  newdata <- newdata[, list(CTY = sum(CTY, na.rm = TRUE) ),
           by=c("country", "DateTimeYear", "DateTimeMonth")]
  setorderv(newdata, cols = c("country", "DateTimeYear", "DateTimeMonth") )
  setDF(newdata)

  do_write_files_option2(data = newdata, iter_on_column = "country", y_column = "CTY", data_path = "input/hydro/series", file_mask = "%s/mod.txt")

  invisible()
}

#' @export
#' @rdname init_antares_project
#' @section add_hps_to_project_in_psp:
#' The function will use Hydro Pumped Storage production
#' and write a TS to "input/misc-gen" directory.
add_hps_to_project_in_psp <- function(data, start_time, end_time){
  newdata <- data[data$DateTime >= start_time & data$DateTime <= end_time,]
  newdata <- newdata[newdata$production_type %in% "Hydro Pumped Storage",]

  do_write_files_psp(data = newdata, iter_on_column = "country", y_column = "CTY", data_path = "input/misc-gen", file_mask = "miscgen-%s.txt")

  invisible()
}

#' @importFrom antaresEditObject createPSP
#' @export
#' @rdname init_antares_project
#' @param namePumping The name of the pumping area
#' @param nameTurbining The name of the turbining area
#' @param overwrite Overwrite the Pumped Storage Power plant if already exist.
#' This will overwrite the previous area and links.
#' @param efficiency The efficiency of the virtual PSP
#' @param timeStepBindConstraint Time step for the binding constraint : \code{daily} or \code{weekly}
#' @section add_hps_to_project_in_virtualPsp:
#' The function will use Hydro Pumped Storage installed capacity
#' and create Pumped Storage Power plant (PSP) with the function \code{\link[antaresEditObject]{createPSP}}
add_hps_to_project_in_virtualPsp <- function(data, namePumping="PSP_In", nameTurbining="PSP_Out", overwrite = FALSE, efficiency=NULL, timeStepBindConstraint="weekly", ...){

  correctedData <- .check_data_for_add_hps_to_project_in_virtualPsp(data)
  #get the list of countries and their step capacity
  dataDT <- as.data.table(correctedData)
  #create a PSP only if generation_output is > 0
  areasAndCapacities <- unique(dataDT[AreaTypeCode == "CTY" & production_type == "Hydro Pumped Storage"  & (installed_capacity > 0 | generation_output>0), .(country, installedCapacity)])

  for (area in areasAndCapacities$country){
    create_area_if_necessary(area)
  }

  antaresEditObject::createPSP(areasAndCapacities = areasAndCapacities, namePumping = namePumping, nameTurbining = nameTurbining, efficiency = efficiency, overwrite = overwrite)

}

.check_data_for_add_hps_to_project_in_virtualPsp <- function(data){
  #data.frame
  if (!is.data.frame(data)){
    stop("data must be a data.frame.")
  }

  #check if we have the necessary data
  if (is.null(data$installed_capacity) & is.null(data$installedCapacity) ){
    stop("data should contain installed_capacity or installedCapacity")
  }
  if (is.null(data$installed_capacity)){
    data$installed_capacity <- data$installedCapacity
  }
  if (is.null(data$installedCapacity)){
    data$installedCapacity <- data$installed_capacity
  }
  #colum
  if (is.null(data$production_type) ){
    stop("data should contain production_type")
  }
  #colum
  if (is.null(data$AreaTypeCode) ){
    stop("data should contain AreaTypeCode")
  }
  #colum
  if (!("CTY" %in% unique(data$AreaTypeCode))){
    stop("AreaTypeCode should contain CTY")
  }
  #colum
  if (is.null(data$generation_output) ){
    stop("data should contain generation_output")
  }
  #colum
  if (is.null(data$country) ){
    stop("data should contain country")
  }

  #check if we have the necessary data
  if (!("Hydro Pumped Storage" %in% unique(data$production_type))){
    stop("production_type should contain Hydro Pumped Storage", call. = FALSE)
  }

  #add a step if a capacity of Hydro Pumped Storage is > than 0
  if (max(unique(data[data$production_type == "Hydro Pumped Storage",]$installed_capacity), na.rm = TRUE)<0){
    stop("All installed_capacities are negative.", call. = FALSE)
  }

  return(data)

}
