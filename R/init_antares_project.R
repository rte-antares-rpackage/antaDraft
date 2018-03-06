#' @importFrom utils unzip
#' @importFrom antaresEditObject createArea readIniFile writeIni
#' @importFrom antaresRead setSimulationPath getAreas
#' @export
#' @title create an empty antares project
#' @description create an empty antares project to be then
#' enriched with data.
#' @param path folder to be created
#' @examples
#' init_antares_project("demo_proj")
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
  if( !casefold(ctry, upper = FALSE) %in% getAreas() )
    createArea(ctry, overwrite = FALSE)
  invisible()
}

#' @export
#' @importFrom data.table fwrite
#' @param data dataset
#' @param start_time,end_time time limits (posixct)
#' @rdname init_antares_project
add_load_to_project <- function(data, start_time, end_time){
  data <- data[data$DateTime >= start_time & data$DateTime <= end_time,]

  for( ctry in unique(data$country) ){
    curr_data <- data[data$country %in% ctry, "CTY", drop = FALSE]
    if( nrow(curr_data) > 8760 ) stop("can not write more than 8760 rows", call. = FALSE)
    if( any( is.na(curr_data$CTY) ) )
      stop(ctry, " has NA values")

    create_area_if_necessary(ctry)

    curr_data <- rbind( curr_data, data.frame( CTY = rep( 0, 8760 - nrow(curr_data) ) ) )
    filename <- sprintf("load_%s.txt", casefold(ctry, upper = TRUE) )
    filename <- file.path(getOption("antares")$studyPath, "input/load/series", filename)
    fwrite(curr_data, file = filename, sep = "\t", col.names = FALSE, dateTimeAs = "write.csv")
  }

  invisible()
}


#' @export
#' @rdname init_antares_project
add_wind_to_project <- function(data, start_time, end_time){
  newdata <- data[data$DateTime >= start_time & data$DateTime <= end_time,]
  setDT(newdata)
  newdata <- newdata[grepl("^wind", production_type, ignore.case = TRUE),]
  newdata <- newdata[, list(CTY = sum(CTY, na.rm = FALSE) ),
           by=c("country", "DateTime")]
  setDF(newdata)
  for( ctry in unique(newdata$country) ){
    curr_data <- newdata[newdata$country %in% ctry, "CTY", drop = FALSE]
    if( nrow(curr_data) > 8760 ) stop("can not write more than 8760 rows", call. = FALSE)
    if( any( is.na(curr_data$CTY) ) )
      stop(ctry, " has NA values")
    create_area_if_necessary(ctry)
    curr_data <- rbind( curr_data, data.frame( CTY = rep( 0, 8760 - nrow(curr_data) ) ) )
    filename <- sprintf("wind_%s.txt", casefold(ctry, upper = TRUE) )
    filename <- file.path(getOption("antares")$studyPath, "input/wind/series", filename)
    fwrite(curr_data, file = filename, sep = "\t", col.names = FALSE, dateTimeAs = "write.csv")
  }

  invisible()
}

#' @export
#' @rdname init_antares_project
add_solar_to_project <- function(data, start_time, end_time){
  newdata <- data[data$DateTime >= start_time & data$DateTime <= end_time,]
  setDT(newdata)
  newdata <- newdata[grepl("^solar", production_type, ignore.case = TRUE),]
  newdata <- newdata[, list(CTY = sum(CTY, na.rm = FALSE) ),
           by=c("country", "DateTime")]
  setDF(newdata)
  for( ctry in unique(newdata$country) ){
    curr_data <- newdata[newdata$country %in% ctry, "CTY", drop = FALSE]
    if( nrow(curr_data) > 8760 ) stop("can not write more than 8760 rows", call. = FALSE)
    if( any( is.na(curr_data$CTY) ) )
      stop(ctry, " has NA values")
    create_area_if_necessary(ctry)
    curr_data <- rbind( curr_data, data.frame( CTY = rep( 0, 8760 - nrow(curr_data) ) ) )
    filename <- sprintf("solar_%s.txt", casefold(ctry, upper = TRUE) )
    filename <- file.path(getOption("antares")$studyPath, "input/solar/series", filename)
    fwrite(curr_data, file = filename, sep = "\t", col.names = FALSE, dateTimeAs = "write.csv")
  }

  invisible()
}


#' @export
#' @rdname init_antares_project
add_ror_to_project <- function(data, start_time, end_time){
  newdata <- data[data$DateTime >= start_time & data$DateTime <= end_time,]
  setDT(newdata)
  newdata <- newdata[grepl("^hydro", production_type, ignore.case = TRUE),]
  newdata <- newdata[, list(CTY = sum(CTY, na.rm = FALSE) ),
           by=c("country", "DateTime")]
  setDF(newdata)
  for( ctry in unique(newdata$country) ){
    create_area_if_necessary(ctry)
    curr_data <- newdata[newdata$country %in% ctry, "CTY", drop = FALSE]
    if( any( is.na(curr_data$CTY) ) )
      stop(ctry, " has NA values")
    if( nrow(curr_data) > 8760 ) stop("can not write more than 8760 rows", call. = FALSE)
    filename <- file.path(getOption("antares")$studyPath, "input/hydro/series", casefold(ctry, upper = FALSE), "ror.txt")
    fwrite(curr_data, file = filename, sep = "\t", col.names = FALSE, dateTimeAs = "write.csv")
  }

  invisible()
}


