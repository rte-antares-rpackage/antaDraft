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
  if( !casefold(ctry, upper = FALSE) %in% getAreas() ){
    message("createArea for ", ctry)
    createArea(ctry, overwrite = FALSE)
  }
  invisible()
}

do_write_files <- function(data, iter_on_column = "country", y_column = "CTY",
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
  do_write_files(data = data, iter_on_column = "country", y_column = "CTY", data_path = "input/load/series", file_mask = "load_%s.txt")
  invisible()
}


#' @export
#' @rdname init_antares_project
add_wind_to_project <- function(data, start_time, end_time){
  newdata <- data[data$DateTime >= start_time & data$DateTime <= end_time,]
  setDT(newdata)
  newdata <- newdata[grepl("^wind", production_type, ignore.case = TRUE),]
  newdata <- newdata[, list(CTY = sum(CTY, na.rm = TRUE) ), by=c("country", "DateTime")]
  setDF(newdata)
  do_write_files(data = newdata, iter_on_column = "country", y_column = "CTY", data_path = "input/wind/series", file_mask = "wind_%s.txt")
  invisible()
}

#' @export
#' @rdname init_antares_project
add_solar_to_project <- function(data, start_time, end_time){
  newdata <- data[data$DateTime >= start_time & data$DateTime <= end_time,]
  setDT(newdata)
  newdata <- newdata[grepl("^solar", production_type, ignore.case = TRUE),]
  newdata <- newdata[, list(CTY = sum(CTY, na.rm = TRUE) ), by=c("country", "DateTime")]
  setDF(newdata)

  do_write_files(data = newdata, iter_on_column = "country", y_column = "CTY", data_path = "input/solar/series", file_mask = "solar_%s.txt")

  invisible()
}


#' @export
#' @rdname init_antares_project
add_ror_to_project <- function(data, start_time, end_time){
  newdata <- data[data$DateTime >= start_time & data$DateTime <= end_time,]
  setDT(newdata)
  newdata <- newdata[grepl("^hydro", production_type, ignore.case = TRUE),]
  newdata <- newdata[, list(CTY = sum(CTY, na.rm = TRUE) ),
           by=c("country", "DateTime")]
  setDF(newdata)

  do_write_files(data = newdata, iter_on_column = "country", y_column = "CTY", data_path = "input/hydro/series", file_mask = "ror_%s.txt")

  invisible()
}


