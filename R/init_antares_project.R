#' @importFrom utils unzip
#' @importFrom antaresEditObject createArea readIniFile writeIni
#' @importFrom antaresRead setSimulationPath
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

#' @export
#' @importFrom data.table fwrite
add_load_to_project <- function(data, start_time, end_time){
  data <- data[data$DateTime >= start_time & data$DateTime <= end_time,]
  for( ctry in unique(data$country) ){
    createArea(ctry, overwrite = TRUE)
    curr_data <- data[data$country %in% ctry, c("DateTime", "CTY")]
    if( nrow(curr_data) > 8760 ) stop("can not write more than 8760 rows", call. = FALSE)
    filename <- sprintf("load_%s.txt", casefold(ctry, upper = TRUE) )
    filename <- file.path(getOption("antares")$studyPath, "input/load/series", filename)
    print(filename)
    fwrite(curr_data, file = filename, sep = "\t", col.names = FALSE)
  }

  invisible()
}


