#' @importFrom utils unzip
#' @export
init_antares_project <- function(path){
  f_template <- system.file(package = package_name, "etude_vide_package.zip")

  unzip(zipfile = f_template, exdir = path )
}
