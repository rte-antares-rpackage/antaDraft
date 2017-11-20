#' @importFrom yaml yaml.load_file
#' @importFrom purrr map_df
#' @importFrom tibble tibble
get_rules <- function(add_complex = FALSE){

  frules <- system.file(package = "entsoe", "templates/cty_rules.yaml")

  cty_rules <- yaml.load_file(frules)

  ref_mapcode <- map_df(cty_rules, function(x) {
    rbind(
      tibble(MapCode = x$CTY, AreaTypeCode = "CTY"),
      tibble(MapCode = x$CTA, AreaTypeCode = "CTA"),
      tibble(MapCode = x$BZN, AreaTypeCode = "BZN") )
  }, .id = "country")

  data <- within(ref_mapcode, {
    simple_type = !grepl("^[!]{0,1}(CTA|CTY|BZN)\\|", MapCode)
    prod = ifelse( grepl("^!", MapCode), -1, 1)
    rel = gsub("^[!]{0,1}(CTA|CTY|BZN)\\|(.*)$", "\\1", MapCode)
    rel_ctry = gsub("^[!]{0,1}(CTA|CTY|BZN)\\|(.*)$", "\\2", MapCode)
    MapCode = ifelse(simple_type, MapCode, NA)
    rel = ifelse(simple_type, NA, rel)
    rel_ctry = ifelse(simple_type, NA, rel_ctry)
  })

  if( !add_complex ){
    data <- data[data$simple_type,]
  }
  data

}


#' @export
#' @title import load data from an entsoe repository
#' @description import csv data representing load data
#' from an entsoe repository.
#' @param data_dir datasets directory
#' @importFrom purrr map_df
#' @importFrom dplyr left_join
#' @importFrom lubridate minute
#' @importFrom tidyr complete
#' @examples
#' if( dir.exists( Sys.getenv("LOAD_DIR") ) )
#'   load_data <- anta_load_read( data_dir = Sys.getenv("LOAD_DIR") )
anta_load_read <- function( data_dir = NULL ){
  stopifnot(dir.exists(data_dir))

  agg_files <- list.files(data_dir, pattern = "(\\.csv)$", full.names = TRUE)
  data <- map_df(agg_files, function(f){
    read_load_file(f)
  })
  data <- data[minute( data$DateTime ) < 1, ]

  dimensions <- get_rules( add_complex = FALSE )
  dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]
  raw_db <- left_join(data, dimensions, by = c("MapCode", "AreaTypeCode"))
  raw_db <- as.data.frame(raw_db)

  {
    raw_db$SubmissionTS <- NULL
    raw_db$year <- NULL
    raw_db$month <- NULL
    raw_db$day <- NULL
  }
  raw_db$observed <- TRUE

  raw_db <- complete(raw_db, DateTime, country, fill = list(observed = FALSE ))

  class(raw_db) <- c(class(raw_db), "raw_level" )
  attr( raw_db, "id.vars") <- c("country", "MapCode", "AreaTypeCode", "DateTime", "AreaName")
  attr( raw_db, "timevar") <- "DateTime"


  raw_db
}

