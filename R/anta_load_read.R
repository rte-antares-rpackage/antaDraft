#' @importFrom yaml yaml.load_file
#' @importFrom purrr map_df
get_rules <- function(add_complex = FALSE){

  load_options <- getOption("load_options")

  cty_rules <- yaml.load_file(load_options$cty_rules)

  ref_mapcode <- map_df(cty_rules, function(x) {
    rbind(
      data.frame(MapCode = x$CTY, AreaTypeCode = "CTY", stringsAsFactors = FALSE),
      data.frame(MapCode = x$CTA, AreaTypeCode = "CTA", stringsAsFactors = FALSE),
      data.frame(MapCode = x$BZN, AreaTypeCode = "BZN", stringsAsFactors = FALSE) )
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
#' @importFrom lubridate minute
#' @importFrom anytime anytime
#' @importFrom data.table fread rbindlist CJ
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
#' load_data <- anta_load_read(data_dir = load_dir )
anta_load_read <- function( data_dir = NULL ){
  stopifnot(dir.exists(data_dir))

  agg_files <- list.files(data_dir, pattern = "(\\.csv)$", full.names = TRUE)

  data <- rbindlist( lapply(agg_files, read_load_file ) )
  data <- data[, DateTime := anytime(DateTime)]
  data <- subset(data, minute( DateTime ) < 1)

  dimensions <- get_rules( add_complex = FALSE )
  dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]

  raw_db <- merge(data, dimensions, by = c("MapCode", "AreaTypeCode"), all = FALSE)

  raw_db$observed <- TRUE

  vars <- c("DateTime","country")
  raw_db <- raw_db[do.call(CJ, c(mget(vars), unique=TRUE)), on=vars]
  raw_db$observed[is.na(raw_db$observed)] <- FALSE

  raw_db <- as.data.frame(raw_db)
  class(raw_db) <- c(class(raw_db), "raw_level" )
  attr( raw_db, "id.vars") <- c("country", "MapCode", "AreaTypeCode", "DateTime", "AreaName")
  attr( raw_db, "timevar") <- "DateTime"

  raw_db
}

