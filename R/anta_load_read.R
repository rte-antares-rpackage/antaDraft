#' @importFrom yaml yaml.load_file
get_rules <- function(add_complex = FALSE){

  load_options <- getOption("load_options")

  cty_rules <- yaml.load_file(load_options$cty_rules)

  cty_rules <- lapply( cty_rules, function(x) {
    rbind(
      data.frame(MapCode = x$CTY, AreaTypeCode = "CTY", stringsAsFactors = FALSE),
      data.frame(MapCode = x$CTA, AreaTypeCode = "CTA", stringsAsFactors = FALSE),
      data.frame(MapCode = x$BZN, AreaTypeCode = "BZN", stringsAsFactors = FALSE) )
  } )
  ref_mapcode <- rbindlist(cty_rules, idcol = "country")

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
#' @importFrom lubridate minute
#' @importFrom data.table fread rbindlist CJ data.table
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
#' load_data <- anta_load_read(data_dir = load_dir )
anta_load_read <- function( data_dir = NULL ){
  stopifnot(dir.exists(data_dir))

  agg_files <- list.files(data_dir, pattern = "(\\.csv)$", full.names = TRUE)

  data <- rbindlist( lapply(agg_files, read_load_file ) )
  data <- data[, DateTime := as.POSIXct(DateTime)]
  data <- subset(data, minute( DateTime ) < 1)

  dimensions <- get_rules( add_complex = FALSE )
  dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]

  all_datetime <- seq(min( data$DateTime, na.rm = TRUE),
           max( data$DateTime, na.rm = TRUE), by = "hour")
  ref_data <- data.table(DateTime = all_datetime)
  ref_data$dummy_id <- 1
  dimensions_dummy <- dimensions
  dimensions_dummy$dummy_id <- 1
  ref_data <- merge(ref_data, dimensions_dummy, by = c("dummy_id"), all = FALSE, allow.cartesian = TRUE)
  ref_data$dummy_id <- NULL

  raw_db <- merge(data, dimensions, by = c("MapCode", "AreaTypeCode"), all.x = FALSE, all.y = TRUE)
  raw_db$AreaName <- NULL
  raw_db <- merge(ref_data, raw_db, by = c("DateTime", "MapCode", "AreaTypeCode", "country"), all.x = TRUE, all.y = TRUE)
  raw_db <- subset(raw_db, !is.na(DateTime))
  raw_db$observed <- TRUE

  vars <- c("DateTime","country")
  raw_db <- raw_db[do.call(CJ, c(mget(vars), unique=TRUE)), on=vars]
  raw_db$observed[is.na(raw_db$observed)] <- FALSE

  raw_db <- as.data.frame(raw_db)
  class(raw_db) <- c(class(raw_db), "raw_level" )
  attr( raw_db, "id.vars") <- c("country", "MapCode", "AreaTypeCode", "DateTime")
  attr( raw_db, "timevar") <- "DateTime"
  attr( raw_db, "countryvar") <- "country"

  raw_db
}

