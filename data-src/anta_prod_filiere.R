#' @export
#' @title import production data per channels from an entsoe repository
#' @description import csv data representing production per channels data
#' from an entsoe repository.
#' @param data_dir datasets directory
#' @param utf16 whether text file are encoded in UTF16 (if not, UTF8)
#' @importFrom purrr map_df
#' @importFrom dplyr left_join
#' @importFrom tidyr complete
#' @examples
#' if( dir.exists( Sys.getenv("CHANNEL_PROD_DIR") ) )
#'   load_data <- anta_prod_channel( data_dir = Sys.getenv("CHANNEL_PROD_DIR") )
anta_prod_channel <- function( data_dir = NULL, utf16 = FALSE ){
  stopifnot(dir.exists(data_dir))

  agg_files <- list.files(data_dir, pattern = "(\\.csv)$", full.names = TRUE)
  data <- map_df(agg_files, function(f){
    read_delim(f, delim = "\t", col_types = cols(
                 year = col_skip(),
                 month = col_skip(),
                 day = col_skip(),
                 DateTime = col_datetime(format = ""),
                 AreaTypeCode = col_character(),
                 AreaName = col_character(),
                 MapCode = col_character(),
                 ProductionType_Name = col_character(),
                 ActualConsumption = col_double(),
                 ActualGenerationOutput = col_double(),
                 SubmissionTS = col_skip() )

                 )
  })
  data$production_type <- gsub("^\\s+|\\s+$", "", data$ProductionType_Name)
  data$ProductionType_Name <- NULL
  filter_ <- data$production_type %in% c(charbon="Fossil Hard coal", charbonpdtpargaz = "Fossil Coal-derived gas",
                              lignite = "Fossil Brown coal/Lignite", nucleaire = "Nuclear", diesel = "Fossil Oil",
                              gaz = "Fossil Gas", tourbe = "Fossil Peat", kerogene = "Fossil Oil shale" )
  data <- data[ filter_, ]

  dimensions <- get_rules( add_complex = FALSE )
  dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]
  data <- left_join(data, dimensions, by = c("MapCode", "AreaTypeCode"))
  data <- as.data.frame(data)

  data$observed <- TRUE

  data <- complete(data, DateTime, country, fill = list(observed = FALSE ))

  class(data) <- c(class(data), "raw_channel_prod" )
  attr( data, "id.vars") <- c("country", "MapCode", "AreaTypeCode", "DateTime", "AreaName", "production_type")
  attr( data, "timevar") <- "DateTime"

  data
}

#' @export
anta_capacity_channel <- function( data_dir = NULL, utf16 = FALSE ){
  stopifnot(dir.exists(data_dir))

  agg_files <- list.files(data_dir, pattern = "(\\.csv)$", full.names = TRUE)
  data <- map_df(agg_files, function(f){
    read_delim(f, delim = "\t", col_types = cols(
      year = col_skip(),
      month = col_skip(),
      day = col_skip(),
      DateTime = col_datetime(format = ""),
      AreaTypeCode = col_character(),
      AreaName = col_character(),
      MapCode = col_character(),
      ProductionType_Name = col_character(),
      AggregatedInstalledCapacity = col_double(),
      SubmissionTS = col_skip() ) )
  })

  data$production_type <- gsub("^\\s+|\\s+$", "", data$ProductionType_Name)
  data$ProductionType_Name <- NULL
  filter_ <- data$production_type %in% c(charbon="Fossil Hard coal", charbonpdtpargaz = "Fossil Coal-derived gas",
                              lignite = "Fossil Brown coal/Lignite", nucleaire = "Nuclear", diesel = "Fossil Oil",
                              gaz = "Fossil Gas", tourbe = "Fossil Peat", kerogene = "Fossil Oil shale" )
  data <- data[ filter_, ]

  dimensions <- get_rules( add_complex = FALSE )
  dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]
  data <- left_join(data, dimensions, by = c("MapCode", "AreaTypeCode"))
  data <- as.data.frame(data)

  data$observed <- TRUE

  data <- complete(data, DateTime, country, fill = list(observed = FALSE ))

  class(data) <- c(class(data), "raw_channel_capacity" )
  attr( data, "id.vars") <- c("country", "MapCode", "AreaTypeCode", "DateTime", "AreaName", "production_type")
  attr( data, "timevar") <- "DateTime"

  data
}

