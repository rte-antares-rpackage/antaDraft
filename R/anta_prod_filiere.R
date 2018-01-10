prod_type <- c(charbon="Fossil Hard coal", charbonpdtpargaz = "Fossil Coal-derived gas",
               lignite = "Fossil Brown coal/Lignite", nucleaire = "Nuclear", diesel = "Fossil Oil",
               gaz = "Fossil Gas", tourbe = "Fossil Peat", kerogene = "Fossil Oil shale", autre = "Other" )


get_ref_prod_full <- function( min_dt, max_dt ){

  dimensions <- get_rules( add_complex = FALSE )
  dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]
  dimensions$dummy_id <- 1

  all_datetime <- seq(min_dt, max_dt, by = "hour")
  ref_data <- data.table(DateTime = all_datetime)
  ref_data$dummy_id <- 1

  ref_data <- merge(ref_data, dimensions, by = c("dummy_id"), all = FALSE, allow.cartesian=TRUE)
  ref_data$dummy_id <- NULL

  existing_prod <- yaml.load_file(system.file(package = "antaDraft", "config/production/production_types.yml"))
  existing_prod <- rbindlist(
    lapply( existing_prod,
            function(x)
              data.frame(production_type = x, stringsAsFactors = FALSE)
    ), idcol = "country" )

  merge(ref_data, existing_prod, by = c("country"), all = FALSE, allow.cartesian=TRUE)
}


#' @importFrom data.table year setnames
#' @export
#' @title import production data per channels from an entsoe repository
#' @description import csv data representing production per channels data
#' from an entsoe repository.
#' @param production_dir datasets directory of production by type files
#' @param capacity_dir datasets directory for capacities by type files
anta_prod_channel <- function(production_dir = NULL, capacity_dir = NULL){
  stopifnot(dir.exists(production_dir))

  agg_files <- list.files(production_dir, pattern = "(\\.csv)$", full.names = TRUE)
  data <- rbindlist( lapply(agg_files, read_prod_file ) )
  data <- subset(data, minute( DateTime ) < 1)

  setnames(data, "ActualConsumption","consumption")
  setnames(data, "ActualGenerationOutput","generation_output")

  # dedup of data, choose latest SubmissionTS ---
  data <- setorderv(data, cols = c("DateTime", "AreaTypeCode", "AreaName", "MapCode", "production_type", "SubmissionTS"))
  data <- unique(data, by = c("DateTime", "AreaTypeCode", "AreaName", "MapCode", "production_type"), fromLast = TRUE )
  data$SubmissionTS <- NULL
  data$observed <- TRUE

  min_dt <- min( data$DateTime, na.rm = TRUE)
  max_dt <- max( data$DateTime, na.rm = TRUE)

  data <- merge(get_ref_prod_full( min_dt, max_dt ), data,
                by = c("DateTime", "MapCode", "AreaTypeCode", "production_type"),
                all.x = FALSE, all.y = FALSE)

  data$observed[is.na(data$observed)] <- FALSE

  capacity_channel <- anta_capacity_channel(
    data_dir = capacity_dir,
    min_dt = min_dt, max_dt = max_dt)

  data <- merge( x = data, y = capacity_channel,
                 by = c("DateTime", "MapCode", "AreaTypeCode",
                        "production_type", "AreaName", "country"),
                 all.x = FALSE, all.y = TRUE )
  data$SubmissionTS <- NULL

  data <- as.data.frame(data)

  class(data) <- c(class(data), "raw_channel_prod" )
  attr( data, "id.vars") <- c("country", "MapCode", "AreaTypeCode", "DateTime", "AreaName", "production_type")
  attr( data, "timevar") <- "DateTime"
  attr( data, "countryvar") <- "country"

  data
}

anta_capacity_channel <- function( data_dir = NULL, min_dt, max_dt ){
  stopifnot(dir.exists(data_dir))

  agg_files <- list.files(data_dir, pattern = "(\\.csv)$", full.names = TRUE)
  data <- rbindlist( lapply(agg_files, read_capacity_file ) )
  data$DateTime <- NULL

  setnames(data, "AggregatedInstalledCapacity","installed_capacity")

  ref_data <- get_ref_prod_full(min_dt = min_dt, max_dt = max_dt)
  ref_data$year <- year(ref_data$DateTime)
  data <- merge(ref_data, data,
                by = c("year", "MapCode", "AreaTypeCode", "production_type"),
                all.x = TRUE, all.y = FALSE)
  data$year <- NULL
  filter_ <- data$installed_capacity > 0
  data <- data[ filter_, ]

  data
}

