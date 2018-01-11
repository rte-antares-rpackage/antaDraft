read_prod_group_file <- function( f ){
  data <- fread(input = f, sep = "\t",
                header = TRUE,
                colClasses = list(POSIXct = c("DateTime", "SubmissionTS"),
                                  character = c("AreaTypeCode", "AreaName", "MapCode", "PowerSystemResourceName",
                                                "ProductionTypeName"),
                                  double = c("ActualConsumption", "ActualGenerationOutput", "InstalledGenCapacity" )
                ),
                drop = c("year", "month", "day") )

  data$production_type <- gsub("^\\s+|\\s+$", "", data$ProductionTypeName)
  data$ProductionTypeName <- NULL
  filter_ <- data$production_type %in% prod_type
  data <- data[ filter_, ]
  data <- data[, `:=`(DateTime = anytime(DateTime), SubmissionTS = anytime(SubmissionTS))]
  data <- subset(data, minute( DateTime ) < 1)
  data

}


#' @export
anta_prod_group <- function(production_dir = NULL){
  stopifnot(dir.exists(production_dir))

  agg_files <- list.files(production_dir, pattern = "(\\.csv)$", full.names = TRUE)
  data <- rbindlist( lapply(agg_files, read_prod_group_file ) )
  setnames(data, "ActualConsumption","consumption")
  setnames(data, "ActualGenerationOutput","generation_output")
  setnames(data, "InstalledGenCapacity","installed_capacity")

  # dedup of data, choose latest SubmissionTS ---
  data <- setorderv(data, cols = c("DateTime", "AreaTypeCode", "AreaName", "MapCode", "PowerSystemResourceName", "SubmissionTS"))
  data <- unique(data, by = c("DateTime", "AreaTypeCode", "AreaName", "MapCode", "production_type"), fromLast = TRUE )
  data$SubmissionTS <- NULL
  data$observed <- TRUE

  min_dt <- min( data$DateTime, na.rm = TRUE)
  max_dt <- max( data$DateTime, na.rm = TRUE)

  data <- merge(get_ref_prod_full( min_dt, max_dt ), data,
                by = c("DateTime", "MapCode", "AreaTypeCode", "production_type"),
                all.x = FALSE, all.y = FALSE)

  data$observed[is.na(data$observed)] <- FALSE

  data <- as.data.frame(data)

  class(data) <- c(class(data), "raw_group_prod" )
  attr( data, "id.vars") <- c("country", "MapCode", "AreaTypeCode", "DateTime", "AreaName", "production_type", "PowerSystemResourceName")
  attr( data, "timevar") <- "DateTime"
  attr( data, "countryvar") <- "country"

  data
}

