globalVariables(c("AreaTypeCode", "DateTime", "TotalLoadValue", "country", ":=", "time_frame", "SubmissionTS", "y"))

package_name <- "antaDraft"

read_load_file <- function( f ){
  fread(input = f, sep = "\t",
        header = TRUE,
        colClasses = list(POSIXct = "DateTime",
                          character = c("AreaTypeCode", "AreaName", "MapCode"),
                          integer = "TotalLoadValue" ),
        drop = c("SubmissionTS", "year", "month", "day") )
}

read_prod_file <- function( f ){
  data <- fread(input = f, sep = "\t",
        header = TRUE,
        colClasses = list(POSIXct = c("DateTime", "SubmissionTS"),
                          character = c("AreaTypeCode", "AreaName", "MapCode", "ProductionType_Name"),
                          double = c("ActualConsumption", "ActualGenerationOutput" )
        ),
        drop = c("year", "month", "day") )

  data$production_type <- gsub("^\\s+|\\s+$", "", data$ProductionType_Name)
  data$ProductionType_Name <- NULL
  filter_ <- data$production_type %in% prod_type
  data <- data[ filter_, ]
  data <- data[, `:=`(DateTime = as.POSIXct(DateTime), SubmissionTS = as.POSIXct(SubmissionTS))]

  data

}

read_capacity_file <- function( f ){
  data <- fread(input = f, sep = "\t",
        header = TRUE,
        colClasses = list(POSIXct = c("DateTime", "SubmissionTS"),
                          character = c("AreaTypeCode", "AreaName", "MapCode", "ProductionType_Name"),
                          double = c("year", "AggregatedInstalledCapacity" )
        ),
        drop = c("month", "day") )
  data$production_type <- gsub("^\\s+|\\s+$", "", data$ProductionType_Name)
  data$ProductionType_Name <- NULL
  filter_ <- data$production_type %in% prod_type
  data <- data[ filter_, ]
  data <- data[, `:=`(DateTime = as.POSIXct(DateTime), SubmissionTS = as.POSIXct(SubmissionTS))]

  data

}


fp_expr <- function( fp_rules ){
  fp <- yaml::yaml.load_file(fp_rules)
  exprs <- lapply(fp, function(x){
    sprintf( "%s <- ifelse( %s == FALSE & %s == FALSE, TRUE, %s)", x$drop, paste0(x$when, collapse = "&"), x$drop, x$drop )
  })
  exprs <- unlist(exprs)
  exprs <- paste0( unlist(exprs), collapse = ";\n" )
  parse(text = exprs)
}

capture_df_meta <- function( x ){
  out <- attributes(x)
  out$names <- NULL
  out$row.names <- NULL
  out$class <- setdiff(out$class, c("data.frame", "tbl_df", "tbl", "data.table"))
  out
}

add_df_meta <- function( meta, key, value ){
  meta[[key]] <- value
  meta
}

new_df_meta <- function( ){
  meta <- list(
    class = character(0) )
  meta
}

restore_df_meta <- function( x, meta, new_class = character(0) ){
  x <- as.data.frame(x)
  class_ <- meta$class
  meta$class <- NULL
  for( att_ in names(meta) )
    attr(x, att_) <- meta[[att_]]
  class(x) <- unique(c(new_class, class_, "data.frame"))
  x
}
