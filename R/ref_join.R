ref_join_class <- function(x, classobj, ...){
  old_class <- class(x)
  class(x) <- c(classobj, old_class)
  x <- ref_join(x = x, ...)
  class(x) <- old_class
  x

}

ref_join <- function( x, ... ){
  UseMethod("ref_join")
}

ref_join.std_data <- function(x, date_time){

  dimensions <- get_ctry_rules( add_complex = FALSE )
  dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]
  dimensions$dummy_id <- 1

  ref_data <- data.table(
    DateTime = seq(min( x[[date_time]], na.rm = TRUE),
                   max( x[[date_time]], na.rm = TRUE),
                   by = "hour"))
  ref_data$dummy_id <- 1
  ref_data <- merge(ref_data, dimensions,
                    by = c("dummy_id"),
                    all = FALSE, allow.cartesian = TRUE)
  ref_data$dummy_id <- NULL
  by_vars <- intersect(names(ref_data), names(x) )

  x$observed <- TRUE
  x <- merge(ref_data, x, by = by_vars, all.x = TRUE, all.y = FALSE)
  x$observed[is.na(x$observed)] <- FALSE
  x
}


ref_join.agg_data <- function(x, date_time){

  dimensions <- get_ctry_rules( add_complex = FALSE )
  dimensions <- unique(dimensions[, c("country") ])
  dimensions$dummy_id <- 1

  ref_data <- data.table(
    DateTime = seq(min( x[[date_time]], na.rm = TRUE),
                   max( x[[date_time]], na.rm = TRUE),
                   by = "hour"))
  ref_data$dummy_id <- 1
  ref_data <- merge(ref_data, dimensions,
                    by = c("dummy_id"),
                    all = FALSE, allow.cartesian = TRUE)
  ref_data$dummy_id <- NULL
  by_vars <- intersect(names(ref_data), names(x) )
  x <- merge(ref_data, x, by = by_vars, all.x = TRUE, all.y = FALSE)
  x
}


ref_join.prod_type <- function(x, date_time){

  dimensions <- get_ctry_rules( add_complex = FALSE )
  dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]
  dimensions$dummy_id <- 1

  global_options <- getOption("global_options")
  existing_prod <- yaml.load_file(global_options$thermal_production_per_country)
  existing_prod <- rbindlist(
    lapply( existing_prod,
            function(x)
              data.frame(production_type = x, stringsAsFactors = FALSE)
    ), idcol = "country" )

  dimensions <- merge(dimensions, existing_prod, by = c("country"), all = FALSE, allow.cartesian=TRUE)


  ref_data <- data.table(
    DateTime = seq(min( x[[date_time]], na.rm = TRUE),
                   max( x[[date_time]], na.rm = TRUE),
                   by = "hour"))
  ref_data$dummy_id <- 1
  ref_data <- merge(ref_data, dimensions,
                    by = c("dummy_id"),
                    all = FALSE, allow.cartesian = TRUE)
  ref_data$dummy_id <- NULL
  by_vars <- intersect(names(ref_data), names(x) )
  x <- merge(ref_data, x, by = by_vars, all.x = TRUE, all.y = FALSE)
  x
}


ref_join.prod_capacity_type <- function(x, date_time){

  dimensions <- get_ctry_rules( add_complex = FALSE )
  dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]
  dimensions$dummy_id <- 1
  global_options <- getOption("global_options")
  existing_prod <- yaml.load_file(global_options$thermal_production_per_country)
  existing_prod <- rbindlist(
    lapply( existing_prod,
            function(x)
              data.frame(production_type = x, stringsAsFactors = FALSE)
    ), idcol = "country" )

  dimensions <- merge(dimensions, existing_prod, by = c("country"), all = FALSE, allow.cartesian=TRUE)
  year_r <- range(year(x[[date_time]]), na.rm = TRUE)

  ref_data <- data.table(
    new_datetime = seq(fastPOSIXct(sprintf("%04.0f-01-01 00:00:00.000", year_r[1]),
                                   required.components = 6, tz = "GMT"),
                       fastPOSIXct(sprintf("%04.0f-12-31 23:00:00.000", year_r[2]),
                                   required.components = 6, tz = "GMT"),
                       by = "hour"))
  date_key <- function(x) fastPOSIXct(sprintf("%04.0f-01-01 00:00:00.000", year(x)),
                                      required.components = 6, tz = "GMT")
  ref_data[, (date_time) := lapply(.SD, date_key), .SDcols = "new_datetime"]

  ref_data$dummy_id <- 1
  ref_data <- merge(ref_data, dimensions,
                    by = c("dummy_id"),
                    all = FALSE, allow.cartesian = TRUE)
  ref_data$dummy_id <- NULL
  by_vars <- intersect(names(ref_data), names(x) )

  x <- merge(ref_data, x, by = by_vars, all.x = TRUE, all.y = FALSE)
  x[[date_time]] <- NULL
  setnames(x, "new_datetime", date_time)
  x
}

ref_join.renewable_prod_type <- function(x, date_time){

  dimensions <- get_ctry_rules( add_complex = FALSE )
  dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]
  dimensions$dummy_id <- 1
  global_options <- getOption("global_options")
  existing_prod <- yaml.load_file(global_options$renewable_production_per_country)
  existing_prod <- rbindlist(
    lapply( existing_prod,
            function(x)
              data.frame(production_type = x, stringsAsFactors = FALSE)
    ), idcol = "country" )

  dimensions <- merge(dimensions, existing_prod, by = c("country"), all = FALSE, allow.cartesian=TRUE)
  year_r <- range(year(x[[date_time]]), na.rm = TRUE)

  ref_data <- data.table(
    new_datetime = seq(fastPOSIXct(sprintf("%04.0f-01-01 00:00:00.000", year_r[1]),
                                   required.components = 6, tz = "GMT"),
                       fastPOSIXct(sprintf("%04.0f-12-31 23:00:00.000", year_r[2]),
                                   required.components = 6, tz = "GMT"),
                       by = "hour"))
  date_key <- function(x) fastPOSIXct(sprintf("%04.0f-01-01 00:00:00.000", year(x)),
                                      required.components = 6, tz = "GMT")
  ref_data[, (date_time) := lapply(.SD, date_key), .SDcols = "new_datetime"]

  ref_data$dummy_id <- 1
  ref_data <- merge(ref_data, dimensions,
                    by = c("dummy_id"),
                    all = FALSE, allow.cartesian = TRUE)
  ref_data$dummy_id <- NULL
  by_vars <- intersect(names(ref_data), names(x) )

  x <- merge(ref_data, x, by = by_vars, all.x = TRUE, all.y = FALSE)
  x[[date_time]] <- NULL
  setnames(x, "new_datetime", date_time)
  x
}


ref_join.incomplete_raw_prod_type <- function(x, date_time){

  ref_data <- data.table(
    DateTime = seq(min( x[[date_time]], na.rm = TRUE),
                   max( x[[date_time]], na.rm = TRUE),
                   by = "hour"))
  ref_data$dummy_id <- 1

  global_options <- getOption("global_options")
  existing_prod <- yaml.load_file(global_options$thermal_production_per_country)
  existing_prod <- rbindlist(
    lapply( existing_prod,
            function(x)
              data.frame(production_type = x, stringsAsFactors = FALSE)
    ), idcol = "country" )
  existing_prod$dummy_id <- 1

  ref_data <- merge(ref_data, existing_prod, by = c("dummy_id"), all = FALSE, allow.cartesian=TRUE)
  ref_data$dummy_id <- NULL

  by_vars <- intersect(names(ref_data), names(x) )
  x <- merge(ref_data, x, by = by_vars, all.x = TRUE, all.y = FALSE)
  x
}

