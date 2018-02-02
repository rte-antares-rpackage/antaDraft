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

ref_join.raw_load <- function(x, date_time){

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

ref_join.prod_capacity_type <- function(x, date_time){

  dimensions <- get_ctry_rules( add_complex = FALSE )
  dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]
  global_options <- getOption("global_options")
  existing_prod <- yaml.load_file(global_options$thermal_production_per_country)
  existing_prod <- rbindlist(
    lapply( existing_prod,
            function(x)
              data.frame(production_type = x, stringsAsFactors = FALSE)
    ), idcol = "country" )

  ref_data <- merge(dimensions, existing_prod, by = c("country"), all = FALSE, allow.cartesian=TRUE)


  by_vars <- intersect(names(ref_data), names(x) )
  x <- merge(ref_data, x, by = by_vars, all.x = TRUE, all.y = FALSE)

  as.data.frame(x)
}



ref_join.on_ctry_dates <- function(x, date_time){

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

ref_join.on_ctry_dates_prod_type <- function(x, date_time, prod_file_yaml){

  dimensions <- get_ctry_rules( add_complex = FALSE )
  dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]
  dimensions$dummy_id <- 1

  existing_prod <- yaml.load_file(prod_file_yaml)
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
ref_join.on_ctry_dates_prod_group <- function(x, date_time, prod_file_yaml){

  dimensions <- get_ctry_rules( add_complex = FALSE )
  dimensions <- dimensions[, c("country", "MapCode", "AreaTypeCode") ]
  dimensions <- dimensions[dimensions$AreaTypeCode %in% "CTA", ]
  dimensions$dummy_id <- 1

  existing_prod <- yaml.load_file(prod_file_yaml)
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


ref_join.agg_ctry_dates_prod_type <- function(x, date_time, prod_file_yaml){

  dimensions <- get_ctry_rules( add_complex = FALSE )
  dimensions <- unique(dimensions[, c("country") ])
  dimensions$dummy_id <- 1

  existing_prod <- yaml.load_file(prod_file_yaml)
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


