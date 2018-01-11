get_ref_prod_aggregated <- function( min_dt, max_dt ){

  all_datetime <- seq(min_dt, max_dt, by = "hour")
  ref_data <- data.table(DateTime = all_datetime)
  ref_data$dummy_id <- 1

  existing_prod <- yaml.load_file(system.file(package = "antaDraft", "config/production/production_types.yml"))
  existing_prod <- rbindlist(
    lapply( existing_prod,
            function(x)
              data.frame(production_type = x, stringsAsFactors = FALSE)
    ), idcol = "country" )
  existing_prod$dummy_id <- 1

  x <- merge(ref_data, existing_prod, by = c("dummy_id"), all = FALSE, allow.cartesian=TRUE)
  x$dummy_id <- 1
  x
}


#' @export
#' @title Aggregate production raw dataset from country rules
#' @description From a raw dataset and a set of rules, aggregations are performed
#' to produce for each country and possible date points a set of three measures: CTY, CTA
#' and BZN.
#' @param x raw dataset (an object of class \code{raw_channel_prod})
aggregate_prod_with_rules <- function(x){

  meta <- new_df_meta()

  x <- x[ apply( x[, attr(x, "validators"), drop = FALSE], 1, all ) , , drop = FALSE]

  dimensions <- get_rules(add_complex = TRUE )
  measures <- unique(dimensions$AreaTypeCode)
  cyclic_computations <- dimensions[!dimensions$simple_type,]
  cyclic_computations$id <- seq_along(cyclic_computations$country)
  pivot_data <- unique(cyclic_computations[, c("rel_ctry", "rel", "prod", "id") ])

  x <- as.data.table(x)
  x$y <- x$generation_output + x$consumption

  # aggregation en faisant fi de MapCode
  x <- x[, list(y = sum(y, na.rm = FALSE) ), by=c("country", "AreaTypeCode", "production_type", "DateTime")]

  # recuperer les data a tricotter
  add_x <- merge( x, pivot_data,
               by.x = c("country", "AreaTypeCode"), by.y = c("rel_ctry", "rel"),
               all = FALSE)
  add_x$y <- add_x$y * add_x$prod
  add_x <- add_x[, list(y = sum(y, na.rm = FALSE) ),
           by=c("id", "DateTime", "production_type")]

  add_x <- merge( add_x, cyclic_computations[, c("country", "AreaTypeCode", "id") ],
               by = "id", all = FALSE)
  add_x$id <- NULL

  x <- rbind(x, add_x)

  x <- x[, list(y = sum(y, na.rm = FALSE) ),
           by=c("country", "AreaTypeCode", "production_type", "DateTime")]

  x <- dcast(x, country + DateTime + production_type ~ AreaTypeCode,
                    value.var = "y", fun.aggregate = sum, na.rm = FALSE)
  x$observed <- TRUE

  vars <- c("DateTime","country", "production_type")
  ref_prod <- get_ref_prod_aggregated(min_dt = min(x$DateTime, na.rm = TRUE), max_dt = max(x$DateTime, na.rm = TRUE) )
  x <- merge( ref_prod, x, by = c("DateTime", "country", "production_type"), all.x = TRUE)
  x$observed[!x$observed %in% TRUE] <- FALSE

  meta <- add_df_meta(meta, "id.vars", c("country", "production_type", "DateTime"))
  meta <- add_df_meta(meta, "timevar", c("DateTime"))
  meta <- add_df_meta(meta, "measures", measures )
  meta <- add_df_meta(meta, "countryvar", "country" )
  restore_df_meta(x, meta = meta, new_class = "aggregated_prod" )
}

