#' @export
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
  out <- x[, list(y = sum(y, na.rm = FALSE) ),
                               by=c("country", "AreaTypeCode", "production_type", "DateTime")]

  add_db <- merge( out, pivot_data,
               by.x = c("country", "AreaTypeCode"),
               by.y = c("rel_ctry", "rel"),
               all = FALSE)
  add_db$y <- add_db$y * add_db$prod
  add_db <- add_db[, list(y = sum(y, na.rm = FALSE) ),
           by=c("id", "DateTime", "production_type")]

  add_db <- merge( add_db, cyclic_computations[, c("country", "AreaTypeCode", "id") ],
               by = "id", all = FALSE)
  add_db$id <- NULL

  out <- rbind(out, add_db)

  out <- out[, list(y = sum(y, na.rm = FALSE) ),
           by=c("country", "AreaTypeCode", "production_type", "DateTime")]

  out <- dcast(out, country + DateTime + production_type ~ AreaTypeCode,
                    value.var = "y", fun.aggregate = sum, na.rm = FALSE)

  vars <- c("DateTime","country", "production_type")
  out <- out[do.call(CJ, c(mget(vars), unique=TRUE)), on=vars]
  out <- setorderv(out, c("country", "production_type", "DateTime"))
  out$observed <- TRUE

  min_dt <- min( out$DateTime, na.rm = TRUE)
  max_dt <- max( out$DateTime, na.rm = TRUE)

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

  ref_data <- merge(ref_data, existing_prod, by = c("dummy_id"), all = FALSE, allow.cartesian=TRUE)
  ref_data$dummy_id <- NULL

  out <- merge( ref_data, out, by = c("DateTime", "country", "production_type"), all.x = TRUE, all.y = FALSE)
  out$observed[!out$observed %in% TRUE] <- FALSE

  meta <- add_df_meta(meta, "id.vars", c("country", "production_type", "DateTime"))
  meta <- add_df_meta(meta, "timevar", c("DateTime"))
  meta <- add_df_meta(meta, "measures", measures )
  meta <- add_df_meta(meta, "countryvar", "country" )
  restore_df_meta(out, meta = meta, new_class = "aggregated_prod" )
}

