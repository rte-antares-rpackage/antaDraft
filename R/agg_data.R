#' @export
#' @title Aggregate raw dataset from country rules
#' @description From a raw dataset and a set of rules, aggregations are performed
#' to produce for each country, possible date points (and
#' eventually other dimensions) a set of measure(s).
#' @param x raw dataset.
#' @param ... arguments to be passed to methods.
#' @examples
#' load_dir <- system.file(package = "antaDraft",
#'   "data_sample/load_sample_2017")
#'
#' load_data <- anta_load(data_dir = load_dir )
#' load_data <- augment_validation(data = load_data)
#' head(load_data)
#'
#' aggregated_db <- agg_data(load_data)
#'
#'
#' data_neg_val <- structure(list(country = rep("LUXEMBOURG", 24),
#' DateTime = structure(seq(1502582400, to = 1502665200, by = 3600),
#' class = c("POSIXct", "POSIXt")), BZN = c(346.000000000003,
#' 352.000000000003, 354.999999999998, 330.000000000002, 345.000000000003,
#' 363.999999999998, 367, 374,
#' 389.999999999997, 373, 351, 342, 339, 344, 367, 386, -36032.64,
#' 398.999999999998, 414.000000000005, 412.000000000005, 381.999999999998,
#' 351.999999999998, 336.000000000002, 332.999999999995), CTA = c(346,
#' 352, 355, 330, 345, 364, 367, 374, 390, 373, 351, 342, 339, 344,
#' 367, 386, 401, 399, 414, 412, 382, 352, 336, 333), CTY = c(346,
#' 352, 355, 330, 345, 364, 367, 374, 390, 373, 351, 342, 339, 344,
#' 367, 386, 401, 399, 414, 412, 382, 352, 336, 333)), .Names = c("country",
#' "DateTime", "BZN", "CTA", "CTY"), row.names = 179664:179687,
#' class = c("data.frame",
#' "aggregated"))
#'
#' aggregated_db <- augment_validation(data_neg_val)
#' aggregated_db[data.table::hour(aggregated_db$DateTime) %in% 18,
#'               "BZN_LAG_LT_30"]
agg_data <- function( x, ... ){
  UseMethod("agg_data")
}


#' @export
#' @rdname agg_data
#' @importFrom data.table as.data.table dcast setorderv
agg_data.raw_level <- function(x, ...){

  meta <- new_df_meta()
  dimensions <- get_ctry_rules(add_complex = TRUE )
  measures <- unique(dimensions[["AreaTypeCode"]])

  x <- x[ apply( x[, attr(x, "validators"), drop = FALSE], 1, all ) , , drop = FALSE]
  x <- as.data.table(x)
  out <- x[, list(TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE) ),
           by=c("country", "AreaTypeCode", "DateTime")]

  add_db <- cyclic_dataset(out, y = "TotalLoadValue", group_col = "country", measures_col = "AreaTypeCode" )

  out <- rbind(out, add_db)

  out <- out[, list(TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE) ),
             by=c("country", "AreaTypeCode", "DateTime")]

  out <- dcast(out, country + DateTime ~ AreaTypeCode,
               value.var = "TotalLoadValue",
               fun.aggregate = sum, na.rm = FALSE)

  out <- ref_join_class(x = out, classobj = "on_ctry_dates", date_time = "DateTime")

  meta <- add_df_meta(meta, "id.vars", c("country", "DateTime"))
  meta <- add_df_meta(meta, "timevar", c("DateTime"))
  meta <- add_df_meta(meta, "measures", measures )
  meta <- add_df_meta(meta, "countryvar", "country" )
  restore_df_meta(out, meta = meta, new_class = "aggregated" )
}






#' @export
#' @rdname agg_data
agg_data.raw_prod_type <- function(x, ...){

  meta <- new_df_meta()
  dimensions <- get_ctry_rules(add_complex = TRUE )
  measures <- unique(dimensions[["AreaTypeCode"]])

  # x <- x[ apply( x[, attr(x, "validators"), drop = FALSE], 1, all ) , , drop = FALSE]
  out <- as.data.table(x)
  out$y <- out$generation_output + out$consumption
  out <- out[, list(y = sum(y, na.rm = FALSE) ), by=c("country", "AreaTypeCode", "production_type", "DateTime")]

  add_db <- cyclic_dataset(out, y = "y",
                           gp_col = c("DateTime", "production_type"),
                           group_col = c("country"),
                           measures_col = "AreaTypeCode" )

  out <- rbind(out, add_db)

  out <- out[, list(y = sum(y, na.rm = FALSE) ),
             by=c("country", "AreaTypeCode", "production_type", "DateTime")]

  out <- dcast(out, country + DateTime + production_type ~ AreaTypeCode,
               value.var = "y",
               fun.aggregate = sum, na.rm = FALSE)

  global_options <- getOption("global_options")
  out <- ref_join_class(x = out, classobj = "on_ctry_dates_prod_type", date_time = "DateTime", global_options$thermal_production_per_country)

  # out <- ref_join_class(x = out, classobj = "incomplete_raw_prod_type", date_time = "DateTime")

  meta <- add_df_meta(meta, "id.vars", c("country", "production_type", "DateTime"))
  meta <- add_df_meta(meta, "timevar", c("DateTime"))
  meta <- add_df_meta(meta, "measures", measures )
  meta <- add_df_meta(meta, "countryvar", "country" )
  restore_df_meta(out, meta = meta, new_class = "aggregated_prod" )
}

#' @export
#' @rdname agg_data
agg_data.raw_prod_renewable_type <- function(x, ...){

  meta <- new_df_meta()
  dimensions <- get_ctry_rules(add_complex = TRUE )
  measures <- unique(dimensions[["AreaTypeCode"]])

  # x <- x[ apply( x[, attr(x, "validators"), drop = FALSE], 1, all ) , , drop = FALSE]
  out <- as.data.table(x)
  out$y <- out$generation_output + out$consumption
  out <- out[, list(y = sum(y, na.rm = FALSE) ), by=c("country", "AreaTypeCode", "production_type", "DateTime")]

  add_db <- cyclic_dataset(out, y = "y",
                           gp_col = c("DateTime", "production_type"),
                           group_col = c("country"),
                           measures_col = "AreaTypeCode" )

  out <- rbind(out, add_db)

  out <- out[, list(y = sum(y, na.rm = FALSE) ),
             by=c("country", "AreaTypeCode", "production_type", "DateTime")]

  out <- dcast(out, country + DateTime + production_type ~ AreaTypeCode,
               value.var = "y",
               fun.aggregate = sum, na.rm = FALSE)

  global_options <- getOption("global_options")
  out <- ref_join_class(x = out, classobj = "on_ctry_dates_prod_type", date_time = "DateTime", global_options$renewable_production_per_country)

  meta <- add_df_meta(meta, "id.vars", c("country", "production_type", "DateTime"))
  meta <- add_df_meta(meta, "timevar", c("DateTime"))
  meta <- add_df_meta(meta, "measures", measures )
  meta <- add_df_meta(meta, "countryvar", "country" )
  restore_df_meta(out, meta = meta, new_class = "aggregated_renewable_prod" )
}



cyclic_dataset <- function(x, y = "TotalLoadValue",
                           gp_col = "DateTime",
                           group_col = "country",
                           measures_col = "AreaTypeCode" ){
  dimensions <- get_ctry_rules(add_complex = TRUE )
  measures <- unique(dimensions[[measures_col]])

  cyclic_computations <- dimensions[!dimensions$simple_type,]
  cyclic_computations$id <- seq_along(cyclic_computations[[group_col]])
  cyclic_computations <- as.data.frame(cyclic_computations)

  pivot_data <- unique(cyclic_computations[, c("rel_ctry", "rel", "prod", "id") ])
  pivot_data <- as.data.frame(pivot_data)

  add_db <- merge( as.data.table(x), pivot_data,
                   by.x = c(group_col, measures_col),
                   by.y = c("rel_ctry", "rel"),
                   all = FALSE)
  add_db[[y]] <- add_db[[y]] * add_db$prod
  setnames(add_db, y, "TotalLoadValue")

  # na.rm = TRUE pour noter les aggregations avec donnees manquantes
  add_db <- add_db[, list(TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE) ),
                   by=c("id", gp_col)]
  setnames(add_db, "TotalLoadValue", y)

  add_db <- merge( add_db, cyclic_computations[, c(group_col, measures_col, "id") ],
                   by = "id", all = FALSE)
  add_db$id <- NULL

  add_db
}

