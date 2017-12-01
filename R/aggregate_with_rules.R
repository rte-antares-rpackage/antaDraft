#' @export
#' @title Aggregate raw dataset from country rules
#' @description From a raw dataset and a set of rules, aggregations are performed
#' to produce for each country and possible date points a set of three measures: CTY, CTA
#' and BZN.
#' @param x raw dataset (an object of class \code{raw_level})
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
#'
#' load_data <- anta_load_read(data_dir = load_dir )
#' load_data <- augment_validation(data = load_data)
#' head(load_data)
#'
#' aggregated_db <- aggregate_with_rules(load_data)
#' @importFrom data.table as.data.table dcast setorderv
aggregate_with_rules <- function(x){

  meta <- new_df_meta()

  x <- x[ apply( x[, attr(x, "validators"), drop = FALSE], 1, all ) , , drop = FALSE]

  dimensions <- get_rules(add_complex = TRUE )
  measures <- unique(dimensions$AreaTypeCode)

  cyclic_computations <- dimensions[!dimensions$simple_type,]
  cyclic_computations$id <- seq_along(cyclic_computations$country)
  pivot_data <- unique(cyclic_computations[, c("rel_ctry", "rel", "prod", "id") ])

  x <- as.data.table(x)
  out <- x[, list(TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE) ),
                               by=c("country", "AreaTypeCode", "DateTime")]

  add_db <- merge( out, pivot_data,
               by.x = c("country", "AreaTypeCode"),
               by.y = c("rel_ctry", "rel"),
               all = FALSE)
  add_db$TotalLoadValue <- add_db$TotalLoadValue * add_db$prod
  add_db <- add_db[, list(TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE) ),
           by=c("id", "DateTime")]

  add_db <- merge( add_db, cyclic_computations[, c("country", "AreaTypeCode", "id") ],
               by = "id", all = FALSE)
  add_db$id <- NULL

  out <- rbind(out, add_db)

  out <- out[, list(TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE) ),
           by=c("country", "AreaTypeCode", "DateTime")]

  out <- dcast(out, country + DateTime ~ AreaTypeCode,
                    value.var = "TotalLoadValue", fun.aggregate = sum, na.rm = FALSE)

  vars <- c("DateTime","country")
  out <- out[do.call(CJ, c(mget(vars), unique=TRUE)), on=vars]
  out <- setorderv(out, c("country", "DateTime"))

  meta <- add_df_meta(meta, "id.vars", c("country", "DateTime"))
  meta <- add_df_meta(meta, "timevar", c("DateTime"))
  meta <- add_df_meta(meta, "measures", measures )
  meta <- add_df_meta(meta, "countryvar", "country" )
  restore_df_meta(out, meta = meta, new_class = "aggregated" )
}

