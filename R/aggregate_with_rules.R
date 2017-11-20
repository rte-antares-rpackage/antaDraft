#' @export
#' @importFrom dplyr inner_join group_by summarise bind_rows ungroup distinct group_by_at
#' @importFrom tidyr spread
#' @title Aggregate raw dataset from country rules
#' @description From a raw dataset and a set of rules, aggregations are performed
#' to produce for each country and possible date points a set of three measures: CTY, CTA
#' and BZN.
#' @param x raw dataset (an object of class \code{raw_level})
#' @param file_rules yaml file containing for each country aggregation rules
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
#'
#' load_data <- anta_load_read(data_dir = load_dir )
#' load_data <- augment_validation(data = load_data)
#' head(load_data)
#'
#' aggregated_db <- aggregate_with_rules(load_data)
aggregate_with_rules <- function(x, file_rules = NULL){

  x <- x[ apply( x[, attr(x, "validators"), drop = FALSE], 1, all ) , , drop = FALSE]

  dimensions <- get_rules(add_complex = TRUE )
  cyclic_computations <- dimensions[!dimensions$simple_type,]
  cyclic_computations$id <- seq_along(cyclic_computations$country)
  pivot_data <- distinct(cyclic_computations[, c("rel_ctry", "rel", "prod", "id") ])


  out <- group_by_at(x, c("country", "AreaTypeCode", "DateTime"))
  out <- summarise(out, TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE))
  out <- as.data.frame(out)


  db <- inner_join(out, pivot_data, by = c("country"="rel_ctry", "AreaTypeCode" = "rel"))
  db$TotalLoadValue <- db$TotalLoadValue * db$prod
  db <- group_by_at(db, c("id", "DateTime"))
  db <- summarise(db, TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE))

  add_db <- inner_join(ungroup(db),
             cyclic_computations[, c("country", "AreaTypeCode", "id") ],
             by = "id" )
  add_db <- as.data.frame(add_db)
  add_db$id <- NULL

  out <- bind_rows(out, add_db)
  out <- group_by_at(out, c("country", "AreaTypeCode", "DateTime"))
  out <- summarise(out, TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE))
  out <- spread(out, "AreaTypeCode", "TotalLoadValue")

  all_comb <- expand.grid( country = unique(out$country),
               DateTime = unique(out$DateTime), stringsAsFactors = FALSE )

  out <- left_join(all_comb, out, by = c("country", "DateTime") )
  out <- out[order(out$country, out$DateTime), ]
  out <- as.data.frame(out)
  class(out) <- c( class( out ), "aggregated" )
  attr( out, "id.vars") <- c("country", "DateTime")
  attr( out, "timevar") <- "DateTime"

  out
}

