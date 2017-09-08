#' @export
#' @importFrom dplyr inner_join group_by summarise bind_rows ungroup
#' @importFrom data.table dcast.data.table
#' @title Aggregate raw dataset from country rules
#' @description From a raw dataset and a set of rules, aggregations are performed
#' to produce for each country and possible date points a set of three measures: CTY, CTA
#' and BZN.
#' @param x raw dataset (an object of class \code{raw_level})
#' @param file_rules yaml file containing for each country aggregation rules
#' @examples
#' data(load_example)
#' cty_rules <- system.file(package = "entsoe", "templates/cty_rules.yaml")
#' val_rules <- system.file(package = "entsoe", "templates/raw/validate.yml")
#' fp_rules <- system.file(package = "entsoe", "templates/raw/false_positives.yml")
#' raw_db <- augment_rules(load_example, file_rules = cty_rules)
#' raw_db <- augment_validation(data = raw_db, val_rules = val_rules,
#'   fp_rules = fp_rules)
#'
#' agg_data <- aggregate_with_rules(raw_db)
aggregate_with_rules <- function(x, file_rules = NULL){
  dimensions <- get_rules(frules = file_rules, add_complex = TRUE )
  cyclic_computations <- dimensions[!dimensions$simple_type,]
  cyclic_computations$id <- seq_along(cyclic_computations$country)
  pivot_data <- distinct(cyclic_computations[, c("rel_ctry", "rel", "prod", "id") ])

  # db <- inner_join(x, pivot_data, by = c("country"="rel_ctry", "AreaTypeCode" = "rel"))
  # db$TotalLoadValue <- db$TotalLoadValue * db$prod
  # db <- group_by(db, !!!syms(c("id", "DateTime")))
  # db <- summarise(db, TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE))
  #
  # add_db <- inner_join(ungroup(db),
  #            cyclic_computations[, c("country", "AreaTypeCode", "id") ],
  #            by = "id" )
  # add_db <- as.data.frame(add_db)
  # add_db$id <- NULL

  gb_ <- syms(c("country", "AreaTypeCode", "DateTime"))

  out <- group_by(x, !!!gb_)
  out <- summarise(out, TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE))
  out <- as.data.frame(out)


  db <- inner_join(out, pivot_data, by = c("country"="rel_ctry", "AreaTypeCode" = "rel"))
  db$TotalLoadValue <- db$TotalLoadValue * db$prod
  db <- group_by(db, !!!syms(c("id", "DateTime")))
  db <- summarise(db, TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE))

  add_db <- inner_join(ungroup(db),
             cyclic_computations[, c("country", "AreaTypeCode", "id") ],
             by = "id" )
  add_db <- as.data.frame(add_db)
  add_db$id <- NULL

  #browser()#7452.16

  out <- bind_rows(out, add_db)
  out <- group_by(out, !!!gb_)
  out <- summarise(out, TotalLoadValue = sum(TotalLoadValue, na.rm = FALSE))
  out <- as.data.frame(out)

  out <- dcast.data.table(as.data.table(out),
                   country + DateTime ~ AreaTypeCode,
                   fill = NA, value.var = "TotalLoadValue")
  out <- as.data.frame(out)
  class(out) <- c( class( out ), "aggregated" )
  attr( out, "id.vars") <- c("country", "DateTime")
  attr( out, "timevar") <- "DateTime"

  out
}

