#' @export
#' @importFrom dplyr full_join left_join
#' @title add to a raw dataset columns relative to country aggregation
#' @description add country column to a raw dataset to enable
#' calculations of CTY, CTA and BZN
#' @param raw_db raw dataset
#' @param file_rules yaml file containing for each country aggregation rules
#' @examples
#' data(load_example)
#' cty_rules <- system.file(package = "entsoe", "templates/cty_rules.yaml")
#' raw_db <- augment_rules(load_example, file_rules = cty_rules)
augment_rules <- function(raw_db, file_rules = NULL){
  dimensions <- get_rules(frules = file_rules, add_complex = FALSE )
  dimensions$dummy = 1
  dimensions$rel_ctry <- NULL
  dimensions$rel <- NULL
  dimensions$prod <- NULL
  dimensions$simple_type <- NULL

  all_dt <- tibble(
    dummy = 1,
    DateTime = seq( min( raw_db$DateTime ), max( raw_db$DateTime ), by = "hour" )
  )

  full_data <- full_join(dimensions, all_dt, by = "dummy")
  full_data$dummy <- NULL
  raw_db$observed = TRUE

  raw_db <- left_join(
    full_data, raw_db,
    by = c("MapCode" = "MapCode", "AreaTypeCode" = "AreaTypeCode", "DateTime" = "DateTime")
  )
  raw_db$observed[is.na( raw_db$observed ) ] <- FALSE

  raw_db <- as.data.frame(raw_db)
  class(raw_db) <- c(class(raw_db), "raw_level" )

  attr( raw_db, "id.vars") <- c("country", "MapCode", "AreaTypeCode", "DateTime", "year", "month", "day", "AreaName")
  attr( raw_db, "timevar") <- "DateTime"

  raw_db
}

