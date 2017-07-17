#' @importFrom tibble tibble
#' @importFrom dplyr semi_join left_join select rename group_by summarise ungroup mutate
#' @importFrom tidyr gather
#' @importFrom purrr map_df
#' @export
#' @title tidy raw data
#' @description tidy data imported with function \code{read_load_files}.
#' A set of rules defined in cty_rules.yaml will be used to reshape untidy data
#' @param raw_db data returned by \code{\link{read_load_files}}
#' @param file_rules yaml file containing rules for each country
#' @section rules:
#' A set of rules is to be defined for each country. Items CTY, CTA and BZN
#' must be provided as array of MapCode to be aggregated together.
#' @examples
#' library(antadraft)
#' rep_path <- system.file(package = "antadraft", "files/load")
#' load_db <- read_load_files(rep_path)
#' db <- fortify_from_rules(raw_db = load_db)
fortify_from_rules <- function( raw_db, file_rules = NULL ){

  cty_rules <- get_cty_rules(file_rules = file_rules)
  pivot_data <- expand.grid( country = names(cty_rules),
               DateTime = seq( min(raw_db$DateTime), max(raw_db$DateTime), by = "hour" ),
               stringsAsFactors = FALSE )

  data_from_cty <- map_df(cty_rules, function(cty_rule, db){
    ref_ <- tibble(AreaTypeCode = "CTY", MapCode = cty_rule$CTY)
    semi_join(db, ref_, by = c("AreaTypeCode", "MapCode") ) %>%
      select(DateTime, TotalLoadValue ) %>% group_by(DateTime) %>%
      summarise(CTY = sum(TotalLoadValue, na.rm = TRUE)) %>%
      ungroup()
  }, db = raw_db, .id = "country")

  data_from_cta <- map_df(cty_rules, function(cty_rule, db){
    ref_ <- tibble(AreaTypeCode = "CTA", MapCode = cty_rule$CTA)
    semi_join(db, ref_, by = c("AreaTypeCode", "MapCode") ) %>%
      select(DateTime, TotalLoadValue ) %>% group_by(DateTime) %>%
      summarise(CTA = sum(TotalLoadValue, na.rm = TRUE)) %>%
      ungroup()
  }, db = raw_db, .id = "country")


  data_from_bzn <- map_df(cty_rules, function(cty_rule, db){
    ref_ <- tibble(AreaTypeCode = "BZN", MapCode = cty_rule$BZN)
    semi_join(db, ref_, by = c("AreaTypeCode", "MapCode") ) %>%
      select(DateTime, TotalLoadValue ) %>% group_by(DateTime) %>%
      summarise(BZN = sum(TotalLoadValue, na.rm = TRUE)) %>%
      ungroup()
  }, db = raw_db, .id = "country")

  pivot_data %>%
    left_join(data_from_cty, by = c("country", "DateTime")) %>%
    left_join(data_from_cta, by = c("country", "DateTime")) %>%
    left_join(data_from_bzn, by = c("country", "DateTime"))
}
