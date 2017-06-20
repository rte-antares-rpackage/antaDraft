#' @importFrom tibble tibble
#' @importFrom dplyr semi_join left_join select rename group_by summarise ungroup mutate
#' @importFrom tidyr gather
#' @importFrom purrr map_df
#' @export
#' @title tidy raw data
#' @description tidy data imported with function \code{import_load_db}.
#' A set of rules defined in cty_rules.yaml will be used to reshape untidy data
#' @param raw_db data returned by \code{\link{import_load_db}}
build_db <- function( raw_db ){

  cty_rules <- get_cty_rules()

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

  left_join(data_from_cty, data_from_cta, by = c("country", "DateTime")) %>%
    left_join(data_from_bzn, by = c("country", "DateTime"))
}


