#' @importFrom tibble tibble
#' @importFrom dplyr semi_join bind_rows inner_join
#' @importFrom purrr map_df
#' @export
#' @title extract raw data to be linked with errors diagnostics
#' @description extract raw data to be linked with errors diagnostics
#' @param raw_db data returned by \code{\link{read_load_files}}
#' @param issues_db data returned by \code{\link{qualcon}}
extract_raw_data <- function( raw_db, issues_db ){

  cty_rules <- get_cty_rules()

  data_from_cty <- map_df(cty_rules, function(cty_rule, db){
    ref_ <- tibble(AreaTypeCode = "CTY", MapCode = cty_rule$CTY)
    semi_join(db, ref_, by = c("AreaTypeCode", "MapCode") )
  }, db = raw_db, .id = "country")

  data_from_cta <- map_df(cty_rules, function(cty_rule, db){
    ref_ <- tibble(AreaTypeCode = "CTA", MapCode = cty_rule$CTA)
    semi_join(db, ref_, by = c("AreaTypeCode", "MapCode") )
  }, db = raw_db, .id = "country")

  data_from_bzn <- map_df(cty_rules, function(cty_rule, db){
    ref_ <- tibble(AreaTypeCode = "BZN", MapCode = cty_rule$BZN)
    semi_join(db, ref_, by = c("AreaTypeCode", "MapCode") )
  }, db = raw_db, .id = "country")


  raw_data <- bind_rows(data_from_cty, data_from_cta, data_from_bzn )

  alert_summary <- gather( issues_db, alert, value, - DateTime, -country)
  alert_summary <- group_by( alert_summary[!alert_summary$value,], DateTime, country )
  alert_summary <- summarise(alert_summary, alert = paste(alert, collapse = ", "))
  raw_data <- raw_data %>% inner_join(alert_summary, by = c("DateTime", "country") )
  raw_data <- raw_data %>% inner_join(issues_db, by = c("DateTime", "country") )
  raw_data
}


