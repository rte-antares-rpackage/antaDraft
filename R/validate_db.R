#' @importFrom validate validator confront values
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @importFrom tidyr gather
#' @export
#' @title eval validation rules against a dataset
#' @description Confront data with a set of validation rules
#' @param db data to be confronted with rules
extract_nonvalid_data <- function( db ){
  v <- validator(.file=system.file(package = "antadraft", 'validation_rules.yml') )
  all_res <- confront(db, v) %>% values() %>% as_tibble()
  all_res$DateTime <- db$DateTime
  all_res$country <- db$country
  gather( all_res, validator, value, -DateTime, -country) %>%
    group_by(country, validator) %>%
    filter(!value) %>%
    mutate(time_frame = cumsum( c( TRUE, diff(DateTime) != 1 ) ) ) %>%
    summarise(start = min(DateTime), end = max( DateTime)) %>%
    ungroup()
}

