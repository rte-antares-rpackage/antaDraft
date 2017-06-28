#' @importFrom validate validator confront values voptions
#' @importFrom tibble as_tibble
#' @importFrom tidyr replace_na
#' @export
#' @title eval validation rules against a dataset
#' @description Confront data with a set of validation rules
#' @param db data to be confronted with rules
#' @param yaml_rules yaml file containing rules to evaluate
qualcon <- function( db, yaml_rules = system.file(package = "antadraft", 'validation_rules.yml') ){



  v <- validator(.file = yaml_rules )
  voptions(v,raise='all')

  all_res <- confront(db, v) %>% values()
  all_res[is.na(all_res)] <- FALSE
  keep_row <- apply( all_res, 1, function( x ) !all(x) )
  all_res <- as_tibble(all_res) %>% replace_na()
  all_res$DateTime <- db$DateTime
  all_res$country <- db$country
  all_res[keep_row, ]
}


#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by filter mutate summarise
#' @importFrom tidyr gather
#' @export
#' @title eval validation rules against a dataset
#' @description Confront data with a set of validation rules
#' @param dat errors data.frame to be summarised. It should be a
#' data.frame returned by the call to \code{\link{qualcon}}
fortify_qualcon <- function( dat ){
  gather( dat, validator, value, -DateTime, -country) %>%
    group_by(country, validator) %>%
    filter(!value) %>%
    mutate(time_frame = cumsum( c( TRUE, diff(DateTime) != 1 ) ) ) %>%
    group_by(country, validator, time_frame) %>%
    summarise(start = min(DateTime), end = max( DateTime)) %>%
    ungroup()
}

