#' @importFrom validate validator confront values voptions
#' @importFrom tibble as_tibble
#' @importFrom tidyr replace_na
#' @export
#' @title eval validation rules against a dataset
#' @description Confront data with a set of validation rules
#' @param db data to be confronted with rules
#' @param rules yaml file containing rules to evaluate
#' @param fp_rules yaml file containing false positive rules to exclude
#' a posteriori.
#' @import data.table
qualcon <- function( db, rules = system.file(package = "antadraft", 'validation_rules.yml'),
                     fp_rules = system.file(package = "antadraft", 'false_positive_rules.yml')
                     ){

  v <- validator(.file = rules )
  voptions(v, raise='all', na.value = FALSE)# from MVDL

  all_res <- confront(db, v) %>% values()
  keep_row <- apply( all_res, 1, function( x ) !all(x) )
  all_res <- as_tibble(all_res)
  all_res$DateTime <- db$DateTime
  all_res$country <- db$country
  all_res <- all_res[keep_row, ]


  exclude_data <- yaml.load_file(fp_rules) %>%
    map_df( function(x) {
      x$stringsAsFactors <- FALSE
      out <- as_tibble( do.call( expand.grid, x) )
      names(out) <- c("alert", "country")
      out$value <- rep(FALSE, nrow(out))
      out
      } ) %>%
    as.data.table %>%
    setkeyv(c("alert", "country", "value"))

  all_res <- as.data.table(all_res) %>%
    melt.data.table(id.vars = c("DateTime", "country"), variable.name = "alert" ) %>%
    setkeyv(c("alert", "country", "value"))

  dcast.data.table(all_res[!exclude_data], DateTime + country ~ alert, fill = FALSE) %>%
    as_tibble()
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

  out <- gather( dat, validator, value, -DateTime, -country) %>%
    group_by(country, validator) %>%
    filter(!value) %>%
    mutate(time_frame = cumsum( c( TRUE, diff(DateTime) != 1 ) ) ) %>%
    group_by(country, validator, time_frame) %>%
    summarise(start = min(DateTime), end = max( DateTime)) %>%
    ungroup()
  out$time_frame <- NULL
  out
}


