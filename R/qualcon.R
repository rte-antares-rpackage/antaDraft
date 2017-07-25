eval_quality <- function( db, rules = system.file(package = "antadraft", 'validation_rules.yml') ){
    v <- validator(.file = rules )
    voptions(v, raise='all', na.value = FALSE)# from MVDL
    all_res <- confront(db, v) %>% values()
    all_valid <- apply( all_res, 1, function( x ) all(x) )
    all_res <- as_tibble(all_res)
    all_res$DateTime <- db$DateTime
    all_res$country <- db$country
    all_res$is_valid <- all_valid
    all_res
}

#' @export
#' @title correct a data frame
#' @description Correction of a data frame with a set of yaml rules
#' @param db data to be eventually modified
#' @param v_rules yaml file containing rules to evaluate for quality evaluation
#' @param c_rules yaml file containing rules for corrections
#' @importFrom yaml yaml.load_file
correct_db <- function( db, v_rules, c_rules ){
  quality <- eval_quality(db, v_rules )
  auto_correct <- yaml::yaml.load_file( c_rules )
  false_cond <- !quality[[auto_correct$when_false]]
  true_cond <- Reduce("&" , quality[auto_correct$when_true])
  country_cond <- quality$country %in% auto_correct$country
  db[false_cond & true_cond & country_cond, auto_correct$replace ] <- db[false_cond & true_cond & country_cond, auto_correct$use ]
  db
}

#' @importFrom validate validator confront values voptions
#' @importFrom tibble as_tibble
#' @importFrom tidyr replace_na
#' @importFrom dplyr anti_join semi_join
#' @importFrom purrr map map_df
#' @export
#' @title eval validation rules against a dataset
#' @description Confront data with a set of validation rules
#' @param db data to be confronted with rules
#' @param rules yaml file containing rules to evaluate
#' @param fp_rules yaml file containing false positive rules to exclude
#' a posteriori.
#' @import data.table
#' @examples
#' library(antadraft)
#' rep_path <- system.file(package = "antadraft", "files/load")
#' load_db <- read_load_files(rep_path)
#' db <- fortify_from_rules(raw_db = load_db)
#' db_errors <- qualcon(db)
qualcon <- function( db, rules = system.file(package = "antadraft", 'validation_rules.yml'),
                     fp_rules = system.file(package = "antadraft", 'false_positive_rules.yml')
                     ){

  all_res <- eval_quality( db = db, rules = rules )
  keep_row <- !(all_res$is_valid)
  all_res <- all_res[keep_row, ]
  all_res$is_valid <- NULL

  all_res <- anti_cascade_errors(yaml_rules = rules, data = all_res )

  exclude_data <- yaml.load_file(fp_rules) %>%
    map_df( function(x) {
      x$stringsAsFactors <- FALSE
      out <- as_tibble( do.call( expand.grid, x) )
      out$value <- rep(FALSE, nrow(out))
      out
    } )

  # unfiltered stacked alerts ----
  all_res <- as.data.table(all_res) %>%
    melt.data.table(id.vars = c("DateTime", "country"), variable.name = "alert", variable.factor = FALSE ) %>%
    as.data.frame() %>% as_tibble()

  rows_when <- all_res %>%
    semi_join(exclude_data, by = c("country" = "country", "alert" = "when", "value" = "value" ) ) %>%
    select( DateTime, country) %>%
    mutate( value = FALSE)

  drop_data <- all_res %>%
    semi_join(rows_when, by = c("DateTime", "country", "value") ) %>%
    semi_join(exclude_data, by = c("country", "alert") )

  all_res <- anti_join(all_res, drop_data, by = c("DateTime", "country", "alert", "value"))

  dcast.data.table(as.data.table(all_res), DateTime + country ~ alert, fill = TRUE) %>%
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
#' @examples
#' library(antadraft)
#' rep_path <- system.file(package = "antadraft", "files/load")
#' load_db <- read_load_files(rep_path)
#' db <- fortify_from_rules(raw_db = load_db)
#' db_errors <- qualcon(db)
#' errors_summary <- fortify_qualcon(db_errors)
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





#' @importFrom purrr map_df
#' @importFrom yaml yaml.load_file
#' @importFrom tibble tibble
anti_cascade_errors <- function( yaml_rules, data ){
  rules <- yaml.load_file(yaml_rules)$rules

  rules_id <- map_df(rules, function(x){
    tibble( name = x$name, rule_uid = x$id)
  })

  for( rule in rules ){
    if ( is.null(rule$drop_if_not) )
      next
    column_ <- rules_id$name[rules_id$rule_uid %in% rule$id]
    columns_to_scan_4_false <- rules_id$name[rules_id$rule_uid %in% rule$drop_if_not]
    filter_vector <- lapply( data[columns_to_scan_4_false], function(x) !x)
    filter_vector <- Reduce("|", filter_vector)
    data[filter_vector,column_] <- TRUE
  }
  data
}
