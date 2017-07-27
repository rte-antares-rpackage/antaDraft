eval_quality <- function( db, rules = system.file(package = "antadraft", 'yaml_data/validate/validation_rules.yml') ){
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


#' @importFrom validate validator confront values voptions
#' @importFrom tibble as_tibble
#' @importFrom tidyr replace_na
#' @importFrom dplyr anti_join semi_join
#' @importFrom purrr map map_df
#' @export
#' @title eval validation rules against a dataset
#' @description Confront data with a set of validation rules
#' @param db data to be confronted with rules
#' @param rules yaml file containing rules to be used to validate data
#' @param fp_rules yaml file containing false positive rules to exclude
#' a posteriori.
#' @import data.table
#' @examples
#' library(antadraft)
#' rep_path <- system.file(package = "antadraft", "files/load")
#' load_db <- read_load_files(rep_path)
#' db <- fortify_from_rules(raw_db = load_db)
#' db_errors <- qualcon(db)
qualcon <- function( db, rules = system.file(package = "antadraft", 'yaml_data/validate/validation_rules.yml'),
                     fp_rules = system.file(package = "antadraft", 'yaml_data/validate/false_positive_rules.yml')
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

#' produce reports for each country and error type
#'
#' @param error_sum result returned by function \code{fortify_qualcon}
#' @param raw_data result returned by function \code{read_load_files}
#' @param yaml_v_rules yaml file containing rules to be used to validate data
#' @param dir directory where to write reports
#' @importFrom rmarkdown render
#' @importFrom R.utils getAbsolutePath
#' @importFrom rlang syms
#' @importFrom tidyr nest
#' @importFrom dplyr group_by right_join mutate
#' @export
report_errors_summary <- function( error_sum, raw_data, yaml_v_rules = NULL, dir = getwd() ){
  dir <- getAbsolutePath(dir)
  if( !dir.exists(dir) ){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }


  if( is.null(yaml_v_rules))
    yaml_v_rules <- system.file(package = "antadraft", "yaml_data/validate/validation_rules.yml" )
  reference_errors <- yaml.load_file(yaml_v_rules)$rules %>%
    map_df(function(x) {
      tibble(validator = x$name, id = paste(x$var, collapse = ", ") )
    })

  myerrors <- error_sum %>%
    mutate(period = ifelse(end - start>0, TRUE, FALSE) ) %>%
    group_by(!!!syms(c("country", "validator")) ) %>%
    nest() %>%
    right_join(reference_errors, by = "validator")

  report_files <- list()
  rmd_file <- system.file(package = "antadraft", "template_rapport1.Rmd" )
  for(i in seq_len( nrow(myerrors) ) ){
    csv_data <- raw_data[ !raw_data[, myerrors[i, ]$"validator"] & raw_data$country %in% myerrors[i, ]$"country", ]

    par <- list( country = myerrors[i, "country"],
                 id = myerrors[i, "id"],
                 title = myerrors[i, "validator"],
                 data = myerrors[[i, "data"]] )
    outfile <- paste0(par$country, "_[" , par$title, "].html" )
    outfile <- file.path(dir, outfile)

    outcsv <- paste0(par$country, "_[" , par$title, "].csv" )
    outcsv <- file.path(dir, outcsv)
    write.csv2(csv_data, file = outcsv)
    par$csv <- basename(outcsv)

    render(rmd_file, params = par, output_file = outfile )
    report_files <- append( report_files, list(outfile) )
  }
  unlist(report_files)
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
