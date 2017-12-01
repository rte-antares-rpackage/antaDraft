#' @export
#' @title correct datasets
#' @description correct data based on condition expressed in a yaml file.
#' @param data dataset
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
#'
#' load_data <- anta_load_read(data_dir = load_dir )
#' load_data <- augment_validation(data = load_data)
#' head(load_data)
#'
#' aggregated_db <- aggregate_with_rules(load_data)
#' aggregated_db <- augment_validation(aggregated_db)
#' aggregated_db <- data_correct_with_rules(aggregated_db)
#' head(aggregated_db)
data_correct_with_rules <- function( data ){

  load_options <- getOption("load_options")
  meta <- capture_df_meta(data)
  old_names <- names(data)
  markers_exprs_ <- mark_correct_exprs(load_options$correct)
  correct_exprs_ <- correct_exprs(load_options$correct)
  data <- within(data, eval(markers_exprs_))
  data <- within(data, eval(correct_exprs_))
  new_names <- setdiff( names(data), old_names )

  meta <- add_df_meta(meta, "corrected_markers", new_names )
  restore_df_meta(data, meta = meta, new_class = "corrected" )
}

when_false_str <- function(rules){
  str1 <- sapply(rules, function(x) {
    if( !is.null( x$when_false ) )
      paste0( "(", paste0( "!", x$when_false, collapse = " | " ), ")")
    else "TRUE"
    } )
  str1
}
when_true_str <- function(rules){
  str1 <- sapply(rules, function(x) {
    if( !is.null( x$when_true ) )
      paste0( "(", paste( x$when_true, collapse = " & " ), ")")
    else "TRUE"
  } )
  str1
}
countries_str <- function(rules){
  str1 <- sapply(rules, function(x) {
    if( !is.null( x$country ) )
      paste0( "( country %in% c(", paste( shQuote(x$country), collapse = ", " ), ") )" )
    else "TRUE"
  } )
  str1
}

correct_exprs <- function( correct_rules ){

  rules <- yaml::yaml.load_file(correct_rules)

  str1 <- when_false_str(rules)
  str2 <- when_true_str(rules)
  str3 <- countries_str(rules)
  cond_ <- sprintf( "%s & %s & %s", str1, str2, str3)

  replace_var <- sapply(rules, function(x) x$replace )
  with_var <- sapply(rules, function(x) x$use )

  exprs <- sprintf("%s <- ifelse(%s, %s, %s)", replace_var, cond_, with_var, replace_var)
  exprs <- paste0( exprs, collapse = ";\n" )
  parse(text = exprs)
}


mark_correct_exprs <- function( correct_rules ){

  rules <- yaml::yaml.load_file(correct_rules)

  str1 <- when_false_str(rules)
  str2 <- when_true_str(rules)
  str3 <- countries_str(rules)
  cond_ <- sprintf( "%s & %s & %s", str1, str2, str3)

  replace_var <- rule_names(rules)
  with_var <- sprintf("TRUE", seq_along(rules))

  init_exprs <- sprintf("%s <- FALSE", replace_var)
  exprs <- sprintf("%s <- ifelse(%s, %s, %s)", replace_var, cond_, with_var, replace_var)
  exprs <- paste0( c(init_exprs, exprs), collapse = ";\n" )
  parse(text = exprs)
}

rule_names <- function( l.){
  sprintf("rule_%04.0f_%s", seq_along(l.), sapply(l., function(x) x$replace) )
}

