#' @export
#' @title correct datasets
#' @description correct data based on condition expressed in a yaml file.
#' @param data dataset
#' @examples
#' if( dir.exists( Sys.getenv("LOAD_DIR") ) ){
#'   load_data <- anta_load_read( data_dir = Sys.getenv("LOAD_DIR") )
#'   load_data <- augment_validation(data = load_data)
#'   agg_db <- aggregate_with_rules(load_data)
#'   agg_db <- augment_validation(agg_db)
#'   agg_db <- data_correct_with_rules(agg_db)
#' }
data_correct_with_rules <- function( data ){

  corrections_rules <- system.file(package = package_name, "agg_correct.yml")

  init_classes <- class(data)
  markers_exprs_ <- mark_correct_exprs(corrections_rules)
  correct_exprs_ <- correct_exprs(corrections_rules)
  data <- within(data, eval(markers_exprs_))
  data <- within(data, eval(correct_exprs_))

  class(data) <- c(init_classes, "corrected" )

  data

}

when_false_str <- function(rules){
  str1 <- map_chr(rules, function(x) paste0( "(", paste0( "!", x$when_false, collapse = " & " ), ")") )
  str1
}
when_true_str <- function(rules){
  str1 <- map_chr(rules, function(x) paste0( "(", paste( x$when_true, collapse = " & " ), ")") )
  str1
}
countries_str <- function(rules){
  str1 <- map_chr(rules, function(x) {
    if( length(x$country) < 1 ) "TRUE"
    else paste0( "( country %in% c(", paste( shQuote(x$country), collapse = ", " ), ") )" )
  } )
  str1
}

#' @importFrom purrr map_chr
correct_exprs <- function( correct_rules ){

  rules <- yaml::yaml.load_file(correct_rules)

  str1 <- when_false_str(rules)
  str2 <- when_true_str(rules)
  str3 <- countries_str(rules)
  cond_ <- sprintf( "%s & %s & %s", str1, str2, str3)

  replace_var <- map_chr(rules, function(x) x$replace )
  with_var <- map_chr(rules, function(x) x$use )

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

