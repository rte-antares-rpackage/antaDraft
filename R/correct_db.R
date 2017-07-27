#' @export
#' @title correct a data frame
#' @description Correction of a data frame with a set of yaml rules
#' @param db data to be eventually modified
#' @param v_rules yaml file containing rules to evaluate for quality evaluation
#' @param c_rules yaml file containing rules for corrections
#' @importFrom yaml yaml.load_file
correct_db <- function( db, v_rules = NULL, c_rules = NULL ){

  if( is.null(v_rules))
    v_rules = system.file(package = "antadraft", 'yaml_data/validate/validation_rules.yml')

  if( is.null(c_rules))
    stop("argument c_rules must be provided")

  if( !file.exists(v_rules) )
    stop("Could not find file for validation: ", v_rules )
  if( !file.exists(c_rules) )
    stop("Could not find file for corrections: ", v_rules )

  quality <- eval_quality(db, v_rules )
  auto_correct <- yaml.load_file( c_rules )
  false_cond <- !quality[[auto_correct$when_false]]
  true_cond <- Reduce("&" , quality[auto_correct$when_true])
  country_cond <- quality$country %in% auto_correct$country
  db[false_cond & true_cond & country_cond, auto_correct$replace ] <- db[false_cond & true_cond & country_cond, auto_correct$use ]
  db
}

