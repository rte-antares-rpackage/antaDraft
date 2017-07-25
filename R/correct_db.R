#' @export
#' @title correct a data frame
#' @description Correction of a data frame with a set of yaml rules
#' @param db data to be eventually modified
#' @param v_rules yaml file containing rules to evaluate for quality evaluation
#' @param c_rules yaml file containing rules for corrections
#' @importFrom yaml yaml.load_file
correct_db <- function( db, v_rules, c_rules ){
  quality <- eval_quality(db, v_rules )
  auto_correct <- yaml.load_file( c_rules )
  false_cond <- !quality[[auto_correct$when_false]]
  true_cond <- Reduce("&" , quality[auto_correct$when_true])
  country_cond <- quality$country %in% auto_correct$country
  db[false_cond & true_cond & country_cond, auto_correct$replace ] <- db[false_cond & true_cond & country_cond, auto_correct$use ]
  db
}
