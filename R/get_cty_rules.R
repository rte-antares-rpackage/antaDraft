#' @importFrom yaml yaml.load_file
get_cty_rules <- function( ){
  file_rules <- system.file(package = "antadraft", "cty_rules.yaml")
  yaml.load_file(file_rules)
}

