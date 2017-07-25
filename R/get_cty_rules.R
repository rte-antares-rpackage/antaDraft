#' @importFrom yaml yaml.load_file
get_cty_rules <- function( file_rules = NULL ){
  if( is.null(file_rules) )
    file_rules <- system.file(package = "antadraft", "yaml_data/cty_rules.yaml")

  yaml.load_file(file_rules)
}

