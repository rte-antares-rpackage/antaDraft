get_rule <- function( name ){
  file_rules <- system.file(package = "antadraft", "rules.yaml")
  dat <- yaml::yaml.load_file(file_rules)
  dat$rules[[name]]
}

