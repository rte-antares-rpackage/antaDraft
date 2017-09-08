#' @importFrom yaml yaml.load_file
#' @importFrom purrr map_df
#' @importFrom tibble tibble
get_rules <- function(frules = NULL, add_complex = FALSE){

  if( is.null(frules) )
    frules <- system.file(package = "entsoe", "templates/cty_rules.yaml")
  cty_rules <- yaml.load_file(frules)

  ref_mapcode <- map_df(cty_rules, function(x) {
    rbind(
      tibble(MapCode = x$CTY, AreaTypeCode = "CTY"),
      tibble(MapCode = x$CTA, AreaTypeCode = "CTA"),
      tibble(MapCode = x$BZN, AreaTypeCode = "BZN") )
  }, .id = "country")

  data <- within(ref_mapcode, {
    simple_type = !grepl("^[!]{0,1}(CTA|CTY|BZN)\\|", MapCode)
    prod = ifelse( grepl("^!", MapCode), -1, 1)
    rel = gsub("^[!]{0,1}(CTA|CTY|BZN)\\|(.*)$", "\\1", MapCode)
    rel_ctry = gsub("^[!]{0,1}(CTA|CTY|BZN)\\|(.*)$", "\\2", MapCode)
    MapCode = ifelse(simple_type, MapCode, NA)
    rel = ifelse(simple_type, NA, rel)
    rel_ctry = ifelse(simple_type, NA, rel_ctry)
  })

  if( !add_complex ){
    data <- data[data$simple_type,]
  }
  data

}
