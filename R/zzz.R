globalVariables(c(":=", "DateTime", "SubmissionTS", "TotalLoadValue", "installed_capacity",
                  "time_frame", "y",
                  "generation_output", "consumption"))

package_name <- "antaDraft"

fp_expr <- function( fp_rules ){
  fp <- yaml::yaml.load_file(fp_rules)
  exprs <- lapply(fp, function(x){
    sprintf( "%s <- ifelse( %s == FALSE & %s == FALSE, TRUE, %s)", x$drop, paste0(x$when, collapse = "&"), x$drop, x$drop )
  })
  exprs <- unlist(exprs)
  exprs <- paste0( unlist(exprs), collapse = ";\n" )
  parse(text = exprs)
}

get_ctry_rules <- function(add_complex = FALSE){

  global_options <- getOption("global_options")

  atc_per_country <- yaml.load_file(global_options$atc_per_country)

  atc_per_country <- lapply( atc_per_country, function(x) {
    rbind(
      data.frame(MapCode = x$CTY, AreaTypeCode = "CTY", stringsAsFactors = FALSE),
      data.frame(MapCode = x$CTA, AreaTypeCode = "CTA", stringsAsFactors = FALSE),
      data.frame(MapCode = x$BZN, AreaTypeCode = "BZN", stringsAsFactors = FALSE) )
  } )
  ref_mapcode <- rbindlist(atc_per_country, idcol = "country")

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
