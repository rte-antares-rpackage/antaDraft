globalVariables(c("AreaTypeCode", "DateTime", "TotalLoadValue", "country", ":="))

package_name <- "antaDraft"

read_load_file <- function( f ){
  fread(input = f, sep = "\t",
        header = TRUE,
        colClasses = list(POSIXct = "DateTime",
                          character = c("AreaTypeCode", "AreaName", "MapCode"),
                          integer = "TotalLoadValue" ),
        drop = c("SubmissionTS", "year", "month", "day") )
}


fp_expr <- function( fp_rules ){
  fp <- yaml::yaml.load_file(fp_rules)
  exprs <- lapply(fp, function(x){
    sprintf( "%s <- ifelse( %s == FALSE & %s == FALSE, TRUE, %s)", x$drop, paste0(x$when, collapse = "&"), x$drop, x$drop )
  })
  exprs <- unlist(exprs)
  exprs <- paste0( unlist(exprs), collapse = ";\n" )
  parse(text = exprs)
}

