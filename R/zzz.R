globalVariables(c("AreaTypeCode", "DateTime", "TotalLoadValue", "country", ":="))

package_name <- "antaDraft"

#' @importFrom readr read_delim cols col_integer col_datetime col_character col_double col_skip
read_load_file <- function( f ){
  x <- read_delim(file = f, delim = "\t", col_names = TRUE,
                  col_types = cols(
                    year = col_integer(), month = col_integer(), day = col_integer(),
                    DateTime = col_datetime(format = ""), AreaTypeCode = col_character(),
                    AreaName = col_character(), MapCode = col_character(),
                    TotalLoadValue = col_double(), SubmissionTS = col_datetime(format = "")
                  )
  )
  as.data.frame(x)
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

