#' @export
#' @title perform quality control
#' @description perform quality control
#' @param x data where controls have been evaluated
#' @examples
#' data(load_example)
#' cty_rules <- system.file(package = "entsoe", "templates/cty_rules.yaml")
#' val_rules <- system.file(package = "entsoe", "templates/raw/validate.yml")
#' fp_rules <- system.file(package = "entsoe", "templates/raw/false_positives.yml")
#' raw_db <- augment_rules(load_example, file_rules = cty_rules)
#' raw_db <- augment_validation(data = raw_db, val_rules = val_rules,
#'   fp_rules = fp_rules)
#' qualcon(raw_db)
qualcon <- function( x ){
  UseMethod("qualcon")
}



#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by filter mutate summarise
#' @importFrom rlang syms
#' @importFrom lubridate hours
#' @rdname qualcon
#' @export
qualcon.controled <- function( x ){

  old_class <- class( x )
  dat <- isolate_invalid(x)
  id.vars <- attr(x, "id.vars")
  timevar <- attr( x, "timevar")
  validators <- attr( x, "validators")

  measure.vars <- intersect(names(dat), validators)

  x <- melt.data.table(
    data = as.data.table(dat), id.vars = id.vars, measure.vars = measure.vars,
    variable.name = "validator", value.name = "validated" )
  x <- x[!x$validated, ]
  x <- x[order(country, DateTime)]

  index_vars_1 <- c( setdiff(id.vars, timevar), "validator" )
  index_vars_2 <- c( index_vars_1, "time_frame" )

  out <- group_by(as_tibble(x), !!!syms( index_vars_1 ) )
  out <- mutate(out, time_frame = cumsum( c( TRUE, diff(DateTime) != 1 ) ) )
  out <- group_by(out, !!!syms( index_vars_2 ))
  out <- summarise(out, start = min(!!!syms(timevar)), end = max( !!!syms(timevar)))
  out <- as.data.frame( out, stringsAsFactors = FALSE )

  out$end <- out$end + hours( ifelse( out$end - out$start > 0, 1, 0 ) )
  out$time_frame <- NULL
  class( out ) <- c( old_class, "qualcon" )

  attr(out, "id.vars") <- id.vars
  attr(out, "timevar") <- timevar
  attr( out, "validators") <- validators


  out
}


#' @importFrom rmarkdown render
#' @importFrom R.utils getAbsolutePath
#' @export
render_quality <- function( x, dir ){
  dir <- getAbsolutePath(dir)
  if( !dir.exists(dir) ){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  cties_list <- unique(x$country)
  valid_list <- attr( x, "validators")
  attributes(x)
  x$period = ifelse(x$end - x$start>0, TRUE, FALSE)
  by_data <- split(x, x$country)
  by_data <- lapply( by_data, function(x)  split(x, x$validator) )

  report_files <- list()
  rmd_file <- system.file(package = "entsoe", "template_rapport1.Rmd" )
  for(country in names(by_data) ){

    for( validator in names(by_data[[country]]) ){

      outfile <- paste0(country, "_[" , validator, "].html" )
      outfile <- file.path(dir, outfile)

      par <- list( country = country,
                   id = validator,
                   title = validator,
                   data = by_data[[country]][[validator]] )
      render(rmd_file, params = par, output_file = outfile )
      report_files <- append( report_files, list(outfile) )
    }
  }
  unlist(report_files)




}




