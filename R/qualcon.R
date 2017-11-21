#' @export
#' @title perform quality control
#' @description perform quality control
#' @param x data where controls have been evaluated
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
#' load_data <- anta_load_read(data_dir = load_dir )
#' load_data <- anta_load_read(data_dir = load_dir )
#' load_data <- augment_validation(load_data)
#'
#' qualcon(load_data)
#'
#' aggregated_db <- aggregate_with_rules(load_data)
#' aggregated_db <- augment_validation(aggregated_db)
#'
#' qualcon(aggregated_db)
qualcon <- function( x ){
  UseMethod("qualcon")
}



#' @importFrom data.table melt.data.table
#' @rdname qualcon
#' @export
qualcon.controled <- function( x ){

  old_class <- class( x )
  dat <- isolate_invalid(x)
  id.vars <- attr(x, "id.vars")
  timevar <- attr( x, "timevar")
  validators <- attr( x, "validators")

  measure.vars <- intersect(names(dat), validators)

  x <- data.table::melt.data.table(as.data.table(dat), id.vars = id.vars, measure.vars = measure.vars,
            variable.name = "validator", value.name = "validated")
  x <- x[!x$validated, ]

  index_vars_1 <- c( setdiff(id.vars, timevar), "validator" )
  index_vars_2 <- c( index_vars_1, "time_frame" )

  x <- as.data.table(x)

  out <- x[, time_frame := cumsum( c( TRUE, diff(DateTime) != 1 ) ), by = index_vars_1 ]
  out <- out[, list(start = min(get(timevar)), end = max(get(timevar)) ), by = index_vars_2 ]

  out$end <- out$end + ifelse( out$end - out$start > 0, 60*60, 0 )
  out <- as.data.frame(out)
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
#' @title report QC results
#' @description create reports from results of \code{qualcon} call.
#' @param x an object of class \code{qualcon}
#' @param dir directory where reports should be written
render_quality <- function( x, dir ){
  dir <- getAbsolutePath(dir)
  if( !dir.exists(dir) ){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  cties_list <- unique(x$country)
  valid_list <- attr( x, "validators")

  x$period = ifelse(x$end - x$start>0, TRUE, FALSE)
  by_data <- split(x, x$country)
  by_data <- lapply( by_data, function(x)  split(x, x$validator) )

  report_files <- list()
  rmd_file <- system.file(package = package_name, "template_raw_quality.Rmd" )
  for(country in names(by_data) ){

    for( validator in names(by_data[[country]]) ){
      if( nrow( by_data[[country]][[validator]] ) < 1 ) next
      outfile <- paste0(country, "_[" , validator, "].html" )
      outfile <- file.path(dir, outfile)

      par <- list( country = country,
                   id = validator,
                   title = validator,
                   data = by_data[[country]][[validator]] )
      render(rmd_file, params = par, output_file = outfile, intermediates_dir = getwd() )
      report_files <- append( report_files, list(outfile) )
    }
  }
  unlist(report_files)




}




