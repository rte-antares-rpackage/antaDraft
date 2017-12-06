#' @export
#' @title perform quality control
#' @description perform quality control
#' @param x data where controls have been evaluated
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
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


#' @export
#' @title report QC results
#' @description create reports from results of \code{qualcon} call.
#' @param x an object of class \code{qualcon}
#' @param dir directory where reports should be written
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
#' load_data <- anta_load_read(data_dir = load_dir )
#' load_data <- augment_validation(load_data)
#'
#' qc <- qualcon(load_data)
#' qc_raw_dir <- file.path( tempfile(), "raw_qc" )
#' dir.create(qc_raw_dir, recursive = TRUE, showWarnings = FALSE)
#' render_quality(qc, qc_raw_dir)
#'
#' aggregated_db <- aggregate_with_rules(load_data)
#' aggregated_db <- augment_validation(aggregated_db)
#'
#' qc <- qualcon(aggregated_db)
#' qc_agg_dir <- file.path( tempfile(), "agg_qc" )
#' dir.create(qc_agg_dir, recursive = TRUE, showWarnings = FALSE)
#' render_quality(qc, qc_agg_dir)
render_quality <- function( x, dir ){
  UseMethod("render_quality")
}


#' @importFrom data.table melt.data.table
#' @rdname qualcon
#' @export
qualcon.aggregated <- function( x ){

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

#' @rdname qualcon
#' @export
qualcon.raw_level <- function( x ){

  if( !inherits(x, "controled") )
    stop("x has not been validated by function augment_validation(), please run it before.")

  meta <- capture_df_meta(x)

  dat <- isolate_invalid(x)

  measure.vars <- intersect(names(dat), meta$validators)

  x <- data.table::melt.data.table(
    as.data.table(dat), id.vars = meta$id.vars,
    measure.vars = measure.vars,
    variable.name = "validator", value.name = "validated")

  x <- x[!x$validated, ]

  index_vars_1 <- c( setdiff(meta$id.vars, meta$timevar), "validator" )
  index_vars_2 <- c( index_vars_1, "time_frame" )

  out <- x[, time_frame := cumsum( c( TRUE, diff(DateTime) != 1 ) ), by = index_vars_1 ]
  out <- out[, list(start = min(get(meta$timevar)), end = max(get(meta$timevar)) ), by = index_vars_2 ]

  out$end <- out$end + ifelse( out$end - out$start > 0, 60*60, 0 )
  out$time_frame <- NULL
  out <- as.data.frame(out)

  restore_df_meta(out, meta = meta, new_class = "qualcon_raw" )
}


#' @rdname render_quality
#' @export
render_quality.raw_level <- function( x, dir ){

  if( !dir.exists(dir) ){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  browser()

  cties_list <- unique(x$country)
  valid_list <- attr( x, "validators")

  x$period = ifelse(x$end - x$start>0, TRUE, FALSE)
  by_data <- split(x, x$country)
  by_data <- lapply( by_data, function(x)  split(x, x$validator) )

  for(country in names(by_data) ){

    for( validator in names(by_data[[country]]) ){
      if( nrow( by_data[[country]][[validator]] ) < 1 ) next

      outfile <- paste0(country, "_[" , validator, "].md" )
      outfile <- file.path(dir, outfile)

      sink(file = outfile )

      cat( "# Quality report\n", sep = "")
      cat( "\n", sep = "")

      cat( "### ", sprintf("timestamp: %s", format(Sys.time(), '%Y-%m-%d %H:%M:%S')), "\n", sep = "")
      cat( "\n", sep = "")

      cat( "**Item: Actual Total Load [6.1]**", "\n", sep = "")
      cat( "\n", sep = "")

      cat( sprintf("Area: **%s** | **%s**", validator, country), "\n", sep = "")
      cat( "\n", sep = "")

      data <- by_data[[country]][[validator]]
      data <- data[data$period,  ]

      if( nrow( data) > 0 ){

        between_values <- paste0("* ", format(data$start, "%Y-%m-%d %H:%M:%S"),
                                 " to ", format(data$end, "%Y-%m-%d %H:%M:%S"))
        cat( "### Between", "\n", sep = "")
        cat( "\n", sep = "")
        cat( paste0(between_values, collapse = "\n"), "\n", sep = "")
        cat( "\n", sep = "")
      }
      data <- by_data[[country]][[validator]]
      data <- data[!data$period,  ]

      if( nrow( data) > 0 ){

        at_values <- paste0("* ", format(data$start, "%Y-%m-%d %H:%M:%S") )
        cat( "### At", "\n", sep = "")
        cat( "\n", sep = "")
        cat( paste0(at_values, collapse = "\n"), "\n", sep = "")
        cat( "\n", sep = "")
      }


      sink()

    }
  }

  invisible()

}




