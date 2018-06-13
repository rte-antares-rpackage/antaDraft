#Copyright © 2018 RTE Réseau de transport d’électricité

#' @export
#' @title perform quality control
#' @description perform quality control
#' @param x data where controls have been evaluated
#' @examples
#' load_dir <- system.file(package = "antaDraft",
#'   "data_sample/load_sample_2017")
#' load_data <- anta_load(data_dir = load_dir )
#' load_data <- augment_validation(load_data)
#'
#' qualcon(load_data)
#'
#' aggregated_db <- agg_data(load_data)
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
#' load_dir <- system.file(package = "antaDraft",
#'   "data_sample/load_sample_2017")
#' load_data <- anta_load(data_dir = load_dir )
#' load_data <- augment_validation(load_data)
#'
#' qc <- qualcon(load_data)
#' render_quality(qc, "raw_qc")
#'
#' aggregated_db <- agg_data(load_data)
#' aggregated_db <- augment_validation(aggregated_db)
#'
#' qc <- qualcon(aggregated_db)
#' render_quality(qc, "agg_qc")
render_quality <- function( x, dir ){
  UseMethod("render_quality")
}

#' @importFrom data.table melt.data.table
#' @rdname qualcon
#' @export
qualcon.data.frame <- function( x ){

  if( !inherits(x, "controled") )
    stop("x has not been validated by function augment_validation(), please run it before.")

  meta <- capture_df_meta(x)
  dat <- isolate_invalid(x)

  measure.vars <- intersect(names(dat), meta$validators)

  x <- data.table::melt.data.table(
    as.data.table(dat), id.vars = c(meta$countryvar, meta$timevar, meta$id.vars),
    measure.vars = measure.vars,
    variable.name = "validator", value.name = "validated")

  x <- x[!x$validated, ]

  index_vars_1 <- c( meta$id.vars, meta$countryvar, "validator" )
  index_vars_2 <- c( index_vars_1, "time_frame" )

  out <- x[, time_frame := cumsum( c( TRUE, diff(DateTime) != 1 ) ), by = index_vars_1 ]
  out <- out[, list(start = min(get(meta$timevar)), end = max(get(meta$timevar)) ), by = index_vars_2 ]

  out$end <- out$end + ifelse( out$end - out$start > 0, 60*60, 0 )
  out$time_frame <- NULL
  setDF(out)

  restore_df_meta(out, meta = meta, new_class = "qualcon" )
}



#' @rdname render_quality
#' @export
render_quality.data.frame <- function( x, dir ){

  if( !dir.exists(dir) ){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  meta <- capture_df_meta(x)

  x$period = ifelse(x$end - x$start>0, TRUE, FALSE)

  by_vars <- c(meta$countryvar, meta$id.vars, "validator" )
  setDT(x)
  x <- x[, (by_vars) := lapply(.SD, function(x) as.character(x) ), .SDcols = by_vars]
  x <- x[ , "new" := do.call(paste, c(.SD, sep = "_")), .SDcols = by_vars]
  by_data <- split(as.data.table(x), flatten = TRUE, by = "new" )

  for( i in seq_len(length(by_data))){
    dat <- by_data[[i]]
    if( nrow( dat ) < 1 ) next

    by_vars_values <- unique( as.data.frame(dat)[by_vars] )

    args_list <- append(by_vars_values, list(sep = "_" ) )

    outfile <- make.names( do.call( paste, args_list ) )
    outfile <- file.path(dir, paste0(outfile, ".md") )

    sink(file = outfile )

    cat( "# Quality report\n", sep = "")
    cat( "\n", sep = "")

    cat( "### ", sprintf("timestamp: %s", format(Sys.time(), '%Y-%m-%d %H:%M:%S')), "\n", sep = "")
    cat( "\n", sep = "")

    cat( "**Item: ", meta$label, "**", "\n", sep = "")
    cat( "\n", sep = "")

    fmt <- paste0(by_vars, ": **%s** ", collapse = "| ")
    cat( do.call( sprintf, append( list(fmt = fmt), by_vars_values ) ), "\n", sep = "")
    cat( "\n", sep = "")

    data <- dat[dat$period,  ]

    if( nrow( data) > 0 ){

      between_values <- paste0("* ", format(data$start, "%Y-%m-%d %H:%M:%S"),
                               " to ", format(data$end, "%Y-%m-%d %H:%M:%S"))
      cat( "### Between", "\n", sep = "")
      cat( "\n", sep = "")
      cat( paste0(between_values, collapse = "\n"), "\n", sep = "")
      cat( "\n", sep = "")
    }

    data <- dat[!dat$period,  ]
    if( nrow( data) > 0 ){

      at_values <- paste0("* ", format(data$start, "%Y-%m-%d %H:%M:%S") )
      cat( "### At", "\n", sep = "")
      cat( "\n", sep = "")
      cat( paste0(at_values, collapse = "\n"), "\n", sep = "")
      cat( "\n", sep = "")
    }

    sink()
  }

  invisible()

}



