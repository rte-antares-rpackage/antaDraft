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



#' @importFrom data.table melt.data.table
#' @rdname qualcon
#' @export
qualcon.aggregated <- function( x ){

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
  restore_df_meta(out, meta = meta, new_class = "qualcon_agg" )
}


#' @rdname qualcon
#' @export
qualcon.type_group_prod <- function( x ){

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
  restore_df_meta(out, meta = meta, new_class = "qualcon_comp_prod" )
}




#' @rdname render_quality
#' @export
render_quality.raw_level <- function( x, dir ){

  if( !dir.exists(dir) ){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  meta <- capture_df_meta(x)

  x$period = ifelse(x$end - x$start>0, TRUE, FALSE)
  by_vars <- c(meta$countryvar, "AreaTypeCode", "MapCode", "validator" )
  by_data <- split(as.data.table(x), flatten = FALSE, by = by_vars )

  for(country in names(by_data) ){
    for( atc in names(by_data[[country]]) ){
      for( mc in names(by_data[[country]][[atc]]) ){
        for( validator in names(by_data[[country]][[atc]][[mc]]) ){
          dat <- by_data[[country]][[atc]][[mc]][[validator]]
          if( nrow( dat ) < 1 ) next

          outfile <- paste0(country,
                            "_" , atc,
                            "_" , mc,
                            "_" , validator,
                            ".md" )
          outfile <- file.path(dir, outfile)

          sink(file = outfile )

          cat( "# Quality report\n", sep = "")
          cat( "\n", sep = "")

          cat( "### ", sprintf("timestamp: %s", format(Sys.time(), '%Y-%m-%d %H:%M:%S')), "\n", sep = "")
          cat( "\n", sep = "")

          cat( "**Item: Actual Total Load [6.1]**", "\n", sep = "")
          cat( "\n", sep = "")

          cat( sprintf("Country: **%s** | AreaTypeCode: **%s** | MapCode: **%s** | validator: **%s**", country, atc, mc, validator), "\n", sep = "")
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
      }
    }
  }


  invisible()

}





#' @rdname render_quality
#' @export
render_quality.aggregated <- function( x, dir ){

  if( !dir.exists(dir) ){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  meta <- capture_df_meta(x)

  x$period = ifelse(x$end - x$start>0, TRUE, FALSE)
  by_vars <- c(meta$countryvar, "validator" )
  by_data <- split(as.data.table(x), flatten = FALSE, by = by_vars )

  for(country in names(by_data) ){
    for( validator in names(by_data[[country]]) ){
      dat <- by_data[[country]][[validator]]
      if( nrow( dat ) < 1 ) next

      outfile <- paste0(country,
                        "_" , validator,
                        ".md" )
      outfile <- file.path(dir, outfile)

      sink(file = outfile )

      cat( "# Quality report\n", sep = "")
      cat( "\n", sep = "")

      cat( "### ", sprintf("timestamp: %s", format(Sys.time(), '%Y-%m-%d %H:%M:%S')), "\n", sep = "")
      cat( "\n", sep = "")

      cat( "**Item: Actual Total Load [6.1]**", "\n", sep = "")
      cat( "\n", sep = "")

      cat( sprintf("Country: **%s** | validator: **%s**", country, validator), "\n", sep = "")
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
  }


  invisible()

}



#' @export
qualcon.raw_prod_type <- qualcon.raw_level

#' @export
qualcon.raw_prod_group <- qualcon.raw_level

#' @rdname render_quality
#' @export
render_quality.raw_prod_type <- function( x, dir ){

  if( !dir.exists(dir) ){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  meta <- capture_df_meta(x)

  x$period = ifelse(x$end - x$start>0, TRUE, FALSE)
  by_vars <- c(meta$countryvar, "AreaTypeCode", "MapCode", "production_type", "validator" )
  by_data <- split(as.data.table(x), flatten = FALSE, by = by_vars )

  for(country in names(by_data) ){
    for( atc in names(by_data[[country]]) ){
      for( mc in names(by_data[[country]][[atc]]) ){
        for( pt in names(by_data[[country]][[atc]][[mc]]) ){
          for( validator in names(by_data[[country]][[atc]][[mc]][[pt]]) ){

            dat <- by_data[[country]][[atc]][[mc]][[pt]][[validator]]
            if( nrow( dat ) < 1 ) next

            outfile <- paste0(country,
                              "_" , atc,
                              "_" , mc,
                              "_" , make.names(pt),
                              "_" , validator,
                              ".md" )
            outfile <- file.path(dir, outfile)

            sink(file = outfile )

            cat( "# Quality report\n", sep = "")
            cat( "\n", sep = "")

            cat( "### ", sprintf("timestamp: %s", format(Sys.time(), '%Y-%m-%d %H:%M:%S')), "\n", sep = "")
            cat( "\n", sep = "")

            cat( "**Item: Actual Total Prod [6.1]**", "\n", sep = "")
            cat( "\n", sep = "")

            cat( sprintf("Country: **%s** | AreaTypeCode: **%s** | MapCode: **%s** | Production type: **%s** | validator: **%s**", country, atc, mc, pt, validator), "\n", sep = "")
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

        }
      }
    }
  }


  invisible()

}


#' @export
qualcon.aggregated_prod <- qualcon.aggregated






#' @rdname render_quality
#' @export
render_quality.aggregated_prod <- function( x, dir ){

  if( !dir.exists(dir) ){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  meta <- capture_df_meta(x)

  x$period = ifelse(x$end - x$start>0, TRUE, FALSE)
  by_vars <- c(meta$countryvar, "validator", "production_type" )
  by_data <- split(as.data.table(x), flatten = FALSE, by = by_vars )

  for(country in names(by_data) ){
    for( validator in names(by_data[[country]]) ){
      for( ptype in names(by_data[[country]][[validator]]) ){

        dat <- by_data[[country]][[validator]][[ptype]]
        if( nrow( dat ) < 1 ) next

        outfile <- paste0(country,
                          "_" , validator,
                          "_" , make.names(ptype),
                          ".md" )
        outfile <- file.path(dir, outfile)

        sink(file = outfile )

        cat( "# Quality report\n", sep = "")
        cat( "\n", sep = "")

        cat( "### ", sprintf("timestamp: %s", format(Sys.time(), '%Y-%m-%d %H:%M:%S')), "\n", sep = "")
        cat( "\n", sep = "")

        cat( "**Item: Actual Total Prod [6.1]**", "\n", sep = "")
        cat( "\n", sep = "")

        cat( sprintf("Country: **%s** | validator: **%s** | Production type: **%s**", country, validator, ptype), "\n", sep = "")
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

    }
  }


  invisible()

}





#' @rdname render_quality
#' @export
render_quality.raw_prod_group <- function( x, dir ){

  if( !dir.exists(dir) ){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  meta <- capture_df_meta(x)

  x$period = ifelse(x$end - x$start>0, TRUE, FALSE)
  by_vars <- c(meta$countryvar, "MapCode", "production_type", "validator" )
  by_data <- split(as.data.table(x), flatten = FALSE, by = by_vars )

  for(country in names(by_data) ){
    for( atc in names(by_data[[country]]) ){
        for( pt in names(by_data[[country]][[atc]]) ){
          for( validator in names(by_data[[country]][[atc]][[pt]]) ){

            dat <- by_data[[country]][[atc]][[pt]][[validator]]
            if( nrow( dat ) < 1 ) next

            outfile <- paste0(country,
                              "_" , atc,
                              "_" , make.names(pt),
                              "_" , validator,
                              ".md" )
            outfile <- file.path(dir, outfile)

            sink(file = outfile )

            cat( "# Quality report\n", sep = "")
            cat( "\n", sep = "")

            cat( "### ", sprintf("timestamp: %s", format(Sys.time(), '%Y-%m-%d %H:%M:%S')), "\n", sep = "")
            cat( "\n", sep = "")

            cat( "**Item: Actual Total Prod [6.1]**", "\n", sep = "")
            cat( "\n", sep = "")

            cat( sprintf("Country: **%s** | AreaTypeCode: **%s** | Production type: **%s** | validator: **%s**", country, atc, pt, validator), "\n", sep = "")
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

        }
    }
  }


  invisible()

}


