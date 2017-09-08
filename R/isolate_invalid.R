isolate_invalid <- function( x ){
  UseMethod("isolate_invalid")
}


#' @importFrom data.table as.data.table melt.data.table dcast.data.table
isolate_invalid.controled <- function( x ){

  if( !inherits(x, what = "controled" ) )
    stop("data need to be validated with function augment_validation")

  if( is.null( attr(x, "validators") ) )
    stop("no validator attributes found")

  validators <- attr(x, "validators")

  ref_db <- cbind( x[, validators ], id = seq_len(nrow(x) ) )
  ref_db <- as.data.table(ref_db)
  all_res <- melt.data.table(
    data = ref_db, id.vars = "id", measure.vars = validators,
    variable.name = "test", value.name = "validated" )

  all_res <- all_res[!all_res$validated, ]

  temp_db <- as.data.table( cbind( x[, setdiff(names(x), validators)], id = seq_len(nrow(x) ) ) )
  cases <- dcast.data.table(all_res, id ~ test, fill = TRUE, value.var = "validated")
  out <- merge(temp_db, cases, by="id")
  out$id <- NULL
  out <- as.data.frame(out)
  class(out) <- c(class(x), "invalid_raw" )
  out
}



