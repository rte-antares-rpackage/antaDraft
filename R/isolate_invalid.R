#Copyright © 2018 RTE Réseau de transport d’électricité

isolate_invalid <- function( x ){
  UseMethod("isolate_invalid")
}


isolate_invalid.controled <- function( x ){

  if( !inherits(x, what = "controled" ) )
    stop("data need to be validated with function augment_validation")

  if( is.null( attr(x, "validators") ) )
    stop("no validator attributes found")

  validators <- attr(x, "validators")
  invalids <- which(!apply(x[, validators ], 1, all))
  out <- x[invalids, , drop = FALSE]

  out <- as.data.frame(out)
  class(out) <- c(class(x), "invalid_raw" )
  out
}



