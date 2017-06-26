#' @export
are_equals_if_not_na <- function(y, z){

  is.na(y) | is.na(z) | abs(y-z)<.Machine$double.eps

}


#' @export
abs_diff_lt <- function(y, z, abs_diff = 0.05 ){
  is.na(y) | is.na(z) | abs(1 - (y / z) ) < abs_diff
}


