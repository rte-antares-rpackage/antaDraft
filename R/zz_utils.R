globalVariables(c('DateTime', 'TotalLoadValue', 'value', 'country', 'time_frame'))


#' @export
#' @title test equality
#' @description test equality when 2 values are not NA.
#' This function is provided to be used in validity checking.
#' @param y variable 1
#' @param z variable 2
#' @seealso \code{\link{qualcon}}
are_equals_if_not_na <- function(y, z){

  abs(y-z) < 1 #.Machine$double.eps
  # Le logiciel antares ne gere pas les chiffres apres la virgule

}


#' @export
#' @title test difference below a threshold
#' @description test difference between 2 values. The absolute value
#' should be below a given threshold.
#' @param y variable 1
#' @param z variable 2
#' @param abs_diff absolute difference to test in percent
#' @seealso \code{\link{qualcon}}
abs_diff_lt <- function(y, z, abs_diff = 0.05 ){
  abs(1 - (y / z) ) < abs_diff
}


#' @export
#' @title test positive value
#' @description test if values are positive when not NA
#' @param y variable to test
#' @seealso \code{\link{qualcon}}
is_positive <- function( y ){
  y > 0
}




#' @export
#' @importFrom stats lag
#' @title test lag values
#' @description test if values are not less than fifty percent of their preceding values.
#' @param y variable to test
#' @seealso \code{\link{qualcon}}
lag_diff_ok <- function( y ){
  calc <- ((y - lag(y)) / y)
  calc < .5
}

