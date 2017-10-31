#' countries coordinates
#'
#' The dataset describes for each country the corresponding latitude and longitude.
#'
#' \itemize{
#'   \item country country name
#'   \item lat,lon latitude and longitude
#' }
#'
#' @name country_coordinates
#' @docType data
#' @keywords datasets
#' @usage data(country_coordinates)
#' @format A data frame with 3 variables, \code{country}, \code{lat} and \code{lon}.
NULL


#' European holidays
#'
#' A raw dataset containing for each country dates of public holidays and a
#' column indicating if the previous day could be used as holiday because
#' preceding a public holidays.
#'
#' @name holidays
#' @docType data
#' @keywords datasets
#' @usage data(holidays)
#' @format A data frame with 4 variables, \code{Date}, \code{country},
#' \code{is_off}, \code{likely_off}.
NULL
