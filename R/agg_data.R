#Copyright © 2018 RTE Réseau de transport d’électricité

#' @export
#' @title Aggregate raw dataset from country rules
#' @description From a raw dataset and a set of rules, aggregations are performed
#' to produce for each country, possible date points (and
#' eventually other dimensions) a set of measure(s).
#' @param x raw dataset.
#' @param ... arguments to be passed to methods.
#' @examples
#' load_dir <- system.file(package = "antaDraft",
#'   "data_sample/load_sample_2017")
#'
#' load_data <- anta_load(data_dir = load_dir )
#' load_data <- augment_validation(data = load_data)
#' head(load_data)
#'
#' aggregated_db <- agg_data(load_data)
agg_data <- function( x, ... ){
  UseMethod("agg_data")
}



