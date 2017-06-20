#' @importFrom purrr map_df
#' @importFrom tibble as_tibble
#' @import magrittr
#' @importFrom utils read.table
#' @importFrom dplyr filter mutate
#' @export
#' @title import load data files into a tibble
#' @description import load data files into a tibble
#' @param dir_src data source directory
#' @note
#' file are supposed to be UTF-16 encoded.
import_load_db <- function( dir_src ){

  agg_files <- list.files(dir_src, pattern = "(\\.csv)$", full.names = TRUE)

  map_df(agg_files, function(i){
    dat <- read.table(i, sep="\t", header=T,
                      fileEncoding = "UTF-16LE",
                      stringsAsFactors = FALSE) %>%
      as_tibble() %>%
      filter(substr(DateTime,15,19)=="00:00") %>%
      mutate(DateTime = as.POSIXct(DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC") )
  })

}

