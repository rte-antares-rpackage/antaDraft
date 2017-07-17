#' @importFrom purrr map_df
#' @importFrom tibble as_tibble
#' @import magrittr
#' @importFrom utils read.table
#' @importFrom dplyr filter mutate
#' @export
#' @title import load data
#' @description import all load data csv files from a directory.
#' @param dir_src data source directory
#' @section file encoding:
#' file are expected to be UTF-16 encoded. This should be changed to UTF8 as
#' it increases file size and is not a standard file encoding and is likely to
#' cause troubles in the future.
#' @examples
#' library(antadraft)
#' rep_path <- system.file(package = "antadraft", "files/load")
#' load_db <- read_load_files(rep_path)
read_load_files <- function( dir_src ){
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

