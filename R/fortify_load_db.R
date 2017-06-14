liste_area <- c("AT", "BE", "CH", "DE", "ES", "FR", "GB", "IE", "IT", "IT_CNOR", "IT_CSUD", "IT_NORD", "IT_SARD",
                "IT_SICI", "IT_SUD", "NIE", "NL", "PT")

globalVariables(c("DateTime", "MapCode", "TotalLoadValue", "DateTime", "%>%", "AreaName", "AreaTypeCode"))

#' @importFrom purrr map_df
#' @importFrom tibble as_tibble
#' @import magrittr
#' @importFrom utils read.table
#' @importFrom dplyr filter mutate group_by summarise transmute bind_rows ungroup
#' @export
#' @title parse load data files into a tibble
#' @description parse check and correct data to build a unique dataset containing
#' corrected values
#' @param dir_src data source directory
#' @note
#' file are supposed to be UTF-16 encoded.
fortify_load_db <- function( dir_src ){

  agg_files <- list.files(dir_src, pattern = "(\\.csv)$", full.names = TRUE)

  load_data <- map_df(agg_files, function(i){
    dat <- read.table(i, sep="\t", header=T,
                      fileEncoding = "UTF-16LE",
                      stringsAsFactors = FALSE) %>%
      as_tibble() %>%
      filter(substr(DateTime,15,19)=="00:00") %>%
      mutate(DateTime = as.POSIXct(DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC") )
  })

  # donnees pas structurée pareil - a aggreger , pas de doublons
  conso_DE <- load_data %>%
    filter(grepl("^DE_", MapCode ), !MapCode %in% "DE_AT_LU" ) %>%
    # arrange(DateTime, MapCode) %>%
    group_by(DateTime) %>%
    summarise(TotalLoadValue = sum(TotalLoadValue)) %>% ungroup() %>%
    transmute(id_area = "DE", datetime = DateTime, load_value = TotalLoadValue)

  # donnees pas structurée pareil - a aggreger , pas de doublons
  conso_GB <- load_data %>% filter( AreaName %in% "National Grid BZ" ) %>%
    group_by(DateTime) %>%
    summarise(TotalLoadValue = sum(TotalLoadValue)) %>% ungroup() %>%
    transmute(id_area = "GB", datetime = DateTime, load_value = TotalLoadValue)

  # donnees pas structurée pareil - a aggreger , pas de doublons
  conso_IT <- load_data %>% filter( substr(MapCode,1,3) %in% "IT_" ) %>%
    group_by(DateTime) %>%
    summarise(TotalLoadValue = sum(TotalLoadValue)) %>% ungroup()%>%
    transmute(id_area = "IT", datetime = DateTime, load_value = TotalLoadValue)

  # donnees pas a aggreger , doublons, du coup je choisis AreaTypeCode==CTY
  conso_db <- load_data %>%
    filter(!MapCode %in% c("DE", "GB", "IT") ) %>%
    filter(MapCode %in% liste_area, AreaTypeCode %in% "CTY" ) %>%
    transmute(id_area = MapCode, datetime = DateTime, load_value = TotalLoadValue) %>%
    bind_rows(conso_DE, conso_GB, conso_IT)
}
