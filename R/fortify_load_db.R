liste_area <- c("AT", "BE", "CH", "DE", "ES", "FR", "GB", "IE", "IT", "IT_CNOR", "IT_CSUD", "IT_NORD", "IT_SARD",
                "IT_SICI", "IT_SUD", "NIE", "NL", "PT")

globalVariables(c("DateTime", "MapCode", "TotalLoadValue", "DateTime", "%>%", "AreaName", "AreaTypeCode"))



#' @importFrom dplyr filter group_by summarise transmute bind_rows ungroup
#' @export
#' @title tidy load data
#' @description tidy and correct load data
#' @param load_data data returned by \code{\link{import_load_db}}
fortify_load_db <- function( load_data ){

  # donnees pas structurée pareil - a aggreger , pas de doublons
  conso_DE <- load_data %>%
    filter(grepl("^DE_", MapCode ), !MapCode %in% "DE_AT_LU" ) %>%
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
