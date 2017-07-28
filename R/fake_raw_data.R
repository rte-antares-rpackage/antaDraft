#' rebuild an expected fake dataset from raw data
#'
#' @importFrom purrr map_df
#' @importFrom dplyr bind_rows full_join left_join
#' @importFrom tibble tibble
#' @export
#' @param raw_db the raw data returned by \code{read_load_files}
#' @param file_rules yaml file containing country rules
fake_raw_data <- function(raw_db, file_rules = NULL){
  cty_rules <- get_cty_rules(file_rules = file_rules)
  ref_mapcode <- map_df(cty_rules, function(x) {
    bind_rows(
      tibble(MapCode = x$CTY, AreaTypeCode = "CTY"),
      tibble(MapCode = x$CTA, AreaTypeCode = "CTA"),
      tibble(MapCode = x$BZN, AreaTypeCode = "BZN") )
  }, .id = "country")
  ref_mapcode$dummy = 1
  all_dt <- tibble( dummy = 1,
                    DateTime = seq( min( raw_db$DateTime ),
                                    max( raw_db$DateTime ), by = "hour" ) )

  full_data <- full_join(ref_mapcode, all_dt, by = "dummy")
  full_data$dummy <- NULL
  raw_db$observed = TRUE
  raw_db <- left_join(full_data, raw_db, by = c("MapCode", "AreaTypeCode", "DateTime"))
  raw_db$observed[is.na( raw_db$observed ) ] <- FALSE
  raw_db
}

