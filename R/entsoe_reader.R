#' @importFrom fasttime fastPOSIXct
entsoe_dir_reader <- function(dir, datetime_col, submissin_col,
                              drops = character(0), id_vars,
                              sep = "\t", ct_format = "%Y-%m-%d %H:%M:%S"){

  agg_files <- list.files(dir, pattern = "(\\.csv)$", full.names = TRUE)

  data <- rbindlist( lapply(agg_files, function(f){
    data <- fread(input = f, sep = "\t", header = TRUE, drop = drops )
    filter_ <- grepl("00:00\\.000", data[[datetime_col]])
    data[filter_,]
  } ) )

  data <- setorderv(data, cols = c(id_vars, submissin_col))
  data <- unique(data, by = id_vars, fromLast = TRUE )
  data[[submissin_col]] <- NULL

  data <- data[, (datetime_col) := lapply(.SD, function(x) fastPOSIXct(x, required.components = 6L, tz = "GMT") ), .SDcols = datetime_col]
  data
}

