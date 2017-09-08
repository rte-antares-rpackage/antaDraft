#' @export
#' @title create an entsoe repository ready to be analysed
#' @description an entsoe repository consists in a serie of folder, each representing
#' a successive step in the data treatment of entsoe datasets. Each folder will contain
#' output datamarts, qualified invalidations and eventually reports.
#' @param wrangling_dir workspace directory
#' @param data_dir datasets directory
#' @param utf16 whether text file are encoded in UTF16 (if not, UTF8)
#' @param force should the directories be all initiated if already existing
#' @importFrom feather write_feather
entsoe_repository <- function(wrangling_dir, data_dir,
                              utf16 = TRUE, force = FALSE ){

  stopifnot(dir.exists(data_dir))

  str_paths <- structure_create(wrangling_dir, force = force)

  x <- list(wrangling_dir = wrangling_dir,
       data_dir = data_dir)

  agg_files <- list.files(data_dir, pattern = "(\\.csv)$", full.names = TRUE)
  fst_dir <- str_paths$path[str_paths$directories == "01_rawdata" & str_paths$subdirectories == "01_feather"]

  # files importation
  for( f in agg_files){
    fst_file <- file.path(fst_dir, gsub( "\\.csv$", ".feather", basename(f) ) )

    if( force || !file.exists(fst_file) ){
      if( utf16 ) f <- ficonv_utf8(f)
      data <- read_load_file(f)
      write_feather(data, fst_file)
    }

  }

  class(x) <- "entsoe_repository"
  x
}

