
globalVariables(c("AreaTypeCode", "DateTime", "TotalLoadValue", "country"))

fp_expr <- function( fp_rules ){
  fp <- yaml::yaml.load_file(fp_rules)
  exprs <- lapply(fp, function(x){
    sprintf( "%s <- ifelse( %s == FALSE & %s == FALSE, TRUE, %s)", x$drop, paste0(x$when, collapse = "&"), x$drop, x$drop )
  })
  exprs <- unlist(exprs)
  exprs <- paste0( unlist(exprs), collapse = ";\n" )
  parse(text = exprs)
}


#' @importFrom purrr map_chr
correct_exprs <- function( correct_rules ){

  rules <- yaml::yaml.load_file(correct_rules)

  str1 <- map_chr(rules, function(x) paste0( "(", paste0( "!", x$when_false, collapse = " & " ), ")") )
  str2 <- map_chr(rules, function(x) paste0( "(", paste( x$when_true, collapse = " & " ), ")") )
  str3 <- map_chr(rules, function(x) paste0( "( country %in% c(", paste( shQuote(x$country), collapse = ", " ), ") )" ) )
  cond_ <- sprintf( "%s & %s & %s", str1, str2, str3)

  replace_var <- map_chr(rules, function(x) x$replace )
  with_var <- map_chr(rules, function(x) x$use )

  exprs <- sprintf("%s <- ifelse(%s, %s, %s)", replace_var, cond_, with_var, replace_var)
  exprs <- paste0( exprs, collapse = ";\n" )
  parse(text = exprs)
}

mark_correct_exprs <- function( correct_rules ){

  rules <- yaml::yaml.load_file(correct_rules)

  str1 <- map_chr(rules, function(x) paste0( "(", paste0( "!", x$when_false, collapse = " & " ), ")") )
  str2 <- map_chr(rules, function(x) paste0( "(", paste( x$when_true, collapse = " & " ), ")") )
  str3 <- map_chr(rules, function(x) paste0( "( country %in% c(", paste( shQuote(x$country), collapse = ", " ), ") )" ) )
  cond_ <- sprintf( "%s & %s & %s", str1, str2, str3)

  replace_var <- sprintf("rule_%04.0f", seq_along(rules))
  with_var <- sprintf("TRUE", seq_along(rules))

  init_exprs <- sprintf("%s <- FALSE", replace_var)
  exprs <- sprintf("%s <- ifelse(%s, %s, %s)", replace_var, cond_, with_var, replace_var)
  exprs <- paste0( c(init_exprs, exprs), collapse = ";\n" )
  parse(text = exprs)
}


dir_create_ <- function( dir ){
  if( !dir.exists(dir) )
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}


ficonv_utf8 <- function( file, outfile = tempfile(fileext = ".txt") ){
  cmd_ <- sprintf("iconv -f UTF-16LE -t UTF-8 %s > %s", file, outfile)
  system(command = cmd_, intern = TRUE)
  outfile
}

#' @importFrom readr read_delim cols col_integer col_datetime col_character col_double
read_load_file <- function( f ){
  x <- read_delim(file = f, delim = "\t", col_names = TRUE,
                  col_types = cols(
                    year = col_integer(), month = col_integer(), day = col_integer(),
                    DateTime = col_datetime(format = ""), AreaTypeCode = col_character(),
                    AreaName = col_character(), MapCode = col_character(),
                    TotalLoadValue = col_double(), SubmissionTS = col_datetime(format = "")
                  )
  )
  as.data.frame(x)
}

#' @importFrom yaml yaml.load_file
structure_create <- function(dir, force = TRUE){

  structure_yaml <- system.file(package = "entsoe", "structure.yml")
  specs <- yaml.load_file(structure_yaml)
  specs <- specs[c("directories", "subdirectories")]

  specs$stringsAsFactors <- FALSE
  specs <- do.call(expand.grid, specs)

  specs$path <- file.path(dir, specs$directories, specs$subdirectories)

  if( force )
    unlink(dir, recursive = TRUE, force = TRUE)

  for(dir_ in specs$path )
    dir_create_(dir_)

  specs
}

as_grp_index <- function(x){
  sprintf( "gp_%09.0f", x )
}


group_index <- function(x, by, varname = "grp"){
  order_ <- do.call( order, x[ by ] )
  x$ids_ <- seq_along(order_)
  x <- x[order_, ,drop = FALSE]
  gprs <- cumsum(!duplicated(x[, by ]) )
  gprs <- gprs[order(x$ids_)]
  as_grp_index(gprs)
}

group_ref <- function(x, by, varname = "grp"){
  order_ <- do.call( order, x[ by ] )
  x$ids_ <- seq_along(order_)
  x <- x[order_, ,drop = FALSE]
  ref <- x[!duplicated(x[, by ]), by]
  ref$index_ <- as_grp_index( seq_len( nrow(ref) ) )
  row.names(ref) <- NULL
  ref
}
