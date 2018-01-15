capture_df_meta <- function( x ){
  out <- attributes(x)
  out$names <- NULL
  out$row.names <- NULL
  out$class <- setdiff(out$class, c("data.frame", "tbl_df", "tbl", "data.table"))
  out
}

add_df_meta <- function( meta, key, value ){
  meta[[key]] <- value
  meta
}

new_df_meta <- function( ){
  meta <- list(
    class = character(0) )
  meta
}

restore_df_meta <- function( x, meta, new_class = character(0) ){
  x <- as.data.frame(x)
  class_ <- meta$class
  meta$class <- NULL
  for( att_ in names(meta) )
    attr(x, att_) <- meta[[att_]]
  class(x) <- unique(c(new_class, class_, "data.frame"))
  x
}
