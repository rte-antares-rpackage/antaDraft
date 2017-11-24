#' @title prepare an aggregated load dataset to be model
#' @description add columns to an aggregated load dataset
#' so that a model can be run.
#' @param dat aggregated load dataset
#' @param hour_decay integer specifying to shift data Y from \code{hour_decay}
#' and add the resulting column as variable
#' @export
as_learning_db <- function( dat, hour_decay = -1 ){

  dat <- drop_validators(dat)

  dat2 <- augment_holiday(dat)
  dat2 <- augment_seasons_id(dat2)
  dat2 <- augment_daylight(dat2)
  dat2 <- augment_daily(dat2, col = "CTY", decay = 1)
  dat2 <- augment_daily(dat2, col = "CTY", decay = 2)

  CTY_H1 <- dat2[c("country", "DateTime", "CTY")]
  CTY_H1 <- as.data.table(CTY_H1)
  CTY_H1 <- CTY_H1[, DateTime := DateTime + (hour_decay * 60*60 ), by = "country" ]
  names( CTY_H1 )[3] <- paste0("CTY", "_HOUR_DECAY_", hour_decay)

  dat2 <- as.data.table(dat2)
  dat2 <- merge( dat2, CTY_H1, by = c("DateTime", "country"), all.x = TRUE, all.y = FALSE)

  as.data.frame( dat2 )
}

drop_validators <- function(x){
  id.vars <- attr( x, "id.vars")
  ts_key <- attr( x, "timevar")

  x <- x[, !names(x) %in% attr(x, "validators")]
  x <- as.data.frame(x)

  attr(x, "validators") <- character(0)
  attr( x, "id.vars") <- id.vars
  attr( x, "timevar") <- ts_key
  x
}

