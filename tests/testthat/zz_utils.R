library(purrr)
library(lubridate)


pattern_data <-
  structure(
    list(
      country = "FRANCE",
      MapCode = "FR",
      AreaTypeCode = "CTY",
      DateTime = structure(
        1483228800,
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      year = 2017L,
      month = 1L,
      day = 1L,
      AreaName = "France",
      TotalLoadValue = 73330,
      SubmissionTS = structure(
        1483257040,
        class = c("POSIXct",
                  "POSIXt"),
        tzone = "UTC"
      )
    ),
    .Names = c(
      "country",
      "MapCode",
      "AreaTypeCode",
      "DateTime",
      "year",
      "month",
      "day",
      "AreaName",
      "TotalLoadValue",
      "SubmissionTS"
    ),
    id.vars = c(
      "country",
      "MapCode",
      "AreaTypeCode",
      "DateTime",
      "year",
      "month",
      "day",
      "AreaName"
    ),
    timevar = "DateTime",
    row.names = 1L,
    class = "data.frame"
  )
pattern_data <- rbind(pattern_data, pattern_data, pattern_data)
pattern_data$AreaTypeCode <- c("CTY", "CTA", "BZN")
pattern_data$country <- NULL



seq_pattern_on_hours <- function( hours_ ){
  out <- map_df( .x = hours_, .f = function(day_, dat) {
    dat$DateTime <- dat$DateTime + lubridate::hours(day_)
    dat$year <- lubridate::year(dat$DateTime)
    dat$month <- lubridate::month(dat$DateTime)
    dat$day <- lubridate::day(dat$DateTime)
    dat
  }, pattern_data)
  out[order(out$DateTime), ]
}

load_example <- seq_pattern_on_hours(1:(24 * 3))
load_example$TotalLoadValue <- load_example$TotalLoadValue + round( runif( n = nrow(load_example), min = -300, max = 300 ))
# devtools::use_data(load_example)
