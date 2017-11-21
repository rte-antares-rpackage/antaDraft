library(suncalc)
library(data.table)

data("country_coordinates", envir = environment() )

all_dates <- seq.Date(as.Date("2010-01-01"), as.Date("2020-01-01"), "days")

data <- expand.grid( date = all_dates, country = country_coordinates$country, stringsAsFactors = FALSE )
data <- as.data.table(data)
data <- merge(data, country_coordinates, all=TRUE, by = "country")
daylight <- getSunlightTimes(
  data = data, keep = c("sunset", "sunrise"), tz = "CET")
daylight$date <- as.Date(substr(daylight$date, 1, 10)  )


daylight <- daylight[c("country", "date", "sunset", "sunrise")]
devtools::use_data(daylight, overwrite = TRUE)
