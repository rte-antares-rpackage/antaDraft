library(antaDraft)
library(magrittr)
library(dplyr)

load_data <- anta_load_read(
  data_dir = "/Users/davidgohel/Documents/consulting/RTE/load") %>%
  augment_validation()

load_gerluxaus_20150115 <- filter(load_data,
                             as.Date(DateTime) == as.Date("2015-01-15") ,
                             lubridate::hour(DateTime) == 7,
                             country %in% c("AUSTRIA", "GERMANY", "LUXEMBOURG") )
devtools::use_data(load_gerluxaus_20150115, overwrite = TRUE)
