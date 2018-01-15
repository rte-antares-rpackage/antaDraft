library(antaDraft)
library(magrittr)
library(dplyr)

load_data <- anta_load_read(
  data_dir = "/Users/davidgohel/Documents/consulting/RTE/load_files") %>%
  augment_validation()

# cas de test avec agg ou ref autre pays a prendre en compte ----
load_gerluxaus_20150115 <- load_data[ as.Date(load_data$DateTime) == as.Date("2015-01-15") &
             lubridate::hour(load_data$DateTime) == 7 &
             load_data$country %in% c("AUSTRIA", "GERMANY", "LUXEMBOURG"), ]

devtools::use_data(load_gerluxaus_20150115, overwrite = TRUE)
