library(antaDraft)

load_dir <- "/Users/davidgohel/Documents/consulting/RTE/load_files"

load_data <- anta_load_read(data_dir = load_dir )
load_data <- augment_validation(load_data)
head(load_data)

aggregated_db <- aggregate_with_rules(load_data)
aggregated_db <- augment_validation(aggregated_db)
aggregated_db <- data_correct_with_rules(aggregated_db)
aggregated_db <- augment_process_summary(aggregated_db)

corrected_by_rf <- impute_cty_rf(aggregated_db, hour_decay = -1, loop = 10)
corrected_by_rf <- impute_cty_rf(corrected_by_rf, hour_decay = 1, loop = 10)
corrected_by_rf <- impute_cty_rf(corrected_by_rf, hour_decay = 1, loop = 20)
corrected_by_rf <- impute_cty_rf(corrected_by_rf, hour_decay = 1, loop = 5)
corrected_by_rf <- impute_cty_rf(corrected_by_rf, hour_decay = 1, loop = 5)
corrected_by_rf <- impute_cty_rf(corrected_by_rf, hour_decay = -1, loop = 5)
corrected_by_rf <- impute_cty_rf(corrected_by_rf, hour_decay = 1, loop = 5)


library(tidyverse)
corrected_by_rf %>%
  filter( lubridate::year( DateTime ) %in% 2015:2016 ) %>%
  group_by(country, summary) %>%
  summarise(n = n() ) %>% ungroup() %>%
  spread(summary, n, fill = 0)

aggregated_db %>%
  filter( lubridate::year( DateTime ) %in% 2015:2016 ) %>%
  group_by(country, summary) %>%
  summarise(n = n() ) %>% ungroup() %>%
  spread(summary, n, fill = 0)

aggregated_db %>%
  filter( lubridate::year( DateTime ) %in% 2015:2016,
          country %in% "IRELAND",
          summary %in% "invalid") %>%
  View

library(ggplot2)
corrected_by_rf %>%
  filter( lubridate::year( DateTime ) %in% 2015:2016 ) %>%
  ggplot(mapping = aes(DateTime, CTY)) + geom_line() + facet_wrap(~country, scales = "free")




AUSTRIA <- corrected_by_rf %>%
  filter( lubridate::year( DateTime ) %in% 2015, country == "AUSTRIA" )
