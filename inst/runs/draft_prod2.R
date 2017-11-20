library(antaDraft)
library(tidyverse)

production_channel <- anta_prod_channel( data_dir = "/Users/davidgohel/Documents/consulting/RTE/B-PRODUCTION/channel_production", utf16 = FALSE )
capacity_channel <- anta_capacity_channel( data_dir = "/Users/davidgohel/Documents/consulting/RTE/B-PRODUCTION/channel_capacity", utf16 = FALSE )

capacity_channel_ <- mutate(capacity_channel, year = lubridate::year(DateTime) ) %>%
  group_by_at(c("production_type", "country", "year")) %>% summarise(AggregatedInstalledCapacity = sum(AggregatedInstalledCapacity, na.rm = TRUE))
production_channel_ <- filter(production_channel, AreaTypeCode %in% "CTA") %>%
  mutate(year = lubridate::year(DateTime) ) %>%
  filter(year > 2014) %>%
  group_by_at(c("production_type", "country", "year")) %>% summarise(ActualGenerationOutput = sum(ActualGenerationOutput, na.rm = TRUE) - sum(ActualConsumption, na.rm = TRUE) )

inner_join(capacity_channel_, production_channel_) %>% View
