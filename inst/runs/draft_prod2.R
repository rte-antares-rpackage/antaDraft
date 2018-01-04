library(antaDraft)

production_channel <- anta_prod_channel(
  production_dir = "/Users/davidgohel/Documents/consulting/RTE/PROD/B01-Production_réalisée_par_filière",
  capacity_dir = "/Users/davidgohel/Documents/consulting/RTE/PROD/B06-Capacité_installée_par_filière"
  )
toto <- augment_validation(production_channel)


capacity_channel <- antaDraft:::anta_capacity_channel(
  data_dir = "/Users/davidgohel/Documents/consulting/RTE/PROD/B06-Capacité_installée_par_filière",
  min_dt = min(toto$DateTime), max_dt = max(toto$DateTime))
prod_by_ctry <- unique(capacity_channel[,c("country", "production_type" )])

# dt_range <- range(production_channel$DateTime, na.rm = TRUE)
head(production_channel)
capacity_channel <- anta_capacity_channel(
  data_dir = "/Users/davidgohel/Documents/consulting/RTE/B-PRODUCTION/channel_capacity",
  min_dt = dt_range[1], max_dt = dt_range[2])
capacity_channel$SubmissionTS <- NULL
capacity_channel$observed <- NULL
head(capacity_channel)
head(production_channel)

library(data.table)
setDT(capacity_channel)
setDT(production_channel)


toto <- merge(
  x = production_channel,
  y = capacity_channel,
  by = c("DateTime", "MapCode", "AreaTypeCode", "production_type", "AreaName", "country"),
  all.x = FALSE, all.y = TRUE )

toto <- toto[!is.na(toto$AggregatedInstalledCapacity),]
summary(toto$ActualGenerationOutput)
summary(toto$AggregatedInstalledCapacity)
capacity_channel_ <- mutate(capacity_channel, year = lubridate::year(DateTime) ) %>%
  group_by_at(c("production_type", "country", "year")) %>% summarise(AggregatedInstalledCapacity = sum(AggregatedInstalledCapacity, na.rm = TRUE))
production_channel_ <- filter(production_channel, AreaTypeCode %in% "CTA") %>%
  mutate(year = lubridate::year(DateTime) ) %>%
  filter(year > 2014) %>%
  group_by_at(c("production_type", "country", "year")) %>% summarise(ActualGenerationOutput = sum(ActualGenerationOutput, na.rm = TRUE) - sum(ActualConsumption, na.rm = TRUE) )

inner_join(capacity_channel_, production_channel_) %>% View
