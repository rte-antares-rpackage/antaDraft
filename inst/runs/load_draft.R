library(antaDraft)
library(magrittr)
library(flextable)
library(mgcv)
library(dplyr)
library(purrr)

load_path <- "/Users/davidgohel/Github/dat.entsoe/load"


load_data <- anta_load_read(
  data_dir = load_path) %>%
  augment_validation()

# dir.create(path = "qc_raw")
# qc <- qualcon(load_data) %T>% render_quality(dir = "qc_raw")

agg_db <- aggregate_with_rules(load_data) %>%
  augment_validation() %>%
  data_correct_with_rules() %>%
  augment_process_summary() %>%
  augment_holiday() %>%
  augment_seasons_id() %>%
  augment_daylight() %>%
  augment_daily(col = "CTY", decay = 1) %>%
  augment_daily(col = "CTY", decay = 2)


# dir.create(path = "qc_agg")
# wrong_agg_db <- qualcon(agg_db) %T>% render_quality(dir = "qc_agg")


agg_db %>%
  group_by(country, summary ) %>%
  tally() %>% ungroup() %>%
  tidyr::complete(country, summary, fill = list(n=0) ) %>%
  tidyr::spread(summary, n) %>%
  regulartable() %>%
  theme_booktabs() %>% autofit()
#
#

CTY_H1 <- agg_db %>%
  select(country, DateTime, CTY) %>%
  group_by(country) %>%
  transmute(CTY_H1 = CTY, DateTime = DateTime - 60*60) %>%
  ungroup()
agg_db <- agg_db %>% left_join(CTY_H1, by = c("DateTime", "country") )


run_model <- function(data){
  x <- try(
    gam(data = data, formula = CTY ~
          s(week.iso, k=50) + s(day.iso, k=5) + s(hour.iso, k=10) +
          s(light_time, k = 5) + is_off + likely_off +
          s(AVG_CTY_D1, k = 30 ) + s(MIN_CTY_D1, k = 30 ) + s(MAX_CTY_D1, k = 30 ) +
          s(AVG_CTY_D2, k = 30 ) + s(MIN_CTY_D2, k = 30 ) + s(MAX_CTY_D2, k = 30 ) +
          s(CTY_H1, k = 30 )
    ) )
  x
}


training_datasets <- agg_db %>%
  # filter( year.iso %in% c(2015, 2016), summary %in% "original" ) %>%
  group_by_at("country") %>%
  tidyr::nest()


# gams_by_country <- mutate(training_datasets, model = map(data, run_model))
# gams_by_country <- select_at(gams_by_country, .vars = c("country", "model") )

# xvar <- c("week.iso", "day.iso", "hour.iso",
#   "light_time", "is_off", "likely_off",
#   "AVG_CTY_D1", "MIN_CTY_D1", "MAX_CTY_D1",
#   "AVG_CTY_D2", "MIN_CTY_D2", "MAX_CTY_D2",
#   "CTY_H1")
# yvar <- 'CTY'


# gams_by_country[-3,] %>%
#   mutate( r.sq = map_dbl(model, function(x) summary(x)$r.sq ) )


# NETHERLANDS <- agg_db %>%
#   filter( year.iso %in% c(2015, 2016), country %in% "NETHERLANDS", summary %in% "invalid" )
# prev <- do_prev(model = gams_by_country[[5,"model"]], data = NETHERLANDS )
# match( prev$DateTime, NETHERLANDS$DateTime )

# augment_prev <- function(model, data){
#   if( !is.null(model) && !inherits(model, "try-error"))
#     prev <- as.vector(predict(model, newdata = data))
#   else prev <- rep(NA_real_, nrow(data) )
#   data <- cbind(data, prev = prev)
#   data
# }
#
# country_datasets <- agg_db %>%
#   filter( year.iso %in% c(2015, 2016) ) %>%
#   group_by_at("country") %>%
#   tidyr::nest() %>%
#   left_join(gams_by_country) %>%
#   mutate( data = map2(model, data, augment_prev ) )



library(randomForest)
rt_by_country <- training_datasets %>%
  semi_join(agg_db %>%
              filter( year.iso %in% c(2015, 2016), summary %in% c("original", "corrected") ) %>%
              group_by_at("country") %>% tally() %>% ungroup() %>% filter(n > 11000)) %>%
  mutate(model = map(data, function(x){
    zz <- randomForest(formula = CTY ~
                         week.iso + day.iso + hour.iso +
                         light_time + is_off + likely_off +
                         AVG_CTY_D1 + MIN_CTY_D1 + MAX_CTY_D1 +
                         AVG_CTY_D2 + MIN_CTY_D2 + MAX_CTY_D2 +
                         CTY_H1, data = x, na.action = na.omit)
    zz
  }))

rt_by_country %>%
  mutate( r.sq = map_dbl(model, function(x) mean(x$rsq) ) )
# saveRDS(rt_by_country, "~/Downloads/rt_by_country.RDS")
rt_by_country <- readRDS("~/Downloads/rt_by_country.RDS")

country_datasets <- agg_db %>%
  filter( year.iso %in% c(2015, 2016) ) %>%
  group_by_at("country") %>%
  tidyr::nest() %>%
  left_join(rt_by_country[,-2]) %>%
  mutate( data = map2(model, data, function(model, data){
    data <- data[ data$summary %in% "invalid",]
    if( !is.null(model) && !inherits(model, "try-error"))
      prev <- as.vector(predict(model, newdata = data))
    else prev <- rep(NA_real_, nrow(data) )
    data$CTY <- prev
    data$summary <- "mod_rf"
    data[!is.na(prev),]
  } ) ) %>%
  select(-model) %>%
  tidyr::unnest(data)

dat <- agg_db %>%
  filter( year.iso %in% c(2015, 2016) ) %>%
  anti_join(country_datasets, c("country", "DateTime") ) %>%
  bind_rows(country_datasets)

by_ctry <- dat %>%
  group_by(country) %>%
  tally() %>% rename(total = n)

by_ctry_invalid <- dat %>% filter(!summary %in% 'invalid') %>%
  group_by(country) %>%
  tally() %>% rename(ok = n)

dat %>%
  group_by(country, summary) %>%
  tally() %>% ungroup() %>%
  tidyr::complete(country, summary, fill = list(n=0) ) %>%
  inner_join(by_ctry) %>%
  inner_join(by_ctry_invalid) %>%
  mutate(valid_rate = round(ok / total * 100, 2) ) %>%
  select(-ok, -total) %>%
  tidyr::spread(summary, n) %>%
  arrange(valid_rate) %>%
  regulartable() %>%
  theme_booktabs() %>% autofit()

validated_countries <- c("PORTUGAL", "NETHERLANDS", "GERMANY",
                         "FRANCE", "AUSTRIA")
library(openxlsx)
for(i in validated_countries){
  dat %>%
    filter(country %in% i ) %>%
    select(DateTime, CTY, summary) %>%
    write.xlsx(file = paste0(i, ".xlsx"))
}

dat %>%
  filter(country %in% "ITALY" ) %>%
  View

