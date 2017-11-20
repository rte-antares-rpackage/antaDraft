library(antaDraft)
library(magrittr)
library(flextable)
library(mgcv)
library(dplyr)
library(purrr)
library(randomForest)

eval_results <- function(x){
  z <- x %>%
    filter( lubridate::year(DateTime) %in% c(2015, 2016) )

  by_ctry <- z %>%
    group_by(country) %>%
    tally() %>% rename(total = n)

  by_ctry_invalid <- z %>% filter(!summary %in% 'invalid') %>%
    group_by(country) %>%
    tally() %>% rename(ok = n)


  z %>%
    group_by(country, summary) %>%
    tally() %>% ungroup() %>%
    tidyr::complete(country, summary, fill = list(n=0) ) %>%
    inner_join(by_ctry) %>%
    inner_join(by_ctry_invalid) %>%
    mutate(valid_rate = round(ok / total * 100, 5) ) %>%
    select(-ok, -total) %>%
    tidyr::spread(summary, n) %>%
    arrange(valid_rate) %>%
    regulartable() %>%
    theme_booktabs() %>% autofit()
}



load_path <- "/Users/davidgohel/Github/dat.entsoe/load"


load_data <- anta_load_read(
  data_dir = load_path) %>%
  augment_validation()

dir.create(path = "qc_raw")
qc <- qualcon(load_data) %T>% render_quality(dir = "qc_raw")

agg_db <- aggregate_with_rules(load_data) %>%
  augment_validation() %>%
  data_correct_with_rules() %>%
  augment_process_summary()

rt_by_country <- readRDS("~/Downloads/rt_by_country.RDS")
x = complete_with_model(agg_db, rt_by_country)
print(eval_results(x))
for(i in 1:10 ){
  x = complete_with_model(x, rt_by_country)
}
print(eval_results(x))
for(i in 1:10 ){
  x = complete_with_model(x, rt_by_country)
}
print(eval_results(x))
for(i in 1:10 ){
  x = complete_with_model(x, rt_by_country)
}
print(eval_results(x))
for(i in 1:10 ){
  x = complete_with_model(x, rt_by_country)
}
print(eval_results(x))

print(eval_results(x))
for(i in 1:10 ){
  x = complete_with_model(x, rt_by_country)
}
print(eval_results(x))

