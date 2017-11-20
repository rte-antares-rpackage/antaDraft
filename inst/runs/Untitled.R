library(antaDraft)
library(magrittr)
library(timevis)
library(DT)
library(glue)
library(purrr)
library(data.table)
library(flextable)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggiraph)
library(UpSetR)
library(ggalt)

theme_ <- theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

load_path <- "/Users/davidgohel/Github/dat.entsoe/load"
load_path <- "/Users/davidgohel/Documents/consulting/RTE/A01-Consommation_réalisée"
load_data <- anta_load_read(data_dir = load_path)

# what is the time frame ---
load_data %>% filter(observed) %>%
  select(DateTime, country) %>%
  group_by(country) %>%
  summarise(start = min(DateTime, na.rm = TRUE),
            end = max(DateTime, na.rm = TRUE) ) %>%
  ungroup() %>%
  rename(content = country) %>%
  mutate(type = "range") %>% timevis()

# how load value is spread across dimensions ---
load_data %>% filter(observed) %>%
  group_by(country, AreaTypeCode, MapCode) %>%
  summarise(min_load = min(TotalLoadValue, na.rm = TRUE),
            med_load = median(TotalLoadValue, na.rm = TRUE),
            avg_load = mean(TotalLoadValue, na.rm = TRUE),
            max_load = max(TotalLoadValue, na.rm = TRUE) ) %>%
  datatable()

# qualify invalidities ----
load_data <- augment_validation(load_data)

qc <- load_data %>% qualcon()
groups_data <- qc %>% select(country) %>% distinct() %>% transmute(content = country, id = seq_along(country))

qc_is_pos <- qc %>% filter(validator%in% "IS_POS") %>%
  mutate(
    content = glue("MapCode '{MapCode}' - AreaTypeCode '{AreaTypeCode}' - AreaName '{AreaName}'")
  ) %>% inner_join(groups_data, by = c("country"="content")) %>%
  rename(group = id )

timevis(qc_is_pos, groups = groups_data %>% semi_join(qc_is_pos, by = c("content"="country")))

qc_is_obs <- qc %>% filter(validator%in% "IS_OBS") %>%
  mutate(
    content = glue("MapCode '{MapCode}' - AreaTypeCode '{AreaTypeCode}' - AreaName '{AreaName}'")
  ) %>% inner_join(groups_data, by = c("country"="content")) %>%
  rename(group = id )
timevis(qc_is_obs, groups = groups_data %>% semi_join(qc_is_obs, by = c("content"="country")))

load_data %>%
  group_by(IS_OBS, IS_POS) %>%
  tally()





do.call(iron, waffles$part)
dir.create(path = "qc_raw")
render_quality(qc, dir = "qc_raw")

aggregated_db <- aggregate_with_rules(load_data)

drop_suffix <- function(x, suf){
  nn <- names(x)
  nn <- gsub(paste0("_", suf,"$"), "", nn )
  names(x) <- nn
  x$type = suf
  x
}
dat_analysis <- aggregated_db %>%
  group_by(country) %>%
  summarise_if(function(x) !inherits(x, "POSIXct"),
               .funs = funs(min=min(., na.rm = TRUE), max(., na.rm = TRUE),
                            median(., na.rm = TRUE)))
min_dat <- dat_analysis %>% select(country, ends_with("min")) %>% drop_suffix("min")
max_dat <- dat_analysis %>% select(country, ends_with("max")) %>% drop_suffix("max")
med_dat <- dat_analysis %>% select(country, ends_with("median")) %>% drop_suffix("median")
dat_analysis <- bind_rows(min_dat, max_dat, med_dat) %>%
  gather(measure, load, -country, -type) %>%
  mutate(type = factor(type, levels = c("min", "median", "max") ),
         measure = factor(measure, levels = c("CTY", "CTA", "BZN"))
         ) %>% as.data.table() %>%
  dcast.data.table(country ~ measure + type, fun = mean, value.var = "load")


formatters <- function(x){
  f. <- lapply(x$body$dataset, function(x){
    if(is.character(x)) function(x) x
    else function(x) sprintf("%.0f", x)
  })
  f.$x <- x
  do.call(set_formatter, f.)
}

ref_header <- expand.grid(stringsAsFactors = FALSE,
  statistic = c("min", "median", "max"),
  measure = c("CTY", "CTA", "BZN")) %>%
  mutate(col_keys = paste(measure, statistic, sep = "_")) %>%
  select(col_keys, measure, statistic) %>%
  add_row(col_keys = "country", measure = "country", statistic = "")

regulartable(dat_analysis) %>%
  set_header_df(ref_header) %>%
  formatters() %>%
  theme_zebra() %>%
  merge_h(part = "header") %>%
  border( border=fp_border(color = "orange"), part = "all") %>%
  border( i = 2, border.bottom = fp_border(width = 2,color = "orange"), part = "header") %>%
  align(i = 1, align = "center", part = "header")

aug_db <- aggregated_db %>%
  augment_validation()

qc <- aug_db %>% qualcon()

groups_data <- qc %>% filter(country %in% "BELGIUM") %>%
  select(validator) %>% distinct() %>% transmute(content = validator, id = seq_along(validator))

qc %>%
  filter(country %in% "BELGIUM") %>%
  inner_join(groups_data, by = c("validator"="content")) %>%
  rename(group = id ) %>%
  timevis(groups = groups_data)


dummy_fun <- function(x) sum(!x)

aug_db %>%
  group_by(country) %>%
  summarise_at( attr(aug_db, "validators"), .funs = funs( dummy_fun ) ) %>%
  gather(validator, count, -country) %>% filter(count > 5) %>%
  ggplot(aes(x=validator, y=count)) +
  geom_lollipop(point.size=2, horizontal=FALSE) +
  facet_wrap( ~ country, scales = "free") +
  theme_

features <- as.matrix( aug_db[, attr(aug_db, "validators")] )
features <- !features
mode(features) <- "integer"

datafeat <- features[aug_db$country %in% "FRANCE" & year(aug_db$DateTime) == 2015 ,] %>% as.data.frame()
upset(datafeat, nsets = 7)

for(country in unique(aug_db$country) ){
  datafeat <- features[aug_db$country %in% country,] %>%
    as.data.frame()
  upset(datafeat, nsets = 7)
  zz <- recordPlot()
  filewid <- paste0("feature", tolower(country), ".html")
  wid <- ggiraph(code = replayPlot(zz), width = .9, zoom_max = 3)
  htmlwidgets::saveWidget(widget = wid, file = filewid, selfcontained = TRUE)
}



aug_db <- aug_db %>%
  data_correct_with_rules() %>%
  augment_process_summary()

aug_db %>% as_tibble() %>% filter(lubridate::year(DateTime) %in% 2015:2016) %>%
  select(country, DateTime, CTY, CTA, BZN, summary) %>%
  saveRDS("~/Documents/sandbox/budapestbi_ggiraph/load_data.RDS")

count_waffle <- function(x){
  as.integer( sum(!x) )
}
#

library(ggiraph)
aug_db %>%
  mutate(for_n = FALSE) %>%
  group_by(country) %>%
  summarise_at(c( attr(aug_db, "validators"), "for_n"), count_waffle ) %>%
  tidyr::gather(names, vals, -country) %>% arrange(country, names) %>%
  group_by_at("names") %>%
  tidyr::nest() %>%
  mutate(part = map(data, function(x) {
    gg <- ggplot(x, aes(country, vals)) + geom_lollipop()
    gg
  } ) ) %>% mutate(part = map2(part, names, function(x, lab) {
    x <- x + labs(title = lab )
    x
  } ) ) -> waffles

library(ggrepel)
aug_db %>%
  group_by(country) %>%
  summarise_at(c( attr(aug_db, "validators") ), count_waffle ) %>%
  tidyr::gather(names, vals, -country) %>%
  ggplot(aes(x = country, y = vals)) + geom_point(position = "jitter") +
  facet_wrap(~names)

aug_db %>%
  group_by(country) %>%
  summarise_at(c( attr(aug_db, "validators") ), count_waffle ) %>%
  tidyr::gather(names, vals, -country) %>%
  ggplot(aes(x = country, y = vals)) + geom_point(position = "jitter") +
  facet_wrap(~names, scales = "free")



dat_analysis <- aug_db %>%
  group_by(country, summary ) %>%
  tally() %>% ungroup() %>%
  tidyr::complete(country, summary, fill = list(n=0) )

ggplot(dat_analysis, aes(x=summary, y=n)) +
  geom_lollipop(point.size=2, horizontal=FALSE) +
  facet_wrap( ~ country, scales = "free") +
  theme_

dat_analysis %>%
  tidyr::spread(summary, n) %>%
  regulartable() %>%
  theme_booktabs() %>%
  fontsize(size = 13, part = "header") %>%
  bg(i = ~ invalid > 0, bg = "#FFFFFF") %>%
  bg(i = ~ invalid > 100, bg = "#EAC8A9") %>%
  bg(i = ~ invalid > 1000, bg = "#DA6759") %>%
  bg(i = ~ invalid > 10000, bg = "#CB060A") %>%
  bold(j = 1) %>%
  padding(padding.left = 5, padding.right = 5, part = "all") %>%
  border(j = 2:4, border.left = fp_border(), part = "all") %>%
  autofit()

