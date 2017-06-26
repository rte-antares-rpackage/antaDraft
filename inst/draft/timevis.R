library(timevis)

sum_db_erros %>% select(-time_frame) %>%
  filter(country %in% "FRANCE") %>%
  rename(content = validator, group = country) %>%
  timevis()

groups <- sum_db_erros %>% select(-time_frame) %>%
  group_by(country) %>%
  tally() %>% filter(n < 100) %>% ungroup() %>%
  transmute(id = country, content = country)

sum_db_erros %>% select(-time_frame) %>%
  inner_join(select(groups, -content), by = c("country"="id") ) %>%
  rename(content = validator, group = country) %>%
  timevis(groups = groups)

