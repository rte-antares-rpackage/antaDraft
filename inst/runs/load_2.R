library(antaDraft)

load_dir <- "/Users/davidgohel/Documents/consulting/RTE/load_files"

load_data <- anta_load_read(data_dir = load_dir )
load_data <- augment_validation(load_data)
# head(load_data)
# str(load_data)

aggregated_db <- aggregate_with_rules(load_data)
aggregated_db <- augment_validation(aggregated_db)
aggregated_db <- data_correct_with_rules(aggregated_db)
aggregated_db <- augment_process_summary(aggregated_db)

dat <- as_learning_db(aggregated_db )




attrs <- attributes(dat)
vars <- c( attrs$id.vars, "CTY",
   attrs$season_columns,
   attrs$daylight_columns,
   attrs$daily_summary,
   attrs$shift_columns,
   attrs$holiday_columns)

x_vars <- c("year.iso", "week.iso", "hour.iso",
            "day.iso", "light_time",
            "is_off", "likely_off",
            "DAILY_MIN_CTY_MINUS_1", "DAILY_AVG_CTY_MINUS_1", "DAILY_MAX_CTY_MINUS_1",
            "HOUR_SHIFT_CTY_MINUS_1")

dat <- define_model_rf( data = dat, x_vars = x_vars, y_var = "CTY",
                 save_model_dir = file.path( getwd(), "ttt"),
                 id = "BACKWARD" )

x_vars <- c("year.iso", "week.iso", "hour.iso",
            "day.iso", "light_time",
            "is_off", "likely_off",
            "DAILY_MIN_CTY_PLUS_1", "DAILY_AVG_CTY_PLUS_1", "DAILY_MAX_CTY_PLUS_1",
            "HOUR_SHIFT_CTY_PLUS_1")

dat <- define_model_rf( data = dat, x_vars = x_vars, y_var = "CTY",
                 save_model_dir = file.path( getwd(), "ttt"),
                 id = "FORWARD" )
for(i in 1:7 ){
  dat <- impute_with_model(dat, id = "FORWARD")
  Sys.sleep(2)
  dat <- impute_with_model(dat, id = "BACKWARD")
  Sys.sleep(2)
  dat <- update_learning_db(dat)
}
for(i in 1:2 ){
  dat <- impute_with_model(dat, id = "FORWARD")
  Sys.sleep(2)
  dat <- impute_with_model(dat, id = "BACKWARD")
  Sys.sleep(2)
  dat <- update_learning_db(dat)
}

library(tidyverse)
run_5 <- dat %>%
  filter( lubridate::year( DateTime ) %in% 2015:2016 ) %>%
  group_by(country, summary) %>%
  summarise(n = n() ) %>% ungroup() %>%
  spread(summary, n, fill = 0)

