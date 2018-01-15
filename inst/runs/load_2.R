library(antaDraft)

load_dir <- "/Users/davidgohel/Documents/consulting/RTE/load_files/"

load_data <- anta_load_read(data_dir = load_dir )
# > dim(load_data)
# [1] 1221671       6
# > dim(load_data)
# [1] 1222141       6
load_data <- augment_validation(load_data)
plot( load_data, subset = load_data$country %in% "FRANCE" )
plot( load_data)
qc <- qualcon(load_data)
render_quality(qc, dir = "test")
plot(qc)
plot(qc, subset = qc$country %in% "FRANCE")
# head(load_data)
# str(load_data)

aggregated_db <- agg_data(load_data)
plot(aggregated_db, subset = aggregated_db$country %in% "FRANCE")
aggregated_db <- augment_validation(aggregated_db)
plot( aggregated_db, subset = aggregated_db$country %in% "FRANCE", nsets = 7 )
plot( aggregated_db, subset = aggregated_db$country %in% "SWITZERLAND", nsets = 7 )
aggregated_db <- data_correct_with_rules(aggregated_db)
aggregated_db <- augment_process_summary(aggregated_db)
class(aggregated_db)

plot(aggregated_db, subset = aggregated_db$country %in% "FRANCE", nsets = 7 )
aggregated_db <- augment_validation(aggregated_db)


qc <- qualcon(aggregated_db)
plot(qc, subset = qc$country %in% c("FRANCE") )
render_quality(qc, dir = "qcagg")


dat <- as_learning_db(aggregated_db )

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

