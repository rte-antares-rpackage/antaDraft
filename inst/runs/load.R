library(antaDraft)
library(magrittr)

# consommation -----
main_dir <- "quality/load"
dir.create(main_dir, recursive = TRUE, showWarnings = TRUE)

load_dir <- "/Users/davidgohel/Documents/consulting/RTE/load_2016"

load_data <- anta_load(data_dir = load_dir ) %>%
  augment_validation()

qc <- qualcon(load_data)
render_quality(qc, dir = file.path(main_dir, "raw"))
plot( load_data )
plot(qc)
plot(qc, subset = qc$country %in% "FRANCE")


aggregated_db <- agg_data(load_data) %>%
  augment_validation()

plot_agg(aggregated_db, subset = aggregated_db$country %in% "FRANCE")
plot( aggregated_db, subset = aggregated_db$country %in% "FRANCE", nsets = 7 )
plot( aggregated_db, subset = aggregated_db$country %in% "SWITZERLAND", nsets = 7 )

aggregated_db <- aggregated_db %>%
  data_correct_with_rules(refresh_validation=FALSE) %>%
  augment_process_summary()

plot(aggregated_db, subset = aggregated_db$country %in% "FRANCE", nsets = 7 )


qc <- qualcon(aggregated_db)
render_quality(qc, dir = file.path(main_dir, "agg"))
plot(qc, subset = qc$country %in% c("FRANCE") )

