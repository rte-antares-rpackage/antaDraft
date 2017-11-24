library(antaDraft)

load_dir <- "/Users/davidgohel/Documents/consulting/RTE/load_files"

load_data <- anta_load_read(data_dir = load_dir )
load_data <- augment_validation(load_data)
head(load_data)

aggregated_db <- aggregate_with_rules(load_data)
aggregated_db <- augment_validation(aggregated_db)
aggregated_db <- data_correct_with_rules(aggregated_db)
aggregated_db <- augment_process_summary(aggregated_db)

corrected_by_rf <- impute_cty_rf(aggregated_db)
# learning_db <- as_learning_db(aggregated_db)
# learning_db <- split(learning_db, learning_db$country)
#
# library(h2o)
# h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
#
# corrections <- list()
# for( ctry in names(learning_db) ){
#   ctry_db <- learning_db[[ctry]]
#
#   frame_id_learn <- paste0(ctry, "_learn")
#   frame_id_prev <- paste0(ctry, "_prev")
#   model_id <- paste0(ctry, "_rf")
#
#   learn_db <- ctry_db[ctry_db$summary %in% c("original", "corrected"), c(5:18, 3)]# x -> 1:14
#   prev_db <- ctry_db[!ctry_db$summary %in% c("original", "corrected"), ]
#   prev_frame <- prev_db[, 5:18]
#
#   as.h2o( learn_db, destination_frame = frame_id_learn)
#   prev_data <- as.h2o( prev_frame, destination_frame = frame_id_prev)
#   model <- h2o.randomForest(x = 1:14, y = 15, training_frame = frame_id_learn, model_id = model_id)
#
#   prev_env <- h2o.predict(model, newdata = prev_data)
#   prev_values <- as.data.frame(prev_env)
#   prev_db$CTY <- prev_values$predict
#   prev_db$summary <- rep("rf", nrow(prev_db))
#   corrections[[ctry]] <- prev_db
#   h2o.rm(c(frame_id_learn, frame_id_prev, model_id) )
# }
# h2o.rm(as.character( h2o.ls()$key) )
# h2o.shutdown(prompt = FALSE)
