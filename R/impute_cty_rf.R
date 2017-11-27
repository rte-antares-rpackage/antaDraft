#' @import h2o
#' @export
model_save_cty_rf <- function( db ){

  id.vars <- attr( db, "id.vars")
  ts_key <- attr( db, "timevar")

  learning_db <- as_learning_db( db )
  learning_db <- split(learning_db, learning_db$country)

  for( ctry in names(learning_db) ){
    ctry_db <- learning_db[[ctry]]

    frame_id_learn <- paste0(ctry, "_learn")
    model_id <- paste0(ctry, "_rf")

    learn_db <- ctry_db[ctry_db$summary %in% c("original", "corrected"), c(5:18, 3)]# x -> 1:14

    as.h2o( learn_db, destination_frame = frame_id_learn)
    model <- h2o.randomForest(x = 1:14, y = 15, training_frame = frame_id_learn, model_id = model_id)
  }

}


#' @import h2o
#' @export
impute_cty_rf <- function( db, h2o_port = 54321, h2o_ip = "localhost", loop = 3 ){

  h2o.init(ip = h2o_ip, port = h2o_port, startH2O = TRUE)
  id.vars <- attr( db, "id.vars")
  ts_key <- attr( db, "timevar")

  model_save_cty_rf(db)

  for(i in seq_len(loop)){
    learning_db <- as_learning_db( db )
    learning_db <- split(learning_db, learning_db$country)

    corrections <- list()

    for( ctry in names(learning_db) ){
      ctry_db <- learning_db[[ctry]]

      frame_id_prev <- paste0(ctry, "_prev")
      model_id <- paste0(ctry, "_rf")

      prev_db <- na.omit(ctry_db[!ctry_db$summary %in% c("original", "corrected"), ])
      prev_frame <- prev_db[, 5:18]
      model <- h2o.getModel(model_id)

      prev_data <- as.h2o( prev_frame, destination_frame = frame_id_prev)

      prev_env <- h2o.predict(model, newdata = prev_data)
      prev_values <- as.data.frame(prev_env)
      prev_db$CTY <- prev_values$predict
      prev_db$summary <- rep("rf", nrow(prev_db))
      corrections[[ctry]] <- prev_db
    }


    corrections <- do.call(rbind, corrections)
    corrections$BZN <- corrections$CTY
    corrections$CTA <- corrections$CTY
    corrections <- corrections[, c(attr(db, "id.vars"), "CTY", "CTA", "BZN") ]

    new_data <- as.data.table(db)
    new_data_old <- new_data[!corrections, on = attr(db, "id.vars")]

    w = unique(new_data[corrections,which=TRUE, on = attr(db, "id.vars")])  # the row numbers in x which have a match from y
    new_data_new <- new_data[w]
    new_data_new$CTY <- NULL
    new_data_new$BZN <- NULL
    new_data_new$CTA <- NULL

    new_data_new <- new_data_new[corrections, on = attr(db, "id.vars")]
    new_data_new$summary <- "impute_model"
    new_data <- rbind( new_data_new, new_data_old)
    new_data <- setorderv(new_data, c("country", "DateTime"))

    db <- as.data.frame(new_data)
    attr( db, "id.vars") <- id.vars
    attr( db, "timevar") <- ts_key
  }
  h2o.shutdown(prompt = FALSE)

  db
}


