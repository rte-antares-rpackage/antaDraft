#' @importFrom h2o as.h2o h2o.randomForest h2o.saveModel
model_save_cty_rf <- function( data, ctry_col, x_vars, y_var, save_model = FALSE, save_model_dir = getwd() ){

  learning_db <- split(data, data[[ctry_col]])

  for( ctry in names(learning_db) ){
    frame_id_learn <- paste0(ctry, "_learn")
    model_id <- paste0(ctry, "_rf")
    as.h2o( learning_db[[ctry]], destination_frame = frame_id_learn)
    current_model <- h2o.randomForest(x = x_vars, y = y_var, training_frame = frame_id_learn, model_id = model_id)
    if( save_model ){
      h2o.saveModel(current_model, save_model_dir)
    }
  }

}

#' @importFrom stats na.omit
#' @title CTY imputation with random forests
#' @description CTY imputation with random forests
#' @param db aggregated load dataset
#' @param hour_decay integer specifying to shift data Y from \code{hour_decay}
#' and add the resulting column as variable
#' @param loop specify how many correction loops have to be run
#' @param h2o_ip IP address of the server where H2O is running
#' @param h2o_port port number of the H2O server
#' @param save_model should the models be saved on disk
#' @param save_model_dir where models should be saved
#' @export
#' @examples
#' \dontrun{
#' load_dir <- system.file(package = "antaDraft", "data_sample")
#'
#' load_data <- anta_load_read(data_dir = load_dir )
#' load_data <- augment_validation(data = load_data)
#' head(load_data)
#'
#' aggregated_db <- aggregate_with_rules(load_data)
#' aggregated_db <- augment_validation(aggregated_db)
#' aggregated_db <- data_correct_with_rules(aggregated_db)
#' aggregated_db <- augment_process_summary(aggregated_db)
#'
#' corrected_by_rf <- impute_cty_rf(aggregated_db, hour_decay = -1, loop = 4)
#' corrected_by_rf <- impute_cty_rf(corrected_by_rf, hour_decay = 1, loop = 3)
#' }
#' @importFrom h2o h2o.init h2o.shutdown h2o.getModel h2o.predict
impute_cty_rf <- function( db, hour_decay = -1, loop = 3, h2o_port = 54321, h2o_ip = "localhost",
                           save_model = FALSE, save_model_dir = getwd()){

  h2o.init(ip = h2o_ip, port = h2o_port, startH2O = TRUE)
  id.vars <- attr( db, "id.vars")
  ts_key <- attr( db, "timevar")
  validators <- attr( db, "validators")


  first <- TRUE
  datamart <- as_learning_db( db, hour_decay = hour_decay )


  x_vars <- c("is_off", "likely_off", "year.iso", "week.iso", "hour.iso", "day.iso", "light_time",
              "MIN_CTY_1", "AVG_CTY_1", "MAX_CTY_1", "MIN_CTY_2", "AVG_CTY_2", "MAX_CTY_2")
  decay_var <- names(datamart)[grepl( "^CTY_HOUR_DECAY", names(datamart) )]
  x_vars <- c(x_vars, decay_var)

  learning_db <- datamart[ !datamart$summary %in% "invalid", c(x_vars, "country", "CTY") ]
  model_save_cty_rf(learning_db, ctry_col = "country",
                    x_vars = x_vars, y_var = "CTY",
                    save_model = save_model, save_model_dir = save_model_dir )

  whole_db <- datamart[ , c(x_vars, "country", "CTY", "summary", "DateTime") ]
  whole_db_l <- split(whole_db, whole_db$country)

  for( iter in seq_len(loop)){

    if( !first ){
      datamart <- as_learning_db( db, hour_decay = hour_decay )
      whole_db <- datamart[ , c(x_vars, "country", "CTY", "summary", "DateTime") ]
      whole_db_l <- split(whole_db, whole_db$country)
    }
    corrections <- list()

    for( ctry in names(whole_db_l) ){

      ctry_db <- whole_db_l[[ctry]]

      frame_id_prev <- paste0(ctry, "_prev")
      model_id <- paste0(ctry, "_rf")

      prev_db <- na.omit(ctry_db[ctry_db$summary %in% "invalid", c("DateTime", x_vars)])
      model <- h2o.getModel(model_id)

      prev_data <- as.h2o( prev_db[, setdiff(names(prev_db), "DateTime")], destination_frame = frame_id_prev)

      prev_env <- h2o.predict(model, newdata = prev_data)
      prev_values <- as.data.frame(prev_env)
      prev_db$predict <- prev_values$predict

      corrections[[ctry]] <- prev_db
    }

    corrections2 <- rbindlist(corrections, use.names = TRUE, idcol = "country")
    corrections2 <- corrections2[, c(id.vars, "predict"), with = FALSE]
    db <- merge(as.data.table(db), corrections2, by = c("country", "DateTime"), all.x = TRUE, all.y = TRUE )
    db$CTY <- ifelse( is.finite(db$predict), db$predict, db$CTY )
    db$summary <- ifelse( is.finite(db$predict), "rf_model", db$summary )
    db$predict <- NULL
    db <- as.data.frame(db)

    first <- FALSE

    attr( db, "id.vars") <- id.vars
    attr( db, "timevar") <- ts_key
    attr( db, "validators") <- validators
  }

  h2o.shutdown(prompt = FALSE)
  db
}

