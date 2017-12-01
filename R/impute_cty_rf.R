#' @title define and save model
#' @description create a model for each country of an validated and corrected
#' load dataset. Each model is saved in a file.
#' @param data aggregated load dataset
#' @param x_vars x variables
#' @param y_var y variable
#' @param save_model_dir directory where to save models
#' @param id models id, will be used to prefix files and qualify produced models
#' @param h2o_ip IP address of the server where H2O is running
#' @param h2o_port port number of the H2O server
#' @importFrom h2o as.h2o h2o.randomForest h2o.saveModel h2o.loadModel
#' @importFrom stats na.omit
#' @export
define_model_rf <- function( data, x_vars, y_var, save_model_dir = getwd(), id = "",
                             h2o_port = 54321, h2o_ip = "localhost" ){
  meta <- capture_df_meta(data)

  learning_db <- split(data, data[[meta$countryvar]])

  h2o.init(ip = h2o_ip, port = h2o_port, startH2O = TRUE)

  out_data <- list()

  for( ctry in names(learning_db) ){
    model_dir <- file.path(save_model_dir, ctry )
    dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)
    frame_id_learn <- paste0(id, "_", ctry, "_", "LEARN")
    model_id <- paste0(id, "_", ctry)

    datamart_country <- learning_db[[ctry]]
    datamart <- datamart_country[datamart_country[[meta$colname_process_summary]] %in% c("corrected", "original") , , drop = FALSE ]
    datamart <- datamart[, c(x_vars, y_var) ]
    datamart <- na.omit( datamart )

    as.h2o( datamart, destination_frame = frame_id_learn)

    current_model <- h2o.randomForest(x = x_vars, y = y_var, training_frame = frame_id_learn, model_id = model_id)

    unlink(file.path(model_dir, model_id), force = TRUE)
    h2o.saveModel(current_model, model_dir)
    out_data[[ctry]] <- data.frame(
      country = ctry, id = id,
      n = nrow(datamart),
      invalid_n2 = Inf,
      invalid_n1 = sum( datamart_country[[meta$colname_process_summary]] %in% "invalid" ),
      rmsle = current_model@model$training_metrics@metrics$rmsle,
      h2o_file = file.path(model_dir, model_id ), stringsAsFactors = FALSE )
  }
  # h2o.rm(h2o.ls()$key)
  h2o.shutdown(prompt = FALSE)
  out_data <- do.call(rbind, out_data)
  row.names(out_data) <- NULL
  models <- meta$models
  models[[id]] <- out_data
  meta <- add_df_meta( meta, "models", models )
  restore_df_meta(data, meta = meta )
}


#' @export
#' @importFrom h2o h2o.rm h2o.ls
impute_with_model <- function( db, id, rmsle_max = .03,
                               h2o_port = 54321, h2o_ip = "localhost" ){
  meta <- capture_df_meta(db)

  models <- meta$models[[id]]
  if(is.null(models) ) return(db)
  models <- models[models$rmsle < rmsle_max, , drop = FALSE]

  if(nrow(models) < 1) return(db)

  h2o.init(ip = h2o_ip, port = h2o_port, startH2O = TRUE)
  out_data <- list()
  for(i in seq_len(nrow(models)) ){
    country <- models$country[i]

    if( models$invalid_n2[i] < models$invalid_n1[i] )
      break

    model <- h2o.loadModel(models$h2o_file[i])
    country_data <- db[db[[meta$countryvar]] %in% country &
                         db[[meta$colname_process_summary]] %in% "invalid", ]

    data <- country_data[, c( meta$countryvar, meta$timevar, model@parameters$x), drop = FALSE]
    data <- na.omit(data)

    models$invalid_n2[i] <- models$invalid_n1[i]
    models$invalid_n1[i] <- nrow(data)
    if( models$invalid_n2[i] < models$invalid_n1[i] )
      break

    frame_id_prev <- paste0("PREV_", country)
    prev_data <- as.h2o( data[, -(1:2), drop = FALSE], destination_frame = frame_id_prev)
    prev_env <- h2o.predict(model, newdata = prev_data)
    prev_values <- as.data.frame(prev_env)
    data$predict <- prev_values$predict
    out_data[[country]] <- data[, c(meta$countryvar, meta$timevar, "predict") ]
  }
  # h2o.rm(h2o.ls()$key)
  h2o.shutdown(prompt = FALSE)

  out_data <- rbindlist(out_data)
  db <- merge(as.data.table(db), out_data,
              by = c(meta$countryvar, meta$timevar),
              all.x = TRUE, all.y = TRUE )
  for( j in meta$measures ){
    db[[j]] <- ifelse( is.finite(db$predict), db$predict, db[[j]] )
  }

  db$summary <- ifelse( is.finite(db$predict), id, db$summary )
  db$predict <- NULL
  db <- restore_df_meta(db, meta = meta )

  db
}

#' @export
update_learning_db <- function( data, target = "CTY" ){

  meta <- capture_df_meta(data)

  hour_shift <- meta$hour_shift
  daily_summary <- meta$daily_shift

  for( hs in hour_shift)
    data <- augment_shifted(data, col = target, hour_shift = hs)
  for( ds in daily_summary)
    data <- augment_daily(data, col = target, decay = ds)

  restore_df_meta(data, meta = meta )
}

