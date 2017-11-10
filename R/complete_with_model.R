copy_load_attributes <- function(x){
  id.vars <- c("country", "DateTime")
  ts_key <- "DateTime"

  x <- as.data.frame(x)
  x <- x[, c(id.vars, c("CTY", "summary"))]
  attr(x, "validators") <- character(0)
  attr( x, "id.vars") <- id.vars
  attr( x, "timevar") <- ts_key
  x
}

#' @export
complete_with_model <- function( dat, models ){

  dat2 <- dat %>%
    copy_load_attributes() %>%
    augment_holiday() %>%
    augment_seasons_id() %>%
    augment_daylight() %>%
    augment_daily(col = "CTY", decay = 1) %>%
    augment_daily(col = "CTY", decay = 2)


  CTY_H1 <- dat2 %>%
    select(country, DateTime, CTY) %>%
    group_by(country) %>%
    transmute(CTY_H1 = CTY, DateTime = DateTime - 60*60) %>%
    ungroup()

  dat2 <- dat2 %>%
    left_join(CTY_H1, by = c("DateTime", "country") )

  corrected_datasets <- dat2 %>%
    filter(summary %in% "invalid") %>%
    group_by_at("country") %>%
    tidyr::nest() %>%
    left_join(models[,-2]) %>%
    mutate( data = map2(model, data, function(model, data){
      data <- data[ data$summary %in% "invalid",]
      if( !is.null(model) && !inherits(model, "try-error"))
        prev <- as.vector(predict(model, newdata = data))
      else prev <- rep(NA_real_, nrow(data) )
      data$CTY <- prev
      data$summary <- "mod_rf"
      data[!is.na(prev),]
    } ) ) %>%
    select(-model) %>%
    tidyr::unnest(data)


  dat %>%
    anti_join(corrected_datasets, c("country", "DateTime") ) %>%
    bind_rows(corrected_datasets) %>%
    arrange(country, DateTime) %>%
    select(country, DateTime, CTY, summary)
}


