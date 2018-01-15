#' @export
#' @importFrom ggplot2 ggplot aes_string geom_line scale_y_continuous theme_minimal
#' @title plot an aggregated dataset
#' @description display on a new plot aggregated time serie
#' @param x aggregated dataset (returned by \code{agg_data()})
#' @param y one of the available measure (.i.e CTY, CTA, BZN)
#' @param subset a logical vector used to subset x before plotting
#' @param ... unused parameter
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
#' load_data <- anta_load_read(data_dir = load_dir )
#' load_data <- augment_validation(data = load_data)
#' aggregated_db <- agg_data(load_data)
#' plot(aggregated_db)
plot.aggregated <- function( x, y = "CTY", subset = NULL, ...){
  x <- augment_seasons_id(x)
  meta <- capture_df_meta(x)

  stopifnot(y %in% meta$measures)

  if( !is.null(subset) )
    x <- x[subset,]

  gg <- ggplot(data = x,
         mapping = aes_string(x = meta$timevar, y = y)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA),
                       labels = function(x) sprintf("%.0f", x)  ) +
    theme_minimal() + facet_wrap(meta$countryvar, scales = "free")
  print(gg)
  invisible()
}

#' @export
#' @importFrom ggplot2 geom_segment arrow unit facet_wrap scale_x_datetime geom_jitter
#' @importFrom ggplot2 scale_color_gradient facet_wrap
#' @importFrom lubridate seconds_to_period
#' @title plot qualcon object
#' @description display on a new plot qualcon informations
#' @param x qualcon object
#' @param subset a logical vector used to subset x before plotting
#' @param ... unused parameter
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
#' load_data <- anta_load_read(data_dir = load_dir )
#' load_data <- augment_validation(load_data)
#'
#' raw_qualcon <- qualcon(load_data)
#' plot(raw_qualcon)
#'
#' aggregated_db <- agg_data(load_data)
#' aggregated_db <- augment_validation(aggregated_db)
#'
#' add_qualcon <- qualcon(aggregated_db)
#' plot(add_qualcon)
plot.qualcon_agg <- function( x, subset = NULL, ...){
  meta <- capture_df_meta(x)

  if( !is.null(subset) )
    data <- x[subset,]
  else data <- x

  data$duration <- as.integer( data$end - data$start )
  data$interval <- data$duration > 0
  data$happen_at <- data$end
  data$happen_at[data$interval] <- NA
  data$happen_at_h <- hour(data$happen_at)
  data$end[!data$interval] <- NA
  data$start[!data$interval] <- NA

  ggplot( data = data ) +
    geom_segment(
      mapping = aes_string(x = "start", xend = "end",
                           y = "validator", yend = "validator", color = "duration"),
      size = 3) +
    geom_jitter(
      mapping = aes_string(x = "happen_at", y = "validator", fill = "happen_at_h"),
      width = 0, shape = 21, color = "transparent", alpha = .5 ) +
    theme_minimal() + scale_x_datetime() +
    scale_color_gradient(name = "Duration", low = "#E50008", high = "#5C5C5C", labels = seconds_to_period)
}

#' @export
#' @rdname plot.qualcon_agg
plot.qualcon_raw <- plot.qualcon_agg


#' @export
#' @import UpSetR
#' @title plot validators
#' @description display on a new plot validators grouped by intersections.
#' @param x controled dataset (returned by \code{augment_validation()})
#' @param subset a logical vector used to subset x before plotting
#' @param ... parameters to be sent to \code{\link[UpSetR]{upset}}
#' @examples
#' load_dir <- system.file(package = "antaDraft", "data_sample")
#' load_data <- anta_load_read(data_dir = load_dir )
#' load_data <- augment_validation(data = load_data)
#' aggregated_db <- agg_data(load_data)
#' aggregated_db <- augment_validation(aggregated_db)
#' plot( aggregated_db, nsets = 7 )
plot.controled <- function(x, subset = NULL, ...){
  meta <- capture_df_meta(x)

  if( !is.null(subset) )
    data <- x[subset,]
  else data <- x

  data <- data[, meta$validators, drop = FALSE]
  data <- lapply(data, function(x) as.integer(!x))
  data <- as.data.frame(data)
  upset(data, ...)
  invisible()
}



