#' @export
#' @title compare channel and group production data
#' @description compare channel and group production data.
#' @param channel_prod data produced by \code{anta_prod_channel}
#' @param group_prod data produced by \code{anta_prod_group}
prod_compare <- function(channel_prod, group_prod){

  group_prod_data <- as.data.table(group_prod)
  group_prod_data <- group_prod_data[, list(
    group_generation = sum(generation_output, na.rm = TRUE),
    consumption = sum(consumption, na.rm = TRUE),
    group_capacity = sum(installed_capacity, na.rm = TRUE) ),
    by=c("country", "DateTime", "production_type")]
  group_prod_data$group_generation <- group_prod_data$group_generation + group_prod_data$consumption
  group_prod_data$consumption <- NULL

  channel_prod_data <- as.data.table(channel_prod[channel_prod$AreaTypeCode %in% "CTA",])
  channel_prod_data <- channel_prod_data[, list(channel_generation = sum(generation_output, na.rm = TRUE),
                                                consumption = sum(consumption, na.rm = TRUE),
                                                channel_capacity = sum(installed_capacity, na.rm = TRUE)
    ), by=c("country", "DateTime", "production_type")]
  channel_prod_data$channel_generation <- channel_prod_data$channel_generation + channel_prod_data$consumption
  channel_prod_data$consumption <- NULL

  data <- merge( x = group_prod_data, y = channel_prod_data,
                 by = c("DateTime", "production_type", "country"),
                 all = TRUE)

  # # not necessary as tables already has the correct dimensions
  # data <- ref_join_class(x = data, classobj = "channel_prod_agg", date_time = "DateTime")

  data <- as.data.frame(data)

  class(data) <- c(class(data), "channel_group_prod" )
  attr( data, "id.vars") <- c("country", "DateTime", "production_type")
  attr( data, "timevar") <- "DateTime"
  attr( data, "countryvar") <- "country"

  data
}

