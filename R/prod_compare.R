#Copyright © 2018 RTE Réseau de transport d’électricité

#' @export
#' @title compare production by types and production by groups
#' @description compare by types and by groups production data.
#' @param prod_type data produced by \code{read_prod_type}
#' @param group_prod data produced by \code{read_prod_group}
#' @examples
#' global_options <- getOption("global_options")
#' production_dir <- system.file(package = "antaDraft", "data_sample",
#'   "prod_sample_20160129/B01")
#' capacity_dir <- system.file(package = "antaDraft", "data_sample",
#'   "prod_sample_20160129/B06")
#' prod_by_types <- read_prod_type(production_dir, capacity_dir,
#'   production_file = global_options$thermal_production_per_country)
#' production_dir <- system.file(package = "antaDraft", "data_sample",
#'   "prod_sample_20160129/B02")
#' prod_by_groups <- read_prod_group(production_dir,
#'   production_file = global_options$thermal_production_per_country)
#' comparison <- prod_compare( prod_by_types, prod_by_groups )
#' comparison <- augment_validation(comparison)
#' qc_comp <- qualcon(comparison)
#' render_quality(qc_comp, "qc_cmp_dir")
prod_compare <- function(prod_type, group_prod){

  group_prod_data <- as.data.table(group_prod)
  group_prod_data <- group_prod_data[, list(
    group_generation = sum(generation_output, na.rm = TRUE),
    consumption = sum(consumption, na.rm = TRUE),
    group_capacity = sum(installed_capacity, na.rm = TRUE) ),
    by=c("country", "DateTime", "production_type")]
  group_prod_data$group_generation <- group_prod_data$group_generation + group_prod_data$consumption
  group_prod_data$consumption <- NULL

  prod_type_data <- as.data.table(prod_type[prod_type$AreaTypeCode %in% "CTA",])
  prod_type_data <- prod_type_data[, list(type_generation = sum(generation_output, na.rm = TRUE),
                                                consumption = sum(consumption, na.rm = TRUE),
                                                type_capacity = sum(installed_capacity, na.rm = TRUE)
    ), by=c("country", "DateTime", "production_type")]
  prod_type_data$type_generation <- prod_type_data$type_generation + prod_type_data$consumption
  prod_type_data$consumption <- NULL

  data <- merge( x = group_prod_data, y = prod_type_data,
                 by = c("DateTime", "production_type", "country"),
                 all = TRUE)

  # # not necessary as tables already has the correct dimensions
  # data <- ref_join_class(x = data, classobj = "prod_type_agg", date_time = "DateTime")

  data <- as.data.frame(data)

  class(data) <- c(class(data), "type_group_prod" )
  attr( data, "id.vars") <- c("production_type")
  attr( data, "timevar") <- "DateTime"
  attr( data, "countryvar") <- "country"
  attr( data, "label") <- "Comparison of Generation Capacity [14.1]"

  data
}

#' @export
#' @rdname augment_validation
augment_validation.type_group_prod <- function( data ){

  load_options <- getOption("prod_options")

  val_rules <- load_options$validate$cmp_typ_grp$validate
  fp_rules <- load_options$validate$cmp_typ_grp$false_pos

  add_validation_columns( val_rules_file = val_rules,
                          falsepos_rules_file = fp_rules,
                          data = data )
}

