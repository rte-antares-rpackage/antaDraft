.onLoad= function(lib, pkg){



  global_options <- list(
    atc_per_country = system.file(package = package_name, "config", "global", "atc_per_country.yml"),
    thermal_production_per_country = system.file(package = package_name, "config", "global", "thermal_production.yml"),
    renewable_production_per_country = system.file(package = package_name, "config", "global", "renewable_production.yml"),
    hps_production_per_country = system.file(package = package_name, "config", "global", "hps_production.yml")
  )
  options("global_options" = global_options)

  yml_file <- function( f )
    system.file(package = package_name, "config", "load", f)
  load_options <- list(
    validate = list(
      raw = list( validate = yml_file("raw_validate.yml"), false_pos = yml_file("raw_fp.yml") ),
      agg = list( validate = yml_file("agg_validate.yml"), false_pos = yml_file("agg_fp.yml") )
      ),
    correct = yml_file("agg_correct.yml")
    )

  options("load_options" = load_options)

  yml_file <- function( f )
    system.file(package = package_name, "config", "production", f)

  prod_options <- list(
    validate = list(
      raw = list( validate = yml_file("raw_validate.yml"),
                  false_pos = yml_file("raw_fp.yml") ),
      groupes_raw = list( validate = yml_file("raw_validate.yml"),
                          false_pos = yml_file("raw_fp.yml") ),
      agg = list( validate = yml_file("agg_validate.yml"),
                  false_pos = yml_file("agg_fp.yml") ),
      cmp_typ_grp = list(validate = yml_file("compare_validate.yml"),
                         false_pos = yml_file("compare_fp.yml") )
      )
    )

  options("prod_options" = prod_options)

  invisible()
}

#' @export
#' @title set options for load data processing
#' @description set options for load data processing by specifying
#' \code{yaml} files to be used at various stages.
#' @param atc_per_country rules used when aggregating data
#' @param raw_val rules to validate raw data
#' @param raw_val_fp rules to exclude irrelevant raw validations because defined as false positive
#' @param agg_val rules to validate aggregated data
#' @param agg_val_fp rules to exclude irrelevant aggregation validations because defined as false positive
#' @param correct_rules correction rules for aggregated data
set_antadraft_load_option <- function( atc_per_country = NULL, raw_val = NULL, raw_val_fp = NULL,
                                       agg_val = NULL, agg_val_fp = NULL, correct_rules = NULL){

  global_options <- getOption("global_options")
  if( !is.null(atc_per_country) ){
    global_options$atc_per_country <- atc_per_country
  }
  options("global_options" = global_options)


  load_options <- getOption("load_options")

  if( !is.null(raw_val) ){
    load_options$validate$raw$validate <- raw_val
  }
  if( !is.null(raw_val_fp) ){
    load_options$validate$raw$false_pos <- raw_val_fp
  }

  if( !is.null(agg_val) ){
    load_options$validate$agg$validate <- agg_val
  }
  if( !is.null(agg_val_fp) ){
    load_options$validate$agg$false_pos <- agg_val_fp
  }
  if( !is.null(correct_rules) ){
    load_options$correct <- correct_rules
  }

  options("load_options" = load_options)

  invisible(load_options)
}



