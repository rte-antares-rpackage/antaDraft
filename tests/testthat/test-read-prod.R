context("prod reading")

tmpfile <- tempfile(fileext = ".yml")
cat("FRANCE:
- Fossil Gas
- Fossil Hard coal
- Fossil Oil
- Nuclear
- Other
", file = tmpfile)

prod_type_dir <- system.file(package = "antaDraft", "data_sample",
                              "prod_sample_20160129/B01")
prod_group_dir <- system.file(package = "antaDraft", "data_sample",
                              "prod_sample_20160129/B02")
capacity_dir <- system.file(package = "antaDraft", "data_sample",
                            "prod_sample_20160129/B06")

test_that("expected columns", {

  prod_data <- read_prod_type(
    production_dir = prod_type_dir, capacity_dir = capacity_dir,
    production_file = tmpfile)

  expect_true( all( c("MapCode", "AreaTypeCode", "DateTime", "production_type",
                      "consumption", "generation_output", "installed_capacity",
                      "country", "observed") %in% names(prod_data) ) )

  expect_is(prod_data$DateTime, "POSIXct")
  expect_is(prod_data$consumption, "numeric")
  expect_is(prod_data$generation_output, "numeric")
  expect_is(prod_data$installed_capacity, "numeric")

  prod_data_agg <- agg_data(prod_data)
  expect_equal( c("DateTime", "country", "production_type", "BZN",
                  "CTA", "CTY"), names(prod_data_agg) )
  expect_is(prod_data_agg$BZN, "numeric")
  expect_is(prod_data_agg$CTA, "numeric")
  expect_is(prod_data_agg$CTY, "numeric")


  prod_data <- read_prod_group(production_dir = prod_group_dir, production_file = tmpfile)

  expect_true( all( c("MapCode", "AreaTypeCode", "DateTime", "production_type", "group_name",
                      "consumption", "generation_output", "installed_capacity",
                      "country", "observed") %in% names(prod_data) ) )

  expect_is(prod_data$DateTime, "POSIXct")
  expect_is(prod_data$consumption, "numeric")
  expect_is(prod_data$generation_output, "numeric")
  expect_is(prod_data$installed_capacity, "numeric")
})

test_that("missing combinations are added", {
  prod_data <- read_prod_type(
    production_dir = prod_type_dir, capacity_dir = capacity_dir,
    production_file = tmpfile)

  num_comb_by_dates <- aggregate(prod_data$consumption, list(prod_data$DateTime), length)
  expect_true(all( diff( num_comb_by_dates$x ) == 0 ))

  fr_prods <- yaml::yaml.load_file(tmpfile)$FRANCE
  expect_true( all( prod_data$production_type %in% fr_prods ) )
  expect_true( all( fr_prods %in% prod_data$production_type ) )
})



prod_type_dir <- system.file(package = "antaDraft", "data_sample",
                             "prod_sample_20160114_BE_FR/B01")
prod_group_dir <- system.file(package = "antaDraft", "data_sample",
                              "prod_sample_20160114_BE_FR/B02")
capacity_dir <- system.file(package = "antaDraft", "data_sample",
                            "prod_sample_20160114_BE_FR/B06")


test_that("minimum ensemble is giving dimensions", {

  prod_file <- tempfile(fileext = ".yml")
  cat("FRANCE:\n- Fossil Gas\n- Fossil Hard coal\n- Fossil Oil\n- Nuclear\nBELGIUM:\n- Fossil Gas\n- Fossil Hard coal\n- Fossil Oil\n- Nuclear\n",
      file = prod_file)

  ctry_file <- tempfile(fileext = ".yml")
  cat("FRANCE:\n  CTY:\n    - FR\n  CTA:\n    - FR\n  BZN:\n    - FR\n",
    file = ctry_file)

  antaDraft::set_antadraft_load_option(atc_per_country = ctry_file)

  prod_data <- read_prod_type(
    production_dir = prod_type_dir, capacity_dir = capacity_dir,
    production_file = prod_file)
  expect_true( all( unique(prod_data$country) %in% "FRANCE" ) )

})

