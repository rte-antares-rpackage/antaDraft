context("check raw data")
source(file = "zz_utils.R")



dataset1 <- seq_pattern_on_hours( c(1, 3:48) )

test_that("countries can be found", {
  raw_data <- seq_pattern_on_hours( 1:2 )
  data_with_dims <- augment_rules(raw_data, file_rules = "cty_rules_fr.yaml")
  expect_true( all( data_with_dims$country %in% "FRANCE" ) )
})

test_that("column observed is added", {
  raw_data <- seq_pattern_on_hours( 1:2 )
  data_with_dims <- augment_rules(raw_data, file_rules = "cty_rules_fr.yaml")
  expect_true( all( data_with_dims$observed ) )

  raw_data <- seq_pattern_on_hours( c(1,3) )
  data_with_dims <- augment_rules(raw_data, file_rules = "cty_rules_fr.yaml")
  missing_dt_id <- which( lubridate::hour(data_with_dims$DateTime) == 2 )
  expect_equal( data_with_dims$observed[missing_dt_id], rep(FALSE, 3) )
})

test_that("qualcon output", {

  raw_data <- seq_pattern_on_hours( c(1,4:5) )

  data_with_dims <- augment_rules(raw_data, file_rules = "cty_rules_fr.yaml")

  validated_data <- augment_validation(data = data_with_dims, val_rules = "raw_validate.yml", fp_rules = "raw_false_positives.yml")

  expect_true( all( c("observed", "IS_OBS", "IS_FINITE", "IS_POS", "VALIDATED") %in% names(validated_data ) ) )

  missing_dt_id <- which( lubridate::hour(data_with_dims$DateTime) %in% 2:3 )
  expect_equal( data_with_dims$observed[missing_dt_id], rep(FALSE, 6) )
  expect_equal( validated_data$VALIDATED[missing_dt_id], rep(FALSE, 6) )

  non_missing_dt_id <- which( !lubridate::hour(validated_data$DateTime) %in% 2:3 )
  expect_true( all( validated_data$VALIDATED[non_missing_dt_id] ) )
})


