context("check aggregated data")




val_rules <- system.file(package = "entsoe", "templates/aggregated/validation.yml")
fp_rules <- system.file(package = "entsoe", "templates/aggregated/false_positives.yml")
correct_rules <- "correct_agg_01.yaml"


test_that("correction are made", {
  data("fr_agg")
  agg_data <- augment_validation(fr_agg, val_rules = val_rules,
                                 fp_rules = fp_rules)
  out <- data_correct(agg_data, corrections_rules = correct_rules)

  expect_true(is.na(agg_data$CTA[1]))
  expect_true(out$rule_0002[1])
  expect_equal(out$CTA[1], out$CTY[1])
})

test_that("complex aggregations are valid", {
  data(sample_composite)
  class(sample_composite) <- c("data.frame", "raw_level")

  val_rules <- system.file(package = "entsoe", "templates/raw/validate.yml")
  fp_rules <- system.file(package = "entsoe", "templates/raw/false_positives.yml")

  agg_db <- augment_validation(sample_composite, val_rules = val_rules, fp_rules = fp_rules) %>%
    aggregate_with_rules()

  expect_equal(agg_db$BZN, agg_db$CTA)
  expect_equal(agg_db$BZN, agg_db$CTY)
})

