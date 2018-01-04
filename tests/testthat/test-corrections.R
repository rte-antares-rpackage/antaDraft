context("corrections")

test_that("null values are not valid and not corrected", {

  aggregated_db <- structure(list(country = c(
  "SPAIN", "SPAIN", "SPAIN", "SPAIN",
  "SPAIN"
), DateTime = structure(c(
  1461546000, 1461553200, 1473271200,
  1475038800, 1475694000
), class = c("POSIXct", "POSIXt")), BZN = c(
  0,
  0, 0, 0, 0
), CTA = c(21486, 27283, 34159, 31296, 26870), CTY = c(
  0,
  0, 0, 0, 0
), CTY_NA = c(TRUE, TRUE, TRUE, TRUE, TRUE), CTA_NA = c(
  TRUE,
  TRUE, TRUE, TRUE, TRUE
), BZN_NA = c(TRUE, TRUE, TRUE, TRUE, TRUE), CTY_IS_POS = c(FALSE, FALSE, FALSE, FALSE, FALSE), CTA_IS_POS = c(
  TRUE,
  TRUE, TRUE, TRUE, TRUE
), BZN_IS_POS = c(
  FALSE, FALSE, FALSE,
  FALSE, FALSE
), CTY_CTA_EQUAL = c(TRUE, TRUE, TRUE, TRUE, TRUE), CTY_BZN_EQUAL = c(TRUE, TRUE, TRUE, TRUE, TRUE), CTA_BZN_EQUAL = c(
  TRUE,
  TRUE, TRUE, TRUE, TRUE
), CTY_CTA_DIFF_LT_05 = c(
  TRUE, TRUE, TRUE,
  TRUE, TRUE
), CTY_BZN_DIFF_LT_05 = c(TRUE, TRUE, TRUE, TRUE, TRUE), CTA_BZN_DIFF_LT_05 = c(TRUE, TRUE, TRUE, TRUE, TRUE), CTY_CTA_DIFF_LT_10 = c(
  FALSE,
  FALSE, FALSE, FALSE, FALSE
), CTY_BZN_DIFF_LT_10 = c(
  FALSE, FALSE,
  FALSE, FALSE, FALSE
), CTA_BZN_DIFF_LT_10 = c(
  FALSE, FALSE, FALSE,
  FALSE, FALSE
), CTY_LAG_LT_30 = c(
  FALSE, FALSE, FALSE, FALSE,
  FALSE
), CTA_LAG_LT_30 = c(TRUE, TRUE, TRUE, TRUE, TRUE), BZN_LAG_LT_30 = c(
  FALSE,
  FALSE, FALSE, FALSE, FALSE
)), class = "data.frame", row.names = c(
  NA,
  -5L
), validators = c(
  "CTY_NA", "CTA_NA", "BZN_NA", "CTY_IS_POS",
  "CTA_IS_POS", "BZN_IS_POS", "CTY_CTA_EQUAL", "CTY_BZN_EQUAL",
  "CTA_BZN_EQUAL", "CTY_CTA_DIFF_LT_05", "CTY_BZN_DIFF_LT_05",
  "CTA_BZN_DIFF_LT_05", "CTY_CTA_DIFF_LT_10", "CTY_BZN_DIFF_LT_10",
  "CTA_BZN_DIFF_LT_10", "CTY_LAG_LT_30", "CTA_LAG_LT_30", "BZN_LAG_LT_30"
), id.vars = c("country", "DateTime"), timevar = "DateTime", .Names = c(
  "country",
  "DateTime", "BZN", "CTA", "CTY", "CTY_NA", "CTA_NA", "BZN_NA",
  "CTY_IS_POS", "CTA_IS_POS", "BZN_IS_POS", "CTY_CTA_EQUAL", "CTY_BZN_EQUAL",
  "CTA_BZN_EQUAL", "CTY_CTA_DIFF_LT_05", "CTY_BZN_DIFF_LT_05",
  "CTA_BZN_DIFF_LT_05", "CTY_CTA_DIFF_LT_10", "CTY_BZN_DIFF_LT_10",
  "CTA_BZN_DIFF_LT_10", "CTY_LAG_LT_30", "CTA_LAG_LT_30", "BZN_LAG_LT_30"
))

  test_db <- data_correct_with_rules(aggregated_db)

  expect_equivalent(aggregated_db[,c("BZN", "CTY", "CTA")], test_db[,c("BZN", "CTY", "CTA")])

})



test_that("CTY is corrected", {
  aggregated_db <- structure(
    list(country = "SWITZERLAND",
         DateTime = structure(1461614400, class = c("POSIXct", "POSIXt")),
         BZN = 6296.81, CTA = 6296.81, CTY = 0),
    .Names = c("country", "DateTime", "BZN", "CTA", "CTY"),
    id.vars = c("country", "DateTime"),
    timevar = "DateTime", measures = c("CTY", "CTA", "BZN"),
    countryvar = "country", row.names = 298285L,
    class = c("aggregated", "data.frame"))

  aggregated_db <- augment_validation(aggregated_db)
  aggregated_db <- data_correct_with_rules(aggregated_db)
  expect_equivalent(aggregated_db[,c("CTY")], aggregated_db[,c("CTA")])
})
