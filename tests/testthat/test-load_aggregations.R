context("aggregations")

test_that("aggregation with reference", {
  data(load_gerluxaus_20150115)
  agg_db <- aggregate_with_rules(load_gerluxaus_20150115)
  expect_equal(agg_db$BZN, agg_db$CTA)
  expect_equal(agg_db$CTA, agg_db$CTY)
})


test_that("walk through #25", {

  UK_NIE_20170427210000 <- structure(list(DateTime = structure(c(
    1493319600, 1493319600,
    1493319600, 1493319600, 1493319600, 1493319600, 1493319600, 1493319600,
    1493319600
  ), class = c("POSIXct", "POSIXt")), MapCode = c(
    "IE",
    "IE", "IE_SEM", "IE_SEM", "NIE", "NIE", "GB", "GB", "GB"
  ), AreaTypeCode = c(
    "CTA",
    "CTY", "BZN", "BZN", "CTA", "CTY", "BZN", "CTA", "CTY"
  ), country = c(
    "IRELAND",
    "IRELAND", "IRELAND", "NORTH_IRELAND", "NORTH_IRELAND", "NORTH_IRELAND",
    "UK", "UK", "UK"
  ), TotalLoadValue = c(
    3445.83, 3445.83, 4497.83,
    4497.83, 1052, NA, 36250, 36250, 37302
  ), observed = c(
    TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
  ), IS_OBS = c(
    TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
  ), IS_FINITE = c(
    TRUE,
    TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE
  ), IS_POS = c(
    TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
  )), .Names = c(
    "DateTime",
    "MapCode", "AreaTypeCode", "country", "TotalLoadValue", "observed",
    "IS_OBS", "IS_FINITE", "IS_POS"
  ), row.names = c(
    991474L, 991475L,
    991476L, 991491L, 991492L, 991493L, 991503L, 991504L, 991505L
  ), class = c("data.frame", "raw_level", "controled"))

  agg_db <- aggregate_with_rules(UK_NIE_20170427210000)
  expect_equal(agg_db$BZN, agg_db$CTA)
  expect_equal(agg_db$CTY, c(3445.83, NA, 36250.00))

  agg_db <- augment_validation(agg_db)


  expect_equivalent(agg_db$CTY_NA, c(TRUE, FALSE, TRUE) )
  expect_true(all(agg_db$CTY_CTA_EQUAL))
  expect_true(all(agg_db$CTY_BZN_EQUAL))
  expect_true(all(agg_db$CTA_BZN_EQUAL))


})
