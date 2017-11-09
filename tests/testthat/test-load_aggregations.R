context("aggregations")

test_that("aggregation with reference", {

  data(load_gerluxaus_20150115)
  agg_db <- aggregate_with_rules(load_gerluxaus_20150115)
  expect_equal(agg_db$BZN, agg_db$CTA)
  expect_equal(agg_db$CTA, agg_db$CTY)
})
