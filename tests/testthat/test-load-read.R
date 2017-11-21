context("load reading")

test_that("expected columns", {
  load_dir <- system.file(package = "antaDraft", "data_sample")
  load_data <- anta_load_read(data_dir = load_dir )

  expect_true( all( c("MapCode", "AreaTypeCode", "DateTime", "AreaName", "TotalLoadValue",
    "country", "observed") %in% names(load_data) ) )

  expect_is(load_data$DateTime, "POSIXct")
  expect_is(load_data$TotalLoadValue, "numeric")

})

test_that("missing combinations are added", {
  load_dir <- system.file(package = "antaDraft", "data_sample")
  load_data <- anta_load_read(data_dir = load_dir )

  likely_not_missing <- !is.na( load_data$TotalLoadValue ) & !is.na( load_data$MapCode )
  expect_equal(nrow(load_data[likely_not_missing,]), sum(load_data$observed, na.rm = TRUE))
})
