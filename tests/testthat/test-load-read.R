context("load reading")

test_that("expected columns", {
  load_dir <- system.file(package = "antaDraft", "data_sample")
  load_data <- anta_load_read(data_dir = load_dir )

  expect_true( all( c("MapCode", "AreaTypeCode", "DateTime", "TotalLoadValue",
    "country", "observed") %in% names(load_data) ) )

  expect_is(load_data$DateTime, "POSIXct")
  expect_is(load_data$TotalLoadValue, "numeric")

})

test_that("missing combinations are added", {
  load_dir <- system.file(package = "antaDraft", "data_sample")
  load_data <- anta_load_read(data_dir = load_dir )

  num_comb_by_dates <- aggregate(load_data$TotalLoadValue, list(load_data$DateTime), length)
  expect_true(all( diff( num_comb_by_dates$x ) == 0 ))
})
