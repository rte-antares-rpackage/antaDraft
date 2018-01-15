context("quality control on load data")

test_that("expected columns", {
  load_dir <- system.file(package = "antaDraft", "data_sample")
  load_data <- anta_load_read(data_dir = load_dir )
  load_data <- augment_validation(load_data)

  qc_raw <- qualcon(load_data)
  expect_true( all( c("country", "validator", "start", "end") %in% names(qc_raw) ) )
  expect_is(qc_raw$start, "POSIXct")
  expect_is(qc_raw$end, "POSIXct")


  aggregated_db <- agg_data(load_data)
  aggregated_db <- augment_validation(aggregated_db)

  qc_agg <- qualcon(aggregated_db)
  expect_true( all( c("country", "validator", "start", "end") %in% names(qc_agg) ) )
  expect_is(qc_agg$start, "POSIXct")
  expect_is(qc_agg$end, "POSIXct")
})

test_that("reporting files", {
  load_dir <- system.file(package = "antaDraft", "data_sample")
  load_data <- anta_load_read(data_dir = load_dir )
  load_data <- augment_validation(load_data)

  qc_raw <- qualcon(load_data)
  qc_raw_dir <- file.path( tempfile(), "raw_qc" )
  dir.create(qc_raw_dir, recursive = TRUE, showWarnings = FALSE)
  render_quality(qc_raw, qc_raw_dir)
  combs <- aggregate(qc_raw$start,
            by = list( country = qc_raw$country,
                       AreaTypeCode = qc_raw$AreaTypeCode,
                       MapCode = qc_raw$MapCode, validator = qc_raw$validator),
            FUN = length )
  expected_file_names <- paste0(combs$country, "_", combs$AreaTypeCode, "_", combs$MapCode, "_", combs$validator, ".md")
  expect_identical( sort( list.files(qc_raw_dir) ), sort(expected_file_names) )

  aggregated_db <- agg_data(load_data)
  aggregated_db <- augment_validation(aggregated_db)

  qc_agg <- qualcon(aggregated_db)
  qc_agg_dir <- file.path( tempfile(), "agg_qc" )
  dir.create(qc_agg_dir, recursive = TRUE, showWarnings = FALSE)
  render_quality(qc_agg, qc_agg_dir)
  combs <- aggregate(qc_agg$start,
                     by = list( country = qc_agg$country, validator = qc_agg$validator),
                     FUN = length )
  expected_file_names <- paste0(combs$country, "_", combs$validator, ".md")
  expect_identical( sort( list.files(qc_agg_dir) ), sort(expected_file_names) )

})

