context("validating rows")

test_that("expected results with lag less than 30 percent", {

  aggregated_db <- structure(
    list(country = c("AUSTRIA", "AUSTRIA"),
         DateTime = structure(c(1418245200, 1418248800), class = c("POSIXct", "POSIXt")),
         BZN = c(7238.4, 14024.95),
         CTA = c(7238.4, 7242.4),
         CTY = c(7238.4, 7242.4)),
    .Names = c("country", "DateTime", "BZN", "CTA", "CTY" ),
    id.vars = c("country", "DateTime"), timevar = "DateTime", row.names = 1:2,
    class = c( "data.frame", "aggregated" ) )

  aggregated_db <- augment_validation(aggregated_db)

  expect_equal(aggregated_db$BZN_LAG_LT_30, c(TRUE, FALSE) )
})

test_that("issue 18 with LAG", {

  raw_belgium <- structure(list(DateTime = structure(c(1488733200, 1488733200,
  1488733200, 1488736800, 1488736800, 1488736800, 1488740400, 1488740400,
  1488740400, 1488744000, 1488744000, 1488744000), class = c("POSIXct",
  "POSIXt")), MapCode = c("BE", "BE", "BE", "BE", "BE", "BE", "BE",
  "BE", "BE", "BE", "BE", "BE"), AreaTypeCode = c("BZN", "CTA",
  "CTY", "BZN", "CTA", "CTY", "BZN", "CTA", "CTY", "BZN", "CTA",
  "CTY"), country = c("BELGIUM", "BELGIUM", "BELGIUM", "BELGIUM",
  "BELGIUM", "BELGIUM", "BELGIUM", "BELGIUM", "BELGIUM", "BELGIUM",
  "BELGIUM", "BELGIUM"), TotalLoadValue = c(10816.45, 10816.45,
  10816.45, NA, NA, NA, 10128.71, 10128.71, 10128.71, 9845.97,
  9845.97, 9845.97), observed = c(TRUE, TRUE, TRUE, TRUE, TRUE,
  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), IS_OBS = c(TRUE, TRUE,
  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
      IS_FINITE = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE), IS_POS = c(TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)), .Names = c("DateTime",
  "MapCode", "AreaTypeCode", "country", "TotalLoadValue", "observed",
  "IS_OBS", "IS_FINITE", "IS_POS"), row.names = c(931540L, 931541L,
  931542L, 931587L, 931588L, 931589L, 931634L, 931635L, 931636L,
  931681L, 931682L, 931683L), class = c("data.frame", "raw_level",
  "controled"))

  aggregated_db <- aggregate_with_rules(raw_belgium)
  aggregated_db <- augment_validation(aggregated_db)

  expect_equal( aggregated_db$CTY_NA, c(TRUE, FALSE, TRUE, TRUE) )
  expect_equal( aggregated_db$BZN_NA, c(TRUE, FALSE, TRUE, TRUE) )
  expect_equal( aggregated_db$CTA_NA, c(TRUE, FALSE, TRUE, TRUE) )

  expect_true(all(aggregated_db$CTY_LAG_LT_30 ) )
  expect_true(all(aggregated_db$BZN_LAG_LT_30 ) )
  expect_true(all(aggregated_db$CTA_LAG_LT_30 ) )


})

