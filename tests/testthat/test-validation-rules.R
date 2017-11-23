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

  expect_equal(aggregated_db$BZN_LAG_LT_30, c(FALSE, FALSE) )
})

