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

  ctry_file <- tempfile(fileext = ".yml")
  cat("BELGIUM:\n  CTY:\n    - BE\n  CTA:\n    - BE\n  BZN:\n    - BE\n",
      file = ctry_file)

  antaDraft::set_antadraft_load_option(atc_per_country = ctry_file)

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
  931681L, 931682L, 931683L), class = c("data.frame", "load_raw",
  "controled"))

  aggregated_db <- agg_data(raw_belgium)
  aggregated_db <- aggregated_db[aggregated_db$country %in% c("BELGIUM"),]

  aggregated_db <- augment_validation(aggregated_db)

  expect_equal( aggregated_db$CTY_NA, c(TRUE, FALSE, TRUE, TRUE) )
  expect_equal( aggregated_db$BZN_NA, c(TRUE, FALSE, TRUE, TRUE) )
  expect_equal( aggregated_db$CTA_NA, c(TRUE, FALSE, TRUE, TRUE) )

  expect_true(all(aggregated_db$CTY_LAG_LT_30 ) )
  expect_true(all(aggregated_db$BZN_LAG_LT_30 ) )
  expect_true(all(aggregated_db$CTA_LAG_LT_30 ) )



  data_neg_val <- structure(list(country = rep("LUXEMBOURG", 24),
  DateTime = structure(seq(1502582400, to = 1502665200, by = 3600),
  class = c("POSIXct", "POSIXt")), BZN = c(346.000000000003,
  352.000000000003, 354.999999999998, 330.000000000002, 345.000000000003,
  363.999999999998, 367, 374,
  389.999999999997, 373, 351, 342, 339, 344, 367, 386, -36032.64,
  398.999999999998, 414.000000000005, 412.000000000005, 381.999999999998,
  351.999999999998, 336.000000000002, 332.999999999995), CTA = c(346,
  352, 355, 330, 345, 364, 367, 374, 390, 373, 351, 342, 339, 344,
  367, 386, 401, 399, 414, 412, 382, 352, 336, 333), CTY = c(346,
  352, 355, 330, 345, 364, 367, 374, 390, 373, 351, 342, 339, 344,
  367, 386, 401, 399, 414, 412, 382, 352, 336, 333)), .Names = c("country",
  "DateTime", "BZN", "CTA", "CTY"), row.names = 179664:179687,
  class = c("data.frame",
  "aggregated"))

  aggregated_db <- augment_validation(data_neg_val)
  expect_is(aggregated_db$DateTime, "POSIXct")
  resToCheck <- aggregated_db[as.character(aggregated_db$DateTime)=="2017-08-13 18:00:00",
                         "BZN_LAG_LT_30"]
  if(!resToCheck){
    expect_error(max(2), warning(paste0("aggregated_db[aggregated_db$DateTime==2017-08-13 18:00:00,
                         BZN_LAG_LT_30]", " is not true" )))
    expect_error(max(2), warning(paste0( " value : " , aggregated_db[aggregated_db$DateTime=="2017-08-13 18:00:00",
                                                      ])))
    expect_error(max(2), warning(paste0( " date : " , aggregated_db[as.character(aggregated_db$DateTime)=="2017-08-13 18:00:00",
                        "DateTime"] )))
  }
  expect_true(resToCheck)

  #dont work in appveyor
  #expect_true(aggregated_db[data.table::hour(aggregated_db$DateTime) %in% 18,
  #                          "BZN_LAG_LT_30"])

})

