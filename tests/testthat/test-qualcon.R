context("qualcon use cases")


test_that("NA values are reported", {

  load_db <- read_load_files("repos_load_data/case_1d_1c_no_bzn")
  db <- fortify_from_rules(raw_db = load_db )
  db_erros <- qualcon(db, yaml_rules = "repos_load_data/case_1d_1c_no_bzn/validation_rules.yml")

  expect_equal( nrow(db_erros ), 1 )
  expect_false(db_erros$BZN_NA )
})

