context("add HPS to virtual PSP")


test_that("create PSP with installed capacity", {
  ## Get the data
  prod_type_dir <- system.file(package = "antaDraft", "data_sample",
                               "prod_sample_20160114_BE_FR/B01")
  capacity_dir <- system.file(package = "antaDraft", "data_sample",
                              "prod_sample_20160114_BE_FR/B06")

  global_options <- getOption("global_options")

  prod_type <- read_prod_type(
    production_dir = prod_type_dir,
    capacity_dir = capacity_dir,
    production_file = global_options$hps_production_per_country
  )

  prod_type <- prod_type[prod_type$production_type %in% c("Hydro Pumped Storage"), ]

  prod_type_valid <- augment_validation(prod_type)
  #run
  add_hps_to_project_in_virtualPsp(prod_type_valid,
                                   overwrite = TRUE,
                                   efficiency = 0.75,
                                   timeStepBindConstraint = "weekly")

  #test / check
  expect_true("austria" %in% antaresRead::getAreas())
  #create two virtual areas
  expect_true("psp_in_w" %in% antaresRead::getAreas())
  expect_true("psp_out_w" %in% antaresRead::getAreas())
  #create virtual links
  expect_true("france - psp_in_w" %in% antaresRead::getLinks())
  expect_true("france - psp_out_w" %in% antaresRead::getLinks())
  #check the capacity
  capaPSP <- readInputTS(linkCapacity = "belgium - psp_out_w",
                       showProgress = FALSE,
                       opts = opts)
  expect_equal(unique(capaPSP$transCapacityIndirect), 1308)
  expect_equal(unique(capaPSP$hurdlesCostIndirect), 0.0005)
  #check the efficiency
  binding <- readBindingConstraints(opts = opts)
  #for R CMD Check
  if (is.na(binding$belgium_psp_weekly$coefs["belgium%psp_in_w"])){
    efficiencyTest <- as.double(binding$belgium_psp_weekly$coefs["psp_in_w%belgium"])
  } else{
    efficiencyTest <- as.double(binding$belgium_psp_weekly$coefs["belgium%psp_in_w"])
  }
  expect_equal(efficiencyTest, 0.75)
  expect_equal(binding$belgium_psp_weekly$operator, "equal")
  expect_equal(binding$belgium_psp_weekly$timeStep, "weekly")
  expect_equal(binding$belgium_psp_weekly$enabled, TRUE)
})

test_that("wrong data", {
  expect_error(add_hps_to_project_in_virtualPsp(c("98"),
                                                overwrite = TRUE,
                                                efficiency = 0.75,
                                                timeStepBindConstraint = "weekly"),
               "data must be a data.frame.")

  pspData <- data.frame(area = c("a", "b"), installedCpacity22 = c(800, 900))
  expect_error(add_hps_to_project_in_virtualPsp(pspData,
                                                overwrite = TRUE,
                                                efficiency = 0.75,
                                                timeStepBindConstraint = "weekly"),
               "data should contain installed_capacity or installedCapacity")
  pspData <- data.frame(area = c("a", "b"), installedCapacity = c(800, 900))
  expect_error(add_hps_to_project_in_virtualPsp(pspData,
                                                overwrite = TRUE,
                                                efficiency = 0.75,
                                                timeStepBindConstraint = "weekly"),
               "data should contain production_type")
  pspData <- data.frame(area = c("a", "b"),
                      installedCapacity = c(800, 900),
                      production_type = c("Hydro Pumped Storagppe"))
  expect_error(add_hps_to_project_in_virtualPsp(pspData,
                                                overwrite = TRUE,
                                                efficiency = 0.75,
                                                timeStepBindConstraint = "weekly"),
               "data should contain AreaTypeCode")
  pspData <- data.frame(area = c("a", "b"),
                      installedCapacity = c(800, 900),
                      production_type = c("Hydro Pumped Storagppe"),
                      AreaTypeCode = c("BZN"))
  expect_error(add_hps_to_project_in_virtualPsp(pspData,
                                                overwrite = TRUE,
                                                efficiency = 0.75,
                                                timeStepBindConstraint = "weekly"),
               "AreaTypeCode should contain CTY")
  pspData <- data.frame(area = c("a", "b"),
                      installedCapacity = c(800, 900),
                      production_type = c("Hydro pppe"),
                      AreaTypeCode = c("CTY"))
  expect_error(add_hps_to_project_in_virtualPsp(pspData,
                                                overwrite = TRUE,
                                                efficiency = 0.75,
                                                timeStepBindConstraint = "weekly"),
               "data should contain generation_output")
  pspData <- data.frame(area = c("a", "b"),
                      installedCapacity = c(800, 900),
                      production_type = c("Hydro Pumped Storage"),
                      AreaTypeCode = c("CTY"),
                      generation_output = c(100, 599))
  expect_error(add_hps_to_project_in_virtualPsp(pspData,
                                                overwrite = TRUE,
                                                efficiency = 0.75,
                                                timeStepBindConstraint = "weekly"),
               "data should contain country")
  pspData <- data.frame(country = c("a", "b"),
                        installedCapacity = c(800, 900),
                        production_type = c("Hydro Pumped Stge"),
                        AreaTypeCode = c("CTY"),
                        generation_output = c(100, 599))
  expect_error(add_hps_to_project_in_virtualPsp(pspData,
                                                overwrite = TRUE,
                                                efficiency = 0.75,
                                                timeStepBindConstraint = "weekly"),
               "production_type should contain Hydro Pumped Storage")

})
