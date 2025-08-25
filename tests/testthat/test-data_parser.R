# Testing functions in data_parser.R

# Test load_maindata
test_that("Test loading main data works", {
  # Note: unable to test successful upload due to the use of rstudioapi package
  # Make sure project names that violate rules throw errors
  expect_error(load_maindata(dat = FishSET::scallop, 
                             over_write = TRUE, 
                             project = "test load"))
  expect_error(load_maindata(dat = FishSET::scallop, 
                             over_write = TRUE, 
                             project = ""))
  expect_error(load_maindata(dat = cbind(FishSET::scallop, FishSET::scallop), 
                             over_write = TRUE, 
                             project = "testload"))
})

# Test load_spatial
test_that("Test loading spatial data works", {
  # Note: unable to test successful upload due to the use of rstudioapi package
  # Make sure project names that violate rules throw errors
  expect_error(load_spatial(spat = FishSET::tenMNSQR, 
                            over_write = TRUE, 
                            project = "testload", 
                            name = "spat name"))
  expect_error(load_spatial(spat = FishSET::tenMNSQR, 
                            over_write = TRUE, 
                            project = "testload", 
                            name = ""))
})