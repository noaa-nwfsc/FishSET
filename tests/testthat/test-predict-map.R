

test_that("Test loading main data works", {
  # Note: unable to test successful upload due to the use of rstudioapi package
  # Make sure project names that violate rules throw errors
  expect_error(load_maindata(dat = FishSET::scallop, over_write = TRUE, project = "test load"))
  expect_error(load_maindata(dat = FishSET::scallop, over_write = TRUE, project = ""))
  expect_error(load_maindata(dat = cbind(FishSET::scallop, FishSET::scallop), over_write = TRUE, project = "testload"))
})

test_that("predict_map output works", {
  project <- 
  mod.name <-
  policy.name <- 
  spat <- 
  zone.spat <-
  plot_type <- 
  outsample <- FALSE
  outsample_pred <- NULL
  
  
})