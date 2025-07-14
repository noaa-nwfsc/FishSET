library(shinytest2)

test_that("MainApp works", {
  skip_on_cran() # Skip this test on CRAN

  app_dir <- system.file("ShinyFiles/MainApp", package = "FishSET")

  # Ensure the app directory exists
  expect_true(dir.exists(app_dir), info = "App directory does not exist")
  expect_true(nchar(app_dir) > 0, info = "App directory path is empty")

  # Run shinytest2 tests
  shinytest2::test_app(app_dir, test_file = NULL)
})


