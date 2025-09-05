# -------------------------------------------------------------------------------------------------
# File: test-outlier_check.R
# Purpose: 
# Description:
#
# Scenarios tested:
#
# Notes:
#
# -------------------------------------------------------------------------------------------------

# Create mock log_call() --------------------------------------------------------------------------
# Description: Override the FishSET::log_call() function to avoid errors with connecting when
#              running spat_qaqc() through unit tests
# Get a reference to the original log_call function from its namespace.
# We'll use this to restore it later.
original_log_call <- get("log_call", envir = as.environment("package:FishSET"))

# This is the fake function that will do nothing.
mock_log_call <- function(...) {
  # This function does nothing and returns invisibly.
  invisible(NULL)
}

# On exit, restore the original function to the namespace. This is crucial
# for clean testing and prevents side effects.
on.exit({
  assignInNamespace("log_call", original_log_call, ns = "FishSET")
})

# Now, replace the original log_call with our mock function.
assignInNamespace("log_call", mock_log_call, ns = "FishSET")

# More mock functions -----------------------------------------------------------------------------
original_save_plot <- get("save_plot", envir = as.environment("package:FishSET"))
original_save_nplot <- get("save_nplot", envir = as.environment("package:FishSET"))

# This is the fake function that will do nothing.
mock_save_plot <- function(...) {
  # This function does nothing and returns invisibly.
  invisible(NULL)
}

# This is the fake function that will do nothing.
mock_save_nplot <- function(...) {
  # This function does nothing and returns invisibly.
  invisible(NULL)
}

# On exit, restore the original function to the namespace. This is crucial
# for clean testing and prevents side effects.
on.exit({
  assignInNamespace("save_plot", original_save_plot, ns = "FishSET")
})

on.exit({
  assignInNamespace("save_nplot", mock_save_nplot, ns = "FishSET")
})


# Now, replace the original log_call with our mock function.
assignInNamespace("save_plot", mock_save_plot, ns = "FishSET")
assignInNamespace("save_nplot", mock_save_plot, ns = "FishSET")

# outlier_table -----------------------------------------------------------------------------------
## Test for the outlier_table() function ----------------------------------------------------------
test_that("test outlier_table() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Run the function
  result <- outlier_table(dat = "s1MainDataTable",
                          project = "s1",
                          x = "LANDED_OBSCURED")
  
  # Check that the result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check that the data frame has the correct columns
  expect_named(result, c("Vector", "outlier_check", "N", "mean", "median",
                         "SD", "min", "max", "NAs", "skew"))
  
  # Check that the statistics are calculated correctly
  expect_equal(result$mean[2], 13824.95, tolerance = 0.01)
  expect_equal(result$median[2], 16127.70, tolerance = 0.01)
  expect_equal(result$SD[2], 5949.98, tolerance = 0.01)
  expect_equal(result$min[2], 1435.44, tolerance = 0.01)
  expect_equal(result$max[2], 24994.48, tolerance = 0.01)
  expect_equal(result$NAs[2], 0, tolerance = 0.01)
  expect_equal(result$skew[2], -0.45, tolerance = 0.01)
})

## Test that outlier_table() function handles errors ----------------------------------------------
test_that("test outlier_table() handles errors", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Run the function
  expect_error(outlier_table(dat = "s1MainDataTable",
                             project = "s1",
                             x = "GEARCODE"))
})

# outlier_plot ------------------------------------------------------------------------------------
## Test the outlier_plot() function ---------------------------------------------------------------
test_that("test outlier_plot() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Test single page output (default)
  result_single <- outlier_plot(
    dat = "s1MainDataTable",
    project = "s1",
    x = "LANDED_OBSCURED",
    output.screen = FALSE
  )
  
  # The output of arrangeGrib is a 'gtable' object
  expect_s3_class(result_single, "gtable")
  
  # Test multipage output
  result_multi <- outlier_plot(
    dat = "s1MainDataTable",
    project = "s1",
    x = "LANDED_OBSCURED",
    pages = "multi",
    output.screen = FALSE
  ) 
  
  # Should return list
  expect_type(result_multi, "list")
  expect_named(result_multi, c("data_plot", "prob_plot"))
})

## Test the outlier_plot() handles errors ---------------------------------------------------------
test_that("test outlier_plot() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Test single page output (default)
  expect_error(
    outlier_plot(
      dat = "s1MainDataTable",
      project = "s1",
      x = "GEARCODE",
      output.screen = FALSE
    )  
  )
})

## Test outlier_plot() filtering ------------------------------------------------------------------
test_that("dat.remove filtering logic works as expected", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Test with a rule that should remove the outlier (100)
  p <- outlier_plot(
    dat = "s1MainDataTable",
    project = "s1",
    x = "LANDED_OBSCURED",
    dat.remove = "mean_2SD",
    pages = "multi",
    output.screen = FALSE
  )
  
  # Build the plot to inspect the data used for rendering the layers
  built_plot <- ggplot_build(p$data_plot)
  
  # The first layer is the red "removed" points
  # The second layer is the blue "kept" points (the filtered data)
  kept_data <- built_plot$data[[2]]
  expect_equal(nrow(kept_data), 1935)
  # The 'value' column is mapped to the 'y' aesthetic in the plot
  expect_false(76507.14 %in% round(kept_data$y, 2))
})


# outlier_remove ----------------------------------------------------------------------------------
# Test outlier_remove() works ---------------------------------------------------------------------
test_that("outlier_plot returns the correct object type", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Setup sample data
  sample_dat <- data.frame(
    value = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 100), # 100 is a clear outlier
    category = rep(c("A", "B"), each = 5),
    event_date = seq.Date(from = as.Date("2020-01-01"), by = "year", length.out = 10),
    non_numeric = letters[1:10]
  )
  
  # The "mean_2SD" rule should remove the value 100
  filtered_dat <- outlier_remove(
    dat = sample_dat,
    project = "s1",
    x = "value",
    dat.remove = "mean_2SD"
  )
  
  expect_equal(nrow(filtered_dat), 9)
  expect_false(100 %in% filtered_dat$value)
  
  # A numeric value of 2 is equivalent to mean_2SD
  filtered_dat <- outlier_remove(
    dat = sample_dat,
    project = "s1",
    x = "value",
    dat.remove = 2
  )
  
  expect_equal(nrow(filtered_dat), 9)
  expect_false(100 %in% filtered_dat$value)
  
  # does nothing when dat.remove is 'none'
  unfiltered_dat <- outlier_remove(
    dat = sample_dat,
    project = "s1",
    x = "value",
    dat.remove = 'none'
  )
  
  expect_equal(nrow(unfiltered_dat), 10)
  expect_true(100 %in% unfiltered_dat$value)
})

# Test outlier_remove handles errors --------------------------------------------------------------
test_that("outlier_remove handles input validation correctly", {
  # Expect an error when the specified column 'x' is not numeric
  expect_error(
    outlier_remove(
      dat = "s1MainDataTable",
      project = "s1",
      x = "GEARCODE"
    ),
    "Data is not numeric."
  )
})

