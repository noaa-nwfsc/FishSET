# -------------------------------------------------------------------------------------------------
# File: test-zone_summary.R
# Purpose: Unit tests for the `zone_summary()` function in the FishSET package
# Description:
# This script tests the `zone_summary()` function to 
# 
# Scenarios tested:
# - Tests table outputs and functions
# - Tests plots and grouped data 
#
# Notes:
# - The script uses mock functions (`mock_log_call`, `mock_save_plot`, `mock_save_table`)
#   to isolate the test environment and avoid external dependencies like database connections.
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


# Create mock save_plot() and save_table() --------------------------------------------------------
# Description: Override the FishSET functions for saving the output folder
# Get a reference to the original function from its namespace.
# We'll use this to restore it later.
original_save_plot <- get("save_plot", envir = as.environment("package:FishSET"))
original_save_table <- get("save_table", envir = as.environment("package:FishSET"))

# This is the fake function that will do nothing.
mock_save_plot <- function(...) {
  # This function does nothing and returns invisibly.
  invisible(NULL)
}

mock_save_table <- function(...) {
  # This function does nothing and returns invisibly.
  invisible(NULL)
}

# On exit, restore the original functions to the namespace. This is crucial
# for clean testing and prevents side effects.
on.exit({
  assignInNamespace("save_plot", original_save_plot, ns = "FishSET")
  assignInNamespace("save_table", original_save_table, ns = "FishSET")
})

# Now, replace the original log_call with our mock function.
assignInNamespace("save_plot", mock_save_plot, ns = "FishSET")
assignInNamespace("save_table", mock_save_table, ns = "FishSET")


# Tests table outputs and functions ---------------------------------------------------------------
test_that("Calculations are correct for various argument combinations", {
  
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Save a sample of the data
  test_df <- table_view("s1MainDataTable", "s1")
  spat_test_df <- table_view("s1spatSpatTable", "s1")
  
  # Simple count per zone
  res_count <- zone_summary(dat = test_df, spat = spat_test_df, project = "s1",
    zone.dat = "ZoneID", zone.spat = "TEN_ID",
    count = TRUE, output = "table")
  expect_equal(head(res_count$ZoneID, 3), c( 387331, 387332, 406926))
  expect_equal(head(res_count$n, 3), c(103, 81, 79))
  
  # Summarize a variable by zone (e.g., sum)
  res_sum <- zone_summary(dat = test_df, spat = spat_test_df, project = "s1",
    zone.dat = "ZoneID", zone.spat = "TEN_ID",
    count = FALSE, var = "landed_thousands", fun = "sum",
    output = "table")
  expected_sum <- test_df %>% group_by(ZoneID) %>% summarise(s = sum(landed_thousands))
  expect_equal(res_sum$landed_thousands, expected_sum$s)
   
  # Count by a grouping variable
  res_group_count <- zone_summary(dat = test_df, spat = spat_test_df, project = "s1",
    zone.dat = "ZoneID", zone.spat = "TEN_ID",
    count = TRUE, group = "GEARCODE", output = "table")
  expect_equal(nrow(res_group_count), 247) 
  
  # Percentage of total observations
  res_perc <- zone_summary(dat = test_df, spat = spat_test_df, project = "s1",
    zone.dat = "ZoneID", zone.spat = "TEN_ID",
    count = TRUE, fun = "percent", output = "table")
  expect_equal(head(res_perc$perc,3), c(5.1706827, 4.0662651, 3.9658635))
  expect_equal(sum(res_perc$perc),100)
})

# Tests plots and grouped data --------------------------------------------------------------------
test_that("Multi-plot logic for grouped data returns correct objects", {
  
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Save a sample of the data
  test_df <- table_view("s1MainDataTable", "s1")
  spat_test_df <- table_view("s1spatSpatTable", "s1")
  
  # Static multi-plot should return a list of ggplots
  res_static_multi <- zone_summary(dat = test_df, spat = spat_test_df, project = "s1",
    zone.dat = "ZoneID", zone.spat = "TEN_ID",
    count = TRUE, group = "GEARCODE",
    plot_type = "static", output = "plot")
  expect_type(res_static_multi, "list")
  expect_length(res_static_multi, 3) # One plot for each species
  expect_s3_class(res_static_multi[[1]], "ggplot")
  
  # Case 2: Dynamic multi-plot returns a single leaflet object with layers
  res_dynamic_multi <- zone_summary(dat = test_df, spat = spat_test_df, project = "s1",
    zone.dat = "ZoneID", zone.spat = "TEN_ID",
    count = TRUE, group = "GEARCODE",
    plot_type = "dynamic", output = "plot")
  
  expect_s3_class(res_dynamic_multi, "leaflet")
  expect_true("addLayersControl" %in% sapply(res_dynamic_multi$x$calls, `[[`, "method"))
})

