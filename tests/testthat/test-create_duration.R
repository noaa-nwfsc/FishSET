# -------------------------------------------------------------------------------------------------
# File: test-create_duration.R
# Purpose: Unit tests for transforming units of date variables
# Description:
#   This script tests the that `temporal_mod()` function creates a new temporal variable by
#    extracting temporal unit, such as year, month, or day from a date variable.
#   
# Scenarios tested:
#   - correct duration in minutes
#   - duration in hours and days
#   - duration in weeks
#   - custom column name
#   - handles NA values
#
# Notes:
#   - Uses dummy data frames
#   - Mocks the log_call() function to prevent side effects during testing.
# -------------------------------------------------------------------------------------------------

# Create a sample dataset for testing
test_data_df <- data.frame(
  ID = 1:4,
  TRIP_START_DATE = c(
    "2023-10-15 10:00:00",
    "2023-10-16 09:00:00",
    "2023-10-17 12:00:00",
    "2023-10-18 00:00:00"
  ),
  TRIP_END_DATE = c(
    "2023-10-15 11:30:00", # 1.5 hours
    "2023-10-18 09:00:00", # 2 days
    "2023-10-24 12:00:00", # 7 days
    "2023-10-25 03:00:00"  # 7 days, 3 hours
  )
)
# Convert to POSIXct to match expected real-world data types
test_data_df$TRIP_START_DATE <- ymd_hms(test_data_df$TRIP_START_DATE, tz = "UTC")
test_data_df$TRIP_END_DATE <- ymd_hms(test_data_df$TRIP_END_DATE, tz = "UTC")


## Create mock log_call() --------------------------------------------------------------------------
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

project = "test"

# correct duration in minutes ---------------------------------------------------------------------
test_that("create_duration calculates correct duration in minutes", {
  with_mocked_bindings(
    {
      result <- create_duration(
        dat = test_data_df,
        project = 'test',
        start = "TRIP_START_DATE",
        end = "TRIP_END_DATE",
        units = "minute",
        name = "TripDurationMinutes"
      ) },
    log_call = function(...) invisible(NULL)
  )
  
  expect_true("TripDurationMinutes" %in% colnames(result))
  expected_minutes <- c(90, 2880, 10080, 10260)
  expect_equal(result$TripDurationMinutes, expected_minutes, tolerance = 1e-6)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
})

# duration in hours and days ----------------------------------------------------------------------
test_that("create_duration calculates correct duration in hours and days", {
  # ACT 1: Hours
  with_mocked_bindings(
    {
      result_h <- create_duration(
        dat = test_data_df,
        project = "test",
        start = "TRIP_START_DATE",
        end = "TRIP_END_DATE",
        units = "hour",
        name = "TripDurationHours"
      )
      # ACT 2: Days
      result_d <- create_duration(
        dat = test_data_df,
        project = "test",
        start = "TRIP_START_DATE",
        end = "TRIP_END_DATE",
        units = "day",
        name = "TripDurationDays"
      ) },
    log_call = function(...) invisible(NULL)
  )
  
  expected_hours <- c(1.5, 48, 168, 171)
  expect_equal(result_h$TripDurationHours, expected_hours, tolerance = 1e-6)
  expect_true("TripDurationHours" %in% colnames(result_h))
  
  expected_days <- c(1.5 / 24, 2, 7, 7.125)
  expect_equal(result_d$TripDurationDays, expected_days, tolerance = 1e-6)
  expect_true("TripDurationDays" %in% colnames(result_d))
})

# duration in weeks ------------------------------------------------------------------------------
test_that("create_duration calculates correct duration in weeks", {
  # ACT
  with_mocked_bindings(
    {
      result <- create_duration(
        dat = test_data_df,
        project = "test",
        start = "TRIP_START_DATE",
        end = "TRIP_END_DATE",
        units = "week",
        name = "TripDurationWeeks"
      ) },
    log_call = function(...) invisible(NULL)
  )
  
  expected_weeks <- c((1.5/24)/7, 2/7, 1, 7.125/7)
  expect_equal(result$TripDurationWeeks, expected_weeks, tolerance = 1e-6)
  expect_true("TripDurationWeeks" %in% colnames(result))
})

# custom column name ------------------------------------------------------------------------------
test_that("create_duration handles a custom column name", {
  # ACT
  custom_name <- "Custom_Duration_Col"
  with_mocked_bindings(
    { 
      result <- create_duration(
        dat = test_data_df,
        project = "test",
        start = "TRIP_START_DATE",
        end = "TRIP_END_DATE",
        units = "hour",
        name = custom_name
      )},
    log_call = function(...) invisible(NULL)
  )
  
  expect_true(custom_name %in% colnames(result))
  # Ensure it didn't use the default "create_duration"
  expect_false("create_duration" %in% colnames(result))
})


# handles NA values-------------------------------------------------------------------------------
test_that("create_duration handles NA values (e.g., if date is missing)", {
  # Create data with an NA end date
  na_data_df <- test_data_df
  na_data_df[1, "TRIP_END_DATE"] <- NA
  assign("na_data_df", na_data_df, envir = .GlobalEnv)
  
  # ACT
  with_mocked_bindings(
    { 
      result <- create_duration(
        dat = na_data_df,
        project = "test",
        start = "TRIP_START_DATE",
        end = "TRIP_END_DATE",
        units = "minute",
        name = "DurationWithNA"
      )},
    log_call = function(...) invisible(NULL)
  )
  
  # ASSERT
  expect_true(is.na(result$DurationWithNA[1]))
  # The other values should still be correctly calculated
  expected_minutes <- c(NA, 2880, 10080, 10260)
  expect_equal(result$DurationWithNA, expected_minutes, tolerance = 1e-6)
  
  # Clean up
  rm("na_data_df", envir = .GlobalEnv)
})
