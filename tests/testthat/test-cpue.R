# -------------------------------------------------------------------------------------------------
# File: test-cpue.R
# Purpose: To perform unit tests on the cpue() function.
# Description: 
#   This script tests the that `cpue()` function returns catch per unit effort (CPUE) or revenue 
#   per unit effort variable to the primary dataset.
#   
# Scenarios tested:
#   - CPUE is calculated
#   - RPUE is calculated
#   - RPUE is calculated, xWeight = NULL
#   - Error handling for missing arguments
#
# Notes:
#   - The FishSET::log_call() function is mocked to prevent errors from database connection
#     attempts during isolated testing.
# -------------------------------------------------------------------------------------------------

dummy_data <- tibble(
  ID = 1:5,
  CATCH_LB = c(100, 200, 300, 0, 500),
  TIME_HRS = c(2, 4, 6, 0, 10),
  PRICE_USD = c(5, 5.5, 6, 7, 5),
  REVENUE_USD = c(500, 1100, 1800, 0, 2500)
)

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

project = "test"

# CPUE is calculated ------------------------------------------------------------------------------
test_that("CPUE is calculated correctly (Catch/Time)", {
  
  result <- suppressWarnings(cpue(
    dat = dummy_data,
    project = "test",
    xWeight = "CATCH_LB",
    xTime = "TIME_HRS",
    name = "my_cpue"
  ))
  
  # Expected calculation: CATCH_LB / TIME_HRS
  expected_cpue <- c(50, 50, 50, NaN, 50)
  
  expect_true("my_cpue" %in% names(result))
  expect_equal(result$my_cpue, expected_cpue)
  
})

# RPUE is calculated -----------------------------------------------------------------------------
test_that("RPUE is calculated correctly (Catch * Price / Time)", {
  result <- suppressWarnings(cpue(
    dat = dummy_data,
    project = "test",
    xWeight = "CATCH_LB",
    xTime = "TIME_HRS",
    price = "PRICE_USD",
    name = "my_rpue"
  ))
  
  # Expected calculation: (CATCH_LB * PRICE_USD) / TIME_HRS
  expected_rpue <- c(250, 275, 300, NaN, 250)
  
  expect_true("my_rpue" %in% names(result))
  expect_equal(result$my_rpue, expected_rpue)
})

# RPUE is calculated, xWeight = NULL --------------------------------------------------------------
test_that("RPUE is calculated correctly (Revenue / Time, xWeight = NULL)", {
  result <- suppressWarnings(cpue(
    dat = dummy_data,
    project = "test",
    xWeight = NULL,
    xTime = "TIME_HRS",
    price = "REVENUE_USD", # Using revenue column as 'price'
    name = "my_rpue_direct"
  ))
  
  # Expected calculation: REVENUE_USD / TIME_HRS
  expected_rpue <- c(250, 275, 300, NaN, 250)
  
  expect_true("my_rpue_direct" %in% names(result))
  expect_equal(result$my_rpue_direct, expected_rpue)
})

# Error handling for missing arguments ------------------------------------------------------------
test_that("Error handling for missing arguments works", {
  # Both xWeight and price are missing/NULL
  expect_error(
    cpue(dat = dummy_data,
         project = "test", xWeight = NULL, xTime = "TIME_HRS", price = NULL),
    "'xWeight' and/or 'price' argument must be provided.",
    fixed = TRUE
  )
})



