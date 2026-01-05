# -------------------------------------------------------------------------------------------------
# File: test-group_perc.R
# Purpose: Unit tests for the `group_perc()` function 
# Description:
#   This script tests the that `group_perc()` function creates a within-group percentage variable 
#   using a primary group variable. A total value variable is also created and can be saved as well.
#   
# Scenarios tested:
#   - Calculate percentage with one group 
#   - Calculate percentage with multiple group
#   - Calculate percentage with group = NULL
#   - Test if total column is properly removed 
#   - Name parameter correctly names new column 
#   - Handles NAs correctly with a group 
#   - Handles NAs correctly with group = NULL
#   - Handles zero-sum groups
#   
# Notes:
#   - Created sample data for simplicity 
#   - The script uses mock functions (`mock_log_call`, `mock_data_pull`, `mock_parse_data_name`)
#     to isolate the test environment and avoid external dependencies like database connections.
# -------------------------------------------------------------------------------------------------

# Create sample data for testing-------------------------------------------------------------------
data_simple <- tibble::tibble(
  region = c("North", "North", "South", "South", "East"),
  sector = c("A", "B", "A", "B", "A"),
  catch = c(10, 20, 30, 40, 50)
)
# Expected totals for data_simple:
# By region: North=30, South=70, East=50
# Grand total: 150

data_na <- tibble::tibble(
  region = c("North", "North", "South", "West"),
  catch = c(10, 20, 30, NA)
)

# Expected totals for data_na (na.rm = FALSE by default in sum):
# By region: North=30, South=30, West=NA
# Grand total: NA

data_zero <- tibble::tibble(
  region = c("A", "A", "B", "B"),
  catch = c(10, -10, 0, 0)
)

# Expected totals for data_zero:
# By region: A=0, B=0

# Define the mock functions ----------------------------------------------------------------------
mock_log_call <- function(...) invisible(NULL)
mock_data_pull <- function(dat, project) list(dataset = dat)
mock_parse_data_name <- function(dat, type, project) dat

# Save the original functions from the package namespace
original_log_call <- get("log_call", envir = as.environment("package:FishSET"))
original_data_pull <- get("data_pull", envir = as.environment("package:FishSET"))
original_parse_data_name <- get("parse_data_name", envir = as.environment("package:FishSET"))

# Schedule the restoration of all original functions.
on.exit({
  assignInNamespace("log_call", original_log_call, ns = "FishSET")
  assignInNamespace("data_pull", original_data_pull, ns = "FishSET")
  assignInNamespace("parse_data_name", original_parse_data_name, ns = "FishSET")
})

# Overwrite the real functions with our mocks
assignInNamespace("log_call", mock_log_call, ns = "FishSET")
assignInNamespace("data_pull", mock_data_pull, ns = "FishSET")
assignInNamespace("parse_data_name", mock_parse_data_name, ns = "FishSET")

# Calculate percentage with one group -------------------------------------------------------------
test_that("calculates percentage correctly with one group", {
  
  result <- group_perc(data_simple, 
                       "proj",
                       group = "region",
                       value = "catch",
                       include_total_col= TRUE)
  
  expected_perc <- c(
    (10 / 30) * 100,  # North
    (20 / 30) * 100,  # North
    (30 / 70) * 100,  # South
    (40 / 70) * 100,  # South
    (50 / 50) * 100   # East
  )
  expected_total <- c(30, 30, 70, 70, 50)
  
  expect_equal(result$group_perc, expected_perc)
  expect_equal(result$total_value, expected_total)
})

# Calculate percentage with multiple group --------------------------------------------------------
test_that("calculates percentage correctly with multiple groups", {
  # Each row is its own group, so all percentages should be 100
  result <- group_perc(data_simple, "proj", group = c("region", "sector"),
                       value = "catch",
                       include_total_col= TRUE)
  
  expect_equal(result$group_perc, rep(100, 5))
  expect_equal(result$total_value, c(10, 20, 30, 40, 50))
})

# Calculate percentage with group = NULL ----------------------------------------------------------
test_that("calculates percentage correctly with group = NULL", {
  # Calculates percentage of the grand total (150)
  result <- group_perc(data_simple, "proj", group = NULL, value = "catch",
                       include_total_col= TRUE)
  
  expected_perc <- c(
    (10 / 150) * 100,
    (20 / 150) * 100,
    (30 / 150) * 100,
    (40 / 150) * 100,
    (50 / 150) * 100
  )
  expect_equal(result$group_perc, expected_perc)
  expect_equal(result$total_value, rep(150, 5))
})

# Test if total column is properly removed --------------------------------------------------------
test_that("include_total_col = FALSE removes the total column", {
  # Test with a group
  res_group <- group_perc(data_simple, "proj", group = "region", value = "catch",
                       include_total_col= FALSE)
  expect_false("total_value" %in% names(res_group))
  expect_true("group_perc" %in% names(res_group))
  
  # Test with group = NULL
  res_null <- group_perc(data_simple, "proj", group = NULL, value = "catch",
                       include_total_col= FALSE)
  expect_false("total_value" %in% names(res_null))
  expect_true("group_perc" %in% names(res_null))
})

# Name parameter correctly names new column -------------------------------------------------------
test_that("name parameter correctly names the new column", {
  result <- group_perc(data_simple, "proj", group = "region", value = "catch", 
                       name = "my_custom_perc")
  
  expect_true("my_custom_perc" %in% names(result))
  expect_false("group_perc" %in% names(result))
})

# Handles NAs correctly with a group -------------------------------------------------------------
test_that("handles NAs correctly (group = region)", {
  # Because sum() defaults to na.rm = FALSE, the 'West' group total is NA
  # This results in NaN (NA / NA) for the percentage.
  
  # Mock data_pull to return the NA data
  result <- group_perc(data_na, "proj", group = "region", value = "catch",
                       include_total_col= TRUE)
  
  expected_perc <- c(
    (10 / 30) * 100,  # North
    (20 / 30) * 100,  # North
    (30 / 30) * 100,  # South
    NaN               # West (NA / NA)
  )
  expected_total <- c(30, 30, 30, NA) # Note: dplyr 1.1.0+ propagates the group value
  
  expect_equal(result$group_perc, expected_perc)
  expect_equal(result$total_value, expected_total)
})

# Handles NAs correctly with group = NULL ---------------------------------------------------------
test_that("handles NAs correctly (group = NULL)", {
  # Grand total is NA, so all percentages are NaN (x / NA)
  
  result <- group_perc(data_na, "proj", group = NULL, value = "catch",
                       include_total_col= TRUE)
  
  expect_equal(result$group_perc, rep(NaN, 4))
  expect_equal(result$total_value, rep(NA_real_, 4))
})

# Handles zero-sum groups -------------------------------------------------------------------------
test_that("handles zero-sum groups correctly", {
  # Group A total = 0. Group B total = 0.
  # 10 / 0 = Inf
  # -10 / 0 = -Inf
  # 0 / 0 = NaN
  
  result <- group_perc(data_zero, "proj", group = "region", value = "catch",
                       include_total_col= TRUE)
  
  expected_perc <- c(Inf, -Inf, NaN, NaN)
  
  expect_equal(result$group_perc, expected_perc)
  expect_equal(result$total_value, rep(0, 4))
})

