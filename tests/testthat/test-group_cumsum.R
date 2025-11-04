# -------------------------------------------------------------------------------------------------
# File: test-group_cumsum.R
# Purpose: Unit tests for the `group_cumsum()` function 
# Description:
#   This script tests the that `group_cumsum()` function creates a within-group running sum variable
#   using a primary group variable and a date sorting variable. A total value variable is also 
#   created and can be saved as well.
#   
# Scenarios tested:
#   - Correctly calculates cumsum
#   - Drops the group_total column
#   - Uses the custom name provided 
#   - Multiple grouping columns
#   - Multiple sorting date columns
#   
# Notes:
#   - Created sample data for simplicity 
#   - The script uses mock functions (`mock_log_call`, `mock_data_pull`, `mock_parse_data_name`)
#     to isolate the test environment and avoid external dependencies like database connections.
# ------------------------------------------------------------------------------------------------

# Sample data
test_data <- dplyr::tribble(
  ~grp, ~date_col,         ~val,
  "A",  as.Date("2023-01-02"), 10,
  "B",  as.Date("2023-01-01"), 100,
  "A",  as.Date("2023-01-01"), 5,    # Out of order
  "B",  as.Date("2023-01-03"), 300,
  "A",  as.Date("2023-01-03"), NA,   # NA value
  "B",  as.Date("2023-01-02"), 200,
  "A",  as.Date("2023-01-04"), 20
)

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

# Correctly calculates cumsum ---------------------------------------------------------------------
test_that("it correctly calculates cumsum, group_total, and handles NAs", {
  
  # Define the expected output after sorting, grouping, and mutating
  expected_result <- dplyr::tribble(
    # grp, date_col, val, group_total (sum(5,10,0,20)=35 / sum(100,200,300)=600), group_cumsum
    ~grp, ~date_col,         ~val, ~group_total, ~group_cumsum,
    "A",  as.Date("2023-01-01"), 5,    35,           5,
    "A",  as.Date("2023-01-02"), 10,   35,           15,   # 5 + 10
    "A",  as.Date("2023-01-03"), NA,   35,           15,   # 15 + replace_na(NA, 0)
    "A",  as.Date("2023-01-04"), 20,   35,           35,   # 15 + 20
    "B",  as.Date("2023-01-01"), 100,  600,          100,
    "B",  as.Date("2023-01-02"), 200,  600,          300,  # 100 + 200
    "B",  as.Date("2023-01-03"), 300,  600,          600   # 300 + 300
  )
  
  result <- group_cumsum(
    dat = test_data,
    project = "test_proj",
    group = "grp",
    sort_by = "date_col", 
    value = "val",
    name = "group_cumsum",
    drop_total_col = FALSE
  )
  
  expect_equal(result, expected_result)
  
})
# Drops the group_total column --------------------------------------------------------------------
test_that("it drops the group_total column when drop_total_col = TRUE", {
  
  result <- group_cumsum(
    dat = test_data,
    project = "test_proj",
    group = "grp",
    sort_by = "date_col", 
    value = "val",
    drop_total_col = TRUE # Key argument being tested
  )
  
  expect_false("group_total" %in% names(result))
  expect_true("group_cumsum" %in% names(result))
  
})

# Uses the custom name provided ------------------------------------------------------------------
test_that("it uses the custom name provided in the 'name' argument", {
  
  custom_name <- "my_running_total"
  
  result <- group_cumsum(
    dat = test_data,
    project = "test_proj",
    group = "grp",
    sort_by = "date_col", 
    value = "val",
    name = custom_name # Key argument being tested
  )
  
  expect_true(custom_name %in% names(result))
  expect_false("group_cumsum" %in% names(result))
  
})

# Multiple grouping columns ----------------------------------------------------------------------
test_that("it works correctly with multiple grouping columns", {
  
  # sample data set 1
  data_multi_group <- dplyr::tribble(
    ~grp1, ~grp2, ~date_col,         ~val,
    "A",   "X",   as.Date("2023-01-02"), 10,   # Group A-X
    "A",   "Y",   as.Date("2023-01-01"), 5,    # Group A-Y
    "A",   "X",   as.Date("2023-01-01"), 1,    # Group A-X
    "B",   "X",   as.Date("2023-01-01"), 100,  # Group B-X
    "A",   "Y",   as.Date("2023-01-02"), 50    # Group A-Y
  )
  
  # sample data set 2
  expected_result <- dplyr::tribble(
    ~grp1, ~grp2, ~date_col,         ~val, ~group_total, ~group_cumsum,
    "A",   "X",   as.Date("2023-01-01"), 1,    11,           1,
    "A",   "X",   as.Date("2023-01-02"), 10,   11,           11,
    "A",   "Y",   as.Date("2023-01-01"), 5,    55,           5,
    "A",   "Y",   as.Date("2023-01-02"), 50,   55,           55,
    "B",   "X",   as.Date("2023-01-01"), 100,  100,          100
  )
  
  result <- group_cumsum(
    dat = data_multi_group,
    project = "test_proj",
    group = c("grp1", "grp2"), # Key argument being tested
    sort_by = "date_col",
    value = "val"
  )
  
  expect_equal(result, expected_result)
  
})

# Multiple sorting date columns -------------------------------------------------------------------
test_that("it works correctly with multiple sorting date columns", {
  
  # sample data with 2 date columns
  data_multi_sort <- dplyr::tribble(
    ~grp, ~date1,                ~date2,                ~val,
    "A",  as.Date("2023-01-02"), as.Date("2023-10-01"), 10,
    "A",  as.Date("2023-01-01"), as.Date("2023-10-02"), 5,
    "A",  as.Date("2023-01-01"), as.Date("2023-10-01"), 1,   # Should be first
    "A",  as.Date("2023-01-02"), as.Date("2023-10-02"), 20   # Should be last
  )
  
  expected_result <- dplyr::tribble(
    ~grp, ~date1,                ~date2,                ~val, ~group_total, ~group_cumsum,
    "A",  as.Date("2023-01-01"), as.Date("2023-10-01"), 1,    36,           1,
    "A",  as.Date("2023-01-01"), as.Date("2023-10-02"), 5,    36,           6,   # 1 + 5
    "A",  as.Date("2023-01-02"), as.Date("2023-10-01"), 10,   36,           16,  # 6 + 10
    "A",  as.Date("2023-01-02"), as.Date("2023-10-02"), 20,   36,           36   # 16 + 20
  )
  
  result <- group_cumsum(
    dat = data_multi_sort,
    project = "test_proj",
    group = "grp",
    sort_by = c("date1", "date2"), # Key argument being tested
    value = "val"
  )
  
  expect_equal(result, expected_result)
  
})

