# -------------------------------------------------------------------------------------------------
# File: test-dummy_num.R
# Purpose: To perform unit tests on the dummy_num() function.
# Description: 
#    This script tests the that `dummy_num()` function create a binary vector from numeric, date,
#    and character or factor vectors.
#   
# Scenarios tested:
#   - Numeric 'more_less' works
#   - Numeric 'x_y' works 
#   - Date variables with 'more_less'
#   - Date 'x_y' works with multiple years
#   - Character 'x_y' works
#   - Character 'x_y' works with multiple values 
#   - Factor variables
#   - Stops if variable name does not exist
#   - Default name 'dummy_num'
#
# Notes:
#   - Uses test project: "s1" and its associated tables.
# -------------------------------------------------------------------------------------------------

# Create mock log_call() --------------------------------------------------------------------------
# Description: Override the FishSET::log_call() function to avoid errors with connecting when
#              running  unit tests
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


# Numeric 'more_less' works ----------------------------------------------------------------------
test_that("Numeric 'more_less' works with a range (using mean)", {
  
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- dummy_num(
    dat = result_main,
    project = 'test',
    var = 'TRIP_LENGTH',
    value = 10.416667,
    opts = 'more_less',
    name = 'trip_length_dum'
  )
  n <- result %>% group_by(trip_length_dum) %>% count()
  
  expect_equal(n$n, c(1481,511))
})

# Numeric 'x_y' works ----------------------------------------------------------------------------
test_that("Numeric 'x_y' works with a range", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- dummy_num(
    dat = result_main,
    project = 'test',
    var = 'TRIP_LENGTH',
    value = c( 10.416667, 12.229167),
    opts = 'x_y',
    name = 'trip_length_dum'
  )
  
  expect_equal(head(result$trip_length_dum,5), c(1, 1, 1, 0, 0))
})

# Date variables with 'more_less' -----------------------------------------------------------------
test_that("dummy_num handles date variables with 'more_less' (year)", {
  
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- dummy_num(
    dat = result_main,
    project = 'test',
    var = 'DATE_TRIP',
    value = 2008,
    opts = 'more_less',
    name = 'before_after_2008'
  )
  
  # Check column name
  expect_true('before_after_2008' %in% colnames(result))
  
  # Check dimensions
  expect_equal(nrow(result), 1992)
  expect_equal(ncol(result), 21) # 4 original + 1 new
  n <- result %>% group_by(before_after_2008) %>% count()
  expect_equal(n$n, c(768,1224))
})

# Date 'x_y' works with multiple years ------------------------------------------------------------
test_that("Date 'x_y' works with multiple years", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- dummy_num(
    dat = result_main,
    project = 'test',
    var = 'DATE_TRIP',
    value = c(2007, 2008),
    opts = 'x_y',
    name = 'is_07_or_08'
  )
  
  # Expected: 1 if year is 2020 or 2023
  expect_equal(head(result$is_07_or_08,5), c(1, 1, 1, 1, 1))
})


# Character 'x_y' works --------------------------------------------------------------------------
test_that("Character 'x_y' works with a single value", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- dummy_num(
    dat = result_main,
    project = 'test',
    var = "GEARCODE",
    value = 'TRAWL-BOTTOM',
    opts = 'x_y',
    name = 'is_bottom'
  )
  
  bottom <- result %>% filter(GEARCODE == 'TRAWL-BOTTOM')
  expect_equal(head(bottom$is_bottom,3), c(1, 1, 1))
})

# Character 'x_y' works with multiple values ------------------------------------------------------
test_that("Character 'x_y' works with multiple values", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- dummy_num(
    dat = result_main,
    project = 'test',
    var = "GEARCODE",
    value = c('TRAWL-BOTTOM',"DREDGE-CLAM"),
    opts = 'x_y',
    name = 'dum_var'
  )
  
  dum_df <- result_main %>% filter(GEARCODE %in% c('TRAWL-BOTTOM',"DREDGE-CLAM"))
  expect_equal(nrow(dum_df), nrow(result %>% filter(dum_var == 1)))
  
})

# Factor variables -------------------------------------------------------------------------------
test_that("Factor variables are handled like characters", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result_main_fct <- result_main %>% 
    mutate(`Program Code` = as.factor(`Program Code`))
  
  result <- dummy_num(
    dat = result_main_fct,
    project = 'test',
    var = "Program Code",
    value = "SCA",
    opts = 'x_y',
    name = 'dum_var'
  )
  dum_df <- result_main_fct %>% filter(`Program Code` == "SCA")
  
  expect_equal(nrow(dum_df), nrow(result %>% filter(dum_var == 1)))
})

# Stops if variable name does not exist -----------------------------------------------------------
test_that("Function stops if variable name does not exist", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  expect_error(
    dummy_num(result_main, project = "test", var = "non_existent_var", value = 100),
    "Variable 'non_existent_var' not found in the data frame."
  )
})

# Default name 'dummy_num'  -----------------------------------------------------------------------
test_that("Default name 'dummy_num' is used when `name` is not provided", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  # The improved function doesn't provide a default for 'name' anymore,
  # but we can test the function signature's default.
  result <- dummy_num(result_main, project = "test",var = 'TRIP_LENGTH',
                      value = 10.416667,
                      opts = 'more_less')
  expect_true("dummy_num" %in% names(result))
})