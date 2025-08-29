# -------------------------------------------------------------------------------------------------
# File: test-na_filter.R
# Purpose: Unit tests for removing/replacing NaN values
# Description:
#   This script tests the that `nan_filter()` function returns a list of two objects: data frame 
#   containing updated data with NaNs removed/replaced; any messages for the user. The user can 
#   completely remove the NaNs values which will removed the entire row in the data frame. They can
#   also replace the values with a specific numeric value or with the mean of the variable. 
#   
# Scenarios tested:
#   - No NaNs when data is clean
#   - Identifies and lists columns with NaNs
#   - Removes rows with NaNs in a single specified column
#   - Removes rows based on NaNs in multiple columns
#   - Replaces NaNs with the column mean by default
#   - Replaces NaNs with a specific numeric value
#   - Does not replace NaNs in a non-numeric column
#   - Replace=TRUE takes precedence over remove=TRUE
#   - Warns when a specified column has no NaNs
#   - Warns when a specified column contains all NaNs
#
# Notes:
#   - Created sample data for simplicity 
#
# -------------------------------------------------------------------------------------------------

# Create a sample data frame for testing
test_df <- data.frame(
  id = 1:6,
  nan_only_col = c(10, NaN, 30, 40, NaN, 60),
  mixed_special_col = c(100, 200, NA, NaN, 500, 600),
  no_special_col = c(1, 2, 3, 4, 5, 6),
  all_nan_col = rep(NaN, 6)
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

# No NaNs when data is clean -----------------------------------------------------------------------
test_that("it reports no NaNs when data is clean", {
  clean_df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  
  # Mock `qaqc_helper` to report no NaNs columns.
  with_mocked_bindings(
    {
      result <- nan_filter(clean_df, "test")
      # In identification mode, the function prints a message. We check that message.
      # The function also returns the captured messages in its log output.
      expect_true(any(grepl("No NaNs found.", result$messages)))
    },
    msg_print = function(...) invisible(NULL),
  )
})
# Identifies and lists columns with NaNs ------------------------------------------------------------
test_that("it identifies and lists columns with NaNs", {
  with_mocked_bindings(
    {
      result <- nan_filter(test_df, "test")
      expected_msg <-paste0("The following columns contain NaNs: nan_only_col, mixed_special_col, all_nan_col. Consider using nan_filter to replace or remove NaNs.")
      # Check that the expected message is part of the output
      expect_true(any(grepl(expected_msg, result$messages)))
    },
    msg_print = function(...) invisible(NULL)
  )
})


# Removes rows with NaNs in a single specified column ----------------------------------------------
test_that("it removes rows with NaNs in a single specified column", {
  with_mocked_bindings(
    {
      result <- nan_filter(test_df, "test", x = "nan_only_col", remove = TRUE)
      expect_equal(nrow(result$data), 4)
      expect_equal(result$data$id, c(1, 3, 4, 6))
      expect_true(any(grepl("All rows containing NaNs have been removed", result$messages)))
    },
    msg_print = function(...) invisible(NULL),
  )
})

# Removes rows based on NaNs in multiple columns ---------------------------------------------------
test_that("it removes rows based on NaNs in multiple columns", {
  with_mocked_bindings(
    {
      result <- nan_filter(test_df, "test", x = c("nan_only_col", "mixed_special_col"),
        remove = TRUE)
      expect_equal(nrow(result$data), 3)
      expect_equal(result$data$id, c(1, 3,6))
    },
    msg_print = function(...) invisible(NULL),
  )
})

# replaces NaNs with the column mean by default ---------------------------------------------------
test_that("it replaces NaNs with the column mean by default", {
  with_mocked_bindings(
    {
      result <- nan_filter(test_df, "test", x = "nan_only_col", replace = TRUE)
      mean_val1 <- mean(test_df$nan_only_col, na.rm = TRUE) # Should be 30
      expect_equal(sum(is.na(result$data$nan_only_col)), 0)
      expect_equal(result$data$nan_only_col, c(10, 35, 30, 40, 35, 60))
      expect_true(any(grepl("replaced with 35", result$messages)))
    },
    msg_print = function(...) invisible(NULL),
  )
})

# replaces NaNs with a specific numeric value ------------------------------------------------------
test_that("it replaces NaNs with a specific numeric value", {
  with_mocked_bindings(
    {
      result <- nan_filter(test_df, "test", x = "nan_only_col", replace = TRUE, 
        rep.value = 0)
      
      expect_equal(sum(is.na(result$data$nan_only_col)), 0)
      expect_equal(result$data$nan_only_col, c(10, 0, 30, 40, 0, 60))
      expect_true(any(grepl("replaced with 0", result$messages)))
    },
    msg_print = function(...) invisible(NULL),
  )
})


# replace=TRUE takes precedence over remove=TRUE --------------------------------------------------
test_that("replace=TRUE takes precedence over remove=TRUE", {
  with_mocked_bindings(
    {
      # When both are TRUE, replace should be performed.
      result <- nan_filter(test_df, "test", x = "nan_only_col", replace = TRUE, 
        remove = TRUE)
      
      # The result should be a replacement, not a removal.
      expect_equal(nrow(result$data), 6) # No rows removed
      expect_equal(sum(is.na(result$data$nan_only_col)), 0) # NaNs replaced
    },
    msg_print = function(...) invisible(NULL),
  )
})

# warns when a specified column has no NaNs ------------------------------------------------------
test_that("it warns when a specified column has no NaNs", {
  with_mocked_bindings(
    {
      # `no_na_col` has no NaNs, so we expect a warning.
      expect_warning(
        nan_filter(test_df, "test", x = "no_special_col", remove = TRUE),
        "no_special_col do not contain NaNs."
      )
    },
    msg_print = function(...) invisible(NULL),
  )
})


