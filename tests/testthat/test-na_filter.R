# -------------------------------------------------------------------------------------------------
# File: test-na_filter.R
# Purpose: Unit tests for removing/replacing NA values
# Description:
#   This script tests the that `na_filter()` function returns a list of two objects: data frame 
#   containing updated data with NAs removed/replaced; any messages for the user. The user can 
#   completely remove the NAs values which will removed the entire row in the data frame. They can
#   also replace the values with a specific numeric value or with the mean of the variable. 
#   
# Scenarios tested:
#   - No NAs when data is clean
#   - Identifies and lists columns with NAs
#   - Removes rows with NAs in a single specified column
#   - Removes rows based on NAs in multiple columns
#   - Replaces NAs with the column mean by default
#   - Replaces NAs with a specific numeric value
#   - Does not replace NAs in a non-numeric column
#   - Replace=TRUE takes precedence over remove=TRUE
#   - Warns when a specified column has no NAs
#   - Warns when a specified column contains all NAs
#
# Notes:
#   - Created sample data for simplicity 
#
# -------------------------------------------------------------------------------------------------

# Create a sample data frame for testing
test_df <- data.frame(
  id = 1:6,
  val1 = c(10, 20, NA, 40, 50, NaN),         # Numeric with NA/NaN. Mean is 30. Median is 30.
  val2 = c(100, NA, 300, 400, NaN, 600),     # Numeric with NA/NaN. Mean is 350.
  char_col = c("a", "b", "c", NA, "e", "f"), # Character with NA.
  no_na_col = c(1, 2, 3, 4, 5, 6),           # No NAs.
  all_na_col = rep(NA_real_, 6)              # All NAs.
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

# No NAs when data is clean -----------------------------------------------------------------------
test_that("it reports no NAs when data is clean", {
  with_mocked_bindings(
    {
      clean_df <- data.frame(a = 1:3, b = c("x", "y", "z"))
      result <- na_filter(clean_df, "test")
      mesg <- attr(result, "messages")
      expect_true(any(grepl("No NAs found.", attr(result, "messages"))))
    },
    msg_print = function(...) invisible(NULL)
  )
})

# Identifies and lists columns with NAs ------------------------------------------------------------
test_that("it identifies and lists columns with NAs", {
  with_mocked_bindings(
    { 
      result <- na_filter(test_df, "test", remove = FALSE, replace = FALSE)
      expected_msg <- "The following columns contain NAs: val1, val2, char_col, all_na_col"
      esg <- attr(result, "messages")
      # Check that the expected message is part of the output
      expect_true(any(grepl(expected_msg, esg)))
    },
    msg_print = function(...) invisible(NULL),
  )
})

# Removes rows with NAs in a single specified column ----------------------------------------------
test_that("it removes rows with NAs in a single specified column", {
  with_mocked_bindings(
    { 
      result <- na_filter(test_df, "test", x = "val1", remove = TRUE)
      # Rows 3 and 6 have NAs in `val1`. The remaining rows should be 1, 2, 4, 5.
      expect_equal(nrow(result), 4)
      expect_equal(result$id, c(1, 2, 4, 5))
      expect_true(any(grepl("All rows containing NAs have been removed", 
                            attr(result, "messages"))))
    },
    msg_print = function(...) invisible(NULL),
  )
  
})
# Removes rows based on NAs in multiple columns ---------------------------------------------------
test_that("it removes rows based on NAs in multiple columns", {
  with_mocked_bindings(
    { 
      # NAs are in rows 2, 3, 5, 6 for val1 OR val2.
      result <- na_filter(test_df, "test", x = c("val1", "val2"), remove = TRUE)
      expect_equal(nrow(result), 2)
      expect_equal(result$id, c(1, 4))
    },
    msg_print = function(...) invisible(NULL),
  )
  
})

# replaces NAs with the column mean by default ---------------------------------------------------
test_that("it replaces NAs with the column mean by default", {
  with_mocked_bindings(
    {
      result <- na_filter(test_df, "test", x = "val1", replace = TRUE)
      mean_val1 <- mean(test_df$val1, na.rm = TRUE) # Should be 30
      
      expect_equal(sum(is.na(result$val1)), 0)
      expect_equal(result$val1, c(10, 20, 30, 40, 50, 30))
      expect_true(any(grepl("replaced with 30", attr(result, "messages"))))
    },
    msg_print = function(...) invisible(NULL),
  )
  
})

# replaces NAs with a specific numeric value ------------------------------------------------------
test_that("it replaces NAs with a specific numeric value", {
  with_mocked_bindings(
    {
      result <- na_filter(test_df, "test", x = "val1", replace = TRUE, rep.value = 0)
      
      expect_equal(sum(is.na(result$val1)), 0)
      expect_equal(result$val1, c(10, 20, 0, 40, 50, 0))
      expect_true(any(grepl("replaced with 0", attr(result, "messages"))))
    },
    msg_print = function(...) invisible(NULL),
  )
  
})

# does not replace NAs in a non-numeric column ----------------------------------------------------
test_that("it does not replace NAs in a non-numeric column", {
  with_mocked_bindings(
    { 
      result <- na_filter(test_df, "test", x = "char_col", replace = TRUE)
      
      # Data should be unchanged for this column
      expect_equal(result$char_col, test_df$char_col)
      expect_true(any(grepl("Variable is not numeric. Function not applied.", 
                            attr(result, "messages"))))
    },
    msg_print = function(...) invisible(NULL),
  )
  
})

# replace=TRUE takes precedence over remove=TRUE --------------------------------------------------
test_that("replace=TRUE takes precedence over remove=TRUE", {
  with_mocked_bindings(
    {
      # When both are TRUE, replace should be performed.
      result <- na_filter(test_df, "test", x = "val1", replace = TRUE, remove = TRUE)
      
      # The result should be a replacement, not a removal.
      expect_equal(nrow(result), 6) # No rows removed
      expect_equal(sum(is.na(result$val1)), 0) # NAs replaced
    },
    msg_print = function(...) invisible(NULL),
  )
  
})

# warns when a specified column has no NAs --------------------------------------------------------
test_that("it warns when a specified column has no NAs", {
  with_mocked_bindings(
    {
      result <- na_filter(test_df, "test", x = "no_na_col", remove = TRUE)
      expect_true(any(grepl("no_na_col  do not contain NAs.", attr(result, "messages")[2])))
    },
    msg_print = function(...) invisible(NULL),
  )
  
})

