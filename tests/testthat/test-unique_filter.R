# -------------------------------------------------------------------------------------------------
# File: test-unique_filter.R
# Purpose: Unit tests for unique row observations in the FishSET package
# Description: 
#   This script tests the `unique_filter()` function so that it returns both a dataset and messages
#   notifying the users if duplicate observations exist. The user can remove those observations 
#   using this function as well. 
#   
# Scenarios tested:
#   - All rows are unique, remove = FALSE (default)
#   - Duplicate rows exist, remove = FALSE (default)
#   - Duplicate rows exist, remove = TRUE 
#   - Edge case with an empty data frame
#
# Notes:
#   - Uses dummy data frames
#   - Mocks the log_call() function to prevent side effects during testing.
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


# Define Test Data --------------------------------------------------------------------------------
unique_df <- data.frame(id = 1:4, val = c("a", "b", "c", "d"))
duplicate_df <- data.frame(id = c(1, 2, 2, 3), val = c("a", "b", "b", "c"))
empty_df <- data.frame(id = integer(), val = character())

# All rows are unique, remove = FALSE (default) ---------------------------------------------------
test_that("it handles unique data correctly with remove=FALSE", {
  
  with_mocked_bindings(
    {
      # Run the function
      result <- unique_filter(dat = unique_df, project = "test")
      
      # Assertions
      expect_equal(nrow(result), 4)
      expect_equal(result, unique_df, ignore_attr = TRUE)
      expect_true(any(grepl("Each row is a unique choice occurrence", 
        attr(result, "messages"))))
    },
    msg_print = function(...) invisible(NULL)
  )
})
  
  # Duplicate rows exist, remove = FALSE (default) -----------------------------------------------
  test_that("it identifies duplicates but does not remove them when remove=FALSE", {
     with_mocked_bindings(
    { 
    # Run the function
    result <- unique_filter(dat = duplicate_df, project = "test_proj", remove = FALSE)
    
    # Assertions
    expect_equal(nrow(result), 4) # Should still have 4 rows
    expect_equal(result, duplicate_df, ignore_attr = TRUE) # Data frame should be unchanged
    expect_true(any(grepl("Consider removing non-unique rows", attr(result, "messages"))))
    },
    msg_print = function(...) invisible(NULL)
  )
  })
  
  # Duplicate rows exist, remove = TRUE -----------------------------------------------------------
  test_that("it removes duplicates when remove=TRUE", {
    with_mocked_bindings(
    {
    # Run the function
    result <- unique_filter(dat = duplicate_df, project = "test_proj", remove = TRUE)
    
    expect_equal(nrow(result), 3) 
    # Should match the distinct version
    expect_equal(result, dplyr::distinct(duplicate_df), ignore_attr = TRUE) 
    expect_true(any(grepl("Non-unique rows removed", attr(result, "messages"))))
    },
    msg_print = function(...) invisible(NULL)
  )
  })
  
  # Edge case with an empty data frame -----------------------------------------------------------
  test_that("it handles an empty data frame without error", {
     with_mocked_bindings(
    { 
    # Run the function
    result <- unique_filter(dat = empty_df, project = "test_proj")
    
    # Assertions
    expect_equal(nrow(result), 0)
    expect_s3_class(result, "data.frame")
    expect_true(any(grepl("Each row is a unique choice occurrence", 
      attr(result, "messages"))))
    },
    msg_print = function(...) invisible(NULL)
  )
  })
  

