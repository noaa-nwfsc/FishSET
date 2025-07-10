# -------------------------------------------------------------------------------------------------
# File: test-shift_sort_xcpp.R
# Purpose: Unit tests for the `shift_sort_xcpp()` function in the FishSET package
# Description:
#   This test checks the functionality of the shift_sort_xcpp() function, which is used to rearrange
#   and encode design matrices used in logistic regression based on shifting and sorting logic
#   involving alternative choices and distances.
#
# Scenarios tested:
#   - Correct reshaping and encoding of binary design matrix
#   - Proper alignment and inclusion of distance matrix
#   - Correct soring order based on specified alternatives and shift params
#   - Integrity of numeric outputs
#
# Notes:
#   This test provides fixed inputs and checks exact equality against known output.
# -------------------------------------------------------------------------------------------------

test_that("Test shift_sort_xcpp() function", {
  # Generate 10x16 flattened identity matrix
  dataCompile <- matrix(0, nrow = 10, ncol = 16)
  dataCompile[, c(1, 6, 11, 16)] <- 1
  
  # Define choice matrix
  choice <- as.matrix(c(1, 3, 1, 1, 2, 1, 4, 2, 3, 2))
  
  # Catch amounts corresponding to each observation
  catch <- c(0.68, 18.99, 6.84, 17.83, 17.29, 6.27, 11.42, 17.16, 17.35, 16.23)
  
  # Create distance matrix: 10x4, each row contains distances for the 4 alternatives
  distance <- matrix(c(
    234.06, 243.47, 123.82,  99.76,
    234.06, 243.47, 123.82,  99.76,
    71.30,  72.50, 286.66, 347.34, 
    199.17, 192.00, 414.52, 486.49,
    71.30,  72.50, 286.66, 347.34, 
    234.06, 243.47, 123.82,  99.76,
    234.06, 243.47, 123.82,  99.76,
    234.06, 243.47, 123.82,  99.76,
    234.06, 243.47, 123.82,  99.76,
    234.06, 243.47, 123.82,  99.76 
  ), nrow = 10, ncol = 4, byrow = TRUE)
  
  # Set shift param
  ab <- 5
  
  # Run the function
  d <- shift_sort_xcpp(x = dataCompile, ch = choice, y = catch,
                       distance = distance, alts = max(choice), ab = ab)
  
  # Manually constructed expected result matrix (10x22) based on known correct output
  expected <- matrix(c(
    # Row 1
    0.68, 1, 
    1, 0, 0, 0, 
    0, 1, 0, 0, 
    0, 0, 1, 0, 
    0, 0, 0, 1, 
    234.06, 243.47, 123.82, 99.76,
    # Row 2
    18.99, 3, 
    0, 0, 1, 0, 
    0, 0, 0, 1, 
    1, 0, 0, 0, 
    0, 1, 0, 0, 
    123.82, 99.76, 234.06, 243.47,
    # Row 3
    6.84, 1, 
    1, 0, 0, 0, 
    0, 1, 0, 0, 
    0, 0, 1, 0, 
    0, 0, 0, 1, 
    71.30, 72.50, 286.66, 347.34,
    # Row 4
    17.83, 1,
    1, 0, 0, 0, 
    0, 1, 0, 0, 
    0, 0, 1, 0, 
    0, 0, 0, 1, 
    199.17, 192.00, 414.52, 486.49,
    # Row 5
    17.29, 2, 
    0, 0, 0, 1, 
    1, 0, 0, 0, 
    0, 1, 0, 0, 
    0, 0, 1, 0, 
    72.50, 286.66, 347.34, 71.30,
    # Row 6
    6.27, 1, 
    1, 0, 0, 0, 
    0, 1, 0, 0, 
    0, 0, 1, 0, 
    0, 0, 0, 1, 
    234.06, 243.47, 123.82, 99.76,
    # Row 7
    11.42, 4, 
    0, 1, 0, 0, 
    0, 0, 1, 0, 
    0, 0, 0, 1, 
    1, 0, 0, 0, 
    99.76, 234.06, 243.47, 123.82,
    # Row 8
    17.16, 2, 
    0, 0, 0, 1, 
    1, 0, 0, 0, 
    0, 1, 0, 0, 
    0, 0, 1, 0, 
    243.47, 123.82, 99.76, 234.06,
    # Row 9
    17.35, 3, 
    0, 0, 1, 0, 
    0, 0, 0, 1, 
    1, 0, 0, 0, 
    0, 1, 0, 0, 
    123.82, 99.76, 234.06, 243.47,
    # Row 10
    16.23, 2, 
    0, 0, 0, 1, 
    1, 0, 0, 0, 
    0, 1, 0, 0, 
    0, 0, 1, 0, 
    243.47, 123.82, 99.76, 234.06
  ), nrow = 10, ncol = 22, byrow = TRUE)
  
  # Check dimensions
  expect_equal(dim(d), c(10, 22))
  expect_equal(dim(d), dim(expected))
  
  # Check column values
  d_unnamed <- unname(d)
  expected_unnamed <- unname(expected)
  expect_equal(d_unnamed, expected_unnamed, tolerance = 1e-2)
})