context("QAQC helper")

test_that("Logical ouput returns one value per column", {
  
  out <- qaqc_helper(PollockData, is.numeric, output = "logical")
  expect_vector(out)
  expect_equal(length(out), ncol(PollockData))
  
})

test_that("Correct columns are identified for NAs, NaNs, and Infs", {
          
  expect_equal(qaqc_helper(PollockData, "NA", "names"),
               names(PollockData)[vapply(PollockData, anyNA, logical(1))])
  expect_equal(qaqc_helper(PollockData, "NaN", "names"),
               names(PollockData)[vapply(PollockData, function(x) any(is.nan(x)), logical(1))])
  expect_equal(qaqc_helper(PollockData, "Inf", "names"),
               names(PollockData)[vapply(PollockData, function(x) any(is.infinite(x)), logical(1))])
})

test_that("Correctly detects NAs, NaNs, and Infs", {
  temp_dat <- data.frame(A = c(1:9, NA), B = c(runif(9), NaN), C = c(letters[1:9], Inf))
  expect_equal(qaqc_helper(temp_dat, "NA", "names"), 
               names(temp_dat)[vapply(temp_dat, anyNA, logical(1))])
  expect_equal(qaqc_helper(temp_dat, "NaN", "names"),
               names(temp_dat)[vapply(temp_dat, function(x) any(is.nan(x)), logical(1))])
  expect_equal(qaqc_helper(temp_dat, "Inf", "names"),
               names(temp_dat)[vapply(temp_dat, function(x) any(is.infinite(x)), logical(1))])
  
})


test_that("Expect error if input function returns multiple values", {
  
  expect_error(qaqc_helper(PollockData, range, output = "value"))
})


