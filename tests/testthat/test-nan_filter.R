context("Detecting and filtering NAs and NaNs")

test_that("nan_identify correctly detects NAs", {
  
  temp_dat <- data.frame(A = c(1:9, NA), B = c(runif(10)), C = c(letters[1:10]))
  
  expect_message(nan_identify(temp_dat, "pollock"),  
                 "The following columns contain NAs: A.")
})

test_that("nan_identify correctly detects NaNs", {
  
  temp_dat <- data.frame(A = c(1:10), B = c(runif(9), NaN), C = c(letters[1:10]))
  
  expect_message(nan_identify(temp_dat, "pollock"), 
                 "The following columns contain NaNs: B")
  
})

test_that("nan_identify correctly detects -999", {
  
  temp_dat <- data.frame(A = c(1:10), B = c(runif(9), -999), C = c(letters[1:10]))
  
  expect_message(nan_identify(temp_dat, "pollock"), 
                 "The following columns contain the value -999: B.")
})

test_that("nan_filter correctly detects NaNs", {
  
  temp_dat <- data.frame(A = c(1:10), B = c(runif(9), NaN), C = c(letters[1:10]))
  
  expect_message(nan_filter(temp_dat, "pollock", x = "B"), 
                 "The following columns contain NaNs: B")
})

test_that("nan_filter replaces correct value", {
  
  temp_dat <- data.frame(A = c(1:9, NaN), B = c(runif(10)), C = c(letters[1:10]))
  
  expect_message(out <- nan_filter(temp_dat, "pollock", x = "A", replace = TRUE), 
                 "All NaNs in A have been replaced with 5")
  
  expect_equal(out$A[[10]], 5)
})

test_that("nan_filter removes correct row", {
  
  temp_dat <- data.frame(A = c(1:9, NaN), B = c(runif(10)), C = c(letters[1:10]))
  
  expect_message(out <- nan_filter(temp_dat, "pollock", x = "A", remove = TRUE), 
                 "The entire row will be removed from the dataframe.")
  
  expect_equal(nrow(out), 9)
})


test_that("na_filter correctly detects NAs", {
  
  temp_dat <- data.frame(A = c(1:10), B = c(runif(9), NA), C = c(letters[1:9], NA))
  
  expect_message(na_filter(temp_dat, "pollock", x = c("B", "C")),  
                 "The following columns contain NAs: B, C.")
})

test_that("na_filter replaces correct value", {
  
  temp_dat <- data.frame(A = c(1:9, NA), B = c(runif(10)), C = c(letters[1:10]))
  
  expect_message(out <- na_filter(temp_dat, "pollock", x = "A", replace = TRUE), 
                 "All NAs in A have been replaced with 5")
  
  expect_equal(out$A[[10]], 5)
})

test_that("na_filter removes correct row", {
  
  temp_dat <- data.frame(A = c(1:9, NA), B = c(runif(10)), C = c(letters[1:10]))
  
  expect_message(out <- na_filter(temp_dat, "pollock", x = "A", remove = TRUE), 
                 "The entire row will be removed from the dataframe.")
  
  expect_equal(nrow(out), 9)
})
