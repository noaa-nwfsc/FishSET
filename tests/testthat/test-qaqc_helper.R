context("QAQC helper")

test_that("Logical ouput returns one value per column", {
  
  out <- qaqc_helper(PollockData, is.numeric, output = "logical")
  expect_equal(length(out), ncol(PollockData))
})


test_that("Expect error if function returns multiple values", {
  
  expect_error(qaqc_helper(PollockData, range, output = "value"))
})


