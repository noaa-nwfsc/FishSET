

test_that("agg_helper returns a dataframe", {
  
  expect_true(
    is.data.frame(agg_helper(PollockData, value = "LBS_270_POLLOCK_LBS", fun = "sum"))
  )
  
  expect_true(
    is.data.frame(agg_helper(PollockData, value = c("LBS_270_POLLOCK_LBS", "LBS_110_PACIFIC_COD_LBS"), 
                             fun = "sum"))
  )
  
  expect_true(
    is.data.frame(agg_helper(PollockData, value = c("LBS_270_POLLOCK_LBS", "LBS_110_PACIFIC_COD_LBS"), 
                             group = "GEAR_TYPE", fun = "sum"))
  )
})

test_that("Throws warning if column is not in dataset",
          expect_warning(agg_helper(PollockData, value = "FOOBAR"))
)

test_that("Calling an anonymous function works",
          
          expect_true(
            is.data.frame(
              agg_helper(PollockData, value = "LBS_OTHER_LBS", fun = function(x) sum(x)^2))
          )
)

test_that("Correct value is generated for one value, zero groups", {
  out <- agg_helper(PollockData, value = "LBS_OTHER_LBS", fun = "sum")
  expect_equal(out$LBS_OTHER_LBS, sum(PollockData$LBS_OTHER_LBS, na.rm = FALSE))
  
})

test_that("Correct value is generated for one value, one group", {
  
  out <- agg_helper(PollockData, value = "LBS_OTHER_LBS", group = "GEAR_TYPE", fun = "sum")
  out2 <- aggregate(LBS_OTHER_LBS ~ GEAR_TYPE, PollockData, FUN = sum, na.action = NULL)
  expect_equal(out, out2)
})

test_that("Correct value is generated for 2 values, one group", {
  
  out <- agg_helper(PollockData, value = c("LBS_OTHER_LBS", "LBS_270_POLLOCK_LBS"), 
                    group = "GEAR_TYPE", fun = "sum")
  out2 <- aggregate(cbind(LBS_OTHER_LBS, LBS_270_POLLOCK_LBS) ~ GEAR_TYPE, 
                    PollockData, FUN = sum, na.action = NULL)
  expect_equal(out, out2)
})

test_that("Correct value is generated for 2 values, 2 groups", {
  
  out <- agg_helper(PollockData, value = c("LBS_OTHER_LBS", "LBS_270_POLLOCK_LBS"), 
                    group = c("GEAR_TYPE", "EMBARKED_PORT"), fun = "sum")
  out2 <- aggregate(cbind(LBS_OTHER_LBS, LBS_270_POLLOCK_LBS) ~ GEAR_TYPE + EMBARKED_PORT, PollockData, 
                    FUN = sum, na.action = NULL)
  expect_equal(out, out2)
})
