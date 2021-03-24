context("Fleet summary functions")


test_that("Vessel Count returns a dataframe", {
  
  expect_true(
    is.data.frame(
      vessel_count(PollockData, "pollock", v_id = "PERMIT", output = "table")
      )
    )
})
