

test_that("Test that data can be uploaded", {
  
  expect_true(load_maindata(PollockData, over_write = TRUE, project = "testload"))

  on.exit({
    lapply(project_tables("testload"), table_remove)
  }, add = TRUE, after = FALSE)
})

#test_that("Table can be saved locally", {
#  
#  expect_true(write_dat(PollockData, "~/pollocktest.csv", file_type = "csv", "pollock"))
#  on.exit(file.remove("~/pollocktest.csv"), add = TRUE, after = FALSE)
#})
