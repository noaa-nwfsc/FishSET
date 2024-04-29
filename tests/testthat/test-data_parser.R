
# Test load_maindata ----
test_that("Test loading main data works", {
  
  # Make sure project names that violate rules throw errors
  expect_error(load_maindata(dat = FishSET::scallop, over_write = TRUE, project = "test load"))
  expect_error(load_maindata(dat = FishSET::scallop, over_write = TRUE, project = ""))
  
  # 
  # on.exit({
  #   lapply(project_tables(project = "testload"), table_remove, project = "testload")
  # }, add = TRUE, after = FALSE)
})