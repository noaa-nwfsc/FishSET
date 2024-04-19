# this script runs before tests are executed
# TODO: this may need to be changed to testDat <- FishSET::PollockData
data("PollockData")

# change the database and log/output filepaths to the test folder
loc <- "~/FishSET_RPackage/tests/testdb" # change DB directory
loc2 <- "~/FishSET_RPackage/tests/testinst" # change log/output directory

# upload a testMainDataTable for general use
# if (!table_exists("testMainDataTable")) {
#   
#   load_maindata(PollockData, project = "test")
# }


save_png <- function(code, width = 800, height = 800) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  print(code)
  
  path
}

expect_snapshot_plot <- function(name, code) {

  local_edition(3)
  
  skip_if_not_installed("ggplot2", "2.0.0")
  
  path <- save_png(code)
  expect_snapshot_file(path, paste0(name, ".png"))
}
