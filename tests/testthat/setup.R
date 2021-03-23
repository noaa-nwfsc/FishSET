# this script runs before tests are executed

data("PollockData")

# change the database and log/output filepaths to the test folder
loc <<- "~/FishSET_RPackage/tests/testdb" # change DB directory
loc2 <<- "~/FishSET_RPackage/tests/testinst" # change log/output directory

# upload a testMainDataTable for general use
# if (!table_exists("testMainDataTable")) {
#   
#   load_maindata(PollockData, project = "test")
# }