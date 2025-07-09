library(shinytest2)
app_dir <- system.file("ShinyFiles/MainApp", package = "FishSET")
shinytest2::test_app(app_dir, test_file = NULL)
