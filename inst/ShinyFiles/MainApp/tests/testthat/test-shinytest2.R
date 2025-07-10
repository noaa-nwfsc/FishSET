
test_that("MainApp", {
  library(shinytest2)
  
  app <- AppDriver$new(name = "MainApp")
  app$click("folderpath-change_fs_folder_btn")
  
  expect_equal(
    app$get_values()$input$`select_main-main_select_input`,
    "s1MainDataTable"
  )
})


# test_that("{shinytest2} recording: load-data", {
#   app <- AppDriver$new(name = "load-data")
#   app$click("folderpath-change_fs_folder_btn")
#   app$click("load_data-load_data_btn")
#   app$expect_values()
# })
# 
# 
# test_that("{shinytest2} recording: test-load", {
#   app <- AppDriver$new(name = "test-load")
#   app$set_inputs(`select_project-load_existing_proj_input` = TRsUE)
#   app$click("folderpath-change_fs_folder_btn")
#   app$click("load_data-load_data_btn")
#   app$expect_values()
# })
# 
# 
# test_that("{shinytest2} recording: test-locproj", {
#   app_dir <- system.file("ShinyFiles/MainApp", package = "FishSET")
#   app <- AppDriver$new(
#     app_dir = app_dir,
#     name = "test-locproj")
#   app$click("folderpath-change_fs_folder_btn")
#   app$click("load_data-load_data_btn")
# 
#   app$get_values(export = TRUE)
# })
