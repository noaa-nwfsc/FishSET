
test_that("MainApp", {
  library(shinytest2)
  
  app <- AppDriver$new(name = "MainApp")
  app$click("folderpath-change_fs_folder_btn")
  
  expect_equal(
    app$get_values()$input$`select_main-main_select_input`,
    "s1MainDataTable"
  )
})
