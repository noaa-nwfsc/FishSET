library(shinytest2)

test_that("{shinytest2} recording: first_test", {
  app <- AppDriver$new(name = "first_test", height = 695, width = 1235)
  app$expect_values()
})
