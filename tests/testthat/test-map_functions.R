
test_that("map_plot plot is correct", {
  
  expect_snapshot_plot("map_plot", 
                       map_plot(PollockData, "pollock", "LonLat_START_LAT", 
                                "LonLat_START_LON"))
})

test_that("map_kernel point plot is correct", {
  
  expect_snapshot_plot("map_kernel_point", 
                       map_kernel(PollockData, "pollock", type = "point", 
                                  c("LonLat_START_LAT", "LonLat_START_LON")))
})

test_that("map_kernel point plot w/ group", {
  
  expect_snapshot_plot("map_kernel_point_grp", 
                       map_kernel(PollockData, "pollock", type = "point", 
                                  c("LonLat_START_LAT", "LonLat_START_LON"),
                                  group = "GEAR_TYPE"))
})

test_that("map_kernel point plot w/ group + facet", {
  
  expect_snapshot_plot("map_kernel_point_grp_fct", 
                       map_kernel(PollockData, "pollock", type = "point", 
                                  c("LonLat_START_LAT", "LonLat_START_LON"),
                                  group = "GEAR_TYPE", facet = TRUE))
})

test_that("map_kernel contour plot is correct", {
  
  expect_snapshot_plot("map_kernel_contours", 
                       map_kernel(PollockData, "pollock", type = "contours", 
                                  c("LonLat_START_LAT", "LonLat_START_LON")))
})

test_that("map_kernel contour w/ group", {
  
  expect_snapshot_plot("map_kernel_contours_grp", 
                       map_kernel(PollockData, "pollock", type = "contours", 
                                  c("LonLat_START_LAT", "LonLat_START_LON"),
                                  group = "GEAR_TYPE"))
})

test_that("map_kernel contour w/ group + facet", {
  
  expect_snapshot_plot("map_kernel_contours_grp_fct", 
                       map_kernel(PollockData, "pollock", type = "contours", 
                                  c("LonLat_START_LAT", "LonLat_START_LON"),
                                  group = "GEAR_TYPE", facet = TRUE))
})

test_that("map_kernel gradient plot is correct", {
  
  expect_snapshot_plot("map_kernel_gradient", 
                       map_kernel(PollockData, "pollock", type = "gradient", 
                                  c("LonLat_START_LAT", "LonLat_START_LON")))
})

test_that("map_kernel gradient plot w/ group + facet", {
  
  expect_snapshot_plot("map_kernel_gradient_grp_fct", 
                       map_kernel(PollockData, "pollock", type = "gradient", 
                                  c("LonLat_START_LAT", "LonLat_START_LON"),
                                  group = "GEAR_TYPE", facet = TRUE))
})


test_that("gradient w/ group no facet returns error", {
  
  expect_error(map_kernel(PollockData, "pollock", type = "gradient", 
                          c("LonLat_START_LAT", "LonLat_START_LON"),
                          group = "GEAR_TYPE"))
})