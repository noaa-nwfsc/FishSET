test_that("compute_closure_overlaps selects zones meeting the threshold", {
  zones <- sf::st_as_sf(
    data.frame(second_location_id = c("Zone_1", "Zone_2"), wkt = c(
      "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
      "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
    )),
    wkt = "wkt",
    crs = 4326
  )
  closure <- sf::st_as_sf(
    data.frame(id = 1, wkt = "POLYGON ((0 0, 0.6 0, 0.6 1, 0 1, 0 0))"),
    wkt = "wkt",
    crs = 4326
  )
  shapefile <- tempfile(fileext = ".shp")
  sf::st_write(closure, shapefile, quiet = TRUE)
  on.exit(unlink(sub("\\.shp$", ".*", shapefile)), add = TRUE)
  
  components <- list.files(
    dirname(shapefile),
    pattern = paste0("^", tools::file_path_sans_ext(basename(shapefile)), "\\."),
    full.names = TRUE
  )
  uploaded_files <- data.frame(
    name = basename(components),
    datapath = components,
    stringsAsFactors = FALSE
  )
  
  expect_equal(
    FishSET:::compute_closure_overlaps(uploaded_files, zones, 50),
    "Zone_1"
  )
  expect_equal(
    FishSET:::compute_closure_overlaps(uploaded_files, zones, 70),
    character(0)
  )
})
