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
  overlap_result <- FishSET:::compute_closure_overlaps(
    uploaded_files,
    zones,
    50,
    return_shape = TRUE
  )
  expect_equal(overlap_result$ids, "Zone_1")
  expect_s3_class(overlap_result$shape, "sf")
  expect_equal(
    FishSET:::compute_closure_overlaps(uploaded_files, zones, 70),
    character(0)
  )
})

test_that("compute_closure_overlaps accepts a single GeoJSON file", {
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
  geojson <- tempfile(fileext = ".geojson")
  sf::st_write(closure, geojson, quiet = TRUE)
  on.exit(unlink(geojson), add = TRUE)

  uploaded_files <- data.frame(
    name = basename(geojson),
    datapath = geojson,
    stringsAsFactors = FALSE
  )

  expect_equal(
    FishSET:::compute_closure_overlaps(uploaded_files, zones, 50),
    "Zone_1"
  )
})

test_that("compute_closure_overlaps accepts an sf RDS file", {
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
  rds <- tempfile(fileext = ".rds")
  saveRDS(closure, rds)
  on.exit(unlink(rds), add = TRUE)

  expect_equal(
    FishSET:::compute_closure_overlaps(
      data.frame(name = basename(rds), datapath = rds),
      zones,
      50
    ),
    "Zone_1"
  )
})

test_that("compute_closure_overlaps accepts CSV WKT data", {
  zones <- sf::st_as_sf(
    data.frame(second_location_id = c("Zone_1", "Zone_2"), wkt = c(
      "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
      "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
    )),
    wkt = "wkt",
    crs = 4326
  )
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(geometry = "POLYGON ((0 0, 0.6 0, 0.6 1, 0 1, 0 0))"),
    csv,
    row.names = FALSE
  )
  on.exit(unlink(csv), add = TRUE)

  expect_equal(
    FishSET:::compute_closure_overlaps(
      data.frame(name = basename(csv), datapath = csv),
      zones,
      50
    ),
    "Zone_1"
  )
})

test_that("compute_closure_overlaps preserves point-zone selection", {
  zones <- sf::st_as_sf(
    data.frame(second_location_id = c("Zone_1", "Zone_2"), lon = c(0, 2), lat = c(0, 0)),
    coords = c("lon", "lat"),
    crs = 4326
  )
  closure <- sf::st_as_sf(
    data.frame(id = 1, wkt = "POLYGON ((-1 -1, 1 -1, 1 1, -1 1, -1 -1))"),
    wkt = "wkt",
    crs = 4326
  )
  rds <- tempfile(fileext = ".rds")
  saveRDS(closure, rds)
  on.exit(unlink(rds), add = TRUE)

  expect_equal(
    FishSET:::compute_closure_overlaps(
      data.frame(name = basename(rds), datapath = rds),
      zones,
      50
    ),
    "Zone_1"
  )
})
