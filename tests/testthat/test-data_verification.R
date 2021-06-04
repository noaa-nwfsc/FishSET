context("Data verification")

test_that("Detects duplicate rows", {
  
  temp_dat <- data.frame(A = rep(1:5, each = 2), B = rep(letters[1:5], each = 2), 
                         C = rep(seq(10, 50, by = 10), each = 2), 
                         lat = rep(seq(55, 56, by = .25), each = 2), 
                         lon = rep(seq(-165, -164, by = .25), each = 2))
  
  expect_message(data_verification(temp_dat, "pollock"),
                "Each row in dataset is not a unique choice occurrence at haul or trip level.")
})

test_that("Detects empty vars", {
  
  temp_dat <- data.frame(A = 1:10, B = letters[1:10], C = rep(NA, 10), 
                         lat = rep(seq(55, 56, by = .25), each = 2), 
                         lon = rep(seq(-165, -164, by = .25), each = 2))
  
  expect_message(data_verification(temp_dat, "pollock"),
                 "C is empty. Consider removing the column from the dataset")
})

test_that("Detects non-degree lat lon", {
  
  temp_dat <- data.frame(A = 1:10, B = letters[1:10],
                         lat = rep(seq(5500, 5600, by = 25), each = 2), 
                         lon = rep(seq(-16500, -16400, by = 25), each = 2))
  
  expect_message(data_verification(temp_dat, "pollock"),
                 "At least one lat/lon variable is not in decimal degrees")
})

test_that("unique_filter: detects duplicate rows", {
  
  temp_dat <- data.frame(A = rep(1:5, each = 2), B = rep(letters[1:5], each = 2), 
                         C = rep(seq(10, 50, by = 10), each = 2))
  
  expect_message(unique_filter(temp_dat, "pollock"),
                 "Each row in data set is not a unique choice occurrence at haul or trip level")
})

test_that("unique_filter: removes duplicate rows", {
  
  temp_dat <- data.frame(A = rep(1:5, each = 2), B = rep(letters[1:5], each = 2), 
                         C = rep(seq(10, 50, by = 10), each = 2)) # 5 unique rows
  
  out <- unique_filter(temp_dat, "pollock", remove = TRUE)
  expect_equal(nrow(out), 5)
})

test_that("unique_filter: returns dataset", {
  
  temp_dat <- data.frame(A = rep(1:5, each = 2), B = rep(letters[1:5], each = 2), 
                         C = rep(seq(10, 50, by = 10), each = 2)) # 5 unique rows
  
  out <- unique_filter(temp_dat, "pollock", remove = TRUE)
  expect_true(is.data.frame(out))
})


test_that("empty_vars_filter: detects empty var", {
  
  temp_dat <- data.frame(A = 1:10, B = letters[1:10], C = rep(NA, 10))
  
  expect_message(empty_vars_filter(temp_dat, "pollock"),
                 "C is empty. Consider removing from the data set")
})

test_that("empty_vars_filter: detects empty var", {
  
  temp_dat <- data.frame(A = 1:10, B = letters[1:10], C = rep(NA, 10))
  
  expect_message(out <- empty_vars_filter(temp_dat, "pollock", remove = TRUE),
                 "C is empty and has been removed from the data set.")
  
  expect_equal(out, temp_dat[-3])
})


dd_df <- data.frame(lat_dd = PollockData$LonLat_START_LAT[1:10],
                    lon_dd = PollockData$LonLat_START_LON[1:10])

dms_df <- data.frame(lat_dms = as.character(sp::dd2dms(dd_df$lat_dd)),
                     lon_dms = paste0("-", as.character(sp::dd2dms(dd_df$lon_dd))))

test_that("degree: detects lat lon degrees", {

expect_message(degree(dd_df, "pollock", lat = "lat_dd", lon = "lon_dd"),
               "Latitude and longitude variables in decimal degrees")
  
})

test_that("degree: converts decimal minutes", {
  
  out <- degree(dms_df, "pollock", lat = "lat_dms", lon = "lon_dms", replace = TRUE)
  
  expect_equal(out$lat_dms, dd_df$lat_dd)
  expect_equal(out$lon_dms, dd_df$lon_dd)
})

test_that("degree: converts packed DMS", {
  
  pdms_df <- data.frame(lat_pdms = dms_to_pdms(dms_df$lat_dms, type = "lat", as_num = TRUE),
                        lon_pdms = dms_to_pdms(dms_df$lon_dms, type = "lon", as_num = TRUE))
  
  out <- degree(pdms_df, "pollock", lat = "lat_pdms", lon = "lon_pdms", replace = TRUE)
  
  expect_equal(round(out$lat_pdms, 3), round(dd_df$lat_dd, 3))
  expect_equal(round(out$lon_pdms, 3), round(dd_df$lon_dd, 3))
})


test_that("degree: changes signs", {
  
  sign_df <- data.frame(lat_dd = dd_df$lat_dd * -1, lon_dd = dd_df$lon_dd * -1)
  
  out <- degree(sign_df, "pollock", lat = "lat_dd", lon = "lon_dd", latsign = "pos",
                lonsign = "neg", replace = TRUE)

  expect_equal(out, dd_df)
  
  out <- degree(sign_df, "pollock", lat = "lat_dd", lon = "lon_dd", latsign = "all",
                lonsign = "all", replace = TRUE)
  
  expect_equal(out, dd_df)
  
  out <- degree(sign_df, "pollock", lat = "lat_dd", lon = "lon_dd", latsign = "neg",
                lonsign = "pos", replace = TRUE)
  
  expect_equal(out, sign_df)
})
