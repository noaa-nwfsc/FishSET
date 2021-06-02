context("Data type conversion (changeclass)")

test_that("Converts integer to numeric", {
  
  temp_dat <- data.frame(A = as.integer(1:10), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "numeric")
  
  expect_equal(class(out$A), "numeric")
  expect_equal(out$A, as.numeric(temp_dat$A))
})

test_that("Converts character to numeric", {
  
  temp_dat <- data.frame(A = as.character(1:10), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "numeric")
  
  expect_equal(class(out$A), "numeric")
  expect_equal(out$A, as.numeric(as.character(temp_dat$A)))
})

test_that("Converts Date to numeric", {
  
  temp_dat <- data.frame(A = seq.Date(as.Date("2021-01-01"), by = "day", length.out = 10),
                         B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "numeric")
  
  expect_equal(class(out$A), "numeric")
  expect_equal(out$A, as.numeric(temp_dat$A))
})

test_that("Converts character factor to numeric", {
  
  temp_dat <- data.frame(A = factor(rep(c("a", "b"), 5)),
                         B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "numeric")
  
  expect_equal(class(out$A), "numeric")
  expect_equal(out$A, as.numeric(temp_dat$A))
})

test_that("Converts numeric factor to numeric", {
  
  temp_dat <- data.frame(A = factor(11:20), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "numeric")
  
  expect_equal(class(out$A), "numeric")
  expect_equal(out$A, 11:20)
})

test_that("Correctly converts numeric to character", {
  
  temp_dat <- data.frame(A = as.numeric(1:10), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "character")
  
  expect_equal(class(out$A), "character")
  expect_equal(out$A, as.character(1:10))
})

test_that("Correctly converts integer to character", {
  
  temp_dat <- data.frame(A = as.integer(1:10), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "character")
  
  expect_equal(class(out$A), "character")
  expect_equal(out$A, as.character(temp_dat$A))
})

test_that("Converts character factor to character", {
  
  temp_dat <- data.frame(A = factor(rep(letters[2:6], 2)), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "character")
  
  expect_equal(class(out$A), "character")
  expect_equal(out$A, rep(letters[2:6], 2))
})

test_that("Converts numeric factor to character", {
  
  temp_dat <- data.frame(A = factor(1:10), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "character")
  
  expect_equal(class(out$A), "character")
  expect_equal(out$A, as.character(1:10))
})

test_that("Converts date to character", {
  
  d_var <- seq.Date(as.Date("2021-01-01"), by = "day", length.out = 10)
  temp_dat <- data.frame(A = d_var, B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "character")
  
  expect_equal(class(out$A), "character")
  expect_equal(out$A, as.character(d_var))
})

test_that("Converts integer to factor", {
  
  temp_dat <- data.frame(A = as.integer(1:10), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "factor")
  
  expect_equal(class(out$A), "factor")
  expect_equal(out$A, as.factor(temp_dat$A))
})

test_that("Converts numeric to factor", {
  
  temp_dat <- data.frame(A = as.numeric(1:10), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "factor")
  
  expect_equal(class(out$A), "factor")
  expect_equal(out$A, as.factor(temp_dat$A))
})

test_that("Converts character to factor", {
  
  temp_dat <- data.frame(A = letters[1:10], B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "factor")
  
  expect_equal(class(out$A), "factor")
  expect_equal(out$A, as.factor(temp_dat$A))
})

test_that("Converts date to factor", {
  
  d_var <- seq.Date(as.Date("2021-01-01"), by = "day", length.out = 10)
  temp_dat <- data.frame(A = d_var, B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "factor")
  
  expect_equal(class(out$A), "factor")
  expect_equal(out$A, as.factor(d_var))
})


test_that("Converts date factor to date", {
  
  d_var <- seq.Date(as.Date("2021-01-01"), by = "day", length.out = 10)
  temp_dat <- data.frame(A = as.factor(format(as.Date(d_var), "%Y%m%d")), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "date")
  
  expect_equal(class(out$A), "Date")
  expect_equal(out$A, d_var)
})

test_that("Converts integer Date to Date", {
  
  d_var <- seq.Date(as.Date("2021-01-01"), by = "day", length.out = 10)
  temp_dat <- data.frame(A =  as.integer(format(as.Date(d_var), "%Y%m%d")), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "date")
  
  expect_equal(class(out$A), "Date")
  expect_equal(out$A, d_var)
})

test_that("Converts numeric Date to Date", {
  
  d_var <- seq.Date(as.Date("2021-01-01"), by = "day", length.out = 10)
  temp_dat <- data.frame(A =  as.numeric(format(as.Date(d_var), "%Y%m%d")), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "date")
  
  expect_equal(class(out$A), "Date")
  expect_equal(out$A, d_var)
})

test_that("Converts integer Date-Time to Date", {
  
  d_var <- seq.Date(as.Date("2021-01-01"), by = "day", length.out = 10)
  dt_var <- as.POSIXct(paste(d_var, paste0(1:10, ":00:00")))
  temp_dat <- data.frame(A = as.integer(as.POSIXct(dt_var, format="%Y%m%d %H:%M:%S")), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "date")
  
  expect_equal(class(out$A), "Date")
  expect_equal(out$A, d_var)
})

test_that("Converts numeric Date-Time to Date", {
  
  d_var <- seq.Date(as.Date("2021-01-01"), by = "day", length.out = 10)
  dt_var <- as.POSIXct(paste(d_var, paste0(1:10, ":00:00")))
  temp_dat <- data.frame(A = as.numeric(dt_var), B = runif(10))
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "date")
  
  expect_equal(class(out$A), "Date")
  expect_equal(out$A, d_var)
})

test_that("Converts character Date to Date", {
  
  d_var <- seq.Date(as.Date("2021-01-01"), by = "day", length.out = 10)
  temp_dat <- data.frame(A = as.character(d_var), B = runif(10))
  temp_dat$A <- as.character(temp_dat$A)
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "date")
  
  expect_equal(class(out$A), "Date")
  expect_equal(out$A, d_var)
})

test_that("Converts character Date-Time to Date", {
  
  d_var <- seq.Date(as.Date("2021-01-01"), by = "day", length.out = 10)
  dt_var <- as.POSIXct(paste(d_var, paste0(1:10, ":00:00")))
  temp_dat <- data.frame(A = as.character(dt_var), B = runif(10))
  temp_dat$A <- as.character(temp_dat$A)
  out <- changeclass(temp_dat, "pollock", x = "A", newclass = "date")
  
  expect_equal(class(out$A), "Date")
  expect_equal(out$A, d_var)
})
