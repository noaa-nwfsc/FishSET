context("Outlier Check")

test_that("outlier table is returned", {
  
 out <- outlier_table(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS")
 
 expect_true(is.data.frame(out))
 
 expect_equal(names(out), c("Vector", "outlier_check", "N", "mean", "median", "SD", 
                            "min", "max", "NAs", "skew"))
})

test_that("outlier_table: Non-numeric variables are detected, factor", {
  
  expect_output(outlier_table(PollockData, "pollock", x = "GEAR_TYPE"),
                "Table not available. Data is not numeric.")
})

test_that("outlier_table: Non-numeric variables are detected, character and date", {
  
  temp_dat <- data.frame(A = as.character(1:10), 
                         B = seq.Date(as.Date("2021-01-01"), by = "day", length.out = 10))
  
  expect_output(outlier_table(temp_dat, "pollock", x = "A"),
                "Table not available. Data is not numeric.")
  expect_output(outlier_table(temp_dat, "pollock", x = "B"),
                "Table not available. Data is not numeric.")
})

test_that("outlier_plot: plot is returned", {
  
  out <- 
  outlier_plot(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS", 
               dat.remove = "5_95_quant", x.dist = "normal", output.screen = TRUE)
  
  expect_true(ggplot2::is.ggplot(out))
})

test_that("outlier remove: dat.remove = 'none'", {
  
  out_rem <- outlier_remove(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS",
                            dat.remove = "none")
  expect_equal(nrow(out_rem), nrow(PollockData))
})

test_that("outlier_remove: dat.remove = '5_95_quant'", {
  
  out_tab <- outlier_table(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS")
  out_rem <- outlier_remove(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS",
                            dat.remove = "5_95_quant")
  expect_equal(nrow(out_rem), out_tab[out_tab$outlier_check == "5_95_quant", "N"])
})

test_that("outlier_remove: dat.remove = '25_75_quant'", {
  
  out_tab <- outlier_table(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS")
  out_rem <- outlier_remove(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS",
                            dat.remove = "25_75_quant")
  expect_equal(nrow(out_rem), out_tab[out_tab$outlier_check == "25_75_quant", "N"])
})

test_that("outlier_remove: dat.remove = 'mean_2SD'", {
  
  out_tab <- outlier_table(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS")
  out_rem <- outlier_remove(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS",
                            dat.remove = "mean_2SD")
  expect_equal(nrow(out_rem), out_tab[out_tab$outlier_check == "mean_2SD", "N"])
})

test_that("outlier_remove: dat.remove = 'median_2SD'", {
  
  out_tab <- outlier_table(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS")
  out_rem <- outlier_remove(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS",
                            dat.remove = "median_2SD")
  expect_equal(nrow(out_rem), out_tab[out_tab$outlier_check == "median_2SD", "N"])
})

test_that("outlier_remove: dat.remove = 'mean_3SD'", {
  
  out_tab <- outlier_table(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS")
  out_rem <- outlier_remove(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS",
                            dat.remove = "mean_3SD")
  expect_equal(nrow(out_rem), out_tab[out_tab$outlier_check == "mean_3SD", "N"])
})

test_that("outlier_remove: dat.remove = 'median_3SD'", {
  
  out_tab <- outlier_table(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS")
  out_rem <- outlier_remove(PollockData, "pollock", x = "LBS_270_POLLOCK_LBS",
                            dat.remove = "median_3SD")
  expect_equal(nrow(out_rem), out_tab[out_tab$outlier_check == "median_3SD", "N"])
})