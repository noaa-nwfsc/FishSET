PollockData <- subset(PollockData, 
                      !is.na(LBS_270_POLLOCK_LBS) & !is.na(LBS_110_PACIFIC_COD_LBS))

test_that("roll_catch: basic plot", {
  
  expect_snapshot_plot("plot", 
                       roll_catch(PollockData, "pollock", catch = "LBS_270_POLLOCK_LBS",
                                  date = "HAUL_DATE", fun = "mean", output = "plot"))
})

test_that("roll_catch: plot by group", {
  
  expect_snapshot_plot("plot_grp", 
                       roll_catch(PollockData, "pollock", catch = "LBS_270_POLLOCK_LBS",
                                  date = "HAUL_DATE", group = "GEAR_TYPE", fun = "mean",
                                  output = "plot"))
})

test_that("roll_catch: plot by group + facet", {
  
  expect_snapshot_plot("plot_grp_fct", 
                       roll_catch(PollockData, "pollock", catch = "LBS_270_POLLOCK_LBS",
                                  date = "HAUL_DATE", facet_by = "GEAR_TYPE",
                                  group = "GF_TARGET_FT", fun = "mean", output = "plot"))
})

test_that("roll_catch: plot by 2 species", {
  
  expect_snapshot_plot("plot_2spec", 
                       roll_catch(PollockData, "pollock",
                                  catch = c("LBS_270_POLLOCK_LBS", 
                                            "LBS_110_PACIFIC_COD_LBS"),
                                  date = "HAUL_DATE", fun = "mean", output = "plot"))
})

test_that("roll_catch: facet by species", {
  
  expect_snapshot_plot("plot_spec_fct", 
                       roll_catch(PollockData, "pollock",
                                  catch = c("LBS_270_POLLOCK_LBS", 
                                            "LBS_110_PACIFIC_COD_LBS"),
                                  date = "HAUL_DATE", facet_by = "species",
                                  fun = "mean", output = "plot"))
})

test_that("roll_catch: plot 2 groups", {
  
  expect_snapshot_plot("plot_2grp", 
                       roll_catch(PollockData, "pollock",
                                  catch = c("LBS_270_POLLOCK_LBS", 
                                            "LBS_110_PACIFIC_COD_LBS"),
                                  date = "HAUL_DATE", group = "GEAR_TYPE",
                                  fun = "mean", output = "plot"))
})

test_that("roll_catch: combine group", {
  
  expect_snapshot_plot("plot_combine", 
                       roll_catch(PollockData, "pollock", 
                                  catch = "LBS_270_POLLOCK_LBS", 
                                  date = "HAUL_DATE", group = c("GEAR_TYPE", 
                                                                "GF_TARGET_FT"),
                                  combine = TRUE, fun = "mean", output = "plot"))
})

PollockData <- PollockData[c("LBS_270_POLLOCK_LBS", "HAUL_DATE", "GEAR_TYPE")]

PollockData$HAUL_DATE <- date_parser(PollockData$HAUL_DATE)
new_year <- PollockData$HAUL_DATE + lubridate::dyears(1)
pdat2 <- PollockData
pdat2$HAUL_DATE <- new_year

PollockData <- rbind(PollockData, pdat2)

test_that("roll_catch: facet by year", {
  
  expect_snapshot_plot("plot_fct_yr", 
                       roll_catch(PollockData, "pollock",
                                  catch = "LBS_270_POLLOCK_LBS",
                                  date = "HAUL_DATE", group = "GEAR_TYPE",
                                  facet_by = "year", fun = "mean", output = "plot"))
})