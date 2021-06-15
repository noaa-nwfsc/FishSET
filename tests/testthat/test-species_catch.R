PollockData <- subset(PollockData, 
                      !is.na(LBS_270_POLLOCK_LBS) & !is.na(LBS_110_PACIFIC_COD_LBS))

test_that("species_catch: basic plot", {
  
  expect_snapshot_plot("basic_plot",
                       species_catch(PollockData, "pollock", "LBS_OTHER_LBS", 
                                     output = "plot"))
})

test_that("species_catch: plot by year", {
  
  expect_snapshot_plot("plot_year",
                       species_catch(PollockData, "pollock", "LBS_OTHER_LBS", 
                                     date = "HAUL_DATE", period = "year", output = "plot"))
})

test_that("species_catch: plot by month", {
  
  expect_snapshot_plot("plot_month",
                       species_catch(PollockData, "pollock", "LBS_270_POLLOCK_LBS", 
                                     date = "HAUL_DATE", period = "month", 
                                     output = "plot"))
})

test_that("species_catch: plot by week", {
  
  expect_snapshot_plot("plot_week",
                       species_catch(PollockData, "pollock", "LBS_270_POLLOCK_LBS", 
                                     date = "HAUL_DATE", period = "week", output = "plot"))
})

test_that("species_catch: plot by day", {
  
  expect_snapshot_plot("plot_day",
                       species_catch(PollockData, "pollock", "LBS_270_POLLOCK_LBS", 
                                     date = "HAUL_DATE", period = "cal_date", 
                                     output = "plot"))
})

test_that("species_catch: plot by group", {
  
  expect_snapshot_plot("plot_grp",
                       species_catch(PollockData[!is.na(PollockData$LBS_270_POLLOCK_LBS),], 
                                     "pollock", "LBS_270_POLLOCK_LBS", 
                                     group = "GEAR_TYPE", output = "plot"))
})

test_that("species_catch: plot by 2 groups", {
  
  expect_snapshot_plot("plot_2grp",
                       species_catch(PollockData, "pollock", "LBS_270_POLLOCK_LBS", 
                                     group = c("GEAR_TYPE", "GF_TARGET_FT"),
                                     output = "plot"))
})

test_that("species_catch: plot by facet", {
  
  expect_snapshot_plot("plot_fct",
                       species_catch(PollockData[!is.na(PollockData$LBS_270_POLLOCK_LBS),],
                                     "pollock", "LBS_270_POLLOCK_LBS", 
                                     facet_by = c("GF_TARGET_FT"), output = "plot"))
})

test_that("species_catch: plot by group + facet", {
  
  expect_snapshot_plot("plot_grp_fct",
                       species_catch(PollockData, "pollock", "LBS_270_POLLOCK_LBS", 
                                     facet_by = c("GEAR_TYPE"), group = "GF_TARGET_FT",
                                     output = "plot"))
})

test_that("species_catch: plot month + grp", {
  
  expect_snapshot_plot("plot_mon_grp",
                       species_catch(PollockData, "pollock", "LBS_270_POLLOCK_LBS", 
                                     date = "HAUL_DATE",period = "month", 
                                     group = "GEAR_TYPE", output = "plot"))
})

test_that("species_catch: plot month + grp + fct", {
  
  expect_snapshot_plot("mon_grp_fct",
                       species_catch(PollockData, 
                                     "pollock", "LBS_270_POLLOCK_LBS", 
                                     date = "HAUL_DATE", period = "month", 
                                     facet_by = "GEAR_TYPE", group = "GF_TARGET_FT", 
                                     output = "plot"))
})

test_that("species_catch: plot by month + 2 species", {
  
  expect_snapshot_plot("plot_month_2spec",
                       species_catch(PollockData, "pollock",
                                     species = c("LBS_270_POLLOCK_LBS", "LBS_110_PACIFIC_COD_LBS"),
                                     date = "HAUL_DATE", period = "month", 
                                     output = "plot"))
})

test_that("species_catch: plot species facet", {
  
  expect_snapshot_plot("plot_month_spec_fct",
                       species_catch(PollockData, "pollock",
                                     species = c("LBS_270_POLLOCK_LBS", "LBS_110_PACIFIC_COD_LBS"),
                                     date = "HAUL_DATE", period = "month", 
                                     facet_by = "species", output = "plot"))
})