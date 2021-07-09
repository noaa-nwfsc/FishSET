PollockData <- subset(PollockData, 
                      !is.na(LBS_270_POLLOCK_LBS) & !is.na(LBS_110_PACIFIC_COD_LBS))

PollockData <- create_duration(PollockData, "pollock", 
                               start = "DATE_FISHING_BEGAN", end = "DATE_LANDED", 
                               "day", "trip_dur_days")
set.seed(4)
PollockData$split_var <- sample(c(TRUE, FALSE), size = nrow(PollockData), replace = TRUE)

PollockData$trip_dur_days <- abs(PollockData$trip_dur_days)

PollockData <- cpue(PollockData, "pollock", "LBS_270_POLLOCK_LBS", 
                    "trip_dur_days", name = "pollock_cpue")

PollockData <- cpue(PollockData, "pollock", "LBS_110_PACIFIC_COD_LBS", 
                    "trip_dur_days", name = "pcod_cpue")

test_that("bycatch: basic plot", {
  
  expect_snapshot_plot("plot",
                       bycatch(PollockData, "pollock", cpue = "pollock_cpue",
                               catch = "LBS_270_POLLOCK_LBS", date = "HAUL_DATE", 
                               period = "month", value = "stc", output = "plot"))
})

test_that("bycatch: plot by group", {
  
  expect_snapshot_plot("plot_grp",
                       bycatch(PollockData, "pollock", cpue = "pollock_cpue",
                               catch = "LBS_270_POLLOCK_LBS", date = "HAUL_DATE", 
                               period = "month", group = "GEAR_TYPE", value = "stc", 
                               output = "plot"))
})

test_that("bycatch: plot by facet", {
  
  expect_snapshot_plot("plot_fct",
                       bycatch(PollockData, "pollock", cpue = "pollock_cpue",
                               catch = "LBS_270_POLLOCK_LBS", date = "HAUL_DATE", 
                               period = "month", facet_by = "GEAR_TYPE", value = "stc", 
                               output = "plot"))
})

test_that("bycatch: plot by grp + facet", {
  
  expect_snapshot_plot("plot_grp_fct",
                       bycatch(PollockData, "pollock", cpue = "pollock_cpue",
                               catch = "LBS_270_POLLOCK_LBS", date = "HAUL_DATE", 
                               period = "month", group = "GEAR_TYPE", facet_by = "split_var",
                               value = "stc", 
                               output = "plot"))
})

test_that("bycatch: plot by 2 species", {
  
  expect_snapshot_plot("plot_2spec",
                       bycatch(PollockData, "pollock", cpue = c("pollock_cpue", "pcod_cpue"),
                               catch = c("LBS_270_POLLOCK_LBS", "LBS_110_PACIFIC_COD_LBS"),
                               date = "HAUL_DATE", period = "month", value = "stc", 
                               output = "plot"))
})

test_that("bycatch: plot by 2 species + grp", {
  
  expect_snapshot_plot("plot_2spec_grp",
                       bycatch(PollockData, "pollock", cpue = c("pollock_cpue", "pcod_cpue"),
                               catch = c("LBS_270_POLLOCK_LBS", "LBS_110_PACIFIC_COD_LBS"),
                               date = "HAUL_DATE", period = "month", value = "stc", 
                               group = "GEAR_TYPE", output = "plot"))
})

test_that("bycatch: plot by 2 species + facet", {
  
  expect_snapshot_plot("plot_2spec_fct",
                       bycatch(PollockData, "pollock", cpue = c("pollock_cpue", "pcod_cpue"),
                               catch = c("LBS_270_POLLOCK_LBS", "LBS_110_PACIFIC_COD_LBS"),
                               date = "HAUL_DATE", period = "month", value = "stc", 
                               facet_by = "GEAR_TYPE", output = "plot"))
})

test_that("bycatch: plot by 2 species + fct + grp", {
  
  expect_snapshot_plot("plot_2spec_grp_fct",
                       bycatch(PollockData, "pollock", cpue = c("pollock_cpue", "pcod_cpue"),
                               catch = c("LBS_270_POLLOCK_LBS", "LBS_110_PACIFIC_COD_LBS"),
                               date = "HAUL_DATE", period = "month", value = "stc", 
                               group = "GEAR_TYPE", facet_by = "split_var", output = "plot"))
})