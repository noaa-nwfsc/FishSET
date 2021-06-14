PollockData <- subset(PollockData, 
                      !is.na(LBS_270_POLLOCK_LBS) & !is.na(LBS_110_PACIFIC_COD_LBS))

PollockData <- create_duration(PollockData, "pollock", 
                               start = "DATE_FISHING_BEGAN", end = "DATE_LANDED", 
                               "day", "trip_dur_days")

PollockData$trip_dur_days <- abs(PollockData$trip_dur_days)

PollockData <- cpue(PollockData, "pollock", "LBS_270_POLLOCK_LBS", 
                    "trip_dur_days", name = "pollock_cpue")

PollockData <- cpue(PollockData, "pollock", "LBS_110_PACIFIC_COD_LBS", 
                    "trip_dur_days", name = "pcod_cpue")

test_that("weekly_effort: basic plot", {
  
  expect_snapshot_plot("plot",
                       weekly_effort(PollockData, "pollock", cpue = "pollock_cpue",
                                    date = "HAUL_DATE", output = "plot"))
})

test_that("weekly_effort: plot by group", {
  
  expect_snapshot_plot("plot_grp",
                       weekly_effort(PollockData, "pollock", cpue = "pollock_cpue",
                                    date = "HAUL_DATE", group = "GEAR_TYPE",
                                    output = "plot"))
})


test_that("weekly_effort: plot group + facet", {
  
  expect_snapshot_plot("plot_grp_fct",
                       weekly_effort(PollockData, "pollock", cpue = "pollock_cpue",
                                    date = "HAUL_DATE", group = "GF_TARGET_FT",
                                    facet_by = "GEAR_TYPE", output = "plot"))
})

test_that("weekly_effort: plot 2 cpue", {
  
  expect_snapshot_plot("plot_2cpue",
                       weekly_effort(PollockData, "pollock", 
                                     cpue = c("pollock_cpue", "pcod_cpue"),
                                    date = "HAUL_DATE", output = "plot"))
})

test_that("weekly_effort: plot facet by species", {
  
  expect_snapshot_plot("plot_cpue_fct",
                       weekly_effort(PollockData, "pollock", 
                                     cpue = c("pollock_cpue", "pcod_cpue"),
                                    date = "HAUL_DATE", facet_by = "species",
                                    output = "plot"))
})

test_that("weekly_effort: plot 2 group", {
  
  expect_snapshot_plot("plot_2grp",
                       weekly_effort(PollockData, "pollock", 
                                     cpue = c("pollock_cpue", "pcod_cpue"),
                                    date = "HAUL_DATE", group = "GEAR_TYPE",
                                    output = "plot"))
})