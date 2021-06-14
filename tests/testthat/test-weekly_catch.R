PollockData <- subset(PollockData, 
                      !is.na(LBS_270_POLLOCK_LBS) & !is.na(LBS_110_PACIFIC_COD_LBS))

test_that("weekly_catch: basic plot", {
  
  expect_snapshot_plot("plot",
                       weekly_catch(PollockData, "pollock", "LBS_270_POLLOCK_LBS",
                                    date = "HAUL_DATE", output = "plot"))
})

test_that("weekly_catch: plot by group", {
  
  expect_snapshot_plot("plot_grp",
                       weekly_catch(PollockData, "pollock", "LBS_270_POLLOCK_LBS",
                                    date = "HAUL_DATE", group = "GEAR_TYPE",
                                    output = "plot"))
})


test_that("weekly_catch: plot group + facet", {
  
  expect_snapshot_plot("plot_grp_fct",
                       weekly_catch(PollockData, "pollock", "LBS_270_POLLOCK_LBS",
                                    date = "HAUL_DATE", group = "GF_TARGET_FT",
                                    facet_by = "GEAR_TYPE", output = "plot"))
})

test_that("weekly_catch: plot 2 species", {
  
  expect_snapshot_plot("plot_2spec",
                       weekly_catch(PollockData, "pollock", 
                                    c("LBS_270_POLLOCK_LBS", "LBS_110_PACIFIC_COD_LBS"),
                                    date = "HAUL_DATE", output = "plot"))
})

test_that("weekly_catch: plot facet by species", {
  
  expect_snapshot_plot("plot_spec_fct",
                       weekly_catch(PollockData, "pollock", 
                                    c("LBS_270_POLLOCK_LBS", "LBS_110_PACIFIC_COD_LBS"),
                                    date = "HAUL_DATE", facet_by = "species",
                                    output = "plot"))
})

test_that("weekly_catch: plot 2 group", {
  
  expect_snapshot_plot("plot_2grp",
                       weekly_catch(PollockData, "pollock", 
                                    c("LBS_270_POLLOCK_LBS", "LBS_110_PACIFIC_COD_LBS"),
                                    date = "HAUL_DATE", group = "GEAR_TYPE",
                                    type = "line", output = "plot"))
})