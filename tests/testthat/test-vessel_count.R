


test_that("Vessel Count returns a dataframe", {
  
  expect_true(
    is.data.frame(
      vessel_count(PollockData, "pollock", v_id = "PERMIT", output = "table")
      )
    )
})

test_that("vessel_count: basic plot", {
  
  expect_snapshot_plot("vessel_count_plot",
                       vessel_count(PollockData, "pollock", "PERMIT", output = "plot"))
})

test_that("vessel_count: plot by year", {
  
  expect_snapshot_plot("vessel_count_plot_year",
                       vessel_count(PollockData, "pollock", "PERMIT", date = "HAUL_DATE",
                                    period = "year", output = "plot"))
})

test_that("vessel_count: plot by month", {
  
  expect_snapshot_plot("vessel_count_plot_month",
                       vessel_count(PollockData, "pollock", "PERMIT", date = "HAUL_DATE",
                                    period = "month", output = "plot"))
})

test_that("vessel_count: plot by week", {
  
  expect_snapshot_plot("vessel_count_plot_week",
                       vessel_count(PollockData, "pollock", "PERMIT", date = "HAUL_DATE",
                                    period = "week", output = "plot"))
})

test_that("vessel_count: plot by day", {
  
  expect_snapshot_plot("vessel_count_plot_day",
                       vessel_count(PollockData, "pollock", "PERMIT", date = "HAUL_DATE",
                                    period = "cal_date", output = "plot"))
})

test_that("vessel_count: plot by group", {
  
  expect_snapshot_plot("vessel_count_plot_grp",
                       vessel_count(PollockData, "pollock", "PERMIT", group = "GEAR_TYPE",
                                    output = "plot"))
})

test_that("vessel_count: plot by 2 groups", {
  
  expect_snapshot_plot("vessel_count_plot_2grp",
                       vessel_count(PollockData, "pollock", "PERMIT", 
                                    group = c("GEAR_TYPE", "GF_TARGET_FT"),
                                    output = "plot"))
})

test_that("vessel_count: plot by facet", {
  
  expect_snapshot_plot("vessel_count_plot_fct",
                       vessel_count(PollockData, "pollock", "PERMIT", 
                                    facet_by = c("GEAR_TYPE"),
                                    output = "plot"))
})

test_that("vessel_count: plot by group + facet", {
  
  expect_snapshot_plot("vessel_count_plot_grp_fct",
                       vessel_count(PollockData, "pollock", "PERMIT", 
                                    facet_by = c("GEAR_TYPE"), group = "GF_TARGET_FT",
                                    output = "plot"))
})

test_that("vessel_count: plot month + grp", {
  
  expect_snapshot_plot("vessel_count_plot_mon_grp",
                       vessel_count(PollockData, "pollock", "PERMIT", date = "HAUL_DATE",
                                    period = "month", group = "GEAR_TYPE",
                                    output = "plot"))
})

test_that("vessel_count: plot month + grp + fct", {
  
  expect_snapshot_plot("vessel_count_plot_mon_grp_fct",
                       vessel_count(PollockData, "pollock", "PERMIT", date = "HAUL_DATE",
                                    period = "month", facet_by = "GEAR_TYPE",
                                    group = "GF_TARGET_FT", output = "plot"))
})