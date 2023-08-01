## ----eval=FALSE---------------------------------------------------------------
#  install.packages('devtools')
#  library(devtools)
#  install.packages("PATH/TO/Directory/Containing/FishSET", repos=NULL, type='source')
#  library(FishSET)

## ----eval=FALSE---------------------------------------------------------------
#  #Upload data
#  load_maindata(dat, over_write = TRUE, project = 'EXAMP')
#  load_port(dat, port_name = port, over_write = TRUE, project = 'EXAMP')
#  spatdat <- read_dat('')

## ----eval=FALSE---------------------------------------------------------------
#  tables_database()
#  head(table_view('projectMainDataTable'))

## ----eval=FALSE---------------------------------------------------------------
#  #Check whether a table exists in the database.
#    table_exists()
#  #Remove a table from the database.
#   table_remove()
#  table_fields()      View column names of data table.

## ----eval=FALSE---------------------------------------------------------------
#    data_verification()
#    # The following functions are contained in `data_verification` but can be run on their own too.
#      outlier_check()
#      nan_identify()

## ----eval=FALSE---------------------------------------------------------------
#  #Table output functions
#    summary_stats()
#    outlier_table()
#  
#  #Plots output functions
#    outlier_plot()

## ----eval=FALSE---------------------------------------------------------------
#    na_filter()
#    nan_filter()
#  
#    outlier_remove()
#  
#    unique_filter()
#  
#    empty_vars_filter()
#  
#    degree()

## ----eval=FALSE---------------------------------------------------------------
#  #Filter
#    filter_table()
#    filter_dat()
#  
#  #Add variables back into the working data set
#    add_vars()

## ----eval=FALSE---------------------------------------------------------------
#  
#  #Map
#    map_plot()
#  #map kernel
#    map_kernel()
#  #Getis Ord
#    getis_ord_stats()
#  #Moran's I
#    moran_stats()
#  #Temporal Plots
#    temp_plot()
#  #x-y plots
#    xy_plot(dat, project, var1, var2, regress = FALSE)
#    density_plot()
#  

## ----eval=FALSE---------------------------------------------------------------
#  #Define and store fleet expressions
#    fleet_assign(dat, project, cond = NULL, fleet_val = NULL, table = NULL, save = TRUE)
#  #Number of unique vessels by time period
#    vessel_count(dat, project, v, t, period = "month", group = NULL,
#                           year = NULL, position = "stack", output = c("table", "plot"))
#  #Total species catch by period
#    species_catch(dat, project, species, date, period = "month_abv", fun = "sum", group = NULL, year = NULL,
#                            convert_to_tons = TRUE, value = c("count", "percent"),
#                            output = c("table", "plot"), position = "stack", format_tab = c("wide", "long"))
#  #Compare bycatch to other species caught
#    bycatch(dat, project, cpue, catch = NULL , date, names = NULL, group = NULL,
#                      year = NULL, period = "year", value = c("count", "stc"),
#                      output = c("table", "plot"), format_tab = "wide")
#  #Aggregate species catch by unique vessel ID
#    sum_catch("MainDataTable", "myProject", "catch", "species == 'cod' & catch > .5", val = "per", out = "logical")
#  #Catch total by week
#    weekly_catch(dat, project, species, date, year = NULL, group = NULL, fun = "sum", position = "stack",
#                           convert_to_tons = FALSE, value ="percent", output = c("plot", "table"))
#  #Average CPUE by week
#    weekly_effort(dat, project, cpue, date, group = NULL, year = NULL, plot_type = "line_point",
#                            output = c("plot", "table"), format_tab = "wide")
#  #Create a plot or table of vessel trip length
#    trip_length(dat, project, start, end, units = "days", catch = NULL,
#                          hauls = NULL, output = c("table", "plot"), haul_to_trip = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  #correlation
#    corr_out(dat, project, variables='all')
#  #regression
#    xy_plot(dat, project, var1, var2, regress = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  #Data Transformations
#    ##Change time unit
#    temporal_mod
#    ##Coded variable based on quantiles
#    set_quants()
#  #Nominal ID
#    #Haul or Trip ID
#    ID_var()
#    #Fishery Seasonal Identifier
#    create_seasonal_ID()
#  #Arithmetic and temporal
#    #Numeric functions
#    create_var_num()
#    #CPUE
#    cpue()
#  #Dummy
#    #From variable
#    dummy_num()
#    dummy_matrix()
#  #spatial
#    #Distance between two points
#    create_dist_between()
#    #Haul midpoint
#    create_mid_haul()
#    #Duration of time
#    create_duration()
#    #Zone when choice where to go next was made
#    create_startingloc()
#  #Trip-level Functions
#    #Collapse Haul to trip
#    haul_to_trip()
#    #Trip distance
#    create_trip_distance()
#    #Trip centroid
#    create_trip_centroid()

## ----eval=FALSE---------------------------------------------------------------
#    find_centroid()
#    assignment_column()
#    create_alternative_choice()
#  

## ----eval=FALSE---------------------------------------------------------------
#    sparsetable()
#    create_expectations()

## ----eval=FALSE---------------------------------------------------------------
#  make_model_design()
#  discretefish_subroutine()

## ----eval=FALSE---------------------------------------------------------------
#  #Display errorExplain output
#  globalcheck_view('pcodldglobalcheck20190604')
#  #Display all other model output.
#  model_out_view('pcod')
#  

## ----eval=FALSE---------------------------------------------------------------
#  model_fit('pollock')
#  select_model()

## ----eval=FALSE---------------------------------------------------------------
#  run_fishset_gui()
#  select_vars()
#  select_model()
#  add_vars()

## ----eval=FALSE---------------------------------------------------------------
#  filter_table(dat = 'pcodmaindatatable', project = 'pcod', x = 'PERFORMANCE_Code', exp = 'PERFORMANCE_Code==1')

## ----eval=FALSE---------------------------------------------------------------
#  
#  
#   {
#            "functionID": "filter_table",
#            "args": [
#              "pcodMainDataTable",
#              "pcod",
#              "PERFORMANCE_Code",
#              "PERFORMANCE_Code==1"
#            ],
#            "kwargs": [],
#            "output": "",
#            "msg": [
#              {
#                "dataframe": "pcodMainDataTable",
#                "vector": "PERFORMANCE_Code",
#                "FilterFunction": "PERFORMANCE_Code==1"
#              }
#            ]
#          }

## ----eval=FALSE---------------------------------------------------------------
#  `log_reset`
#  `log_func_model`

