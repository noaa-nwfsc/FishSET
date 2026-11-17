test_that("mixed-logit controls only accept supported column identifiers", {
  module_env <- new.env(parent = asNamespace("shiny"))
  module_path <- test_path("..", "..", "inst", "ShinyFiles", "MainApp",
                           "modules", "model_design_module.R")
  if (!file.exists(module_path)) {
    module_path <- system.file("ShinyFiles", "MainApp", "modules",
                               "model_design_module.R", package = "FishSET")
  }
  sys.source(module_path, envir = module_env)
  choices_env <- new.env()
  module_env$updateSelectizeInput <- function(session, inputId, choices, ...) {
    assign(inputId, choices, envir = choices_env)
  }
  module_env$load_gui_variables <- function(...) {
    list(main = list(main_unique_obs_id = "haul_id", main_zone_id = "zone_id"))
  }
  designs <- list()
  module_env$fishset_design <- function(...) {
    designs[[length(designs) + 1L]] <<- list(...)
  }
  project <- shiny::reactiveVal(NULL)
  folder <- shiny::reactiveVal(tempdir())
  data <- shiny::reactiveValues(main = NULL)

  shiny::testServer(module_env$model_design_server,
                    args = list(rv_folderpath = folder, rv_project_name = project,
                                rv_data = data), {
    project(list(value = "TestProject"))
    session$setInputs(formatted_data_input = "test")
    df <- data.frame(
      "fuel cost" = 1:3, "vessel id" = letters[1:3],
      "catch-rate" = 2:4, "catch.rate" = 3:5,
      vessel_id = letters[1:3], fuel_cost = 4:6, "if" = 5:7,
      "." = 6:8, "..." = 7:9, "..1" = 8:10, check.names = FALSE
    )
    rv_current_formatted_data(df)
    session$flushReact()
    expect_identical(choices_env$group_id_input,
                     c("catch.rate", "vessel_id", "fuel_cost"))
    expect_identical(choices_env$random_vars_input, c("catch.rate", "fuel_cost"))

    session$setInputs(model_type_input = "mixed_logit",
                      random_vars_input = c("catch-rate", "catch.rate", "fuel_cost"))
    html <- output$random_dists_ui$html
    expect_false(grepl("catch-rate", html, fixed = TRUE))
    expect_true(grepl(paste0('id="', session$ns("dist_catch.rate"), '"'),
                      html, fixed = TRUE))
    expect_true(grepl(paste0('id="', session$ns("dist_fuel_cost"), '"'),
                      html, fixed = TRUE))

    session$setInputs(model_name_input = "test",
                      formula_input = "chosen ~ catch.rate + fuel_cost",
                      group_id_input = "vessel id", random_vars_input = "catch.rate",
                      run_design_btn = 1)
    expect_match(output$design_error_out, "Please select a grouping variable", fixed = TRUE)
    expect_length(designs, 0)

    session$setInputs(group_id_input = "vessel_id",
                      random_vars_input = "catch-rate", run_design_btn = 2)
    expect_match(output$design_error_out, "Please select at least one random parameter",
                 fixed = TRUE)
    expect_length(designs, 0)

    session$setInputs(random_vars_input = c("catch.rate", "fuel_cost"),
                      dist_catch.rate = "lognormal", dist_fuel_cost = "normal",
                      run_design_btn = 3)
    expect_length(designs, 1)
    expect_identical(designs[[1]]$random_dists,
                     c(catch.rate = "lognormal", fuel_cost = "normal"))
    expect_identical(all.vars(designs[[1]]$formula),
                     c("chosen", "catch.rate", "fuel_cost", "vessel_id"))
  })
})
