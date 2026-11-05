model_design_module_fixture <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  project <- "MixedLogitTest"
  formatted_dir <- file.path(root, project, "Models", "FormattedData")
  dir.create(formatted_dir, recursive = TRUE)
  df <- data.frame(
    chosen = c(1, 0, 0, 1),
    distance = c(1, 2, 3, 4),
    expected_catch = c(4, 3, 2, 1),
    vessel_id = c("a", "a", "b", "b"),
    obs_id = c(1, 1, 2, 2),
    zone_id = c(1, 2, 1, 2)
  )
  saveRDS(list(first = df, second = df[c("chosen", "expected_catch", "obs_id", "zone_id")]),
          file.path(formatted_dir, paste0(project, "LongFormatData.rds")))

  env <- new.env(parent = asNamespace("shiny"))
  module_path <- testthat::test_path("..", "..", "inst", "ShinyFiles", "MainApp",
                                    "modules", "model_design_module.R")
  if (!file.exists(module_path)) {
    module_path <- system.file("ShinyFiles", "MainApp", "modules", "model_design_module.R",
                               package = "FishSET")
  }
  sys.source(module_path, envir = env)
  env$locproject <- function() root
  env$locdatabase <- function(project) file.path(root, project, "fishset_db.sqlite")
  env$load_gui_variables <- function(...) {
    list(main = list(main_unique_obs_id = "obs_id", main_zone_id = "zone_id"))
  }
  env$calls <- list()
  env$fishset_design <- function(...) {
    args <- list(...)
    env$calls[[length(env$calls) + 1L]] <- args
    design_dir <- file.path(root, project, "Models", "ModelDesigns")
    dir.create(design_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(args, file.path(design_dir, paste0(args$model_name, ".rds")))
  }
  list(
    env = env, root = root, project = project, df = df,
    args = list(rv_folderpath = shiny::reactiveVal(root),
                rv_project_name = shiny::reactiveVal(list(value = project)),
                rv_data = shiny::reactiveValues(main = df))
  )
}

test_that("mixed-logit controls construct random slopes and named distributions", {
  fixture <- model_design_module_fixture()
  shiny::testServer(fixture$env$model_design_server, args = fixture$args, {
    session$setInputs(formatted_data_input = "first", model_name_input = "mixed",
                      model_type_input = "Standard Logit", scale_input = TRUE,
                      formula_input = "chosen ~ expected_catch + distance",
                      enable_random_input = TRUE)
    expect_identical(colnames(rv_current_formatted_data()), colnames(fixture$df))
    session$setInputs(group_id_input = "vessel_id", random_vars_input = "distance")
    expect_match(output$random_dists_ui$html, ns(random_dist_input_id("distance")),
                 fixed = TRUE)
    do.call(session$setInputs,
            setNames(list("lognormal"), random_dist_input_id("distance")))
    session$setInputs(random_vars_input = c("distance", "expected_catch"))
    expect_match(output$random_dists_ui$html, 'value="lognormal" selected', fixed = TRUE)
    do.call(session$setInputs,
            setNames(list("normal"), random_dist_input_id("expected_catch")))
    session$setInputs(run_design_btn = 1)

    expect_length(fixture$env$calls, 1)
    args <- fixture$env$calls[[1]]
    expect_identical(paste(deparse(args$formula, width.cutoff = 500L), collapse = " "),
                     paste("chosen ~ expected_catch + distance +",
                           "(0 + distance + expected_catch | vessel_id)"))
    expect_identical(args$random_dists, c(distance = "lognormal", expected_catch = "normal"))
    expect_true(args$scale)
    expect_identical(args$unique_obs_id, "obs_id")
    expect_identical(args$zone_id, "zone_id")
    expect_true("mixed" %in% rv_existing_design_names())
    saved <- readRDS(file.path(fixture$root, fixture$project, "Models", "ModelDesigns",
                              "mixed.rds"))
    expect_identical(saved$random_dists, args$random_dists)
  })
})

test_that("disabling mixed logit preserves standard and EPM arguments", {
  fixture <- model_design_module_fixture()
  shiny::testServer(fixture$env$model_design_server, args = fixture$args, {
    session$setInputs(formatted_data_input = "first", model_name_input = "standard",
                      model_type_input = "Standard Logit", scale_input = FALSE,
                      formula_input = "chosen ~ distance | expected_catch",
                      enable_random_input = FALSE, group_id_input = "vessel_id",
                      random_vars_input = "distance", run_design_btn = 1)
    expect_length(fixture$env$calls, 1)
    args <- fixture$env$calls[[1]]
    expect_identical(paste(deparse(args$formula), collapse = " "),
                     "chosen ~ distance | expected_catch")
    expect_false("random_dists" %in% names(args))

    session$setInputs(model_type_input = "epm", model_name_input = "epm",
                      catch_formula_input = "expected_catch ~ distance",
                      price_var_input = "expected_catch", run_design_btn = 2)
    expect_length(fixture$env$calls, 2)
    args <- fixture$env$calls[[2]]
    expect_identical(paste(deparse(args$catch_formula), collapse = " "),
                     "expected_catch ~ distance")
    expect_identical(args$price_var, "expected_catch")
    expect_false("random_dists" %in% names(args))
  })
})

test_that("mixed-logit validation prevents incomplete or stale specifications", {
  fixture <- model_design_module_fixture()
  shiny::testServer(fixture$env$model_design_server, args = fixture$args, {
    session$setInputs(formatted_data_input = "first", model_name_input = "invalid",
                      model_type_input = "Standard Logit", scale_input = FALSE,
                      formula_input = "chosen ~ distance", enable_random_input = TRUE,
                      run_design_btn = 1)
    expect_match(output$design_error_out, "grouping variable")
    session$setInputs(group_id_input = "vessel_id", run_design_btn = 2)
    expect_match(output$design_error_out, "at least one random parameter")
    session$setInputs(random_vars_input = "distance", run_design_btn = 3)
    expect_match(output$design_error_out, "select a distribution for distance")
    do.call(session$setInputs,
            setNames(list("weibull"), random_dist_input_id("distance")))
    session$setInputs(run_design_btn = 4)
    expect_match(output$design_error_out, "select a distribution for distance")
    session$setInputs(random_vars_input = "vessel_id", run_design_btn = 5)
    expect_match(output$design_error_out, "numeric covariates")

    session$setInputs(random_vars_input = "distance",
                      formula_input = "chosen ~ distance + (0 + distance | vessel_id)",
                      run_design_btn = 6)
    expect_match(output$design_error_out, "not both")
    session$setInputs(formula_input = "chosen ~ distance", formatted_data_input = "second")
    expect_false("distance" %in% colnames(rv_current_formatted_data()))
    expect_identical(as.character(output$random_dists_ui$html), "")
    session$setInputs(run_design_btn = 7)
    expect_match(output$design_error_out, "grouping variable")
    expect_length(fixture$env$calls, 0)

    session$setInputs(formatted_data_input = "")
    expect_null(rv_current_formatted_data())
    fixture$args$rv_project_name(list(value = "AnotherProject"))
    session$flushReact()
    expect_null(rv_current_formatted_data())
  })
})

test_that("mixed-logit slopes preserve the multipart fixed-effect formula", {
  fixture <- model_design_module_fixture()
  shiny::testServer(fixture$env$model_design_server, args = fixture$args, {
    session$setInputs(formatted_data_input = "first", model_name_input = "multipart",
                      model_type_input = "Standard Logit", scale_input = FALSE,
                      formula_input = "chosen ~ distance | expected_catch",
                      enable_random_input = TRUE)
    session$setInputs(group_id_input = "vessel_id", random_vars_input = "distance")
    do.call(session$setInputs, setNames(list("normal"), random_dist_input_id("distance")))
    session$setInputs(run_design_btn = 1)
    expect_length(fixture$env$calls, 1)
    form_str <- paste(deparse(fixture$env$calls[[1]]$formula), collapse = " ")
    expect_identical(form_str,
                     "chosen ~ distance | expected_catch + (0 + distance | vessel_id)")
  })
})

test_that("mixed-logit UI inputs are namespaced and disabled by default", {
  fixture <- model_design_module_fixture()
  fixture$env$spinner_ui <- function(...) shiny::div()
  html <- as.character(fixture$env$model_design_ui("design"))
  expect_match(html, 'id="design-enable_random_input"', fixed = TRUE)
  expect_match(html, 'id="design-group_id_input"', fixed = TRUE)
  expect_match(html, 'id="design-random_vars_input"', fixed = TRUE)
  expect_match(html, 'id="design-random_dists_ui"', fixed = TRUE)
  expect_match(html, 'data-ns-prefix="design-"', fixed = TRUE)
  expect_false(grepl('id="design-enable_random_input"[^>]*checked', html))
})

test_that("mixed-logit controls produce a persisted backend design", {
  fixture <- model_design_module_fixture()
  backend_path <- testthat::test_path("..", "..", "R", "fishset_design.R")
  if (file.exists(backend_path)) {
    sys.source(backend_path, envir = fixture$env)
  } else {
    fixture$env$fishset_design <- getFromNamespace("fishset_design", "FishSET")
    environment(fixture$env$fishset_design) <- fixture$env
  }
  fixture$env$model_design_list <- function(...) character(0)
  fixture$env$log_call <- function(...) invisible(NULL)

  shiny::testServer(fixture$env$model_design_server, args = fixture$args, {
    session$setInputs(formatted_data_input = "first", model_name_input = "backend_mixed",
                      model_type_input = "Standard Logit", scale_input = FALSE,
                      formula_input = "chosen ~ expected_catch + distance",
                      enable_random_input = TRUE)
    session$setInputs(group_id_input = "vessel_id",
                      random_vars_input = c("distance", "expected_catch"))
    do.call(session$setInputs,
            setNames(list("lognormal", "normal"),
                     vapply(c("distance", "expected_catch"), random_dist_input_id,
                            character(1))))
    session$setInputs(run_design_btn = 1)
    if (!("backend_mixed" %in% rv_existing_design_names())) {
      stop(output$design_error_out)
    }
    expect_true("backend_mixed" %in% rv_existing_design_names())
    design_path <- file.path(fixture$root, fixture$project, "Models", "ModelDesigns",
                             "backend_mixed")
    design <- if (file.exists(paste0(design_path, ".qs2"))) {
      qs2::qs_read(paste0(design_path, ".qs2"))
    } else {
      readRDS(paste0(design_path, ".rds"))
    }
    expect_s3_class(design, "fishset_design")
    expect_identical(design$random_effects$group_id, "vessel_id")
    expect_identical(design$random_effects$dist_codes,
                     c(distance = 2, expected_catch = 1))
    expect_identical(colnames(design$random_effects$X_random),
                     c("distance", "expected_catch"))
    expect_equal(unname(design$random_effects$X_random),
                 as.matrix(fixture$df[c("distance", "expected_catch")]),
                 ignore_attr = TRUE)
    expect_identical(design$ids$group, fixture$df$vessel_id)
  })
})
