test_that("publish_zenodo metadata derives from DESCRIPTION and manual fallback", {
  skip_if_not_installed("desc")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("withr")

  withr::local_envvar(
    GITHUB_EVENT_PATH = NA_character_,
    ZENODO_SANDBOX_TOKEN = "token",
    ZENODO_ARCHIVE_PATH = tempfile(fileext = ".zip")
  )
  writeBin(charToRaw("archive"), Sys.getenv("ZENODO_ARCHIVE_PATH"))

  script_path <- normalizePath(file.path("..", "..", ".github", "scripts", "publish_zenodo.R"), mustWork = TRUE)
  description_path <- normalizePath(file.path("..", "..", "DESCRIPTION"), mustWork = TRUE)

  withr::local_options(list(fishset.zenodo.skip_main = TRUE))
  script_env <- new.env(parent = baseenv())
  sys.source(script_path, envir = script_env)

  release_info <- script_env$read_release_payload(Sys.getenv("GITHUB_EVENT_PATH", unset = ""))
  metadata <- script_env$build_metadata(description_path, release_info)

  expect_equal(metadata$metadata$upload_type, "software")
  expect_equal(metadata$metadata$version, desc::desc_get_version(file = description_path))
  expect_equal(metadata$metadata$title, sprintf("%s %s", desc::desc_get("Package", file = description_path), desc::desc_get_version(file = description_path)))
  expect_true(length(metadata$metadata$creators) >= 1)
  expect_equal(metadata$metadata$license, sub("[[:space:]]*\\.$", "", desc::desc_get("License", file = description_path)))
})

test_that("publish_zenodo release payload uses release metadata when available", {
  skip_if_not_installed("desc")
  skip_if_not_installed("jsonlite")

  payload_path <- tempfile(fileext = ".json")
  jsonlite::write_json(list(
    release = list(
      name = "FishSET v9.9.9",
      body = "Published release body",
      tag_name = "v9.9.9",
      html_url = "https://github.com/noaa-nwfsc/FishSET/releases/tag/v9.9.9",
      published_at = "2026-08-04T00:00:00Z"
    ),
    repository = list(
      html_url = "https://github.com/noaa-nwfsc/FishSET"
    )
  ), payload_path, auto_unbox = TRUE)

  script_path <- normalizePath(file.path("..", "..", ".github", "scripts", "publish_zenodo.R"), mustWork = TRUE)
  description_path <- normalizePath(file.path("..", "..", "DESCRIPTION"), mustWork = TRUE)

  withr::local_options(list(fishset.zenodo.skip_main = TRUE))
  script_env <- new.env(parent = baseenv())
  sys.source(script_path, envir = script_env)

  release_info <- script_env$read_release_payload(payload_path)
  metadata <- script_env$build_metadata(description_path, release_info)

  expect_equal(metadata$metadata$title, "FishSET v9.9.9")
  expect_equal(metadata$metadata$description, "Published release body")
  expect_equal(metadata$metadata$publication_date, "2026-08-04")
  expect_true(any(vapply(metadata$metadata$related_identifiers, `[[`, character(1), "identifier") == "https://github.com/noaa-nwfsc/FishSET/releases/tag/v9.9.9"))
})
