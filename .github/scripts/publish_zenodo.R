#!/usr/bin/env Rscript

options(warn = 1)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && (is.na(x) || identical(x, "")))) y else x
}

read_required_env <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    stop(sprintf("Required environment variable '%s' is missing.", name), call. = FALSE)
  }
  value
}

read_optional_env <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else NULL
}

trim_period <- function(x) sub("[[:space:]]*\\.$", "", x)

normalize_description <- function(text) {
  text <- gsub("\r|\n", " ", text)
  text <- gsub("[[:space:]]+", " ", text)
  trimws(text)
}

coerce_to_https <- function(url) {
  if (is.null(url) || !nzchar(url)) return(NULL)
  if (!grepl("^https?://", url)) return(NULL)
  sub("^http://", "https://", url)
}

read_release_payload <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(list())
  }

  payload <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  release <- payload$release %||% list()
  repo <- payload$repository %||% list()

  list(
    release = release,
    repository = repo,
    release_name = release$name %||% NULL,
    release_body = release$body %||% NULL,
    release_tag = release$tag_name %||% NULL,
    upload_url = release$zipball_url %||% NULL,
    html_url = release$html_url %||% NULL,
    repo_url = repo$html_url %||% NULL
  )
}

parse_authors <- function(description_path) {
  authors <- desc::desc_get_authors(file = description_path)
  if (length(authors) == 0) {
    stop("No authors found in DESCRIPTION Authors@R field.", call. = FALSE)
  }

  creator_roles <- c("aut", "cre")
  creators <- lapply(authors, function(author) {
    roles <- author$role %||% character()
    if (!any(roles %in% creator_roles)) {
      return(NULL)
    }

    given <- trimws(paste(author$given %||% "", collapse = " "))
    family <- trimws(paste(author$family %||% "", collapse = " "))
    name <- trimws(paste(given, family))
    if (!nzchar(name)) {
      name <- trimws(author$name %||% "")
    }
    if (!nzchar(name)) {
      return(NULL)
    }

    comment <- author$comment
    comment_named <- if (is.list(comment) && !is.null(names(comment))) comment else list()
    affiliation <- comment_named$affiliation %||% NULL
    orcid <- comment_named$ORCID %||% comment_named$orcid %||% NULL
    if (!is.null(orcid) && nzchar(orcid) && !grepl("^https?://", orcid)) {
      orcid <- sprintf("https://orcid.org/%s", orcid)
    }

    creator <- list(name = name)
    if (!is.null(affiliation) && nzchar(affiliation)) {
      creator$affiliation <- affiliation
    }
    if (!is.null(orcid) && nzchar(orcid)) {
      creator$orcid <- orcid
    }
    creator
  })

  creators <- Filter(Negate(is.null), creators)
  if (length(creators) == 0) {
    stop("No DESCRIPTION authors with aut/cre roles were found.", call. = FALSE)
  }
  creators
}

build_metadata <- function(description_path, release_info) {
  description <- desc::desc_get("Description", file = description_path)
  version <- desc::desc_get_version(file = description_path)
  license <- desc::desc_get("License", file = description_path)
  package_name <- desc::desc_get("Package", file = description_path)
  url <- coerce_to_https(desc::desc_get("URL", file = description_path))

  release_title <- release_info$release_name %||% sprintf("%s %s", package_name, version)
  release_description <- release_info$release_body %||% description

  metadata <- list(
    metadata = list(
      title = release_title,
      version = version,
      description = normalize_description(release_description),
      upload_type = "software",
      creators = parse_authors(description_path),
      license = trim_period(license),
      keywords = c(package_name, "R", "fisheries"),
      notes = normalize_description(description),
      language = "eng"
    )
  )

  publication_date <- Sys.Date()
  if (!is.null(release_info$release$published_at)) {
    publication_date <- as.Date(substr(release_info$release$published_at, 1, 10))
  }
  metadata$metadata$publication_date <- as.character(publication_date)

  related_identifiers <- list()
  for (candidate in list(
    list(url = release_info$html_url, relation = "isSupplementTo", scheme = "url"),
    list(url = release_info$repo_url %||% url, relation = "isSupplementTo", scheme = "url")
  )) {
    if (!is.null(candidate$url) && nzchar(candidate$url)) {
      related_identifiers[[length(related_identifiers) + 1]] <- list(
        identifier = candidate$url,
        relation = candidate$relation,
        resource_type = "software",
        scheme = candidate$scheme
      )
    }
  }
  if (length(related_identifiers) > 0) {
    metadata$metadata$related_identifiers <- related_identifiers
  }

  if (!is.null(url) && nzchar(url)) {
    metadata$metadata$access_right <- "open"
  }

  metadata
}

zenodo_headers <- function(token) {
  httr2::req_headers(Authorization = sprintf("******", token))
}

zenodo_request <- function(url, token) {
  request <- httr2::request(url)
  request <- zenodo_headers(token)(request)
  httr2::req_user_agent(request, "FishSET Zenodo Publisher/1.0")
}

perform_request <- function(request, step) {
  tryCatch(
    httr2::req_perform(request),
    error = function(err) {
      message(sprintf("Zenodo API request failed during '%s': %s", step, conditionMessage(err)))
      stop(err)
    }
  )
}

parse_json_response <- function(response, step) {
  tryCatch(
    httr2::resp_body_json(response, simplifyVector = FALSE),
    error = function(err) {
      body <- tryCatch(httr2::resp_body_string(response), error = function(...) "<unable to read body>")
      message(sprintf("Failed to parse JSON response during '%s'. Body: %s", step, body))
      stop(err)
    }
  )
}

find_latest_concept_version <- function(base_url, token, concept_recid) {
  request <- zenodo_request(base_url, token)
  request <- httr2::req_url_query(
    request,
    q = sprintf('conceptrecid:%s', concept_recid),
    sort = "mostrecent"
  )
  response <- perform_request(request, "search existing concept DOI")
  body <- parse_json_response(response, "search existing concept DOI")
  hits <- body$hits$hits %||% list()
  if (length(hits) == 0) {
    stop(sprintf("No Zenodo deposition found for concept record id '%s'.", concept_recid), call. = FALSE)
  }
  hits[[1]]
}

create_deposition <- function(base_url, token) {
  request <- zenodo_request(base_url, token)
  request <- httr2::req_method(request, "POST")
  request <- httr2::req_body_json(request, list(), auto_unbox = TRUE)
  response <- perform_request(request, "create deposition")
  parse_json_response(response, "create deposition")
}

create_new_version <- function(record, token) {
  links <- record$links %||% list()
  new_version_url <- links$newversion %||% NULL
  if (is.null(new_version_url)) {
    stop("Zenodo record does not provide a newversion link.", call. = FALSE)
  }

  request <- zenodo_request(new_version_url, token)
  request <- httr2::req_method(request, "POST")
  response <- perform_request(request, "create new version")
  body <- parse_json_response(response, "create new version")

  latest_draft_url <- body$links$latest_draft %||% NULL
  if (is.null(latest_draft_url)) {
    stop("Zenodo new version response did not include a latest draft link.", call. = FALSE)
  }

  draft_request <- zenodo_request(latest_draft_url, token)
  draft_response <- perform_request(draft_request, "fetch latest draft")
  parse_json_response(draft_response, "fetch latest draft")
}

upload_archive <- function(deposition, archive_path, token) {
  bucket_url <- deposition$links$bucket %||% NULL
  if (is.null(bucket_url)) {
    stop("Zenodo deposition does not include a bucket upload URL.", call. = FALSE)
  }

  upload_url <- sprintf("%s/%s", sub("/$", "", bucket_url), basename(archive_path))
  request <- zenodo_request(upload_url, token)
  request <- httr2::req_method(request, "PUT")
  request <- httr2::req_body_file(request, path = archive_path)
  response <- perform_request(request, "upload archive")
  parse_json_response(response, "upload archive")
}

update_metadata <- function(deposition, metadata, token) {
  latest_url <- deposition$links$latest_draft %||% deposition$links$self %||% NULL
  if (is.null(latest_url)) {
    stop("Zenodo deposition does not provide a metadata update URL.", call. = FALSE)
  }

  request <- zenodo_request(latest_url, token)
  request <- httr2::req_method(request, "PUT")
  request <- httr2::req_body_json(request, metadata, auto_unbox = TRUE)
  response <- perform_request(request, "update metadata")
  parse_json_response(response, "update metadata")
}

publish_deposition <- function(deposition, token) {
  publish_url <- deposition$links$publish %||% NULL
  if (is.null(publish_url)) {
    stop("Zenodo deposition does not provide a publish URL.", call. = FALSE)
  }

  request <- zenodo_request(publish_url, token)
  request <- httr2::req_method(request, "POST")
  response <- perform_request(request, "publish deposition")
  parse_json_response(response, "publish deposition")
}

main <- function() {
  description_path <- normalizePath("DESCRIPTION", mustWork = TRUE)
  token <- read_required_env("ZENODO_SANDBOX_TOKEN")
  archive_path <- normalizePath(read_required_env("ZENODO_ARCHIVE_PATH"), mustWork = TRUE)
  base_url <- read_optional_env("ZENODO_API_BASE_URL") %||% "https://sandbox.zenodo.org/api/deposit/depositions"
  concept_recid <- read_optional_env("ZENODO_CONCEPT_REC_ID")
  github_event_path <- read_optional_env("GITHUB_EVENT_PATH")

  release_info <- read_release_payload(github_event_path)
  metadata <- build_metadata(description_path, release_info)

  message(sprintf("Preparing Zenodo publication for FishSET version %s using archive %s", metadata$metadata$version, basename(archive_path)))

  deposition <- if (is.null(concept_recid)) {
    message("No ZENODO_CONCEPT_REC_ID provided; creating initial Zenodo deposition.")
    create_deposition(base_url, token)
  } else {
    message(sprintf("Creating new version under existing Zenodo concept record %s.", concept_recid))
    latest_record <- find_latest_concept_version(base_url, token, concept_recid)
    create_new_version(latest_record, token)
  }

  upload_response <- upload_archive(deposition, archive_path, token)
  message(sprintf("Uploaded archive to Zenodo bucket as %s", upload_response$filename %||% basename(archive_path)))

  updated_deposition <- update_metadata(deposition, metadata, token)
  published_record <- publish_deposition(updated_deposition, token)

  concept_doi <- published_record$conceptdoi %||% published_record$metadata$prereserve_doi$doi %||% NA_character_
  version_doi <- published_record$doi %||% published_record$metadata$doi %||% NA_character_
  concept_recid_out <- published_record$conceptrecid %||% concept_recid %||% NA_character_

  message(sprintf("Published Zenodo record successfully. Version DOI: %s", version_doi))
  if (!is.na(concept_doi)) {
    message(sprintf("Concept DOI: %s", concept_doi))
  }
  if (!is.na(concept_recid_out)) {
    message(sprintf("Concept record ID: %s", concept_recid_out))
  }
}

if (!isTRUE(getOption("fishset.zenodo.skip_main", FALSE))) {
  main()
}
