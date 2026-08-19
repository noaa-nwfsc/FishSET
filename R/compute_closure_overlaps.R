#' Identify model zones overlapped by an uploaded shapefile
#'
#' @param uploaded_files The data frame returned by a Shiny shapefile `fileInput`.
#' @param zones An `sf` object with a `second_location_id` column.
#' @param overlap_threshold Minimum percentage of a zone that must be covered.
#' @return A character vector of selected `second_location_id` values.
#' @keywords internal
compute_closure_overlaps <- function(uploaded_files, zones, overlap_threshold) {
  if (!is.data.frame(uploaded_files) ||
      !all(c("name", "datapath") %in% names(uploaded_files))) {
    stop("Upload all required shapefile components.", call. = FALSE)
  }
  if (!is.numeric(overlap_threshold) || length(overlap_threshold) != 1 ||
      is.na(overlap_threshold) || overlap_threshold < 0 || overlap_threshold > 100) {
    stop("Overlap threshold must be between 0 and 100.", call. = FALSE)
  }
  if (!inherits(zones, "sf") || !"second_location_id" %in% names(zones)) {
    stop("Zones must be an sf object with second_location_id values.", call. = FALSE)
  }

  shp_index <- which(tolower(tools::file_ext(uploaded_files$name)) == "shp")
  if (length(shp_index) != 1) {
    stop("Upload exactly one .shp file and its companion files.", call. = FALSE)
  }

  upload_dir <- tempfile("closure_shapefile_")
  dir.create(upload_dir)
  on.exit(unlink(upload_dir, recursive = TRUE), add = TRUE)

  file.copy(uploaded_files$datapath,
            file.path(upload_dir, uploaded_files$name),
            overwrite = TRUE)
  uploaded_shape <- sf::st_read(
    file.path(upload_dir, uploaded_files$name[[shp_index]]),
    quiet = TRUE
  )
  if (is.na(sf::st_crs(uploaded_shape))) {
    stop("The uploaded shapefile must define a coordinate reference system.", call. = FALSE)
  }

  zones <- sf::st_make_valid(zones)
  uploaded_shape <- sf::st_make_valid(sf::st_transform(uploaded_shape, sf::st_crs(zones)))
  if (any(sf::st_geometry_type(zones) %in% c("POINT", "MULTIPOINT"))) {
    overlaps <- lengths(sf::st_intersects(zones, uploaded_shape)) > 0
    return(as.character(zones$second_location_id[overlaps]))
  }

  zone_area <- as.numeric(sf::st_area(zones))
  intersections <- sf::st_intersection(
    zones[, "second_location_id", drop = FALSE],
    sf::st_union(uploaded_shape)
  )
  if (nrow(intersections) == 0) {
    return(character(0))
  }

  overlap_area <- stats::aggregate(
    as.numeric(sf::st_area(intersections)),
    by = list(second_location_id = intersections$second_location_id),
    FUN = sum
  )
  names(overlap_area)[2] <- "area"
  zone_overlap <- data.frame(
    second_location_id = as.character(zones$second_location_id),
    area = zone_area,
    stringsAsFactors = FALSE
  )
  zone_overlap <- merge(zone_overlap, overlap_area,
                        by = "second_location_id", all.x = TRUE,
                        suffixes = c("_zone", "_overlap"))
  zone_overlap$area_overlap[is.na(zone_overlap$area_overlap)] <- 0

  zone_overlap$second_location_id[
    zone_overlap$area_zone > 0 &
      (100 * zone_overlap$area_overlap / zone_overlap$area_zone) >= overlap_threshold
  ]
}
