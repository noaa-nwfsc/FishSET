#' Identify and Filter Model Zones by Spatial Overlap
#'
#' @description
#' Processes user-uploaded spatial files from a Shiny application, standardizes and 
#' cleans the geometries, and calculates their intersection against a predefined set of 
#' reference zones. It returns the IDs of the zones that meet a specific overlap threshold.
#'
#' @details
#' The function supports multiple uploaded file formats:
#' * **Shapefiles**: Requires the `.shp` file and its companion files to be uploaded together.
#' * **Single files**: GeoJSON, JSON, GeoPackage (.gpkg), and RDS (containing an `sf` object).
#' * **CSV**: Must contain either a WKT/geometry column, or explicit longitude (`lon`/`longitude`) 
#'   and latitude (`lat`/`latitude`) columns (assumed EPSG:4326).
#' 
#' **Geometry Processing:**
#' Z and M dimensions are dropped. Geometries are made valid using `sf::st_make_valid()`, and 
#' polygons are buffered by 0 to fix self-intersections. The uploaded shape is then transformed 
#' to match the CRS of the `zones` object.
#' 
#' **Overlap Logic:**
#' * If `zones` contains point geometries, the `overlap_threshold` is ignored, and any zone 
#'   intersecting the uploaded shape is returned.
#' * If `zones` contains polygonal geometries, the function calculates the exact area of 
#'    intersection. 
#'   A zone is only returned if the percentage of its area covered by the uploaded shape is 
#'   greater than or equal to `overlap_threshold`.
#'
#' @param uploaded_files A data frame returned by a Shiny `fileInput` control. Must contain 
#'   at least the columns `name` and `datapath`.
#' @param zones An `sf` object representing the reference zones. Must contain a column 
#'   named `second_location_id`.
#' @param overlap_threshold A numeric value between 0 and 100 representing the minimum 
#'   percentage of a zone's area that must be covered by the uploaded shape to be selected.
#' @param return_shape A logical value. If `FALSE` (default), the function returns a character 
#'   vector of selected zone IDs. If `TRUE`, it returns a list containing both the selected IDs 
#'   and the cleaned uploaded `sf` object.
#'
#' @return Depending on the `return_shape` argument:
#' * If `return_shape = FALSE`: A character vector of selected `second_location_id` values.
#' * If `return_shape = TRUE`: A list with two elements:
#'     * `ids`: A character vector of selected `second_location_id` values.
#'     * `shape`: The cleaned, validated, and transformed uploaded `sf` object.
#'     
#' @importFrom sf sf_use_s2 st_read st_as_sf st_zm st_crs st_make_valid st_geometry_type st_buffer 
#' st_transform st_intersects st_area st_intersection st_union
#' @importFrom tools file_ext
#' @importFrom stats aggregate
#' @importFrom utils read.csv
#' @keywords internal

compute_closure_overlaps <- function(uploaded_files, zones, overlap_threshold,
                                     return_shape = FALSE) {
  # Temporarily disable S2 geometry to avoid planar/spherical calculation errors
  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)

  # Input Validation ------------------------------------------------------------------------------
  if (!is.data.frame(uploaded_files) ||
      !all(c("name", "datapath") %in% names(uploaded_files))) {
    stop("Upload a valid spatial file.", call. = FALSE)
  }
  if (!is.numeric(overlap_threshold) || length(overlap_threshold) != 1 ||
      is.na(overlap_threshold) || overlap_threshold < 0 || overlap_threshold > 100) {
    stop("Overlap threshold must be between 0 and 100.", call. = FALSE)
  }
  if (!inherits(zones, "sf") || !"second_location_id" %in% names(zones)) {
    stop("Zones must be an sf object with second_location_id values.", call. = FALSE)
  }

  # File Reading & Parsing ------------------------------------------------------------------------
  shp_index <- which(tolower(tools::file_ext(uploaded_files$name)) == "shp")
  
  if (length(shp_index) > 0) {
    # Handle Shapefile uploads (requires multiple companion files)
    if (length(shp_index) != 1) {
      stop("Upload exactly one .shp file and its companion files.", call. = FALSE)
    }

    upload_dir <- tempfile("closure_shapefile_")
    dir.create(upload_dir)
    on.exit(unlink(upload_dir, recursive = TRUE), add = TRUE)

    # Copy files from temp directory to a structured temp directory
    file.copy(uploaded_files$datapath,
              file.path(upload_dir, uploaded_files$name),
              overwrite = TRUE)
              
    uploaded_shape <- sf::st_read(
      file.path(upload_dir, uploaded_files$name[[shp_index]]),
      quiet = TRUE
    )
    
  } else {
    # Handle single-file uploads (GeoJSON, GPKG, RDS, CSV)
    rds_index <- which(tolower(tools::file_ext(uploaded_files$name)) == "rds")
    single_file_index <- which(
      tolower(tools::file_ext(uploaded_files$name)) %in%
        c("geojson", "json", "gpkg", "rds", "csv")
    )
    
    if (length(single_file_index) != 1) {
      stop(
        paste(
          "Upload one GeoJSON, JSON, GeoPackage, RDS, or CSV file,",
          "or a shapefile with its companion files."
        ),
        call. = FALSE
      )
    }

    file_extension <- tolower(tools::file_ext(uploaded_files$name[[single_file_index]]))

    if (identical(file_extension, "rds")) {
      # Handle RDS containing sf objects
      uploaded_shape <- readRDS(uploaded_files$datapath[rds_index])
      if (!inherits(uploaded_shape, "sf")) {
        stop("The uploaded .rds file must contain an 'sf' spatial object.", call. = FALSE)
      }
      
    } else if (identical(file_extension, "csv")) {
      # Handle CSV containing WKT or Lat/Lon columns
      uploaded_data <- utils::read.csv(
        uploaded_files$datapath[[single_file_index]],
        stringsAsFactors = FALSE
      )
      column_names <- tolower(names(uploaded_data))
      
      # Look for geometry/wkt columns first
      wkt_index <- match("geometry", column_names, nomatch = 0)
      if (wkt_index == 0) {
        wkt_index <- match("wkt", column_names, nomatch = 0)
      }

      if (wkt_index > 0) {
        uploaded_shape <- sf::st_as_sf(
          uploaded_data,
          wkt = names(uploaded_data)[[wkt_index]],
          crs = 4326
        )
      } else {
        # Fall back to lat/lon coordinates
        lon_index <- match(c("lon", "longitude"), column_names, nomatch = 0)
        lat_index <- match(c("lat", "latitude"), column_names, nomatch = 0)
        lon_index <- lon_index[lon_index > 0][1]
        lat_index <- lat_index[lat_index > 0][1]

        if (is.na(lon_index) || is.na(lat_index)) {
          stop(
            "CSV files must contain a geometry/WKT column or longitude and latitude columns.",
            call. = FALSE
          )
        }

        uploaded_shape <- sf::st_as_sf(
          uploaded_data,
          coords = c(names(uploaded_data)[[lon_index]], names(uploaded_data)[[lat_index]]),
          crs = 4326
        )
      }
    } else {
      # Handle standard formats readable by st_read (GeoJSON, GPKG)
      uploaded_shape <- sf::st_read(uploaded_files$datapath[[single_file_index]], quiet = TRUE)
    }
  }
  
  # Geometry Standardization & Cleaning -----------------------------------------------------------
  # Drop Z and M dimensions to ensure 2D geometry operations
  uploaded_shape <- sf::st_zm(uploaded_shape, drop = TRUE, what = "ZM")
  zones <- sf::st_zm(zones, drop = TRUE, what = "ZM")

  if (is.na(sf::st_crs(uploaded_shape))) {
    stop("The uploaded spatial file must define a coordinate reference system.", call. = FALSE)
  }

  # Helper function to fix invalid geometries and self-intersections
  clean_geometry <- function(x) {
    x <- suppressWarnings(suppressMessages(sf::st_make_valid(x)))
    polygonal <- sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON")
    if (any(polygonal)) {
      x[polygonal, ] <- suppressWarnings(suppressMessages(sf::st_buffer(x[polygonal, ], dist = 0)))
    }
    x
  }

  uploaded_shape <- clean_geometry(uploaded_shape)
  zones <- clean_geometry(zones)
  
  # Ensure the uploaded shape matches the CRS of the reference zones
  uploaded_shape <- clean_geometry(sf::st_transform(uploaded_shape, sf::st_crs(zones)))
  
  # Overlap Calculations --------------------------------------------------------------------------
  # Reference zones are points (simple intersection)
  if (any(sf::st_geometry_type(zones) %in% c("POINT", "MULTIPOINT"))) {
    overlaps <- suppressWarnings(suppressMessages(
      lengths(sf::st_intersects(zones, uploaded_shape)) > 0))
    selected_ids <- as.character(zones$second_location_id[overlaps])
    
    return(if (return_shape) list(ids = selected_ids, shape = uploaded_shape) else selected_ids)
  }

  # Reference zones are polygons (area-based intersection)
  zone_area <- suppressWarnings(suppressMessages(as.numeric(sf::st_area(zones))))
  
  # Dissolve uploaded shape to prevent double-counting overlaps from overlapping input features
  intersections <- suppressWarnings(suppressMessages(sf::st_intersection(
    zones[, "second_location_id", drop = FALSE],
    suppressWarnings(suppressMessages(sf::st_union(uploaded_shape)))
  )))
  
  if (nrow(intersections) == 0) {
    selected_ids <- character(0)
    return(if (return_shape) list(ids = selected_ids, shape = uploaded_shape) else selected_ids)
  }

  # Calculate the area of the intersections
  overlap_area <- stats::aggregate(
    suppressWarnings(suppressMessages(as.numeric(sf::st_area(intersections)))),
    by = list(second_location_id = intersections$second_location_id),
    FUN = sum
  )
  names(overlap_area)[2] <- "area"
  
  # Merge total zone area with overlapped area
  zone_overlap <- data.frame(
    second_location_id = as.character(zones$second_location_id),
    area = zone_area,
    stringsAsFactors = FALSE
  )
  zone_overlap <- merge(zone_overlap, overlap_area,
                        by = "second_location_id", all.x = TRUE,
                        suffixes = c("_zone", "_overlap"))
  
  zone_overlap$area_overlap[is.na(zone_overlap$area_overlap)] <- 0

  # Filter based on the threshold (overlap area / total zone area >= threshold)
  selected_ids <- zone_overlap$second_location_id[
    zone_overlap$area_zone > 0 &
      (100 * zone_overlap$area_overlap / zone_overlap$area_zone) >= overlap_threshold
  ]
  
  if (return_shape) list(ids = selected_ids, shape = uploaded_shape) else selected_ids
}