spatial_qaqc <- function(dat, project, spat, lon.dat, lat.dat, lon.spat = NULL,
                         lat.spat = NULL, id.spat = NULL, epsg = NULL, date = NULL, 
                         group = NULL, filter_dist = NULL) {
  #' Spatial Data Quality Checks
  #' 
  #' This function performs spatial quality checks and outputs summary tables and 
  #' plots.
  #' 
  #' @param dat Primary data containing information on hauls or trips. Table in 
  #'   FishSET database contains the string 'MainDataTable'.
  #' @param project Name of project.
  #' @param spat Spatial data containing information on fishery management or 
  #'   regulatory zones. See \code{\link{read_dat}} for details on importing 
  #'   spatial data. 
  #' @param lon.dat Longitude variable in \code{dat}.
  #' @param lat.dat Latitude variable in \code{dat}.
  #' @param lon.spat Variable or list from \code{spat} containing longitude data. 
  #'    Required for csv files. Leave as NULL if \code{spat} is a shape or json file.
  #' @param lat.spat Variable or list from \code{spat} containing latitude data. 
  #'   Required for csv files. Leave as NULL if \code{spat} is a shape or json file.
  #' @param id.spat Polygon ID column. Required for csv files. Leave as NULL if 
  #'   \code{spat} is a shape or json file.
  #' @param epsg EPSG number. Set the epsg to ensure that \code{spat} and \code{dat} 
  #'   have the same projections. If epsg is not specified but is defined for 
  #'   \code{spat}, then the \code{spat} epsg will be applied to \code{dat}. 
  #'   See \url{http://spatialreference.org/} to help identify optimal epsg number.
  #' @param date String, name of date variable. Used to summarize over year. If
  #'   \code{NULL} the first date column will be used. Returns an error if no date
  #'   columns can be found. 
  #' @param group String, optional. Name of variable to group spatial summary by. 
  #' @param filter_dist (Optional) Numeric, distance value to filter primary data by. 
  #'   Rows containing distance values greater than or equal to \code{filter_dist}
  #'   will be removed from the data. This action will be saved to the filter table.
  #' @export
  #' @import ggplot2
  #' @import sf
  #' @importFrom rlang sym
  #' @importFrom dplyr group_by summarize
  #' @importFrom magrittr %>% 
  #' @importFrom stats dist
  #' @return A list of plots and/or dataframes depending on whether spatial data 
  #' quality issues are detected. The list includes:
  #'   \describe{
  #'     \item{dataset}{Primary data. Up to five logical columns will be added if
  #'       spatial issues are found: "ON_LAND" (if obs fall on land), "OUTSIDE_ZONE"
  #'       (if obs occur at sea but outside zone), "ON_ZONE_BOUNDARY" (if obs occurs
  #'       on zone boundary), "EXPECTED_LOC" (whether obs occurs at sea, within a zone,
  #'       and not on zone boundary), and "NEAREST_ZONE_DIST_M" (distance in meters from
  #'       nearest zone. Applies only to obs outside zone or on land).}
  #'     \item{spatial_summary}{Dataframe containing the percentage of observations 
  #'       that occur at sea and within zones, on land, outside zones but at sea, 
  #'       or on zone boundary by year and/or group. The total number of observations by 
  #'       year/group are in the "N" column.}
  #'     \item{outside_plot}{Plot of observations outside regulatory zones.}
  #'     \item{land_plot}{Plot of observations that fall on land.}
  #'     \item{land_out_plot}{Plot of observations that occur on land and are outside
  #'       the regulatory zones (combines outside_plot and land_plot if both occur).}
  #'     \item{boundary_plot}{Plot of observations that fall on zone boundary.}
  #'     \item{expected_plot}{Plot of observations that occur at sea and within zones.}
  #'     \item{distance_plot}{Histogram of distance form nearest zone (meters) by year 
  #'       for observations that are outside regulatory grid.}
  #'     \item{distance_freq}{Binned frequency table of distance values.}
  #'     \item{distance_summary}{Dataframe containing the minimum, 1st quartile, 
  #'       median, mean, 3rd quartile, and maximum distance values by year and/or group.}
  #'   }
  #' @examples 
  #' \dontrun{
  #' spatial_qaqc("pollockMainDataTable", "pollock", "NMFS_ZONE", lon.dat = "LonLat_START_LON",
  #'              lat.dat = "LonLat_START_LAT")
  #' }
  #'   
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  spatout <- data_pull(spat, project)
  spatdat <- spatout$dataset
  spat <- parse_data_name(spat, "spat")
  
  tmp <- tempfile()
  
  end <- FALSE
  out_col <- NULL
  land_col <- NULL
  bound_col <- NULL
  expected_col <- NULL
  
  grp_len <- length(c("YEAR", group))
  
  # Year ----
  
  if (!is.null(date)) {
    
    dataset[[date]] <- date_parser(dataset[[date]])
    dataset$YEAR <- as.integer(format(dataset[[date]], "%Y"))
    
  } else {
    
    date <- date_cols(dataset)[1]
    
    if (length(date) == 0) {
      
      warning("'date' column required.")
      end <- TRUE
      
    } else {
      
      dataset[[date]] <- date_parser(dataset[[date]])
      dataset$YEAR <- as.integer(format(dataset[[date]], "%Y"))
    }
  }
  
  # Lat Lon checks ----
  
  lat_lon <- grep("lat|lon", names(dataset), ignore.case = TRUE)
  lat_cols <- find_lat(dataset)
  lon_cols <- find_lon(dataset)
  
  num_ll <- !qaqc_helper(dataset[lat_lon], is.numeric)
  
  lat_deg <- qaqc_helper(dataset[lat_cols], function(x) {
    if (!is.numeric(x)) TRUE
    else any(nchar(trunc(abs(x))) > 2)}) 
  
  lon_deg <- qaqc_helper(dataset[lon_cols], function(x) {
    if (!is.numeric(x)) TRUE
    else any(nchar(trunc(abs(x))) > 3)}) 
  
  if (any(c(lat_deg, lon_deg, num_ll))) {
    
    lat_deg_ind <- which(lat_deg)
    lon_deg_ind <- which(lon_deg)
    num_ind <- which(num_ll)
    
    warning(paste("The following latitude/longitude variables are not in decimal degrees:", 
                  paste(names(dataset)[unique(c(num_ind, lat_deg_ind, lon_deg_ind))], collapse = ","), 
                  "\nRun 'degree' function to convert to decimal degrees."))
    
    end <- TRUE
  } 
  
  if (any(abs(dataset[[lon.dat]]) > 180)) {
    
    warning("Longitude is not valid (outside -180:180). Function not run")
    end <- TRUE
  }
  
  if (any(abs(dataset[[lat.dat]]) > 90)) {
    
    warning("Latitude is not valid (outside -90:90. Function not run")
    end <- TRUE
  }
  
  if (!is.null(filter_dist)) {
    
    if (!is.numeric(filter_dist)) {
      
      warning("filter_dist must be numeric")
      end <- TRUE
      
    } else if (filter_dist < 0) {
      
      warning("filter_dist must be positive")
      end <- TRUE
    }
  }
  
  if (end == FALSE) {

    # convert dat to sf object
    dat_sf <- sf::st_as_sf(x = dataset, coords = c(lon.dat, lat.dat), 
                            crs = "+proj=longlat +datum=WGS84")
    
    if (!("sf" %in% class(spatdat))) {
      
      if ("sp" %in% class(spatdat)) spatdat <- sf::st_as_sf(spatdat)
      
      else {
        
        spatdat <- sf::st_as_sf(x = spatdat, coords = c(lon.spat, lat.spat), 
                                crs = "+proj=longlat +datum=WGS84")
        
        # st_shift_longitude
        
        
        # Convert point geometry to polygon
        spatdat <- 
          spatdat %>%
          dplyr::group_by(dplyr::across(id.spat)) %>% 
          dplyr::summarize(do_union = FALSE) %>% 
          sf::st_cast("POLYGON")
        
        spatdat <- sf::st_cast(spatdat, "MULTIPOLYGON")
      }
    }
    
    if (sf::st_crs(spatdat) != sf::st_crs(dat_sf)) {
      
      warning("Projection does not match. The detected projection in the",
              " spatial file will be used unless epsg is specified.")
    }
    
    if (!is.null(epsg)) {
      
      dat_sf <- sf::st_transform(dat_sf, epsg)
      spatdat <- sf::st_transform(spatdat, epsg)
      
    } else if (!is.na(sf::st_crs(spatdat))) {
      
      dat_sf <- sf::st_transform(dat_sf, sf::st_crs(spatdat))
      
    } else {
      
      spatdat <- sf::st_transform(spatdat, "+proj=longlat +datum=WGS84")
    }
    
    # base map ---- 
    
    bbox <- sf::st_bbox(dat_sf)
    
    base_map <- ggplot2::map_data("world", 
                                  xlim = c(bbox["xmin"], bbox["xmax"]), 
                                  ylim = c(bbox["ymin"], bbox["ymax"]))
    
    base_map <- sf::st_as_sf(base_map, coords = c("long", "lat"), 
                             crs = sf::st_crs(spatdat))
    
    # convert points to polygon
    base_map <- 
      base_map %>%
      dplyr::group_by(group) %>% 
      dplyr::summarize(do_union = FALSE) %>% 
      sf::st_cast("POLYGON")
    
    # plot functions 
    lon_sym <- rlang::sym(lon.dat)
    lat_sym <- rlang::sym(lat.dat)
    
    group_exp <- function() {
      
      if (!is.null(group)) {
        
        g_sym <- rlang::sym(group)
        rlang::expr(as.factor(!!g_sym))
        
      } else NULL
    }
    
    if (any(!(sf::st_is_valid(spatdat)))) {
      
      spatdat <- sf::st_make_valid(spatdat)
    } 
    
    # points on land ----
    
    land_pts <- sf::st_intersects(dat_sf, base_map)
    
    obs_on_land <- vapply(land_pts, function(x) length(x) > 0, logical(1))
    
    if (sum(obs_on_land) > 0) {
      
      land_ind <- which(obs_on_land)
      n_land <- sum(obs_on_land)
      p_land <- round(n_land/nrow(dataset) * 100, 1)
      land_msg <- paste0(n_land, " observations (", p_land, "%) occur on land.\n")
      
      cat(land_msg, file = tmp)
      warning(land_msg)
      
      land_col <- "ON_LAND"
      dataset[[land_col]] <- obs_on_land
      
      land_plot <- 
        ggplot2::ggplot() + 
        ggplot2::geom_sf(data = base_map) + 
        ggplot2::geom_point(data = dataset[land_ind, ], 
                            ggplot2::aes(x = !!lon_sym, y = !!lat_sym), 
                            size = 1, alpha = .25, color = "red") + 
        ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                          expand = TRUE) +
        ggplot2::labs(title = "Obs on land", x = "Longitude", y = "Latitude",
                      subtitle = paste0("N:", n_land, " (", p_land, "%)")) + 
        fishset_theme() # + guides(color = guide_legend(override.aes = list(alpha = 1)))
    }
    
    # points outside zone ----
    
    pts_int <- sf::st_intersects(dat_sf, spatdat)
    
    obs_outside <- vapply(pts_int, function(x) length(x) == 0, logical(1))
    
    if (sum(obs_outside) > 0) {
      
      obs_out_not_land <- obs_outside != obs_on_land # remove land obs
      
    } else {
      
      obs_out_not_land <- FALSE
    }
    
    
    if (sum(obs_out_not_land) > 0) {
      
      out_nl_ind <- which(obs_out_not_land)
      n_out <- sum(obs_out_not_land)
      p_out <- round(n_out/nrow(dataset) * 100, 1)
      out_msg <- paste0(n_out, " observations (", p_out, "%) are outside the regulatory zones.\n")
      
      cat(out_msg, file = tmp, append = TRUE)
      warning(out_msg)
      
      out_col <- "OUTSIDE_ZONE"
      dataset[[out_col]] <- obs_out_not_land
      
      outside_plot <- 
        ggplot2::ggplot() + 
        ggplot2::geom_sf(data = base_map) + 
        ggplot2::geom_point(data = dataset[out_nl_ind, ], 
                            ggplot2::aes(x = !!lon_sym, y = !!lat_sym), 
                            size = 1, alpha = .25, color = "red") + 
        ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                          expand = TRUE) +
        ggplot2::labs(title = "Obs outside zone", x = "Longitude", y = "Latitude",
                      subtitle = paste0("N:", n_out, " (", p_out, "%)")) + 
        fishset_theme() # + guides(color = guide_legend(override.aes = list(alpha = 1)))
    }
    
    # obs on zone boundary lines ----
    obs_on_bound <- vapply(pts_int, function(x) length(x) > 1, logical(1))
    
    if (sum(obs_on_bound) > 0) {
      
      bound_ind <- which(obs_on_bound)
      n_bound <- sum(obs_on_bound)
      p_bound <- round(n_bound/nrow(dataset) * 100, 1)
      bound_msg <- paste0(n_bound, " observations (", p_bound, "%) occur on boundary",
                          " line between regulatory zones.\n")
      
      cat(bound_msg, file = tmp, append = TRUE)
      warning(bound_msg)
      
      bound_col <- "ON_ZONE_BOUNDARY"
      dataset[[bound_col]] <- obs_on_bound
      
      bound_plot <- 
        ggplot2::ggplot() + 
        ggplot2::geom_sf(data = base_map) + 
        ggplot2::geom_point(data = dataset[bound_ind, ], 
                            ggplot2::aes(x = !!lon_sym, y = !!lat_sym), 
                            size = 1, alpha = .25, color = "red") + 
        ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                          expand = TRUE) +
        ggplot2::labs(title = "Obs on zone boundary", x = "Longitude", y = "Latitude",
                      subtitle = paste0("N:", n_bound, " (", p_bound, "%)")) + 
        fishset_theme() # + guides(color = guide_legend(override.aes = list(alpha = 1)))
    }
    
    # expected location ----
    obs_expected_loc <- !obs_on_bound & !obs_on_land & !obs_outside
    
    expected_col <- "EXPECTED_LOC"
    dataset[[expected_col]] <- obs_expected_loc
    
    n_expected <- sum(obs_expected_loc)
    p_expected <- round(n_expected/nrow(dataset) * 100, 1)
    
    expected_plot <- 
      ggplot2::ggplot() + 
      ggplot2::geom_sf(data = base_map) + 
      ggplot2::geom_point(data = dataset[obs_expected_loc, ], 
                          ggplot2::aes(x = !!lon_sym, y = !!lat_sym), 
                          size = 1, alpha = .15, color = "red") + 
      ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                        expand = TRUE) + 
      ggplot2::labs(title = "Obs in expected location", x = "Longitude", y = "Latitude",
                    subtitle = paste0("N:", n_expected, " (", p_expected, "%)")) + 
      fishset_theme() # + guides(color = guide_legend(override.aes = list(alpha = 1)))
    
    # Spatial summary table ----
    
    if (any(!is.null(out_col), !is.null(land_col), !is.null(bound_col))) {
      
      spat_tab <- agg_helper(dataset, value = c(expected_col, out_col, land_col, bound_col),
                             group = c("YEAR", group), fun = sum)
    
      year_tab <- agg_helper(dataset, value = "YEAR", group = c("YEAR", group),
                             fun = length)
      
      spat_tab$N <- year_tab[[grp_len + 1]]
      
      spat_nm <- names(spat_tab[c(expected_col, out_col, land_col, bound_col)])
      perc_nm <- paste0(c(expected_col, out_col, land_col, bound_col), "_perc")
      
      spat_tab[perc_nm] <-
        round(spat_tab[ ,c(expected_col, out_col, land_col, bound_col)] / spat_tab[ ,"N"] * 100, 2)
      
      spat_tab[spat_nm] <- NULL
      
      spat_tab <- spat_tab[order(spat_tab$YEAR), 
                           c("YEAR", "N", group, perc_nm)]
      
      row.names(spat_tab) <- 1:nrow(spat_tab)
    }
    
    # distance from nearest zone (meters) ----
    
    if (sum(obs_outside) > 0) {
      
      nearest <- sf::st_nearest_feature(dat_sf[obs_outside, ], spatdat) 
      dist.rec <- sf::st_distance(dat_sf[obs_outside, ], spatdat[nearest, ], 
                                  by_element = TRUE)
      
      dataset[obs_outside, "dist"] <- as.numeric(dist.rec)
      dataset$dist[is.na(dataset$dist)] <- 0
      dist_vec <- dataset$dist
      
      # dist plot
      dist_df <- dataset[obs_outside, c(lat.dat, lon.dat, "YEAR", group, "dist")]
      
      dist_rng <- range(dist_df$dist, finite = TRUE)
      p_brks <- pretty(dist_rng, n = 15, min.n = 1)
      
      dist_plot <- 
        ggplot2::ggplot(data = dist_df, ggplot2::aes(dist)) + 
        ggplot2::labs(title = "Distance (m) from nearest zone", 
                      x = "Distance (m)", fill = "Year") +
        ggplot2::stat_density(aes(fill = factor(YEAR)), position = "stack", 
                              adjust = 2, color = "black") +
        fishset_theme() + 
        ggplot2::theme(legend.position = "bottom")
      
      # freq table
      dist_freq <- freq_table(dist_df, "dist", group = c("YEAR", group), 
                              bins = 15, type = "freq", format_lab = "decimal")
      
      # dist summary table
      dist_sum <- agg_helper(dist_df, "dist", group = c("YEAR", group), 
                             fun = function(x) summary(x, digits = 2))
      
      dsm <- as.data.frame(dist_sum$dist)
      dist_sum$dist <- NULL
      dist_sum <- cbind(dist_sum, dsm)
      dist_sum <- dist_sum[order(dist_sum$YEAR), ]
      row.names(dist_sum) <- 1:nrow(dist_sum)
      
      names(dataset)[names(dataset) == "dist"] <- "NEAREST_ZONE_DIST_M"
      
      if (!is.null(filter_dist)) {
        
        dataset <- dataset[dataset$NEAREST_ZONE_DIST_M < filter_dist, ]
        
        filter_table(dataset, project, x = "NEAREST_ZONE_DIST_M",
                     exp = paste0("NEAREST_ZONE_DIST_M < ", filter_dist))
      }
    }
    
    if (sum(obs_on_bound, obs_outside) == 0) {
      
      cat(c("All observations occur on land and within regulatory zones.", 
            "No observations fall on zone boundaries."), file = tmp)
    }
    
    # arrange plots ----
    # on land/outside zone 
    if (sum(obs_on_land) > 0 & sum(obs_out_not_land) > 0) {
      
      land_out_plot <- gridExtra::arrangeGrob(grobs = list(land_plot, outside_plot), 
                                         nrow = 1, ncol = 2)
    }
    
    msg_print(tmp)
    
    # Log function
    spatial_qaqc_function <- list()
    spatial_qaqc_function$functionID <- "spatial_qaqc"
    spatial_qaqc_function$args <- list(dat, project, spat, lon.dat, lat.dat, 
                                       lon.spat, lat.spat, id.spat, epsg, date, group,
                                       filter_dist)
    spatial_qaqc_function$msg <- suppressWarnings(readLines(tmp))
    log_call(project, spatial_qaqc_function)
    
    unlink(tmp)
    
    f_plot <- function(x) if (!is.null(x)) gridExtra::grid.arrange(x) else NULL
    
    f_land <- function() {
      
      if (sum(obs_on_land) > 0 & sum(obs_out_not_land) == 0) land_plot
      else NULL
    }
    
    f_outside <- function() {
      
      if (sum(obs_out_not_land) > 0 & sum(obs_on_land) == 0) outside_plot
      else NULL
    }
    
    f_land_out <- function() {
      
      if (sum(obs_out_not_land) > 0 & sum(obs_on_land) > 0) {
        gridExtra::grid.arrange(land_out_plot)
      } else NULL
    }
    
    out <- 
    list(dataset = dataset,
         spatial_summary = get0("spat_tab"),
         land_plot = f_land(), 
         outside_plot =  f_outside(), 
         land_outside_plot = f_land_out(),
         boundary_plot = get0("bound_plot"),
         expected_plot = expected_plot,
         distance_plot = get0("dist_plot"),
         distance_freq = get0("dist_freq"),
         distance_summary = get0("dist_sum"),
         land_ind = get0("land_ind"),
         outside_ind = get0("out_nl_ind"),
         bound_ind = get0("bound_ind"),
         dist_vector = get0("dist_vec")
         )
    
    ind <- vapply(out, function(x) !is.null(x), logical(1))
    out_nms <- names(ind[ind][-1])# skip main data
    
    # save output ----
    lapply(out_nms, function(nm) {
      
      if (!is.null(out[[nm]])) {
        
        if (is.data.frame(out[[nm]])) {
          
          save_table(out[[nm]], project, paste0("spatial_qaqc_", nm))
          
        } else if (nm %in% c("land_ind", "outside_ind", "bound_ind", "dist_vector")) {
          
          out_ind <- data.frame(out[[nm]]) 
          names(out_ind) <- nm
          
          save_table(out_ind, project, paste0("spatial_qaqc_", nm))
          
        } else {
          
          save_plot(project, paste0("spatial_qaqc_", nm), plot = out[[nm]])
        }
      }
    })
    
    ind[c("land_ind", "outside_ind", "bound_ind", "dist_vector")] <- FALSE
    out[ind]
  }
}


spat_qaqc_gui <- function(dataset, project, spatdat, checks = NULL) {
  
  #' GUI for spatial data checks
  #' 
  #' Runs the spatial checks performed by \code{\link{spatial_qaqc}} in a shiny
  #' application. 
  #' 
  #' @param dataset Primary data containing information on hauls or trips. Table in 
  #'   FishSET database contains the string 'MainDataTable'.
  #' @param project Name of project.
  #' @param spatdat Spatial data containing information on fishery management or 
  #'   regulatory zones. See \code{\link{read_dat}} for details on importing 
  #'   spatial data. 
  #' @param checks (Optional) A list of spatial data quality checks outputted by
  #'   \code{spatial_qaqc}.
  # @param ... Additional arguments passed to \code{\link{spat_qaqc}}.
  #' @export
  #' @import shiny
  #' @importFrom shinycssloaders withSpinner
  #' @importFrom DT DTOutput renderDT dataTableProxy editData replaceData
  #' @importFrom gridExtra grid.arrange
  #' @importFrom utils capture.output
  #' @seealso \code{\link{spatial_qaqc}}

  # plot rendering function
  n_plot_output <- function(out, ...) {
    
    if ("ggplot" %in% class(out)) {
      
      tagList(renderPlot(out, ...))
      
    } else if ("gtable" %in% class(out)) {
      
      tagList(renderPlot(gridExtra::grid.arrange(out), ...))
      
      
    } else if (all(class(out) == "list")) {
      
      lapply(out, function(x) renderPlot(x, ...))
    }
  }
  
  shinyApp(
    
    ui = fluidPage(
      
      sidebarLayout(
        
        sidebarPanel(
          
          actionButton('saveData','Save data to FishSET database',
                       style = "color: white; background-color: blue;"),
          
          tags$button(
            id = 'close',
            type = "button",
            style ="color: #fff; background-color: #FF6347; border-color: #800000;",
            class = "btn action-button",
            onclick = "setTimeout(function(){window.close();},500);",  # close browser
            "Close app"),
          
          tags$br(), tags$br(), tags$br(),
          
          conditionalPanel("input.spat_qaqc_tab == 'checks'",
                           uiOutput("spatQAQC_checkUI")),
          
          conditionalPanel("input.spat_qaqc_tab == 'corrections'",
          uiOutput("spatQAQC_correctUI")),
          
          textInput("exprQA", label = "Enter an R expression",
                    value = "values$dataset"),
          actionButton("runQA", "Run", class = "btn-success"),
          div(style = "margin-top: 2em;",
              uiOutput('resultQA'))
          
        ),
        
        mainPanel(
          
          tabsetPanel(id = "spat_qaqc_tab", selected = "checks",
                      
                      tabPanel(title = "Spatial Checks", value = "checks",
                               shinycssloaders::withSpinner(uiOutput("spatQAQC_checkOut"))      
                      ),
                      
                      tabPanel(title = "Spatial Corrections", value = "corrections",
                               uiOutput("spat_correct_msg"),
                               tags$div(shinycssloaders::withSpinner(DT::DTOutput("spat_correct_tab")), 
                                        style = "font-size: 75%; width: 100%")
                      )
          )
        )
      )
    ),
    
    server = function(input, output, session) {
      
      spat_qaqc_r <- reactiveValues(flag = FALSE, c_tab = NULL, remove = FALSE)
      
      values <- reactiveValues(dataset = dataset)
      
      spat_out <- reactive({
        
        if (is.null(checks)) spat_qaqc() else checks
      })
      
      r <- reactiveValues(done = 0, ok = TRUE, output = "")
      
      observeEvent(input$runQA, {
        shinyjs::hide("error")
        r$ok <- FALSE
        tryCatch(
          {
            r$output <- isolate(
              paste(utils::capture.output(eval(parse(text = input$exprQA))), collapse = '\n')
            )
            r$ok <- TRUE
          },
          error = function(err) {r$output <- err$message}
        )
        r$done <- r$done + 1
      })
      
      output$resultQA <- renderUI({
        if(r$done > 0 ) { 
          content <- paste(paste(">", isolate(input$exprQA)), r$output, sep = '\n')
          if(r$ok) {
            pre(content)
          } else {
            pre( style = "color: red; font-weight: bold;", content)
          }
        }
      })
      
      
      observe({
        
        if (!is.null(checks)) {
          
          flag_nms <- c("land_ind", "outside_ind", "bound_ind")
          
          spat_qaqc_r$flag <- vapply(checks[flag_nms], function(x) !is.null(x), logical(1))
        }
      })
      
      output$spatQAQC_checkUI <- renderUI({
          
        if (is.null(checks)) {
          
          tagList(         
           actionButton("runSpatQAQC", "Run spatial check",
                        style = "color: white; background-color: #0073e6;"), 
           selectInput("spat_qaqc_lat", "Select Latitude from main data",
                       choices = find_lat(dataset)),
           selectInput("spat_qaqc_lon", "Select Longitude from main data",
                       choices = find_lon(dataset)),
           selectInput("spat_qaqc_date", "Select date variable", 
                       choices = date_cols(dataset)),
           numericInput("spat_qaqc_epsg", "(Optional) enter EPSG code",
                        value = NULL),
           selectizeInput("spat_qaqc_grp", "(Optional) select grouping variable",
                          choices = category_cols(dataset),
                          multiple = TRUE, options = list(maxItems = 1, create = TRUE))
          )
        }
      })
      
      # run spatial checks 
      spat_qaqc <- eventReactive(input$runSpatQAQC, {
        
        if (!is.null(spatdat)) {
          
          q_test <- quietly_test(spatial_qaqc)
          
          out <- q_test(dataset, project, spatdat, lon.dat = input$spat_qaqc_lon, 
                        lat.dat = input$spat_qaqc_lat, date = input$spat_qaqc_date, 
                        group = input$spat_qaqc_grp)
          
          flag_nms <- c("ON_LAND", "OUTSIDE_ZONE", "ON_ZONE_BOUNDARY")
          spat_qaqc_r$flag <- vapply(flag_nms, function(x) x %in% names(out$dataset), logical(1))
          
          values$dataset <- out$dataset
          
          out$dataset <- NULL
      
          out
        }
      })
      
      # spatial checks output
      output$spatQAQC_checkOut <- renderUI({
        
        render_out <- 
          lapply(names(spat_out()), function(x) {  
            
            if (is.data.frame(spat_out()[[x]])) {
              
              tab_header <- switch(x, "spatial_summary" = "Spatial summary table",
                                   "distance_freq" = "Distance (m) frequency table",
                                   "distance_summary" = "Distance (m) summary table")
              
              tagList(
                h4(strong(tab_header)),
                DT::renderDT(spat_out()[[x]])
              )
              
            } else {
              
              plot_header <- switch(x, "outside_plot" = "Points outside zone",
                                    "land_plot" = "Points on land",
                                    "land_outside_plot" = "Points on land/outside zone",
                                    "boundary_plot" = "Points on zone boundary",
                                    "expected_plot" = "Points at sea and within zones",
                                    "distance_plot" = "Density of point distance (m) from nearest zone")
              
              tagList(
                h4(strong(plot_header)),
                n_plot_output(spat_out()[[x]])
              )
            }
        
            })
          
          render_out
      })
      
      # Spatial Correction
      
      output$spatQAQC_correctUI <- renderUI({
        
        if (any(spat_qaqc_r$flag)) {
          tagList(
            
            actionButton("spat_filter_bttn", "Apply Lat/Lon sign changes",
                         style = "color: white; background-color: #0073e6;"),
            
            uiOutput("latLonCorr"),
            
            selectInput('spat_filter_lat', 'Change sign for latitude direction', 
                        choices=c('None', 'All values'='all', 'Positve to negative'='neg', 
                                  'Negative to positive'='pos'), selected='None'),
            
            selectInput('spat_filter_lon', 'Change sign for longitude direction', 
                        choices=c('None', 'All values'='all', 'Positve to negative'='neg', 
                                  'Negative to positive'='pos'), selected='None'),
            
            actionButton("dist_remove_bttn", "Remove points",
                         style = "color: white; background-color: #0073e6;"),
            numericInput("dist_remove", "Distance (m) from nearest zone",
                         value = 100, min = 1),
            sliderInput("dist_slider", "",
                        # min = floor(min(spat_qaqc()$dist_vector, na.rm = TRUE)),
                        min = 1,
                        max = ceiling(max(values$dataset$NEAREST_ZONE_DIST_M, na.rm = TRUE)),
                        value = 100)
              )
        }
        
      })
      
      output$latLonCorr <- renderUI({
        
        if (!is.null(checks)) {
          
          tagList(
            selectInput("spat_qaqc_lat", "Select Latitude from main data",
                        choices = find_lat(dataset)), 
            selectInput("spat_qaqc_lon", "Select Longitude from main data",
                        choices = find_lon(dataset)),
          )
        }
      })
      
      output$spat_correct_msg <- renderUI({
        
        if (names(spatdat)[1] == "var1") {
          
          p("Spatial data not loaded. Import spatial data on the 'Upload' tab.")
          
        } else if (length(spat_qaqc_r$flag) == 1) {
          
          p("Spatial checks have not been run.")
          
        } else if (length(spat_qaqc_r$flag) == 3 & all(spat_qaqc_r$flag) == FALSE) {
          
          p("No spatial issues were found.")
        }
      })
      
      # add flag cols to dataset
      observeEvent(any(spat_qaqc_r$flag), {
          
          # disable editing for non-latlon columns
          s_lon <- if (is.null(checks)) input$spat_qaqc_lon else find_lon(dataset)
          s_lat <- if (is.null(checks)) input$spat_qaqc_lat else find_lat(dataset)
          
          latlon <- which(names(dataset) %in% c(s_lon, s_lat))
          
          spat_qaqc_r$disable <- which(!(seq_along(dataset) %in% latlon))
      })
      
      # filter distance
      dist_filter <- eventReactive(any(spat_qaqc_r$flag), {
        
        if ("NEAREST_ZONE_DIST_M" %in% names(values$dataset)) {
          
          values$dataset[["NEAREST_ZONE_DIST_M"]] >= input$dist_slider
        }
      })
      
      c_tab <- eventReactive(c(sum(dist_filter()), input$spat_filter_bttn), {
        
        values$dataset[dist_filter(), ]
      })
      
      observe({
        
        spat_qaqc_r$c_tab <- c_tab()
      })
      
      # Correction table
      output$spat_correct_tab <- DT::renderDT({
        
        if (any(spat_qaqc_r$flag)) {
          
          c_tab()
        }
      },
      
      server = TRUE, 
      editable = list(target ='cell', disable = list(columns = spat_qaqc_r$disable)), 
      filter = 'top', extensions = c("Buttons"),
      options = list(autoWidth = TRUE, scrolly = TRUE, responsive = TRUE, pageLength = 15,
                     buttons = c('csv'))
      )
      
      # edit table
      spat_qaqc_proxy <- DT::dataTableProxy("spat_correct_tab", session)
      
      observeEvent(input$spat_correct_tab_cell_edit, {
        
        spat_qaqc_r$c_tab <<- DT::editData(spat_qaqc_r$c_tab,
                                           info = input$spat_correct_tab_cell_edit,
                                           proxy = "spat_correct_tab",
                                           resetPaging = FALSE)
        
        DT::replaceData(spat_qaqc_proxy, spat_qaqc_r$c_tab, resetPaging = FALSE)
      })
      
      # update correction table UI
      observeEvent(input$dist_remove, {
        
        updateSliderInput(session, "dist_slider", value = input$dist_remove)
      })
      
      observeEvent(input$dist_slider, {
        
        updateSliderInput(session, "dist_remove", value = input$dist_slider)
      })
      
      observeEvent(sum(dist_filter()), {
        
        updateActionButton(session, "dist_remove_bttn",
                           label = paste("Remove", sum(dist_filter()), "points"))
      })
      
      # remove points based on distance
      observeEvent(input$dist_remove_bttn, {
        
        # add pop-up confirming removal 
        
        showModal(
          modalDialog(title = paste("Remove", sum(dist_filter()), "rows?"),
                      
                      actionButton("confirm_dist_remove", "Remove", 
                                   style = "color: white; background-color: #0073e6;"),
                      actionButton("dist_remove_cancel", "Cancel", 
                                   style = "color: #fff; background-color: #FF6347; border-color: #800000;"),
                      
                      footer = tagList(modalButton("Close")),
                      easyClose = FALSE, size = "s"))
      })
      
      observeEvent(input$confirm_dist_remove, spat_qaqc_r$remove <- TRUE)
      
      observeEvent(input$dist_remove_cancel, removeModal())
      
      observeEvent(spat_qaqc_r$remove == TRUE, {
        
        nr <- sum(dist_filter())
        
        values$dataset <- values$dataset[!dist_filter(), ]
        
        removeModal()
        
        showNotification(paste(nr, "points removed"), type = "message")
        
        filter_table(dataset, project, x = "NEAREST_ZONE_DIST_M",
                     exp = paste0("NEAREST_ZONE_DIST_M < ", input$dist_remove))
        
      }, ignoreInit = TRUE)
      
      # update Lat Lon
      observeEvent(input$spat_correct_tab_cell_edit, {
        
        values$dataset[dist_filter(), 
                       c(input$spat_qaqc_lat, 
                         input$spat_qaqc_lon)] <- spat_qaqc_r$c_tab[c(input$spat_qaqc_lat, 
                                                                      input$spat_qaqc_lon)]
        showNotification("Latitude and longitude values updated to main table",
                         type = "message")
      })
      
      # change Lat/Lon signs
      observeEvent(input$spat_filter_bttn, {
        
        q_test <- quietly_test(degree)
        
        values$dataset[dist_filter(), ] <-
          q_test(c_tab(), project = project, lat = input$spat_qaqc_lat, 
                 lon = input$spat_qaqc_lon, latsign = input$spat_filter_lat,
                 lonsign = input$spat_filter_lon, replace = TRUE)
      })
      
      # save data to FishSET DB
      observeEvent(input$saveData, {
        
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
        DBI::dbWriteTable(fishset_db, paste0(project(), 'MainDataTable'), values$dataset, overwrite = TRUE)
        DBI::dbDisconnect(fishset_db)
        showNotification('Data saved to FishSET database', type = 'message', duration = 10)
      })
      
      # close app
      observeEvent(input$close, stopApp())
    }
  )
} 