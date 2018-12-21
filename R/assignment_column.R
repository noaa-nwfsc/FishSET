#'  Generate centroid of polygon of zone or area

#' @param dataset dataframe or matrix
#' @param gridfile name of gridded dataset
#' @param hull.polygon If TRUE, create convex hull polygon. Use if data points creating polygon are incomplete.
#' @param lon.dat Longitude of points from dataset
#' @param lat.dat Latitude of points from dataset
#' @param lon.grid Longitude of points from gridfile
#' @param lat.grid Latitude of points from gridfile
#' @param cat Categorical variable defining the individual areas or zones
#' @keywords  zone, polygon
#' @return Returns dataset with new assignment column labeled zoneID
#' @details Converts point data from gridfile into polygons and then finds which polygon each point in the dataset is within. Use hull.polygon=T if data is sparse or irregular.


#require(geosphere)
#require(sf)  # for shape file
#require(rgeos)  # create polygones from points
#require(sp)
#adfg <- read.csv("Data/adfgstat6_map.csv", header = F)
# require(rjson) map <- fromJSON('C:/Users/Melanie/Documents/FishSET_RPackage/Data/NMFS_RA.json', flatten=TRUE) map <- map$features

assignmentColumn <- function(dataset, gridfile, hull.polygon = c(TRUE, FALSE), lon.grid, lat.grid, lon.dat, lat.dat, cat) {
    # sort data
    gridfile <- gridfile[order(gridfile[, cat], gridfile[, lon.grid], gridfile[, lat.grid]), ]
    
    # Create spatial polygon dataframe from grid data make a list
    map_list <- split(gridfile[, c(lon.grid, lat.grid, cat)], gridfile[[cat]])
    # only want lon-lats in the list, not the names
    map_list <- lapply(map_list, function(x) {
        x[cat] <- NULL
        x
    })
    
    if (hull.polygon == T) {
        ps <- lapply(map_list, function(x) x[c(chull(x), chull(x)[1]), ])
        p1 <- lapply(seq_along(ps), function(i) Polygons(list(Polygon(ps[[i]])), ID = names(map_list)[i]))
    } else {
        # add id variable
        ps <- lapply(map_list, Polygon)
        p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), ID = names(map_list)[i]))
    }
    
    my_spatial_polys <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    # Change to spatial polygon dataframe
    srdf <- SpatialPolygonsDataFrame(my_spatial_polys, data.frame(row.names = c(names(map_list)), ID = names(map_list)))
    
    # Assign zone to dataset based on lat and long
    dat_sub <- dataset
    # Assignment modified according
    coordinates(dat_sub) <- c(lon.dat, lat.dat)
    
    # Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
    proj4string(dat_sub) <- CRS("+proj=longlat +datum=WGS84")  #proj4string(sodo)
    # identify intersections of points in dataset with polygons in grid file
    pts <- over(dat_sub, srdf, duplicate = F)
    pts <- cbind(MainDataTable, pts$ID)
    pts <- as.data.frame(pts)
    colnames(pts)[colnames(pts) == "pts$ID"] <- "ZoneID"
    return(pts)
}


