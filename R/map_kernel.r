map_kernel <- function(project, type, latlon, group = NULL, facet = FALSE, minmax = NULL) {
    #' Wrapper function to map kernel densities
    #'
    #' Wrapper function to map kernel densities using ggplot2
    #'
    #' @param project Name of project
    #' @param type Type of plot ("point", "contours", "gradient"). Note if you
    #' have a group, you must facet when choosing "gradient" (cannot overlap
    #' polygons clearly).
    #' @param project Name of project.
    #' @param latlon A matrix of (# of observations) x 2 corresponding to 
    #' latitude/longitude pair coordinates in decimal degrees.
    #' @param group Optional group parameter if user maps based on a factor 
    #' group. Should be a factor with length of (# of observations), 
    #' where each observation corresponds to the latlon coordinate of the same 
    #' index. Recall that the legend will output the names of
    #' factor levels as you have named them. (See ?factor).
    #' @param facet Optional facet parameter if user maps each group as a 
    #' separate facet ("TRUE", "FALSE"), default = "FALSE".
    #' @param minmax Optional map extent parameter, a vector (num) of length 4 
    #' corresponding to c(minlat, maxlat, minlon, maxlon).
    #' @return mapout: ggplot2 object
    #' @import ggplot2
    #' @importFrom maps map
    #' @export
    #' @examples
    #' \dontrun{
    #' type <- "contours"
    #' latlon <- as.matrix(cbind(datatomap.2001$lat, datatomap.2001$lon))
    #' group <- datatomap.2001$Vessel
    #' minmax = c(50, max(datatomap.2001$lat,datatomap.2001$lat)*1.001,
    #'  min(datatomap.2001$lon,datatomap.2001$lon)*1.001 , -142.5)
    #' 
    #' map_kernel('project',type,latlon,group=group,minmax=minmax)
    #' 
    #' type <- "gradient"
    #' map_kernel('project',type,latlon,group=group,facet=TRUE,minmax=minmax)
    #' 
    #' type <- "point"
    #' map_kernel('project',type,latlon)
    #' }    
    
## currently outputs to FishSET not file (could include dirout as argument)
requireNamespace('ggplot2')
world <- ggplot2::map_data("world")
  
datatomap <- as.data.frame(latlon)
colnames(datatomap) <- c("lat", "lon")
datatomap <- datatomap[-which(is.na(datatomap$lon)==TRUE|is.na(datatomap$lat)==TRUE),]
datatomap$groupv <- group

if (is.null(minmax) == TRUE) {

if (min(datatomap$lat) < 0 & max(datatomap$lat) < 0) {
    minlat <- min(datatomap$lat)*1.001
    maxlat <- max(datatomap$lat)*0.999
} else if (min(datatomap$lat) > 0 & max(datatomap$lat) > 0) {
    minlat <- min(datatomap$lat,datatomap$lat)*0.999
    maxlat <- max(datatomap$lat,datatomap$lat)*1.001
} else {
    minlat <- min(datatomap$lat,datatomap$lat)*0.999
    maxlat <- max(datatomap$lat,datatomap$lat)*1.001
    warning( "User should specify own minmax lat" )
}

if (min(datatomap$lon) < 0 & max(datatomap$lon) < 0) {
    minlon <- min(datatomap$lon)*1.001
    maxlon <- max(datatomap$lon)*0.999
} else if (min(datatomap$lon) > 0 & max(datatomap$lon) > 0) {
    minlon <- min(datatomap$lon,datatomap$lon)*0.999
    maxlon <- max(datatomap$lon,datatomap$lon)*1.001
} else {
    minlon <- min(datatomap$lon,datatomap$lon)*0.999
    maxlon <- max(datatomap$lon,datatomap$lon)*1.001
    warning("User should specify own minmax lon")
}

} else {

if (length(minmax) == 4) {
    minlat <- minmax[1]
    maxlat <- minmax[2]
    minlon <- minmax[3]
    maxlon <- minmax[4]
} else {

    stop("Variable minmax wrong dimensions")

}

}

################################################################################
if (type == "point") {

if (is.null(group) == FALSE & facet == FALSE) {

mapout <- ggplot2::ggplot() +
  ggplot2::geom_map(data=world, map=world, ggplot2::aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
  ggplot2::geom_point(data=datatomap,ggplot2::aes(x=lon,y=lat, colour = groupv), 
        size=0.375, alpha = 0.25) +
  ggplot2::xlim(minlon,maxlon) + ggplot2::ylim(minlat,maxlat) +
  ggplot2::ggtitle("Points") + 
  ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=10))) +
  ggplot2::theme(text = ggplot2::element_text(size=12), axis.title.y = ggplot2::element_text(vjust=1.5),
        legend.title= ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1)) + 
  ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")

return(mapout)

} else if (is.null(group) == FALSE & facet == TRUE) {

mapout <- ggplot2::ggplot() +
  ggplot2::geom_map(data=world, map=world, ggplot2::aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
  ggplot2::geom_point(data=datatomap, ggplot2::aes(x=lon,y=lat, colour = groupv), size=0.375) +
  ggplot2::xlim(minlon,maxlon) + ggplot2::ylim(minlat,maxlat) +
  ggplot2::ggtitle("Points") +
  ggplot2::facet_wrap(.~groupv) +
  ggplot2::theme(text = ggplot2::element_text(size=12), axis.title.y = ggplot2::element_text(vjust=1.5),
        legend.position = "none", panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1)) + 
  ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")

return(mapout)

} else {

mapout <- ggplot2::ggplot() +
  ggplot2::geom_map(data=world, map=world, ggplot2::aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
  ggplot2::geom_point(data=datatomap, ggplot2::aes(x=lon,y=lat), color="black", size=0.375) +
  ggplot2::xlim(minlon,maxlon) + ggplot2::ylim(minlat,maxlat) +
  ggplot2::ggtitle("Points") +
  ggplot2::theme(text = ggplot2::element_text(size=12), axis.title.y = ggplot2::element_text(vjust=1.5),
        panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
        panel.background = ggplot2::element_blank(), 
        panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1)) + 
  ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")

return(mapout)

}

################################################################################
} else if (type == "contours") {

if (is.null(group) == FALSE & facet == FALSE) {

mapout <- ggplot2::ggplot() +
  ggplot2::geom_map(data=world, map=world, ggplot2::aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
  ggplot2::geom_point(data=datatomap, ggplot2::aes(x=lon,y=lat, colour = groupv), 
        color="black", size=0.375) +
  ggplot2::geom_density_2d(data=datatomap, ggplot2::aes(x=lon,y=lat, colour = groupv)) +
  ggplot2::xlim(minlon,maxlon) + ggplot2::ylim(minlat,maxlat)+
  ggplot2::ggtitle("Spatial kernel (contours)") +
  ggplot2::theme(text = ggplot2::element_text(size=12), axis.title.y = ggplot2::element_text(vjust=1.5),
        legend.title = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1)) + 
  ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")

return(mapout)

} else if (is.null(group) == FALSE & facet == TRUE) {

mapout <- ggplot2::ggplot() +
  ggplot2::geom_map(data=world, map=world, ggplot2::aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
  ggplot2::geom_point(data=datatomap, ggplot2::aes(x=lon,y=lat, colour = groupv), 
        color="black", size=0.375) +
  ggplot2::geom_density_2d(data=datatomap, ggplot2::aes(x=lon,y=lat, colour = groupv)) +
  ggplot2::xlim(minlon,maxlon) + ggplot2::ylim(minlat,maxlat)+
  ggplot2::ggtitle("Spatial kernel (contours)") +
  ggplot2::facet_wrap(.~groupv) +
  ggplot2::theme(text = ggplot2::element_text(size=12), axis.title.y = ggplot2::element_text(vjust=1.5),
        legend.position = "none", panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1)) + 
  ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")

return(mapout)

} else {

mapout <- ggplot2::ggplot() +
  ggplot2::geom_map(data=world, map=world, ggplot2::aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
  ggplot2::geom_point(data=datatomap, ggplot2::aes(x=lon,y=lat), color="black", size=0.375) +
  ggplot2::geom_density_2d(data=datatomap, ggplot2::aes(x=lon,y=lat)) +
  ggplot2::xlim(minlon,maxlon) + ggplot2::ylim(minlat,maxlat)+
  ggplot2::ggtitle("Spatial kernel (contours)") +
  ggplot2::theme(text = ggplot2::element_text(size=12), axis.title.y = ggplot2::element_text(vjust=1.5),
        legend.title = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1)) + 
  ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")

return(mapout)

}

################################################################################
} else if (type == "gradient") { 

if (is.null(group) == FALSE & facet == FALSE) {

## can't overlap gradient (use alpha?)

stop("Cannot overlap gradients please facet or use contours")

} else if (is.null(group) == FALSE & facet == TRUE) {

mapout <- ggplot2::ggplot() +
  ggplot2::geom_map(data=world, map=world, ggplot2::aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
  ggplot2::geom_point(data=datatomap, ggplot2::aes(x=lon,y=lat, colour = groupv), color="black", 
        size=0.375) +
  ggplot2::stat_density_2d(data=datatomap, ggplot2::aes(x=lon,y=lat, fill = stat(level)), 
        geom = "polygon")+
  ggplot2::xlim(minlon,maxlon) + ggplot2::ylim(minlat,maxlat)+
  ggplot2::facet_wrap(.~groupv) +
  ggplot2::scale_fill_gradient(name="Level\n(density)") +
  ggplot2::ggtitle("Spatial kernel (gradient)") +
  ggplot2::theme(text = ggplot2::element_text(size=12), axis.title.y = ggplot2::element_text(vjust=1.5),
      panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1)) + 
  ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")

return(mapout)

} else {

mapout <- ggplot2::ggplot() +
  ggplot2::geom_map(data=world, map=world, ggplot2::aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
  ggplot2::geom_point(data=datatomap, ggplot2::aes(x=lon,y=lat), color="black", size=0.375) +
  ggplot2::stat_density_2d(data=datatomap, ggplot2::aes(x=lon,y=lat, fill = stat(level)), 
        geom = "polygon")+
  ggplot2::xlim(minlon,maxlon) + ggplot2::ylim(minlat,maxlat)+
  ggplot2::scale_fill_gradient(name="Level\n(density)") +
  ggplot2::ggtitle("Spatial kernel (gradient)") +
  ggplot2::theme(text = ggplot2::element_text(size=12), axis.title.y = ggplot2::element_text(vjust=1.5),
      panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1) ) + 
  ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")

return(mapout)

}

}

save_plot(project, "map_kernel", mapout)

}
