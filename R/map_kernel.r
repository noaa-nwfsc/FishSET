map_kernel <- function(type, latlon, group = NULL, facet = FALSE, minmax = NULL) {
    #' Wrapper function to map kernel densities
    #'
    #' Wrapper function to map kernel densities using ggplot2
    #'
    #' @param type Type of plot ("point", "contours", "gradient"). Note if you
    #' have a group, you must facet when choosing "gradient" (cannot overlap
    #' polygons clearly).
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
    #' map_kernel(type,latlon,group=group,minmax=minmax)
    #' 
    #' type <- "gradient"
    #' map_kernel(type,latlon,group=group,facet=TRUE,minmax=minmax)
    #' 
    #' type <- "point"
    #' map_kernel(type,latlon)
    #' }    
    
## currently outputs to FishSET not file (could include dirout as argument)
requireNamespace('ggplot2')
world <- map_data("world")
  
datatomap <- as.data.frame(latlon)
colnames(datatomap) <- c("lat", "lon")
datatomap$groupv <- group

if (is.null(minmax) == TRUE) {

if (min(datatomap$lat < 0) & max(datatomap$lat) < 0) {
    minlat <- min(datatomap$lat)*1.001
    maxlat <- max(datatomap$lat)*0.999
} else if (min(datatomap$lat > 0) & max(datatomap$lat) > 0) {
    minlat <- min(datatomap$lat,datatomap$lat)*0.999
    maxlat <- max(datatomap$lat,datatomap$lat)*1.001
} else {
    minlat <- min(datatomap$lat,datatomap$lat)*0.999
    maxlat <- max(datatomap$lat,datatomap$lat)*1.001
    warning( "User should specify own minmax lat" )
}

if (min(datatomap$lon < 0) & max(datatomap$lon) < 0) {
    minlon <- min(datatomap$lon)*1.001
    maxlon <- max(datatomap$lon)*0.999
} else if (min(datatomap$lon > 0) & max(datatomap$lon) > 0) {
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

mapout <- ggplot() +
    geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
    geom_point(data=datatomap,aes(x=lon,y=lat, colour = groupv), 
        size=0.375, alpha = 0.25) +
    xlim(minlon,maxlon) + ylim(minlat,maxlat) +
    ggtitle("Points") + 
    guides(colour = guide_legend(override.aes = list(size=10))) +
    theme(text = element_text(size=12), axis.title.y=element_text(vjust=1.5),
        legend.title=element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
        xlab("Longitude") + ylab("Latitude")

return(mapout)

} else if (is.null(group) == FALSE & facet == TRUE) {

mapout <- ggplot() +
    geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
    geom_point(data=datatomap,aes(x=lon,y=lat, colour = groupv), size=0.375) +
    xlim(minlon,maxlon) + ylim(minlat,maxlat) +
    ggtitle("Points") +
    facet_wrap(.~groupv) +
    theme(text = element_text(size=12), axis.title.y=element_text(vjust=1.5),
        legend.position = "none", panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
        xlab("Longitude") + ylab("Latitude")

return(mapout)

} else {

mapout <- ggplot() +
    geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
    geom_point(data=datatomap,aes(x=lon,y=lat), color="black", size=0.375) +
    xlim(minlon,maxlon) + ylim(minlat,maxlat) +
    ggtitle("Points") +
    theme(text = element_text(size=12), axis.title.y=element_text(vjust=1.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
        xlab("Longitude") + ylab("Latitude")

return(mapout)

}

################################################################################
} else if (type == "contours") {

if (is.null(group) == FALSE & facet == FALSE) {

mapout <- ggplot() +
    geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
    geom_point(data=datatomap,aes(x=lon,y=lat, colour = groupv), 
        color="black", size=0.375) +
    geom_density_2d(data=datatomap,aes(x=lon,y=lat, colour = groupv)) +
    xlim(minlon,maxlon) + ylim(minlat,maxlat)+
    ggtitle("Spatial kernel (contours)") +
    theme(text = element_text(size=12), axis.title.y=element_text(vjust=1.5),
        legend.title=element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
        xlab("Longitude") + ylab("Latitude")

return(mapout)

} else if (is.null(group) == FALSE & facet == TRUE) {

mapout <- ggplot() +
    geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
    geom_point(data=datatomap,aes(x=lon,y=lat, colour = groupv), 
        color="black", size=0.375) +
    geom_density_2d(data=datatomap,aes(x=lon,y=lat, colour = groupv)) +
    xlim(minlon,maxlon) + ylim(minlat,maxlat)+
    ggtitle("Spatial kernel (contours)") +
    facet_wrap(.~groupv) +
    theme(text = element_text(size=12), axis.title.y=element_text(vjust=1.5),
        legend.position = "none", panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
        xlab("Longitude") + ylab("Latitude")

return(mapout)

} else {

mapout <- ggplot() +
    geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
    geom_point(data=datatomap,aes(x=lon,y=lat), color="black", size=0.375) +
    geom_density_2d(data=datatomap,aes(x=lon,y=lat)) +
    xlim(minlon,maxlon) + ylim(minlat,maxlat)+
    ggtitle("Spatial kernel (contours)") +
    theme(text = element_text(size=12), axis.title.y=element_text(vjust=1.5),
        legend.title=element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
        xlab("Longitude") + ylab("Latitude")

return(mapout)

}

################################################################################
} else if (type == "gradient") { 

if (is.null(group) == FALSE & facet == FALSE) {

## can't overlap gradient (use alpha?)

stop("Cannot overlap gradients please facet or use contours")

} else if (is.null(group) == FALSE & facet == TRUE) {

mapout <- ggplot() +
    geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
    geom_point(data=datatomap,aes(x=lon,y=lat, colour = groupv), color="black", 
        size=0.375) +
    stat_density_2d(data=datatomap, aes(x=lon,y=lat, fill = stat(level)), 
        geom = "polygon")+
    xlim(minlon,maxlon) + ylim(minlat,maxlat)+
    facet_wrap(.~groupv) +
    scale_fill_gradient(name="Level\n(density)") +
    ggtitle("Spatial kernel (gradient)") +
    theme(text = element_text(size=12), axis.title.y=element_text(vjust=1.5),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
      xlab("Longitude") + ylab("Latitude")

return(mapout)

} else {

mapout <- ggplot() +
    geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
    geom_point(data=datatomap,aes(x=lon,y=lat), color="black", size=0.375) +
    stat_density_2d(data=datatomap, aes(x=lon,y=lat, fill = stat(level)), 
        geom = "polygon")+
    xlim(minlon,maxlon) + ylim(minlat,maxlat)+
    scale_fill_gradient(name="Level\n(density)") +
    ggtitle("Spatial kernel (gradient)") +
    theme(text = element_text(size=12), axis.title.y=element_text(vjust=1.5),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=1) ) + 
      xlab("Longitude") + ylab("Latitude")

return(mapout)

}

}

save_plot(project, "map_kernel", mapout)

}
