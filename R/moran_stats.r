moran_stats <- function(datatomap) {
    #' Wrapper function to calculate Moran's I
    #'
    #' Wrapper function to calculate global and local Moran's I by discrete area
    #'
    #' @param datatomap A data frame with the ADFG Stat6 area as a factor
    #'     "ADFGstat6", the lon/lat centroid for each area ("centroid_lon",
    #'     "centroidlat"), the path_lon/path_lat outlining each area
    #'     ("path_lon", "path_lat"), and the variable of interest ("varofint").
    #' @return moranmap: ggplot2 object; morantable: table of statistics
    #' @import ggplot2
    #' @import spdep
    #' @importFrom maps map
    #' @export
    #' @examples
    #' \dontrun{
    #' datatomap <- merge(subadfgstat6, varofint, by = c("ADFGstat6"), 
    #'     all.x = TRUE, all.y = TRUE)
    #' test <- test
    #' }
    #'

# library(ape)
library(spdep)
library(ggplot2)
world <- map_data("world")

uniquedatatomap <- datatomap[!duplicated(datatomap$ADFGstat6),
    c("ADFGstat6", "centroid_lon","centroid_lat", "varofint")]
    
# adfg.dists <- as.matrix(dist(as.matrix(
    # uniquedatatomap[,c("centroid_lon","centroid_lat")])))
# adfg.dists.inv <- 1/adfg.dists
# diag(adfg.dists.inv) <- 0
# gmoran <- Moran.I(uniquedatatomap$varofint, adfg.dists.inv)

nb.rk <- knn2nb(knearneigh(as.matrix(uniquedatatomap[,c("centroid_lon",
    "centroid_lat")]), longlat=TRUE))
locm <- localmoran(uniquedatatomap$varofint, listw = nb2listw(nb.rk))

uniquedatatomap$Moran <- locm[,1]

gmoranspdep <- moran.test(uniquedatatomap$varofint, listw = nb2listw(nb.rk))

datatomap <- merge(datatomap, uniquedatatomap, by = c("ADFGstat6", 
    "centroid_lon", "centroid_lat", "varofint"))

minlon = min(adfgstat6$path_lon)*1.001 #lon negative
maxlon = max(adfgstat6$path_lon)*0.985
minlat = min(adfgstat6$path_lat)*0.992
maxlat = max(adfgstat6$path_lat)*1.001

annotatesize <- 6

moranmap <- ggplot(data=datatomap) +
    geom_path(aes(x=path_lon,y=path_lat,group = ADFGstat6), color="black", 
        size=0.375) + 
    geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
    geom_polygon(data=datatomap,aes(x=path_lon,y=path_lat,group = ADFGstat6,
    fill=Moran,),color="black",alpha=1,size=0.375) + 
    scale_fill_gradient2(low="skyblue2", high="firebrick1", 
        mid = "white", name="Local\nMoran's I")+
    xlim(minlon,maxlon) + 
    ylim(minlat,maxlat) + 
    ggtitle("Moran's I statistics") +
    annotate(geom='text', x = min(adfgstat6$path_lon)*0.9915, 
        y = min(adfgstat6$path_lat)*0.997,
        label=paste0("Global Moran's I = ", round(gmoranspdep$estimate[1],2)),
        parse=FALSE, size = annotatesize, color = "black", hjust = 0) +
    annotate(geom='text', x = min(adfgstat6$path_lon)*0.9915, 
        y = min(adfgstat6$path_lat)*0.994,
        label=paste0("p-value = ", round(gmoranspdep$p.value,2)),
        parse=FALSE, size = annotatesize, color = "black", hjust = 0) +
    theme(text = element_text(size=20), axis.title.y=element_text(vjust=1.5),
    legend.position = c(0.875, 0.7), legend.text=element_text(size=15),
    legend.title=element_text(size=15)) + 
    xlab("Longitude") + 
    ylab("Latitude")
    
return(list(moranmap = moranmap, 
    morantable = uniquedatatomap[,c("ADFGstat6", "Moran")]))

}
