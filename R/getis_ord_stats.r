getis_ord_stats <- function(datatomap) {
    #' Wrapper function to calculate Getis-Ord statistics
    #'
    #' Wrapper function to calculate global and local Getis-Ord by discrete area
    #'
    #' @param datatomap A data frame with the ADFG Stat6 area as a factor
    #'     "ADFGstat6", the lon/lat centroid for each area ("centroid_lon",
    #'     "centroidlat"), the path_lon/path_lat outlining each area
    #'     ("path_lon", "path_lat"), and the variable of interest ("varofint").
    #' @return moranmap: ggplot2 object; morantable: table of statistics
    #' @import ggplot2
    #' @importFrom maps map
    #' @importFrom spdep knn2nb knearneigh nb2listw localG globalG.test
    #' @export
    #' @examples
    #' \dontrun{
    #' names(datatomap)[1] <- "path_lon"
    #' names(datatomap)[2] <- "path_lat"
    #' names(datatomap)[3] <- "centroid_lon"
    #' names(datatomap)[4] <- "centroid_lat"
    #' names(datatomap)[5] <- "ADFGstat6"
    #' names(datatomap)[5] <- "varofint"
    #' getis_ord_stats(datatomap)
    #' }
    #'

requireNamespace(ggplot2)
world <- map_data("world")

uniquedatatomap <- datatomap[!duplicated(datatomap$ADFGstat6),
    c("ADFGstat6", "centroid_lon","centroid_lat", "varofint")]
    
nb.rk <- knn2nb(knearneigh(as.matrix(uniquedatatomap[,c("centroid_lon",
    "centroid_lat")]), longlat=TRUE))
locg <- localG(uniquedatatomap$varofint, listw = nb2listw(nb.rk))

uniquedatatomap$GetisOrd <- locg

globalgetis <- globalG.test(uniquedatatomap$varofint, 
    listw = nb2listw(nb.rk, style="B"))

datatomap <- merge(datatomap, uniquedatatomap, by = c("ADFGstat6", 
    "centroid_lon", "centroid_lat", "varofint"))

minlon = min(adfgstat6$path_lon)*1.001 #lon negative
maxlon = max(adfgstat6$path_lon)*0.985
minlat = min(adfgstat6$path_lat)*0.992
maxlat = max(adfgstat6$path_lat)*1.001

annotatesize <- 6

getismap <- ggplot(data=datatomap) +
    geom_path(aes(x=path_lon,y=path_lat,group = ADFGstat6), color="black", 
        size=0.375) + 
    geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region),
        fill="grey", color="black", size=0.375) +
    geom_polygon(data=datatomap,aes(x=path_lon,y=path_lat,group = ADFGstat6,
    fill=GetisOrd,),color="black",alpha=1,size=0.375) + 
    scale_fill_gradient2(low="skyblue2", high="firebrick1", 
        mid = "white", name="Local\nGetis-Ord") +
    xlim(minlon,maxlon) + 
    ylim(minlat,maxlat) + 
    ggtitle("Getis-Ord statistics") +
    annotate(geom='text', x = min(adfgstat6$path_lon)*0.9915, 
        y = min(adfgstat6$path_lat)*0.997,
        label=paste0("Global Getis-Ord = ", round(globalgetis$estimate[1],2)),
        parse=FALSE, size = annotatesize, color = "black", hjust = 0) +
    annotate(geom='text', x = min(adfgstat6$path_lon)*0.9915, 
        y = min(adfgstat6$path_lat)*0.994,
        label=paste0("p-value = ", round(globalgetis$p.value,2)),
        parse=FALSE, size = annotatesize, color = "black", hjust = 0) +
    theme(text = element_text(size=20), axis.title.y=element_text(vjust=1.5),
    legend.position = c(0.875, 0.7), legend.text=element_text(size=15),
    legend.title=element_text(size=15)) + 
    xlab("Longitude") + 
    ylab("Latitude")
    
return(list(getismap = getismap, 
    getistable = uniquedatatomap[,c("ADFGstat6", "GetisOrd")]))

}
