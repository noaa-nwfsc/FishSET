#' Sea Surface Temperature data
#' 
#' Remote Sensing Systems (\url{http://www.remss.com}) sea surface temperature (SST). 
#' The data includes one week of SST for Bering Sea / Aleutian Islands, 2011.
#' 
#' @docType data
#' 
#' @format A data.frame with 7203 rows and 5 variables:
#' \describe{
#' \item{time}{Date and time temperature was captured}
#' \item{lat}{Latitude}
#' \item{lon}{Longitude}
#' \item{analysed_sst}{analysed sea surface temperature}
#' \item{sea_ice_fraction}{sea ice area fraction}
#' }
#' @source \url{https://upwell.pfeg.noaa.gov/erddap/info/nasa_jpl_844f_dc54_9c41/index.html}
"SST"