#' Northeast Scallop Data
#' 
#' A subset of anonymized scallop data
#' 
#' @format `scallop`
#' A data.frame with 10,000 rows and 19 columns:
#' \describe{
#'   \item{TRIPID}{Randomly assigned trip ID number.}
#'   \item{DATE_TRIP}{Date of landing.}
#'   \item{PERMIT.y}{Randomly assigned six-digit vessel fishing permit number.}
#'   \item{TRIP_LENGTH}{Days calculated from the elapsed time between the 
#'   date-time sailed and date-time landed; this is a measure of days absent.}
#'   \item{GEARCODE}{Fishing gear used on the trip.}
#'   \item{port_lat}{Latitude of the geoid.}
#'   \item{port_lon}{longitude of the geoid.}
#'   \item{previous_port_lat}{Previous latitude of geoid.}
#'   \item{previous_port_lon}{Previous longitude of geoid.}
#'   \item{Plan Code}{Portion of the VMS declaration code that identifies the 
#'   fishery being declared into for the trip.}
#'   \item{Program Code}{Portion of the VMS declaration code that identifies the 
#'   program within the declared fishery. For scallops, the program code delineates 
#'   LA and LAGC trips, as well as access area trips from other trips.}
#'   \item{TRIP_COST_WINSOR_2020_DOL}{The estimated or real composite trip cost 
#'   for the VTR trip record generated using the methods described in the 
#'   Commercial Trip Cost Estimation 2007-2019 PDF file. However, these values 
#'   have been Winsorized by gear type as a method of avoiding unreasonably high 
#'   or low trip costs, replacing any value within each gear-group that is less 
#'   than the 1st percentile or greater than the 99th percentile with the 1st and 
#'   99th percentile value, respectively.}
#'   \item{DDLAT}{The latitude reported on a VTR (Vessel Trip Reports).}
#'   \item{DDLON}{The longitude reported on a VTR (Vessel Trip Reports).}
#'   \item{ZoneID}{FishSET's version of a ten minute square.}
#'   \item{LANDED_OBSCURED}{Landed pounds from the dealer report (jittered/obscured).}
#'   \item{DOLLAR_OBSCURED}{The value of catch paid by the dealer, from the 
#'   dealer report (jittered/obscured).}
#'   \item{DOLLAR_2020_OBSCURED}{The value of catch paid by the dealer, from the 
#'   dealer report (in 2020 dollars, jittered/obscured).}
#'   \item{DOLLAR_ALL_SP_2020_OBSCURED}{The value of catch for all species caught
#'    (in 2020 dollars, jittered/obscured).}
#' }
#' @source Add source here
"scallop"


#' Ports from the NE scallop fishery
#' 
#' @description
#'  A dataset containing the names and lat/lon coordinates of ports used in the US northeast scallop fishery.
#' 
#' @usage scallop_ports
#' 
#' @format A data frame (tibble) with 40 observations and 3 variables.\cr
#' \cr
#' [,1] Port names \cr
#' [,2] Longitude \cr
#' [,3] Latitude \cr
#' 
#' @source NEED TO ADD SOURCE DESCRIPTION
"scallop_ports"


#' Northeast Ten Minute Squares
#' 
#' @format `tenMNSQR`
#' A simple feature COLLECTION with 5267 features and 9 fields:
#' \describe{
#'   \item{AREA}{}
#'   \item{PERIMETER}{}
#'   \item{TEN_}{}
#'   \item{TEN_ID}{}
#'   \item{LL}{}
#'   \item{LAT}{}
#'   \item{LON}{}
#'   \item{TEMP}{}
#'   \item{LOC}{}
#' }
#' 
"tenMNSQR"

#' Northeast wind closure areas
#' 
#' 
#' @format `windLease`
#' Simple features collection with 32 features and 1 field:
#' \describe{
#'   \item{NAME}{Name of wind lease.}
#' }
#' 
"windLease"