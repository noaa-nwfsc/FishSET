library(R.matlab)
library(RSQLite)
library(DBI)


#Load data
#1) Create database
testdb <- dbConnect(RSQLite::SQLite(), "")

#2) Import .mat file
## Port Data
portTable <- readMat('C:/Users/Melanie/Documents/FishSET/fishset-matlab codes/port_AKFIN.mat')
print(portTable)

# convert .mat table into dataframe
PortTable <- data.frame(as.factor(unlist(portTable$portData[[1]], recursive = TRUE, use.names = FALSE)),
                        as.factor(c(unlist(portTable$portData[[4]], recursive = TRUE, use.names = FALSE)[1:24],NA,
                                    unlist(portTable$portData[[4]], recursive = TRUE, use.names = FALSE)[25:121]
                                    )),
                        as.matrix(portTable$portData[[2]][,1]),
                        as.matrix(portTable$portData[[2]][,2]),
                        rep(NA, 122))
colnames(PortTable)=c('Port_Name','Port_ID','Port_Long','Port_Lat', 'Port_Notes')
#save dataframe to database
dbWriteTable(testdb,'PortTable', PortTable)


# MainDataTable
MainDataTable <- load('Data/MainDataTable.RData')
names(MainDataTable)
# convert .mat table into dataframe
#save dataframe to database
dbWriteTable(testdb,'MainDataTable', MainDataTable)



#MainDataTableInfo
MainDataTableInfo <- data.frame(variable_name=colnames(MainDataTable), 
                               units=c(ifelse(grepl('DATE|TRIP_END|TRIP_START',colnames(MainDataTable)), 'yyyymmdd', 
                                             ifelse(grepl('MIN',colnames(MainDataTable)), 'min',
                                                    ifelse(grepl('FATHOMS',colnames(MainDataTable)), 'fathoms',
                                                           ifelse(grepl('HOURS|CHINOOK|CHUM|PROPORTION|SIZE', colnames(MainDataTable)), 'numeric',
                                                                  ifelse(grepl('DOLLARS',colnames(MainDataTable)), 'dollars',
                                                                         ifelse(grepl('POUNDS|LBS',colnames(MainDataTable)), 'lbs',
                                                                                ifelse(grepl('Lon|Lat|LON|LAT',colnames(MainDataTable)), 'decimal degrees',
                                                                                       ifelse(grepl('PERCENT',colnames(MainDataTable)), 'percent',
                                                                                              ifelse(grepl('MT',colnames(MainDataTable)), 'metric tons',
                                                                                                     ifelse(grepl('WEEK',colnames(MainDataTable)), 'WK',
                                                                                                            ifelse(grepl('WEEK',colnames(MainDataTable)), 'Y/N',NA
                                                                                                            )))))))))))),
                               generalType=c(ifelse(grepl('DATE|MIN',colnames(MainDataTable)), 'Time',
                                                    ifelse(grepl('IFQ',colnames(MainDataTable)), 'Flag',
                                                           ifelse(grepl('ID',colnames(MainDataTable)), 'Code',
                                                                  ifelse(grepl('Long|Lat',colnames(MainDataTable)), 'Latitude',
                                                                         ifelse(grepl('TYPE|PROCESSOR|LOCATION|METHOD',colnames(MainDataTable)), 'Code String',
                                                                                ifelse(grepl('CHINOOK|CHUM|FATHOMS|DOLLARS|LBS|PROPORTION|VALUE|PERCENT|MT',colnames(MainDataTable)), 'Other Numeric',
                                                                                       ifelse(grepl('HAUL|AREA|PERFORMANCE|PERMIT',colnames(MainDataTable)), 'Code Numeric', NA)
                                                                                       ))))))),
                               isXY=ifelse(grepl('HOURS|CHINOOK|CHUM|PROPORTION|SIZE', colnames(MainDataTable)), 1,0),
                               isID=ifelse(grepl('ID', colnames(MainDataTable)), 1,0),
                               variable_link=rep(NA, length(colnames(MainDataTable))),
                               isTime=ifelse(grepl('DATE|MIN', colnames(MainDataTable)), 1,0),
                               isCatch=ifelse(grepl('CATCH|POUNDS|LBS', colnames(MainDataTable)), 1,0),
                               isEffort=ifelse(grepl('DURATION',colnames(MainDataTable)), 1,0),
                               isCPUE=rep(0, length(colnames(MainDataTable))),
                               isLon=ifelse(grepl('LON',colnames(MainDataTable)), 1,0), 
                               isLat=ifelse(grepl('LAT',colnames(MainDataTable)), 1,0),
                               isValue=ifelse(grepl('DOLLARS',colnames(MainDataTable)), 1,0),
                               isZoneArea=ifelse(grepl('AREA',colnames(MainDataTable)), 1,0),
                               isPort=ifelse(grepl('PORT', colnames(MainDataTable)), 1,0),
                               isPrice= rep(0, length(colnames(MainDataTable))),
                               isTrip=ifelse(grepl('TRIP', colnames(MainDataTable)), 1,0),
                               isHaul=ifelse(grepl('HAUL', colnames(MainDataTable)), 1,0),
                               isOther=rep(0, length(colnames(MainDataTable))),
                               tableLink=rep(NA, length(colnames(MainDataTable))))
dbWriteTable(testdb,'MainDataTableInfo', MainDataTableInfo)
