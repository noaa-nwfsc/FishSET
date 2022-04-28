## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----table1, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'--------
tabl <- "  

| File Type        | Description                                                         |
|------------------|---------------------------------------------------------------------|
| Primary data     | Required. Contains the main data for the model.                     |
| Port data        | Required. Contains latitude and longitude locations of ports.       |
| Gridded data     | Optional. Contains additional data that varies by two dimensions.   |
| Auxiliary data   | Optional. Contains additional data to link to the primary data.     |
| Spatial data     | Required. File defining boundaries of fishery or regulatory zones.  |
"
cat(tabl) # output the table in a format good for HTML/PDF/docx conversion

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  ##Load in the data from the pollock project
#  library(FishSET)
#  load_data("pollock")

## -----------------------------------------------------------------------------
#data_check(pollockMainDataTable, 'pollock', 'OFFICIAL_TOTAL_CATH_MT')

## ----table2, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'--------
tabl <- '   
| Data table      |   Vector       |  FilterFunction           |
|-----------------|----------------|---------------------------|
| MainDataTable   |  "PORT_CODE"   |  PORT_CODE == 1           |
| MainDataTable   |  "TRIP_START"  |  TRIP_START >= 2011-02-01 |
| PortDataTable   |  "LATITUDE"    |  LATITUDE < 57            |

'
cat(tabl) # output the table in a format good for HTML/PDF/docx conversion

## ----pressure, echo=FALSE, fig.cap="Example of interactive table", out.width = '1%'----
# knitr::include_graphics('C:/Users/melanie.harsch/Pictures/Screenshots/ModelOutTable.png')

