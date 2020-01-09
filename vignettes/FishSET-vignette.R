## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig1, echo=FALSE, fig.cap="Opening a new R Notebook file", out.width = '1%'----
knitr::include_graphics('C:/Users/melanie.harsch/Pictures/Screenshots/OpeningNotebook.png')

## ----figLog, echo=FALSE, , out.width = '1%'-----------------------------------
knitr::include_graphics('C:/Users/melanie.harsch/Pictures/Screenshots/LogFolder.png')

## ----table1, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'--------
tabl <- "  

| File Type        | Description                                                                 |
|------------------|-----------------------------------------------------------------------------|
| Primary data     | Required file that contains the main data for the model.                    |
| Port data        | Required file with the latitude and longitude locations of ports.           |
| Gridded data     | Optional file that contains additional data that varies by two dimensions.  |
| Auxiliary data   | Optional file that contains additional data to link to the primary data.    |
|------------------|-----------------------------------------------------------------------------|

"
cat(tabl) # output the table in a format good for HTML/PDF/docx conversion

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
knitr::include_graphics('C:/Users/melanie.harsch/Pictures/Screenshots/ModelOutTable.png')

## ----pressure2, echo=FALSE, fig.cap="Example of interactive table output", out.width = '1%'----
knitr::include_graphics('C:/Users/melanie.harsch/Pictures/Screenshots/TableSaved.png')

