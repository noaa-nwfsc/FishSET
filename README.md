FishSET
=========
---

If you run into problems you can contact FishSET@noaa.gov

# Installation #
---
Please note that devtools is required for installation. 

To install:

   install.packages("devtools")
	 library(devtools)
	 devtools::install_local("PATH/TO/Directory/Containing/FishSET")

Directories:
FishSET functions call data files from the SQLite FishSET database and save output (log of function calls, plot output, table output, notes, function messages) to a "Logs" folder and an "output" folder.

The FishSET database and output folders are assumed to be in the FishSET package directory. If they are not, 
then the location needs to be specificed.
Use loc to specify directory location of the database and loc2 to specify location of Log file.
For exmaple, loc <- getwd().

# Libraries #
---

The following libaries are called and will be imported if not already installed:
DBI 
dplyr
DT
ff
foreign
formattable
geojsonio
geosphere
ggcorrplot
ggmap
ggplot2
ggpubr
gridExtra
haven
jsonlite
knitr
lubridate
magrittr
maps
methods
openxlsx
OSMscale
pander
raster
purrr
RCurl
readxl
reshape2
rgeos
rlang
R.matlab
RSQLite
scales
servr
sf
shiny
shinyjs
shinycssloaders
signal
sp
spdep
stringi
stringr
tibbletidyr
XML
zoo



  




