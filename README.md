FishSET
=========
---

If you run into problems you can contact melanie.harsch@noaa.gov

# Installation #
---
Please note that devtools is required for installation. 

To install:

   install.packages("devtools")
	 library(devtools)
	 install("PATH/TO/Directory/Containing/FishSET")

Directories:
FishSET function call files from the SQLIte database and save log files to a Log folder.
The database and folder are assumed to be in the FishSET package folder. If they are not, 
then the location needs to be specificed.
Use loc to specify directory location of the database and loc2 to specify location of Log file.
For exmaple, loc <- getwd();  loc2 <- paste0(getwd(), '/inst')

# Libraries #
---

The following libaries are imported:
shiny

Functions are imported from the following libraries:
DBI
DT
ff
R.matlab
foreign
geosphere
grDevices
jsonlite
lubridate
rgeos
sf
sp
spatialEco
zoo

