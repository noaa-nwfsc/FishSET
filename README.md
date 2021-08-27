FishSET
=========
---

If you run into problems you can contact FishSET@noaa.gov

<ul class="nav">
  <li><a href="#install">Install</a></li>
  <li><a href="#documentation">Documentation</a></li>
  <li><a href="#bugs">Issues</a></li>
  <li><a href="#cite">Citation</a></li>
  <li><a href="#license">License</a></li>
</ul>

The Spatial Economics Toolbox for Fisheries (FishSET) is a set of tools for organizing data; developing, improving and disseminating modeling best practices.


### INSTALL {#install}
---
Please note that devtools is required for installation. 

To install:

```
   install.packages("devtools")
	 library(devtools)
	 devtools::install_local("PATH/TO/Directory/Containing/FishSET")
```

Directories:
FishSET functions call data files from the SQLite FishSET database and save output (log of function calls, plot output, table output, notes, function messages) to a "Logs" folder and an "output" folder.

The FishSET database and output folders are assumed to be in the FishSET package directory. If they are not, 
then the location needs to be specificed.
Use loc to specify directory location of the database and loc2 to specify location of Log file.
For exmaple, loc <- getwd().

### DOCUMENTATION and TUTORIALS  {#documentation}

### ISSUES and BUG REPORTS {#bugs}

Add in issue tab on github site

Issues? [https://github.com/FishSET/issues]() 
         FishSET@noaa.gov

### CITATION  {#cite}

If you use FishSET results in publications, please cite the the package:

Alan Haynie, Melanie Harsch, Bryce McManus, and Allen Chen (2021). FishSET: Spatial Economics Toolbox for Fisheries. R package version 1.0.1.

### NOAA Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and
Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is
provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of
Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed
by all applicable Federal law. Any reference to specific commercial products, processes, or services by service
mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or
favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a
DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by
DOC or the United States Government.