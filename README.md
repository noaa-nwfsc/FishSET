# FishSET  <img src="man/figures/logo.png" align="right" align="right" width="20%"  hspace="0" vspace="0"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/noaa-nwfsc/FishSET/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/noaa-nwfsc/FishSET/actions/workflows/R-CMD-check.yaml)
[![gitleaks](https://github.com/noaa-nwfsc/FishSET/actions/workflows/secretScan.yml/badge.svg)](https://github.com/noaa-nwfsc/FishSET/actions/workflows/secretScan.yml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Contact nmfs.fishset@noaa.gov with any questions regarding the FishSET R package and to report issues.

## Overview
The Spatial Economics Toolbox for Fisheries (FishSET) is a set of tools developed as an R package for organizing and visualizing data; developing, improving and disseminating modeling best practices; and simulating policy scenarios to explore the welfare consequences of management decisions. 

## GitHub Install
Currently, the FishSET repo is only available internally and access to the NOAA GHEC organization is required for installation. We plan on making this repo public soon.

Run the following lines of code in R:

```
# Set configuration
usethis::use_git_config(user.name = "INSERT GITHUB USER NAME", user.email = "INSERT EMAIL")

# This line will take you to GitHub to generate a personal access token (PAT).
# IMPORTANT: Copy and save the PAT once it is generated. Once you leave the GitHub page you will not be able to view the PAT.
usethis::create_github_token()

# Paste your PAT here
credentials::set_github_pat("PASTE PAT HERE")

# Install the package (see troubleshooting section below if this doesn't work)
install.packages("pak")

pak::pak("noaa-nwfsc/FishSET")
```
 

## Local Install
The team is phasing out local installs, but the team will provide for a local install if a user cannot install through GitHub.

To install, run the following lines in R (remember to replace path in install_local() function to the actual file path on your computer):
```
install.packages("devtools")
library(devtools)
devtools::install_local("PATH/TO/Directory/Containing/FishSET")
```

Directories:
FishSET functions call data files from the SQLite FishSET database and save output (log of function calls, plot output, table output, notes, function messages) to a "Logs" folder and an "output" folder.

The FishSET database and output folders are assumed to be in the FishSET package directory. If they are not, 
then the location needs to be specified.
Use loc to specify directory location of the database and loc2 to specify location of Log file.
For example, loc <- getwd().


## Documentation and Tutorials

Refer to the [FishSET R Package User Manual](https://docs.google.com/document/d/1p8mK65uG8yp-HbzCeBgtO0q6DSpKV1Zyk_ucNskt5ug/edit) for more package information, quickstart guides, and troubleshooting tips.

## Issues and Bug Reports

Add issues in GitHub [https://github.com/noaa-nwfsc/FishSET/issues](https://github.com/noaa-nwfsc/FishSET/issues). Or contact [FishSET@noaa.gov](mailto:FishSET@noaa.gov).

## <a name="cite"> Citation </a>

If you use FishSET results in publications, please cite the the package:

Alan Haynie, Melanie Harsch, Bryce McManus, Allen Chen, Min-Yang Lee, Anna Abelman, Paul Carvalho, Lisa Pfeiffer (2024). FishSET: Spatial Economics Toolbox for Fisheries. R package version 1.0.1.

## Troubleshooting
<details><summary>Error in utils::download.file(url, path, method = method, quiet = quiet...</summary>
Run the following line of code, then run remotes::install_github
  
```options(download.file.method = "wininet")```
</details> 

<details><summary>Error in dyn.load(file, DLLpath = DLLpath, ...): unable to load shared object ... </summary>
This error message indicates that the filepath to a necessary package is 'corrupted' and cannot load properly. To fix this issue, reinstall the package indicated in the error message using `install.packages([Name of package])` and restart the R session. If the issue persists, try uninstalling and reinstalling R/RStudio. If both options fail, report the[issue](https://gitlab-afsc.fisheries.noaa.gov/bryce.mcmanus/FishSET_RPackage/-/issues).
</details> 

<details><summary>Error: failed to lock directory...</summary>
This error could appear when your last package installation was interrupted, when updated you version of R, and probably other situations that we are not aware of.  

1. Locate and delete the ".../00LOCK-[packagename]" and "[packagename]" folders in the library folder, which should be displayed with the error message (this can also be done using the unlink() function in R), then attempt to reinstall the problem package using install.packages(). If FishSET is the problem package, follow the steps above to install again.

2. If the first options does not work, try adding "--no-lock" to your install options: "install.packages(INSTALL_opts = '--no-lock')" 
</details> 

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## License
This content was created by U.S. Government employees as part of their official duties. This content is not subject to copyright in the United States (17 U.S.C. §105) and is in the public domain within the United States of America. Additionally, copyright is waived worldwide through the MIT License.

<img src="https://raw.githubusercontent.com/nmfs-fish-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" width="200" style="height: 75px !important;"   alt="NOAA Fisheries Logo">


 [U.S. Department of Commerce](https://www.commerce.gov/) | [National Oceanographic and Atmospheric Administration](https://www.noaa.gov) | [NOAA Fisheries](https://www.fisheries.noaa.gov/)

