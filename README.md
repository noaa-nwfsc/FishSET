FishSET
=========
---

If you run into problems you can contact [FishSET@noaa.gov](mailto:FishSET@noaa.gov)

<ul class="nav">
  <li>[Local Install](#LocalInstall)
  <li>[Gitlab Install](#GitlabInstall)
  <li>[Documentation](#Documentation)
  <li>[Issues](#bugs)
  <li>[Citation](#cite)
  <li>[License](#license)
</ul>

The Spatial Economics Toolbox for Fisheries (FishSET) is a set of tools for organizing data; developing, improving and disseminating modeling best practices.


## <a name="LocalInstall"> Local Install </a>
---
Please note that devtools is required for a local installation. 

To install:

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


## <a name="GitlabInstall"> Gitlab Install </a>

1.  You will need a Gitlab account and you'll need an access token.
2.  To get an access token:
    1.  Log into gitlab. Click the little down arrow next you your profile (top right).
    2.  Click the “access tokens” on the left hand side. 
    3.  Fill in a token name, give it all the scopes and click “create personal access token.”  It’s good to have an expiration date.  But a bit of a pain.
    4.  Copy and paste that token into notepad. It should start with the letters “glpat”.
3.  Now, you need to save the Personal Access Token as the Environment Variable “GITLAB_PAT”. One way to do this is to ask you IT person to do this for you. Another way to do it is to have R do it for you.



### Tokens, Method 1. 
Run this code in R
```
Sys.setenv(GITLAB_PAT="glpat-the rest of your pat here")
```
Tokens are mysterious creatures. Your token may not persist the next time you use R.

### Tokens, Method 2. 

You can set your token in your .Rprofile. 

1.  If you’re on Windows, open Rstudio and do this:

```
Sys.getenv("R_USER")
```
If you’re on *nix, do this:
```
Sys.getenv("HOME")
```

2. Store the GITLAB_PAT. 

If there is an ‘.Renviron’ in that folder, add this line:
```
GITLAB_PAT="glpat-the rest of your pat here"
```
Don’t forget to save the file. If there isn’t an ‘.Renviron’ in that folder, create one using a text editor and add that line. 

3. Restart R (Session→ Restart R should do the trick).

4. Test by typing this into R

```
Sys.getenv("GITLAB_PAT")
```


###  Install!

Now that your GILAB token is stored, you can install FishSET, you will need to do this in the R console:

```
remotes::install_gitlab("bryce.mcmanus/FishSET_RPackage@master", host="gitlab-afsc.fisheries.noaa.gov", build_vignettes=TRUE)
```
use the ``build_vignettes=FALSE`` option to turn off vignettes, which takes a little less time.



## <a name="Documentation"> Documentation and Tutorials </a>


## <a name="bugs"> Issues and Bug Reports </a>

Add in issue tab on github site

Issues? [https://github.com/FishSET/issues](https://gitlab-afsc.fisheries.noaa.gov/bryce.mcmanus/FishSET_RPackage/-/issues) 
       <!--  FishSET@noaa.gov -->

## <a name="cite"> Citation </a>

If you use FishSET results in publications, please cite the the package:

Alan Haynie, Melanie Harsch, Bryce McManus, and Allen Chen (2021). FishSET: Spatial Economics Toolbox for Fisheries. R package version 1.0.1.

## <a name="license"> NOAA Disclaimer </a>

This repository is a scientific product and is not official communication of the National Oceanic and
Atmospheric Administration, or the United States Department of Commerce. All NOAA Gitlabs project code is
provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of
Commerce or Department of Commerce bureaus stemming from the use of this Gitlab project will be governed
by all applicable Federal law. Any reference to specific commercial products, processes, or services by service
mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or
favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a
DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by
DOC or the United States Government.
