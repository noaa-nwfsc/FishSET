if(!exists("meta") || is.null(meta)) meta <- packageDescription("FishSET")


citHeader("To cite the 'FishSET' package in publications use:")

year <- sub(".*(2[[:digit:]]{3})-.*", "\\1", meta$Date, perl = TRUE)
vers <- paste("R package version", meta$Version)

citEntry(entry="Manual",
         title = "FishSET: Spatial Economics Toolbox for Fisheries",
         author = personList(as.person("Alan Haynie"),
                             as.person("Melanie Harsch"),
                             as.person("Bryce McManus")),
         year = year,
         note = vers,
         url = "",
         textVersion =
           paste("Alan Haynie, Melanie Harsch and Bryce McManus (", year,
                 "). FishSET: Spatial Economics Toolbox for Fisheries. ",
                 vers, ".", sep="") )

citEntry(entry="Manual",
         title = paste0("Introduction to FishSET", meta$Version),
         author = personList(as.person("Alan Haynie"),
                             as.person("Melanie Harsch"),
                             as.person("Bryce McManus")),
         year = year,
         note = vers,
         url = "",
         textVersion =
           paste0("Haynie, A., M. Harsch, and A. B. McManus (", year, ") Introduction to FishSET", meta$Version, ". NOAA Fisheries, Alaska Fisheries Science Center, 7600 Sand Point Way NE, Seattle, WA 98115" ) )

#citEntry(entry="Article",
#         title = "FishSET: Spatial Economics Toolbox for Fisheries",
##         author = personList(as.person("Alan Haynie"),
#                             as.person("Melanie Harsch"),
#                             as.person("Bryce McManus")),
#         year     = {2022},
#         journal = "The R Journal",
#         volume     = {X},
#         number     = {X},
#         pages     = {xx--xx},
#         
#         textVersion =
#           paste("Haynie, A., Harsch, M. & McManus, B. (2022)",
#                 "FishSET: Spatial Economics Toolbox for Fisheriesa.", "The R Journal. X(X):xx-xx")
#)

