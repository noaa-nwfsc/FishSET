raw <- jsonlite::fromJSON("C:/Users/melanie.harsch/Work/FishSET_RPackage/inst/logs/2020-05-15.json")
int <- raw$fishset_run$function_calls[[2]]

# replace dat if required