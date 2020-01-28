requireNamespace('shiny')
requireNamespace('ggplot2')
requireNamespace('shinyjs')
requireNamespace('DT')
#----
#Helper functions
#----
enableBookmarking(store = "server")

#locdatabase <- paste0(system.file(package='FishSET'), '/fishset_db.sqlite')

# default global search value
if (!exists("default_search")) {default_search <- ""}

# default column search values
if (!exists("default_search_columns")) {default_search_columns <- NULL}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

model_table <- data.frame('mod_name'='', 'likelihood'='', 'alternatives'='', 'optimOpts'='', 'inits'='', 
                          'vars1'='','vars2'='', 'catch'='', 'lon'='', 'lat'='', 'project'='', 'price'='', 'startloc'='', 'polyn'='')

