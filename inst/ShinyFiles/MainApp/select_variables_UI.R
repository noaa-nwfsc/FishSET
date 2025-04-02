### select variables modules

sel_variablesUI <- function(id){
   ns <- NS(id)
   tagList(
   uiOutput(ns("select_variables")),
   )
}

