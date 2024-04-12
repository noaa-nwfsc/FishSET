

run_policyUI <- function(id){
  ns <- NS(id)
  
  tagList(
      uiOutput(ns("policy_select_mod")),
  add_prompter(
    numericInput(ns("pol_betadraws"), "Betadraws", value = 1000),
        position = "bottom", type='info', size='medium', 
        message = "Integer indicating the number of times to run the welfare simulation."),
  uiOutput(ns("pol_prim_cat")),
  
  add_prompter(
    uiOutput(ns("pol_likelihood")),
        position = "bottom", type='info', size='medium', 
        message = "For conditional and zonal logit models. Name of the coefficient to use as marginal utility of income."),

    actionButton(ns("run_policy_button"), "Run Policy Function",
                 class = "btn-primary")
  )

}

predict_map_sidebarUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("policy_select_scenario")),
    uiOutput(ns("pred_map_cat")),
    actionButton(ns("run_pred_map"), "Run Predict Map",
                 class = "btn-primary")
  )   
}


pred_plotsUI <- function(id){
  ns <- NS(id)
  
  bslib::accordion_panel("Summarize predicted probabilities of fishing per zone for each model",
                         bslib::card(
                           full_screen = TRUE,
                           bslib::card_header(strong("Table"), class = "bg-info"), 
                           bslib::card_body(shinycssloaders::withSpinner(DT::DTOutput(ns("pred_prob_tbl"))
                                                        ,type = 6))
                         ),
                         bslib::layout_column_wrap(
                           width = 1/2,
                           bslib::card(
                             full_screen = TRUE,
                             bslib::card_header(strong("Model plot"), class = "bg-info"), 
                             bslib::card_body(shinycssloaders::withSpinner(plotly::plotlyOutput(ns('pred_prod_mod_fig'))
                                                          ,type = 6))
                           ),
                           bslib::card(
                             full_screen = TRUE,
                             bslib::card_header(strong("Policy Plot"), class = "bg-info"), 
                             bslib::card_body(shinycssloaders::withSpinner(plotly::plotlyOutput(ns('pred_prod_pol_fig'))
                                                          ,type = 6))
                           )
                         )
  )
  
}


predict_map_mainUI <- function(id){
  
  ns <- NS(id)
  
  bslib::accordion_panel("Map of predicted probabilities by zone",
                         bslib::card(
                           full_screen = TRUE,
                           bslib::card_header(strong("Map"), class = "bg-info"), 
                           bslib::card_body(shinycssloaders::withSpinner(leaflet::leafletOutput(ns("predict_map"))
                                                        ,type = 6))
                         )
  )
}