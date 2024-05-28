

run_policyUI <- function(id){
  ns <- NS(id)
  
  tagList(
      uiOutput(ns("policy_select_mod")),
      uiOutput(ns("run_pol_sel_scen")),
  add_prompter(
    numericInput(ns("pol_betadraws"), "Betadraws", value = 1000),
        position = "top", type='info', size='medium', 
        message = "Integer indicating the number of times to run the welfare simulation."),
  uiOutput(ns("pol_prim_cat")),
  
  add_prompter(
    uiOutput(ns("pol_likelihood")),
        position = "top", type='info', size='medium', 
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
                             bslib::card_header(strong("Policy plot"), class = "bg-info"), 
                             bslib::card_body(shinycssloaders::withSpinner(plotly::plotlyOutput(ns('pred_prod_pol_fig'))
                                                          ,type = 6))
                           )
                           
                         ),
                         bslib::card(
                           full_screen = TRUE,
                           bslib::card_header(strong("Policy difference table"), class = "bg-info"), 
                           bslib::card_body(shinycssloaders::withSpinner(tableOutput(ns('pol_mod_diff_tbl'))
                                                                         ,type = 6))
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

welfare_outputsUI <- function(id){
  ns <- NS(id)
  
  bslib::accordion_panel("Welfare figures",
                         bslib::layout_column_wrap(
                           width = 1/2,
                           bslib::card(
                             full_screen = TRUE,
                             bslib::card_header(strong("Welfare loss/gain for all scenarios as dollars"), class = "bg-info"), 
                             bslib::card_body(shinycssloaders::withSpinner(plotly::plotlyOutput(ns('welfare_plot_dol'))
                                                                           ,type = 6), 
                                               shinycssloaders::withSpinner(DT::DTOutput(ns("welfare_tbl_dol"))
                                                                            ,type = 6)
                                              )
                           ),
                           bslib::card(
                             full_screen = TRUE,
                             bslib::card_header(strong("Welfare loss/gain for all scenarios as percentage"), class = "bg-info"),
                             bslib::card_body(shinycssloaders::withSpinner(plotly::plotlyOutput(ns('welfare_plot_prc'))
                                                                           ,type = 6),
                                              shinycssloaders::withSpinner(DT::DTOutput(ns("welfare_tbl_prc"))
                                                                           ,type = 6))
                           )),
                           bslib::card(
                             full_screen = TRUE,
                             bslib::card_header(strong("Supplementary information table"), class = "bg-info"),
                             bslib::card_body(shinycssloaders::withSpinner(DT::DTOutput(ns("welfare_tbl_details"))
                                                                           ,type = 6))
                            )

                         
  )
  
}
