
rp_checkboxModuleServer <- function(id, project, spatdat, values, dynamicCheckboxData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$policy_select_mod <- renderUI({
      req(project)
      checkboxGroupInput(
        ns("select_pol_mod"), 'Select a model', 
        choices = dynamicCheckboxData())
    })
    
    output$run_pol_sel_scen <- renderUI({
      req(project)
      checkboxGroupInput(
        inputId = ns("run_pol_chk_scen"),
        label = "Select closure scenario(s):",
        choices = c(close_names(project)),
        inline = TRUE
      )
    })
    
    # Return the reactive selected checkboxes
    reactive({
      list(
        models = input$select_pol_mod,
        sel_closures = input$run_pol_chk_scen,
        zone_id = input$pol_prim_sel_cat
      )
    })
  })
}

rp_selectInputModuleServer <- function(id, project, spatdat, values, selected_choices){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # selections 
    marg_income_selections <- reactiveValues()
    
    output$pol_likelihood <- renderUI({
      req(project)
      req(selected_choices())
      req(input$select_pol_mod)
      
      selected_options <- selected_choices()
      
      if (is.null(selected_options)){
        return(NULL)}  # No selections made
      
      tagList(
        lapply(selected_options$models, function(opt) {
          if(!(model_design_list(project)[[which(lapply(model_design_list(project=project), "[[", "mod.name")
                                                 %in% opt)]]$likelihood %in% c("logit_c", "logit_zonal"))) {
            return(NULL)  # No selections made
            
          } else {
            
            dynamic_choices <- row.names(model_out_view(project)[[which(lapply(model_out_view(project), "[[", "name") == opt)]]$OutLogit)
            selected_value <- marg_income_selections[[opt]]
            
            if (is.null(selected_value)) {
              selected_value <- NULL}
            tagList(
              selectInput(
                inputId = ns(paste0("select_marg_inc_", opt)),
                label = paste("Select marginal utility of income for", opt),
                choices = dynamic_choices,
                selected = selected_value
              ),
              add_prompter(
                selectInput(ns(paste0("income_cost_pol_", opt)), 
                            label = paste("Income Cost for ", opt),
                            choices = c("TRUE", "FALSE")),
                position = "bottom", type='info', size='medium',
                message = "For conditional and zonal logit models. Logical indicating whether the coefficient
                                       for the marginal utility of income relates to cost (TRUE) or revenue (FALSE).")
            )
          }
        })
        
      )
    })
    
    
    # Observe selectInputs to store their values in reactiveValues
    observe({
      selected_options <- selected_choices()
      
      lapply(selected_options$models, function(opt) {
        
        observeEvent(input[[paste0("income_cost_pol_", opt)]], {
          marg_income_selections[[paste0("income_cost_pol_", opt)]] <- input[[paste0("income_cost_pol_", opt)]]
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("select_marg_inc_", opt)]], {
          marg_income_selections[[paste0("select_marg_inc_", opt)]] <- input[[paste0("select_marg_inc_", opt)]]
        }, ignoreInit = TRUE)
        
      })
      
      
      
      
    })
    
    # Return the reactive values
    reactive({
      # # Get all keys from marg_income_selections
      all_keys <- selected_choices()$models
      
      # Filter keys for income_cost_pol and select_marg_inc
      income_cost_keys <- paste0("income_cost_pol_", all_keys)
      select_marg_keys <- paste0("select_marg_inc_", all_keys)
      # 
      # Extract the values for income_cost_pol
      income_cost_list <- lapply(income_cost_keys, function(key) marg_income_selections[[key]])
      
      # Extract the values for select_marg_inc
      select_marg_list <- lapply(select_marg_keys, function(key) marg_income_selections[[key]])
      
      # Return the separated lists
      list(
        income_cost_pol = income_cost_list,
        select_marg_inc = select_marg_list
      )
    })
  })
}

rp_welf_predModuleServer <- function(id, project, spatdat, values, selected_choices, marg_selections, all_variables){
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      fdf <- reactiveValues(outputs_welf = NULL)
      
      pol <- reactiveVal(NULL)
      more_pol <- reactiveVal(NULL)
      
      observeEvent(input$run_policy_button, {
        # Check for required values 
        req(input$run_pol_chk_scen)
        req(input$pol_betadraws)
        req(all_variables())
        
        # Set up reactives
        selections <- selected_choices()
        marg <- marg_selections()  
        pol(selections)
        welfare <- pol()
        
        fdf$outputs_welf <- run_policy(project,
                                       mod.name =isolate(c(welfare$models)),
                                       policy.name = c(welfare$sel_closures),
                                       betadraws = input$pol_betadraws,
                                       marg_util_income = c(marg$select_marg_inc),
                                       income_cost = c(marg$income_cost_pol),
                                       zone.dat =  all_variables()$pz_id,
                                       group_var = NULL,
                                       enteredPrice = NULL,
                                       expected.catch = NULL,
                                       use.scalers = FALSE,
                                       scaler.func = NULL)
        
        if(any(unlist(lapply(fdf$outputs_welf[[2]], function(x) (x < 0))))==TRUE){
          shinyWidgets::show_alert(
            title = NULL,
            text = paste0("Marginal utility of income is negative for ", names(fdf$outputs_welf[[2]])[sapply(fdf$outputs_welf[[2]], function(x) x < 0)],
                          
                          ". Check model coefficient (estimate and standard error) and select appropriate marginal utility of income."),
            type = "error",
            btn_colors = "#2A90A1",
            closeOnClickOutside = TRUE,
            width = "50%"
          )
        }
      })
      
      output$welfare_plot_dol <-  plotly::renderPlotly({
        req(input$run_policy_button)
        req(isTruthy(fdf$outputs_welf))
        
        if(any(unlist(lapply(fdf$outputs_welf[[2]], function(x) (x < 0))))==TRUE){
          return(NULL)
        }else {
          fdf$outputs_welf[[1]][[4]]
        }
      })
      
      output$welfare_tbl_dol <- DT::renderDataTable({
        req(input$run_policy_button)
        req(isTruthy(fdf$outputs_welf))
        
        if(any(unlist(lapply(fdf$outputs_welf[[2]], function(x) (x < 0))))==TRUE){
          return(NULL)
        }else {
          fdf$outputs_welf[[1]][[1]]
        }
      })
      
      output$welfare_plot_prc <- plotly::renderPlotly({
        req(input$run_policy_button)
        req(isTruthy(fdf$outputs_welf))
        
        if(any(unlist(lapply(fdf$outputs_welf[[2]], function(x) (x < 0))))==TRUE){
          return(NULL)
        }else {
          fdf$outputs_welf[[1]][[5]]
        }
      })
      
      
      output$welfare_tbl_prc <- DT::renderDataTable({
        req(input$run_policy_button)
        req(isTruthy(fdf$outputs_welf))
        
        if(any(unlist(lapply(fdf$outputs_welf[[2]], function(x) (x < 0))))==TRUE){
          return(NULL)
        }else {
          fdf$outputs_welf[[1]][[2]]
        }
      })
      
      output$welfare_tbl_details <- DT::renderDataTable({
        req(input$run_policy_button)
        req(isTruthy(fdf$outputs_welf))
        
        if(any(unlist(lapply(fdf$outputs_welf[[2]], function(x) (x < 0))))==TRUE){
          return(NULL)
        }else {
          
          fdf$outputs_welf[[1]][[3]]
        }
      })
      
      output$pred_prob_tbl <-function() {
        req(project)
        req(input$run_policy_button)
        
        if(any(unlist(lapply(fdf$outputs_welf[[2]], function(x) (x < 0))))==TRUE){
          return(NULL)
        }else {
          pred_prob_outputs(project, mod.name = isolate(c(selected_choices()$model)), output_option = "table")
        }
      }
      
      output$pred_prod_mod_fig <- plotly::renderPlotly({
        req(project)
        req(input$run_policy_button)
        
        if(any(unlist(lapply(fdf$outputs_welf[[2]], function(x) (x < 0))))==TRUE){
          return(NULL)
        }else {
          pred_prob_outputs(project, mod.name = isolate(c(selected_choices()$model)), output_option = "model_fig")
        }
        
      })
      
      output$pred_prod_pol_fig <- plotly::renderPlotly({
        req(project)
        req(input$run_policy_button)
        
        if(any(unlist(lapply(fdf$outputs_welf[[2]], function(x) (x < 0))))==TRUE){
          return(NULL)
        }else {
          pred_prob_outputs(project, mod.name = isolate(c(selected_choices()$model)),
                            policy.name = isolate(c(selected_choices()$sel_closures)),
                            output_option = "policy_fig")
        }
        
      })
      
      output$pol_mod_diff_tbl <- function() {
        req(project)
        req(input$run_policy_button)
        
        if(any(unlist(lapply(fdf$outputs_welf[[2]], function(x) (x < 0))))==TRUE){
          return(NULL)
        }else {
          pred_prob_outputs(project,  mod.name = isolate(c(selected_choices()$model)),
                            policy.name = isolate(c(selected_choices()$sel_closures)),
                            output_option = "diff_table")
        }
        
      }
    }
  )
}


pred_mapServer <- function(id, project, spatdat, selected_choices, all_variables){
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      # Initialize output plot as a reactive
      rv_output <- reactiveValues(plot = NULL)
      
      output$pred_map_sel_mod <- renderUI({
        # Check for required values
        req(project)
        req(input$run_policy_button)
        
        selectInput(
          ns("select_pred_pol_mod"), 'Select a model', 
          choices = c(isolate(selected_choices()$models)))
      })
      
      output$policy_select_scenario <- renderUI({
        
        req(project)
        req(input$run_policy_button)
        
        
        selectInput(ns("pred_pol_name"), "Select closure scenario",
                    choices = c("no closure", isolate(selected_choices()$sel_closures)))
        
      })
      
      observeEvent(input$run_pred_map, {
        # Check for required values
        req(project)
        req(input$select_pred_pol_mod)
        req(input$pred_pol_name)
        req(all_variables())
        
        if(input$pred_pol_name == "no closure"){
          rv_output$plot <-  predict_map(project, mod.name = isolate(input$select_pred_pol_mod),
                                         policy.name = isolate(input$select_pred_pol_mod), 
                                         spat = spatdat, zone.spat = all_variables()$sz_id)
          
        } else {
          rv_output$plot <-  predict_map(project, mod.name = isolate(input$select_pred_pol_mod),
                                         policy.name = isolate(input$pred_pol_name), 
                                         spat = spatdat, zone.spat = all_variables()$sz_id)
        }
      })
      
      output$predict_map <- leaflet::renderLeaflet({
        req(input$run_pred_map)
        req(isTruthy(rv_output$plot))
        
        validate(
          need(input$run_policy_button, 'To view map, run policy function above.'),
        )
        
        rv_output$plot
      })
    }
  )
}
