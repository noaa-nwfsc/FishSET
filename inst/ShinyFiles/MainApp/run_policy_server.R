
pred_plotsServer <- function(id, project, spatdat, values){
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      mod_pol_out <- reactive({
        
        req(project)
        
        tab <- paste0(project, 'modelOut')
        
        if (table_exists(tab, project)) {
          
          model_out_view(project)
        }
      })
      
      output$policy_select_mod <- renderUI({
        
        req(project)
        
        selectInput(ns("select_pol_mod"), 'Select a model', 
                    choices = c(model_names(project)))
        
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
      
      output$pol_prim_cat <- renderUI({
        selectInput(ns("pol_prim_sel_cat"), 
                    "Select zone ID from primary data",
                    choices = unique(names(values)))
        
      })
      
      output$pol_likelihood <- renderUI({
        req(project)
        req(input$select_pol_mod)
        if((model_design_list(project)[[which(lapply(model_design_list(project=project), "[[", "mod.name")
                                                      == input$select_pol_mod)]]$likelihood %in% c("logit_c", "logit_zonal"))){
          tagList(
          selectInput(ns("select_marg_inc"),'Marginal utility of income coefficient', 
                     choices = row.names(model_out_view(project)[[which(lapply(model_design_list(project=project), "[[", "mod.name")
                                                                          == input$select_pol_mod)]]$OutLogit)),
     
          add_prompter(
            selectInput(ns("income_cost_pol"), "Income Cost",
                        choices = c("TRUE", "FALSE")),
                position = "bottom", type='info', size='medium',
                message = "For conditional and zonal logit models. Logical indicating whether the coefficient
                          for the marginal utility of income relates to cost (TRUE) or revenue (FALSE).")
          )

        } else{
          return()
        }

      })
      
    pol <- reactiveValues(outputs_welf = NULL)
    

    observeEvent(input$run_policy_button,{
        req(project)
        req(input$select_pol_mod)
        req(input$run_pol_chk_scen)
        req(input$pol_betadraws)
        req(input$select_marg_inc)
        req(input$income_cost_pol)
        req(input$pol_prim_sel_cat)

        pol$outputs_welf <- run_policy(project, 
                                       mod.name = isolate(input$select_pol_mod),
                                       policy.name = c(input$run_pol_chk_scen), 
                                       betadraws = input$pol_betadraws, 
                                       marg_util_income = input$select_marg_inc, 
                                       income_cost = isolate(input$income_cost_pol),
                                       zone.dat = input$pol_prim_sel_cat, 
                                       group_var = NULL,
                                       enteredPrice = NULL,
                                       expected.catch = NULL, 
                                       use.scalers = FALSE, 
                                       scaler.func = NULL) 
        
        
          if(pol$outputs_welf[[2]] <0)
              shinyWidgets::show_alert(
                title = NULL,
                text = paste0("Marginal utility of income is negative. Check model coefficient (estimate and standard error) and select appropriate marginal utility of income."),
                type = "error",
                btn_colors = "#2A90A1",
                closeOnClickOutside = TRUE,
                width = "50%"
              )
    })     
      
        output$welfare_plot_dol <- plotly::renderPlotly({
          req(project)
          req(input$run_policy_button)

          if(is.null(pol$outputs_welf) | pol$outputs_welf[[2]] <0) return()
          pol$outputs_welf[[1]][[1]]

        })

        output$welfare_plot_prc <- plotly::renderPlotly({
          req(project)
          req(input$run_policy_button)

          if(is.null(pol$outputs_welf) | pol$outputs_welf[[2]] <0) return()
          pol$outputs_welf[[1]][[2]]

        })

        output$welfare_tbl_dol <- DT::renderDataTable({
          req(project)
          req(input$run_policy_button)

          if(is.null(pol$outputs_welf) | pol$outputs_welf[[2]] <0) return()
          pol$outputs_welf[[1]][[3]]

        })

        output$welfare_tbl_prc <- DT::renderDataTable({
          req(project)
          req(input$run_policy_button)

          if(is.null(pol$outputs_welf) | pol$outputs_welf[[2]] <0) return()
          pol$outputs_welf[[1]][[4]]
        })

        output$welfare_tbl_details <- DT::renderDataTable({
          req(project)
          req(input$run_policy_button)

          if(is.null(pol$outputs_welf) | pol$outputs_welf[[2]] <0) return()
          pol$outputs_welf[[1]][[5]]
        })


         output$pred_prob_tbl <- DT::renderDataTable({
           req(project)
           req(input$run_policy_button)
           req(isTruthy(pol$outputs_welf))
        
        
           if(is.null(pol$outputs_welf) | pol$outputs_welf[[2]] <0) return()
           pred_prob_outputs(project, mod.name = isolate(input$select_pol_mod), output_option = "table")
         })
        
         output$pred_prod_mod_fig <- plotly::renderPlotly({
           req(project)
           req(input$run_policy_button)
           req(isTruthy(pol$outputs_welf))
           
        
           if(is.null(pol$outputs_welf) | pol$outputs_welf[[2]] <0) return()
           pred_prob_outputs(project, mod.name = isolate(input$select_pol_mod), output_option = "model_fig")

         })
        
         output$pred_prod_pol_fig <- plotly::renderPlotly({
           req(project)
           req(input$pol_prim_sel_cat)
           req(input$run_policy_button)
           req(isTruthy(pol$outputs_welf))
           
        
        
           if(is.null(pol$outputs_welf) | pol$outputs_welf[[2]] <0) return()
           pred_prob_outputs(project, mod.name = isolate(input$select_pol_mod),
                             policy.name = c(input$run_pol_chk_scen),
                             output_option = "policy_fig")

         })
        
         output$pol_mod_diff_tbl <- function() {
           req(project)
           req(input$run_policy_button)
           req(input$pol_prim_sel_cat)
           req(input$run_pol_chk_scen)
           req(isTruthy(pol$outputs_welf))
           
        
        
           if(is.null(pol$outputs_welf) | pol$outputs_welf[[2]] <0) return()
           pred_prob_outputs(project, mod.name = isolate(input$select_pol_mod),
                             zone.dat = input$pol_prim_sel_cat,
                             policy.name = c(input$run_pol_chk_scen),
                             output_option = "diff_table")

        
         }
      }
    )
  }



pred_mapServer <- function(id, project, spatdat){
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      v <- reactiveValues(plot = NULL,
                          text = NULL)
      
      
      output$pred_map_cat <- renderUI({
        selectInput(ns("pred_map_sel_cat"), "Select zone ID from spatial data",
                    choices = unique(names(spatdat)))
        
      })
      
      output$policy_select_scenario <- renderUI({
        
        req(project)
        
        selectInput(ns("pred_pol_name"), "Select closure scenario",
                    choices = c("no closure", close_names(project)))
        
      })
      
      
      observeEvent(input$run_pred_map, {
        
        req(project)
        req(input$pred_pol_name)
        req(input$pred_map_sel_cat)
        
        if(input$pred_pol_name == "no closure"){
        v$plot <-  predict_map(project, mod.name = isolate(input$select_pol_mod),
                               policy.name = isolate(input$select_pol_mod), 
                               spat = spatdat, zone.spat = input$pred_map_sel_cat)
        
      } else {

        v$plot <-  predict_map(project, mod.name = isolate(input$select_pol_mod),
                               policy.name = paste0(isolate(input$select_pol_mod), " ",input$pred_pol_name), 
                               spat = spatdat, zone.spat = input$pred_map_sel_cat)
      }
    })
      
      output$predict_map <- leaflet::renderLeaflet({
        req(input$run_pred_map)
        
        validate(
          need(input$run_policy_button, 'To view map, run policy function above.'),
        )
        
        if (is.null(v$plot)) return()
        v$plot
        
        
      })
    }
  )
}
