# Module name: checklist
# Purpose: Generate a popup window that provides the user with a progress update and checklist to complete for FishSET processes.

# UI function for the module ----
# This function defines the layout 
checklist_ui <- function(id){
   tagList(
      actionButton(NS(id, "checklist_btn"), "Check progress", class = "btn-primary"), # action button to trigger progress checklist popup
   )
}



# Server function for the module ----
# This function renders the popup window
checklist_server <- function(id, project_name, project_data){
   moduleServer(id, function(input, output, session){
      
      # Create reactives for checklist server
      rv_project_checklist = reactiveVal(NULL)
      
      # Show the modal when the action button is clicked
      observeEvent(input$checklist_btn, {
         q_test <- quietly_test(checklist)
         project_checklist <- q_test(project_name(), project_data())
         rv_project_checklist(project_checklist)
         checklist_status <- all(vapply(project_checklist, function(x) x$pass, logical(1)))

         showModal(modalDialog(title = "FishSET Progress",
                               uiOutput(session$ns("checklist_diagram")), # dynamic UI placeholder
                               easyClose = FALSE, 
                               footer = tagList(
                                  modalButton("Close")
                               )))
      })
      

      # This function returns icons to be displayed in checklist, and sets color based on pass/fail of checklist tests
      pass_icon <- function(type, project_checklist) {
         
         # icon wrapper function to simplify settings below
         icon_wrapper <- function(icon_name, icon_color) {icon(name=icon_name, lib="font-awesome", style=paste0("color: ", icon_color, "; font-size: 30px;"))}
         
         # QAQC
         if(type=='qaqc'){
            if(project_checklist[[type]]$pass){
               tmp_icon <- icon_wrapper('magnifying-glass-chart', 'green')
            } else {
               tmp_icon <- icon_wrapper('magnifying-glass-chart', 'black')
            }
            
         }
         
         return(tmp_icon)
      }

      
      # Dynamically render the progress checlist/diagram
      output$checklist_diagram <- renderUI({
         tags$div(
            tags$p(), # TESTING CODE - REMOVE OR COMMENT WHEN DEPLOYED ###########################################################
            
            fluidRow(
               column(1, icon(name="file-arrow-up", lib="font-awesome", style="color: green; font-size: 30px;")),
               column(1, icon(name="arrow-right", lib="font-awesome", style="color: black; font-size: 30px;")),
               
               column(1, pass_icon('qaqc', rv_project_checklist())),
               column(1, icon(name="arrow-right", lib="font-awesome", style="color: black; font-size: 30px;")),
               
               column(1, icon(name="file-lines", class="fa-solid", lib="font-awesome", style="color: #FFBF00; font-size: 30px;")),
               column(1, icon(name="arrow-right", lib="font-awesome", style="color: black; font-size: 30px;")),
               
               column(1, icon(name="gears", lib="font-awesome", style="color: black; font-size: 30px;")),
               column(1, icon(name="arrow-right", lib="font-awesome", style="color: black; font-size: 30px;")),
               
               column(1, icon(name="fish-fins", lib="font-awesome", style="color: black; font-size: 30px;"))
            ),
            
            # tags$ul(
            #    tags$li(pass_icon("qaqc"), tags$strong("Data quality checks")),
            #    show_msg("qaqc"),
            #    qaqc_msg(),
            #    tags$li(pass_icon("occur_pnts"), tags$strong("Valid occurrence points")),
            #    show_msg("occur_pnts"),
            #    tags$li(pass_icon("alt_choice"), tags$strong("Alternative choice matrix created")),
            #    show_msg("alt_choice"),
            #    tags$li(pass_icon("expect_catch"), tags$strong("Expected catch/revenue matrix created")),
            #    show_msg("expect_catch"),
            #    ec_msg()
            # )
         )
      })
   })
} 
