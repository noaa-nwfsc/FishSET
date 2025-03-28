# Module name: checklist
# Purpose: Generate a popup window that provides the user with a progress update and checklist to complete for FishSET processes.

# UI function for the module
# This function defines the layout 
checklist_ui <- function(id){
   tagList(
      actionButton(NS(id, "checklist_btn"), "Check progress", class = "btn-primary"), # action button to trigger progress checklist popup
   )
}

# Server function for the module
# This function renders the popup window
checklist_server <- function(id, project_name){
   moduleServer(id, function(input, output, session){
      
      # Show the modal when the action button is clicked
      observeEvent(input$checklist_btn, {
         
         q_test <- quietly_test(checklist)
         # cList$out <- q_test(project$name, rv$data)
         # cList$pass <- all(vapply(cList$out, function(x) x$pass, logical(1)))
         
         showModal(modalDialog(title = "FishSET Progress",
                               uiOutput(session$ns("checklist_diagram")), # dynamic UI placeholder
                               easyClose = FALSE, 
                               footer = tagList(
                                  modalButton("Close")
                               )))
      })
      
      # Dynamically render the progress checlist/diagram
      output$checklist_diagram <- renderUI({
         tags$div(
            tags$p(project_name()),
            
            fluidRow(
               column(1, icon(name="file-arrow-up", lib="font-awesome", style="color: green; font-size: 30px;")),
               column(1, icon(name="arrow-right", lib="font-awesome", style="color: black; font-size: 30px;")),
               column(1, icon(name="magnifying-glass-chart", lib="font-awesome", style="color: green; font-size: 30px;")),
               column(1, icon(name="arrow-right", lib="font-awesome", style="color: black; font-size: 30px;")),
               column(1, icon(name="file-lines", class="fa-solid", lib="font-awesome", style="color: #FFBF00; font-size: 30px;")),
               column(1, icon(name="arrow-right", lib="font-awesome", style="color: black; font-size: 30px;")),
               column(1, icon(name="gears", lib="font-awesome", style="color: black; font-size: 30px;")),
               column(1, icon(name="arrow-right", lib="font-awesome", style="color: black; font-size: 30px;")),
               column(1, icon(name="fish-fins", lib="font-awesome", style="color: black; font-size: 30px;"))
            )
         )
      })
   })
} 
