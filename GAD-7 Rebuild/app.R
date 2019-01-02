ui<- 
  fluidPage(
    
    psychlytx::analytics_clientstatus_UI("analytics_clientstatus_id"),
    psychlytx::analytics_widgets_UI("analytics_widgets_id"),
    psychlytx::analytics_newcustom_widgets_UI("analytics_newcustom_widgets_id")
  )
  


server <- function(input, output, session) {

  clientstatus_module<- callModule(psychlytx::analytics_clientstatus, "analytics_clientstatus_id")
  callModule(psychlytx::analytics_widgets, "analytics_widgets_id", clientstatus_module)
  callModule(psychlytx::analytics_newcustom_widgets, "analytics_newcustom_widgets_id")
  

  }





shinyApp(ui, server)













