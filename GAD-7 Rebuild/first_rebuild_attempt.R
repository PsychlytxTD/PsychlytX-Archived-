ui<- function(request) {
  fluidPage(
    
    selectInput("pop", "select population", choices = c("male general population", "female general population", "older adult", "primary care",
                                                        "psychiatric", "Generalized Anxiety Disorder", "chronic musculoskeletal pain", 
                                                        "coronary heart disease", "type 1 diabetes", "type 2 diabetes", "stroke")),
    
    
    psychlytx::stats_widgets_UI("stats_widgets_id"),
    
    psychlytx::analytics_clientstatus_UI("analytics_clientstatus_id"),
    
    sidebarLayout(
      sidebarPanel(width = 6,
                   psychlytx::analytics_widgets_UI("analytics_widgets_id")
      ),
      
      mainPanel()),
    
    fluidRow(
      column(width = 12,
             psychlytx::analytics_newcustom_widgets_UI("analytics_newcustom_widgets_id")
      ))
    
    
    
  )     
  
}

server <- function(input, output, session) {
  
  clientstatus_module<- callModule(psychlytx::analytics_clientstatus, "analytics_clientstatus_id")
  callModule(psychlytx::analytics_widgets, "analytics_widgets_id", clientstatus_module)
  callModule(psychlytx::analytics_newcustom_widgets, "analytics_newcustom_widgets_id")
  
  pop<<- reactive({ input$pop })
  
  callModule(psychlytx::stats_widgets, "stats_widgets_id", list("GAD-7 Total Scale"), psychlytx::gad7_means_df, psychlytx::gad7_refs_df, pop)
  
}





shinyApp(ui, server)













