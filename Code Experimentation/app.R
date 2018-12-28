library(shiny)


#Writing module functions

modUI<- function(id) {
  
ns<- NS(id)

uiOutput(ns("dynamicWidgets"))

}


mod<- function(input, output, session, scaleNames) {
  
  dynamicWidgetReactive<- reactive({  
    
    ns <- session$ns
    
    Input_List <- lapply(scaleNames[1:2], function(scaleNames) {
      
      numericInput(ns(paste(scaleNames)), paste(scaleNames), 0)
      
    }
    )
    
    do.call(tagList, Input_List) 
  })
  
  
  output$dynamicWidgets<- renderUI({
    
    dynamicWidgetReactive()
    
  })
  
  
}



#Start of parent app

ui <- fluidPage(
  
  modUI("firstID")
  
)


server <- function(input, output, session) {
  
  callModule(mod, "firstID", list("depression", "anxiety"))
  
} 



shinyApp(ui = ui, server = server)