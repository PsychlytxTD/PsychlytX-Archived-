library(shiny)

ui <- fluidPage(
  
  uiOutput("dynamicWidgets")
  
)

server <- function(input, output, session) {
  
  dynamicWidgetReactive<- reactive({  
    
    scaleNames<- list("depression", "anxiety")
    
    Input_List <- lapply(scaleNames[1:2], function(scaleNames) {
      
      numericInput(paste(scaleNames), paste(scaleNames), 0)
      
    }
    )
    
    do.call(tagList, Input_List) 
  })
  
  
  output$dynamicWidgets<- renderUI({
    
    dynamicWidgetReactive()
    
  })
  
  
} 



shinyApp(ui = ui, server = server)