library(shiny)


#Writing module functions

modUI<- function(id) {
  
ns<- NS(id)

fluidRow(
uiOutput(ns("dynamicWidgets"))
        )

}


mod<- function(input, output, session, scaleNames, popVals) {
  
  dynamicWidgetReactive<- reactive({ 
 
    
    
    val_list<- lapply(1:length(popVals[[1]]), function(popVals) {
      
   4
      
    }
    
    )
    
    
    
    
    
    ns <- session$ns
    
    Input_List <- lapply(scaleNames[1:length(scaleNames)], function(scaleNames) {
      
      div(
      column(width = 2,
      numericInput(ns(paste(scaleNames)), paste(scaleNames), value = do.call(print, val_list))
            )
        )
      
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
  
  callModule(mod, "firstID", list("depression", "anxiety"),  list("soldier"=c(1,2), "veteran"=c(3,4)))
  
} 



shinyApp(ui = ui, server = server)