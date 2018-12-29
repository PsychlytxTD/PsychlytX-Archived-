library(shiny)

soldier<- c(3,4)
veteran<- c(5,6)
dataset<- data.frame(soldier, veteran)


#Writing module functions

modUI<- function(id) {
  
ns<- NS(id)

fluidRow(
uiOutput(ns("dynamicWidgets"))
        )

}


mod<- function(input, output, session, scaleNames, dataf, pop) {
  
  
  
  dynamicWidgetReactive<- reactive({ 
 

    ns <- session$ns
    
    population<<- pop()
    
    selectedCol<<- dataf[,paste(population)]
    
    Input_List <- lapply(1:length(scaleNames), function(i) {
      
      vals_name<<- paste(selectedCol[i])
      input_name  <- paste(scaleNames[i])
      lab_name<- paste(scaleNames[i])

      div(
      column(width = 2,
      numericInput(ns(input_name), lab_name, value = vals_name)
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
  
  selectInput("pop", "select population", choices = c("soldier", "veteran")),
  
  modUI("firstID")
  
)


server <- function(input, output, session) {
  
 pop<- reactive({ input$pop })
  
  callModule(mod, "firstID", list("depression", "anxiety"), dataset, pop)
  
} 



shinyApp(ui = ui, server = server)