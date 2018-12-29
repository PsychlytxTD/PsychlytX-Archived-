library(shiny)

#Create separate dataframes with mean values for the total scale and subscales 
#I think the best way to do this is to create a seperate .R script file for each scale and store the
# dataframes (e.g. for means and sds) within this script

soldier<- c(3,4)
veteran<- c(5,6)
means_dataset<- data.frame(soldier, veteran)

soldier<- c("John et al 1996", "Harry et al 1997")
veteran<- c("Tom 1984", "Jim 1984")
refs_dataset<- data.frame(soldier, veteran)


#Writing module functions

modUI<- function(id) {
  
ns<- NS(id)

fluidRow(
uiOutput(ns("dynamicWidgets"))
        )

}


mod<- function(input, output, session, scaleNames, dataf, refsDataf, pop) {
  
  
  
  dynamicWidgetReactive<- reactive({ 
 

    ns <- session$ns
    
    population<<- pop()
    
    selectedCol<<- dataf[,paste(population)]
    
    selectedRef<<- refsDataf[,paste(population)]
    
    Input_List <- lapply(1:length(scaleNames), function(i) {
      
      vals_name<<- paste(selectedCol[i])
      input_name  <- paste(scaleNames[i])
      refs_name<- paste(selectedRef[i])
      
      div(
      column(width = 2,
      numericInput(ns(input_name), input_name, value = vals_name),
      paste("Reference:", refs_name)
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
  
  callModule(mod, "firstID", list("depression", "anxiety"), means_dataset, refs_dataset, pop)
  
} 



shinyApp(ui = ui, server = server)