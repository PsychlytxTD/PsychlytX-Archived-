library(shiny)

#Create separate dataframes with mean values for the total scale and subscales 
#I think the best way to do this is to create a seperate .R script file for each scale and store the
# dataframes (e.g. for means and sds) within this script. It may be worthwhile introducing an additional argument
#into the module function — one that allows us to specify the interation number of the lapply statement (e.g. 3:5). This
#would allow us to control how many columns (and widgets) we insert per fluid row. Or maybe we need to create mini dataframes
#with the amount of values that we would like to see in each row. 

soldier_person<- c(3,4)
veteran_actor<- c(5,6)
means_dataset<- data.frame(soldier_person, veteran_actor)

soldier_person<- c("John et al 1996", "Harry et al 1997")
veteran_actor<- c("Tom 1984", "Jim 1984")
refs_dataset<- data.frame(soldier_person, veteran_actor)


population_widget_UI<- function(id) {
  
  ns<- NS(id)
  
  selectInput(ns("pop"), "select population", choices = c("soldier person", "veteran actor"))
  
}

population_widget<- function(input, output, session) {
  
  return(input)
  
}





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
    
    population<- gsub('([[:punct:]])|\\s+','_', pop())
    
    selectedCol<- dataf[ , population]
    
    selectedRef<- refsDataf[ , population]
    
    Input_List <- lapply(seq_along(scaleNames), function(i) {
      
      vals_name<- selectedCol[i]
      input_name  <- scaleNames[i]
      refs_name<- selectedRef[i]
      
      div(
        column(width = 2,
               numericInput(ns(input_name), input_name, value = vals_name),
               h6(paste("Reference:", refs_name))
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
  
  selectInput("pop", "select population", choices = c("soldier person", "veteran actor")),
  
  modUI("firstID")
  
)


server <- function(input, output, session) {
  
  pop<- reactive({ input$pop })
  
  callModule(mod, "firstID", list("depression", "anxiety"), means_dataset, refs_dataset, pop)
  
} 



shinyApp(ui = ui, server = server)