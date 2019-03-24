

library(periscope)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  downloadableTableUI("object_id1", 
                      downloadtypes = c("csv", "tsv"), 
                      hovertext = "Download the data here!",
                      contentHeight = "300px",
                      singleSelect = FALSE)
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
callModule(downloadTable, "object_id1")

}

# Run the application 
shinyApp(ui = ui, server = server)

