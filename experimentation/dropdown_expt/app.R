
library(shiny)
library(nycflights13)

airline_list <- airlines %>%
  collect  %>%
  split(.$name) %>%    # Field that will be used for the labels
  map(~.$carrier) 


first_name<- c("John", "Ernest", "Kathy")
last_name<- c("Smith", "Hemmingway", "Jones")
birth_date<- c("01/02/1995", "08/05/2004", "04/06/1986")
client_id<- c(1223, 3445, 5667)
other<- c("dog", "cat", "bird")

client_data<- data.frame(first_name, last_name, birth_date, client_id, other)


client_list <- client_data %>%
  tidyr::unite(dropdown_client, first_name, last_name, birth_date, sep = " ", remove = FALSE)

client_list<- client_list %>% 
  collect  %>%
  split( .$dropdown_client ) %>%    # Field that will be used for the labels
  map(~.$client_id) 




# Define UI for application that draws a histogram
ui <- fluidPage(
   
  selectInput(
    inputId = "client_selection",
    label = "Client Selection",
    choices = client_list,
    selectize = FALSE),
  
  textOutput("val")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

  output$val<- renderPrint({
    
    input$client_selection
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

