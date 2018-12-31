
newcustom_widgets_UI<- function(id) {
  
  ns<- NS(id)
  
tagList(
h4(tags$strong("Create your own customised variables")),
br(),
textInput(ns("newcustom_names"), "Name Your Variables"),
h5(tags$em("*Separate the names with commas & leave no blank space.")),
h5(tags$em("*E.g. height,weight")),
actionButton(ns("newcustom_widgets_action"),"Create"),

uiOutput(ns("newcustom_widgets_out"))
        )



}

newcustom_widgets<- function(input, output, session) {
  
  newcustom_widgets_reac <- eventReactive(input$newcustom_widgets_action,{
    
    newcustom_names<- as.character(unlist(strsplit(input$newcustom_names,",")))
    
    widget_list <- lapply(seq_along(newcustom_names), function(i) {
    id_label<- paste(newcustom_names[i])
     
     div(
       column(width = 2,
                textAreaInput(id_label,id_label, width = "200px", height = "43px", resize = "vertical")
      
              )
         )
    }
                       )
    
    do.call(tagList, widget_list)                                 },ignoreInit = T)
    
  
  output$newcustom_widgets_out = renderUI({
    
    newcustom_widgets_reac()
    
  }) 
  
  
}


ui <- fluidPage(
 
  newcustom_widgets_UI("newcustom_widgets_id")   
  
)


server <- function(input, output, session) {
  
  callModule(newcustom_widgets,"newcustom_widgets_id")
  
}

shinyApp(ui, server)