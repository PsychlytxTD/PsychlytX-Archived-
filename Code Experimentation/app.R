diagnosis_list<- list("depression", "anxiety", "OCD", "stress")


analytics_tabpanel_UI<- function(id) {
  
  ns<- NS(id)
  
  uiOutput(ns("analytics_tabwidgets_out"))
  
}

analytics_tabpanel<- function(input, output, session, client_type) {
  
  analytics_tabwidgets_reac<- reactive({
    
    req(client_type())
    
    ns <- session$ns
    
    client_selection<<- client_type()
    
    switch(client_selection, 
           
           new = 
             
             tagList(
               h5(tags$em("*Complete or have the client complete this information before administering the first scale.")),
               br(),
               h4(tags$strong("Basic Demographics")),
               br(),
               textInput(ns("ControlName"), "Patient's Name"),
               selectInput(ns("Sex"), "Sex", c("", "Male", "Female"), width = '200px'),
               numericInput(ns("Age"), "Age", value = "", width = '100px'),
               selectInput(ns("Sexuality"), "Sexual Orientation", c("", "Heterosexual", "Lesbian", "Gay", "Bisexual", "Transgender", "Queer", "Other"), width = '200px'),
               selectInput(ns("Relationship"), "Relationship Status", c("", "Married/In Relationship", "Single", "Widowed"), width = '250px'),
               numericInput(ns("Children"), "Number of Dependent Children", value = "", width = '100px'),
               selectInput(ns("Workforce"), "Primary Workforce Status", c("", "Working", "Studying", "Unemployed", "Retired"), width = '200px'),
               selectInput(ns("Education"), "Education", c("", "Did Not Complete High School", "Completed High School", "Completed Tertiary Education"), width = '250px'),
               textInput(ns("Occupation"), "Occupation"),
               textInput(ns("Suburb"), "Suburb", value = "", width = '300px')
             ), 
           
           old = 
             
             tagList( 
               h5(tags$em("*Enter the information below when you administer a scale for the last time (i.e. discharge assessment)")),
               h4(tags$strong("Clinical Information")),
               br(),
               selectInput(ns("Principal_Diagnosis"), "Presenting Principal Diagnosis", diagnosis_list),
               selectizeInput(ns("Secondary_Diagnosis"), "Presenting Additional Diagnosis/Diagnoses", diagnosis_list, multiple = TRUE),
               textInput(ns("Referrer"), "Referrer", value = "", width = '200px'),
               selectInput(ns("Attendance_Arrangement"), "Attendance Arrangement", c("", "It Varies", "Twice A Week", "Once A Week", "Once a Fortnight", "Once Every 3 Weeks", "Once A Month", "More Than 1 Month Apart"), width = '250px'),
               selectInput(ns("Attendance_Quality"), "Quality of Attendance", c("", "Good", "Moderate", "Poor"), width = '200px'),
               textInput(ns("Therapy"), "Therapeutic Approach", value = "", width = '200px'),
               selectInput(ns("Fee"), "Fee Arrangement", c("", "No Out-Of-Pocket Expense", "Discount", "Full-Fee"), width = '250px'),
               numericInput(ns("Duration"), "Number of Sessions Attended", value = "", width = '100px'),
               selectInput(ns("Dropout"), "Early Dropout", c("", "Yes", "No"), width = '200px')
             )
           
    )
    
    
    
  })
  
  
  output$analytics_tabwidgets_out<- renderUI({
    
    analytics_tabwidgets_reac()
    
  })
  
  
}



ui <- fluidPage(
  
  
  tabPanel("Set Client Profile",
           fluidPage(
             br(),
             titlePanel(span(tagList(icon("bar-chart-o", lib = "font-awesome")), h4(tags$b("Enter client information")))),
             br(),
             h4(tags$strong("Step 1.")), h4("Is this a new or existing client? Tick one of the boxes below:"),
             radioButtons("client_type", "", choices = c("New client, first time completing a scale" = "new", 
                                                         "Ongoing client, has completed one or more scales" = "old")),
             sidebarLayout(position = "left",
                           sidebarPanel(width = 5,
                                        analytics_tabpanel_UI("analytics_id")
                                        
                           ), 
                           mainPanel(width = 7
                                     
                           )
             )))
  
)

server <- function(input, output, session) {
  
  client_type<- reactive({ input$client_type })
  
  callModule(analytics_tabpanel, "analytics_id", client_type)
  
}

shinyApp(ui, server)