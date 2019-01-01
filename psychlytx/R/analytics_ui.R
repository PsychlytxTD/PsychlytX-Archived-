#' New or existing client status widget and headings
#'
#' Module generates client status widget and headings.
#'
#' @param id String to create a unique namespace.
#'
#' @export

analytics_clientstatus_UI<- function(id) {

  ns<- NS(id)

  #Generate the widget (and headings) that indicate whether client is new or existing.

  tagList(
    br(),
    titlePanel(span(tagList(icon("bar-chart-o", lib = "font-awesome")), h4(tags$b("Enter client information")))),
    br(),
    h4(tags$strong("Step 1.")), h4("Is this a new or existing client? Select an option below:"),
    radioButtons(ns("client_type"), "", choices = c("New client (first time completing a scale)." = "new",
                                                    "Ongoing client (has already completed one or more scales)." = "old"))

  )

}



#' New or existing client status widget and headings
#'
#' Module generates client status widget and headings.
#'
#' @param id String to create a unique namespace.
#'
#' @export

analytics_clientstatus<- function(input, output, session) {

  #Need to return input to make input parameters available

  return(input)

}



#' Tabpanel widget content for analytics
#'
#' Module generates widgets for entry of basic demographic and clinical information.
#'
#' @param id String to create a unique namespace.
#'
#' @export

analytics_widgets_UI<- function(id) {

  ns<- NS(id)

  uiOutput(ns("analytics_widgets_out"))

}



#' Tabpanel widget content for analytics
#'
#' Module generates widgets for entry of basic demographic and clinical information.
#'
#' @param id String to create a unique namespace.
#'
#' @param client_status_module A module object containing the new/existing client status widget and headings.
#'
#' @export

analytics_widgets<- function(input, output, session, client_status_module) {

  analytics_widgets_reac<- reactive({

    ns <- session$ns

    #Access the client status module object with the $ convention, not with ().
    #Would use the () convention - i.e. client_type() - if accessing inputs from the parent app.

    #Show a sidebar with different questions, depending on whether client is new or existing. The idea is that
    #demographic information is provided at the start (with new clients) and clinical information (with existing clients)
    #is provided at the end of treatment (e.g. sessions attended, attendance quality etc.).

    switch(client_status_modulet$client_type,

           new =

             tagList(
               h5(tags$em("Before scale completion, please answer the questions below:")),
               br(),
               h4(tags$strong("Demographics")),
               br(),
               textInput(ns("control_name"), "Name"),
               selectInput(ns("sex"), "Sex", c("", "Male", "Female"), width = '200px'),
               numericInput(ns("age"), "Age", value = "", width = '100px'),
               selectInput(ns("sexuality"), "Sexual Orientation", c("", "Heterosexual", "Lesbian", "Gay", "Bisexual", "Transgender", "Queer", "Other"), width = '200px'),
               selectInput(ns("relationship"), "Relationship Status", c("", "Married/In Relationship", "Single", "Widowed"), width = '250px'),
               numericInput(ns("children"), "Number of Dependent Children", value = "", width = '100px'),
               selectInput(ns("workforce"), "Primary Workforce Status", c("", "Working", "Studying", "Unemployed", "Retired"), width = '200px'),
               selectInput(ns("education"), "Education", c("", "Did Not Complete High School", "Completed High School", "Completed Tertiary Education"), width = '250px'),
               textInput(ns("occupation"), "Occupation"),
               textInput(ns("suburb"), "Suburb", value = "", width = '300px')
             ),

           old =

             tagList(
               h5(tags$em("Please answer the questions below before your client completes the final/follow-up assessment.")),
               h4(tags$strong("Clinical Information")),
               br(),
               selectInput(ns("principal_diagnosis"), "Presenting Principal Diagnosis", diagnosis_list),
               selectizeInput(ns("secondary_diagnosis"), "Additional Presenting Diagnosis/Diagnoses", diagnosis_list, multiple = TRUE),
               textInput(ns("referrer"), "Referrer", value = "", width = '200px'),
               selectInput(ns("attendance_arrangement"), "Attendance Arrangement", c("", "It Varies", "Twice A Week", "Once A Week", "Once a Fortnight", "Once Every 3 Weeks", "Once A Month", "More Than 1 Month Apart"), width = '250px'),
               selectInput(ns("attendance_quality"), "Quality of Attendance", c("", "Good", "Moderate", "Poor"), width = '200px'),
               textInput(ns("Therapy"), "Therapeutic Approach", value = "", width = '200px'),
               selectInput(ns("Fee"), "Fee Arrangement", c("", "No Out-Of-Pocket Expense", "Discount", "Full-Fee"), width = '250px'),
               numericInput(ns("duration"), "Number of Sessions Attended", value = "", width = '100px'),
               selectInput(ns("dropout"), "Early Dropout", c("", "Yes", "No"), width = '200px')
             )

    )



  })

  output$analytics_widgets_out<- renderUI({

    analytics_widgets_reac()

  })


}



#' Dynamic new customised widgets
#'
#' Module generates widgets for custom variables, defined by the user.
#'
#' @param id String to create a unique namespace.
#'
#' @export

newcustom_widgets_UI<- function(id) {

  ns<- NS(id)

  tagList(
    h4(tags$strong("Create customised variables relevant to data collection in your practice.")),
    br(),
    textInput(ns("newcustom_names"), "Name Your Variables"),
    h5(tags$em("*Separate the names with commas.")),
    h5(tags$em("*E.g. height,weight")),
    actionButton(ns("newcustom_widgets_action"),"Create"),
    br(),
    br(),
    uiOutput(ns("newcustom_widgets_out"))
  )

}


#' Dynamic new customised widgets
#'
#' Module generates widgets for custom variables, defined by the user.
#'
#' @param id String to create a unique namespace.
#'
#' @export

newcustom_widgets<- function(input, output, session) {

  #Produce new textarea widgets when user clicks 'create' (i.e. create new variables).
  #The number of widget depends on how many variables they want to create.

  newcustom_widgets_reac <- eventReactive(input$newcustom_widgets_action,{

    ns <- session$ns

    newcustom_names<- as.character(unlist(strsplit(input$newcustom_names,",")))

    widget_list <- lapply(seq_along(newcustom_names), function(i) {

      id_label<- paste(newcustom_names[i])

      div(
        column(width = 2,
               textAreaInput(ns(id_label),id_label, width = "200px", height = "43px", resize = "vertical")

        )
      )
    }
    )

    do.call(tagList, widget_list)                                 },ignoreInit = T)


  output$newcustom_widgets_out = renderUI({

    newcustom_widgets_reac()

  })


}

