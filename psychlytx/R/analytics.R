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
    radioButtons(ns("client_type"), "", choices = c("Register New Client" = "new",
                                                    "Select Existing Client" = "old"))

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

  tagList(
    sidebarLayout(
      sidebarPanel(width = 12,
  uiOutput(ns("analytics_widgets_out")),

  useShinyjs(),
  radioButtons(ns("show_post_analytics"), "Are you administering a scale to this client for the last time?", choices = c("No", "Yes"))
      ),
  mainPanel())

  )

}



#' Tabpanel widget content for analytics
#'
#' Module generates widgets for entry of basic demographic and clinical information.
#'
#' @param id String to create a unique namespace.
#'
#' @param clientstatus_module A module object containing the new/existing client status widget and headings.
#'
#' @export

analytics_widgets<- function(input, output, session, clientstatus_module) {


  analytics_widgets_reac<- reactive({

    ns <- session$ns

    #Access the client status module object with the $ convention, not with ().
    #Would use the () convention - i.e. client_type() - if accessing inputs from the parent app.

    #Show a sidebar with different questions, depending on whether client is new or existing. The idea is that
    #demographic information is provided at the start (with new clients) and clinical information (with existing clients)
    #is provided at the end of treatment (e.g. sessions attended, attendance quality etc.).

    switch(clientstatus_module$client_type,

           new =

             tagList(
               shinyjs::hide("show_post_analytics"),
               h3(tags$strong("Pre-Treatment Questions")),
               br(),
               textInput(ns("control_name"), "First Name"),
               textInput(ns("control_name"), "Last Name"),
               selectInput(ns("sex"), "Sex", c("", "Male", "Female", "Other")),
               dateInput(ns("dob"), "Date of Birth", value = ""),
               textInput(ns("suburb"), "Postcode", value = ""),
               selectInput(ns("marital_status"), "Marital Status", c("", "Never Married", "Currently Married", "Separated", "Divorced", "Widowed", "Cohabiting")),
               selectInput(ns("sexuality"), "Sexual Orientation", c("", "Heterosexual", "Lesbian", "Gay", "Bisexual", "Transgender", "Queer", "Other")),
               selectInput(ns("ethnicity"), "Ethnicity", c("", "Caucasian", "Latino/Hispanic", "Middle Eastern", "African", "Caribbean", "South Asian",
                                                           "East Asian", "Mixed", "Other")),
               radioButtons(ns("indigenous"), "Identifies as Being of Aboriginal or Torres Strait Islander Descent", choices = c("No", "Yes")),
               numericInput(ns("children"), "Number of Dependent Children", value = ""),
               selectInput(ns("workforce"), "Primary Workforce Status", c("", "Working Full-Time", "Working Part-Time", "Working Casual Hours", "Studying", "Unemployed", "Retired")),
               selectInput(ns("education"), "Highest Education Level", c("", "No education", "Primary Education", "Secondary Education", "Post-Secondary/Tertiary Education",
                                                           "Bachelor or Equivalent", "Master or Equivalent", "Doctoral or Equivalent"))
             ),

           old =

             tagList(

               selectInput(ns("existing_client"), "Select Your Client", choices = c("")),

               shinyjs::show("show_post_analytics"),

               if(input$show_post_analytics == "Yes") {

                 tagList(
                   h3(tags$strong("Post-Treatment Questions")),
                   br(),
                   selectInput(ns("principal_diagnosis"), "Presenting Principal Diagnosis", psychlytx::diagnosis_list),
                   selectizeInput(ns("secondary_diagnosis"), "Additional Presenting Diagnosis/Diagnoses", psychlytx::diagnosis_list, multiple = TRUE),
                   textInput(ns("referrer"), "Referrer", value = ""),
                   selectInput(ns("attendance_arrangement"), "Schedule of Attendance", c("", "Varied", "Twice A Week", "Once A Week", "Once a Fortnight", "Once Every 3 Weeks", "Once A Month", "Greater Than 1 Month Apart")),
                   numericInput(ns("dna"), "Number of Non-Attendances (DNAs)", value = ""),
                   numericInput(ns("duration"), "Number of Sessions Attended", value = ""),
                   selectInput(ns("dropout"), "Premature Dropout", c("", "Yes", "No")),
                   selectInput(ns("therapy"), "Therapeutic Approach Used", psychlytx::therapies_list),
                   selectInput(ns("funder"), "Funding Source", choices = c("", "Entirely Self-Funded", "Medicare", "Private Health Fund",
                                                                           "WorkCover", "Transport Accident Commission (TAC)",
                                                                           "Department of Veterans Affairs (DVA)",
                                                                           "Victims of Crime Assistance Tribunal (VOCAT)",
                                                                           "Other")),
                   numericInput(ns("out_of_pocket"), "Out-Of-Pocket Expense", value = "")

             )

               }

             )


    )



  })


  #Render the andalytics widgets

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

analytics_newcustom_widgets_UI<- function(id) {

  ns<- NS(id)

  tagList(
    fluidRow(
      column(width = 12,
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
  )
)
}


#' Dynamic new customised widgets
#'
#' Module generates widgets for custom variables, defined by the user.
#'
#' @param id String to create a unique namespace.
#'
#' @export

analytics_newcustom_widgets<- function(input, output, session) {

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

