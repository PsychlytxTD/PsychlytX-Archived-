#' Manual Entry Widgets
#'
#' Creates layout and widgets for manual data entry
#'
#' @param id String to create a unique namespace.
#'
#' @export


manual_data_UI<- function(id) {

  ns<- NS(id)

  tagList(

    checkboxInput("manual_entry", "Enter Data Manually"),


conditionalPanel(condition = "input.manual_entry",

tagList(

fluidRow(
  column(width = 12,
         titlePanel(span(tagList(icon("edit", lib = "font-awesome")), h4(tags$b("Enter Scores for Each Timepoint")),
                         tags$ul(
                           tags$li(helpText(h5(tags$em(tags$b("Use commas to separate scores. Enter scores in order, from the first to the last item of the total scale.", style = "color:black")))))
                         )
         ))
  )
),
br(),
br(),
br(),
fluidRow(
  column(4,

         selectInput("timepoint", "Select Number of Timepoints for New Data Entry", choices = c("1", "2", "3"))

  ),

  column(4,
         textInput(ns('text_1'), '1st Data Entry', "0,1,2,etc"),
         textInput(ns('text_2'), '2nd Data Entry', "0,1,2,etc"),
         textInput(ns('text_3'), '3rd Data Entry', "0,1,2,etc")

  ),



  column(4,
         dateInput(ns("date_1"), "Date of 1st Data Entry", format = "dd/mm/yyyy"),
         dateInput(ns("date_2"), "Date of 2nd Data Entry", format = "dd/mm/yyyy"),
         dateInput(ns("date_3"), "Date of 3rd Data Entry", format = "dd/mm/yyyy")
  )))

))

}


#' Manual Entry Widgets
#'
#' Creates layout and widgets for manual data entry
#'
#'
#' @export




manual_data<- function(input, output, session, scale_entry_scores) {

  observe({

    updateTextInput(session, "text_1", value = scale_entry_scores())

  })

}

