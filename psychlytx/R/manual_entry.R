#' Manual Data Entry Widgets
#'
#' Creates layout and widgets for manual data entry
#'
#' @param id String to create a unique namespace.
#'
#' @export


manual_data_UI<- function(id) {

  ns<- NS(id) #Set the namespace

  tagList(

    checkboxInput("manual_entry", "Enter Data Manually"), #Need to check box for manual entry drop-down fields


conditionalPanel(condition = "input.manual_entry",

tagList(

fluidRow(
  column(width = 12,
         titlePanel(span(tagList(icon("edit", lib = "font-awesome")), h4(tags$b("Enter Item Scores")),
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

  column(width = 4,
         textInput(ns('item_scores'), 'Item Scores', "0,1,2,etc")),



  column(width = 4,
         dateInput(ns("date"), "Date of Scale Completion", format = "dd/mm/yyyy")

  )))

))

}


#' Manual Entry Widgets
#'
#' Creates layout and widgets for manual data entry
#'
#' @param scale_entry_scores The item scores passed from the online scale (if completed).
#'
#' @export



#The scale_entry_scores object refers to the item scores from the online scale (if completed)

manual_data<- function(input, output, session, scale_entry_scores) {

  observe({

    updateTextInput(session, "manual_score", value = scale_entry_scores()) #If the online scale was completed, pass these item scores directly into
                                                                     #the manually-entry field. Thus, item scores always come from one place
                                                                     #when used for subsequent processing.

  })

  #Return manually entered items scores and date as seperate objects in a list, for further analyis

  current_data<- reactive({

    date<- format(as.Date(input$date), "%d/%m/%Y") #Make sure date has date class

    item_scores<- as.numeric(unlist(strsplit(input$item_scores, ",")))   #Collect item scores and store in vector

    list(date = input$date, item_scores = input$item_scores)


    })

}

