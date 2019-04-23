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


fluidRow(

  column(width = 4,

         textInput(ns('item_scores'), 'Inputted Item Scores', "0,1,2,etc")),



  column(width = 4,

         dateInput(ns("date"), "Date", format = "dd/mm/yyyy")

  ))


)



}


#' Manual Entry Widgets
#'
#' Creates layout and widgets for manual data entry
#'
#' @param scale_entry_scores The item scores passed from the online scale (if completed).
#'
#' @export



#The scale_entry_scores object refers to the item scores from the online scale (if completed)

manual_data<- function(input, output, session, scale_entry, input_submit_responses) {

  observe({

    updateTextInput(session, "item_scores", value = scale_entry()) #If the online scale was completed, pass these item scores directly into
                                                                     #the manually-entry field. Thus, item scores always come from one place
                                                                     #when used for subsequent processing.

  })

  #Return manually entered items scores and date as seperate objects in a list, for further analyis



    date<- reactive({ input$date })

    item_scores<- reactive({ as.numeric(unlist(strsplit(input$item_scores, ","))) })   #Collect item scores and store in vector

     eventReactive(input_submit_responses(), { list( date = date(), item_scores = item_scores() ) })



}

