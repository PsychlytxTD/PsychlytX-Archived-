#' Confidence Level
#'
#' Generates a confidence interval widget
#'
#' @param id String to create a unique namespace.
#'
#' @export

confidence_level_UI <- function(id) {

  ns <- NS(id) #Set the namespace

  #Create a widget allowing the user to select the level of confidence for intervals

  tagList(fluidRow(column(width = 12, offset = 4,
                          radioButtons(ns("confidence"), label = h4(tags$strong("Interval Confidence Level")), choices = list("99%" = 1,
                          "95%" = 2, "90%" = 3), selected = 2, inline = T)

  )))

}



#' Confidence Level
#'
#' Generates a confidence interval widget
#'
#' @export

confidence_level<- function(input, output, session) {

  reactive({

    req(input$confidence)

  if(input$confidence == "1") {

     1.645

  } else if(input$confidence == "2") {

    1.96

  } else {

    2.575
  }



})

}

