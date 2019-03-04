#' Reliable Change Method & Custom Confidence Intervals
#'
#' Generates widgets to select reliable change method and custom confidence interval width (if custom intervals are selected)
#'
#' @param id String to create a unique namespace.
#'
#' @export

method_widget_UI<- function(id) {

  ns<- NS(id) #Set the namespace

  tagList( #Create a tag list containing the widgets for reliable change method selection and confidence interval width selection

  br(),

  radioButtons(ns("reliable_change_method"), h4(tags$strong("Select Reliable Change Method")), choices = c("Nunnally & Bernstein (1994)", "Jacobson & Truax (1991)"))

)


}


#' Reliable Change Method & Custom Confidence Intervals
#'
#' Generates widgets to select reliable change method and custom confidence interval width (if custom intervals are selected)
#'
#' @export



method_widget<- function(input, output, session) { #In the parent app, need to pass a character vector of subscale names
                                                                      #(underscore replacing white space) in callModule()

  reactive({ list( req(input$reliable_change_method) ) })


}
