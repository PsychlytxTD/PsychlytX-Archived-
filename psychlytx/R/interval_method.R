#' Reliable Change Method & Custom Confidence Intervals
#'
#' Generates widgets to select reliable change method and custom confidence interval width (if custom intervals are selected)
#'
#' @param id String to create a unique namespace.
#'
#' @export

interval_widgets_UI<- function(id) {

  ns<- NS(id) #Set the namespace

  tagList( #Create a tag list containing the widgets for reliable change method selection and confidence interval width selection

  br(),

  checkboxGroupInput(ns("reliable_change_method"), "Select Reliable Change Method", choices = c("Nunnally & Bernstein (1994)", "Jacobson & Truax (1991)", "Speer (1992)",
                                                                                                "Custom Confidence Intervals")),

  uiOutput(ns("interval_widgets_out"))


         )


}


#' Reliable Change Method & Custom Confidence Intervals
#'
#' Generates widgets to select reliable change method and custom confidence interval width (if custom intervals are selected)
#'
#' @param subscale_names A vector of character strings representing the names of subscales (underscores should replace white space between words).
#'
#' @export



interval_widgets<- function(input, output, session, subscale_names) { #In the parent app, need to pass a character vector of subscale names
                                                                      #(underscore replacing white space) in callModule()

  ns <- session$ns #Set the namespace


  interval_widgets_reac<- reactive({


    widget_list <- lapply(seq_along(subscale_names), function(i) {

      interval_id  <- subscale_names[i]

      div(
        column(width = 2,
               numericInput(ns(interval_id), interval_id, value = 0) #Create a dynamic number of numericInput fields to specify custom confidence interval widgth,
                                                                     #based on the number of subscales the scale has
        )
      )

    }
    )

    do.call(tagList, widget_list)


  })


  #Render the widgets.

  output$interval_widgets_out<- renderUI({

    #Render the field(s) for custom interval width specification if custom confidence
    #interval method is selected

    req(input$reliable_change_method) #Avoid error message by not rendering widget unless input value has been specified

    interval_widgets_reac() #Render the field for specifiying custom interval length

  })



}
