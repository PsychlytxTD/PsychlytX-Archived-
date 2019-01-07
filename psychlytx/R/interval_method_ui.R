#' Reliable Change Method
#'
#' Generates widget to select reliable change method
#'
#' @param id String to create a unique namespace.
#'
#' @export

interval_widgets_UI<- function(id) {

  ns<- NS(id)

  tagList(

  selectInput(ns("reliable_change_method"), "", choices = c("Nunnally & Bernstein (1994)", "Jacobson & Truax (1991)","Speer (1992)")),
  selectInput(ns("manual_interval"), "Customise the width of confidence intervals", choices = c("No", "Yes")),

  uiOutput(ns("interval_widgets_out"))


         )


}




interval_widgets<- function(input, output, session, subscale_names) {

  ns <- session$ns

  interval_widgets_reac<- reactive({

    #Necessary when rendering UI object with modules.

    widget_list <- lapply(seq_along(subscale_names), function(i) {

      id_label  <- subscale_names[i]

      div(
        column(width = 2,
               numericInput(ns(id_label), id_label, value = 0)
        )
      )

    }
    )

    do.call(tagList, widget_list)


  })


  #Render the widgets.

  output$interval_widgets_out<- renderUI({

    if(input$manual_interval == "Yes")  interval_widgets_reac()


  })



}
