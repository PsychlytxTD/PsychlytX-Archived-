#' Simplified Cutoff Widget
#'
#' Generates the simplified application cutoff widget
#'
#' @param id String to create a unique namespace.
#'
#' @export


generate_simplified_cutoff_widget_UI <- function(id) {

  ns <- NS(id) #Set the namespace

  fluidRow(uiOutput(ns("simplified_cutoff_widget_out")))

}



#' Simplified Cutoff Widget
#'
#' Generates the simplified application cutoff widget
#'
#' @param subscale_number A number identifiying the subscale (i.e. which row of statistics in the holding table should be selected).
#'
#' @param holding_data A dataframe representing the client's existing available data for this measure.
#'
#' @export


#Subscale list parameters (mostly lists themselves) are arguments to the module function.

generate_simplified_cutoff_widget <-function(input, output, session, subscale_number, holding_data) {


  #Render the widgets

  output$simplified_cutoff_widget_out <- renderUI({

    ns <- session$ns #Set the namespace

    div(
      column(width = 2,

             textInput(inputId = ns("cutoff_label_id_1"), label = "Description", value = holding_data()$cutoff_label_1[subscale_number]),

             numericInput(inputId = ns("cutoff_value_id_1"), label = "Value", value = holding_data()$cutoff_value_1[subscale_number]),

             textInput(inputId = ns("cutoff_reference_id_1"), label = "Reference", value = holding_data()$cutoff_reference_1[subscale_number]),

             hr()

      ),
      column(width = 2,

             textInput(inputId = ns("cutoff_label_id_2"), label = "Description", value = holding_data()$cutoff_label_2[subscale_number]),

             numericInput(inputId = ns("cutoff_value_id_2"), label = "Value", value = holding_data()$cutoff_value_2[subscale_number]),

             textInput(inputId = ns("cutoff_reference_id_2"), label = "Reference", value = holding_data()$cutoff_reference_2[subscale_number]),

             hr()

      ),
      column(width = 2,

             textInput(inputId = ns("cutoff_label_id_3"), label = "Description", value = holding_data()$cutoff_label_3[subscale_number]),

             numericInput(inputId = ns("cutoff_value_id_3"), label = "Value", value = holding_data()$cutoff_value_3[subscale_number]),

             textInput(inputId = ns("cutoff_reference_id_3"), label = "Reference", value = holding_data()$cutoff_reference_3[subscale_number]),

             hr()

      ),
      column(width = 2,

             textInput(inputId = ns("cutoff_label_id_4"), label = "Description", value = holding_data()$cutoff_label_4[subscale_number]),

             numericInput(inputId = ns("cutoff_value_id_4"), label = "Value", value = holding_data()$cutoff_value_4[subscale_number]),

             textInput(inputId = ns("cutoff_reference_id_4"), label = "Reference", value = holding_data()$cutoff_reference_4[subscale_number]),

             hr()

      ),
      column(width = 2,

             textInput(inputId = ns("cutoff_label_id_5"), label = "Description", value = holding_data()$cutoff_label_5[subscale_number]),

             numericInput(inputId = ns("cutoff_value_id_5"), label = "Value", value = holding_data()$cutoff_value_5[subscale_number]),

             textInput(inputId = ns("cutoff_reference_id_5"), label = "Reference", value = holding_data()$cutoff_reference_5[subscale_number]),

             hr()

      )



    )


  })

  #Make sure the values for the mean widgets are accessible even if tab is not clicked

  outputOptions(output, "simplified_cutoff_widget_out", suspendWhenHidden = FALSE)


  #Need to finish the module with reactive value list containing id of value widget & reference widget - so these can be accessed by another module.

  reactive({    #use the param_list_maker() function to access all the needed cutoff widget ids (regardless)
    #of how many there are

    list( req(input$cutoff_label_id_1), req(input$cutoff_label_id_2), req(input$cutoff_label_id_3), req(input$cutoff_label_id_4), req(input$cutoff_label_id_5),
          req(input$cutoff_value_id_1), req(input$cutoff_value_id_2), req(input$cutoff_value_id_3), req(input$cutoff_value_id_4), req(input$cutoff_value_id_5),
          req(input$cutoff_reference_id_1), req(input$cutoff_reference_id_2), req(input$cutoff_reference_id_3),
          req(input$cutoff_reference_id_4), req(input$cutoff_reference_id_5) )

  })



}