#' Display Selected Client Data
#'
#' Display abbreviated score information for selected client
#'
#' @param id A string to create the namespace
#'
#' @export


display_selected_client_scores_UI<- function(id) {

  ns <- NS(id)

  tagList(


    DT::dataTableOutput("selected_client_data_out"),

    verbatimTextOutput("client_selection_message")

  )


}


#' Display Selected Client Data
#'
#' Display abbreviated score information for selected client
#'
#' @param selected_client_data A dataframe displaying abbreviated score information for client selected, pulled from db.
#'
#' @export
#'

display_selected_client_scores<- function(input, output, session, selected_client_data) {



  output$selected_client_data_out<- DT::renderDataTable({


    DT::datatable(

      selected_client_data(),
      extensions = 'Scroller', rownames = FALSE,
      options = list(initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#827717', 'color': '#fff'});",
        "}"), deferRender = TRUE, scrollY = 200, scroller = TRUE, dom = "t" )

    )


  })


  output$client_selection_message<- renderText({


    if(length(selected_client_data() >= 1)) {

      "Client selected."

    } else {

      "No data to show yet for this client."

    }


  })



}
