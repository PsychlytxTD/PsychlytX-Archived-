#' Make Client Dropdown
#'
#' Render the client dropdown menu
#'
#' @param id A string to create the namespace
#'
#' @export


make_client_dropdown_UI<- function(id) {

  ns <- NS(id)

  tagList(

  uiOutput(ns("client_dropdown")),

  verbatimTextOutput("existing_client_status")

  )


}


#' Make Client Dropdown
#'
#' Render the client dropdown menu
#'
#' @param client_list A reactive list of clients pulled from db
#'
#' @export


make_client_dropdown<- function(input, output, session, client_list) {

  output$client_dropdown<- renderUI({

    ns <- session$ns

    selectInput(
      inputId = ns("client_selection"),
      label = "Find Your Client",
      choices = client_list(),
      selectize = FALSE)


  })


  output$existing_client_status<- renderText({

    if(length(client_list()) >= 1) {

      ""
    } else {

      "Please register a client."

    }

  })

  reactive({ input$client_selection })

}
