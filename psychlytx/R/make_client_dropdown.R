#' Make Client Dropdown
#'
#' Render the client dropdown menu
#'
#' @param id A string to create the namespace
#'
#' @export


make_client_dropdown_UI<- function(id) {

  ns <- NS(id)

  uiOutput(ns("client_dropdown"))


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

    validate(need(length(client_list()) >= 1, "You do not yet have any existing clients. Please register a client."))

    ns <- session$ns


    selectInput(
      inputId = ns("client_selection"),
      label = "Find Your Client",
      choices = client_list(),
      selectize = FALSE)

  })

  reactive({ input$client_selection })

}
