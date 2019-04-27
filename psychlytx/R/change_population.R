#' Update population
#'
#' Update population widget
#'
#' @param id String to create the namespace
#'
#' @export


change_population_UI<- function(id) {

  ns <- NS(id)

  tagList(

    br(),

    fluidRow(
      column(width = 10, offset = 2,
             htmlOutput(ns("selected_population_message"))
      )),

    br(),

    fluidRow(
      column(width = 12, offset = 4,
             actionButton(ns("update_population"), "Update Population", class = "submit_data")
      )),

    br()

    )

}




#' Update population
#'
#' Update population widget
#'
#' @param input_population String indicating selected population
#'
#' @export

change_population<- function(input, output, session, input_population, tabsetpanel_id = "tabset") {

  parent_session <- get("session", envir = parent.frame(2))

  observeEvent(input$update_population, {
    updateTabsetPanel(session = parent_session, tabsetpanel_id,
                      selected = paste("update_population"))
  })


  output$selected_population_message<- renderText({

    paste(tags$code("The reference population you have selected is:", gsub("_", " ", input_population()), style = "color:#283747"))

  })
}
