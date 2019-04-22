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

    fluidRow(
      column(width = 10, offset = 2,
             htmlOutput(ns("selected_population_message"))
      )),

    br(),

    fluidRow(
      column(width = 12, offset = 4,
             actionButton(ns("update_population"), "Update Population", class = "submit_data")
      )),


    fluidRow(

      titlePanel(span(tagList(icon("calculator", lib = "font-awesome")),
                      h4(tags$b("Modify the default values that are used to assess reliable change and symptom severity.")))),

      br(),

      column(width = 8, offset = 3, HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'), tags$code(a("Learn more about customisation",
                                                                                               href = "https:://psychlytx.com.au", style = "color:#d35400")) )

    ),

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
                      selected = paste("report_panel"))
  })


  output$selected_population_message<- renderText({

    paste(tags$code("The reference population you have selected is:", gsub("_", " ", input_population()), style = "color:#283747"))

  })
}
