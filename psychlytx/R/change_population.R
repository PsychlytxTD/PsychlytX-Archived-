#' Update population
#'
#' Update population widget
#'
#' @param id String to create the namespace
#'
#' @export


initiate_settings_UI<- function(id) {

  ns <- NS(id)

  tagList(

    titlePanel(span(tagList(icon("edit", lib = "font-awesome", class = "far fa-edit"),
                            h4(tags$b("Complete the questionnaire below and and click"),
                               tags$code("Submit.", style = "color:#d35400"))))),

    column(width = 12, checkboxGroupInput(ns("first_time_scale_completion"), "", width = "100%",
                                          choices = c("My client is completing a questionnaire with PsychlytX for the first time." = "first"))),

    conditionalPanel(condition = "input.first_time_scale_completion == 'first'", ns = ns,

     tagList(

    br(),

    fluidRow(
      column(width = 10, offset = 1,
             htmlOutput(ns("selected_population_message"))
      )),

    br(),

    fluidRow(
      column(width = 12, offset = 1,

             actionButton(ns("update_population"), "Update Client Group", class = "submit_data")
      )),

    br()

    )))

}




#' Update population
#'
#' Update population widget
#'
#' @param input_population String indicating selected population
#'
#' @export

initiate_settings<- function(input, output, session, input_population, tabsetpanel_id = "tabset") {

  parent_session <- get("session", envir = parent.frame(2))

  observeEvent(input$update_population, {
    updateTabsetPanel(session = parent_session, tabsetpanel_id,
                      selected = paste("update_population"))
  })


  output$selected_population_message<- renderText({

    paste(h4("Analyses will be tailored to the following client group:", tags$strong(gsub("_", " ", input_population()))))

  })
}
