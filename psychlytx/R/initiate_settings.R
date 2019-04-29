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

    column(width = 12, h5("*Before proceeding, please check one of the boxes below if applicable.")),

    column(width = 12, checkboxGroupInput(ns("first_time_scale_completion"), "", width = "100%", #Checking the 'first' box should trigger prompt to select a population
                                          choices = c("My client is completing this questionnaire for the first time." = "first"))),

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
#' @param tabsetpanel_id A string indicating the id of the tab that the client is directed to.
#'
#' @export

initiate_settings<- function(input, output, session, input_population, tabsetpanel_id = "tabset") {

  parent_session <- get("session", envir = parent.frame(2)) #Need to ensure correct scoping - want R to look in the parent app not the module

  observeEvent(input$update_population, {
    updateTabsetPanel(session = parent_session, tabsetpanel_id,  #Direct user to new tab upon button click
                      selected = paste("update_population"))
  })


  output$selected_population_message<- renderText({

    paste(h4("Analyses will be tailored to the following client group:", tags$strong(gsub("_", " ", input_population())))) #Send message about population selection

  })
}
