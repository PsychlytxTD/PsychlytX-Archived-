#' Population Selection
#'
#'Selected a population matching client characteristics
#'
#' @param id A string to create the namespace
#'
#' @export


select_population_UI<- function(id) {

  ns<- NS(id)

  selectInput(ns("population"), "To which population does your client most closely belong?",
              choices = c("male general population", "female general population", "older adult",
                          "primary care", "psychiatric", "Generalized Anxiety Disorder",
                          "chronic musculoskeletal pain", "coronary heart disease",
                          "type 1 diabetes", "type 2 diabetes", "stroke"))


}


#' Update Population
#'
#' Update population selection widget with value chosen at registration
#'
#' @param selected_client_data Data for the selected patient pulled from the db. The required value is selected_client_data()$population
#'
#' @export

select_population<- function(input, output, session) {


reactive({  input$population  })



}
