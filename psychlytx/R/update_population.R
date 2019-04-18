#' Population Selection
#'
#'Selected a population matching client characteristics
#'
#' @param id A string to create the namespace
#'
#' @export


select_population_UI<- function(id) {

  ns<- NS(id)

  uiOutput(ns("select_population"))

}


#' Update Population
#'
#' Update population selection widget with value chosen at registration
#'
#' @param selected_client_data Data for the selected patient pulled from the db. The required value is selected_client_data()$population
#'
#' @export

select_population<- function(input, output, session, title, measure, subscale, population_quantity, populations, sds, means,
                             mean_sd_references, reliabilities, reliability_references, cutoff_values, cutoff_labels, cutoff_references, cutoff_quantity,
                             items, max_score, min_score, description) {



  output$select_population<- renderUI({

    ns <- session$ns

    population_labels<- purrr::map(populations, ~ gsub("_", " ", .x))

    population_list<- purrr::set_names(populations, population_labels)


    selectInput(ns("population"), "To which population does your client most closely belong?",
                choices = population_list )

  })

  outputOptions(output, "select_population", suspendWhenHidden = FALSE)


reactive({  input$population  })



}
