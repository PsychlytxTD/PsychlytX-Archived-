#' Population Selection
#'
#'Selected a population matching client characteristics
#'
#' @param id A string to create the namespace
#'
#' @export


select_population_UI<- function(id) {

  ns<- NS(id)

  tagList(

  fluidPage(

  fluidRow(

  column(width = 8, offset = 2,

         HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), h4("Select A Group With Similar Characteristics To Your Client") %>%  helper(type = "inline",
                                                                                    title = "Why select a reference population?",
                                                                                    colour = "#d35400",
                                                                                    content = c("<b>Choosing a client group is required so that:</b>",
                                                                                                "<b>1.</b> Appropriate research statistics can be automatically applied in reliable change
                                                                    calculations. You can also define a new client group (specific to your client) and add suitable statistical values below.",
                                                                                                "<b>2.</b> Comparisons can easily be made between your client's score on a questionnaire
                                                                    and scores that are normative within the population to which he or she belongs. Comparison
                                                                    scores are displayed in your client's clinical report."),
                                                                                    size = "m"))),
fluidRow(

  column(width = 8, offset = 3,

   uiOutput(ns("select_population"))

  )
)

  ))

}


#' Update Population
#'
#' Update population selection widget with value chosen at registration
#'
#' @param selected_client_data Data for the selected patient pulled from the db. The required value is selected_client_data()$population
#'
#' @export

select_population<- function(input, output, session, title, brief_title, measure, subscale, population_quantity, populations, sds, means,
                             mean_sd_references, reliabilities, reliability_references, cutoff_values, cutoff_labels, cutoff_references, cutoff_quantity,
                             items, max_score, min_score, description, existing_data) {



  output$select_population<- renderUI({

    ns <- session$ns

    population_labels<- purrr::map(populations, ~ gsub("_", " ", .x))

    population_list<- purrr::set_names(populations, population_labels)


    selectInput(ns("population"), "",
                choices = population_list, width = "60%")

  })

  outputOptions(output, "select_population", suspendWhenHidden = FALSE)



  observe({

    updateSelectInput(session, "population", selected = existing_data()$population)

  })


reactive({  input$population  })



}
