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

  column(width = 7, titlePanel(span(tagList(icon("file-pdf-o", lib = "font-awesome")),
                  h3(tags$b("Download Your Client's Clinical Report"))))),

  br(),

  column(width = 7, h4("Select A Population With Similar Characteristics To Your Client") %>%  helper(type = "inline",
                                                                                    title = "Why select a reference population?",
                                                                                    colour = "#d35400",
                                                                                    content = c("<b>Choosing a reference population is required so that:</b>",
                                                                                                "<b>1.</b> Appropriate research statistics can be automatically applied in reliable change
                                                                    calculations. Of course, you are free to alter/customise these statistics yourself by
                                                                    navigating to <code style='color:#d35400;'>Customisation</code>.",
                                                                                                "<b>2.</b> Comparisons can easily be made between your client's score on a questionnaire
                                                                    and scores that are normative within the population to which he or she belongs. Comparison
                                                                    scores (also known as cutoff scores) are displayed on plots in clinical reports."),
                                                                                    size = "m")),

  column(width = 3, offset = 1, uiOutput(ns("select_population")) )

  )

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
                             items, max_score, min_score, description) {



  output$select_population<- renderUI({

    ns <- session$ns

    population_labels<- purrr::map(populations, ~ gsub("_", " ", .x))

    population_list<- purrr::set_names(populations, population_labels)


    selectInput(ns("population"), h4("Reference Population"),
                choices = population_list, width = "100%")

  })

  outputOptions(output, "select_population", suspendWhenHidden = FALSE)


reactive({  input$population  })



}
