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
#' @param title A string (white space allowed) indicating the name of the subscale, to be used as a panel title.
#'
#' @param brief_title An abbreviated title or acronym.
#'
#' @param measure A string indicating the global measure.
#'
#' @param subscale A string (underscores should replace white space) indicating the name of the subscale for which the function is being used (e.g. "Anxiety").
#'
#' @param population_quantity A numeric value of possible populations from which the user can select.
#'
#' @param populations A list of strings (underscores should replace white space) indicating the possible range of populations.
#'
#' @param sds A list of numeric values representing the standard deviations for all populations on that subscale.
#'
#' @param means A list of numeric values representing the means for all populations on that subscale.
#'
#' @param mean_sd_references A list of strings indicating the references for each mean/standard deviation by population.
#'
#' @param reliabilities A list of numeric values representing the test-retest reliabilities for all populations on that subscale.
#'
#' @param reliability_references A list of strings indicating the references for each reliability value by population.
#'
#' @param cutoff_values A list of concatenated numeric values representing the cutoff values on this subscale for each population.
#'
#' @param cutoff_labels A list of concatenated strings indicating the cutoff value descriptors. Use rep() function to multiple by populations.
#'
#' @param cutoff_references A list of strings indicating the references for each reliability value by population.
#'
#' @param cutoff_quantity A numeric value indicating the number of cutoff scores for the subscale.
#'
#' @param items A numeric vector representing an item index for the subscale.
#'
#' @param max_score A numeric value indicating maximum possible score on the subscale.
#'
#' @param min_score A numeric value indicating minimum possible score on the subscale.
#'
#' @param description A description of subscale's properties, to display in report.
#'
#' @param existing_data A dataframe representing the client's existing available data for this measure.
#'
#' @export

select_population<- function(input, output, session, title, brief_title, measure, subscale, population_quantity, populations, sds, means,
                             mean_sd_references, reliabilities, reliability_references, cutoff_values, cutoff_labels, cutoff_references, cutoff_quantity,
                             items, max_score, min_score, description, existing_data) {



  output$select_population<- renderUI({

    ns <- session$ns

    population_labels<- purrr::map(populations, ~ gsub("_", " ", .x)) #The population choices that are visible to users should have no white space

    population_list<- purrr::set_names(populations, population_labels)


    selectInput(ns("population"), "",
                choices = population_list, width = "60%")

  })

  outputOptions(output, "select_population", suspendWhenHidden = FALSE)



  observe({

    updateSelectInput(session, "population", selected = existing_data()$population) #Update the population widget based on user's existing data to reinstill their settings

  })


reactive({  input$population  }) #Return the selected value of the population widget



}
