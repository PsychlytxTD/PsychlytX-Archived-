#' Cutoff widget module
#'
#' Generates the widget with correct default values for the reliability widget and references.
#'
#' @param panel_name string (white space allowed) indicating the name of the subscale, to be used as a panel title.
#'
#' @param subscale_name A string (underscores should replace white space) indicating the name of the subscale for which the function is being used (e.g. "Anxiety").
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
#' @param cutoffs A list of concatenated numeric values representing the cutoff values on this subscale for each population.
#'
#' @param cutoff_names A list of concatenated strings indicating the cutoff value descriptors. Use rep() function to multiple by populations.
#'
#' @param cutoff_references A list of strings indicating the references for each reliability value by population.
#'
#' @param cutoff_quantity A numeric value indicating the number of cutoff scores for the subscale.
#'
#' @export
#'
#'

generate_cutoff_widget_UI <- function(id) {
  ns <- NS(id)

  fluidRow(uiOutput(ns("cutoff_widget_out")))

}



generate_cutoff_widget <-
  function(input,
           output,
           session,
           panel_name,
           subscale_name,
           population_quantity,
           populations,
           input_population,
           sds,
           means,
           mean_sd_references,
           reliabilities,
           reliability_references,
           cutoffs,
           cutoff_names,
           cutoff_references,
           cutoff_quantity) {
    cutoff_widget_reac <- reactive({
      ns <- session$ns

      cutoff_widget_list <-

        purrr::pmap(params_list_maker(
          subscale_name = subscale_name,
          population_quantity = population_quantity,
          populations = populations,
          input_population = input_population(),
          means = means,
          sds = sds,
          mean_sd_references = mean_sd_references,
          reliabilities = reliabilities,
          reliability_references = reliability_references,
          cutoffs = cutoffs,
          cutoff_names = cutoff_names,
          cutoff_references = cutoff_references,
          cutoff_quantity = cutoff_quantity
        )[c(8, 9, 10, 11, 12)],

        function(cutoff_ids,
                 cutoff_name_ids,
                 cutoffs,
                 cutoff_names,
                 cutoff_references) {
          div(
          column(
            width = 2,
            textInput(
              inputId = cutoff_name_ids,
              label = h4(tags$strong(panel_name)),
              value = cutoff_names
            ),
            numericInput(
              inputId = cutoff_ids,
              label = "",
              value = cutoffs
            ),
            h6(paste("Reference:", cutoff_references))
          ))

        })

      do.call(tagList, list(cutoff_widget_list))

    })


    output$cutoff_widget_out <- renderUI({
      cutoff_widget_reac()

    })

  }
