#' Reliability widget module
#'
#' Generates the widget with correct default values for the reliability widget and references.
#'
#' @param id String to create a unique namespace.
#'
#' @export


#Create a widget for test-retest reliability

generate_reliability_widget_UI <- function(id) {

#Set the namespace

  ns <- NS(id)

#Dynamically-generated widget

  fluidRow(uiOutput(ns("reliability_widget_out")))

}


#' Reliability widget module
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


#Subscale list parameters (mostly lists themselves) are arguments to the module function.

generate_reliability_widget <- function(input, output, session, panel_name, subscale_name, population_quantity, populations, input_population, sds, means, mean_sd_references, reliabilities,
           reliability_references, cutoffs, cutoff_names, cutoff_references, cutoff_quantity) {



     reliability_widget_reac <- reactive({



      ns <- session$ns  #Set the namespace

       #Widgets will be stored in this list before being called in do.call()

       reliability_widget_list <-

        #Params_list_maker() creates a list of lists, each containing parameters corresponding to a different population
        #It will return a single list matching the population selected by the user

        purrr::pmap(params_list_maker(
          subscale_name = subscale_name,
          population_quantity = population_quantity,
          populations = populations,
          input_population = input_population(), #input_population() is the population reactive object selected from the selectInput widget in the parent app
          means = means,
          sds = sds,
          mean_sd_references = mean_sd_references,
          reliabilities = reliabilities,
          reliability_references = reliability_references,
          cutoffs = cutoffs,
          cutoff_names = cutoff_names,
          cutoff_references = cutoff_references,
          cutoff_quantity = cutoff_quantity
        )[c(1, 6, 7, 13)], #Within the list, iterate only over the parameters  relevant to generating the reliability widget

       #Set these relevant parameters as function arguments (need to do this or the code won't run)

        function(mean_sd_rel_ids, reliabilities, reliability_references, mean_sd_rel_reference_ids) {

         #Create a div containing the dynamically generated widgets

          div(column(width = 2,

            numericInput(inputId = ns(mean_sd_rel_ids), label = h4(tags$strong(panel_name)), value = reliabilities),

            textInput(inputId = ns(mean_sd_rel_reference_ids),label = "Reference", value = mean_sd_references)

          ))

        })


      do.call(tagList, list(reliability_widget_list))

    })



   #Render the widgets

    output$reliability_widget_out <- renderUI({

       reliability_widget_reac()

    })

    #Make sure the values for the mean widgets are accessible even if tab is not clicked

    outputOptions(output, "reliability_widget_out", suspendWhenHidden = FALSE)

    #Need to finish the module with reactive value list containing id of value widget & reference widget - so these can be accessed by another module.
    #Add req() so the input values aren't NULL initially

    reactive({ list( req(input$mean_sd_rel_value_id), req(input$mean_sd_rel_reference_id) ) })


  }
