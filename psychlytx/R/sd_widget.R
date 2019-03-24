#' Sd widget module
#'
#' Generates the widget with correct default values for the sd widget and references.
#'
#' @param id String to create a unique namespace.
#'
#' @export


generate_sd_widget_UI <- function(id) {

  ns <- NS(id) #Set the namespace

  fluidRow(uiOutput(ns("sd_widget_out")))

}


#' Sd widget module
#'
#' Generates the widget with correct default values for the sd widget and references.
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


#Subscale list parameters (mostly lists themselves) are arguments to the module function.

generate_sd_widget <- function(input, output, session, panel_name, subscale_name, population_quantity, populations, input_population, sds,means,
           mean_sd_references, reliabilities, reliability_references, cutoffs, cutoff_names, cutoff_references, cutoff_quantity, items, max_score, min_score) {

    sd_widget_reac <- reactive({

      ns <- session$ns #Set the namespace

      subscale_title<- div(fluidRow(column(width = 2, offset = 5,

                                           h4(tags$strong(subscale_name)) #The name of the subscale should appear centred, above the widgets

      )))

      #Widgets will be stored in this list before being called in do.call()

      sd_widget_list <-

        #Params_list_maker() creates a list of lists, each containing parameters corresponding to a different population
        #It will return a single list matching the population selected by the user

        purrr::pmap(params_list_maker(
          subscale_name = subscale_name,
          population_quantity = population_quantity,
          populations = populations,
          input_population = input_population(), #input_population()is the population (reactive object) selected from the selectInput widget in the parent app
          means = means,
          sds = sds,
          mean_sd_references = mean_sd_references,
          reliabilities = reliabilities,
          reliability_references = reliability_references,
          cutoffs = cutoffs,
          cutoff_names = cutoff_names,
          cutoff_references = cutoff_references,
          cutoff_quantity = cutoff_quantity
        )[c(1, 4, 5, 13)],

        #Set these relevant parameters as function arguments (need to do this or the code won't run)

        function(mean_sd_rel_ids, sds, mean_sd_references, mean_sd_rel_reference_ids) {

          #Create a div containing the dynamically generated widgets

          div(column(width = 2,

                     numericInput(inputId = ns(mean_sd_rel_ids), label = tags$strong("Sd"), value = sds),

                     textInput(inputId = ns(mean_sd_rel_reference_ids), label = "Reference", value = mean_sd_references),

                     hr()

                     ))

        })

      do.call(tagList, list(subscale_title, sd_widget_list))

    })

    #Render the widgets

    output$sd_widget_out <- renderUI({

      sd_widget_reac()

    })

    #Make sure the values for the sd widgets are accessible even if tab is not clicked

    outputOptions(output, "sd_widget_out", suspendWhenHidden = FALSE)

    #Need to finish the module with reactive value list containing id of value widget & reference widget - so these can be accessed by another module.
    #Add req() so the input values aren't NULL initially

    reactive({ list( req(input$mean_sd_rel_value_id), req(input$mean_sd_rel_reference_id) ) })


  }
