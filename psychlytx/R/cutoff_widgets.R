#' Cutoff widget module
#'
#' Generates the widget with correct default values for the reliability widget and references.
#'
#' @param id String to create a unique namespace.
#'
#' @export


generate_cutoff_widget_UI <- function(id) {

  ns <- NS(id) #Set the namespace

  fluidRow(uiOutput(ns("cutoff_widget_out")))

}



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


#Subscale list parameters (mostly lists themselves) are arguments to the module function.

generate_cutoff_widget <-function(input, output, session, panel_name, subscale_name, population_quantity, populations, input_population, sds,means,
                                  mean_sd_references, reliabilities, reliability_references, cutoffs, cutoff_names, cutoff_references, cutoff_quantity) {

    cutoff_widget_reac <- reactive({

      ns <- session$ns #Set the namespace

      subscale_title<- div(fluidRow(column(width = 2, offset = 5,

      h4(tags$strong(subscale_name)) #The name of the subscale should appear centred, above the widgets

      )))


     #Widgets will be stored in this list before being called in do.call()

      cutoff_widget_list <-

        #Params_list_maker() creates a list of lists, each containing parameters corresponding to a different population
         #It will return a single list matching the population selected by the user

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
        )[c(8, 9, 10, 11, 12, 14)],

       #Set these relevant parameters as function arguments (need to do this or the code won't run).

        function(cutoff_ids, cutoff_name_ids, cutoffs, cutoff_names, cutoff_references, cutoff_reference_ids) {
          div(
          column(width = 2,

            textInput(inputId = ns(cutoff_name_ids), label = "Description", value = cutoff_names),

            numericInput(inputId = ns(cutoff_ids), label = "Value", value = cutoffs),

            textInput(inputId = ns(cutoff_reference_ids), label = "Reference", value = cutoff_references),

            hr()

          ))


        })

      #Call the the subscale name (to appear as heading) and call the div containing the widgets

      do.call(tagList, list(subscale_title, cutoff_widget_list))


    })


    #Render the widgets

    output$cutoff_widget_out <- renderUI({

      cutoff_widget_reac()

    })

    #Make sure the values for the mean widgets are accessible even if tab is not clicked

    outputOptions(output, "cutoff_widget_out", suspendWhenHidden = FALSE)


    #Need to finish the module with reactive value list containing id of value widget & reference widget - so these can be accessed by another module.

    reactive({    #use the param_list_maker() function to access all the needed cutoff widget ids (regardless)
                  #of how many there are

      list( req(input$cutoff_name_id_1), req(input$cutoff_name_id_2), req(input$cutoff_name_id_3), req(input$cutoff_name_id_4), req(input$cutoff_name_id_5),
           req(input$cutoff_value_id_1), req(input$cutoff_value_id_2), req(input$cutoff_value_id_3), req(input$cutoff_value_id_4), req(input$cutoff_value_id_5),
           req(input$cutoff_reference_id_1), req(input$cutoff_reference_id_2), req(input$cutoff_reference_id_3),
          req(input$cutoff_reference_id_4), req(input$cutoff_reference_id_5) )

    })



  }
