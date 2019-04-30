#' Apply Initial Settings Through Population Selection
#'
#' Select an initial population thereby applying settings for analyses
#'
#' @param id String to create the namespace
#'
#' @export


apply_initial_population_UI<- function(id) {

  ns <- NS(id)

  tagList(

    fluidPage(

    titlePanel(span(tagList(icon("edit", lib = "font-awesome", class = "far fa-edit"),
                            h4(tags$b("Complete the questionnaire below and and click"),
                               tags$code("Submit.", style = "color:#d35400"))))),


    column(width = 12, checkboxGroupInput(ns("first_time_scale_completion"), "", width = "100%", #Checking the 'first' box should trigger prompt to select a population
                                          choices = c("*Please indicate if your client is completing this questionnaire for the first time." = "first"))),

    conditionalPanel(condition = "input.first_time_scale_completion == 'first'", ns = ns,

                     tagList(
                         fluidRow(
                           column(width = 8, offset = 2, HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),h4(tags$strong("Select A Group With Similar Characteristics To Your Client")) %>%
                                    helper( type = "inline", title = "Why select a client group?", colour = "#d35400",
                                            content = c("<b>Choosing a client group ensures that:</b>",
          "<b>1.</b> Appropriate research statistics (e.g. means, standard deviations) are used to estimate
          measurement error when calculating changes in scores over time.",
          "<b>2.</b> Suitable symptom severity classifications appear in your client's clinical report. "), size = "m"
                                    ))),

    fluidRow(
      column(width = 8, offset = 3,

      uiOutput(ns("select_population"))

    )),

    fluidRow(
      column(width = 8,

             h4(tags$strong("You can create a new client group and/or customise settings for analyses."))

             ),

      column(width = 4,

             actionButton(ns("go_custom_settings"), "Go To Client Settings", class = "submit_data")

             ))))))

}



#' Apply Initial Settings Through Population Selection
#'
#' Select an initial population thereby applying settings for analyses
#'
#' @param id String to create the namespace
#'
#' @export


apply_initial_population<- function(input, output, session, title, brief_title, measure, subscale, population_quantity, populations, sds, means,
                                    mean_sd_references, reliabilities, reliability_references, cutoff_values, cutoff_labels, cutoff_references, cutoff_quantity,
                                    items, max_score, min_score, description, existing_data, tabsetpanel_id = "tabset") {

  parent_session <- get("session", envir = parent.frame(2)) #Need to ensure correct scoping - want R to look in the parent app not the module


  observeEvent(input$go_custom_settings, {
    updateTabsetPanel(session = parent_session, tabsetpanel_id,  #Direct user to new tab upon button click
                      selected = paste("go_custom_settings"))
  })




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


