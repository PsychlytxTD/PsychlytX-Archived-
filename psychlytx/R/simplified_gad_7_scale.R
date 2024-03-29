#' Simplified GAD-7 Scale
#'
#' Dynamically enerates the GAD-7 for simplified application.
#'
#' @param id String to create a unique namespace.
#'
#' @export
#'
simplified_gad7_scale_UI<- function(id) {

  ns<- NS(id)

  uiOutput(ns("simplified_gad7_output"))

}

#' GAD-7 Scale
#'
#' Generates the GAD-7 for data entry
#'
#' @param holding_data A dataframe containing statistics pulled from the holding table in DB.
#'
#'@export
#'
simplified_gad7_scale<- function(input, output, session, holding_data) {


  simplified_gad7_output<- renderUI({

    ns <- session$ns


    tagList(

      fluidRow(
        column(width = 12, offset = 5, h3(tags$strong("GAD-7 ")))
      ),

      fluidRow(
        column(width = 6, h4(tags$strong("Over the past 2 weeks, how often have you been bothered by the following problems?"))),
        column(width = 1, offset = 1, h5(tags$strong("Not at all"))),
        column(width = 1, h5(tags$strong("Several days"))),
        column(width = 1, h5(tags$strong("More than half the days"))),
        column(width = 1, h5(tags$strong("Nearly every day")))
      ),

      fluidRow(
        column(width = 7, h4("Feeling nervous, anxious or on edge")),
        column(width = 5, radioButtons(ns("item_1"), label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, , selected = character(0)))
      ),

      fluidRow(
        column(width = 7, h4("Not being able to stop or control worrying")),
        column(width = 5, radioButtons(ns("item_2"), label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
      ),

      fluidRow(
        column(width = 7, h4("Worrying too much about different things")),
        column(width = 5, radioButtons(ns("item_3"), label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
      ),

      fluidRow(
        column(width = 7, h4("Trouble relaxing")),
        column(width = 5, radioButtons(ns("item_4"), label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
      ),

      fluidRow(
        column(width = 7, h4("Being so restless it is hard to sit still")),
        column(width = 5, radioButtons(ns("item_5"), label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
      ),

      fluidRow(
        column(width = 7, h4("Becoming easily annoyed or irritable")),
        column(width = 5, radioButtons(ns("item_6"), label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
      ),

      fluidRow(
        column(width = 7, h4("Feeling afraid as if something awful might happen")),
        column(width = 5, radioButtons(ns("item_7"), label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
      ),

      fluidRow(
        column(width = 12, h5("Scale Source: Spitzer, R. L., Kroenke, K., Williams, J. B., & Lowe, B. (2006). A brief measure for assessing Generalized Anxiety Disorder: the GAD-7. Archives of Internal Medicine, 166(10), 1092–1097."))
      ))


  })


  scale_entry <- reactive({ paste(input$item_1, input$item_2, input$item_3, input$item_4, input$item_5, input$item_6, input$item_7, sep = ",") })

  return(scale_entry)

}
