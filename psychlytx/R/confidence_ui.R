#' Confidence Level
#'
#' Generates a confidence interval widget
#'
#' @param id String to create a unique namespace.
#'
#' @export

confidence_level_UI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(column(
    width = 12,
    offset = 4,
    radioButtons(ns(
      "Confidence"),
      label = h4(
        tags$strong(
          "Select the level of confidence for intervals"
        )
      ),
      choices = list(
        "99%" = 1,
        "95%" = 2,
        "90%" = 3
      ),
      selected = 2,
      inline = T
    )

  )))

}



#' Confidence Level
#'
#' Generates a confidence interval widget
#'
#' @param id String to create a unique namespace.
#'
#' @export

confidence_level<- function(input, output, session) {

  return(input)

}

