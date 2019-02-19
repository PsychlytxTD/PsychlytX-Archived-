#' Data Submission & Report Generation Widgets
#'
#' Creates a submit action button and a report download button to submit data to a database and download a pdf report.
#'
#' @param id String to create a unique namespace.
#'
#' @export

download_buttons_UI<- function(id) {

  ns<- NS(id)

  tagList(

    actionButton(ns('submit_button'), 'Submit Results'),
    br(),
    br(),
    titlePanel(span(tagList(icon("file-pdf-o", lib = "font-awesome")), h4(tags$b("Download a Pdf Report of Results for this Patient")))),
    downloadButton(ns('download_button'), 'Download Report')

  )


}

#' Data Submission & Report Generation Widgets
#'
#' Creates a submit action button and a report download button to submit data to a database and download a pdf report.
#'
#' @param id String to create a unique namespace.
#'
#' @export

download_buttons<- function(input, output, session) {

  return(input)

}

