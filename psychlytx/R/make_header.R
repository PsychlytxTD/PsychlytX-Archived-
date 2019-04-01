#' Make Header
#'
#' Module to make header
#'
#' @param id A string to create the namespace
#'
#' @export


make_header_UI<- function(id) {


 dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing:
                                             7.8px;font-weight: bolder;"), tags$sup("®"), "| Generalized Anxiety Disorder - 7-Item Scale (GAD-7)"),
                                            style = "color: white; letter-spacing: 1.8px;"), titleWidth = 820)

}



#' Make Header
#'
#' Module to make header
#'
#' @export


make_header<- function(input, output, session) {

  return(NULL)

}
