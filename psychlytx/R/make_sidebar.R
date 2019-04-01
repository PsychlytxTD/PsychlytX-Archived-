#' Make Sidebar
#'
#' Module to make sidebar
#'
#' @param id A string to create the namespace
#'
#' @export


make_sidebar_UI<- function(id) {


  dashboardSidebar(
    sidebarMenu(
      br(),
      menuItem("Home", icon = icon("line-chart"), tabName = "Home"),
      menuItem("About PsychlytX", tabName = "About", icon = icon("info"), selected = TRUE),
      menuItem("References", tabName = "References", icon = icon("book")),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      tags$footer(tags$a(href = "http://psychlytx.com.au", target = "_blank", HTML("<br><center>"), "PsychlytX", tags$sup(icon("registered")), br(),
                         "© PsychlytX 2019"))
    )
  )

}



#' Make Sidebar
#'
#' Module to make sidebar
#'
#' @export


make_sidebar<- function(input, output, session) {

  return(NULL)

}
