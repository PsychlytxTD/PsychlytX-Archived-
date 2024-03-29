#' Make Sidebar
#'
#' Module to make sidebar
#'
#' @param id A string to create the namespace
#'
#' @export


make_sidebar_UI<- function(id) {


  dashboardSidebar(  #Menu items are hyperlinks to website pages
    sidebarMenu(
      br(),
      menuItem("Home", icon = icon("line-chart"), tabName = "Home", selected = TRUE),
      br(),
      div( HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), icon("info"), HTML('&nbsp;'), HTML('&nbsp;'),
            HTML('&nbsp;'), HTML('&nbsp;'), tags$a("About Psychlytx", href = "https:://psychlytx.com.au", style = "color:white")),
      br(),
      div( HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), icon("user-cog"), HTML('&nbsp;'),
            HTML('&nbsp;'), tags$a("Update Account Details", href = "https:://psychlytx.com.au", style = "color:white")),
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
      br(),
      tags$footer(tags$a(href = "http://psychlytx.com.au", target = "_blank", HTML("<br><center>"), "PsychlytX", tags$sup(icon("registered"))
                         ))
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
