
library(readxl)
library(DT)
library(shinydashboard)
library(markdown)
library(knitr)
library(magrittr)
library(ggrepel)
library(dplyr)
library(Rcpp)
library(kableExtra)
library(rhandsontable)
library(shinyjs)
library(V8)
library(data.table)
library(packrat)
library(car)
library(zoo)

source("Diagnoses.R")

Research_Table<- read_excel("ResearchTable.xlsx")

ui<- function(request) {
  sidebar <- dashboardSidebar(
    sidebarMenu(
      br(),
      menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "ADNM"),
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
                         "© Timothy Deitz 2018"))
    )
  )
  
  dashboardPage(
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| Adjustment Disorder New Module - 20 (ADNM-20)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 830),
    sidebar,
    dashboardBody(
      
      tags$head(            #Link to the css style sheet
        tags$link(rel = "stylesheet", type = "text/css", href = "Styling.css")
      ),
      tabItems(
        tabItem(tabName = "About", br(), br(), br(), br(),br(), br(),br(),br(), br(),
                column(12, offset = 4, h1(tags$a(href = "http://psychlytx.com.au", "Visit PsychlytX here.",  style = "color: #827717;")))
        ),
        
        tabItem(tabName = "References", br(), 
                
                h3(tags$strong("References")), br(),
                "Foa, E. B., Huppert, J. D., Leiberg, S., Langner, R., Kichic, R., Hajcak, G., & Salkovskis, P. M. (2002). The obsessive-compulsive inventory: Development and validation of a short version. Psychological Assessment, 14(4), 485.", br(), br(), 
                "Hajcak, G., Huppert, J. D., Simons, R. F., & Foa, E. B. (2004). Psychometric properties of the ADNM-20 in a college sample. Behaviour Research and Therapy, 42(1), 115-123.", br(), br(), 
                "Huppert, J. D., Walther, M. R., Hajcak, G., Yadin, E., Foa, E. B., Simpson, H. B., & Liebowitz, M. R. (2007). The ADNM-20: Validation of the subscales in a clinical sample. Journal of Anxiety Disorders, 21(3), 394-406.", br(), br(), 
                "Rapp, A. M., Bergman, R. L., Piacentini, J., & McGuire, J. F. (2016). Evidence-based assessment of obsessive–compulsive disorder. Journal of Central Nervous System Disease, 8, JCNSD. S38359.", br(), br(), 
                "SmARi, J., Olason, D. T., EYÞÓRSDÓTTIR, Á, & FRÖLUNDE, M. (2007). Psychometric properties of the obsessive compulsive Inventory‐Revised among icelandic college students. Scandinavian Journal of Psychology, 48(2), 127-133.", br(), br(), 
                "Storch, E. A., Benito, K., & Goodman, W. (2011). Assessment scales for obsessive–compulsive disorder. Neuropsychiatry, 1(3), 243-250." 
                
                
        ),
        
        
        
        tabItem(tabName = "ADNM",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel("Scale",
                             wellPanel(style = "background-color: #ededed; color: black",
                                       fluidRow(
                                         column(width = 12, 
                                                h2(tags$strong("ADNM - 20 Questionnaire")),
                                                h3(tags$strong("Adjustment Disorder - New Module 20")),
                                                h4("Below is a list of stressful life events. Please indicate those events that happened during the past (1 / 2) years 
                                                   and are currently a very strong burden to you, or have burdened you in the last six months. You can indicate as many 
                                                   events as applicable. ")
                                                )
                                                ),
                                       fluidRow(
                                         column(width = 12, 
                                                div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", h4(tags$strong("Yes"))),
                                                div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("")),
                                                div(style = "display: inline-block;vertical-align:middle;width:200px;", h4(tags$strong("Date of event (month/year)")))
                                         )
                                       ),
                                       hr(style = "border-width:0.5px;"),
                                       fluidRow(
                                        column(width = 12, 
                                         div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_1", "")),
                                         div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("01. Divorce/separation")),
                                         div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_1", "", width = "50%"))
                                               )
                                                ),
                                       hr(style = "border-width:0.5px;"),
                                       fluidRow(
                                         column(width = 12, 
                                                div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_2", "")),
                                                div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("02. Family conflicts")),
                                                div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_2", "from", width = "50%")),
                                                div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_2a", "to", width = "50%"))
                                         )
                                       ),
                                       hr(style = "border-width:0.5px;"),
                                       fluidRow(
                                         column(width = 12, 
                                                div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_3", "")),
                                                div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("03. Conflicts in worklife")),
                                                div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_3", "from", width = "50%")),
                                                div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_3a", "to", width = "50%"))
                                         )
                                       ),
                                       hr(style = "border-width:0.5px;"),
                                       fluidRow(
                                         column(width = 12, 
                                                div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_4", "")),
                                                div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("04. Conflicts with neighbours")),
                                                div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_4", "from", width = "50%")),
                                                div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_4a", "to", width = "50%"))
                                         )
                                       ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_5", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("05. Illness of a loved one")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_5", "from", width = "50%")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_5a", "to", width = "50%"))
                                        )
                                      ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_6", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("06. Death of a loved one")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_6", "", width = "50%"))
                                        )
                                      ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_7", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("07. Adjustment due to retirement")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_7", "", width = "50%"))
                                        )
                                      ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_8", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("08. Unemployment")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_8", "from", width = "50%")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_8a", "to", width = "50%"))
                                        )
                                      ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_9", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("09. Too much/too little work")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_9", "from", width = "50%")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_9a", "to", width = "50%"))
                                        )
                                       ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_10", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("10. Pressure to meet deadlines/time pressure")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_10", "from", width = "50%")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_10a", "to", width = "50%"))
                                        )
                                      ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_11", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("11. Moving to a new home")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_11", "", width = "50%"))
                                        )
                                      ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_12", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("12. Financial problems")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_12", "from", width = "50%")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_12a", "to", width = "50%"))
                                        )
                                      ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_13", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("13. Own serious illness")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_13", "from", width = "50%")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_13a", "to", width = "50%"))
                                        )
                                      ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_14", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("14. Serious accident")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_14", "", width = "50%"))
                                        )
                                      ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_15", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("15. Assault")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_15", "", width = "50%"))
                                        )
                                      ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_16", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("16. Termination of an important leisure activity")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_16", "from", width = "50%")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_16a", "to", width = "50%"))
                                        )
                                      ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_17", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("17. Any other stressful event (please indicate)")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_17", "from", width = "50%")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_17a", "to", width = "50%")),
                                               div(textInput("Other_Event_1", ""))
                                        )
                                      ),
                                      hr(style = "border-width:0.5px;"),
                                      fluidRow(
                                        column(width = 12, 
                                               div(style = "display: inline-block;vertical-align:middle;width:50px; margin: 9px 0 0;", checkboxInput("Yes_18", "")),
                                               div(style = "display: inline-block;vertical-align:middle;width:400px;", h4("18. Any other stressful event (please indicate)")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_18", "from", width = "50%")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;", textInput("Event_Date_18a", "to", width = "50%")),
                                               div(textInput("Other_Event_2", ""))
                                        )
                                      ),
                                      hr(),
                                      fluidRow(
                                        column(width = 12, 
                                               h4("The events you have just indicated can have numerous consequences for our well-being and behavior. 
                                                  Please indicate the most straining event(s) below:"),
                                               textInput("Straining_Event", "")
                                               )
                                        
                                      ),
                                      fluidRow(
                                        column(width = 12, 
                                               h4("The events you have just indicated can have numerous consequences for our well-being and behavior. Below you will find various statements 
                                                   about which reactions these types of event can trigger. First of all please indicate how often the respective 
                                                  statement applies to you (“never” to “often”). Then secondly, please indicate for how long you have had this reaction.
                                                  It can be for less than one month (< 1 month), between one month and half a year (1-6 months) or longer than 6 months 
                                                  (6 months – 2 years). This may not be very easy to indicate, but please try to give a rough estimation of the duration of the
                                                  reaction. If you did not indicate any stressful life event in the list above, then you can skip the following questions. ")
                                               )
                                      ),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:middle;width:600px;", h4("")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;transform: rotate(270deg);", h4(tags$strong("Never (1)")), HTML('&emsp;'), h4(tags$strong("Rarely (2)")), HTML('&emsp;'), h4(tags$strong("Sometimes (3)")), HTML('&emsp;'), h4(tags$strong("Often (4)")))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                        div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("1. Since the stressful situation, I feel low and sad.")),
                                        div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px; margin-left:5.5px;", checkboxGroupInput("Item_1", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                        column(12, offset = 7, radioButtons("Item_1a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("2. I have to think about the stressful situation repeatedly.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_2", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_2a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("3. I try to avoid talking about the stressful situation whenever possible.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_3", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_3a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("4. I have to think about the stressful situation a lot and thisis a great burden to me.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_4", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_4a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("5. I rarely do those activities which I used to enjoy anymore.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_5", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_5a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("6. If I think about the stressful situation, I find myself in a real state of anxiety.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_6", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_6a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("7. I avoid certain things that might remind me of the stressful situation.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_7", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_7a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("8. I am nervous and restless since the stressful situation")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_8", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_8a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("9. Since the stressful situation, I lose my temper much quicker than I used to, even over small things.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_9", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_9a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("10. Since the stressful situation, I find it difficult to concentrate on certain things.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_10", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_10a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("11. I try to dismiss the stressful situation from my memory.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_11", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_11a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("12. I have noticed that I am becoming more irritable due to the stressful situation.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_12", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_12a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("13. I constantly get memories of the stressful situation and can’t do anything to stop them.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_13", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_13a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("14. I try to suppress my feelings because they are a burden to me.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_14", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_14a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("15. My thoughts often revolve around anything related to the stressful situation.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_15", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_15a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("16. Since the stressful situation, I am scared of doing certain things or of getting into certain situations.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_16", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_16a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("17. Since the stressful situation, I do not like going to work or carrying out the necessary tasks in everyday life.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_17", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_17a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("18. I have been feeling dispirited since the stressful situation and have little hope for the future.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_18", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_18a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("19. Since the stressful situation, I can no longer sleep properly.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_19", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_19a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                      
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               div(style = "display: inline-block;vertical-align:bottom;width:600px;", h4("20. All in all, the situation causes serious impairment in my social or occupational life, my leisure time, and other
                                                                                                                          important areas of functioning.")),
                                               div(style = "display: inline-block;vertical-align:middle;width:200px;letter-spacing: 26px;font-size: 19px;margin-left:5.5px;", checkboxGroupInput("Item_20", "", choices = c(" " = "1", " " = "2", " " = "3", " " = "4"), inline = TRUE)),
                                               column(12, offset = 7, radioButtons("Item_20a", "", choices = c("1 month", "1-6 months", "6m-2years"), inline = TRUE, selected = character(0)))
                                        )
                                      ),
                                           
                                       fluidRow(
                                         hr(),
                                         fluidRow(
                                           column(width = 4, dateInput("Q_Date", "Date",  format = "dd/mm/yyyy")),
                                           column(width = 4, textInput("Q_Name", "Name")),
                                           column(width = 4, textInput("Q_Clin_Name", "Clinician's Name"))
                                         ),
                                         fluidRow(
                                           column(width = 12, h5("Scale Source: Einsle, F., Köllner, V., Dannemann, S & Maercker, A. (2010). Development and validation of a self-report for the assessment of adjustment disorders. Psychology, Health & Medecine, 15(5), 585-595. "))
                                         )
                                       )
                                       
                             )),
                    tabPanel("Enter Data",
                             fluidRow(
                               column(width = 12,
                                      titlePanel(span(tagList(icon("edit", lib = "font-awesome")), h4(tags$b("Enter Scores for Each Timepoint")),
                                                      tags$ul(
                                                        tags$li(helpText(h5(tags$em(tags$b("Use commas to separate frequency scores and duration scores. Enter scores in order, from the first to the last item of the total scale.", style = "color:black")))))
                                                      )
                                      ))
                               )
                             ),
                             br(),
                             br(),
                             br(),
                             fluidRow(
                               column(4,
                                      
                                      selectInput("Timepoint", "Select Timepoints for New Data Entry", choices = c("1", "2", "3")),
                                      tags$hr(),
                                      textInput("PatientName", "Enter your Patient's Name or Initials"),
                                      textInput("ClinicianName", "Enter the Name of the Clinician"),
                                      helpText(tags$em("*Your patient's name cannot be seen or used by PsychlytX. If provided, it will appear in the patient report and analytics spreadsheet (if created), accessible only to you.
                                                       This field is optional when generating the patient report, but mandatory if you wish to store and analyse an analytics spreadsheet."))
                                      
                                      ),
                               
                               column(4,
                                      textInput('Text_1', 'Time 1 Scores', "0,1,2,etc"),
                                      textInput('Duration_1', 'Time 1 Duration Scores', "1 month, 1-6 months, 6m-2years etc."),
                                      hr(),
                                      textInput('Text_2', 'Time 2 Scores', "0,1,2,etc"),
                                      textInput('Duration_2', 'Time 2 Duration Scores', "1 month, 1-6 months, 6m-2years etc."),
                                      hr(),
                                      textInput('Text_3', 'Time 3 Scores', "0,1,2,etc"),
                                      textInput('Duration_3', 'Time 3 Duration Scores', "1 month, 1-6 months, 6m-2years etc.")
                                      
                               ),
                               
                               
                               
                               column(4,
                                      dateInput("Date_1", "Date of 1st Timepoint", format = "dd/mm/yyyy"),
                                      br(), br(), br(), br(), br(),
                                      dateInput("Date_2", "Date of 2nd Timepoint", format = "dd/mm/yyyy"),
                                      br(), br(), br(), br(), br(),
                                      dateInput("Date_3", "Date of 3rd Timepoint", format = "dd/mm/yyyy")
                               )
                               
                             )),
                    
                    tabPanel("Evaluate Reliable & Clinically Significant Change",
                             fluidPage(
                               fluidRow(
                                 column(width = 12,
                                        titlePanel(span(tagList(icon("calculator", lib = "font-awesome")), h4(tags$b("Step 1. Use Default Values to Calculate Reliable & Clinically Significant Change")))), br(),
                                        tags$ul(
                                          tags$li(helpText(h4(tags$em("Select a population that matches the characteristics of your client.", style = "color:black")))),
                                          tags$li(helpText(h4(tags$em("Based on your selection, the program will automatically define values required to calculate confidence intervals & cut-off scores.", style = "color:black")))),
                                          tags$li(helpText(h4(tags$em("Confidence intervals and cut-off scores will then be visible in the summary report, allowing you to view reliable & clinically significant change.", style = "color:black"))))
                                        ), hr(),
                                        h3(tags$b("OR...")),
                                        br(),
                                        span(tagList(icon("calculator", "fa-2x", lib = "font-awesome")), h4(tags$b("Step 2 (Optional). Enter Your Own Values to Calculate Reliable & Clinically Significant Change"))),
                                        br(),
                                        tags$ul(
                                          tags$li(helpText(h4(tags$em("After selecting a population, click across the tabs below, moving from left to right. Additional fields may appear as you go.", style = "color:black")))),
                                          tags$li(helpText(h4(tags$em("Under some or all of the tabs, define your own values by referring to the main table below or by locating research elsewhere.", style = "color:black"))))
                                        ),
                                        br()
                                        
                                 )
                               ),
                               fluidRow(
                                 tabsetPanel(type = "pills",
                                             
                                             
                                             tabPanel("Population", width = 12,
                                                      h4(tags$strong("Select the population")),
                                                      selectInput("Pop", "", choices = c("Stressor Exposure/Adjustment Disorder"))
                                             ),
                                             
                                             tabPanel("Reliable Change Method", width = 12,
                                                      h4(tags$strong("Select a method for calculating confidence intervals to determine reliable change")),
                                                      h5("*Your selection will determine how the program calculates a predicted score and standard error term (required to generate confidence intervals)."),
                                                      h5("*When selecting a method other than Nunnally & Bernstein (1994) or Jacobson & Truax (1991), you must enter a value for the retest mean.
                                                         Most other methods also require a value for the retest standard deviation. If using the method of Crawford & Howell (1998), a value 
                                                         for sample size is necessary. For information and advice on the selection of reliable change methods, 
                                                         visit the", tags$a(href = "www.sentientpsychometrics.com.au", "PsychlytX website.")),
                                                      selectInput("RelChangeMethod", "", choices = c("Nunnally & Bernstein (1994)", "Chelune et al. (1993)", "Crawford & Howell (1998)", "Jacobson & Truax (1991)", "McSweeny et al. (1993)", "Maassen et al. (2006)", "Speer (1992)")),
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'",
                                                                       numericInput("SampleN", "Enter the sample size of the control group", value = 0)),
                                                      
                                                      h4(tags$strong("Would you like to manually define the width of confidence intervals instead?")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               selectInput("Select_CI", label = "ADNM-20 total scale",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI == '2'",
                                                                                numericInput("Man_CI", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Preoccupations", label = "Preoccupations",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Preoccupations == '2'",
                                                                                numericInput("Man_CI_Preoccupations", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Failure_To_Adapt", label = "Failure To Adapt",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Failure_To_Adapt == '2'",
                                                                                numericInput("Man_CI_Failure_To_Adapt", "Specify the width of the confidence interval", value = 0))
                                                        )
                                                        
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               selectInput("Select_CI_Depressive_Mood", label = "Depressive Mood",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Depressive_Mood == '2'",
                                                                                numericInput("Man_CI_Depressive_Mood", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Anxiety", label = "Anxiety",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Anxiety == '2'",
                                                                                numericInput("Man_CI_Anxiety", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Impulse_Control", label = "Impulse Control",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Impulse_Control == '2'",
                                                                                numericInput("Man_CI_Impulse_Control", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Avoidance", label = "Avoidance",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Avoidance == '2'",
                                                                                numericInput("Man_CI_Avoidance", "Specify the width of the confidence interval", value = 0))
                                                        )
                                                      )
                                                      ),
                                             
                                             tabPanel("Mean", width = 12,
                                                      h4(tags$strong("Enter a mean value")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Preoccupations")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Failure_To_Adapt")
                                                        )
                                                        
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Depressive_Mood")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Anxiety")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Impulse_Control")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Avoidance")
                                                        )
                                                      ),
                                                      
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Chelune et al. (1993)' || input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)' || input.RelChangeMethod == 'Speer (1992)'",
                                                                       
                                                                       h4(tags$strong("Enter a mean value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean", "ADNM-20 total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Preoccupations", "Preoccupations", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Failure_To_Adapt", "Failure To Adapt", value = 0)
                                                                         )
                                                                         
                                                                       ),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Depressive_Mood", "Depressive Mood", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Anxiety", "Anxiety", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Impulse_Control", "Impulse Control", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Avoidance", "Avoidance", value = 0)
                                                                         )
                                                                       )
                                                                       
                                                      )
                                                      
                                                      
                                             ),
                                             
                                             tabPanel("Sd", width = 12,
                                                      h4(tags$strong("Enter a standard deviation value")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Preoccupations")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Failure_To_Adapt")
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Depressive_Mood")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Anxiety")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Impulse_Control")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Avoidance")
                                                        )
                                                      ),
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)'",
                                                                       
                                                                       h4(tags$strong("Enter a standard deviation value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd", "ADNM-20 total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Preoccupations", "Preoccupations", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Failure_To_Adapt", "Failure To Adapt", value = 0)
                                                                         )
                                                                         
                                                                       ),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Depressive_Mood", "Depressive Mood", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Anxiety", "Anxiety", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Impulse_Control", "Impulse Control", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Avoidance", "Avoidance", value = 0)
                                                                         )
                                                                       )
                                                                       
                                                      )
                                             ),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      h4(tags$strong("Enter test-retest reliability values")), 
                                                      h6("* No test-retest reliability data was available for subscales. Reported subscale reliability values represent internal consistency (Kuder Richardson Formula 20; KR-20)."),
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability", "ADNM-20 total scale", value = .8),
                                                               h6("Bachem & Maercker (2016)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Preoccupations", "Preoccupations", value = .83),
                                                               h6("Bachem & Maercker (2016)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Failure_To_Adapt", "Failure To Adapt", value = .69),
                                                               h6("Bachem & Maercker (2016)")
                                                        )
                                                        
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability_Depressive_Mood", "Depressive Mood", value = .79),
                                                               h6("Einsle, Köllner, Dannermann & Maercker (2010)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Anxiety", "Anxiety", value = .76),
                                                               h6("Einsle, Köllner, Dannermann & Maercker (2010)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Impulse_Control", "Impulse Control", value = .74),
                                                               h6("Einsle, Köllner, Dannermann & Maercker (2010)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Avoidance", "Avoidance", value = .71),
                                                               h6("Einsle, Köllner, Dannermann & Maercker (2010)")
                                                        )
                                                        
                                                      ),
                                                      checkboxInput("Rel_Calc_Checkbox", tags$strong("Calculate test-retest reliability values from research statistics."), width = '100%'),
                                                      uiOutput("Rel_Calc_Panel")),
                                             
                                             tabPanel("Confidence Level", width = 12,
                                                      
                                                      fluidRow(align = "center",
                                                               radioButtons("Confidence", label = h4(tags$strong("Select the level of confidence for intervals")),
                                                                            choices = list("99%" = 1, "95%" = 2, "90%" = 3),
                                                                            selected = 2, inline = T)
                                                      )
                                                      
                                                      
                                                      
                                             ),
                                             tabPanel("User-Defined Cut-Off Scores", width = 12,
                                                      h4(tags$strong("Define cut-off scores")),
                                                      hr(),
                                                      h4(tags$strong("First cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Preoccupations_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Failure_To_Adapt_1") 
                                                        )
                                                        
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Depressive_Mood_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Anxiety_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Impulse_Control_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Avoidance_1") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Second cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Preoccupations_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Failure_To_Adapt_2") 
                                                        )
                                                        
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Depressive_Mood_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Anxiety_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Impulse_Control_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Avoidance_2") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Third cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Preoccupations_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Failure_To_Adapt_3") 
                                                        )
                                                        
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Depressive_Mood_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Anxiety_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Impulse_Control_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Avoidance_3") 
                                                        )
                                                        
                                                      )
                                                      , hr(),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_4"))
                                                      )
                                                      
                                             ),
                                             
                                             
                                             h4(tags$strong("Psychometric Properties of the ADNM-20 Relevant to Assessing Reliable & Clinically Significant Change")),
                                             h6(em("*Values marked with an asterix represent internal consistency, not test-retest reliability.")),
                                             DT::dataTableOutput("Research_Table")
                                             
                                             )))),
                    
                    
                    tabPanel("Import Data",
                             fluidPage(
                               titlePanel(span(tagList(icon("folder-open", lib = "font-awesome")), h4(tags$b("Import this Patient's Earlier Data")))),
                               sidebarLayout(
                                 sidebarPanel(
                                   fileInput('Input_File1', "Select File",
                                             accept=c('text/csv',
                                                      'text/comma-separated-values,text/plain',
                                                      '.csv'))
                                 ),
                                 mainPanel(
                                   DT::dataTableOutput('Imported_Scale_Scores'),
                                   br(),
                                   br(),
                                   helpText("Swipe left over the table to scroll and view all data.")
                                 )))),
                    
                    
                    tabPanel("Download Report",
                             fluidPage(
                               titlePanel(span(tagList(icon("file-pdf-o", lib = "font-awesome")), h4(tags$b("Download a Pdf Report of Results for this Patient")))),
                               h5(tags$em("*Click the buttons below in order:")),
                               sidebarLayout(
                                 sidebarPanel(width = 5,
                                              actionButton("Action_Submit_Data", div(tags$strong("Step 1."), "Submit Newly-Entered Data"), width = '270px'),
                                              hr(),
                                              br(),
                                              actionButton("Action_Combine", div(tags$strong("Step 2."), "Combine with Imported Data"), width = '270px'),
                                              helpText(h5(tags$em("Do not select this option unless combining imported & newly-entered data.", style = "color:white"))),
                                              hr(),
                                              br(),
                                              downloadButton("report", div(tags$strong("Step 3."), "Generate Patient Report"), class = "reportbutton"),
                                              tags$head(tags$style(".reportbutton{background-color:#283747;} .reportbutton{color:#d35400;}")),
                                              br(),
                                              helpText(h5(tags$em("Allow a few moments for the report to load.", style = "color:white")))
                                 ),
                                 mainPanel()
                               ))),
                    
                    
                    tabPanel("Export Data",
                             fluidPage(
                               titlePanel(span(tagList(icon("file-excel-o", lib = "font-awesome")), h4(tags$b("Download this Patient's Data for Future Import")))),
                               sidebarLayout(
                                 sidebarPanel(
                                   radioButtons("Output_Filetype1", "File Type:",
                                                choices = c("csv")),
                                   actionButton('Action_Export', 'Display Data'),
                                   tags$p(""),
                                   downloadButton('Download_Export_Data', 'Download Data')),
                                 
                                 mainPanel(
                                   fluidRow(
                                     column(width = 12,
                                            DT::dataTableOutput('Display_Export_Data'),
                                            helpText("Swipe left over the table to scroll and view all data.")
                                     ))))
                               
                               
                             )),
                    
                    tabPanel("Service Analytics",
                             fluidPage(
                               br(),
                               titlePanel(span(tagList(icon("bar-chart-o", lib = "font-awesome")), h4(tags$b("Add this Patient's Data to Your Analytics Dataset after Service Completion")))),
                               br(),
                               sidebarLayout(position = "right",
                                             sidebarPanel(width = 5,
                                                          h5(tags$em("*Only complete this section once for each patient.")),
                                                          br(),
                                                          h4(tags$strong("Step 3.")), h4("Provide Information about this Patient"),
                                                          br(),
                                                          h4(tags$strong("Basic Demographics")),
                                                          br(),
                                                          textInput("ControlName", "Patient's Name"),
                                                          selectInput("Sex", "Sex", c("", "Male", "Female"), width = '200px'),
                                                          numericInput("Age", "Age", value = "", width = '100px'),
                                                          selectInput("Sexuality", "Sexual Orientation", c("", "Heterosexual", "Lesbian", "Gay", "Bisexual", "Transgender", "Queer"), width = '200px'),
                                                          selectInput("Relationship", "Relationship Status", c("", "Married/In Relationship", "Single", "Widowed"), width = '250px'),
                                                          numericInput("Children", "Number of Dependent Children", value = "", width = '100px'),
                                                          selectInput("Workforce", "Primary Workforce Status", c("", "Working", "Studying", "Unemployed", "Retired"), width = '200px'),
                                                          selectInput("Education", "Education", c("", "Did Not Complete High School", "Completed High School", "Completed Tertiary Education"), width = '250px'),
                                                          textInput("Suburb", "Suburb", value = "", width = '300px'),
                                                          hr(),
                                                          h4(tags$strong("Clinical Information")),
                                                          br(),
                                                          selectInput("Principal_Diagnosis", "Principal Diagnosis", Diagnosis_List),
                                                          selectizeInput("Secondary_Diagnosis", "Additional Diagnosis/Diagnoses", Diagnosis_List, multiple = TRUE),
                                                          textInput("Referrer", "Referrer", value = "", width = '200px'),
                                                          selectInput("Attendance_Arrangement", "Attendance Arrangement", c("", "It Varies", "Twice A Week", "Once A Week", "Once a Fortnight", "Once Every 3 Weeks", "Once A Month", "More Than 1 Month Apart"), width = '250px'),
                                                          selectInput("Attendance_Quality", "Quality of Attendance", c("", "Good", "Moderate", "Poor"), width = '200px'),
                                                          textInput("Therapy", "Therapeutic Approach", value = "", width = '200px'),
                                                          selectInput("Fee", "Fee Arrangement", c("", "No Out-Of-Pocket Expense", "Discount", "Full-Fee"), width = '250px'),
                                                          numericInput("Duration", "Number of Sessions Attended", value = "", width = '100px'),
                                                          selectInput("Dropout", "Early Dropout", c("", "Yes", "No"), width = '200px'),
                                                          hr(),
                                                          h4(tags$strong("Custom Variables Created Previously")),
                                                          br(),
                                                          uiOutput("Old_Custom_Widgets"),
                                                          hr(),
                                                          h4(tags$strong("Create New Custom Variables")),
                                                          br(),
                                                          textInput("Custom_Names", "Name Your Variables"),
                                                          h5(tags$em("*Separate the names with commas & leave no blank space.")),
                                                          h5(tags$em("*E.g. Height,Weight")),
                                                          actionButton("Create_New_Widg","Create"),
                                                          br(),
                                                          br(),
                                                          uiOutput("New_Custom_Widgets")
                                             ),
                                             mainPanel(width = 7,
                                                       actionButton("Analytics_Info", "About Service Analytics", icon = icon("info-circle"), lib = "font-awesome"),
                                                       br(),
                                                       h4(tags$strong("Step 1.")), h4("Create a New Analytics Dataset or Add to an Existing One"),
                                                       radioButtons("SpreadType", "", c("New Analytics Dataset", "Add To My Analytics Dataset")),
                                                       hr(),
                                                       h4(tags$strong("Step 2 (As Required).")), h4("Import Your Original Analytics Dataset"),        
                                                       fileInput('Input_File2', "Select File",
                                                                 accept=c('text/csv',
                                                                          'text/comma-separated-values,text/plain',
                                                                          '.csv')),

                                                       tags$hr(),
                                                       
                                                       h4(tags$strong("Step 4.")), h4("Download The New/Updated Analytics Dataset"),
                                                       radioButtons("Output_Filetype2", "File Type",
                                                                    choices = c("csv")),
                                                       actionButton('Action_Show_Analytics', 'Show My Analytics Dataset'),
                                                       downloadButton("Download_New_Analytics", "Download The Dataset"),
                                                       DT::dataTableOutput('Display_New_Analytics'),
                                                       helpText("Only the most recent 10 entries are shown.")
                                                       
                                                       
                                                       
                                                       
                                                       
                                             )
                               )))
                    
                    
                    ),
                  
                  column(span(tagList(icon("copyright", lib = "font-awesome")), "Timothy Deitz 2018 | PsychlytX") , offset = 4, width = 12)
                             )))))
  
}

server <- function(input, output, session) {
  
  
  observeEvent(input$Disclaimer, {
    showModal(modalDialog(
      title = "Terms & Conditions of Use", footer = modalButton("Accept"),
      "By clicking", tags$em("Accept"), "I agree to the following terms and conditions:", br(),
      "1. PsychlytX will not view, use or retain any patient data entered into this program.", br(),
      "2. This program is subject to copyright and will not be used without the permission of PsychlytX.", br(),
      "3. PsychlytX assumes no responsibility for the inappropriate use or disclosure of patient information.", br(),
      "4. All psychological scales employed to collect data analysed by this program will be used in accordance with the correct conditions of consent, licensing and payment, as outlined by the scale developer(s).", br(),
      "5. PsychlytX does not encourage or condone the use of psychological scales where such use does not meet the correct conditions of consent, licensing and payment, as outlined by the scale developer(s)."
    ))
  })
  
  
  #Questionnaire responses
  
  observe({
    
    Q_Scores <- paste(input$Item_1, input$Item_2, input$Item_3, input$Item_4, input$Item_5, input$Item_6, input$Item_7, 
                      input$Item_8, input$Item_9, input$Item_10, input$Item_11, input$Item_12, input$Item_13, input$Item_14, input$Item_15, input$Item_16, 
                      input$Item_17, input$Item_18, input$Item_19, input$Item_20, sep = ",")
    
    D_Scores<- paste(input$Item_1a, input$Item_2a, input$Item_3a, input$Item_4a, input$Item_5a, input$Item_6a, input$Item_7a, 
                     input$Item_8a, input$Item_9a, input$Item_10a, input$Item_11a, input$Item_12a, input$Item_13a, input$Item_14a, input$Item_15a, input$Item_16a, 
                     input$Item_17a, input$Item_18a, input$Item_19a, input$Item_20a, sep = ",")
    
    Q_Date<- input$Q_Date
    Q_Name<- input$Q_Name
    Q_Clin_Name<- input$Q_Clin_Name
    
    updateTextInput(session, "Text_1", value = Q_Scores)
    updateDateInput(session, "Date_1", value = Q_Date)
    updateTextInput(session, "Duration_1", value = D_Scores)
    updateTextInput(session, "PatientName", value = Q_Name)
    updateTextInput(session, "ClinicianName", value = Q_Clin_Name)
    
  })
  
  
  #Create the Research Table
  
  output$Research_Table<- renderDataTable({
    
    DT::datatable(Research_Table, extensions = 'FixedColumns', rownames = FALSE,
                  options = list(initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#827717', 'color': '#fff'});",
                    "}")
                    , pageLength = 80, dom = "t", scrollX = TRUE, fixedColumns = list(leftColumns = 3), autoWidth = TRUE, columnDefs = list(list(width = '180px', targets = c(1,9,10)), list(width= '140px', targets = c(0,2))))) %>% formatStyle('Authors', color = '#ffffff', backgroundColor = '#d35400')
    
  })
  
  
  #Render a table showing the imported scale scores
  
  output$Imported_Scale_Scores <- DT::renderDataTable({
    
    
    Infile1<- input$Input_File1
    
    if (is.null(Infile1))
      return(NULL)
    
    Imported_Scores_CSV<- read.csv(Infile1$datapath, header=TRUE, sep=",",
                                   quote="\"" , row.names = NULL)
    
    
    
    DT::datatable(Imported_Scores_CSV, extensions = 'FixedColumns', rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE, fixedColumns = list(leftColumns = 2)))
    
  })
  
  #Use values to calculate test-retest reliability if needed
  
  output$Rel_Calc_Panel<- renderUI({
    if(input$Rel_Calc_Checkbox == "TRUE") {
      tagList(
        h5("Enter values below"),
        fluidRow(
          column(width = 2,
                 numericInput("Calc_Mean", "Initial Mean", value = "")
          ),
          column(width = 2,
                 numericInput("Calc_Sd", "Initial Sd", value = "")
          ), 
          column(width = 2,
                 numericInput("Calc_Retest_Mean", "Retest Mean", value = "")
          ),
          column(width = 2,
                 numericInput("Calc_Retest_Sd", "Retest Sd", value = "")
          ),
          column(width = 2,
                 numericInput("Calc_N", "Sample Size", value = "")
          )),
        radioButtons("Choose_Stat", "Choose a statistic from which to calculate test-retest reliability", choices = c("T-Value", "F-Value"), width = '100%'),
        fluidRow(
          conditionalPanel(condition = "input.Choose_Stat == 'T-Value'",
                           column(width = 2,
                                  numericInput("Calc_T", "T-Value", value = "")
                           )
          ),
          conditionalPanel(condition = "input.Choose_Stat == 'F-Value'",
                           column(width = 2,
                                  numericInput("Calc_F", "F-Value", value = "")
                           )
          )
        ),
        fluidRow(
          column(width = 3,
                 actionButton("Action_Calc_Rel", "Calculate")
          )
        ),
        fluidRow(
          column(width = 8,
                 h4(tags$strong(verbatimTextOutput("Calculated_Reliability"))),
                 h6("*Once calculated, enter this value in the field for test-retest reliability above.")
          )
        )
      )
      
    }
    
    
  })
  
  
  
  Rel_Calc_Reac<-  reactive({
    Calc_M<- input$Calc_Mean 
    Calc_Sd<- input$Calc_Sd
    Calc_Retest_M<- input$Calc_Retest_Mean
    Calc_Retest_Sd<- input$Calc_Retest_Sd
    Calc_N<- input$Calc_N
    
    
    if(input$Choose_Stat == "T-Value") {
      Calc_T<- input$Calc_T
      T_Rel<- (((Calc_Sd*Calc_Sd)+(Calc_Retest_Sd*Calc_Retest_Sd))-(((Calc_Retest_M - Calc_M)/Calc_T)*((Calc_Retest_M - Calc_M)/Calc_T)*Calc_N))/(2*Calc_Sd*Calc_Retest_Sd) 
      T_Rel<- round(T_Rel, digits = 2)
      paste("Test-retest reliability =", T_Rel)
      
    } else if(input$Choose_Stat == "F-Value") {
      Calc_F<- input$Calc_F
      F_Rel<- (((Calc_Sd*Calc_Sd)+(Calc_Retest_Sd*Calc_Retest_Sd))-(((Calc_Retest_M - Calc_M)/(sqrt(Calc_F)))*((Calc_Retest_M - Calc_M)/(sqrt(Calc_F)))*Calc_N))/(2*Calc_Sd*Calc_Retest_Sd) 
      F_Rel<- round(F_Rel, digits = 2)
      paste("Test-retest reliability =" , F_Rel)
      
    }
    
  })
  
  Action_Rel_Calc_Reac<- eventReactive(input$Action_Calc_Rel, {
    
    Rel_Calc_Reac()
    
  })
  
  output$Calculated_Reliability<- renderText({
    
    Action_Rel_Calc_Reac()
    
  })
  
  
  CI_Vals_Reac<- reactive({
    
    if(input$Pop == "Stressor Exposure/Adjustment Disorder") {
      Mean_Val<<-49.53
      Sd_Val<<- 9.83
      Source_Mean<<- "Bachem & Maercker (2016)"
      Source_Sd<<- "Bachem & Maercker (2016)"
      Cut_Val_1<<- 30.1
      Cut_Val_2<<- 45.7
      Cut_Val_3<<- 48
      Cut_Val_4<<- 59.6
      Cut_Lab_1<<- "Low Severity"
      Cut_Lab_2<<- "Moderate Severity"
      Cut_Lab_3<<- "Adjustment Disorder Cut-Off (Lorenz et al., 2016)"
      Cut_Lab_4<<- "High Severity"
      Source_Cutoff_1<<- "Lorenz, Bachem & Maercker (2016)"
      Source_Cutoff_2<<- "Lorenz, Bachem & Maercker (2016)"
      Source_Cutoff_3<<- "Lorenz, Bachem & Maercker (2016)"
      Source_Cutoff_4<<- "Lorenz, Bachem & Maercker (2016)"
      Mean_Val_Preoccupations<<-9.33
      Sd_Val_Preoccupations<<-3.41
      Source_Mean_Preoccupations<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Sd_Preoccupations<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Cut_Val_Preoccupations_1<<- 7.6
      Cut_Val_Preoccupations_2<<- 11.7
      Cut_Val_Preoccupations_3<<- 14.8
      Cut_Lab_Preoccupations_1<<- "Low Severity"
      Cut_Lab_Preoccupations_2<<- "Moderate Severity"
      Cut_Lab_Preoccupations_3<<- "High Severity"
      Source_Cutoff_Preoccupations_1<<- "Lorenz, Bachem & Maercker (2016)"
      Source_Cutoff_Preoccupations_2<<- "Lorenz, Bachem & Maercker (2016)"
      Source_Cutoff_Preoccupations_3<<- "Lorenz, Bachem & Maercker (2016)"
      Mean_Val_Failure_To_Adapt<<- 7.07
      Sd_Val_Failure_To_Adapt<<- 2.93
      Source_Mean_Failure_To_Adapt<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Sd_Failure_To_Adapt<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Cut_Val_Failure_To_Adapt_1<<- 5.1
      Cut_Val_Failure_To_Adapt_2<<- 8
      Cut_Val_Failure_To_Adapt_3<<- 11.6
      Cut_Lab_Failure_To_Adapt_1<<- "Low Severity"
      Cut_Lab_Failure_To_Adapt_2<<- "Moderate Severity"
      Cut_Lab_Failure_To_Adapt_3<<- "High Severity"
      Source_Cutoff_Failure_To_Adapt_1<<- "Lorenz, Bachem & Maercker (2016)"
      Source_Cutoff_Failure_To_Adapt_2<<- "Lorenz, Bachem & Maercker (2016)"
      Source_Cutoff_Failure_To_Adapt_3<<- "Lorenz, Bachem & Maercker (2016)"
      Mean_Val_Depressive_Mood<<- 6.29
      Sd_Val_Depressive_Mood<<- 2.11
      Source_Mean_Depressive_Mood<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Sd_Depressive_Mood<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Cut_Val_Depressive_Mood_1<<- Mean_Val_Depressive_Mood 
      Cut_Val_Depressive_Mood_2<<- Mean_Val_Depressive_Mood + Sd_Val_Depressive_Mood
      Cut_Val_Depressive_Mood_3<<- Mean_Val_Depressive_Mood + (2*Sd_Val_Depressive_Mood)
      Cut_Lab_Depressive_Mood_1<<- "Mean"
      Cut_Lab_Depressive_Mood_2<<- "Mean + 1 Sd"
      Cut_Lab_Depressive_Mood_3<<- "Mean + 2 Sd"
      Source_Cutoff_Depressive_Mood_1<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Cutoff_Depressive_Mood_2<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Cutoff_Depressive_Mood_3<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Mean_Val_Anxiety<<- 4.35
      Sd_Val_Anxiety<<- 1.77
      Source_Mean_Anxiety<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Sd_Anxiety<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Cut_Val_Anxiety_1<<- Mean_Val_Anxiety
      Cut_Val_Anxiety_2<<- Mean_Val_Anxiety + Sd_Val_Anxiety
      Cut_Val_Anxiety_3<<- Mean_Val_Anxiety + (2*Sd_Val_Anxiety)
      Cut_Lab_Anxiety_1<<- "Mean"
      Cut_Lab_Anxiety_2<<- "Mean + 1 Sd"
      Cut_Lab_Anxiety_3<<- "Mean + 2 Sd"
      Source_Cutoff_Anxiety_1<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Cutoff_Anxiety_2<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Cutoff_Anxiety_3<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Mean_Val_Impulse_Control<<- 6.69
      Sd_Val_Impulse_Control<<-  2.76
      Source_Mean_Impulse_Control<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Sd_Impulse_Control<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Cut_Val_Impulse_Control_1<<- Mean_Val_Impulse_Control 
      Cut_Val_Impulse_Control_2<<- Mean_Val_Impulse_Control + Sd_Val_Impulse_Control
      Cut_Val_Impulse_Control_3<<- Mean_Val_Impulse_Control + (2*Sd_Val_Impulse_Control)
      Cut_Lab_Impulse_Control_1<<- "Mean"
      Cut_Lab_Impulse_Control_2<<- "Mean + 1 Sd"
      Cut_Lab_Impulse_Control_3<<- "Mean + 2 Sd"
      Source_Cutoff_Impulse_Control_1<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Cutoff_Impulse_Control_2<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Cutoff_Impulse_Control_3<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Mean_Val_Avoidance<<- 9.38
      Sd_Val_Avoidance<<- 3.42
      Source_Mean_Avoidance<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Sd_Avoidance<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Cut_Val_Avoidance_1<<- Mean_Val_Avoidance 
      Cut_Val_Avoidance_2<<- Mean_Val_Avoidance + Sd_Val_Avoidance
      Cut_Val_Avoidance_3<<- Mean_Val_Avoidance + (2*Sd_Val_Avoidance)
      Cut_Lab_Avoidance_1<<- "Mean"
      Cut_Lab_Avoidance_2<<- "Mean + 1 Sd"
      Cut_Lab_Avoidance_3<<- "Mean + 2 Sd"
      Source_Cutoff_Avoidance_1<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Cutoff_Avoidance_2<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
      Source_Cutoff_Avoidance_3<<- "Zelviene, Kazlauskas, Eimontas & Maercker (2017)"
    }
    
  })
  
  
  output$Mean_Widg<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean", "ADNM-20 total scale", Mean_Val),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Preoccupations<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Preoccupations", "Preoccupations", Mean_Val_Preoccupations),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Preoccupations", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Failure_To_Adapt<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Failure_To_Adapt", "Failure To Adapt", Mean_Val_Failure_To_Adapt),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Failure_To_Adapt", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Depressive_Mood<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Depressive_Mood", "Depressive Mood", Mean_Val_Depressive_Mood),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Depressive_Mood", suspendWhenHidden = FALSE) 
  
  

  output$Mean_Widg_Anxiety<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Anxiety", "Anxiety", Mean_Val_Anxiety),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Anxiety", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Impulse_Control<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Impulse_Control", "Impulse Control", Mean_Val_Impulse_Control),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Impulse_Control", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Avoidance<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Avoidance", "Avoidance", Mean_Val_Avoidance),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Avoidance", suspendWhenHidden = FALSE) 
  
  
  
  output$Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd", "ADNM-20 total scale", Sd_Val),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Preoccupations<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Preoccupations", "Preoccupations", Sd_Val_Preoccupations),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Preoccupations", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Failure_To_Adapt<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Failure_To_Adapt", "Failure To Adapt", Sd_Val_Failure_To_Adapt),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Failure_To_Adapt", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Depressive_Mood<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Depressive_Mood", "Depressive Mood", Sd_Val_Depressive_Mood),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Depressive_Mood", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Anxiety<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Anxiety", "Anxiety", Sd_Val_Anxiety),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Anxiety", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Impulse_Control<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Impulse_Control", "Impulse Control", Sd_Val_Impulse_Control),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Impulse_Control", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Avoidance<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Avoidance", "Avoidance", Sd_Val_Avoidance),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Avoidance", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_1", "ADNM-20 total scale", as.numeric(Cut_Val_1)),
      textInput("Cutoff_Text_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source_Cutoff_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Preoccupations_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Preoccupations_1", "Preoccupations", as.numeric(Cut_Val_Preoccupations_1)),
      textInput("Cutoff_Text_Preoccupations_1", "Cut-Off Score Name", Cut_Lab_Preoccupations_1),
      h6(paste("Reference:", Source_Cutoff_Preoccupations_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Preoccupations_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Failure_To_Adapt_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Failure_To_Adapt_1", "Failure To Adapt", as.numeric(Cut_Val_Failure_To_Adapt_1)),
      textInput("Cutoff_Text_Failure_To_Adapt_1", "Cut-Off Score Name", Cut_Lab_Failure_To_Adapt_1),
      h6(paste("Reference:", Source_Cutoff_Failure_To_Adapt_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Failure_To_Adapt_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Depressive_Mood_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Depressive_Mood_1", "Depressive Mood", as.numeric(Cut_Val_Depressive_Mood_1)),
      textInput("Cutoff_Text_Depressive_Mood_1", "Cut-Off Score Name", Cut_Lab_Depressive_Mood_1),
      h6(paste("Reference:", Source_Cutoff_Depressive_Mood_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Depressive_Mood_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Anxiety_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Anxiety_1", "Anxiety", as.numeric(Cut_Val_Anxiety_1)),
      textInput("Cutoff_Text_Anxiety_1", "Cut-Off Score Name", Cut_Lab_Anxiety_1),
      h6(paste("Reference:", Source_Cutoff_Anxiety_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Anxiety_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Impulse_Control_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Impulse_Control_1", "Impulse Control", as.numeric(Cut_Val_Impulse_Control_1)),
      textInput("Cutoff_Text_Impulse_Control_1", "Cut-Off Score Name", Cut_Lab_Impulse_Control_1),
      h6(paste("Reference:", Source_Cutoff_Impulse_Control_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Impulse_Control_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Avoidance_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Avoidance_1", "Avoidance", as.numeric(Cut_Val_Avoidance_1)),
      textInput("Cutoff_Text_Avoidance_1", "Cut-Off Score Name", Cut_Lab_Avoidance_1),
      h6(paste("Reference:", Source_Cutoff_Avoidance_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Avoidance_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_2", "ADNM-20 total scale", as.numeric(Cut_Val_2)),
      textInput("Cutoff_Text_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source_Cutoff_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Preoccupations_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Preoccupations_2", "Preoccupations", as.numeric(Cut_Val_Preoccupations_2)),
      textInput("Cutoff_Text_Preoccupations_2", "Cut-Off Score Name", Cut_Lab_Preoccupations_2),
      h6(paste("Reference:", Source_Cutoff_Preoccupations_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Preoccupations_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Failure_To_Adapt_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Failure_To_Adapt_2", "Failure To Adapt", as.numeric(Cut_Val_Failure_To_Adapt_2)),
      textInput("Cutoff_Text_Failure_To_Adapt_2", "Cut-Off Score Name", Cut_Lab_Failure_To_Adapt_2),
      h6(paste("Reference:", Source_Cutoff_Failure_To_Adapt_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Failure_To_Adapt_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Depressive_Mood_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Depressive_Mood_2", "Depressive Mood", as.numeric(Cut_Val_Depressive_Mood_2)),
      textInput("Cutoff_Text_Depressive_Mood_2", "Cut-Off Score Name", Cut_Lab_Depressive_Mood_2),
      h6(paste("Reference:", Source_Cutoff_Depressive_Mood_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Depressive_Mood_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Anxiety_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Anxiety_2", "Anxiety", as.numeric(Cut_Val_Anxiety_2)),
      textInput("Cutoff_Text_Anxiety_2", "Cut-Off Score Name", Cut_Lab_Anxiety_2),
      h6(paste("Reference:", Source_Cutoff_Anxiety_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Anxiety_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Impulse_Control_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Impulse_Control_2", "Impulse Control", as.numeric(Cut_Val_Impulse_Control_2)),
      textInput("Cutoff_Text_Impulse_Control_2", "Cut-Off Score Name", Cut_Lab_Impulse_Control_2),
      h6(paste("Reference:", Source_Cutoff_Impulse_Control_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Impulse_Control_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Avoidance_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Avoidance_2", "Avoidance", as.numeric(Cut_Val_Avoidance_2)),
      textInput("Cutoff_Text_Avoidance_2", "Cut-Off Score Name", Cut_Lab_Avoidance_2),
      h6(paste("Reference:", Source_Cutoff_Avoidance_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Avoidance_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_3", "ADNM-20 total scale", as.numeric(Cut_Val_3)),
      textInput("Cutoff_Text_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source_Cutoff_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Preoccupations_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Preoccupations_3", "Preoccupations", as.numeric(Cut_Val_Preoccupations_3)),
      textInput("Cutoff_Text_Preoccupations_3", "Cut-Off Score Name", Cut_Lab_Preoccupations_3),
      h6(paste("Reference:", Source_Cutoff_Preoccupations_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Preoccupations_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Failure_To_Adapt_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Failure_To_Adapt_3", "Failure To Adapt", as.numeric(Cut_Val_Failure_To_Adapt_3)),
      textInput("Cutoff_Text_Failure_To_Adapt_3", "Cut-Off Score Name", Cut_Lab_Failure_To_Adapt_3),
      h6(paste("Reference:", Source_Cutoff_Failure_To_Adapt_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Failure_To_Adapt_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Depressive_Mood_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Depressive_Mood_3", "Depressive Mood", as.numeric(Cut_Val_Depressive_Mood_3)),
      textInput("Cutoff_Text_Depressive_Mood_3", "Cut-Off Score Name", Cut_Lab_Depressive_Mood_3),
      h6(paste("Reference:", Source_Cutoff_Depressive_Mood_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Depressive_Mood_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Anxiety_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Anxiety_3", "Anxiety", as.numeric(Cut_Val_Anxiety_3)),
      textInput("Cutoff_Text_Anxiety_3", "Cut-Off Score Name", Cut_Lab_Anxiety_3),
      h6(paste("Reference:", Source_Cutoff_Anxiety_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Anxiety_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Impulse_Control_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Impulse_Control_3", "Impulse Control", as.numeric(Cut_Val_Impulse_Control_3)),
      textInput("Cutoff_Text_Impulse_Control_3", "Cut-Off Score Name", Cut_Lab_Impulse_Control_3),
      h6(paste("Reference:", Source_Cutoff_Impulse_Control_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Impulse_Control_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Avoidance_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Avoidance_3", "Avoidance", as.numeric(Cut_Val_Avoidance_3)),
      textInput("Cutoff_Text_Avoidance_3", "Cut-Off Score Name", Cut_Lab_Avoidance_3),
      h6(paste("Reference:", Source_Cutoff_Avoidance_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Avoidance_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_4<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_4", "ADNM-20 total scale", as.numeric(Cut_Val_4)),
      textInput("Cutoff_Text_4", "Cut-Off Score Name", Cut_Lab_4),
      h6(paste("Reference:", Source_Cutoff_4))
    )
  })
  outputOptions(output, "Cutoff_Widg_4", suspendWhenHidden = FALSE)
  
  #Create a reactive expression defining entered values  
  
  Entered_Scores_Reac<- reactive({
    
    TP<- input$Timepoint
    
    PN<<- input$PatientName
    CN<<- input$ClinicianName
    
    Pop<- input$Pop
    
    RelChangeMethod<- input$RelChangeMethod
    
    Tab_Reference<<- Source_Mean
    
    CI_Vals_Reac()
    
    M<- input$Pop_Mean
    SD<- input$Pop_Sd
    M_Preoccupations<- input$Pop_Mean_Preoccupations
    SD_Preoccupations<-input$Pop_Sd_Preoccupations
    M_Failure_To_Adapt<- input$Pop_Mean_Failure_To_Adapt
    SD_Failure_To_Adapt<- input$Pop_Sd_Failure_To_Adapt
    M_Depressive_Mood<- input$Pop_Mean_Depressive_Mood
    SD_Depressive_Mood<- input$Pop_Sd_Depressive_Mood
    M_Anxiety<- input$Pop_Mean_Anxiety
    SD_Anxiety<-input$Pop_Sd_Anxiety
    M_Impulse_Control<- input$Pop_Mean_Impulse_Control
    SD_Impulse_Control<- input$Pop_Sd_Impulse_Control
    M_Avoidance<- input$Pop_Mean_Avoidance
    SD_Avoidance<- input$Pop_Sd_Avoidance
   
    M_Retest<- input$Retest_Mean
    SD_Retest<- input$Retest_Sd
    M_Retest_Preoccupations<- input$Retest_Mean_Preoccupations
    SD_Retest_Preoccupations<- input$Retest_Sd_Preoccupations
    M_Retest_Failure_To_Adapt<- input$Retest_Mean_Failure_To_Adapt
    SD_Retest_Failure_To_Adapt<- input$Retest_Sd_Failure_To_Adapt
    M_Retest_Depressive_Mood<- input$Retest_Mean_Depressive_Mood
    SD_Retest_Depressive_Mood<- input$Retest_Sd_Depressive_Mood
    M_Retest_Anxiety<- input$Retest_Mean_Anxiety
    SD_Retest_Anxiety<- input$Retest_Sd_Anxiety
    M_Retest_Impulse_Control<- input$Retest_Mean_Impulse_Control
    SD_Retest_Impulse_Control<- input$Retest_Sd_Impulse_Control
    M_Retest_Avoidance<- input$Retest_Mean_Avoidance
    SD_Retest_Avoidance<- input$Retest_Sd_Avoidance
    
    SampleN<- input$SampleN
    
    Rel<- input$Reliability
    Rel_Preoccupations<- input$Reliability_Preoccupations
    Rel_Failure_To_Adapt<- input$Reliability_Failure_To_Adapt
    Rel_Depressive_Mood<- input$Reliability_Depressive_Mood
    Rel_Anxiety<- input$Reliability_Anxiety
    Rel_Impulse_Control<- input$Reliability_Impulse_Control
    Rel_Avoidance<- input$Reliability_Avoidance
   
    if(input$Confidence == "1") {
      Conf<- 1.645
    } else if(input$Confidence == "2") {
      Conf<- 1.96
    } else if(input$Confidence == "3") {
      Conf<- 2.575
    }
    
    #Generate confidence interval percentages to appear in the pdf report
    
    if(input$Confidence == "1") {
      ConfInt<- "99%"
    } else if(input$Confidence == "2") {
      ConfInt<- "95%"
    } else if(input$Confidence == "3") {
      ConfInt<- "90%"
    }
    
    #Calculate standard errors depending on reliable change method chosen: Crawford et al method needs to be created separately for each timepoint
    
    if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
      SE<-SD * sqrt(1 - Rel^2)
      SE_Preoccupations<-SD_Preoccupations * sqrt(1 - Rel_Preoccupations^2)
      SE_Failure_To_Adapt<-SD_Failure_To_Adapt * sqrt(1 - Rel_Failure_To_Adapt^2)
      SE_Depressive_Mood<-SD_Depressive_Mood * sqrt(1 - Rel_Depressive_Mood^2)
      SE<- round(SE, digits = 2)
      SE_Preoccupations<- round(SE_Preoccupations, digits = 2)
      SE_Failure_To_Adapt<- round(SE_Failure_To_Adapt, digits = 2)
      SE_Depressive_Mood<- round(SE_Depressive_Mood, digits = 2)
      SE_Anxiety<-SD_Anxiety * sqrt(1 - Rel_Anxiety^2)
      SE_Impulse_Control<-SD_Impulse_Control * sqrt(1 - Rel_Impulse_Control^2)
      SE_Avoidance<-SD_Avoidance * sqrt(1 - Rel_Avoidance^2)
      SE_Anxiety<- round(SE_Anxiety, digits = 2)
      SE_Impulse_Control<- round(SE_Impulse_Control, digits = 2)
      SE_Avoidance<- round(SE_Avoidance, digits = 2)
    } else if (input$RelChangeMethod == "Chelune et al. (1993)" | input$RelChangeMethod == "Jacobson & Truax (1991)" | input$RelChangeMethod == "Speer (1992)") {
      SE<- sqrt((2*(SD^2))*(1-Rel))
      SE_Preoccupations<- sqrt((2*(SD_Preoccupations^2))*(1-Rel_Preoccupations))
      SE_Failure_To_Adapt<- sqrt((2*(SD_Failure_To_Adapt^2))*(1-Rel_Failure_To_Adapt))
      SE_Depressive_Mood<- sqrt((2*(SD_Depressive_Mood^2))*(1-Rel_Depressive_Mood))
      SE_Anxiety<- sqrt((2*(SD_Anxiety^2))*(1-Rel_Anxiety))
      SE_Impulse_Control<- sqrt((2*(SD_Impulse_Control^2))*(1-Rel_Impulse_Control))
      SE_Avoidance<- sqrt((2*(SD_Avoidance^2))*(1-Rel_Avoidance))
      SE<- round(SE, digits = 2)
      SE_Preoccupations<- round(SE_Preoccupations, digits = 2)
      SE_Failure_To_Adapt<- round(SE_Failure_To_Adapt, digits = 2)
      SE_Depressive_Mood<- round(SE_Depressive_Mood, digits = 2)
      SE_Anxiety<- round(SE_Anxiety, digits = 2)
      SE_Impulse_Control<- round(SE_Impulse_Control, digits = 2)
      SE_Avoidance<- round(SE_Avoidance, digits = 2)
    } else if (input$RelChangeMethod == "Maassen et al. (2006)") {
      SE<- sqrt((SD^2 + SD_Retest^2)*(1-Rel))
      SE_Preoccupations<- sqrt((SD_Preoccupations^2 + SD_Retest_Preoccupations^2)*(1-Rel_Preoccupations))
      SE_Failure_To_Adapt<- sqrt((SD_Failure_To_Adapt^2 + SD_Retest_Failure_To_Adapt^2)*(1-Rel_Failure_To_Adapt))
      SE_Depressive_Mood<- sqrt((SD_Depressive_Mood^2 + SD_Retest_Depressive_Mood^2)*(1-Rel_Depressive_Mood))
      SE<- round(SE, digits = 2)
      SE_Preoccupations<- round(SE_Preoccupations, digits = 2)
      SE_Failure_To_Adapt<- round(SE_Failure_To_Adapt, digits = 2)
      SE_Depressive_Mood<- round(SE_Depressive_Mood, digits = 2)
      SE_Anxiety<- sqrt((SD_Anxiety^2 + SD_Retest_Anxiety^2)*(1-Rel_Anxiety))
      SE_Impulse_Control<- sqrt((SD_Impulse_Control^2 + SD_Retest_Impulse_Control^2)*(1-Rel_Impulse_Control))
      SE_Avoidance<- sqrt((SD_Avoidance^2 + SD_Retest_Avoidance^2)*(1-Rel_Avoidance))
      SE<- round(SE, digits = 2)
      SE_Anxiety<- round(SE_Anxiety, digits = 2)
      SE_Impulse_Control<- round(SE_Impulse_Control, digits = 2)
      SE_Avoidance<- round(SE_Avoidance, digits = 2)
    } else if (input$RelChangeMethod == "McSweeny et al. (1993)") {
      SE<- SD_Retest*sqrt(1 - Rel^2)
      SE_Preoccupations<- SD_Retest_Preoccupations*sqrt(1 - Rel_Preoccupations^2)
      SE_Failure_To_Adapt<- SD_Retest_Failure_To_Adapt*sqrt(1 - Rel_Failure_To_Adapt^2)
      SE_Depressive_Mood<- SD_Retest_Depressive_Mood*sqrt(1 - Rel_Depressive_Mood^2)
      SE<- round(SE, digits = 2)
      SE_Preoccupations<- round(SE_Preoccupations, digits = 2)
      SE_Failure_To_Adapt<- round(SE_Failure_To_Adapt, digits = 2)
      SE_Depressive_Mood<- round(SE_Depressive_Mood, digits = 2)
      SE_Anxiety<- SD_Retest_Anxiety*sqrt(1 - Rel_Anxiety^2)
      SE_Impulse_Control<- SD_Retest_Impulse_Control*sqrt(1 - Rel_Impulse_Control^2)
      SE_Avoidance<- SD_Retest_Avoidance*sqrt(1 - Rel_Avoidance^2)
      SE<- round(SE, digits = 2)
      SE_Anxiety<- round(SE_Anxiety, digits = 2)
      SE_Impulse_Control<- round(SE_Impulse_Control, digits = 2)
      SE_Avoidance<- round(SE_Avoidance, digits = 2)
    }
    
    #Caclculate McSweeny standard error to be employed in deriving the Crawford & Howell standard error if selected
    
    McSweeny_SE<- SD_Retest*sqrt(1 - Rel^2)
    McSweeny_SE_Preoccupations<- SD_Retest_Preoccupations*sqrt(1 - Rel_Preoccupations^2)
    McSweeny_SE_Failure_To_Adapt<- SD_Retest_Failure_To_Adapt*sqrt(1 - Rel_Failure_To_Adapt^2)
    McSweeny_SE_Depressive_Mood<- SD_Retest_Depressive_Mood*sqrt(1 - Rel_Depressive_Mood^2)
    McSweeny_SE_Anxiety<- SD_Retest_Anxiety*sqrt(1 - Rel_Anxiety^2)
    McSweeny_SE_Impulse_Control<- SD_Retest_Impulse_Control*sqrt(1 - Rel_Impulse_Control^2)
    McSweeny_SE_Avoidance<- SD_Retest_Avoidance*sqrt(1 - Rel_Avoidance^2)
    
    #Generate object storing names of cut-off scores
    
    Cutoff_Name_1<- input$Cutoff_Text_1
    Cutoff_Name_2<- input$Cutoff_Text_2
    Cutoff_Name_3<- input$Cutoff_Text_3
    Cutoff_Name_4<- input$Cutoff_Text_4
    Cutoff_Name_Preoccupations_1<- input$Cutoff_Text_Preoccupations_1
    Cutoff_Name_Preoccupations_2<- input$Cutoff_Text_Preoccupations_2
    Cutoff_Name_Preoccupations_3<- input$Cutoff_Text_Preoccupations_3
    Cutoff_Name_Failure_To_Adapt_1<- input$Cutoff_Text_Failure_To_Adapt_1
    Cutoff_Name_Failure_To_Adapt_2<- input$Cutoff_Text_Failure_To_Adapt_2
    Cutoff_Name_Failure_To_Adapt_3<- input$Cutoff_Text_Failure_To_Adapt_3
    Cutoff_Name_Depressive_Mood_1<- input$Cutoff_Text_Depressive_Mood_1
    Cutoff_Name_Depressive_Mood_2<- input$Cutoff_Text_Depressive_Mood_2
    Cutoff_Name_Depressive_Mood_3<- input$Cutoff_Text_Depressive_Mood_3
    Cutoff_Name_Anxiety_1<- input$Cutoff_Text_Anxiety_1
    Cutoff_Name_Anxiety_2<- input$Cutoff_Text_Anxiety_2
    Cutoff_Name_Anxiety_3<- input$Cutoff_Text_Anxiety_3
    Cutoff_Name_Impulse_Control_1<- input$Cutoff_Text_Impulse_Control_1
    Cutoff_Name_Impulse_Control_2<- input$Cutoff_Text_Impulse_Control_2
    Cutoff_Name_Impulse_Control_3<- input$Cutoff_Text_Impulse_Control_3
    Cutoff_Name_Avoidance_1<- input$Cutoff_Text_Avoidance_1
    Cutoff_Name_Avoidance_2<- input$Cutoff_Text_Avoidance_2
    Cutoff_Name_Avoidance_3<- input$Cutoff_Text_Avoidance_3
   
    
    Cutoff_Names<<- data.frame(Cutoff_Name_1,Cutoff_Name_2,Cutoff_Name_3,Cutoff_Name_4, Cutoff_Name_Preoccupations_1,Cutoff_Name_Preoccupations_2,Cutoff_Name_Preoccupations_3,
                               Cutoff_Name_Failure_To_Adapt_1, Cutoff_Name_Failure_To_Adapt_2, Cutoff_Name_Failure_To_Adapt_3, Cutoff_Name_Depressive_Mood_1,
                               Cutoff_Name_Depressive_Mood_2, Cutoff_Name_Depressive_Mood_3,
                               Cutoff_Name_Anxiety_1,Cutoff_Name_Anxiety_2,Cutoff_Name_Anxiety_3,
                               Cutoff_Name_Impulse_Control_1, Cutoff_Name_Impulse_Control_2, Cutoff_Name_Impulse_Control_3, Cutoff_Name_Avoidance_1,
                               Cutoff_Name_Avoidance_2, Cutoff_Name_Avoidance_3
                               )
    
    #Define the data & calculate PS, SE, confidence intervals and cut-off scores
    
    if(input$Timepoint == "1") {
      Date<- input$Date_1
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Duration_Score<- as.character(unlist(strsplit(input$Duration_1,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_1a), Frequency = Score_1a, Duration = Duration_Score)
      Event<- c("Divorce/separation", "Family conflicts", "Conflicts in Worklife", "Conflicts with neighbours",
                "Illness of a loved one", "Death of a loved one", "Adjustment due to retirement", "Unemployment",
                "Too much/too little work", "Pressure to meet deadlines/time pressure", "Moving to a new home",
                "Financial problems", "Own serious illness", "Serious accident", "Assault", "Termination of an important leisure activity",
                input$Other_Event_1, input$Other_Event_2)
      Event_Dates<- c(input$Event_Date_1, paste(input$Event_Date_2,input$Event_Date_2a), paste(input$Event_Date_3, "to", input$Event_Date_23a),
                      paste(input$Event_Date_4, "to", input$Event_Date_4a), paste(input$Event_Date_5, "to", input$Event_Date_5a), input$Event_Date_6,
                      input$Event_Date_7, paste(input$Event_Date_8, "to", input$Event_Date_8a), paste(input$Event_Date_9, "to", input$Event_Date_9a),
                      paste(input$Event_Date_10, "to", input$Event_Date_10a), input$Event_Date_11, paste(input$Event_Date_12, "to", input$Event_Date_12a),
                      paste(input$Event_Date_13, "to", input$Event_Date_13a), input$Event_Date_14, input$Event_Date_15, paste(input$Event_Date_16, "to", input$Event_Date_16a),
                      paste(input$Event_Date_17, "to", input$Event_Date_17a), paste(input$Event_Date_18, "to", input$Event_Date_18a))
      Event_Endorsement<- c(input$Yes_1, input$Yes_2, input$Yes_3, input$Yes_4, input$Yes_5, input$Yes_6, input$Yes_7, input$Yes_8, input$Yes_9, input$Yes_10, input$Yes_11, 
                            input$Yes_12, input$Yes_13, input$Yes_14, input$Yes_15, input$Yes_16, input$Yes_11, input$Yes_18)
      Event_Df<<- data.frame(Event, Endorsement = Event_Endorsement, Date = Event_Dates)
      Straining_Event<<- input$Straining_Event
      Score<- sum(Score_1a, na.rm = TRUE)
      Score<- round(Score, digits = 2)
      Score_Preoccupations<- sum(Score_1a[c(2,4,13,15)], na.rm = TRUE)
      Score_Failure_To_Adapt<- sum(Score_1a[c(10,17,19,20)], na.rm = TRUE)
      Score_Depressive_Mood<- sum(Score_1a[c(1,5,18)], na.rm = TRUE)
      Score_Anxiety<- sum(Score_1a[c(6,16)], na.rm = TRUE)
      Score_Impulse_Control<- sum(Score_1a[c(8,9,12)], na.rm = TRUE)
      Score_Avoidance<- sum(Score_1a[c(3,7,11,14)], na.rm = TRUE)
      Change<- 0
      Change_Preoccupations<- 0
      Change_Failure_To_Adapt<- 0
      Change_Depressive_Mood<- 0
      Change_Anxiety<- 0
      Change_Impulse_Control<- 0
      Change_Avoidance<- 0
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS<- (Rel * Score) + (M * (1 - Rel))
        PTS_Preoccupations<- (Rel_Preoccupations * Score_Preoccupations) + (M_Preoccupations * (1 - Rel_Preoccupations))
        PTS_Failure_To_Adapt<- (Rel_Failure_To_Adapt * Score_Failure_To_Adapt) + (M_Failure_To_Adapt * (1 - Rel_Failure_To_Adapt))
        PTS_Depressive_Mood<- (Rel_Depressive_Mood * Score_Depressive_Mood) + (M_Depressive_Mood * (1 - Rel_Depressive_Mood))
        PTS_Anxiety<- (Rel_Anxiety * Score_Anxiety) + (M_Anxiety * (1 - Rel_Anxiety))
        PTS_Impulse_Control<- (Rel_Impulse_Control * Score_Impulse_Control) + (M_Impulse_Control * (1 - Rel_Impulse_Control))
        PTS_Avoidance<- (Rel_Avoidance * Score_Avoidance) + (M_Avoidance * (1 - Rel_Avoidance))
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS<- Score + (M_Retest - M)  
        PTS_Preoccupations<- Score_Preoccupations + (M_Retest_Preoccupations - M_Preoccupations)  
        PTS_Failure_To_Adapt<- Score_Failure_To_Adapt + (M_Retest_Failure_To_Adapt - M_Failure_To_Adapt)  
        PTS_Depressive_Mood<- Score_Depressive_Mood + (M_Retest_Depressive_Mood - M_Depressive_Mood) 
        PTS_Anxiety<- Score_Anxiety + (M_Retest_Anxiety - M_Anxiety)  
        PTS_Impulse_Control<- Score_Impulse_Control + (M_Retest_Impulse_Control - M_Impulse_Control)  
        PTS_Avoidance<- Score_Avoidance + (M_Retest_Avoidance - M_Avoidance) 
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Preoccupations<- Score_Preoccupations
        PTS_Failure_To_Adapt<- Score_Failure_To_Adapt
        PTS_Depressive_Mood<- Score_Depressive_Mood
        PTS_Anxiety<- Score_Anxiety
        PTS_Impulse_Control<- Score_Impulse_Control
        PTS_Avoidance<- Score_Avoidance
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS<- (B_Adj * Score) + A_Adj
        B_Slope_Preoccupations<- Rel_Preoccupations * (SD_Retest_Preoccupations/SD_Preoccupations)
        A_Constant_Preoccupations<- M_Retest_Preoccupations - (B_Slope_Preoccupations * M_Preoccupations)
        B_Adj_Preoccupations<- SD_Retest_Preoccupations/SD_Preoccupations
        A_Adj_Preoccupations<- M_Retest_Preoccupations - (B_Adj_Preoccupations * M_Preoccupations)
        PTS_Preoccupations<- (B_Adj_Preoccupations * Score_Preoccupations) + A_Adj_Preoccupations
        B_Slope_Failure_To_Adapt<- Rel_Failure_To_Adapt * (SD_Retest_Failure_To_Adapt/SD_Failure_To_Adapt)
        A_Constant_Failure_To_Adapt<- M_Retest_Failure_To_Adapt - (B_Slope_Failure_To_Adapt * M_Failure_To_Adapt)
        B_Adj_Failure_To_Adapt<- SD_Retest_Failure_To_Adapt/SD_Failure_To_Adapt
        A_Adj_Failure_To_Adapt<- M_Retest_Failure_To_Adapt - (B_Adj_Failure_To_Adapt * M_Failure_To_Adapt)
        PTS_Failure_To_Adapt<- (B_Adj_Failure_To_Adapt * Score_Failure_To_Adapt) + A_Adj_Failure_To_Adapt
        B_Slope_Depressive_Mood<- Rel_Depressive_Mood * (SD_Retest_Depressive_Mood/SD_Depressive_Mood)
        A_Constant_Depressive_Mood<- M_Retest_Depressive_Mood - (B_Slope_Depressive_Mood * M_Depressive_Mood)
        B_Adj_Depressive_Mood<- SD_Retest_Depressive_Mood/SD_Depressive_Mood
        A_Adj_Depressive_Mood<- M_Retest_Depressive_Mood - (B_Adj_Depressive_Mood * M_Depressive_Mood)
        PTS_Depressive_Mood<- (B_Adj_Depressive_Mood * Score_Depressive_Mood) + A_Adj_Depressive_Mood
        B_Slope_Anxiety<- Rel_Anxiety * (SD_Retest_Anxiety/SD_Anxiety)
        A_Constant_Anxiety<- M_Retest_Anxiety - (B_Slope_Anxiety * M_Anxiety)
        B_Adj_Anxiety<- SD_Retest_Anxiety/SD_Anxiety
        A_Adj_Anxiety<- M_Retest_Anxiety - (B_Adj_Anxiety * M_Anxiety)
        PTS_Anxiety<- (B_Adj_Anxiety * Score_Anxiety) + A_Adj_Anxiety
        B_Slope_Impulse_Control<- Rel_Impulse_Control * (SD_Retest_Impulse_Control/SD_Impulse_Control)
        A_Constant_Impulse_Control<- M_Retest_Impulse_Control - (B_Slope_Impulse_Control * M_Impulse_Control)
        B_Adj_Impulse_Control<- SD_Retest_Impulse_Control/SD_Impulse_Control
        A_Adj_Impulse_Control<- M_Retest_Impulse_Control - (B_Adj_Impulse_Control * M_Impulse_Control)
        PTS_Impulse_Control<- (B_Adj_Impulse_Control * Score_Impulse_Control) + A_Adj_Impulse_Control
        B_Slope_Avoidance<- Rel_Avoidance * (SD_Retest_Avoidance/SD_Avoidance)
        A_Constant_Avoidance<- M_Retest_Avoidance - (B_Slope_Avoidance * M_Avoidance)
        B_Adj_Avoidance<- SD_Retest_Avoidance/SD_Avoidance
        A_Adj_Avoidance<- M_Retest_Avoidance - (B_Adj_Avoidance * M_Avoidance)
        PTS_Avoidance<- (B_Adj_Avoidance * Score_Avoidance) + A_Adj_Avoidance
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS<- B_Slope * Score
        B_Slope_Preoccupations<- Rel_Preoccupations * (SD_Retest_Preoccupations/SD_Preoccupations)
        PTS_Preoccupations<- B_Slope_Preoccupations * Score_Preoccupations
        B_Slope_Failure_To_Adapt<- Rel_Failure_To_Adapt * (SD_Retest_Failure_To_Adapt/SD_Failure_To_Adapt)
        PTS_Failure_To_Adapt<- B_Slope_Failure_To_Adapt * Score_Failure_To_Adapt
        B_Slope_Depressive_Mood<- Rel_Depressive_Mood * (SD_Retest_Depressive_Mood/SD_Depressive_Mood)
        PTS_Depressive_Mood<- B_Slope_Depressive_Mood * Score_Depressive_Mood
        B_Slope_Anxiety<- Rel_Anxiety * (SD_Retest_Anxiety/SD_Anxiety)
        PTS_Anxiety<- B_Slope_Anxiety * Score_Anxiety
        B_Slope_Impulse_Control<- Rel_Impulse_Control * (SD_Retest_Impulse_Control/SD_Impulse_Control)
        PTS_Impulse_Control<- B_Slope_Impulse_Control * Score_Impulse_Control
        B_Slope_Avoidance<- Rel_Avoidance * (SD_Retest_Avoidance/SD_Avoidance)
        PTS_Avoidance<- B_Slope_Avoidance * Score_Avoidance
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS<- Score + (M_Retest - M)
        PTS_Preoccupations<- Score_Preoccupations + (M_Retest_Preoccupations - M_Preoccupations)
        PTS_Failure_To_Adapt<- Score_Failure_To_Adapt + (M_Retest_Failure_To_Adapt - M_Failure_To_Adapt)
        PTS_Depressive_Mood<- Score_Depressive_Mood + (M_Retest_Depressive_Mood - M_Depressive_Mood)
        PTS_Anxiety<- Score_Anxiety + (M_Retest_Anxiety - M_Anxiety)
        PTS_Impulse_Control<- Score_Impulse_Control + (M_Retest_Impulse_Control - M_Impulse_Control)
        PTS_Avoidance<- Score_Avoidance + (M_Retest_Avoidance - M_Avoidance)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Preoccupations<- round(PTS_Preoccupations, digits = 2)
      PTS_Failure_To_Adapt<- round(PTS_Failure_To_Adapt, digits = 2)
      PTS_Depressive_Mood<- round(PTS_Depressive_Mood, digits = 2)
      PTS_Anxiety<- round(PTS_Anxiety, digits = 2)
      PTS_Impulse_Control<- round(PTS_Impulse_Control, digits = 2)
      PTS_Avoidance<- round(PTS_Avoidance, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score - M)^2/(SD^2*(SampleN-1))))
        SE_Preoccupations<- McSweeny_SE_Preoccupations*sqrt(1 + (1/SampleN) + ((Score_Preoccupations - M_Preoccupations)^2/(SD_Preoccupations^2*(SampleN-1))))
        SE_Failure_To_Adapt<- McSweeny_SE_Failure_To_Adapt*sqrt(1 + (1/SampleN) + ((Score_Failure_To_Adapt - M_Failure_To_Adapt)^2/(SD_Failure_To_Adapt^2*(SampleN-1))))
        SE_Depressive_Mood<- McSweeny_SE_Depressive_Mood*sqrt(1 + (1/SampleN) + ((Score_Depressive_Mood - M_Depressive_Mood)^2/(SD_Depressive_Mood^2*(SampleN-1))))
        SE_Anxiety<- McSweeny_SE_Anxiety*sqrt(1 + (1/SampleN) + ((Score_Anxiety - M_Anxiety)^2/(SD_Anxiety^2*(SampleN-1))))
        SE_Impulse_Control<- McSweeny_SE_Impulse_Control*sqrt(1 + (1/SampleN) + ((Score_Impulse_Control - M_Impulse_Control)^2/(SD_Impulse_Control^2*(SampleN-1))))
        SE_Avoidance<- McSweeny_SE_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Avoidance - M_Avoidance)^2/(SD_Avoidance^2*(SampleN-1))))
        SE<- round(SE, digits = 2)
        SE_Preoccupations<- round(SE_Preoccupations, digits = 2)
        SE_Failure_To_Adapt<- round(SE_Failure_To_Adapt, digits = 2)
        SE_Depressive_Mood<- round(SE_Depressive_Mood, digits = 2)
        SE_Anxiety<- round(SE_Anxiety, digits = 2)
        SE_Impulse_Control<- round(SE_Impulse_Control, digits = 2)
        SE_Avoidance<- round(SE_Avoidance, digits = 2)
        CI<- (Conf*SE)
        CI<- round(CI, digits = 2)
        CI_Preoccupations<- (Conf*SE_Preoccupations)
        CI_Preoccupations<- round(CI_Preoccupations, digits = 2)
        CI_Failure_To_Adapt<- (Conf*SE_Failure_To_Adapt)
        CI_Failure_To_Adapt<- round(CI_Failure_To_Adapt, digits = 2)
        CI_Depressive_Mood<- (Conf*SE_Depressive_Mood)
        CI_Depressive_Mood<- round(CI_Depressive_Mood, digits = 2)
        CI_Anxiety<- (Conf*SE_Anxiety)
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Impulse_Control<- (Conf*SE_Impulse_Control)
        CI_Impulse_Control<- round(CI_Impulse_Control, digits = 2)
        CI_Avoidance<- (Conf*SE_Avoidance)
        CI_Avoidance<- round(CI_Avoidance, digits = 2)
      }
      CI<- (Conf*SE)
      CI<- round(CI, digits = 2)
      CI<- round(CI, digits = 2)
      CI_Preoccupations<- (Conf*SE_Preoccupations)
      CI_Preoccupations<- round(CI_Preoccupations, digits = 2)
      CI_Failure_To_Adapt<- (Conf*SE_Failure_To_Adapt)
      CI_Failure_To_Adapt<- round(CI_Failure_To_Adapt, digits = 2)
      CI_Depressive_Mood<- (Conf*SE_Depressive_Mood)
      CI_Depressive_Mood<- round(CI_Depressive_Mood, digits = 2)
      CI_Anxiety<- (Conf*SE_Anxiety)
      CI_Anxiety<- round(CI_Anxiety, digits = 2)
      CI_Impulse_Control<- (Conf*SE_Impulse_Control)
      CI_Impulse_Control<- round(CI_Impulse_Control, digits = 2)
      CI_Avoidance<- (Conf*SE_Avoidance)
      CI_Avoidance<- round(CI_Avoidance, digits = 2)
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Preoccupations<- PTS_Preoccupations + CI_Preoccupations
      CI_Upper_Lim_Preoccupations<- round(CI_Upper_Lim_Preoccupations, digits = 2)
      CI_Lower_Lim_Preoccupations<-PTS_Preoccupations - CI_Preoccupations
      CI_Lower_Lim_Preoccupations<- round(CI_Lower_Lim_Preoccupations, digits = 2)
      CI_Upper_Lim_Failure_To_Adapt<- PTS_Failure_To_Adapt + CI_Failure_To_Adapt
      CI_Upper_Lim_Failure_To_Adapt<- round(CI_Upper_Lim_Failure_To_Adapt, digits = 2)
      CI_Lower_Lim_Failure_To_Adapt<-PTS_Failure_To_Adapt - CI_Failure_To_Adapt
      CI_Lower_Lim_Failure_To_Adapt<- round(CI_Lower_Lim_Failure_To_Adapt, digits = 2)
      CI_Upper_Lim_Depressive_Mood<- PTS_Depressive_Mood + CI_Depressive_Mood
      CI_Upper_Lim_Depressive_Mood<- round(CI_Upper_Lim_Depressive_Mood, digits = 2)
      CI_Lower_Lim_Depressive_Mood<-PTS_Depressive_Mood - CI_Depressive_Mood
      CI_Lower_Lim_Depressive_Mood<- round(CI_Lower_Lim_Depressive_Mood, digits = 2)
      CI_Upper_Lim_Anxiety<- PTS_Anxiety + CI_Anxiety
      CI_Upper_Lim_Anxiety<- round(CI_Upper_Lim_Anxiety, digits = 2)
      CI_Lower_Lim_Anxiety<-PTS_Anxiety - CI_Anxiety
      CI_Lower_Lim_Anxiety<- round(CI_Lower_Lim_Anxiety, digits = 2)
      CI_Upper_Lim_Impulse_Control<- PTS_Impulse_Control + CI_Impulse_Control
      CI_Upper_Lim_Impulse_Control<- round(CI_Upper_Lim_Impulse_Control, digits = 2)
      CI_Lower_Lim_Impulse_Control<- PTS_Impulse_Control - CI_Impulse_Control
      CI_Lower_Lim_Impulse_Control<- round(CI_Lower_Lim_Impulse_Control, digits = 2)
      CI_Upper_Lim_Avoidance<- PTS_Avoidance + CI_Avoidance
      CI_Upper_Lim_Avoidance<- round(CI_Upper_Lim_Avoidance, digits = 2)
      CI_Lower_Lim_Avoidance<-PTS_Avoidance - CI_Avoidance
      CI_Lower_Lim_Avoidance<- round(CI_Lower_Lim_Avoidance, digits = 2)
      
      if(input$Select_CI == "2") {
        CI<- input$Man_CI
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Preoccupations == "2") {
        CI_Preoccupations<- input$Man_CI_Preoccupations
        CI_Preoccupations<- round(CI_Preoccupations, digits = 2)
        CI_Upper_Lim_Preoccupations<- Score_Preoccupations + CI_Preoccupations
        CI_Upper_Lim_Preoccupations<- round(CI_Upper_Lim_Preoccupations, digits = 2)
        CI_Lower_Lim_Preoccupations<- Score_Preoccupations - CI_Preoccupations
        CI_Lower_Lim_Preoccupations<- round(CI_Lower_Lim_Preoccupations, digits = 2)
      }
      if(input$Select_CI_Failure_To_Adapt == "2") {
        CI_Failure_To_Adapt<- input$Man_CI_Failure_To_Adapt
        CI_Failure_To_Adapt<- round(CI_Failure_To_Adapt, digits = 2)
        CI_Upper_Lim_Failure_To_Adapt<- Score_Failure_To_Adapt + CI_Failure_To_Adapt
        CI_Upper_Lim_Failure_To_Adapt<- round(CI_Upper_Lim_Failure_To_Adapt, digits = 2)
        CI_Lower_Lim_Failure_To_Adapt<- Score_Failure_To_Adapt - CI_Failure_To_Adapt
        CI_Lower_Lim_Failure_To_Adapt<- round(CI_Lower_Lim_Failure_To_Adapt, digits = 2)
      }
      if(input$Select_CI_Depressive_Mood == "2") {
        CI_Depressive_Mood<- input$Man_CI_Depressive_Mood
        CI_Depressive_Mood<- round(CI_Depressive_Mood, digits = 2)
        CI_Upper_Lim_Depressive_Mood<- Score_Depressive_Mood + CI_Depressive_Mood
        CI_Upper_Lim_Depressive_Mood<- round(CI_Upper_Lim_Depressive_Mood, digits = 2)
        CI_Lower_Lim_Depressive_Mood<- Score_Depressive_Mood - CI_Depressive_Mood
        CI_Lower_Lim_Depressive_Mood<- round(CI_Lower_Lim_Depressive_Mood, digits = 2)
      }
      if(input$Select_CI_Anxiety == "2") {
        CI_Anxiety<- input$Man_CI_Anxiety
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Upper_Lim_Anxiety<- Score_Anxiety + CI_Anxiety
        CI_Upper_Lim_Anxiety<- round(CI_Upper_Lim_Anxiety, digits = 2)
        CI_Lower_Lim_Anxiety<- Score_Anxiety - CI_Anxiety
        CI_Lower_Lim_Anxiety<- round(CI_Lower_Lim_Anxiety, digits = 2)
      }
      if(input$Select_CI_Impulse_Control == "2") {
        CI_Impulse_Control<- input$Man_CI_Impulse_Control
        CI_Impulse_Control<- round(CI_Impulse_Control, digits = 2)
        CI_Upper_Lim_Impulse_Control<- Score_Impulse_Control + CI_Impulse_Control
        CI_Upper_Lim_Impulse_Control<- round(CI_Upper_Lim_Impulse_Control, digits = 2)
        CI_Lower_Lim_Impulse_Control<- Score_Impulse_Control - CI_Impulse_Control
        CI_Lower_Lim_Impulse_Control<- round(CI_Lower_Lim_Impulse_Control, digits = 2)
      }
      if(input$Select_CI_Avoidance == "2") {
        CI_Avoidance<- input$Man_CI_Avoidance
        CI_Avoidance<- round(CI_Avoidance, digits = 2)
        CI_Upper_Lim_Avoidance<- Score_Avoidance + CI_Avoidance
        CI_Upper_Lim_Avoidance<- round(CI_Upper_Lim_Avoidance, digits = 2)
        CI_Lower_Lim_Avoidance<- Score_Avoidance - CI_Avoidance
        CI_Lower_Lim_Avoidance<- round(CI_Lower_Lim_Avoidance, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_4<- round(input$Cutoff_4, digits = 2)
      Cutoff_Score_Preoccupations_1<- round(input$Cutoff_Preoccupations_1, digits = 2)
      Cutoff_Score_Preoccupations_2<- round(input$Cutoff_Preoccupations_2, digits = 2)
      Cutoff_Score_Preoccupations_3<- round(input$Cutoff_Preoccupations_3, digits = 2)
      Cutoff_Score_Failure_To_Adapt_1<- round(input$Cutoff_Failure_To_Adapt_1, digits = 2)
      Cutoff_Score_Failure_To_Adapt_2<- round(input$Cutoff_Failure_To_Adapt_2, digits = 2)
      Cutoff_Score_Failure_To_Adapt_3<- round(input$Cutoff_Failure_To_Adapt_3, digits = 2)
      Cutoff_Score_Depressive_Mood_1<- round(input$Cutoff_Depressive_Mood_1, digits = 2)
      Cutoff_Score_Depressive_Mood_2<- round(input$Cutoff_Depressive_Mood_2, digits = 2)
      Cutoff_Score_Depressive_Mood_3<- round(input$Cutoff_Depressive_Mood_3, digits = 2)
      Cutoff_Score_Anxiety_1<- round(input$Cutoff_Anxiety_1, digits = 2)
      Cutoff_Score_Anxiety_2<- round(input$Cutoff_Anxiety_2, digits = 2)
      Cutoff_Score_Anxiety_3<- round(input$Cutoff_Anxiety_3, digits = 2)
      Cutoff_Score_Impulse_Control_1<- round(input$Cutoff_Impulse_Control_1, digits = 2)
      Cutoff_Score_Impulse_Control_2<- round(input$Cutoff_Impulse_Control_2, digits = 2)
      Cutoff_Score_Impulse_Control_3<- round(input$Cutoff_Impulse_Control_3, digits = 2)
      Cutoff_Score_Avoidance_1<- round(input$Cutoff_Avoidance_1, digits = 2)
      Cutoff_Score_Avoidance_2<- round(input$Cutoff_Avoidance_2, digits = 2)
      Cutoff_Score_Avoidance_3<- round(input$Cutoff_Avoidance_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Cutoff_Score_4,Score_Preoccupations,Change_Preoccupations,PTS_Preoccupations, SE_Preoccupations, CI_Upper_Lim_Preoccupations, CI_Lower_Lim_Preoccupations, Cutoff_Score_Preoccupations_1,Cutoff_Score_Preoccupations_2,Cutoff_Score_Preoccupations_3,
                                      Score_Failure_To_Adapt,Change_Failure_To_Adapt, PTS_Failure_To_Adapt, SE_Failure_To_Adapt, CI_Upper_Lim_Failure_To_Adapt, CI_Lower_Lim_Failure_To_Adapt, Cutoff_Score_Failure_To_Adapt_1,Cutoff_Score_Failure_To_Adapt_2,Cutoff_Score_Failure_To_Adapt_3, 
                                      Score_Depressive_Mood,Change_Depressive_Mood,PTS_Depressive_Mood, SE_Depressive_Mood, CI_Upper_Lim_Depressive_Mood, CI_Lower_Lim_Depressive_Mood, Cutoff_Score_Depressive_Mood_1,Cutoff_Score_Depressive_Mood_2,Cutoff_Score_Depressive_Mood_3, 
                                      Score_Anxiety,Change_Anxiety,PTS_Anxiety, SE_Anxiety, CI_Upper_Lim_Anxiety, CI_Lower_Lim_Anxiety, Cutoff_Score_Anxiety_1,Cutoff_Score_Anxiety_2,Cutoff_Score_Anxiety_3,
                                      Score_Impulse_Control,Change_Impulse_Control, PTS_Impulse_Control, SE_Impulse_Control, CI_Upper_Lim_Impulse_Control, CI_Lower_Lim_Impulse_Control, Cutoff_Score_Impulse_Control_1,Cutoff_Score_Impulse_Control_2,Cutoff_Score_Impulse_Control_3, 
                                      Score_Avoidance,Change_Avoidance,PTS_Avoidance, SE_Avoidance, CI_Upper_Lim_Avoidance, CI_Lower_Lim_Avoidance, Cutoff_Score_Avoidance_1,Cutoff_Score_Avoidance_2,Cutoff_Score_Avoidance_3
                                      )
    } else if(input$Timepoint == "2") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date<- c(Date_1, Date_2)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Score_1<- sum(Score_1a, na.rm = TRUE)
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Duration_Score<- as.character(unlist(strsplit(input$Duration_2,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_2a), Frequency = Score_2a, Duration = Duration_Score)
      Event<- c("Divorce/separation", "Family conflicts", "Conflicts in Worklife", "Conflicts with neighbours",
                "Illness of a loved one", "Death of a loved one", "Adjustment due to retirement", "Unemployment",
                "Too much/too little work", "Pressure to meet deadlines/time pressure", "Moving to a new home",
                "Financial problems", "Own serious illness", "Serious accident", "Assault", "Termination of an important leisure activity",
                input$Other_Event_1, input$Other_Event_2)
      Event_Dates<- c(input$Event_Date_1, paste(input$Event_Date_2,input$Event_Date_2a), paste(input$Event_Date_3, "to", input$Event_Date_23a),
                      paste(input$Event_Date_4, "to", input$Event_Date_4a), paste(input$Event_Date_5, "to", input$Event_Date_5a), input$Event_Date_6,
                      input$Event_Date_7, paste(input$Event_Date_8, "to", input$Event_Date_8a), paste(input$Event_Date_9, "to", input$Event_Date_9a),
                      paste(input$Event_Date_10, "to", input$Event_Date_10a), input$Event_Date_11, paste(input$Event_Date_12, "to", input$Event_Date_12a),
                      paste(input$Event_Date_13, "to", input$Event_Date_13a), input$Event_Date_14, input$Event_Date_15, paste(input$Event_Date_16, "to", input$Event_Date_16a),
                      paste(input$Event_Date_17, "to", input$Event_Date_17a), paste(input$Event_Date_18, "to", input$Event_Date_18a))
      Event_Endorsement<- c(input$Yes_1, input$Yes_2, input$Yes_3, input$Yes_4, input$Yes_5, input$Yes_6, input$Yes_7, input$Yes_8, input$Yes_9, input$Yes_10, input$Yes_11, 
                            input$Yes_12, input$Yes_13, input$Yes_14, input$Yes_15, input$Yes_16, input$Yes_11, input$Yes_18)
      Event_Df<<- data.frame(Event, Endorsement = Event_Endorsement, Date = Event_Dates)
      Straining_Event<<- input$Straining_Event
      Score_2<- sum(Score_2a, na.rm = TRUE)
      Score<- c(Score_1, Score_2)
      Score<- round(Score, digits = 2)
      Score_Preoccupations_1<- sum(Score_1a[c(2,4,13,15)], na.rm = TRUE)
      Score_Preoccupations_2<- sum(Score_2a[c(2,4,13,15)], na.rm = TRUE)
      Score_Preoccupations<- c(Score_Preoccupations_1, Score_Preoccupations_2)
      Score_Preoccupations<- round(Score_Preoccupations, digits = 2)
      Score_Failure_To_Adapt_1<- sum(Score_1a[c(10,17,19,20)], na.rm = TRUE)
      Score_Failure_To_Adapt_2<- sum(Score_2a[c(10,17,19,20)], na.rm = TRUE)
      Score_Failure_To_Adapt<- c(Score_Failure_To_Adapt_1,Score_Failure_To_Adapt_2)
      Score_Failure_To_Adapt<- round(Score_Failure_To_Adapt, digits = 2)
      Score_Depressive_Mood_1<- sum(Score_1a[c(1,5,18)], na.rm = TRUE)
      Score_Depressive_Mood_2<- sum(Score_2a[c(1,5,18)], na.rm = TRUE)
      Score_Depressive_Mood<- c(Score_Depressive_Mood_1, Score_Depressive_Mood_2)
      Score_Depressive_Mood<- round(Score_Depressive_Mood, digits = 2)
      Score_Anxiety_1<- sum(Score_1a[c(6,16)], na.rm = TRUE)
      Score_Anxiety_2<- sum(Score_2a[c(6,16)], na.rm = TRUE)
      Score_Anxiety<- c(Score_Anxiety_1, Score_Anxiety_2)
      Score_Anxiety<- round(Score_Anxiety, digits = 2)
      Score_Impulse_Control_1<- sum(Score_1a[c(8,9,12)], na.rm = TRUE)
      Score_Impulse_Control_2<- sum(Score_2a[c(8,9,12)], na.rm = TRUE)
      Score_Impulse_Control<- c(Score_Impulse_Control_1, Score_Impulse_Control_2)
      Score_Impulse_Control<- round(Score_Impulse_Control, digits = 2)
      Score_Avoidance_1<- sum(Score_1a[c(3,7,11,14)], na.rm = TRUE)
      Score_Avoidance_2<- sum(Score_2a[c(3,7,11,14)], na.rm = TRUE)
      Score_Avoidance<- c(Score_Avoidance_1, Score_Avoidance_2)
      Score_Avoidance<- round(Score_Avoidance, digits = 2)
      Change<- c(0, (Score_2 - Score_1))
      Change<- round(Change, digits = 2)
      Change_Preoccupations<- c(0, (Score_Preoccupations_2 - Score_Preoccupations_1))
      Change_Preoccupations<- round(Change_Preoccupations, digits = 2)
      Change_Failure_To_Adapt<- c(0, (Score_Failure_To_Adapt_2 - Score_Failure_To_Adapt_1))
      Change_Failure_To_Adapt<- round(Change_Failure_To_Adapt, digits = 2)
      Change_Depressive_Mood<- c(0, (Score_Depressive_Mood_2 - Score_Depressive_Mood_1))
      Change_Depressive_Mood<- round(Change_Depressive_Mood, digits = 2)
      Change_Anxiety<- c(0, (Score_Anxiety_2 - Score_Anxiety_1))
      Change_Anxiety<- round(Change_Anxiety, digits = 2)
      Change_Impulse_Control<- c(0, (Score_Impulse_Control_2 - Score_Impulse_Control_1))
      Change_Impulse_Control<- round(Change_Impulse_Control, digits = 2)
      Change_Avoidance<- c(0, (Score_Avoidance_2 - Score_Avoidance_1))
      Change_Avoidance<- round(Change_Avoidance, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2)
        PTS_Preoccupations_1<- (Rel_Preoccupations * Score_Preoccupations_1) + (M_Preoccupations * (1 - Rel_Preoccupations))
        PTS_Preoccupations_2<- (Rel_Preoccupations * Score_Preoccupations_2) + (M_Preoccupations * (1 - Rel_Preoccupations))
        PTS_Preoccupations<- c(PTS_Preoccupations_1, PTS_Preoccupations_2)
        PTS_Failure_To_Adapt_1<- (Rel_Failure_To_Adapt * Score_Failure_To_Adapt_1) + (M_Failure_To_Adapt * (1 - Rel_Failure_To_Adapt))
        PTS_Failure_To_Adapt_2<- (Rel_Failure_To_Adapt * Score_Failure_To_Adapt_2) + (M_Failure_To_Adapt * (1 - Rel_Failure_To_Adapt))
        PTS_Failure_To_Adapt<- c(PTS_Failure_To_Adapt_1, PTS_Failure_To_Adapt_2)
        PTS_Depressive_Mood_1<- (Rel_Depressive_Mood * Score_Depressive_Mood_1) + (M_Depressive_Mood * (1 - Rel_Depressive_Mood))
        PTS_Depressive_Mood_2<- (Rel_Depressive_Mood * Score_Depressive_Mood_2) + (M_Depressive_Mood * (1 - Rel_Depressive_Mood))
        PTS_Depressive_Mood<- c(PTS_Depressive_Mood_1, PTS_Depressive_Mood_2)
        PTS_Anxiety_1<- (Rel_Anxiety * Score_Anxiety_1) + (M_Anxiety * (1 - Rel_Anxiety))
        PTS_Anxiety_2<- (Rel_Anxiety * Score_Anxiety_2) + (M_Anxiety * (1 - Rel_Anxiety))
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2)
        PTS_Impulse_Control_1<- (Rel_Impulse_Control * Score_Impulse_Control_1) + (M_Impulse_Control * (1 - Rel_Impulse_Control))
        PTS_Impulse_Control_2<- (Rel_Impulse_Control * Score_Impulse_Control_2) + (M_Impulse_Control * (1 - Rel_Impulse_Control))
        PTS_Impulse_Control<- c(PTS_Impulse_Control_1, PTS_Impulse_Control_2)
        PTS_Avoidance_1<- (Rel_Avoidance * Score_Avoidance_1) + (M_Avoidance * (1 - Rel_Avoidance))
        PTS_Avoidance_2<- (Rel_Avoidance * Score_Avoidance_2) + (M_Avoidance * (1 - Rel_Avoidance))
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2)
        PTS_Preoccupations_1<- Score_Preoccupations_1 + (M_Retest_Preoccupations - M_Preoccupations)  
        PTS_Preoccupations_2<- Score_Preoccupations_2 + (M_Retest_Preoccupations - M_Preoccupations) 
        PTS_Preoccupations<- c(PTS_Preoccupations_1, PTS_Preoccupations_2)
        PTS_Failure_To_Adapt_1<- Score_Failure_To_Adapt_1 + (M_Retest_Failure_To_Adapt - M_Failure_To_Adapt)  
        PTS_Failure_To_Adapt_2<- Score_Failure_To_Adapt_2 + (M_Retest_Failure_To_Adapt - M_Failure_To_Adapt) 
        PTS_Failure_To_Adapt<- c(PTS_Failure_To_Adapt_1, PTS_Failure_To_Adapt_2)
        PTS_Depressive_Mood_1<- Score_Depressive_Mood_1 + (M_Retest_Depressive_Mood - M_Depressive_Mood)  
        PTS_Depressive_Mood_2<- Score_Depressive_Mood_2 + (M_Retest_Depressive_Mood - M_Depressive_Mood) 
        PTS_Depressive_Mood<- c(PTS_Depressive_Mood_1, PTS_Depressive_Mood_2)
        PTS_Anxiety_1<- Score_Anxiety_1 + (M_Retest_Anxiety - M_Anxiety)  
        PTS_Anxiety_2<- Score_Anxiety_2 + (M_Retest_Anxiety - M_Anxiety) 
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2)
        PTS_Impulse_Control_1<- Score_Impulse_Control_1 + (M_Retest_Impulse_Control - M_Impulse_Control)  
        PTS_Impulse_Control_2<- Score_Impulse_Control_2 + (M_Retest_Impulse_Control - M_Impulse_Control) 
        PTS_Impulse_Control<- c(PTS_Impulse_Control_1, PTS_Impulse_Control_2)
        PTS_Avoidance_1<- Score_Avoidance_1 + (M_Retest_Avoidance - M_Avoidance)  
        PTS_Avoidance_2<- Score_Avoidance_2 + (M_Retest_Avoidance - M_Avoidance) 
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Preoccupations<- Score_Preoccupations
        PTS_Failure_To_Adapt<- Score_Failure_To_Adapt
        PTS_Depressive_Mood<- Score_Depressive_Mood
        PTS_Anxiety<- Score_Anxiety
        PTS_Impulse_Control<- Score_Impulse_Control
        PTS_Avoidance<- Score_Avoidance
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Preoccupations<- Rel_Preoccupations * (SD_Retest_Preoccupations/SD_Preoccupations)
        A_Constant_Preoccupations<- M_Retest_Preoccupations - (B_Slope_Preoccupations * M_Preoccupations)
        B_Adj_Preoccupations<- SD_Retest_Preoccupations/SD_Preoccupations
        A_Adj_Preoccupations<- M_Retest_Preoccupations - (B_Adj_Preoccupations * M_Preoccupations)
        PTS_Preoccupations_1<- (B_Adj_Preoccupations * Score_Preoccupations_1) + A_Adj_Preoccupations
        PTS_Preoccupations_2<- (B_Adj_Preoccupations * Score_Preoccupations_2) + A_Adj_Preoccupations
        PTS_Preoccupations<- c(PTS_Preoccupations_1,PTS_Preoccupations_2)
        B_Slope_Failure_To_Adapt<- Rel_Failure_To_Adapt * (SD_Retest_Failure_To_Adapt/SD_Failure_To_Adapt)
        A_Constant_Failure_To_Adapt<- M_Retest_Failure_To_Adapt - (B_Slope_Failure_To_Adapt * M_Failure_To_Adapt)
        B_Adj_Failure_To_Adapt<- SD_Retest_Failure_To_Adapt/SD_Failure_To_Adapt
        A_Adj_Failure_To_Adapt<- M_Retest_Failure_To_Adapt - (B_Adj_Failure_To_Adapt * M_Failure_To_Adapt)
        PTS_Failure_To_Adapt_1<- (B_Adj_Failure_To_Adapt * Score_Failure_To_Adapt_1) + A_Adj_Failure_To_Adapt
        PTS_Failure_To_Adapt_2<- (B_Adj_Failure_To_Adapt * Score_Failure_To_Adapt_2) + A_Adj_Failure_To_Adapt
        PTS_Failure_To_Adapt<- c(PTS_Failure_To_Adapt_1,PTS_Failure_To_Adapt_2)
        B_Slope_Depressive_Mood<- Rel_Depressive_Mood * (SD_Retest_Depressive_Mood/SD_Depressive_Mood)
        A_Constant_Depressive_Mood<- M_Retest_Depressive_Mood - (B_Slope_Depressive_Mood * M_Depressive_Mood)
        B_Adj_Depressive_Mood<- SD_Retest_Depressive_Mood/SD_Depressive_Mood
        A_Adj_Depressive_Mood<- M_Retest_Depressive_Mood - (B_Adj_Depressive_Mood * M_Depressive_Mood)
        PTS_Depressive_Mood_1<- (B_Adj_Depressive_Mood * Score_Depressive_Mood_1) + A_Adj_Depressive_Mood
        PTS_Depressive_Mood_2<- (B_Adj_Depressive_Mood * Score_Depressive_Mood_2) + A_Adj_Depressive_Mood
        PTS_Depressive_Mood<- c(PTS_Depressive_Mood_1,PTS_Depressive_Mood_2)
        B_Slope_Anxiety<- Rel_Anxiety * (SD_Retest_Anxiety/SD_Anxiety)
        A_Constant_Anxiety<- M_Retest_Anxiety - (B_Slope_Anxiety * M_Anxiety)
        B_Adj_Anxiety<- SD_Retest_Anxiety/SD_Anxiety
        A_Adj_Anxiety<- M_Retest_Anxiety - (B_Adj_Anxiety * M_Anxiety)
        PTS_Anxiety_1<- (B_Adj_Anxiety * Score_Anxiety_1) + A_Adj_Anxiety
        PTS_Anxiety_2<- (B_Adj_Anxiety * Score_Anxiety_2) + A_Adj_Anxiety
        PTS_Anxiety<- c(PTS_Anxiety_1,PTS_Anxiety_2)
        B_Slope_Impulse_Control<- Rel_Impulse_Control * (SD_Retest_Impulse_Control/SD_Impulse_Control)
        A_Constant_Impulse_Control<- M_Retest_Impulse_Control - (B_Slope_Impulse_Control * M_Impulse_Control)
        B_Adj_Impulse_Control<- SD_Retest_Impulse_Control/SD_Impulse_Control
        A_Adj_Impulse_Control<- M_Retest_Impulse_Control - (B_Adj_Impulse_Control * M_Impulse_Control)
        PTS_Impulse_Control_1<- (B_Adj_Impulse_Control * Score_Impulse_Control_1) + A_Adj_Impulse_Control
        PTS_Impulse_Control_2<- (B_Adj_Impulse_Control * Score_Impulse_Control_2) + A_Adj_Impulse_Control
        PTS_Impulse_Control<- c(PTS_Impulse_Control_1, PTS_Impulse_Control_2)
        B_Slope_Avoidance<- Rel_Avoidance * (SD_Retest_Avoidance/SD_Avoidance)
        A_Constant_Avoidance<- M_Retest_Avoidance - (B_Slope_Avoidance * M_Avoidance)
        B_Adj_Avoidance<- SD_Retest_Avoidance/SD_Avoidance
        A_Adj_Avoidance<- M_Retest_Avoidance - (B_Adj_Avoidance * M_Avoidance)
        PTS_Avoidance_1<- (B_Adj_Avoidance * Score_Avoidance_1) + A_Adj_Avoidance
        PTS_Avoidance_2<- (B_Adj_Avoidance * Score_Avoidance_2) + A_Adj_Avoidance
        PTS_Avoidance<- c(PTS_Avoidance_1,PTS_Avoidance_2)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Preoccupations<- Rel_Preoccupations * (SD_Retest_Preoccupations/SD_Preoccupations)
        PTS_Preoccupations_1<- B_Slope_Preoccupations * Score_Preoccupations_1
        PTS_Preoccupations_2<- B_Slope_Preoccupations * Score_Preoccupations_2
        PTS_Preoccupations<- c(PTS_Preoccupations_1, PTS_Preoccupations_2)
        B_Slope_Failure_To_Adapt<- Rel_Failure_To_Adapt * (SD_Retest_Failure_To_Adapt/SD_Failure_To_Adapt)
        PTS_Failure_To_Adapt_1<- B_Slope_Failure_To_Adapt * Score_Failure_To_Adapt_1
        PTS_Failure_To_Adapt_2<- B_Slope_Failure_To_Adapt * Score_Failure_To_Adapt_2
        PTS_Failure_To_Adapt<- c(PTS_Failure_To_Adapt_1, PTS_Failure_To_Adapt_2)
        B_Slope_Depressive_Mood<- Rel_Depressive_Mood * (SD_Retest_Depressive_Mood/SD_Depressive_Mood)
        PTS_Depressive_Mood_1<- B_Slope_Depressive_Mood * Score_Depressive_Mood_1
        PTS_Depressive_Mood_2<- B_Slope_Depressive_Mood * Score_Depressive_Mood_2
        PTS_Depressive_Mood<- c(PTS_Depressive_Mood_1, PTS_Depressive_Mood_2)
        B_Slope_Anxiety<- Rel_Anxiety * (SD_Retest_Anxiety/SD_Anxiety)
        PTS_Anxiety_1<- B_Slope_Anxiety * Score_Anxiety_1
        PTS_Anxiety_2<- B_Slope_Anxiety * Score_Anxiety_2
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2)
        B_Slope_Impulse_Control<- Rel_Impulse_Control * (SD_Retest_Impulse_Control/SD_Impulse_Control)
        PTS_Impulse_Control_1<- B_Slope_Impulse_Control * Score_Impulse_Control_1
        PTS_Impulse_Control_2<- B_Slope_Impulse_Control * Score_Impulse_Control_2
        PTS_Impulse_Control<- c(PTS_Impulse_Control_1, PTS_Impulse_Control_2)
        B_Slope_Avoidance<- Rel_Avoidance * (SD_Retest_Avoidance/SD_Avoidance)
        PTS_Avoidance_1<- B_Slope_Avoidance * Score_Avoidance_1
        PTS_Avoidance_2<- B_Slope_Avoidance * Score_Avoidance_2
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2) 
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2)
        PTS_Preoccupations_1<- Score_Preoccupations_1 + (M_Retest_Preoccupations - M_Preoccupations)
        PTS_Preoccupations_2<- Score_Preoccupations_2 + (M_Retest_Preoccupations - M_Preoccupations)
        PTS_Preoccupations<- c(PTS_Preoccupations_1, PTS_Preoccupations_2)
        PTS_Failure_To_Adapt_1<- Score_Failure_To_Adapt_1 + (M_Retest_Failure_To_Adapt - M_Failure_To_Adapt)
        PTS_Failure_To_Adapt_2<- Score_Failure_To_Adapt_2 + (M_Retest_Failure_To_Adapt - M_Failure_To_Adapt)
        PTS_Failure_To_Adapt<- c(PTS_Failure_To_Adapt_1, PTS_Failure_To_Adapt_2)
        PTS_Depressive_Mood_1<- Score_Depressive_Mood_1 + (M_Retest_Depressive_Mood - M_Depressive_Mood)
        PTS_Depressive_Mood_2<- Score_Depressive_Mood_2 + (M_Retest_Depressive_Mood - M_Depressive_Mood)
        PTS_Depressive_Mood<- c(PTS_Depressive_Mood_1, PTS_Depressive_Mood_2)
        PTS_Anxiety_1<- Score_Anxiety_1 + (M_Retest_Anxiety - M_Anxiety)
        PTS_Anxiety_2<- Score_Anxiety_2 + (M_Retest_Anxiety - M_Anxiety)
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2)
        PTS_Impulse_Control_1<- Score_Impulse_Control_1 + (M_Retest_Impulse_Control - M_Impulse_Control)
        PTS_Impulse_Control_2<- Score_Impulse_Control_2 + (M_Retest_Impulse_Control - M_Impulse_Control)
        PTS_Impulse_Control<- c(PTS_Impulse_Control_1, PTS_Impulse_Control_2)
        PTS_Avoidance_1<- Score_Avoidance_1 + (M_Retest_Avoidance - M_Avoidance)
        PTS_Avoidance_2<- Score_Avoidance_2 + (M_Retest_Avoidance - M_Avoidance)
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Preoccupations<- round(PTS_Preoccupations, digits = 2)
      PTS_Failure_To_Adapt<- round(PTS_Failure_To_Adapt, digits = 2)
      PTS_Depressive_Mood<- round(PTS_Depressive_Mood, digits = 2)
      PTS_Anxiety<- round(PTS_Anxiety, digits = 2)
      PTS_Impulse_Control<- round(PTS_Impulse_Control, digits = 2)
      PTS_Avoidance<- round(PTS_Avoidance, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2)
        SE_Preoccupations_1<- McSweeny_SE_Preoccupations*sqrt(1 + (1/SampleN) + ((Score_Preoccupations_1 - M_Preoccupations)^2/(SD_Preoccupations^2*(SampleN-1))))
        SE_Preoccupations_2<- McSweeny_SE_Preoccupations*sqrt(1 + (1/SampleN) + ((Score_Preoccupations_2 - M_Preoccupations)^2/(SD_Preoccupations^2*(SampleN-1))))
        SE_Preoccupations<-c(SE_Preoccupations_1, SE_Preoccupations_2)
        SE_Failure_To_Adapt_1<- McSweeny_SE_Failure_To_Adapt*sqrt(1 + (1/SampleN) + ((Score_Failure_To_Adapt_1 - M_Failure_To_Adapt)^2/(SD_Failure_To_Adapt^2*(SampleN-1))))
        SE_Failure_To_Adapt_2<- McSweeny_SE_Failure_To_Adapt*sqrt(1 + (1/SampleN) + ((Score_Failure_To_Adapt_2 - M_Failure_To_Adapt)^2/(SD_Failure_To_Adapt^2*(SampleN-1))))
        SE_Failure_To_Adapt<-c(SE_Failure_To_Adapt_1, SE_Failure_To_Adapt_2)
        SE_Depressive_Mood_1<- McSweeny_SE_Depressive_Mood*sqrt(1 + (1/SampleN) + ((Score_Depressive_Mood_1 - M_Depressive_Mood)^2/(SD_Depressive_Mood^2*(SampleN-1))))
        SE_Depressive_Mood_2<- McSweeny_SE_Depressive_Mood*sqrt(1 + (1/SampleN) + ((Score_Depressive_Mood_2 - M_Depressive_Mood)^2/(SD_Depressive_Mood^2*(SampleN-1))))
        SE_Depressive_Mood<-c(SE_Depressive_Mood_1, SE_Depressive_Mood_2)
        SE_Anxiety_1<- McSweeny_SE_Anxiety*sqrt(1 + (1/SampleN) + ((Score_Anxiety_1 - M_Anxiety)^2/(SD_Anxiety^2*(SampleN-1))))
        SE_Anxiety_2<- McSweeny_SE_Anxiety*sqrt(1 + (1/SampleN) + ((Score_Anxiety_2 - M_Anxiety)^2/(SD_Anxiety^2*(SampleN-1))))
        SE_Anxiety<-c(SE_Anxiety_1, SE_Anxiety_2)
        SE_Impulse_Control_1<- McSweeny_SE_Impulse_Control*sqrt(1 + (1/SampleN) + ((Score_Impulse_Control_1 - M_Impulse_Control)^2/(SD_Impulse_Control^2*(SampleN-1))))
        SE_Impulse_Control_2<- McSweeny_SE_Impulse_Control*sqrt(1 + (1/SampleN) + ((Score_Impulse_Control_2 - M_Impulse_Control)^2/(SD_Impulse_Control^2*(SampleN-1))))
        SE_Impulse_Control<-c(SE_Impulse_Control_1, SE_Impulse_Control_2)
        SE_Avoidance_1<- McSweeny_SE_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Avoidance_1 - M_Avoidance)^2/(SD_Avoidance^2*(SampleN-1))))
        SE_Avoidance_2<- McSweeny_SE_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Avoidance_2 - M_Avoidance)^2/(SD_Avoidance^2*(SampleN-1))))
        SE_Avoidance<-c(SE_Avoidance_1, SE_Avoidance_2)
        SE<- round(SE, digits = 2)
        SE_Preoccupations<- round(SE_Preoccupations, digits = 2)
        SE_Failure_To_Adapt<- round(SE_Failure_To_Adapt, digits = 2)
        SE_Depressive_Mood<- round(SE_Depressive_Mood, digits = 2)
        SE_Anxiety<- round(SE_Anxiety, digits = 2)
        SE_Impulse_Control<- round(SE_Impulse_Control, digits = 2)
        SE_Avoidance<- round(SE_Avoidance, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2))
        CI<- round(CI, digits = 2)
        CI_Preoccupations<- c((Conf*SE_Preoccupations_1), (Conf*SE_Preoccupations_2))
        CI_Preoccupations<- round(CI_Preoccupations, digits = 2)
        CI_Failure_To_Adapt<- c((Conf*SE_Failure_To_Adapt_1), (Conf*SE_Failure_To_Adapt_2))
        CI_Failure_To_Adapt<- round(CI_Failure_To_Adapt, digits = 2)
        CI_Depressive_Mood<- c((Conf*SE_Depressive_Mood_1), (Conf*SE_Depressive_Mood_2))
        CI_Depressive_Mood<- round(CI_Depressive_Mood, digits = 2)
        CI_Anxiety<- c((Conf*SE_Anxiety_1), (Conf*SE_Anxiety_2))
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Impulse_Control<- c((Conf*SE_Impulse_Control_1), (Conf*SE_Impulse_Control_2))
        CI_Impulse_Control<- round(CI_Impulse_Control, digits = 2)
        CI_Avoidance<- c((Conf*SE_Avoidance_1), (Conf*SE_Avoidance_2))
        CI_Avoidance<- round(CI_Avoidance, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Preoccupations<- c((Conf*SE_Preoccupations), (Conf*SE_Preoccupations))
        CI_Preoccupations<- round(CI_Preoccupations, digits = 2)
        CI_Failure_To_Adapt<- c((Conf*SE_Failure_To_Adapt), (Conf*SE_Failure_To_Adapt))
        CI_Failure_To_Adapt<- round(CI_Failure_To_Adapt, digits = 2)
        CI_Depressive_Mood<- c((Conf*SE_Depressive_Mood), (Conf*SE_Depressive_Mood))
        CI_Depressive_Mood<- round(CI_Depressive_Mood, digits = 2)
        CI_Anxiety<- c((Conf*SE_Anxiety), (Conf*SE_Anxiety))
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Impulse_Control<- c((Conf*SE_Impulse_Control), (Conf*SE_Impulse_Control))
        CI_Impulse_Control<- round(CI_Impulse_Control, digits = 2)
        CI_Avoidance<- c((Conf*SE_Avoidance), (Conf*SE_Avoidance))
        CI_Avoidance<- round(CI_Avoidance, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Preoccupations<- PTS_Preoccupations + CI_Preoccupations
      CI_Upper_Lim_Preoccupations<- round(CI_Upper_Lim_Preoccupations, digits = 2)
      CI_Lower_Lim_Preoccupations<-PTS_Preoccupations - CI_Preoccupations
      CI_Lower_Lim_Preoccupations<- round(CI_Lower_Lim_Preoccupations, digits = 2)
      CI_Upper_Lim_Failure_To_Adapt<- PTS_Failure_To_Adapt + CI_Failure_To_Adapt
      CI_Upper_Lim_Failure_To_Adapt<- round(CI_Upper_Lim_Failure_To_Adapt, digits = 2)
      CI_Lower_Lim_Failure_To_Adapt<-PTS_Failure_To_Adapt - CI_Failure_To_Adapt
      CI_Lower_Lim_Failure_To_Adapt<- round(CI_Lower_Lim_Failure_To_Adapt, digits = 2)
      CI_Upper_Lim_Depressive_Mood<- PTS_Depressive_Mood + CI_Depressive_Mood
      CI_Upper_Lim_Depressive_Mood<- round(CI_Upper_Lim_Depressive_Mood, digits = 2)
      CI_Lower_Lim_Depressive_Mood<-PTS_Depressive_Mood - CI_Depressive_Mood
      CI_Lower_Lim_Depressive_Mood<- round(CI_Lower_Lim_Depressive_Mood, digits = 2)
      CI_Upper_Lim_Anxiety<- PTS_Anxiety + CI_Anxiety
      CI_Upper_Lim_Anxiety<- round(CI_Upper_Lim_Anxiety, digits = 2)
      CI_Lower_Lim_Anxiety<-PTS_Anxiety - CI_Anxiety
      CI_Lower_Lim_Anxiety<- round(CI_Lower_Lim_Anxiety, digits = 2)
      CI_Upper_Lim_Impulse_Control<- PTS_Impulse_Control + CI_Impulse_Control
      CI_Upper_Lim_Impulse_Control<- round(CI_Upper_Lim_Impulse_Control, digits = 2)
      CI_Lower_Lim_Impulse_Control<- PTS_Impulse_Control - CI_Impulse_Control
      CI_Lower_Lim_Impulse_Control<- round(CI_Lower_Lim_Impulse_Control, digits = 2)
      CI_Upper_Lim_Avoidance<- PTS_Avoidance + CI_Avoidance
      CI_Upper_Lim_Avoidance<- round(CI_Upper_Lim_Avoidance, digits = 2)
      CI_Lower_Lim_Avoidance<-PTS_Avoidance - CI_Avoidance
      CI_Lower_Lim_Avoidance<- round(CI_Lower_Lim_Avoidance, digits = 2)
  
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Preoccupations == "2") {
        CI_Preoccupations<- input$Man_CI_Preoccupations
        CI_Preoccupations<- c(CI_Preoccupations, CI_Preoccupations)
        CI_Preoccupations<- round(CI_Preoccupations, digits = 2)
        CI_Upper_Lim_Preoccupations<- Score_Preoccupations + CI_Preoccupations
        CI_Upper_Lim_Preoccupations<- round(CI_Upper_Lim_Preoccupations, digits = 2)
        CI_Lower_Lim_Preoccupations<- Score_Preoccupations - CI_Preoccupations
        CI_Lower_Lim_Preoccupations<- round(CI_Lower_Lim_Preoccupations, digits = 2)
      }
      if(input$Select_CI_Failure_To_Adapt == "2") {
        CI_Failure_To_Adapt<- input$Man_CI_Failure_To_Adapt
        CI_Failure_To_Adapt<- c(CI_Failure_To_Adapt, CI_Failure_To_Adapt)
        CI_Failure_To_Adapt<- round(CI_Failure_To_Adapt, digits = 2)
        CI_Upper_Lim_Failure_To_Adapt<- Score_Failure_To_Adapt + CI_Failure_To_Adapt
        CI_Upper_Lim_Failure_To_Adapt<- round(CI_Upper_Lim_Failure_To_Adapt, digits = 2)
        CI_Lower_Lim_Failure_To_Adapt<- Score_Failure_To_Adapt - CI_Failure_To_Adapt
        CI_Lower_Lim_Failure_To_Adapt<- round(CI_Lower_Lim_Failure_To_Adapt, digits = 2)
      }
      if(input$Select_CI_Depressive_Mood == "2") {
        CI_Depressive_Mood<- input$Man_CI_Depressive_Mood
        CI_Depressive_Mood<- c(CI_Depressive_Mood, CI_Depressive_Mood)
        CI_Depressive_Mood<- round(CI_Depressive_Mood, digits = 2)
        CI_Upper_Lim_Depressive_Mood<- Score_Depressive_Mood + CI_Depressive_Mood
        CI_Upper_Lim_Depressive_Mood<- round(CI_Upper_Lim_Depressive_Mood, digits = 2)
        CI_Lower_Lim_Depressive_Mood<- Score_Depressive_Mood - CI_Depressive_Mood
        CI_Lower_Lim_Depressive_Mood<- round(CI_Lower_Lim_Depressive_Mood, digits = 2)
      }
      if(input$Select_CI_Anxiety == "2") {
        CI_Anxiety<- input$Man_CI_Anxiety
        CI_Anxiety<- c(CI_Anxiety,  CI_Anxiety)
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Upper_Lim_Anxiety<- Score_Anxiety + CI_Anxiety
        CI_Upper_Lim_Anxiety<- round(CI_Upper_Lim_Anxiety, digits = 2)
        CI_Lower_Lim_Anxiety<- Score_Anxiety - CI_Anxiety
        CI_Lower_Lim_Anxiety<- round(CI_Lower_Lim_Anxiety, digits = 2)
      }
      if(input$Select_CI_Impulse_Control == "2") {
        CI_Impulse_Control<- input$Man_CI_Impulse_Control
        CI_Impulse_Control<- c(CI_Impulse_Control, CI_Impulse_Control)
        CI_Impulse_Control<- round(CI_Impulse_Control, digits = 2)
        CI_Upper_Lim_Impulse_Control<- Score_Impulse_Control + CI_Impulse_Control
        CI_Upper_Lim_Impulse_Control<- round(CI_Upper_Lim_Impulse_Control, digits = 2)
        CI_Lower_Lim_Impulse_Control<- Score_Impulse_Control - CI_Impulse_Control
        CI_Lower_Lim_Impulse_Control<- round(CI_Lower_Lim_Impulse_Control, digits = 2)
      }
      if(input$Select_CI_Avoidance == "2") {
        CI_Avoidance<- input$Man_CI_Avoidance
        CI_Avoidance<- c(CI_Avoidance, CI_Avoidance)
        CI_Avoidance<- round(CI_Avoidance, digits = 2)
        CI_Upper_Lim_Avoidance<- Score_Avoidance + CI_Avoidance
        CI_Upper_Lim_Avoidance<- round(CI_Upper_Lim_Avoidance, digits = 2)
        CI_Lower_Lim_Avoidance<- Score_Avoidance - CI_Avoidance
        CI_Lower_Lim_Avoidance<- round(CI_Lower_Lim_Avoidance, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_4<- round(input$Cutoff_4, digits = 2)
      Cutoff_Score_Preoccupations_1<- round(input$Cutoff_Preoccupations_1, digits = 2)
      Cutoff_Score_Preoccupations_2<- round(input$Cutoff_Preoccupations_2, digits = 2)
      Cutoff_Score_Preoccupations_3<- round(input$Cutoff_Preoccupations_3, digits = 2)
      Cutoff_Score_Failure_To_Adapt_1<- round(input$Cutoff_Failure_To_Adapt_1, digits = 2)
      Cutoff_Score_Failure_To_Adapt_2<- round(input$Cutoff_Failure_To_Adapt_2, digits = 2)
      Cutoff_Score_Failure_To_Adapt_3<- round(input$Cutoff_Failure_To_Adapt_3, digits = 2)
      Cutoff_Score_Depressive_Mood_1<- round(input$Cutoff_Depressive_Mood_1, digits = 2)
      Cutoff_Score_Depressive_Mood_2<- round(input$Cutoff_Depressive_Mood_2, digits = 2)
      Cutoff_Score_Depressive_Mood_3<- round(input$Cutoff_Depressive_Mood_3, digits = 2)
      Cutoff_Score_Anxiety_1<- round(input$Cutoff_Anxiety_1, digits = 2)
      Cutoff_Score_Anxiety_2<- round(input$Cutoff_Anxiety_2, digits = 2)
      Cutoff_Score_Anxiety_3<- round(input$Cutoff_Anxiety_3, digits = 2)
      Cutoff_Score_Impulse_Control_1<- round(input$Cutoff_Impulse_Control_1, digits = 2)
      Cutoff_Score_Impulse_Control_2<- round(input$Cutoff_Impulse_Control_2, digits = 2)
      Cutoff_Score_Impulse_Control_3<- round(input$Cutoff_Impulse_Control_3, digits = 2)
      Cutoff_Score_Avoidance_1<- round(input$Cutoff_Avoidance_1, digits = 2)
      Cutoff_Score_Avoidance_2<- round(input$Cutoff_Avoidance_2, digits = 2)
      Cutoff_Score_Avoidance_3<- round(input$Cutoff_Avoidance_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Cutoff_Score_4,Score_Preoccupations,Change_Preoccupations,PTS_Preoccupations, SE_Preoccupations, CI_Upper_Lim_Preoccupations, CI_Lower_Lim_Preoccupations, Cutoff_Score_Preoccupations_1,Cutoff_Score_Preoccupations_2,Cutoff_Score_Preoccupations_3,
                                      Score_Failure_To_Adapt,Change_Failure_To_Adapt, PTS_Failure_To_Adapt, SE_Failure_To_Adapt, CI_Upper_Lim_Failure_To_Adapt, CI_Lower_Lim_Failure_To_Adapt, Cutoff_Score_Failure_To_Adapt_1,Cutoff_Score_Failure_To_Adapt_2,Cutoff_Score_Failure_To_Adapt_3, 
                                      Score_Depressive_Mood,Change_Depressive_Mood,PTS_Depressive_Mood, SE_Depressive_Mood, CI_Upper_Lim_Depressive_Mood, CI_Lower_Lim_Depressive_Mood, Cutoff_Score_Depressive_Mood_1,Cutoff_Score_Depressive_Mood_2,Cutoff_Score_Depressive_Mood_3, 
                                      Score_Anxiety,Change_Anxiety,PTS_Anxiety, SE_Anxiety, CI_Upper_Lim_Anxiety, CI_Lower_Lim_Anxiety, Cutoff_Score_Anxiety_1,Cutoff_Score_Anxiety_2,Cutoff_Score_Anxiety_3,
                                      Score_Impulse_Control,Change_Impulse_Control, PTS_Impulse_Control, SE_Impulse_Control, CI_Upper_Lim_Impulse_Control, CI_Lower_Lim_Impulse_Control, Cutoff_Score_Impulse_Control_1,Cutoff_Score_Impulse_Control_2,Cutoff_Score_Impulse_Control_3, 
                                      Score_Avoidance,Change_Avoidance,PTS_Avoidance, SE_Avoidance, CI_Upper_Lim_Avoidance, CI_Lower_Lim_Avoidance, Cutoff_Score_Avoidance_1,Cutoff_Score_Avoidance_2,Cutoff_Score_Avoidance_3 
                                      )
    } else if(input$Timepoint == "3") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date_3<- input$Date_3
      Date<- c(Date_1, Date_2, Date_3)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Score_1<- sum(Score_1a, na.rm = TRUE)
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Score_2<- sum(Score_2a, na.rm = TRUE)
      Score_3a<<- as.numeric(unlist(strsplit(input$Text_3,",")))
      Duration_Score<- as.character(unlist(strsplit(input$Duration_3,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_3a), Frequency = Score_3a, Duration = Duration_Score)
      Event<- c("Divorce/separation", "Family conflicts", "Conflicts in Worklife", "Conflicts with neighbours",
                "Illness of a loved one", "Death of a loved one", "Adjustment due to retirement", "Unemployment",
                "Too much/too little work", "Pressure to meet deadlines/time pressure", "Moving to a new home",
                "Financial problems", "Own serious illness", "Serious accident", "Assault", "Termination of an important leisure activity",
                input$Other_Event_1, input$Other_Event_2)
      Event_Dates<- c(input$Event_Date_1, paste(input$Event_Date_2,input$Event_Date_2a), paste(input$Event_Date_3, "to", input$Event_Date_23a),
                      paste(input$Event_Date_4, "to", input$Event_Date_4a), paste(input$Event_Date_5, "to", input$Event_Date_5a), input$Event_Date_6,
                      input$Event_Date_7, paste(input$Event_Date_8, "to", input$Event_Date_8a), paste(input$Event_Date_9, "to", input$Event_Date_9a),
                      paste(input$Event_Date_10, "to", input$Event_Date_10a), input$Event_Date_11, paste(input$Event_Date_12, "to", input$Event_Date_12a),
                      paste(input$Event_Date_13, "to", input$Event_Date_13a), input$Event_Date_14, input$Event_Date_15, paste(input$Event_Date_16, "to", input$Event_Date_16a),
                      paste(input$Event_Date_17, "to", input$Event_Date_17a), paste(input$Event_Date_18, "to", input$Event_Date_18a))
      Event_Endorsement<- c(input$Yes_1, input$Yes_2, input$Yes_3, input$Yes_4, input$Yes_5, input$Yes_6, input$Yes_7, input$Yes_8, input$Yes_9, input$Yes_10, input$Yes_11, 
                            input$Yes_12, input$Yes_13, input$Yes_14, input$Yes_15, input$Yes_16, input$Yes_11, input$Yes_18)
      Event_Df<<- data.frame(Event, Endorsement = Event_Endorsement, Date = Event_Dates)
      Straining_Event<<- input$Straining_Event
      Score_3<- sum(Score_3a, na.rm = TRUE)
      Score<- c(Score_1, Score_2, Score_3)
      Score<- round(Score, digits = 2)
      Score_Preoccupations_1<- sum(Score_1a[c(2,4,13,15)], na.rm = TRUE)
      Score_Preoccupations_2<- sum(Score_2a[c(2,4,13,15)], na.rm = TRUE)
      Score_Preoccupations_3<- sum(Score_3a[c(2,4,13,15)], na.rm = TRUE)
      Score_Preoccupations<- c(Score_Preoccupations_1, Score_Preoccupations_2, Score_Preoccupations_3)
      Score_Preoccupations<- round(Score_Preoccupations, digits = 2)
      Score_Failure_To_Adapt_1<- sum(Score_1a[c(10,17,19,20)], na.rm = TRUE)
      Score_Failure_To_Adapt_2<- sum(Score_2a[c(10,17,19,20)], na.rm = TRUE)
      Score_Failure_To_Adapt_3<- sum(Score_3a[c(10,17,19,20)], na.rm = TRUE)
      Score_Failure_To_Adapt<- c(Score_Failure_To_Adapt_1,Score_Failure_To_Adapt_2, Score_Failure_To_Adapt_3)
      Score_Failure_To_Adapt<- round(Score_Failure_To_Adapt, digits = 2)
      Score_Depressive_Mood_1<- sum(Score_1a[c(1,5,18)], na.rm = TRUE)
      Score_Depressive_Mood_2<- sum(Score_2a[c(1,5,18)], na.rm = TRUE)
      Score_Depressive_Mood_3<- sum(Score_3a[c(1,5,18)], na.rm = TRUE)
      Score_Depressive_Mood<- c(Score_Depressive_Mood_1, Score_Depressive_Mood_2, Score_Depressive_Mood_3)
      Score_Depressive_Mood<- round(Score_Depressive_Mood, digits = 2)
      Score_Anxiety_1<- sum(Score_1a[c(6,16)], na.rm = TRUE)
      Score_Anxiety_2<- sum(Score_2a[c(6,16)], na.rm = TRUE)
      Score_Anxiety_3<- sum(Score_3a[c(6,16)], na.rm = TRUE)
      Score_Anxiety<- c(Score_Anxiety_1, Score_Anxiety_2, Score_Anxiety_3)
      Score_Anxiety<- round(Score_Anxiety, digits = 2)
      Score_Impulse_Control_1<- sum(Score_1a[c(8,9,12)], na.rm = TRUE)
      Score_Impulse_Control_2<- sum(Score_2a[c(8,9,12)], na.rm = TRUE)
      Score_Impulse_Control_3<- sum(Score_3a[c(8,9,12)], na.rm = TRUE)
      Score_Impulse_Control<- c(Score_Impulse_Control_1, Score_Impulse_Control_2, Score_Impulse_Control_3)
      Score_Impulse_Control<- round(Score_Impulse_Control, digits = 2)
      Score_Avoidance_1<- sum(Score_1a[c(3,7,11,14)], na.rm = TRUE)
      Score_Avoidance_2<- sum(Score_2a[c(3,7,11,14)], na.rm = TRUE)
      Score_Avoidance_3<- sum(Score_3a[c(3,7,11,14)], na.rm = TRUE)
      Score_Avoidance<- c(Score_Avoidance_1, Score_Avoidance_2, Score_Avoidance_3)
      Score_Avoidance<- round(Score_Avoidance, digits = 2)
      Change<- c(0, (Score_2 - Score_1), (Score_3 - Score_2))
      Change<- round(Change, digits = 2)
      Change_Preoccupations<- c(0, (Score_Preoccupations_2 - Score_Preoccupations_1), (Score_Preoccupations_3 - Score_Preoccupations_2))
      Change_Preoccupations<- round(Change_Preoccupations, digits = 2)
      Change_Failure_To_Adapt<- c(0, (Score_Failure_To_Adapt_2 - Score_Failure_To_Adapt_1), (Score_Failure_To_Adapt_3 - Score_Failure_To_Adapt_2))
      Change_Failure_To_Adapt<- round(Change_Failure_To_Adapt, digits = 2)
      Change_Depressive_Mood<- c(0, (Score_Depressive_Mood_2 - Score_Depressive_Mood_1),  (Score_Depressive_Mood_3 - Score_Depressive_Mood_2))
      Change_Depressive_Mood<- round(Change_Depressive_Mood, digits = 2)
      Change_Anxiety<- c(0, (Score_Anxiety_2 - Score_Anxiety_1), (Score_Anxiety_3 - Score_Anxiety_2))
      Change_Anxiety<- round(Change_Anxiety, digits = 2)
      Change_Impulse_Control<- c(0, (Score_Impulse_Control_2 - Score_Impulse_Control_1), (Score_Impulse_Control_3 - Score_Impulse_Control_2))
      Change_Impulse_Control<- round(Change_Impulse_Control, digits = 2)
      Change_Avoidance<- c(0, (Score_Avoidance_2 - Score_Avoidance_1), (Score_Avoidance_3 - Score_Avoidance_2))
      Change_Avoidance<- round(Change_Avoidance, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS_3<- (Rel * Score_3) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Preoccupations_1<- (Rel_Preoccupations * Score_Preoccupations_1) + (M_Preoccupations * (1 - Rel_Preoccupations))
        PTS_Preoccupations_2<- (Rel_Preoccupations * Score_Preoccupations_2) + (M_Preoccupations * (1 - Rel_Preoccupations))
        PTS_Preoccupations_3<- (Rel_Preoccupations * Score_Preoccupations_3) + (M_Preoccupations * (1 - Rel_Preoccupations))
        PTS_Preoccupations<- c(PTS_Preoccupations_1, PTS_Preoccupations_2, PTS_Preoccupations_3)
        PTS_Failure_To_Adapt_1<- (Rel_Failure_To_Adapt * Score_Failure_To_Adapt_1) + (M_Failure_To_Adapt * (1 - Rel_Failure_To_Adapt))
        PTS_Failure_To_Adapt_2<- (Rel_Failure_To_Adapt * Score_Failure_To_Adapt_2) + (M_Failure_To_Adapt * (1 - Rel_Failure_To_Adapt))
        PTS_Failure_To_Adapt_3<- (Rel_Failure_To_Adapt * Score_Failure_To_Adapt_3) + (M_Failure_To_Adapt * (1 - Rel_Failure_To_Adapt))
        PTS_Failure_To_Adapt<- c(PTS_Failure_To_Adapt_1, PTS_Failure_To_Adapt_2, PTS_Failure_To_Adapt_3)
        PTS_Depressive_Mood_1<- (Rel_Depressive_Mood * Score_Depressive_Mood_1) + (M_Depressive_Mood * (1 - Rel_Depressive_Mood))
        PTS_Depressive_Mood_2<- (Rel_Depressive_Mood * Score_Depressive_Mood_2) + (M_Depressive_Mood * (1 - Rel_Depressive_Mood))
        PTS_Depressive_Mood_3<- (Rel_Depressive_Mood * Score_Depressive_Mood_3) + (M_Depressive_Mood * (1 - Rel_Depressive_Mood))
        PTS_Depressive_Mood<- c(PTS_Depressive_Mood_1, PTS_Depressive_Mood_2, PTS_Depressive_Mood_3)
        PTS_Anxiety_1<- (Rel_Anxiety * Score_Anxiety_1) + (M_Anxiety * (1 - Rel_Anxiety))
        PTS_Anxiety_2<- (Rel_Anxiety * Score_Anxiety_2) + (M_Anxiety * (1 - Rel_Anxiety))
        PTS_Anxiety_3<- (Rel_Anxiety * Score_Anxiety_3) + (M_Anxiety * (1 - Rel_Anxiety))
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2, PTS_Anxiety_3)
        PTS_Impulse_Control_1<- (Rel_Impulse_Control * Score_Impulse_Control_1) + (M_Impulse_Control * (1 - Rel_Impulse_Control))
        PTS_Impulse_Control_2<- (Rel_Impulse_Control * Score_Impulse_Control_2) + (M_Impulse_Control * (1 - Rel_Impulse_Control))
        PTS_Impulse_Control_3<- (Rel_Impulse_Control * Score_Impulse_Control_3) + (M_Impulse_Control * (1 - Rel_Impulse_Control))
        PTS_Impulse_Control<- c(PTS_Impulse_Control_1, PTS_Impulse_Control_2, PTS_Impulse_Control_3)
        PTS_Avoidance_1<- (Rel_Avoidance * Score_Avoidance_1) + (M_Avoidance * (1 - Rel_Avoidance))
        PTS_Avoidance_2<- (Rel_Avoidance * Score_Avoidance_2) + (M_Avoidance * (1 - Rel_Avoidance))
        PTS_Avoidance_3<- (Rel_Avoidance * Score_Avoidance_3) + (M_Avoidance * (1 - Rel_Avoidance))
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2, PTS_Avoidance_3)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Preoccupations_1<- Score_Preoccupations_1 + (M_Retest_Preoccupations - M_Preoccupations)  
        PTS_Preoccupations_2<- Score_Preoccupations_2 + (M_Retest_Preoccupations - M_Preoccupations) 
        PTS_Preoccupations_3<- Score_Preoccupations_3 + (M_Retest_Preoccupations - M_Preoccupations) 
        PTS_Preoccupations<- c(PTS_Preoccupations_1, PTS_Preoccupations_2, PTS_Preoccupations_3)
        PTS_Failure_To_Adapt_1<- Score_Failure_To_Adapt_1 + (M_Retest_Failure_To_Adapt - M_Failure_To_Adapt)  
        PTS_Failure_To_Adapt_2<- Score_Failure_To_Adapt_2 + (M_Retest_Failure_To_Adapt - M_Failure_To_Adapt) 
        PTS_Failure_To_Adapt_3<- Score_Failure_To_Adapt_3 + (M_Retest_Failure_To_Adapt - M_Failure_To_Adapt) 
        PTS_Failure_To_Adapt<- c(PTS_Failure_To_Adapt_1, PTS_Failure_To_Adapt_2, PTS_Failure_To_Adapt_3)
        PTS_Depressive_Mood_1<- Score_Depressive_Mood_1 + (M_Retest_Depressive_Mood - M_Depressive_Mood)  
        PTS_Depressive_Mood_2<- Score_Depressive_Mood_2 + (M_Retest_Depressive_Mood - M_Depressive_Mood) 
        PTS_Depressive_Mood_3<- Score_Depressive_Mood_3 + (M_Retest_Depressive_Mood - M_Depressive_Mood) 
        PTS_Depressive_Mood<- c(PTS_Depressive_Mood_1, PTS_Depressive_Mood_2, PTS_Depressive_Mood_3)
        PTS_Anxiety_1<- Score_Anxiety_1 + (M_Retest_Anxiety - M_Anxiety)  
        PTS_Anxiety_2<- Score_Anxiety_2 + (M_Retest_Anxiety - M_Anxiety) 
        PTS_Anxiety_3<- Score_Anxiety_3 + (M_Retest_Anxiety - M_Anxiety) 
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2, PTS_Anxiety_3)
        PTS_Impulse_Control_1<- Score_Impulse_Control_1 + (M_Retest_Impulse_Control - M_Impulse_Control)  
        PTS_Impulse_Control_2<- Score_Impulse_Control_2 + (M_Retest_Impulse_Control - M_Impulse_Control) 
        PTS_Impulse_Control_3<- Score_Impulse_Control_3 + (M_Retest_Impulse_Control - M_Impulse_Control) 
        PTS_Impulse_Control<- c(PTS_Impulse_Control_1, PTS_Impulse_Control_2, PTS_Impulse_Control_3)
        PTS_Avoidance_1<- Score_Avoidance_1 + (M_Retest_Avoidance - M_Avoidance)  
        PTS_Avoidance_2<- Score_Avoidance_2 + (M_Retest_Avoidance - M_Avoidance) 
        PTS_Avoidance_3<- Score_Avoidance_3 + (M_Retest_Avoidance - M_Avoidance) 
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2, PTS_Avoidance_3)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Preoccupations<- Score_Preoccupations
        PTS_Failure_To_Adapt<- Score_Failure_To_Adapt
        PTS_Depressive_Mood<- Score_Depressive_Mood
        PTS_Anxiety<- Score_Anxiety
        PTS_Impulse_Control<- Score_Impulse_Control
        PTS_Avoidance<- Score_Avoidance
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS_3<- (B_Adj * Score_3) + A_Adj
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Preoccupations<- Rel_Preoccupations * (SD_Retest_Preoccupations/SD_Preoccupations)
        A_Constant_Preoccupations<- M_Retest_Preoccupations - (B_Slope_Preoccupations * M_Preoccupations)
        B_Adj_Preoccupations<- SD_Retest_Preoccupations/SD_Preoccupations
        A_Adj_Preoccupations<- M_Retest_Preoccupations - (B_Adj_Preoccupations * M_Preoccupations)
        PTS_Preoccupations_1<- (B_Adj_Preoccupations * Score_Preoccupations_1) + A_Adj_Preoccupations
        PTS_Preoccupations_2<- (B_Adj_Preoccupations * Score_Preoccupations_2) + A_Adj_Preoccupations
        PTS_Preoccupations_3<- (B_Adj_Preoccupations * Score_Preoccupations_3) + A_Adj_Preoccupations
        PTS_Preoccupations<- c(PTS_Preoccupations_1,PTS_Preoccupations_2, PTS_Preoccupations_3)
        B_Slope_Failure_To_Adapt<- Rel_Failure_To_Adapt * (SD_Retest_Failure_To_Adapt/SD_Failure_To_Adapt)
        A_Constant_Failure_To_Adapt<- M_Retest_Failure_To_Adapt - (B_Slope_Failure_To_Adapt * M_Failure_To_Adapt)
        B_Adj_Failure_To_Adapt<- SD_Retest_Failure_To_Adapt/SD_Failure_To_Adapt
        A_Adj_Failure_To_Adapt<- M_Retest_Failure_To_Adapt - (B_Adj_Failure_To_Adapt * M_Failure_To_Adapt)
        PTS_Failure_To_Adapt_1<- (B_Adj_Failure_To_Adapt * Score_Failure_To_Adapt_1) + A_Adj_Failure_To_Adapt
        PTS_Failure_To_Adapt_2<- (B_Adj_Failure_To_Adapt * Score_Failure_To_Adapt_2) + A_Adj_Failure_To_Adapt
        PTS_Failure_To_Adapt_3<- (B_Adj_Failure_To_Adapt * Score_Failure_To_Adapt_3) + A_Adj_Failure_To_Adapt
        PTS_Failure_To_Adapt<- c(PTS_Failure_To_Adapt_1,PTS_Failure_To_Adapt_2, PTS_Failure_To_Adapt_3)
        B_Slope_Depressive_Mood<- Rel_Depressive_Mood * (SD_Retest_Depressive_Mood/SD_Depressive_Mood)
        A_Constant_Depressive_Mood<- M_Retest_Depressive_Mood - (B_Slope_Depressive_Mood * M_Depressive_Mood)
        B_Adj_Depressive_Mood<- SD_Retest_Depressive_Mood/SD_Depressive_Mood
        A_Adj_Depressive_Mood<- M_Retest_Depressive_Mood - (B_Adj_Depressive_Mood * M_Depressive_Mood)
        PTS_Depressive_Mood_1<- (B_Adj_Depressive_Mood * Score_Depressive_Mood_1) + A_Adj_Depressive_Mood
        PTS_Depressive_Mood_2<- (B_Adj_Depressive_Mood * Score_Depressive_Mood_2) + A_Adj_Depressive_Mood
        PTS_Depressive_Mood_3<- (B_Adj_Depressive_Mood * Score_Depressive_Mood_3) + A_Adj_Depressive_Mood
        PTS_Depressive_Mood<- c(PTS_Depressive_Mood_1,PTS_Depressive_Mood_2, PTS_Depressive_Mood_3)
        B_Slope_Anxiety<- Rel_Anxiety * (SD_Retest_Anxiety/SD_Anxiety)
        A_Constant_Anxiety<- M_Retest_Anxiety - (B_Slope_Anxiety * M_Anxiety)
        B_Adj_Anxiety<- SD_Retest_Anxiety/SD_Anxiety
        A_Adj_Anxiety<- M_Retest_Anxiety - (B_Adj_Anxiety * M_Anxiety)
        PTS_Anxiety_1<- (B_Adj_Anxiety * Score_Anxiety_1) + A_Adj_Anxiety
        PTS_Anxiety_2<- (B_Adj_Anxiety * Score_Anxiety_2) + A_Adj_Anxiety
        PTS_Anxiety_3<- (B_Adj_Anxiety * Score_Anxiety_3) + A_Adj_Anxiety
        PTS_Anxiety<- c(PTS_Anxiety_1,PTS_Anxiety_2, PTS_Anxiety_3)
        B_Slope_Impulse_Control<- Rel_Impulse_Control * (SD_Retest_Impulse_Control/SD_Impulse_Control)
        A_Constant_Impulse_Control<- M_Retest_Impulse_Control - (B_Slope_Impulse_Control * M_Impulse_Control)
        B_Adj_Impulse_Control<- SD_Retest_Impulse_Control/SD_Impulse_Control
        A_Adj_Impulse_Control<- M_Retest_Impulse_Control - (B_Adj_Impulse_Control * M_Impulse_Control)
        PTS_Impulse_Control_1<- (B_Adj_Impulse_Control * Score_Impulse_Control_1) + A_Adj_Impulse_Control
        PTS_Impulse_Control_2<- (B_Adj_Impulse_Control * Score_Impulse_Control_2) + A_Adj_Impulse_Control
        PTS_Impulse_Control_3<- (B_Adj_Impulse_Control * Score_Impulse_Control_3) + A_Adj_Impulse_Control
        PTS_Impulse_Control<- c(PTS_Impulse_Control_1,PTS_Impulse_Control_2, PTS_Impulse_Control_3)
        B_Slope_Avoidance<- Rel_Avoidance * (SD_Retest_Avoidance/SD_Avoidance)
        A_Constant_Avoidance<- M_Retest_Avoidance - (B_Slope_Avoidance * M_Avoidance)
        B_Adj_Avoidance<- SD_Retest_Avoidance/SD_Avoidance
        A_Adj_Avoidance<- M_Retest_Avoidance - (B_Adj_Avoidance * M_Avoidance)
        PTS_Avoidance_1<- (B_Adj_Avoidance * Score_Avoidance_1) + A_Adj_Avoidance
        PTS_Avoidance_2<- (B_Adj_Avoidance * Score_Avoidance_2) + A_Adj_Avoidance
        PTS_Avoidance_3<- (B_Adj_Avoidance * Score_Avoidance_3) + A_Adj_Avoidance
        PTS_Avoidance<- c(PTS_Avoidance_1,PTS_Avoidance_2, PTS_Avoidance_3)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS_3<- B_Slope*Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Preoccupations<- Rel_Preoccupations * (SD_Retest_Preoccupations/SD_Preoccupations)
        PTS_Preoccupations_1<- B_Slope_Preoccupations * Score_Preoccupations_1
        PTS_Preoccupations_2<- B_Slope_Preoccupations * Score_Preoccupations_2
        PTS_Preoccupations_3<- B_Slope_Preoccupations * Score_Preoccupations_3
        PTS_Preoccupations<- c(PTS_Preoccupations_1, PTS_Preoccupations_2, PTS_Preoccupations_3)
        B_Slope_Failure_To_Adapt<- Rel_Failure_To_Adapt * (SD_Retest_Failure_To_Adapt/SD_Failure_To_Adapt)
        PTS_Failure_To_Adapt_1<- B_Slope_Failure_To_Adapt * Score_Failure_To_Adapt_1
        PTS_Failure_To_Adapt_2<- B_Slope_Failure_To_Adapt * Score_Failure_To_Adapt_2
        PTS_Failure_To_Adapt_3<- B_Slope_Failure_To_Adapt * Score_Failure_To_Adapt_3
        PTS_Failure_To_Adapt<- c(PTS_Failure_To_Adapt_1, PTS_Failure_To_Adapt_2, PTS_Failure_To_Adapt_3)
        B_Slope_Depressive_Mood<- Rel_Depressive_Mood * (SD_Retest_Depressive_Mood/SD_Depressive_Mood)
        PTS_Depressive_Mood_1<- B_Slope_Depressive_Mood * Score_Depressive_Mood_1
        PTS_Depressive_Mood_2<- B_Slope_Depressive_Mood * Score_Depressive_Mood_2
        PTS_Depressive_Mood_3<- B_Slope_Depressive_Mood * Score_Depressive_Mood_3
        PTS_Depressive_Mood<- c(PTS_Depressive_Mood_1, PTS_Depressive_Mood_2, PTS_Depressive_Mood_3)
        B_Slope_Anxiety<- Rel_Anxiety * (SD_Retest_Anxiety/SD_Anxiety)
        PTS_Anxiety_1<- B_Slope_Anxiety * Score_Anxiety_1
        PTS_Anxiety_2<- B_Slope_Anxiety * Score_Anxiety_2
        PTS_Anxiety_3<- B_Slope_Anxiety * Score_Anxiety_3
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2, PTS_Anxiety_3)
        B_Slope_Impulse_Control<- Rel_Impulse_Control * (SD_Retest_Impulse_Control/SD_Impulse_Control)
        PTS_Impulse_Control_1<- B_Slope_Impulse_Control * Score_Impulse_Control_1
        PTS_Impulse_Control_2<- B_Slope_Impulse_Control * Score_Impulse_Control_2
        PTS_Impulse_Control_3<- B_Slope_Impulse_Control * Score_Impulse_Control_3
        PTS_Impulse_Control<- c(PTS_Impulse_Control_1, PTS_Impulse_Control_2, PTS_Impulse_Control_3)
        B_Slope_Avoidance<- Rel_Avoidance * (SD_Retest_Avoidance/SD_Avoidance)
        PTS_Avoidance_1<- B_Slope_Avoidance * Score_Avoidance_1
        PTS_Avoidance_2<- B_Slope_Avoidance * Score_Avoidance_2
        PTS_Avoidance_3<- B_Slope_Avoidance * Score_Avoidance_3
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2, PTS_Avoidance_3) 
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Preoccupations_1<- Score_Preoccupations_1 + (M_Retest_Preoccupations - M_Preoccupations)
        PTS_Preoccupations_2<- Score_Preoccupations_2 + (M_Retest_Preoccupations - M_Preoccupations)
        PTS_Preoccupations_3<- Score_Preoccupations_3 + (M_Retest_Preoccupations - M_Preoccupations)
        PTS_Preoccupations<- c(PTS_Preoccupations_1, PTS_Preoccupations_2, PTS_Preoccupations_3)
        PTS_Failure_To_Adapt_1<- Score_Failure_To_Adapt_1 + (M_Retest_Failure_To_Adapt - M_Failure_To_Adapt)
        PTS_Failure_To_Adapt_2<- Score_Failure_To_Adapt_2 + (M_Retest_Failure_To_Adapt - M_Failure_To_Adapt)
        PTS_Failure_To_Adapt_3<- Score_Failure_To_Adapt_3 + (M_Retest_Failure_To_Adapt - M_Failure_To_Adapt)
        PTS_Failure_To_Adapt<- c(PTS_Failure_To_Adapt_1, PTS_Failure_To_Adapt_2, PTS_Failure_To_Adapt_3)
        PTS_Depressive_Mood_1<- Score_Depressive_Mood_1 + (M_Retest_Depressive_Mood - M_Depressive_Mood)
        PTS_Depressive_Mood_2<- Score_Depressive_Mood_2 + (M_Retest_Depressive_Mood - M_Depressive_Mood)
        PTS_Depressive_Mood_3<- Score_Depressive_Mood_3 + (M_Retest_Depressive_Mood - M_Depressive_Mood)
        PTS_Depressive_Mood<- c(PTS_Depressive_Mood_1, PTS_Depressive_Mood_2, PTS_Depressive_Mood_3)
        PTS_Anxiety_1<- Score_Anxiety_1 + (M_Retest_Anxiety - M_Anxiety)
        PTS_Anxiety_2<- Score_Anxiety_2 + (M_Retest_Anxiety - M_Anxiety)
        PTS_Anxiety_3<- Score_Anxiety_3 + (M_Retest_Anxiety - M_Anxiety)
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2, PTS_Anxiety_3)
        PTS_Impulse_Control_1<- Score_Impulse_Control_1 + (M_Retest_Impulse_Control - M_Impulse_Control)
        PTS_Impulse_Control_2<- Score_Impulse_Control_2 + (M_Retest_Impulse_Control - M_Impulse_Control)
        PTS_Impulse_Control_3<- Score_Impulse_Control_3 + (M_Retest_Impulse_Control - M_Impulse_Control)
        PTS_Impulse_Control<- c(PTS_Impulse_Control_1, PTS_Impulse_Control_2, PTS_Impulse_Control_3)
        PTS_Avoidance_1<- Score_Avoidance_1 + (M_Retest_Avoidance - M_Avoidance)
        PTS_Avoidance_2<- Score_Avoidance_2 + (M_Retest_Avoidance - M_Avoidance)
        PTS_Avoidance_3<- Score_Avoidance_3 + (M_Retest_Avoidance - M_Avoidance)
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2, PTS_Avoidance_3)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Preoccupations<- round(PTS_Preoccupations, digits = 2)
      PTS_Failure_To_Adapt<- round(PTS_Failure_To_Adapt, digits = 2)
      PTS_Depressive_Mood<- round(PTS_Depressive_Mood, digits = 2)
      PTS_Anxiety<- round(PTS_Anxiety, digits = 2)
      PTS_Impulse_Control<- round(PTS_Impulse_Control, digits = 2)
      PTS_Avoidance<- round(PTS_Avoidance, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE3<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_3 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2, SE3)
        SE_Preoccupations_1<- McSweeny_SE_Preoccupations*sqrt(1 + (1/SampleN) + ((Score_Preoccupations_1 - M_Preoccupations)^2/(SD_Preoccupations^2*(SampleN-1))))
        SE_Preoccupations_2<- McSweeny_SE_Preoccupations*sqrt(1 + (1/SampleN) + ((Score_Preoccupations_2 - M_Preoccupations)^2/(SD_Preoccupations^2*(SampleN-1))))
        SE_Preoccupations_3<- McSweeny_SE_Preoccupations*sqrt(1 + (1/SampleN) + ((Score_Preoccupations_3 - M_Preoccupations)^2/(SD_Preoccupations^2*(SampleN-1))))
        SE_Preoccupations<-c(SE_Preoccupations_1, SE_Preoccupations_2, SE_Preoccupations_3)
        SE_Failure_To_Adapt_1<- McSweeny_SE_Failure_To_Adapt*sqrt(1 + (1/SampleN) + ((Score_Failure_To_Adapt_1 - M_Failure_To_Adapt)^2/(SD_Failure_To_Adapt^2*(SampleN-1))))
        SE_Failure_To_Adapt_2<- McSweeny_SE_Failure_To_Adapt*sqrt(1 + (1/SampleN) + ((Score_Failure_To_Adapt_2 - M_Failure_To_Adapt)^2/(SD_Failure_To_Adapt^2*(SampleN-1))))
        SE_Failure_To_Adapt_3<- McSweeny_SE_Failure_To_Adapt*sqrt(1 + (1/SampleN) + ((Score_Failure_To_Adapt_3 - M_Failure_To_Adapt)^2/(SD_Failure_To_Adapt^2*(SampleN-1))))
        SE_Failure_To_Adapt<-c(SE_Failure_To_Adapt_1, SE_Failure_To_Adapt_2, SE_Failure_To_Adapt_3)
        SE_Depressive_Mood_1<- McSweeny_SE_Depressive_Mood*sqrt(1 + (1/SampleN) + ((Score_Depressive_Mood_1 - M_Depressive_Mood)^2/(SD_Depressive_Mood^2*(SampleN-1))))
        SE_Depressive_Mood_2<- McSweeny_SE_Depressive_Mood*sqrt(1 + (1/SampleN) + ((Score_Depressive_Mood_2 - M_Depressive_Mood)^2/(SD_Depressive_Mood^2*(SampleN-1))))
        SE_Depressive_Mood_3<- McSweeny_SE_Depressive_Mood*sqrt(1 + (1/SampleN) + ((Score_Depressive_Mood_3 - M_Depressive_Mood)^2/(SD_Depressive_Mood^2*(SampleN-1))))
        SE_Depressive_Mood<-c(SE_Depressive_Mood_1, SE_Depressive_Mood_2, SE_Depressive_Mood_3)
        SE_Anxiety_1<- McSweeny_SE_Anxiety*sqrt(1 + (1/SampleN) + ((Score_Anxiety_1 - M_Anxiety)^2/(SD_Anxiety^2*(SampleN-1))))
        SE_Anxiety_2<- McSweeny_SE_Anxiety*sqrt(1 + (1/SampleN) + ((Score_Anxiety_2 - M_Anxiety)^2/(SD_Anxiety^2*(SampleN-1))))
        SE_Anxiety_3<- McSweeny_SE_Anxiety*sqrt(1 + (1/SampleN) + ((Score_Anxiety_3 - M_Anxiety)^2/(SD_Anxiety^2*(SampleN-1))))
        SE_Anxiety<-c(SE_Anxiety_1, SE_Anxiety_2, SE_Anxiety_3)
        SE_Impulse_Control_1<- McSweeny_SE_Impulse_Control*sqrt(1 + (1/SampleN) + ((Score_Impulse_Control_1 - M_Impulse_Control)^2/(SD_Impulse_Control^2*(SampleN-1))))
        SE_Impulse_Control_2<- McSweeny_SE_Impulse_Control*sqrt(1 + (1/SampleN) + ((Score_Impulse_Control_2 - M_Impulse_Control)^2/(SD_Impulse_Control^2*(SampleN-1))))
        SE_Impulse_Control_3<- McSweeny_SE_Impulse_Control*sqrt(1 + (1/SampleN) + ((Score_Impulse_Control_3 - M_Impulse_Control)^2/(SD_Impulse_Control^2*(SampleN-1))))
        SE_Impulse_Control<-c(SE_Impulse_Control_1, SE_Impulse_Control_2, SE_Impulse_Control_3)
        SE_Avoidance_1<- McSweeny_SE_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Avoidance_1 - M_Avoidance)^2/(SD_Avoidance^2*(SampleN-1))))
        SE_Avoidance_2<- McSweeny_SE_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Avoidance_2 - M_Avoidance)^2/(SD_Avoidance^2*(SampleN-1))))
        SE_Avoidance_3<- McSweeny_SE_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Avoidance_3 - M_Avoidance)^2/(SD_Avoidance^2*(SampleN-1))))
        SE_Avoidance<-c(SE_Avoidance_1, SE_Avoidance_2, SE_Avoidance_3)
        SE<- round(SE, digits = 2)
        SE_Preoccupations<- round(SE_Preoccupations, digits = 2)
        SE_Failure_To_Adapt<- round(SE_Failure_To_Adapt, digits = 2)
        SE_Depressive_Mood<- round(SE_Depressive_Mood, digits = 2)
        SE_Anxiety<- round(SE_Anxiety, digits = 2)
        SE_Impulse_Control<- round(SE_Impulse_Control, digits = 2)
        SE_Avoidance<- round(SE_Avoidance, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2), (Conf*SE3))
        CI<- round(CI, digits = 2)
        CI_Preoccupations<- c((Conf*SE_Preoccupations_1), (Conf*SE_Preoccupations_2), (Conf*SE_Preoccupations_3))
        CI_Preoccupations<- round(CI_Preoccupations, digits = 2)
        CI_Failure_To_Adapt<- c((Conf*SE_Failure_To_Adapt_1), (Conf*SE_Failure_To_Adapt_2), (Conf*SE_Failure_To_Adapt_3))
        CI_Failure_To_Adapt<- round(CI_Failure_To_Adapt, digits = 2)
        CI_Depressive_Mood<- c((Conf*SE_Depressive_Mood_1), (Conf*SE_Depressive_Mood_2), (Conf*SE_Depressive_Mood_3))
        CI_Depressive_Mood<- round(CI_Depressive_Mood, digits = 2)
        CI_Anxiety<- c((Conf*SE_Anxiety_1), (Conf*SE_Anxiety_2), (Conf*SE_Anxiety_3))
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Impulse_Control<- c((Conf*SE_Impulse_Control_1), (Conf*SE_Impulse_Control_2), (Conf*SE_Impulse_Control_3))
        CI_Impulse_Control<- round(CI_Impulse_Control, digits = 2)
        CI_Avoidance<- c((Conf*SE_Avoidance_1), (Conf*SE_Avoidance_2), (Conf*SE_Avoidance_3))
        CI_Avoidance<- round(CI_Avoidance, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Preoccupations<- c((Conf*SE_Preoccupations), (Conf*SE_Preoccupations), (Conf*SE_Preoccupations))
        CI_Preoccupations<- round(CI_Preoccupations, digits = 2)
        CI_Failure_To_Adapt<- c((Conf*SE_Failure_To_Adapt), (Conf*SE_Failure_To_Adapt), (Conf*SE_Failure_To_Adapt))
        CI_Failure_To_Adapt<- round(CI_Failure_To_Adapt, digits = 2)
        CI_Depressive_Mood<- c((Conf*SE_Depressive_Mood), (Conf*SE_Depressive_Mood), (Conf*SE_Depressive_Mood))
        CI_Depressive_Mood<- round(CI_Depressive_Mood, digits = 2)
        CI_Anxiety<- c((Conf*SE_Anxiety), (Conf*SE_Anxiety), (Conf*SE_Anxiety))
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Impulse_Control<- c((Conf*SE_Impulse_Control), (Conf*SE_Impulse_Control), (Conf*SE_Impulse_Control))
        CI_Impulse_Control<- round(CI_Impulse_Control, digits = 2)
        CI_Avoidance<- c((Conf*SE_Avoidance), (Conf*SE_Avoidance), (Conf*SE_Avoidance))
        CI_Avoidance<- round(CI_Avoidance, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Preoccupations<- PTS_Preoccupations + CI_Preoccupations
      CI_Upper_Lim_Preoccupations<- round(CI_Upper_Lim_Preoccupations, digits = 2)
      CI_Lower_Lim_Preoccupations<-PTS_Preoccupations - CI_Preoccupations
      CI_Lower_Lim_Preoccupations<- round(CI_Lower_Lim_Preoccupations, digits = 2)
      CI_Upper_Lim_Failure_To_Adapt<- PTS_Failure_To_Adapt + CI_Failure_To_Adapt
      CI_Upper_Lim_Failure_To_Adapt<- round(CI_Upper_Lim_Failure_To_Adapt, digits = 2)
      CI_Lower_Lim_Failure_To_Adapt<-PTS_Failure_To_Adapt - CI_Failure_To_Adapt
      CI_Lower_Lim_Failure_To_Adapt<- round(CI_Lower_Lim_Failure_To_Adapt, digits = 2)
      CI_Upper_Lim_Depressive_Mood<- PTS_Depressive_Mood + CI_Depressive_Mood
      CI_Upper_Lim_Depressive_Mood<- round(CI_Upper_Lim_Depressive_Mood, digits = 2)
      CI_Lower_Lim_Depressive_Mood<-PTS_Depressive_Mood - CI_Depressive_Mood
      CI_Lower_Lim_Depressive_Mood<- round(CI_Lower_Lim_Depressive_Mood, digits = 2)
      CI_Upper_Lim_Anxiety<- PTS_Anxiety + CI_Anxiety
      CI_Upper_Lim_Anxiety<- round(CI_Upper_Lim_Anxiety, digits = 2)
      CI_Lower_Lim_Anxiety<-PTS_Anxiety - CI_Anxiety
      CI_Lower_Lim_Anxiety<- round(CI_Lower_Lim_Anxiety, digits = 2)
      CI_Upper_Lim_Impulse_Control<- PTS_Impulse_Control + CI_Impulse_Control
      CI_Upper_Lim_Impulse_Control<- round(CI_Upper_Lim_Impulse_Control, digits = 2)
      CI_Lower_Lim_Impulse_Control<-PTS_Impulse_Control - CI_Impulse_Control
      CI_Lower_Lim_Impulse_Control<- round(CI_Lower_Lim_Impulse_Control, digits = 2)
      CI_Upper_Lim_Avoidance<- PTS_Avoidance + CI_Avoidance
      CI_Upper_Lim_Avoidance<- round(CI_Upper_Lim_Avoidance, digits = 2)
      CI_Lower_Lim_Avoidance<-PTS_Avoidance - CI_Avoidance
      CI_Lower_Lim_Avoidance<- round(CI_Lower_Lim_Avoidance, digits = 2)
    
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Preoccupations == "2") {
        CI_Preoccupations<- input$Man_CI_Preoccupations
        CI_Preoccupations<- c(CI_Preoccupations, CI_Preoccupations, CI_Preoccupations)
        CI_Preoccupations<- round(CI_Preoccupations, digits = 2)
        CI_Upper_Lim_Preoccupations<- Score_Preoccupations + CI_Preoccupations
        CI_Upper_Lim_Preoccupations<- round(CI_Upper_Lim_Preoccupations, digits = 2)
        CI_Lower_Lim_Preoccupations<- Score_Preoccupations - CI_Preoccupations
        CI_Lower_Lim_Preoccupations<- round(CI_Lower_Lim_Preoccupations, digits = 2)
      }
      if(input$Select_CI_Failure_To_Adapt == "2") {
        CI_Failure_To_Adapt<- input$Man_CI_Failure_To_Adapt
        CI_Failure_To_Adapt<- c(CI_Failure_To_Adapt, CI_Failure_To_Adapt, CI_Failure_To_Adapt)
        CI_Failure_To_Adapt<- round(CI_Failure_To_Adapt, digits = 2)
        CI_Upper_Lim_Failure_To_Adapt<- Score_Failure_To_Adapt + CI_Failure_To_Adapt
        CI_Upper_Lim_Failure_To_Adapt<- round(CI_Upper_Lim_Failure_To_Adapt, digits = 2)
        CI_Lower_Lim_Failure_To_Adapt<- Score_Failure_To_Adapt - CI_Failure_To_Adapt
        CI_Lower_Lim_Failure_To_Adapt<- round(CI_Lower_Lim_Failure_To_Adapt, digits = 2)
      }
      if(input$Select_CI_Depressive_Mood == "2") {
        CI_Depressive_Mood<- input$Man_CI_Depressive_Mood
        CI_Depressive_Mood<- c(CI_Depressive_Mood, CI_Depressive_Mood, CI_Depressive_Mood)
        CI_Depressive_Mood<- round(CI_Depressive_Mood, digits = 2)
        CI_Upper_Lim_Depressive_Mood<- Score_Depressive_Mood + CI_Depressive_Mood
        CI_Upper_Lim_Depressive_Mood<- round(CI_Upper_Lim_Depressive_Mood, digits = 2)
        CI_Lower_Lim_Depressive_Mood<- Score_Depressive_Mood - CI_Depressive_Mood
        CI_Lower_Lim_Depressive_Mood<- round(CI_Lower_Lim_Depressive_Mood, digits = 2)
      }
      if(input$Select_CI_Anxiety == "2") {
        CI_Anxiety<- input$Man_CI_Anxiety
        CI_Anxiety<- c(CI_Anxiety,  CI_Anxiety, CI_Anxiety)
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Upper_Lim_Anxiety<- Score_Anxiety + CI_Anxiety
        CI_Upper_Lim_Anxiety<- round(CI_Upper_Lim_Anxiety, digits = 2)
        CI_Lower_Lim_Anxiety<- Score_Anxiety - CI_Anxiety
        CI_Lower_Lim_Anxiety<- round(CI_Lower_Lim_Anxiety, digits = 2)
      }
      if(input$Select_CI_Impulse_Control == "2") {
        CI_Impulse_Control<- input$Man_CI_Impulse_Control
        CI_Impulse_Control<- c(CI_Impulse_Control, CI_Impulse_Control, CI_Impulse_Control)
        CI_Impulse_Control<- round(CI_Impulse_Control, digits = 2)
        CI_Upper_Lim_Impulse_Control<- Score_Impulse_Control + CI_Impulse_Control
        CI_Upper_Lim_Impulse_Control<- round(CI_Upper_Lim_Impulse_Control, digits = 2)
        CI_Lower_Lim_Impulse_Control<- Score_Impulse_Control - CI_Impulse_Control
        CI_Lower_Lim_Impulse_Control<- round(CI_Lower_Lim_Impulse_Control, digits = 2)
      }
      if(input$Select_CI_Avoidance == "2") {
        CI_Avoidance<- input$Man_CI_Avoidance
        CI_Avoidance<- c(CI_Avoidance, CI_Avoidance, CI_Avoidance)
        CI_Avoidance<- round(CI_Avoidance, digits = 2)
        CI_Upper_Lim_Avoidance<- Score_Avoidance + CI_Avoidance
        CI_Upper_Lim_Avoidance<- round(CI_Upper_Lim_Avoidance, digits = 2)
        CI_Lower_Lim_Avoidance<- Score_Avoidance - CI_Avoidance
        CI_Lower_Lim_Avoidance<- round(CI_Lower_Lim_Avoidance, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_4<- round(input$Cutoff_4, digits = 2)
      Cutoff_Score_Preoccupations_1<- round(input$Cutoff_Preoccupations_1, digits = 2)
      Cutoff_Score_Preoccupations_2<- round(input$Cutoff_Preoccupations_2, digits = 2)
      Cutoff_Score_Preoccupations_3<- round(input$Cutoff_Preoccupations_3, digits = 2)
      Cutoff_Score_Failure_To_Adapt_1<- round(input$Cutoff_Failure_To_Adapt_1, digits = 2)
      Cutoff_Score_Failure_To_Adapt_2<- round(input$Cutoff_Failure_To_Adapt_2, digits = 2)
      Cutoff_Score_Failure_To_Adapt_3<- round(input$Cutoff_Failure_To_Adapt_3, digits = 2)
      Cutoff_Score_Depressive_Mood_1<- round(input$Cutoff_Depressive_Mood_1, digits = 2)
      Cutoff_Score_Depressive_Mood_2<- round(input$Cutoff_Depressive_Mood_2, digits = 2)
      Cutoff_Score_Depressive_Mood_3<- round(input$Cutoff_Depressive_Mood_3, digits = 2)
      Cutoff_Score_Anxiety_1<- round(input$Cutoff_Anxiety_1, digits = 2)
      Cutoff_Score_Anxiety_2<- round(input$Cutoff_Anxiety_2, digits = 2)
      Cutoff_Score_Anxiety_3<- round(input$Cutoff_Anxiety_3, digits = 2)
      Cutoff_Score_Impulse_Control_1<- round(input$Cutoff_Impulse_Control_1, digits = 2)
      Cutoff_Score_Impulse_Control_2<- round(input$Cutoff_Impulse_Control_2, digits = 2)
      Cutoff_Score_Impulse_Control_3<- round(input$Cutoff_Impulse_Control_3, digits = 2)
      Cutoff_Score_Avoidance_1<- round(input$Cutoff_Avoidance_1, digits = 2)
      Cutoff_Score_Avoidance_2<- round(input$Cutoff_Avoidance_2, digits = 2)
      Cutoff_Score_Avoidance_3<- round(input$Cutoff_Avoidance_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Cutoff_Score_4,Score_Preoccupations,Change_Preoccupations,PTS_Preoccupations, SE_Preoccupations, CI_Upper_Lim_Preoccupations, CI_Lower_Lim_Preoccupations, Cutoff_Score_Preoccupations_1,Cutoff_Score_Preoccupations_2,Cutoff_Score_Preoccupations_3,
                                      Score_Failure_To_Adapt,Change_Failure_To_Adapt, PTS_Failure_To_Adapt, SE_Failure_To_Adapt, CI_Upper_Lim_Failure_To_Adapt, CI_Lower_Lim_Failure_To_Adapt, Cutoff_Score_Failure_To_Adapt_1,Cutoff_Score_Failure_To_Adapt_2,Cutoff_Score_Failure_To_Adapt_3, 
                                      Score_Depressive_Mood,Change_Depressive_Mood,PTS_Depressive_Mood, SE_Depressive_Mood, CI_Upper_Lim_Depressive_Mood, CI_Lower_Lim_Depressive_Mood, Cutoff_Score_Depressive_Mood_1,Cutoff_Score_Depressive_Mood_2,Cutoff_Score_Depressive_Mood_3, 
                                      Score_Anxiety,Change_Anxiety,PTS_Anxiety, SE_Anxiety, CI_Upper_Lim_Anxiety, CI_Lower_Lim_Anxiety, Cutoff_Score_Anxiety_1,Cutoff_Score_Anxiety_2,Cutoff_Score_Anxiety_3,
                                      Score_Impulse_Control,Change_Impulse_Control, PTS_Impulse_Control, SE_Impulse_Control, CI_Upper_Lim_Impulse_Control, CI_Lower_Lim_Impulse_Control, Cutoff_Score_Impulse_Control_1,Cutoff_Score_Impulse_Control_2,Cutoff_Score_Impulse_Control_3, 
                                      Score_Avoidance,Change_Avoidance,PTS_Avoidance, SE_Avoidance, CI_Upper_Lim_Avoidance, CI_Lower_Lim_Avoidance, Cutoff_Score_Avoidance_1,Cutoff_Score_Avoidance_2,Cutoff_Score_Avoidance_3 
                                      )
    }
    
    
    #Create dataframes to be displayed as tables in pdf report (statistical values used in reliable change analyses)
    
    if(input$RelChangeMethod == 'Nunnally & Bernstein (1994)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Preoccupations<<- data.frame(Pop,  M_Preoccupations, SD_Preoccupations, RelChangeMethod, Rel_Preoccupations, ConfInt)
      names(Stats_Table_Preoccupations)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Failure_To_Adapt<<- data.frame(Pop,  M_Failure_To_Adapt, SD_Failure_To_Adapt, RelChangeMethod, Rel_Failure_To_Adapt, ConfInt)
      names(Stats_Table_Failure_To_Adapt)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Depressive_Mood<<- data.frame(Pop,  M_Depressive_Mood, SD_Depressive_Mood, RelChangeMethod, Rel_Depressive_Mood, ConfInt)
      names(Stats_Table_Depressive_Mood)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Anxiety<<- data.frame(Pop,  M_Anxiety, SD_Anxiety, RelChangeMethod, Rel_Anxiety, ConfInt)
      names(Stats_Table_Anxiety)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Impulse_Control<<- data.frame(Pop,  M_Impulse_Control, SD_Impulse_Control, RelChangeMethod, Rel_Impulse_Control, ConfInt)
      names(Stats_Table_Impulse_Control)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Avoidance<<- data.frame(Pop,  M_Avoidance, SD_Avoidance, RelChangeMethod, Rel_Avoidance, ConfInt)
      names(Stats_Table_Avoidance)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Chelune et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Preoccupations<<- data.frame(Pop,  M_Preoccupations, M_Retest_Preoccupations, SD_Preoccupations, RelChangeMethod, Rel_Preoccupations, ConfInt)
      names(Stats_Table_Preoccupations)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Failure_To_Adapt<<- data.frame(Pop,  M_Failure_To_Adapt, M_Retest_Failure_To_Adapt, SD_Failure_To_Adapt, RelChangeMethod, Rel_Failure_To_Adapt, ConfInt)
      names(Stats_Table_Failure_To_Adapt)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Depressive_Mood<<- data.frame(Pop,  M_Depressive_Mood, M_Retest_Depressive_Mood, SD_Depressive_Mood, RelChangeMethod, Rel_Depressive_Mood, ConfInt)
      names(Stats_Table_Depressive_Mood)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
     
      Stats_Table_Anxiety<<- data.frame(Pop,  M_Anxiety, M_Retest_Anxiety, SD_Anxiety, RelChangeMethod, Rel_Anxiety, ConfInt)
      names(Stats_Table_Anxiety)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Impulse_Control<<- data.frame(Pop,  M_Impulse_Control, M_Retest_Impulse_Control, SD_Impulse_Control, RelChangeMethod, Rel_Impulse_Control, ConfInt)
      names(Stats_Table_Impulse_Control)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Avoidance<<- data.frame(Pop,  M_Avoidance, M_Retest_Avoidance, SD_Avoidance, RelChangeMethod, Rel_Avoidance, ConfInt)
      names(Stats_Table_Avoidance)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    
    } else if (input$RelChangeMethod == 'Maassen et al. (2006)' | input$RelChangeMethod == 'McSweeny et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Preoccupations<<- data.frame(Pop,  M_Preoccupations, M_Retest_Preoccupations, SD_Preoccupations, SD_Retest_Preoccupations, RelChangeMethod, Rel_Preoccupations, ConfInt)
      names(Stats_Table_Preoccupations)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Failure_To_Adapt<<- data.frame(Pop,  M_Failure_To_Adapt, M_Retest_Failure_To_Adapt, SD_Failure_To_Adapt, SD_Retest_Failure_To_Adapt, RelChangeMethod, Rel_Failure_To_Adapt, ConfInt)
      names(Stats_Table_Failure_To_Adapt)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Depressive_Mood<<- data.frame(Pop,  M_Depressive_Mood, M_Retest_Depressive_Mood, SD_Depressive_Mood, SD_Retest_Depressive_Mood, RelChangeMethod, Rel_Depressive_Mood, ConfInt)
      names(Stats_Table_Depressive_Mood)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Anxiety<<- data.frame(Pop,  M_Anxiety, M_Retest_Anxiety, SD_Anxiety, SD_Retest_Anxiety, RelChangeMethod, Rel_Anxiety, ConfInt)
      names(Stats_Table_Anxiety)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Impulse_Control<<- data.frame(Pop,  M_Impulse_Control, M_Retest_Impulse_Control, SD_Impulse_Control, SD_Retest_Impulse_Control, RelChangeMethod, Rel_Impulse_Control, ConfInt)
      names(Stats_Table_Impulse_Control)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Avoidance<<- data.frame(Pop,  M_Avoidance, M_Retest_Avoidance, SD_Avoidance, SD_Retest_Avoidance, RelChangeMethod, Rel_Avoidance, ConfInt)
      names(Stats_Table_Avoidance)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
    
    } else if (input$RelChangeMethod == 'Crawford & Howell (1998)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, SampleN, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Preoccupations<<- data.frame(Pop,  M_Preoccupations, M_Retest_Preoccupations, SD_Preoccupations, SD_Retest_Preoccupations, RelChangeMethod, Rel_Preoccupations, SampleN,ConfInt)
      names(Stats_Table_Preoccupations)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Failure_To_Adapt<<- data.frame(Pop,  M_Failure_To_Adapt, M_Retest_Failure_To_Adapt, SD_Failure_To_Adapt, SD_Retest_Failure_To_Adapt, RelChangeMethod, Rel_Failure_To_Adapt, SampleN, ConfInt)
      names(Stats_Table_Failure_To_Adapt)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Depressive_Mood<<- data.frame(Pop,  M_Depressive_Mood, M_Retest_Depressive_Mood, SD_Depressive_Mood, SD_Retest_Depressive_Mood, RelChangeMethod, Rel_Depressive_Mood, SampleN,ConfInt)
      names(Stats_Table_Depressive_Mood)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Anxiety<<- data.frame(Pop,  M_Anxiety, M_Retest_Anxiety, SD_Anxiety, SD_Retest_Anxiety, RelChangeMethod, Rel_Anxiety, SampleN,ConfInt)
      names(Stats_Table_Anxiety)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Impulse_Control<<- data.frame(Pop,  M_Impulse_Control, M_Retest_Impulse_Control, SD_Impulse_Control, SD_Retest_Impulse_Control, RelChangeMethod, Rel_Impulse_Control, SampleN, ConfInt)
      names(Stats_Table_Impulse_Control)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Avoidance<<- data.frame(Pop,  M_Avoidance, M_Retest_Avoidance, SD_Avoidance, SD_Retest_Avoidance, RelChangeMethod, Rel_Avoidance, SampleN,ConfInt)
      names(Stats_Table_Avoidance)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
    } else if (input$RelChangeMethod == 'Jacobson & Truax (1991)' | input$RelChangeMethod == 'Speer (1992)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Preoccupations<<- data.frame(Pop,  SD_Preoccupations, RelChangeMethod, Rel_Preoccupations, ConfInt)
      names(Stats_Table_Preoccupations)<<- c("Reference Population","Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Failure_To_Adapt<<- data.frame(Pop,  SD_Failure_To_Adapt, RelChangeMethod, Rel_Failure_To_Adapt, ConfInt)
      names(Stats_Table_Failure_To_Adapt)<<- c("Reference Population","Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Depressive_Mood<<- data.frame(Pop,  SD_Depressive_Mood, RelChangeMethod, Rel_Depressive_Mood, ConfInt)
      names(Stats_Table_Depressive_Mood)<<- c("Reference Population","Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Anxiety<<- data.frame(Pop,  SD_Anxiety, RelChangeMethod, Rel_Anxiety, ConfInt)
      names(Stats_Table_Anxiety)<<- c("Reference Population","Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Impulse_Control<<- data.frame(Pop,  SD_Impulse_Control, RelChangeMethod, Rel_Impulse_Control, ConfInt)
      names(Stats_Table_Impulse_Control)<<- c("Reference Population","Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Avoidance<<- data.frame(Pop,  SD_Avoidance, RelChangeMethod, Rel_Avoidance, ConfInt)
      names(Stats_Table_Avoidance)<<- c("Reference Population","Sd", "Reliable Change Method", "Reliability", "Confidence")
  
    }
    
    #If custom confidence intervals options are selected, make sure that no values appear for PS and SE in pdf tables
    #Change the look of tables in report if custom confidence intervals options are selected
    
    if (input$Select_CI == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS = NA, SE = NA)
      Stats_Table_Fullscale<<- Stats_Table_Fullscale %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI[1])
    }
    if (input$Select_CI_Preoccupations == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Preoccupations = NA, SE_Preoccupations = NA)
      Stats_Table_Preoccupations<<- Stats_Table_Preoccupations %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                    "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Preoccupations[1])
    }
    if (input$Select_CI_Failure_To_Adapt == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Failure_To_Adapt = NA, SE_Failure_To_Adapt = NA)
      Stats_Table_Failure_To_Adapt<<- Stats_Table_Failure_To_Adapt %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                                        "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Failure_To_Adapt[1])
    }
    if (input$Select_CI_Depressive_Mood == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Depressive_Mood = NA, SE_Depressive_Mood = NA)
      Stats_Table_Depressive_Mood<<- Stats_Table_Depressive_Mood %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                    "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Depressive_Mood[1])
    }
    if (input$Select_CI_Anxiety == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Anxiety = NA, SE_Anxiety = NA)
      Stats_Table_Anxiety<<- Stats_Table_Anxiety %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                  "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Anxiety[1])
    }
    if (input$Select_CI_Impulse_Control == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Impulse_Control = NA, SE_Impulse_Control = NA)
      Stats_Table_Impulse_Control<<- Stats_Table_Impulse_Control %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                                  "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Impulse_Control[1])
    }
    if (input$Select_CI_Avoidance == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Avoidance = NA, SE_Avoidance = NA)
      Stats_Table_Avoidance<<- Stats_Table_Avoidance %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Avoidance[1])
    }
    
    
    
    #Create a dataframe to be used in the analytics spreadsheet.
    
    #Calculate variables for significant improvement
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$CI_Lower_Lim[1]) {
      ADNM20.Fullscale.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$CI_Lower_Lim[1]) {
      ADNM20.Fullscale.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$CI_Upper_Lim[1]) {
      ADNM20.Fullscale.Sig.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$CI_Upper_Lim[1]) {
      ADNM20.Fullscale.Sig.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Preoccupations[length(Entered_Scores_Df$Score_Preoccupations)] < Entered_Scores_Df$CI_Lower_Lim_Preoccupations[1]) {
      ADNM20.Preoccupations.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Preoccupations[length(Entered_Scores_Df$Score_Preoccupations)] >= Entered_Scores_Df$CI_Lower_Lim_Preoccupations[1]) {
      ADNM20.Preoccupations.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Preoccupations[length(Entered_Scores_Df$Score_Preoccupations)] > Entered_Scores_Df$CI_Upper_Lim_Preoccupations[1]) {
      ADNM20.Preoccupations.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Preoccupations[length(Entered_Scores_Df$Score_Preoccupations)] <= Entered_Scores_Df$CI_Upper_Lim_Preoccupations[1]) {
      ADNM20.Preoccupations.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Failure_To_Adapt[length(Entered_Scores_Df$Score_Failure_To_Adapt)] < Entered_Scores_Df$CI_Lower_Lim_Failure_To_Adapt[1]) {
      ADNM20.Failure.To.Adapt.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Failure_To_Adapt[length(Entered_Scores_Df$Score_Failure_To_Adapt)] >= Entered_Scores_Df$CI_Lower_Lim_Failure_To_Adapt[1]) {
      ADNM20.Failure.To.Adapt.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Failure_To_Adapt[length(Entered_Scores_Df$Score_Failure_To_Adapt)] > Entered_Scores_Df$CI_Upper_Lim_Failure_To_Adapt[1]) {
      ADNM20.Failure.To.Adapt.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Failure_To_Adapt[length(Entered_Scores_Df$Score_Failure_To_Adapt)] <= Entered_Scores_Df$CI_Upper_Lim_Failure_To_Adapt[1]) {
      ADNM20.Failure.To.Adapt.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Depressive_Mood[length(Entered_Scores_Df$Score_Depressive_Mood)] < Entered_Scores_Df$CI_Lower_Lim_Depressive_Mood[1]) {
      ADNM20.Depressive.Mood.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Depressive_Mood[length(Entered_Scores_Df$Score_Depressive_Mood)] >= Entered_Scores_Df$CI_Lower_Lim_Depressive_Mood[1]) {
      ADNM20.Depressive.Mood.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Depressive_Mood[length(Entered_Scores_Df$Score_Depressive_Mood)] > Entered_Scores_Df$CI_Upper_Lim_Depressive_Mood[1]) {
      ADNM20.Depressive.Mood.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Depressive_Mood[length(Entered_Scores_Df$Score_Depressive_Mood)] <= Entered_Scores_Df$CI_Upper_Lim_Depressive_Mood[1]) {
      ADNM20.Depressive.Mood.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] < Entered_Scores_Df$CI_Lower_Lim_Anxiety[1]) {
      ADNM20.Anxiety.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] >= Entered_Scores_Df$CI_Lower_Lim_Anxiety[1]) {
      ADNM20.Anxiety.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] > Entered_Scores_Df$CI_Upper_Lim_Anxiety[1]) {
      ADNM20.Anxiety.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] <= Entered_Scores_Df$CI_Upper_Lim_Anxiety[1]) {
      ADNM20.Anxiety.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Impulse_Control[length(Entered_Scores_Df$Score_Impulse_Control)] < Entered_Scores_Df$CI_Lower_Lim_Impulse_Control[1]) {
      ADNM20.Impulse.Control.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Impulse_Control[length(Entered_Scores_Df$Score_Impulse_Control)] >= Entered_Scores_Df$CI_Lower_Lim_Impulse_Control[1]) {
      ADNM20.Impulse.Control.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Impulse_Control[length(Entered_Scores_Df$Score_Impulse_Control)] > Entered_Scores_Df$CI_Upper_Lim_Impulse_Control[1]) {
      ADNM20.Impulse.Control.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Impulse_Control[length(Entered_Scores_Df$Score_Impulse_Control)] <= Entered_Scores_Df$CI_Upper_Lim_Impulse_Control[1]) {
      ADNM20.Impulse.Control.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] < Entered_Scores_Df$CI_Lower_Lim_Avoidance[1]) {
      ADNM20.Avoidance.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] >= Entered_Scores_Df$CI_Lower_Lim_Avoidance[1]) {
      ADNM20.Avoidance.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] > Entered_Scores_Df$CI_Upper_Lim_Avoidance[1]) {
      ADNM20.Avoidance.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] <= Entered_Scores_Df$CI_Upper_Lim_Avoidance[1]) {
      ADNM20.Avoidance.Sig.Deterioration<- "No"
    }
    
   
    #Calculate variables for improvement
    
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$Score[1]) {
      ADNM20.Fullscale.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$Score[1]) {
      ADNM20.Fullscale.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$Score[1]) {
      ADNM20.Fullscale.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$Score[1]) {
      ADNM20.Fullscale.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Preoccupations[length(Entered_Scores_Df$Score_Preoccupations)] < Entered_Scores_Df$Score_Preoccupations[1]) {
      ADNM20.Preoccupations.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Preoccupations[length(Entered_Scores_Df$Score_Preoccupations)] >= Entered_Scores_Df$Score_Preoccupations[1]) {
      ADNM20.Preoccupations.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Preoccupations[length(Entered_Scores_Df$Score_Preoccupations)] > Entered_Scores_Df$Score_Preoccupations[1]) {
      ADNM20.Preoccupations.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Preoccupations[length(Entered_Scores_Df$Score_Preoccupations)] <= Entered_Scores_Df$Score_Preoccupations[1]) {
      ADNM20.Preoccupations.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Failure_To_Adapt[length(Entered_Scores_Df$Score_Failure_To_Adapt)] < Entered_Scores_Df$Score_Failure_To_Adapt[1]) {
      ADNM20.Failure.To.Adapt.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Failure_To_Adapt[length(Entered_Scores_Df$Score_Failure_To_Adapt)] >= Entered_Scores_Df$Score_Failure_To_Adapt[1]) {
      ADNM20.Failure.To.Adapt.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Failure_To_Adapt[length(Entered_Scores_Df$Score_Failure_To_Adapt)] > Entered_Scores_Df$Score_Failure_To_Adapt[1]) {
      ADNM20.Failure.To.Adapt.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Failure_To_Adapt[length(Entered_Scores_Df$Score_Failure_To_Adapt)] <= Entered_Scores_Df$Score_Failure_To_Adapt[1]) {
      ADNM20.Failure.To.Adapt.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Depressive_Mood[length(Entered_Scores_Df$Score_Depressive_Mood)] < Entered_Scores_Df$Score_Depressive_Mood[1]) {
      ADNM20.Depressive.Mood.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Depressive_Mood[length(Entered_Scores_Df$Score_Depressive_Mood)] >= Entered_Scores_Df$Score_Depressive_Mood[1]) {
      ADNM20.Depressive.Mood.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Depressive_Mood[length(Entered_Scores_Df$Score_Depressive_Mood)] > Entered_Scores_Df$Score_Depressive_Mood[1]) {
      ADNM20.Depressive.Mood.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Depressive_Mood[length(Entered_Scores_Df$Score_Depressive_Mood)] <= Entered_Scores_Df$Score_Depressive_Mood[1]) {
      ADNM20.Depressive.Mood.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] < Entered_Scores_Df$Score_Anxiety[1]) {
      ADNM20.Anxiety.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] >= Entered_Scores_Df$Score_Anxiety[1]) {
      ADNM20.Anxiety.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] > Entered_Scores_Df$Score_Anxiety[1]) {
      ADNM20.Anxiety.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] <= Entered_Scores_Df$Score_Anxiety[1]) {
      ADNM20.Anxiety.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Impulse_Control[length(Entered_Scores_Df$Score_Impulse_Control)] < Entered_Scores_Df$Score_Impulse_Control[1]) {
      ADNM20.Impulse.Control.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Impulse_Control[length(Entered_Scores_Df$Score_Impulse_Control)] >= Entered_Scores_Df$Score_Impulse_Control[1]) {
      ADNM20.Impulse.Control.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Impulse_Control[length(Entered_Scores_Df$Score_Impulse_Control)] > Entered_Scores_Df$Score_Impulse_Control[1]) {
      ADNM20.Impulse.Control.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Impulse_Control[length(Entered_Scores_Df$Score_Impulse_Control)] <= Entered_Scores_Df$Score_Impulse_Control[1]) {
      ADNM20.Impulse.Control.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] < Entered_Scores_Df$Score_Avoidance[1]) {
      ADNM20.Avoidance.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] >= Entered_Scores_Df$Score_Avoidance[1]) {
      ADNM20.Avoidance.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] > Entered_Scores_Df$Score_Avoidance[1]) {
      ADNM20.Avoidance.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] <= Entered_Scores_Df$Score_Avoidance[1]) {
      ADNM20.Avoidance.Deterioration<- "No"
    }
    
   
    ADNM20.Fullscale.Change<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] - Entered_Scores_Df$Score[1]
    ADNM20.Preoccupations.Change<- Entered_Scores_Df$Score_Preoccupations[length(Entered_Scores_Df$Score_Preoccupations)] - Entered_Scores_Df$Score_Preoccupations[1]
    ADNM20.Failure.To.Adapt.Change<- Entered_Scores_Df$Score_Failure_To_Adapt[length(Entered_Scores_Df$Score_Failure_To_Adapt)] - Entered_Scores_Df$Score_Failure_To_Adapt[1]
    ADNM20.Depressive.Mood.Change<- Entered_Scores_Df$Score_Depressive_Mood[length(Entered_Scores_Df$Score_Depressive_Mood)] - Entered_Scores_Df$Score_Depressive_Mood[1]
    ADNM20.Fullscale.Comparisons<- length(Entered_Scores_Df$Change) - 1
    ADNM20.Preoccupations.Comparisons<- length(Entered_Scores_Df$Change_Preoccupations) - 1
    ADNM20.Failure.To.Adapt.Comparisons<- length(Entered_Scores_Df$Change_Failure_To_Adapt) - 1
    ADNM20.Depressive.Mood.Comparisons<- length(Entered_Scores_Df$Change_Depressive_Mood) - 1
    ADNM20.Fullscale.First.Date<- Entered_Scores_Df$Date[1]
    ADNM20.Preoccupations.First.Date<- Entered_Scores_Df$Date[1]
    ADNM20.Failure.To.Adapt.First.Date<- Entered_Scores_Df$Date[1]
    ADNM20.Depressive.Mood.First.Date<- Entered_Scores_Df$Date[1]
    ADNM20.Fullscale.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    ADNM20.Preoccupations.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    ADNM20.Failure.To.Adapt.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    ADNM20.Depressive.Mood.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    ADNM20.Fullscale.First.Score<- Entered_Scores_Df$Score[1]
    ADNM20.Preoccupations.First.Score<- Entered_Scores_Df$Score_Preoccupations[1]
    ADNM20.Failure.To.Adapt.First.Score<- Entered_Scores_Df$Score_Failure_To_Adapt[1]
    ADNM20.Depressive.Mood.First.Score<- Entered_Scores_Df$Score_Depressive_Mood[1]
    ADNM20.Fullscale.Last.Score<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)]
    ADNM20.Preoccupations.Last.Score<- Entered_Scores_Df$Score_Preoccupations[length(Entered_Scores_Df$Score_Preoccupations)]
    ADNM20.Failure.To.Adapt.Last.Score<- Entered_Scores_Df$Score_Failure_To_Adapt[length(Entered_Scores_Df$Score_Failure_To_Adapt)]
    ADNM20.Depressive.Mood.Last.Score<- Entered_Scores_Df$Score_Depressive_Mood[length(Entered_Scores_Df$Score_Depressive_Mood)]

    
    ADNM20.Anxiety.Change<- Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] - Entered_Scores_Df$Score_Anxiety[1]
    ADNM20.Impulse.Control.Change<- Entered_Scores_Df$Score_Impulse_Control[length(Entered_Scores_Df$Score_Impulse_Control)] - Entered_Scores_Df$Score_Impulse_Control[1]
    ADNM20.Avoidance.Change<- Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] - Entered_Scores_Df$Score_Avoidance[1]
    ADNM20.Anxiety.Comparisons<- length(Entered_Scores_Df$Change_Anxiety) - 1
    ADNM20.Impulse.Control.Comparisons<- length(Entered_Scores_Df$Change_Impulse_Control) - 1
    ADNM20.Avoidance.Comparisons<- length(Entered_Scores_Df$Change_Avoidance) - 1
    ADNM20.Anxiety.First.Date<- Entered_Scores_Df$Date[1]
    ADNM20.Impulse.Control.First.Date<- Entered_Scores_Df$Date[1]
    ADNM20.Avoidance.First.Date<- Entered_Scores_Df$Date[1]
    ADNM20.Anxiety.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    ADNM20.Impulse.Control.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    ADNM20.Avoidance.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    ADNM20.Anxiety.First.Score<- Entered_Scores_Df$Score_Anxiety[1]
    ADNM20.Impulse.Control.First.Score<- Entered_Scores_Df$Score_Impulse_Control[1]
    ADNM20.Avoidance.First.Score<- Entered_Scores_Df$Score_Avoidance[1]
    ADNM20.Anxiety.Last.Score<- Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)]
    ADNM20.Impulse.Control.Last.Score<- Entered_Scores_Df$Score_Impulse_Control[length(Entered_Scores_Df$Score_Impulse_Control)]
    ADNM20.Avoidance.Last.Score<- Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)]
  
    
    
    Analytics_Df<<- data.frame(ADNM20.Fullscale.First.Date, ADNM20.Fullscale.First.Score, ADNM20.Fullscale.Comparisons, ADNM20.Fullscale.Change, ADNM20.Fullscale.Last.Date, ADNM20.Fullscale.Last.Score, ADNM20.Fullscale.Improvement,ADNM20.Fullscale.Sig.Improvement, ADNM20.Fullscale.Deterioration, ADNM20.Fullscale.Sig.Deterioration,
                               ADNM20.Preoccupations.First.Date, ADNM20.Preoccupations.First.Score, ADNM20.Preoccupations.Comparisons, ADNM20.Preoccupations.Change, ADNM20.Preoccupations.Last.Date, ADNM20.Preoccupations.Last.Score, ADNM20.Preoccupations.Improvement, ADNM20.Preoccupations.Sig.Improvement, ADNM20.Preoccupations.Deterioration, ADNM20.Preoccupations.Sig.Deterioration,
                               ADNM20.Failure.To.Adapt.First.Date, ADNM20.Failure.To.Adapt.First.Score, ADNM20.Failure.To.Adapt.Comparisons, ADNM20.Failure.To.Adapt.Change, ADNM20.Failure.To.Adapt.Last.Date, ADNM20.Failure.To.Adapt.Last.Score, ADNM20.Failure.To.Adapt.Improvement, ADNM20.Failure.To.Adapt.Sig.Improvement, ADNM20.Failure.To.Adapt.Deterioration, ADNM20.Failure.To.Adapt.Sig.Deterioration, 
                               ADNM20.Depressive.Mood.First.Date, ADNM20.Depressive.Mood.First.Score, ADNM20.Depressive.Mood.Comparisons, ADNM20.Depressive.Mood.Change, ADNM20.Depressive.Mood.Last.Date, ADNM20.Depressive.Mood.Last.Score, ADNM20.Depressive.Mood.Improvement, ADNM20.Depressive.Mood.Sig.Improvement, ADNM20.Depressive.Mood.Deterioration, ADNM20.Depressive.Mood.Sig.Deterioration, 
                               ADNM20.Anxiety.First.Date, ADNM20.Anxiety.First.Score, ADNM20.Anxiety.Comparisons, ADNM20.Anxiety.Change, ADNM20.Anxiety.Last.Date, ADNM20.Anxiety.Last.Score, ADNM20.Anxiety.Improvement,ADNM20.Anxiety.Sig.Improvement, ADNM20.Anxiety.Deterioration, ADNM20.Anxiety.Sig.Deterioration,
                               ADNM20.Impulse.Control.First.Date, ADNM20.Impulse.Control.First.Score, ADNM20.Impulse.Control.Comparisons, ADNM20.Impulse.Control.Change, ADNM20.Impulse.Control.Last.Date, ADNM20.Impulse.Control.Last.Score, ADNM20.Impulse.Control.Improvement, ADNM20.Impulse.Control.Sig.Improvement, ADNM20.Impulse.Control.Deterioration, ADNM20.Impulse.Control.Sig.Deterioration,
                               ADNM20.Avoidance.First.Date, ADNM20.Avoidance.First.Score, ADNM20.Avoidance.Comparisons, ADNM20.Avoidance.Change, ADNM20.Avoidance.Last.Date, ADNM20.Avoidance.Last.Score, ADNM20.Avoidance.Improvement, ADNM20.Avoidance.Sig.Improvement, ADNM20.Avoidance.Deterioration, ADNM20.Avoidance.Sig.Deterioration 
                               )
    
    
    
  })
  
  
  
  #Create an expression to activate the entered data & create notifications to indicate inaccurate data entry
  
  Entered_Scores_Notifications<- observeEvent(input$Action_Submit_Data, {
    
    Entered_Scores_Reac()
    
    if(length(Score_1a) < 20) {
      showNotification("The ADNM-20 is an 20-item scale. You have entered less than 18 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(length(Score_1a) > 20) {
      showNotification("The ADNM-20 is an 20-item scale. You have entered more than 18 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(input$Timepoint != "1") {
      if(length(Score_2a) < 20) {
        showNotification("The ADNM-20 is an 20-item scale. You have entered less than 18 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_2a) > 20) {
        showNotification("The ADNM-20 is an 20-item scale. You have entered more than 18 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
    if(input$Timepoint == "3") {
      if(length(Score_3a) < 20) {
        showNotification("The ADNM-20 is an 20-item scale. You have entered less than 18 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_3a) > 20) {
        showNotification("The ADNM-20 is an 20-item scale. You have entered more than 18 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
  })
  
  
  #Add imported scores to entered scores if submit button is clicked & plug the gap when calculating change between imported & entered scores
  
  Combine_Data_Reac<- reactive({
    
    req(input$Input_File1)
    Infile1a<- input$Input_File1
    Imported_Scores_CSV_ToCombine<- read.csv(Infile1a$datapath, header=TRUE, sep=",",
                                             quote="\"", row.names = NULL)
    
    Imported_Scores_CSV_ToCombine<- data.frame(Imported_Scores_CSV_ToCombine)
    
    Gap<- Entered_Scores_Df[1,2] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),2]
    Entered_Scores_Df[1,3]<- Gap
    
    Gap_Preoccupations<- Entered_Scores_Df[1,12] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),11]
    Entered_Scores_Df[1,13]<- Gap_Preoccupations
    
    Gap_Failure_To_Adapt<- Entered_Scores_Df[1,21] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),20]
    Entered_Scores_Df[1,22]<- Gap_Failure_To_Adapt
    
    Gap_Depressive_Mood<- Entered_Scores_Df[1,30] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),29]
    Entered_Scores_Df[1,31]<- Gap_Depressive_Mood
    
    Gap_Anxiety<- Entered_Scores_Df[1,39] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),38]
    Entered_Scores_Df[1,40]<- Gap_Anxiety
    
    Gap_Impulse_Control<- Entered_Scores_Df[1,48] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),47]
    Entered_Scores_Df[1,49]<- Gap_Impulse_Control
    
    Gap_Avoidance<- Entered_Scores_Df[1,57] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),56]
    Entered_Scores_Df[1,58]<- Gap_Avoidance
    
    
    Entered_Scores_Df<<- data.frame(rbind(Imported_Scores_CSV_ToCombine, Entered_Scores_Df))
    
  })
  
  
  #Create an expression to activate the imported data
  
  Combine_Data_Warning<- observeEvent(input$Action_Combine, {
    
    Combine_Data_Reac()
    
    showModal(modalDialog(
      title = "Important Information About Importing & Combining Scores", footer = modalButton("Okay"),
      "When combining imported and newly-entered scores, ensure that you have used the same method for calculating confidence intervals.", br(), br(),
      "Also make sure that the total number of timepoints does not exceed 5. The patient report cannot be generated if there is data for more than 5 timepoints."
    ))
    
    
  })
  
  
  #Create pdf download functionality
  
  
  output$report <- downloadHandler(
    
    filename = paste0(" ADNM-20 Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Pass data objects to Rmd document
      params <- list(
        PN = PN,
        CN = CN,
        Tab_Reference = Tab_Reference,
        Entered_Scores_Df = Entered_Scores_Df,
        Stats_Table_Fullscale = Stats_Table_Fullscale,
        Stats_Table_Preoccupations = Stats_Table_Preoccupations,
        Stats_Table_Failure_To_Adapt = Stats_Table_Failure_To_Adapt,
        Stats_Table_Depressive_Mood = Stats_Table_Depressive_Mood,
        Stats_Table_Anxiety = Stats_Table_Anxiety,
        Stats_Table_Impulse_Control = Stats_Table_Impulse_Control,
        Stats_Table_Avoidance = Stats_Table_Avoidance,
        Cutoff_Names = Cutoff_Names,
        Item_Df = Item_Df,
        Event_Df = Event_Df,
        Straining_Event = Straining_Event
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv(),
                                        withProgress(message = 'Report generation in progress.',
                                                     detail = 'Please wait a moment...', value = 0, {
                                                       for (i in 1:25) {
                                                         incProgress(1/25)
                                                         Sys.sleep(0.25)
                                                         
                                                       }
                                                     }))
      )
    }
  )
  
  
  
  #Create event reactive expression preparing entered scale scores for exporting
  
  
  Export_Reac<- eventReactive(input$Action_Export, {
    
    Entered_Scores_Df
    
  })
  
  
  
  #Set up functionality to export data to spreadsheet for storage
  
  
  output$Display_Export_Data <- DT::renderDataTable({
    DT::datatable(Export_Reac(), extensions = 'FixedColumns', rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE, fixedColumns = list(leftColumns = 2)))
  })
  
  
  output$Download_Export_Data <- downloadHandler(
    
    
    filename = function() {
      paste(paste0(" ADNM-20 Data ", format(Sys.time(), '%d/%m/%y')), input$Output_Filetype1, sep = ".")
    },
    
    
    content = function(file) {
      sep <- switch(input$Output_Filetype1, "csv" = ",", "tsv" = "\t")
      
      
      write.table(Export_Reac(), file, sep = sep,
                  row.names = FALSE)
    }
  )
  
  
  #Provide information about analytics to be activated by info button
  
  observeEvent(input$Analytics_Info, {
    showModal(modalDialog(
      title = "Service Analytics Information", footer = modalButton("Okay"), "When using the PsychlytX web applications, you have the option of creating an analytics dataset. By using this dataset, you can gain
      critical insights into the performance of your practice — where it is doing well and how it might best be improved. 
      To get started, use a web application to enter your patient's end-of-therapy scores on an outcome measure (e.g. GAD-7). Then click the 'Service Analytics' tab to create a new analytics dataset.
      You'll be asked to provide some demographic information about the patient. After doing so, you can download your new analytics dataset as an Excel spreadsheet, to be safely stored on your
      system (in a secure location of your choosing). When next using a web application (either for this patient or a new patient), you simply re-import your analytics dataset and add to it, by clicking the 'Add to Dataset' button. 
      Once you have a sufficient number of cases stored in your analytics dataset, use our analytics web application to analyse the data."
    ))
  })
  
  
  observe({
    
    Infile2<- input$Input_File2
    
    if (is.null(Infile2))
      return(NULL)
    
    Old_Analytics_CSV<<- read.csv(Infile2$datapath, header=TRUE, sep=",",
                                  quote="\"", row.names = NULL)
    
    
  })
  
  
  #Create a widget allowing user to create new custom variables    
  
  New_Custom_Widg_Reac <- eventReactive(input$Create_New_Widg,{
    
    New_Custom_Names<- as.character(unlist(strsplit(input$Custom_Names,",")))
    
    Input_List <- lapply(1:length(New_Custom_Names), function(i) {
      Input_Name <- paste(paste0(New_Custom_Names[i]))
      Widg_Fun<-function (InputId,Value) {
        textAreaInput(Input_Name,Input_Name, width = "200px", height = "43px", resize = "vertical")
      }
      Widg_Fun(Input_Name, "")
    })
    do.call(tagList, Input_List)},ignoreInit = T)
  
  output$New_Custom_Widgets = renderUI({
    
    New_Custom_Widg_Reac()
    
  })
  
  
  #Create a widget that produces certain number of boxes based on columns containing the word custom
  
  
  Old_Custom_Widg_Reac <- reactive({
    
    Infile2<- input$Input_File2
    
    if (is.null(Infile2))
      return(NULL)
    
    Old_Analytics_CSV_Reread<- read.csv(Infile2$datapath, header=TRUE, sep=",",
                                        quote="\"", row.names = NULL)
    
    Analytics_Df_Old_Widg<- as.data.frame(Old_Analytics_CSV_Reread)
    
    Analytics_Df_Old_Widg<- Analytics_Df_Old_Widg %>% as_tibble() %>% dplyr::select(contains("Custom"))
    
    NList<- names(Analytics_Df_Old_Widg)
    
    if(length(NList >= 1)) {
      Input_List <- lapply(1:ncol(Analytics_Df_Old_Widg), function(i) {
        Input_Name <- paste(paste0(NList[i]))
        Text_Input_Row<-function (inputId,value) {
          textAreaInput(Input_Name,Input_Name, width = "200px", height = "43px", resize = "vertical")
        }
        Text_Input_Row(Input_Name, "")
      })
      
      do.call(tagList, Input_List)
    }
    
  })
  
  output$Old_Custom_Widgets = renderUI({
    
    validate(need(Old_Custom_Widg_Reac(), "No custom variables to show"))
    Old_Custom_Widg_Reac()
    
  })
  
  
  
  #Combine and process all the data from custom variables  
  
  Custom_Analytics_Reac<- eventReactive(input$Action_Show_Analytics, {
    
    Entry.Date<- format(Sys.time(), '%d/%m/%y')
    
    Name<- input$ControlName
    Sex<- input$Sex
    Age<- input$Age
    Sexuality<- input$Sexuality
    Relationship.Status<- input$Relationship
    Children<- input$Children
    Workforce.Status<- input$Workforce
    Education<- input$Education
    Suburb<- input$Suburb
    Principal.Diagnosis<- input$Principal_Diagnosis
    Secondary.Diagnosis<- paste(input$Secondary_Diagnosis, sep = ",", collapse = ",")
    Referrer<- input$Referrer
    Attendance.Arrangement<- input$Attendance_Arrangement
    Attendance.Quality<- input$Attendance_Quality
    Therapy<- input$Therapy
    Fee<- input$Fee
    Therapy.Duration<- input$Duration
    Early.Dropout<- input$Dropout
    Demographics_Df<- data.frame(Entry.Date, Name, Sex, Age, Sexuality, Relationship.Status,
                                 Children, Workforce.Status, Education, Suburb, Attendance.Arrangement, Attendance.Quality,
                                 Principal.Diagnosis, Secondary.Diagnosis, Referrer, Therapy, Therapy.Duration, Early.Dropout)
    
    
    
    New_Custom_Names_Loop<-as.character(unlist(strsplit(input$Custom_Names,",")))
    if(length(New_Custom_Names_Loop) >= 1) {
      val <- 0
      if(length(New_Custom_Names_Loop) >= 1) {
        for(lim in 1:length(New_Custom_Names_Loop)){
          val[lim] <- input[[paste0(New_Custom_Names_Loop[lim])]]
        }
        New_Custom_Vars_Df<- data.frame(Widgvalues = val, row.names = paste0(New_Custom_Names_Loop, ".Custom"))
        New_Custom_Vars_Df<- t(New_Custom_Vars_Df)
        New_Custom_Vars_Df<- data.frame(New_Custom_Vars_Df)
        rownames(New_Custom_Vars_Df)<- NULL
      }
    }
    
    if (input$SpreadType == "Add To My Analytics Dataset") {
      Old_LoopDf<- Old_Analytics_CSV
      Old_LoopDf<- Old_LoopDf %>% dplyr::select(contains("Custom"))
      Old_Names<- paste(names(Old_LoopDf), sep = ",")
      Old_Val <- 0
      if(length(Old_Names) >= 1) {
        Len<- length(Old_Names)
        for(o in 1:Len){
          Old_Val[o] <- input[[paste0(Old_Names[o])]]
        }
        Old_Custom_Vars_Df<- data.frame(Widg_Val = Old_Val, row.names = paste(names(Old_LoopDf), sep = ","))
        Old_Custom_Vars_Df<- t(Old_Custom_Vars_Df)
        rownames(Old_Custom_Vars_Df)<- NULL
        Old_Custom_Vars_Df<- as_tibble(Old_Custom_Vars_Df)
      }
    }
    
    
    if (input$SpreadType == "New Analytics Dataset") {
      if(length(New_Custom_Names_Loop) >= 1) {
        New_Patient_ND<- bind_cols(Demographics_Df, New_Custom_Vars_Df, Analytics_Df)
      } else {
        New_Patient_ND<- bind_cols(Demographics_Df, Analytics_Df) 
      }
      New_Patient_ND
    } else if (input$SpreadType == "Add To My Analytics Dataset") {
      if(Name %in% Old_Analytics_CSV$Name) {
        Old_Analytics_CSV<- as.data.frame(Old_Analytics_CSV)
        
        Same_Patient_OD<- bind_rows(Old_Analytics_CSV, Analytics_Df)
        
        Same_Patient_OD[nrow(Same_Patient_OD),1]<- input$ControlName
        
        Same_Patient_OD<- Same_Patient_OD %>% group_by(Name) %>% mutate_all(funs(na.locf(., na.rm = FALSE, fromLast = FALSE)))%>%filter(row_number()==n())
        
        Same_Patient_OD<- dplyr::select(Same_Patient_OD, Name:Early.Dropout, contains("Custom"), everything())
      } else {
        if(length(New_Custom_Names_Loop) >= 1 & length(Old_Names) == 0) {
          New_Patient_OD<- bind_cols(Demographics_Df, New_Custom_Vars_Df, Analytics_Df)
        } else if(length(Old_Names) >= 1 & length(New_Custom_Names_Loop) == 0) {
          New_Patient_OD<- bind_cols(Demographics_Df, Old_Custom_Vars_Df, Analytics_Df)
        } else if(length(Old_Names) >= 1 & length(New_Custom_Names_Loop) >= 1) {
          New_Patient_OD<- bind_cols(Demographics_Df, Old_Custom_Vars_Df, New_Custom_Vars_Df, Analytics_Df)
        } else {
          New_Patient_OD<- bind_cols(Demographics_Df, Analytics_Df) 
        }
        List1<- list(Old_Analytics_CSV, New_Patient_OD)
        New_Patient_OD<- rbindlist(List1, use.names = TRUE, fill = TRUE)
        New_Patient_OD<- dplyr::select(New_Patient_OD, Name:Early.Dropout, contains("Custom"), everything())
      }
    }
    
  })
  
  
  
  
  #Ensure that the name on the data-entry page automatically appears on the analytics page
  
  observe({
    
    C_Name <- input$PatientName
    
    updateTextInput(session, "ControlName", value = C_Name)
    
  })
  
  
  #Display the updated analytics spreadsheet
  
  output$Display_New_Analytics <- DT::renderDataTable({
    DT::datatable(tail(Custom_Analytics_Reac()), extensions = 'FixedColumns', rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE, fixedColumns = list(leftColumns = 2)))
  })
  
  
  #Create download functionality for updated analytics spreadsheet  
  
  output$Download_New_Analytics <- downloadHandler(
    
    
    filename = function() {
      paste("Analytics Dataset", input$Output_Filetype2, sep = ".")
    },
    
    
    content = function(file) {
      sep <- switch(input$Output_Filetype2, "csv" = ",", "tsv" = "\t")
      
      
      write.table(Custom_Analytics_Reac(), file, sep = sep,
                  row.names = FALSE)
    }
  )
  
  
  
  
  }




enableBookmarking("url")

shinyApp(ui, server)













