
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
      menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "OCI_R"),
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
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| Obsessive Compulsive Inventory - Revised (OCI-R)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 830),
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
                "Hajcak, G., Huppert, J. D., Simons, R. F., & Foa, E. B. (2004). Psychometric properties of the OCI-R in a college sample. Behaviour Research and Therapy, 42(1), 115-123.", br(), br(), 
                "Huppert, J. D., Walther, M. R., Hajcak, G., Yadin, E., Foa, E. B., Simpson, H. B., & Liebowitz, M. R. (2007). The OCI-R: Validation of the subscales in a clinical sample. Journal of Anxiety Disorders, 21(3), 394-406.", br(), br(), 
                "Rapp, A. M., Bergman, R. L., Piacentini, J., & McGuire, J. F. (2016). Evidence-based assessment of obsessive–compulsive disorder. Journal of Central Nervous System Disease, 8, JCNSD. S38359.", br(), br(), 
                "SmARi, J., Olason, D. T., EYÞÓRSDÓTTIR, Á, & FRÖLUNDE, M. (2007). Psychometric properties of the obsessive compulsive Inventory‐Revised among icelandic college students. Scandinavian Journal of Psychology, 48(2), 127-133.", br(), br(), 
                "Storch, E. A., Benito, K., & Goodman, W. (2011). Assessment scales for obsessive–compulsive disorder. Neuropsychiatry, 1(3), 243-250." 
                
                
        ),
        
        
        
        tabItem(tabName = "OCI_R",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel("Scale",
                             wellPanel(style = "background-color: #ffffff; color: black",
                                       fluidRow(
                                         column(width = 12, offset = 5, h3(tags$strong("OCI-R")))
                                       ),
                                       fluidRow(
                                         column(width = 12, div(h3("The following statements refer to experiences that many people have in their everyday lives. Circle the number
                                                that best describes", tags$strong("HOW MUCH"), "that experience has", tags$strong("DISTRESSED"), "or", tags$strong("BOTHERED"), "you during the", tags$strong("PAST MONTH."), "The numbers refer to the 
                                                following verbal labels:")))
                                       ),
                                       hr(),
                                       fluidRow(
                                         h4(tags$strong(
                                                        HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), "0", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), 
                                                        HTML('&emsp;'), HTML('&emsp;'), "1", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), 
                                                        HTML('&emsp;'), HTML('&emsp;'), "2", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                                                        HTML('&emsp;'), HTML('&emsp;'), "3", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                                                        HTML('&emsp;'), HTML('&emsp;'), "4"
                                                        ))
                                       ),
                                       fluidRow(
                                         div(
                                           h4(tags$strong(
                                                          HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), "Not at all", HTML('&emsp;'), 
                                                          HTML('&emsp;'), "|", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), "A little", HTML('&emsp;'), 
                                                          HTML('&emsp;'), "|", HTML('&emsp;'), HTML('&emsp;'), "Moderately", HTML('&emsp;'),
                                                          HTML('&emsp;'), "|", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),"A lot", HTML('&emsp;'),
                                                          HTML('&emsp;'), "|", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), "Extremely"
                                           )))),
                                       hr(),
                                       
                                       fluidRow(style = "background-color: #ededed",
                                         column(width = 6, h4("1. I have saved up so many things that they get in the way.")),
                                         column(width = 6,radioButtons("Item_1", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("2. I check things more often than necessary.")),
                                         column(width = 6, radioButtons("Item_2", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(style = "background-color: #ededed",
                                         column(width = 6, h4("3. I get upset if objects are not arranged properly.")),
                                         column(width = 6, radioButtons("Item_3", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("4. I feel compelled to count while I am doing things.")),
                                         column(width = 6, radioButtons("Item_4", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(style = "background-color: #ededed",
                                         column(width = 6, h4("5. I find it difficult to touch an object when I know it has been touched by strangers or certain people.")),
                                         column(width = 6, radioButtons("Item_5", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("6. I find it difficult to control my own thoughts.")),
                                         column(width = 6, radioButtons("Item_6", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(style = "background-color: #ededed",
                                         column(width = 6, h4("7. I collect things I don’t need.")),
                                         column(width = 6, radioButtons("Item_7", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("8. I repeatedly check doors, windows, drawers, etc.")),
                                         column(width = 6, radioButtons("Item_8", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(style = "background-color: #ededed",
                                         column(width = 6, h4("9. I get upset if others change the way I have arranged things.")),
                                         column(width = 6, radioButtons("Item_9", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("10. I feel I have to repeat certain numbers.")),
                                         column(width = 6, radioButtons("Item_10", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(style = "background-color: #ededed",
                                         column(width = 6, h4("11. I sometimes have to wash or clean myself simply because I feel contaminated.")),
                                         column(width = 6, radioButtons("Item_11", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("12. I am upset by unpleasant thoughts that come into my mind against my will.")),
                                         column(width = 6, radioButtons("Item_12", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(style = "background-color: #ededed",
                                         column(width = 6, h4("13. I avoid throwing things away because I am afraid I might need them later.")),
                                         column(width = 6, radioButtons("Item_13", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("14. I repeatedly check gas and water taps and light switches after turning them off.")),
                                         column(width = 6, radioButtons("Item_14", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(style = "background-color: #ededed",
                                         column(width = 6, h4("15. I need things to be arranged in a particular way.")),
                                         column(width = 6, radioButtons("Item_15", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("16. I feel that there are good and bad numbers.")),
                                         column(width = 6, radioButtons("Item_16", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(style = "background-color: #ededed",
                                         column(width = 6, h4("17. I wash my hands more often and longer than necessary.")),
                                         column(width = 6, radioButtons("Item_17", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("18. I frequently get nasty thoughts and have difficulty in getting rid of them.")),
                                         column(width = 6, radioButtons("Item_18", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         hr(),
                                         fluidRow(
                                           column(width = 4, dateInput("Q_Date", "Date",  format = "dd/mm/yyyy")),
                                           column(width = 4, textInput("Q_Name", "Name")),
                                           column(width = 4, textInput("Q_Clin_Name", "Clinician's Name"))
                                         ),
                                         fluidRow(
                                           column(width = 12, h5("Scale Source: Foa, E.B., Huppert, J.D., Leiberg, S., Hajcak, G., Langner, R., et al. (2002). The Obsessive Compulsive Inventory: Development and validation of a short version. Psychological Assessment,
                                                                 14, 485-496."))
                                         )
                                       )
                                       
                             )),
                    tabPanel("Enter Data",
                             fluidRow(
                               column(width = 12,
                                      titlePanel(span(tagList(icon("edit", lib = "font-awesome")), h4(tags$b("Enter Scores for Each Timepoint")),
                                                      tags$ul(
                                                        tags$li(helpText(h5(tags$em(tags$b("Use commas to separate scores. Enter scores in order, from the first to the last item of the total scale.", style = "color:black")))))
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
                                      textInput('Text_2', 'Time 2 Scores', "0,1,2,etc"),
                                      textInput('Text_3', 'Time 3 Scores', "0,1,2,etc")
                                      
                               ),
                               
                               
                               
                               column(4,
                                      dateInput("Date_1", "Date of 1st Timepoint", format = "dd/mm/yyyy"),
                                      dateInput("Date_2", "Date of 2nd Timepoint", format = "dd/mm/yyyy"),
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
                                                      selectInput("Pop", "", choices = c("OCD", "University Student"))
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
                                                               selectInput("Select_CI", label = "OCI-R total scale",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI == '2'",
                                                                                numericInput("Man_CI", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Washing", label = "Washing",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Washing == '2'",
                                                                                numericInput("Man_CI_Washing", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Checking", label = "Checking",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Checking == '2'",
                                                                                numericInput("Man_CI_Checking", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Ordering", label = "Ordering",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Ordering == '2'",
                                                                                numericInput("Man_CI_Ordering", "Specify the width of the confidence interval", value = 0))
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               selectInput("Select_CI_Obsessing", label = "Obsessing",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Obsessing == '2'",
                                                                                numericInput("Man_CI_Obsessing", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Hoarding", label = "Hoarding",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Hoarding == '2'",
                                                                                numericInput("Man_CI_Hoarding", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Neutralizing", label = "Neutralizing",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Neutralizing == '2'",
                                                                                numericInput("Man_CI_Neutralizing", "Specify the width of the confidence interval", value = 0))
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
                                                               uiOutput("Mean_Widg_Washing")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Checking")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Ordering")
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Obsessing")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Hoarding")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Neutralizing")
                                                        )
                                                      ),
                                                      
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Chelune et al. (1993)' || input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)' || input.RelChangeMethod == 'Speer (1992)'",
                                                                       
                                                                       h4(tags$strong("Enter a mean value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean", "OCI-R total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Washing", "Washing", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Checking", "Checking", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Ordering", "Ordering", value = 0)
                                                                         )
                                                                       ),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Obsessing", "Obsessing", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Hoarding", "Hoarding", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Neutralizing", "Neutralizing", value = 0)
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
                                                               uiOutput("Sd_Widg_Washing")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Checking")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Ordering")
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Obsessing")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Hoarding")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Neutralizing")
                                                        )
                                                      ),
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)'",
                                                                       
                                                                       h4(tags$strong("Enter a standard deviation value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd", "OCI-R total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Washing", "Washing", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Checking", "Checking", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Ordering", "Ordering", value = 0)
                                                                         )
                                                                       ),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Obsessing", "Obsessing", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Hoarding", "Hoarding", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Neutralizing", "Neutralizing", value = 0)
                                                                         )
                                                                       )
                                                                       
                                                      )
                                             ),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      h4(tags$strong("Enter test-retest reliability values")), 
                                                      h6("* No test-retest reliability data was available for subscales. Reported subscale reliability values represent internal consistency (Kuder Richardson Formula 20; KR-20)."),
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability", "OCI-R total scale", value = .92),
                                                               h6("Chanen, Jovev, Yuen & Rawlings (2008)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Washing", "Washing", value = .64),
                                                               h6("Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Checking", "Checking", value = .89),
                                                               h6("Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Ordering", "Ordering", value = .67),
                                                               h6("Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)")
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability_Obsessing", "Obsessing", value = .79),
                                                               h6("Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Hoarding", "Hoarding", value = .77),
                                                               h6("Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Neutralizing", "Neutralizing", value = .81),
                                                               h6("Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)")
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
                                                               uiOutput("Cutoff_Widg_Washing_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Checking_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Ordering_1") 
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Obsessing_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Hoarding_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Neutralizing_1") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Second cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Washing_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Checking_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Ordering_2") 
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Obsessing_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Hoarding_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Neutralizing_2") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Third cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Washing_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Checking_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Ordering_3") 
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Obsessing_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Hoarding_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Neutralizing_3") 
                                                        )
                                                        
                                                      )
                                                      , hr()
                                                      
                                             ),
                                             
                                             
                                             h4(tags$strong("Psychometric Properties of the OCI-R Relevant to Assessing Reliable & Clinically Significant Change")),
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
                      input$Item_17, input$Item_18, sep = ",")
    
    Q_Date<- input$Q_Date
    Q_Name<- input$Q_Name
    Q_Clin_Name<- input$Q_Clin_Name
    
    updateTextInput(session, "Text_1", value = Q_Scores)
    updateDateInput(session, "Date_1", value = Q_Date)
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
    
    if(input$Pop == "University Student") {
      Mean_Val<<-18.82
      Sd_Val<<- 11.1
      Source_Mean<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Sd<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Cut_Val_1<<- Mean_Val
      Cut_Val_2<<- 21
      Cut_Val_3<<- Mean_Val + (1*Sd_Val)
      Cut_Lab_1<<- "Mean"
      Cut_Lab_2<<- "OCD Cut-Off (Scale Developers)"
      Cut_Lab_3<<- "Mean + 1 Sd"
      Source_Cutoff_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_3<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Mean_Val_Washing<<-2.41
      Sd_Val_Washing<<-2.5
      Cut_Val_Washing_1<<- Mean_Val_Washing 
      Cut_Val_Washing_2<<- Mean_Val_Washing + Sd_Val_Washing
      Cut_Val_Washing_3<<- Mean_Val_Washing + (2*Sd_Val_Washing)
      Cut_Lab_Washing_1<<- "Mean"
      Cut_Lab_Washing_2<<- "Mean + 1 SD"
      Cut_Lab_Washing_3<<- "Mean + 2 SD"
      Source_Cutoff_Washing_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Washing_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Washing_3<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Mean_Val_Checking<<- 2.91
      Sd_Val_Checking<<- 2.56
      Cut_Val_Checking_1<<- Mean_Val_Checking 
      Cut_Val_Checking_2<<- Mean_Val_Checking + Sd_Val_Checking
      Cut_Val_Checking_3<<- Mean_Val_Checking + (2*Sd_Val_Checking)
      Cut_Lab_Checking_1<<- "Mean"
      Cut_Lab_Checking_2<<- "Mean + 1 SD"
      Cut_Lab_Checking_3<<- "Mean + 2 SD"
      Source_Cutoff_Checking_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Checking_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Checking_3<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Mean_Val_Ordering<<- 4.4
      Sd_Val_Ordering<<- 3.03
      Cut_Val_Ordering_1<<- Mean_Val_Ordering 
      Cut_Val_Ordering_2<<- Mean_Val_Ordering + Sd_Val_Ordering
      Cut_Val_Ordering_3<<- Mean_Val_Ordering + (2*Sd_Val_Ordering)
      Cut_Lab_Ordering_1<<- "Mean"
      Cut_Lab_Ordering_2<<- "Mean + 1 SD"
      Cut_Lab_Ordering_3<<- "Mean + 2 SD"
      Source_Cutoff_Ordering_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Ordering_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Ordering_3<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Mean_Val_Obsessing<<- 2.86
      Sd_Val_Obsessing<<- 2.72
      Source_Mean_Obsessing<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Sd_Obsessing<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Cut_Val_Obsessing_1<<- Mean_Val_Obsessing
      Cut_Val_Obsessing_2<<- Mean_Val_Obsessing + Sd_Val_Obsessing
      Cut_Val_Obsessing_3<<- Mean_Val_Obsessing + (2*Sd_Val_Obsessing)
      Cut_Lab_Obsessing_1<<- "Mean"
      Cut_Lab_Obsessing_2<<- "Mean + 1 SD"
      Cut_Lab_Obsessing_3<<- "Mean + 2 SD"
      Source_Cutoff_Obsessing_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Obsessing_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Obsessing_3<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Mean_Val_Hoarding<<- 4.41
      Sd_Val_Hoarding<<- 2.67
      Cut_Val_Hoarding_1<<- Mean_Val_Hoarding 
      Cut_Val_Hoarding_2<<- Mean_Val_Hoarding + Sd_Val_Hoarding
      Cut_Val_Hoarding_3<<- 6
      Cut_Lab_Hoarding_1<<- "Mean"
      Cut_Lab_Hoarding_2<<- "Mean + 1 SD"
      Cut_Lab_Hoarding_3<<- "Hoarding Disorder Cut-off (Scale Developers)"
      Source_Cutoff_Hoarding_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Hoarding_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Hoarding_3<<- "Wooton, Bragdon, Frost, Diefenbach, Steketee et al (2015)"
      Mean_Val_Neutralizing<<- 1.82
      Sd_Val_Neutralizing<<- 2.2
      Cut_Val_Neutralizing_1<<- Mean_Val_Neutralizing 
      Cut_Val_Neutralizing_2<<- Mean_Val_Neutralizing + Sd_Val_Neutralizing
      Cut_Val_Neutralizing_3<<- Mean_Val_Neutralizing + (2*Sd_Val_Neutralizing)
      Cut_Lab_Neutralizing_1<<- "Mean"
      Cut_Lab_Neutralizing_2<<- "Mean + 1 SD"
      Cut_Lab_Neutralizing_3<<- "Mean + 2 SD"
      Source_Cutoff_Neutralizing_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Neutralizing_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Neutralizing_3<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
    } else if(input$Pop == "OCD") {
      Mean_Val<<-28.01
      Sd_Val<<- 13.53
      Source_Mean<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Sd<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Cut_Val_1<<- 18.82
      Cut_Val_2<<- 21
      Cut_Val_3<<- Mean_Val 
      Cut_Lab_1<<- "Mean (Student Sample)"
      Cut_Lab_2<<- "OCD Cut-Off (Scale Developers)"
      Cut_Lab_3<<- "Mean (OCD Sample) + 1 Sd"
      Source_Cutoff_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_3<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Mean_Val_Washing<<- 4.35
      Sd_Val_Washing<<- 4.31
      Cut_Val_Washing_1<<- 2.41 
      Cut_Val_Washing_2<<- Mean_Val_Washing 
      Cut_Val_Washing_3<<- Mean_Val_Washing + Sd_Val_Washing
      Cut_Lab_Washing_1<<- "Mean (Student Sample)"
      Cut_Lab_Washing_2<<- "Mean (OCD Sample)"
      Cut_Lab_Washing_3<<- "Mean (OCD Sample) + 1 SD"
      Source_Cutoff_Washing_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Washing_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Washing_3<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Mean_Val_Checking<<- 4.83
      Sd_Val_Checking<<- 3.86
      Cut_Val_Checking_1<<- 2.91
      Cut_Val_Checking_2<<- Mean_Val_Checking 
      Cut_Val_Checking_3<<- Mean_Val_Checking + Sd_Val_Checking
      Cut_Lab_Checking_1<<- "Mean (Student Sample)"
      Cut_Lab_Checking_2<<- "Mean (OCD Sample)"
      Cut_Lab_Checking_3<<- "Mean (OCD Sample) + 1 SD"
      Source_Cutoff_Checking_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Checking_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Checking_3<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Mean_Val_Ordering<<- 4.76
      Sd_Val_Ordering<<- 4
      Cut_Val_Ordering_1<<- 4.4 
      Cut_Val_Ordering_2<<- Mean_Val_Ordering 
      Cut_Val_Ordering_3<<- Mean_Val_Ordering + Sd_Val_Ordering
      Cut_Lab_Ordering_1<<- "Mean (Student Sample)"
      Cut_Lab_Ordering_2<<- "Mean (OCD Sample)"
      Cut_Lab_Ordering_3<<- "Mean (OCD Sample) + 1 SD"
      Source_Cutoff_Ordering_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Ordering_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Ordering_3<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Mean_Val_Obsessing<<- 7.23
      Sd_Val_Obsessing<<- 3.84
      Source_Mean_Obsessing<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Sd_Obsessing<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Cut_Val_Obsessing_1<<- 2.86
      Cut_Val_Obsessing_2<<- Mean_Val_Obsessing 
      Cut_Val_Obsessing_3<<- Mean_Val_Obsessing + Sd_Val_Obsessing
      Cut_Lab_Obsessing_1<<- "Mean (Student Sample)"
      Cut_Lab_Obsessing_2<<- "Mean (OCD Sample)"
      Cut_Lab_Obsessing_3<<- "Mean (OCD Sample) + 1 SD"
      Source_Cutoff_Obsessing_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Obsessing_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Obsessing_3<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Mean_Val_Hoarding<<- 3.67
      Sd_Val_Hoarding<<- 3.87
      Cut_Val_Hoarding_1<<- 4.41
      Cut_Val_Hoarding_2<<- Mean_Val_Hoarding 
      Cut_Val_Hoarding_3<<- 6
      Cut_Lab_Hoarding_1<<- "Mean (Student Sample)"
      Cut_Lab_Hoarding_2<<- "Mean (OCD Sample)"
      Cut_Lab_Hoarding_3<<- "Hoarding Disorder Cut-off (Wooton et al., 2015)"
      Source_Cutoff_Hoarding_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Hoarding_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Hoarding_3<<- "Wooton, Bragdon, Frost, Diefenbach, Steketee et al (2015)"
      Mean_Val_Neutralizing<<- 3.18
      Sd_Val_Neutralizing<<- 3.81
      Cut_Val_Neutralizing_1<<- 1.82
      Cut_Val_Neutralizing_2<<- Mean_Val_Neutralizing
      Cut_Val_Neutralizing_3<<- Mean_Val_Neutralizing + Sd_Val_Neutralizing
      Cut_Lab_Neutralizing_1<<- "Mean (Student Sample)"
      Cut_Lab_Neutralizing_2<<- "Mean (OCD Sample)"
      Cut_Lab_Neutralizing_3<<- "Mean (OCD Sample) + 1 SD"
      Source_Cutoff_Neutralizing_1<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Neutralizing_2<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
      Source_Cutoff_Neutralizing_3<<- "Foa, Huppert, Leiberg, Langner, Kichic et al. (2002)"
    } 
    
  })
  
  
  output$Mean_Widg<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean", "OCI-R total scale", Mean_Val),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Washing<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Washing", "Washing", Mean_Val_Washing),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Washing", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Checking<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Checking", "Checking", Mean_Val_Checking),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Checking", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Ordering<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Ordering", "Ordering", Mean_Val_Ordering),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Ordering", suspendWhenHidden = FALSE) 
  
  

  output$Mean_Widg_Obsessing<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Obsessing", "Obsessing", Mean_Val_Obsessing),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Obsessing", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Hoarding<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Hoarding", "Hoarding", Mean_Val_Hoarding),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Hoarding", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Neutralizing<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Neutralizing", "Neutralizing", Mean_Val_Neutralizing),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Neutralizing", suspendWhenHidden = FALSE) 
  
  
  
  output$Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd", "OCI-R total scale", Sd_Val),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Washing<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Washing", "Washing", Sd_Val_Washing),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Washing", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Checking<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Checking", "Checking", Sd_Val_Checking),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Checking", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Ordering<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Ordering", "Ordering", Sd_Val_Ordering),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Ordering", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Obsessing<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Obsessing", "Obsessing", Sd_Val_Obsessing),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Obsessing", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Hoarding<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Hoarding", "Hoarding", Sd_Val_Hoarding),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Hoarding", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Neutralizing<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Neutralizing", "Neutralizing", Sd_Val_Neutralizing),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Neutralizing", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_1", "OCI-R total scale", as.numeric(Cut_Val_1)),
      textInput("Cutoff_Text_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source_Cutoff_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Washing_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Washing_1", "Washing", as.numeric(Cut_Val_Washing_1)),
      textInput("Cutoff_Text_Washing_1", "Cut-Off Score Name", Cut_Lab_Washing_1),
      h6(paste("Reference:", Source_Cutoff_Washing_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Washing_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Checking_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Checking_1", "Checking", as.numeric(Cut_Val_Checking_1)),
      textInput("Cutoff_Text_Checking_1", "Cut-Off Score Name", Cut_Lab_Checking_1),
      h6(paste("Reference:", Source_Cutoff_Checking_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Checking_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Ordering_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Ordering_1", "Ordering", as.numeric(Cut_Val_Ordering_1)),
      textInput("Cutoff_Text_Ordering_1", "Cut-Off Score Name", Cut_Lab_Ordering_1),
      h6(paste("Reference:", Source_Cutoff_Ordering_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Ordering_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Obsessing_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Obsessing_1", "Obsessing", as.numeric(Cut_Val_Obsessing_1)),
      textInput("Cutoff_Text_Obsessing_1", "Cut-Off Score Name", Cut_Lab_Obsessing_1),
      h6(paste("Reference:", Source_Cutoff_Obsessing_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Obsessing_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Hoarding_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Hoarding_1", "Hoarding", as.numeric(Cut_Val_Hoarding_1)),
      textInput("Cutoff_Text_Hoarding_1", "Cut-Off Score Name", Cut_Lab_Hoarding_1),
      h6(paste("Reference:", Source_Cutoff_Hoarding_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Hoarding_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Neutralizing_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Neutralizing_1", "Neutralizing", as.numeric(Cut_Val_Neutralizing_1)),
      textInput("Cutoff_Text_Neutralizing_1", "Cut-Off Score Name", Cut_Lab_Neutralizing_1),
      h6(paste("Reference:", Source_Cutoff_Neutralizing_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Neutralizing_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_2", "OCI-R total scale", as.numeric(Cut_Val_2)),
      textInput("Cutoff_Text_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source_Cutoff_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Washing_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Washing_2", "Washing", as.numeric(Cut_Val_Washing_2)),
      textInput("Cutoff_Text_Washing_2", "Cut-Off Score Name", Cut_Lab_Washing_2),
      h6(paste("Reference:", Source_Cutoff_Washing_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Washing_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Checking_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Checking_2", "Checking", as.numeric(Cut_Val_Checking_2)),
      textInput("Cutoff_Text_Checking_2", "Cut-Off Score Name", Cut_Lab_Checking_2),
      h6(paste("Reference:", Source_Cutoff_Checking_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Checking_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Ordering_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Ordering_2", "Ordering", as.numeric(Cut_Val_Ordering_2)),
      textInput("Cutoff_Text_Ordering_2", "Cut-Off Score Name", Cut_Lab_Ordering_2),
      h6(paste("Reference:", Source_Cutoff_Ordering_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Ordering_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Obsessing_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Obsessing_2", "Obsessing", as.numeric(Cut_Val_Obsessing_2)),
      textInput("Cutoff_Text_Obsessing_2", "Cut-Off Score Name", Cut_Lab_Obsessing_2),
      h6(paste("Reference:", Source_Cutoff_Obsessing_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Obsessing_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Hoarding_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Hoarding_2", "Hoarding", as.numeric(Cut_Val_Hoarding_2)),
      textInput("Cutoff_Text_Hoarding_2", "Cut-Off Score Name", Cut_Lab_Hoarding_2),
      h6(paste("Reference:", Source_Cutoff_Hoarding_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Hoarding_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Neutralizing_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Neutralizing_2", "Neutralizing", as.numeric(Cut_Val_Neutralizing_2)),
      textInput("Cutoff_Text_Neutralizing_2", "Cut-Off Score Name", Cut_Lab_Neutralizing_2),
      h6(paste("Reference:", Source_Cutoff_Neutralizing_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Neutralizing_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_3", "OCI-R total scale", as.numeric(Cut_Val_3)),
      textInput("Cutoff_Text_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source_Cutoff_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Washing_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Washing_3", "Washing", as.numeric(Cut_Val_Washing_3)),
      textInput("Cutoff_Text_Washing_3", "Cut-Off Score Name", Cut_Lab_Washing_3),
      h6(paste("Reference:", Source_Cutoff_Washing_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Washing_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Checking_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Checking_3", "Checking", as.numeric(Cut_Val_Checking_3)),
      textInput("Cutoff_Text_Checking_3", "Cut-Off Score Name", Cut_Lab_Checking_3),
      h6(paste("Reference:", Source_Cutoff_Checking_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Checking_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Ordering_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Ordering_3", "Ordering", as.numeric(Cut_Val_Ordering_3)),
      textInput("Cutoff_Text_Ordering_3", "Cut-Off Score Name", Cut_Lab_Ordering_3),
      h6(paste("Reference:", Source_Cutoff_Ordering_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Ordering_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Obsessing_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Obsessing_3", "Obsessing", as.numeric(Cut_Val_Obsessing_3)),
      textInput("Cutoff_Text_Obsessing_3", "Cut-Off Score Name", Cut_Lab_Obsessing_3),
      h6(paste("Reference:", Source_Cutoff_Obsessing_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Obsessing_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Hoarding_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Hoarding_3", "Hoarding", as.numeric(Cut_Val_Hoarding_3)),
      textInput("Cutoff_Text_Hoarding_3", "Cut-Off Score Name", Cut_Lab_Hoarding_3),
      h6(paste("Reference:", Source_Cutoff_Hoarding_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Hoarding_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Neutralizing_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Neutralizing_3", "Neutralizing", as.numeric(Cut_Val_Neutralizing_3)),
      textInput("Cutoff_Text_Neutralizing_3", "Cut-Off Score Name", Cut_Lab_Neutralizing_3),
      h6(paste("Reference:", Source_Cutoff_Neutralizing_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Neutralizing_3", suspendWhenHidden = FALSE)
  
  
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
    M_Washing<- input$Pop_Mean_Washing
    SD_Washing<-input$Pop_Sd_Washing
    M_Checking<- input$Pop_Mean_Checking
    SD_Checking<- input$Pop_Sd_Checking
    M_Ordering<- input$Pop_Mean_Ordering
    SD_Ordering<- input$Pop_Sd_Ordering
    M_Obsessing<- input$Pop_Mean_Obsessing
    SD_Obsessing<-input$Pop_Sd_Obsessing
    M_Hoarding<- input$Pop_Mean_Hoarding
    SD_Hoarding<- input$Pop_Sd_Hoarding
    M_Neutralizing<- input$Pop_Mean_Neutralizing
    SD_Neutralizing<- input$Pop_Sd_Neutralizing
   
    M_Retest<- input$Retest_Mean
    SD_Retest<- input$Retest_Sd
    M_Retest_Washing<- input$Retest_Mean_Washing
    SD_Retest_Washing<- input$Retest_Sd_Washing
    M_Retest_Checking<- input$Retest_Mean_Checking
    SD_Retest_Checking<- input$Retest_Sd_Checking
    M_Retest_Ordering<- input$Retest_Mean_Ordering
    SD_Retest_Ordering<- input$Retest_Sd_Ordering
    M_Retest_Obsessing<- input$Retest_Mean_Obsessing
    SD_Retest_Obsessing<- input$Retest_Sd_Obsessing
    M_Retest_Hoarding<- input$Retest_Mean_Hoarding
    SD_Retest_Hoarding<- input$Retest_Sd_Hoarding
    M_Retest_Neutralizing<- input$Retest_Mean_Neutralizing
    SD_Retest_Neutralizing<- input$Retest_Sd_Neutralizing
    
    SampleN<- input$SampleN
    
    Rel<- input$Reliability
    Rel_Washing<- input$Reliability_Washing
    Rel_Checking<- input$Reliability_Checking
    Rel_Ordering<- input$Reliability_Ordering
    Rel_Obsessing<- input$Reliability_Obsessing
    Rel_Hoarding<- input$Reliability_Hoarding
    Rel_Neutralizing<- input$Reliability_Neutralizing
   
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
      SE_Washing<-SD_Washing * sqrt(1 - Rel_Washing^2)
      SE_Checking<-SD_Checking * sqrt(1 - Rel_Checking^2)
      SE_Ordering<-SD_Ordering * sqrt(1 - Rel_Ordering^2)
      SE<- round(SE, digits = 2)
      SE_Washing<- round(SE_Washing, digits = 2)
      SE_Checking<- round(SE_Checking, digits = 2)
      SE_Ordering<- round(SE_Ordering, digits = 2)
      SE_Obsessing<-SD_Obsessing * sqrt(1 - Rel_Obsessing^2)
      SE_Hoarding<-SD_Hoarding * sqrt(1 - Rel_Hoarding^2)
      SE_Neutralizing<-SD_Neutralizing * sqrt(1 - Rel_Neutralizing^2)
      SE_Obsessing<- round(SE_Obsessing, digits = 2)
      SE_Hoarding<- round(SE_Hoarding, digits = 2)
      SE_Neutralizing<- round(SE_Neutralizing, digits = 2)
    } else if (input$RelChangeMethod == "Chelune et al. (1993)" | input$RelChangeMethod == "Jacobson & Truax (1991)" | input$RelChangeMethod == "Speer (1992)") {
      SE<- sqrt((2*(SD^2))*(1-Rel))
      SE_Washing<- sqrt((2*(SD_Washing^2))*(1-Rel_Washing))
      SE_Checking<- sqrt((2*(SD_Checking^2))*(1-Rel_Checking))
      SE_Ordering<- sqrt((2*(SD_Ordering^2))*(1-Rel_Ordering))
      SE_Obsessing<- sqrt((2*(SD_Obsessing^2))*(1-Rel_Obsessing))
      SE_Hoarding<- sqrt((2*(SD_Hoarding^2))*(1-Rel_Hoarding))
      SE_Neutralizing<- sqrt((2*(SD_Neutralizing^2))*(1-Rel_Neutralizing))
      SE<- round(SE, digits = 2)
      SE_Washing<- round(SE_Washing, digits = 2)
      SE_Checking<- round(SE_Checking, digits = 2)
      SE_Ordering<- round(SE_Ordering, digits = 2)
      SE_Obsessing<- round(SE_Obsessing, digits = 2)
      SE_Hoarding<- round(SE_Hoarding, digits = 2)
      SE_Neutralizing<- round(SE_Neutralizing, digits = 2)
    } else if (input$RelChangeMethod == "Maassen et al. (2006)") {
      SE<- sqrt((SD^2 + SD_Retest^2)*(1-Rel))
      SE_Washing<- sqrt((SD_Washing^2 + SD_Retest_Washing^2)*(1-Rel_Washing))
      SE_Checking<- sqrt((SD_Checking^2 + SD_Retest_Checking^2)*(1-Rel_Checking))
      SE_Ordering<- sqrt((SD_Ordering^2 + SD_Retest_Ordering^2)*(1-Rel_Ordering))
      SE<- round(SE, digits = 2)
      SE_Washing<- round(SE_Washing, digits = 2)
      SE_Checking<- round(SE_Checking, digits = 2)
      SE_Ordering<- round(SE_Ordering, digits = 2)
      SE_Obsessing<- sqrt((SD_Obsessing^2 + SD_Retest_Obsessing^2)*(1-Rel_Obsessing))
      SE_Hoarding<- sqrt((SD_Hoarding^2 + SD_Retest_Hoarding^2)*(1-Rel_Hoarding))
      SE_Neutralizing<- sqrt((SD_Neutralizing^2 + SD_Retest_Neutralizing^2)*(1-Rel_Neutralizing))
      SE<- round(SE, digits = 2)
      SE_Obsessing<- round(SE_Obsessing, digits = 2)
      SE_Hoarding<- round(SE_Hoarding, digits = 2)
      SE_Neutralizing<- round(SE_Neutralizing, digits = 2)
    } else if (input$RelChangeMethod == "McSweeny et al. (1993)") {
      SE<- SD_Retest*sqrt(1 - Rel^2)
      SE_Washing<- SD_Retest_Washing*sqrt(1 - Rel_Washing^2)
      SE_Checking<- SD_Retest_Checking*sqrt(1 - Rel_Checking^2)
      SE_Ordering<- SD_Retest_Ordering*sqrt(1 - Rel_Ordering^2)
      SE<- round(SE, digits = 2)
      SE_Washing<- round(SE_Washing, digits = 2)
      SE_Checking<- round(SE_Checking, digits = 2)
      SE_Ordering<- round(SE_Ordering, digits = 2)
      SE_Obsessing<- SD_Retest_Obsessing*sqrt(1 - Rel_Obsessing^2)
      SE_Hoarding<- SD_Retest_Hoarding*sqrt(1 - Rel_Hoarding^2)
      SE_Neutralizing<- SD_Retest_Neutralizing*sqrt(1 - Rel_Neutralizing^2)
      SE<- round(SE, digits = 2)
      SE_Obsessing<- round(SE_Obsessing, digits = 2)
      SE_Hoarding<- round(SE_Hoarding, digits = 2)
      SE_Neutralizing<- round(SE_Neutralizing, digits = 2)
    }
    
    #Caclculate McSweeny standard error to be employed in deriving the Crawford & Howell standard error if selected
    
    McSweeny_SE<- SD_Retest*sqrt(1 - Rel^2)
    McSweeny_SE_Washing<- SD_Retest_Washing*sqrt(1 - Rel_Washing^2)
    McSweeny_SE_Checking<- SD_Retest_Checking*sqrt(1 - Rel_Checking^2)
    McSweeny_SE_Ordering<- SD_Retest_Ordering*sqrt(1 - Rel_Ordering^2)
    McSweeny_SE_Obsessing<- SD_Retest_Obsessing*sqrt(1 - Rel_Obsessing^2)
    McSweeny_SE_Hoarding<- SD_Retest_Hoarding*sqrt(1 - Rel_Hoarding^2)
    McSweeny_SE_Neutralizing<- SD_Retest_Neutralizing*sqrt(1 - Rel_Neutralizing^2)
    
    #Generate object storing names of cut-off scores
    
    Cutoff_Name_1<- input$Cutoff_Text_1
    Cutoff_Name_2<- input$Cutoff_Text_2
    Cutoff_Name_3<- input$Cutoff_Text_3
    Cutoff_Name_Washing_1<- input$Cutoff_Text_Washing_1
    Cutoff_Name_Washing_2<- input$Cutoff_Text_Washing_2
    Cutoff_Name_Washing_3<- input$Cutoff_Text_Washing_3
    Cutoff_Name_Checking_1<- input$Cutoff_Text_Checking_1
    Cutoff_Name_Checking_2<- input$Cutoff_Text_Checking_2
    Cutoff_Name_Checking_3<- input$Cutoff_Text_Checking_3
    Cutoff_Name_Ordering_1<- input$Cutoff_Text_Ordering_1
    Cutoff_Name_Ordering_2<- input$Cutoff_Text_Ordering_2
    Cutoff_Name_Ordering_3<- input$Cutoff_Text_Ordering_3
    Cutoff_Name_Obsessing_1<- input$Cutoff_Text_Obsessing_1
    Cutoff_Name_Obsessing_2<- input$Cutoff_Text_Obsessing_2
    Cutoff_Name_Obsessing_3<- input$Cutoff_Text_Obsessing_3
    Cutoff_Name_Hoarding_1<- input$Cutoff_Text_Hoarding_1
    Cutoff_Name_Hoarding_2<- input$Cutoff_Text_Hoarding_2
    Cutoff_Name_Hoarding_3<- input$Cutoff_Text_Hoarding_3
    Cutoff_Name_Neutralizing_1<- input$Cutoff_Text_Neutralizing_1
    Cutoff_Name_Neutralizing_2<- input$Cutoff_Text_Neutralizing_2
    Cutoff_Name_Neutralizing_3<- input$Cutoff_Text_Neutralizing_3
   
    
    Cutoff_Names<<- data.frame(Cutoff_Name_1,Cutoff_Name_2,Cutoff_Name_3,Cutoff_Name_Washing_1,Cutoff_Name_Washing_2,Cutoff_Name_Washing_3,
                               Cutoff_Name_Checking_1, Cutoff_Name_Checking_2, Cutoff_Name_Checking_3, Cutoff_Name_Ordering_1,
                               Cutoff_Name_Ordering_2, Cutoff_Name_Ordering_3,
                               Cutoff_Name_Obsessing_1,Cutoff_Name_Obsessing_2,Cutoff_Name_Obsessing_3,
                               Cutoff_Name_Hoarding_1, Cutoff_Name_Hoarding_2, Cutoff_Name_Hoarding_3, Cutoff_Name_Neutralizing_1,
                               Cutoff_Name_Neutralizing_2, Cutoff_Name_Neutralizing_3
                               )
    
    #Define the data & calculate PS, SE, confidence intervals and cut-off scores
    
    if(input$Timepoint == "1") {
      Date<- input$Date_1
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_1a), Score = Score_1a)
      Score<- sum(Score_1a, na.rm = TRUE)
      Score<- round(Score, digits = 2)
      Score_Washing<- sum(Score_1a[c(5,11,17)], na.rm = TRUE)
      Score_Checking<- sum(Score_1a[c(2,8,14)], na.rm = TRUE)
      Score_Ordering<- sum(Score_1a[c(3,9,15)], na.rm = TRUE)
      Score_Obsessing<- sum(Score_1a[c(6,12,18)], na.rm = TRUE)
      Score_Hoarding<- sum(Score_1a[c(1,7,13)], na.rm = TRUE)
      Score_Neutralizing<- sum(Score_1a[c(4,10,16)], na.rm = TRUE)
      Change<- 0
      Change_Washing<- 0
      Change_Checking<- 0
      Change_Ordering<- 0
      Change_Obsessing<- 0
      Change_Hoarding<- 0
      Change_Neutralizing<- 0
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS<- (Rel * Score) + (M * (1 - Rel))
        PTS_Washing<- (Rel_Washing * Score_Washing) + (M_Washing * (1 - Rel_Washing))
        PTS_Checking<- (Rel_Checking * Score_Checking) + (M_Checking * (1 - Rel_Checking))
        PTS_Ordering<- (Rel_Ordering * Score_Ordering) + (M_Ordering * (1 - Rel_Ordering))
        PTS_Obsessing<- (Rel_Obsessing * Score_Obsessing) + (M_Obsessing * (1 - Rel_Obsessing))
        PTS_Hoarding<- (Rel_Hoarding * Score_Hoarding) + (M_Hoarding * (1 - Rel_Hoarding))
        PTS_Neutralizing<- (Rel_Neutralizing * Score_Neutralizing) + (M_Neutralizing * (1 - Rel_Neutralizing))
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS<- Score + (M_Retest - M)  
        PTS_Washing<- Score_Washing + (M_Retest_Washing - M_Washing)  
        PTS_Checking<- Score_Checking + (M_Retest_Checking - M_Checking)  
        PTS_Ordering<- Score_Ordering + (M_Retest_Ordering - M_Ordering) 
        PTS_Obsessing<- Score_Obsessing + (M_Retest_Obsessing - M_Obsessing)  
        PTS_Hoarding<- Score_Hoarding + (M_Retest_Hoarding - M_Hoarding)  
        PTS_Neutralizing<- Score_Neutralizing + (M_Retest_Neutralizing - M_Neutralizing) 
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Washing<- Score_Washing
        PTS_Checking<- Score_Checking
        PTS_Ordering<- Score_Ordering
        PTS_Obsessing<- Score_Obsessing
        PTS_Hoarding<- Score_Hoarding
        PTS_Neutralizing<- Score_Neutralizing
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS<- (B_Adj * Score) + A_Adj
        B_Slope_Washing<- Rel_Washing * (SD_Retest_Washing/SD_Washing)
        A_Constant_Washing<- M_Retest_Washing - (B_Slope_Washing * M_Washing)
        B_Adj_Washing<- SD_Retest_Washing/SD_Washing
        A_Adj_Washing<- M_Retest_Washing - (B_Adj_Washing * M_Washing)
        PTS_Washing<- (B_Adj_Washing * Score_Washing) + A_Adj_Washing
        B_Slope_Checking<- Rel_Checking * (SD_Retest_Checking/SD_Checking)
        A_Constant_Checking<- M_Retest_Checking - (B_Slope_Checking * M_Checking)
        B_Adj_Checking<- SD_Retest_Checking/SD_Checking
        A_Adj_Checking<- M_Retest_Checking - (B_Adj_Checking * M_Checking)
        PTS_Checking<- (B_Adj_Checking * Score_Checking) + A_Adj_Checking
        B_Slope_Ordering<- Rel_Ordering * (SD_Retest_Ordering/SD_Ordering)
        A_Constant_Ordering<- M_Retest_Ordering - (B_Slope_Ordering * M_Ordering)
        B_Adj_Ordering<- SD_Retest_Ordering/SD_Ordering
        A_Adj_Ordering<- M_Retest_Ordering - (B_Adj_Ordering * M_Ordering)
        PTS_Ordering<- (B_Adj_Ordering * Score_Ordering) + A_Adj_Ordering
        B_Slope_Obsessing<- Rel_Obsessing * (SD_Retest_Obsessing/SD_Obsessing)
        A_Constant_Obsessing<- M_Retest_Obsessing - (B_Slope_Obsessing * M_Obsessing)
        B_Adj_Obsessing<- SD_Retest_Obsessing/SD_Obsessing
        A_Adj_Obsessing<- M_Retest_Obsessing - (B_Adj_Obsessing * M_Obsessing)
        PTS_Obsessing<- (B_Adj_Obsessing * Score_Obsessing) + A_Adj_Obsessing
        B_Slope_Hoarding<- Rel_Hoarding * (SD_Retest_Hoarding/SD_Hoarding)
        A_Constant_Hoarding<- M_Retest_Hoarding - (B_Slope_Hoarding * M_Hoarding)
        B_Adj_Hoarding<- SD_Retest_Hoarding/SD_Hoarding
        A_Adj_Hoarding<- M_Retest_Hoarding - (B_Adj_Hoarding * M_Hoarding)
        PTS_Hoarding<- (B_Adj_Hoarding * Score_Hoarding) + A_Adj_Hoarding
        B_Slope_Neutralizing<- Rel_Neutralizing * (SD_Retest_Neutralizing/SD_Neutralizing)
        A_Constant_Neutralizing<- M_Retest_Neutralizing - (B_Slope_Neutralizing * M_Neutralizing)
        B_Adj_Neutralizing<- SD_Retest_Neutralizing/SD_Neutralizing
        A_Adj_Neutralizing<- M_Retest_Neutralizing - (B_Adj_Neutralizing * M_Neutralizing)
        PTS_Neutralizing<- (B_Adj_Neutralizing * Score_Neutralizing) + A_Adj_Neutralizing
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS<- B_Slope * Score
        B_Slope_Washing<- Rel_Washing * (SD_Retest_Washing/SD_Washing)
        PTS_Washing<- B_Slope_Washing * Score_Washing
        B_Slope_Checking<- Rel_Checking * (SD_Retest_Checking/SD_Checking)
        PTS_Checking<- B_Slope_Checking * Score_Checking
        B_Slope_Ordering<- Rel_Ordering * (SD_Retest_Ordering/SD_Ordering)
        PTS_Ordering<- B_Slope_Ordering * Score_Ordering
        B_Slope_Obsessing<- Rel_Obsessing * (SD_Retest_Obsessing/SD_Obsessing)
        PTS_Obsessing<- B_Slope_Obsessing * Score_Obsessing
        B_Slope_Hoarding<- Rel_Hoarding * (SD_Retest_Hoarding/SD_Hoarding)
        PTS_Hoarding<- B_Slope_Hoarding * Score_Hoarding
        B_Slope_Neutralizing<- Rel_Neutralizing * (SD_Retest_Neutralizing/SD_Neutralizing)
        PTS_Neutralizing<- B_Slope_Neutralizing * Score_Neutralizing
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS<- Score + (M_Retest - M)
        PTS_Washing<- Score_Washing + (M_Retest_Washing - M_Washing)
        PTS_Checking<- Score_Checking + (M_Retest_Checking - M_Checking)
        PTS_Ordering<- Score_Ordering + (M_Retest_Ordering - M_Ordering)
        PTS_Obsessing<- Score_Obsessing + (M_Retest_Obsessing - M_Obsessing)
        PTS_Hoarding<- Score_Hoarding + (M_Retest_Hoarding - M_Hoarding)
        PTS_Neutralizing<- Score_Neutralizing + (M_Retest_Neutralizing - M_Neutralizing)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Washing<- round(PTS_Washing, digits = 2)
      PTS_Checking<- round(PTS_Checking, digits = 2)
      PTS_Ordering<- round(PTS_Ordering, digits = 2)
      PTS_Obsessing<- round(PTS_Obsessing, digits = 2)
      PTS_Hoarding<- round(PTS_Hoarding, digits = 2)
      PTS_Neutralizing<- round(PTS_Neutralizing, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score - M)^2/(SD^2*(SampleN-1))))
        SE_Washing<- McSweeny_SE_Washing*sqrt(1 + (1/SampleN) + ((Score_Washing - M_Washing)^2/(SD_Washing^2*(SampleN-1))))
        SE_Checking<- McSweeny_SE_Checking*sqrt(1 + (1/SampleN) + ((Score_Checking - M_Checking)^2/(SD_Checking^2*(SampleN-1))))
        SE_Ordering<- McSweeny_SE_Ordering*sqrt(1 + (1/SampleN) + ((Score_Ordering - M_Ordering)^2/(SD_Ordering^2*(SampleN-1))))
        SE_Obsessing<- McSweeny_SE_Obsessing*sqrt(1 + (1/SampleN) + ((Score_Obsessing - M_Obsessing)^2/(SD_Obsessing^2*(SampleN-1))))
        SE_Hoarding<- McSweeny_SE_Hoarding*sqrt(1 + (1/SampleN) + ((Score_Hoarding - M_Hoarding)^2/(SD_Hoarding^2*(SampleN-1))))
        SE_Neutralizing<- McSweeny_SE_Neutralizing*sqrt(1 + (1/SampleN) + ((Score_Neutralizing - M_Neutralizing)^2/(SD_Neutralizing^2*(SampleN-1))))
        SE<- round(SE, digits = 2)
        SE_Washing<- round(SE_Washing, digits = 2)
        SE_Checking<- round(SE_Checking, digits = 2)
        SE_Ordering<- round(SE_Ordering, digits = 2)
        SE_Obsessing<- round(SE_Obsessing, digits = 2)
        SE_Hoarding<- round(SE_Hoarding, digits = 2)
        SE_Neutralizing<- round(SE_Neutralizing, digits = 2)
        CI<- (Conf*SE)
        CI<- round(CI, digits = 2)
        CI_Washing<- (Conf*SE_Washing)
        CI_Washing<- round(CI_Washing, digits = 2)
        CI_Checking<- (Conf*SE_Checking)
        CI_Checking<- round(CI_Checking, digits = 2)
        CI_Ordering<- (Conf*SE_Ordering)
        CI_Ordering<- round(CI_Ordering, digits = 2)
        CI_Obsessing<- (Conf*SE_Obsessing)
        CI_Obsessing<- round(CI_Obsessing, digits = 2)
        CI_Hoarding<- (Conf*SE_Hoarding)
        CI_Hoarding<- round(CI_Hoarding, digits = 2)
        CI_Neutralizing<- (Conf*SE_Neutralizing)
        CI_Neutralizing<- round(CI_Neutralizing, digits = 2)
      }
      CI<- (Conf*SE)
      CI<- round(CI, digits = 2)
      CI<- round(CI, digits = 2)
      CI_Washing<- (Conf*SE_Washing)
      CI_Washing<- round(CI_Washing, digits = 2)
      CI_Checking<- (Conf*SE_Checking)
      CI_Checking<- round(CI_Checking, digits = 2)
      CI_Ordering<- (Conf*SE_Ordering)
      CI_Ordering<- round(CI_Ordering, digits = 2)
      CI_Obsessing<- (Conf*SE_Obsessing)
      CI_Obsessing<- round(CI_Obsessing, digits = 2)
      CI_Hoarding<- (Conf*SE_Hoarding)
      CI_Hoarding<- round(CI_Hoarding, digits = 2)
      CI_Neutralizing<- (Conf*SE_Neutralizing)
      CI_Neutralizing<- round(CI_Neutralizing, digits = 2)
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Washing<- PTS_Washing + CI_Washing
      CI_Upper_Lim_Washing<- round(CI_Upper_Lim_Washing, digits = 2)
      CI_Lower_Lim_Washing<-PTS_Washing - CI_Washing
      CI_Lower_Lim_Washing<- round(CI_Lower_Lim_Washing, digits = 2)
      CI_Upper_Lim_Checking<- PTS_Checking + CI_Checking
      CI_Upper_Lim_Checking<- round(CI_Upper_Lim_Checking, digits = 2)
      CI_Lower_Lim_Checking<-PTS_Checking - CI_Checking
      CI_Lower_Lim_Checking<- round(CI_Lower_Lim_Checking, digits = 2)
      CI_Upper_Lim_Ordering<- PTS_Ordering + CI_Ordering
      CI_Upper_Lim_Ordering<- round(CI_Upper_Lim_Ordering, digits = 2)
      CI_Lower_Lim_Ordering<-PTS_Ordering - CI_Ordering
      CI_Lower_Lim_Ordering<- round(CI_Lower_Lim_Ordering, digits = 2)
      CI_Upper_Lim_Obsessing<- PTS_Obsessing + CI_Obsessing
      CI_Upper_Lim_Obsessing<- round(CI_Upper_Lim_Obsessing, digits = 2)
      CI_Lower_Lim_Obsessing<-PTS_Obsessing - CI_Obsessing
      CI_Lower_Lim_Obsessing<- round(CI_Lower_Lim_Obsessing, digits = 2)
      CI_Upper_Lim_Hoarding<- PTS_Hoarding + CI_Hoarding
      CI_Upper_Lim_Hoarding<- round(CI_Upper_Lim_Hoarding, digits = 2)
      CI_Lower_Lim_Hoarding<- PTS_Hoarding - CI_Hoarding
      CI_Lower_Lim_Hoarding<- round(CI_Lower_Lim_Hoarding, digits = 2)
      CI_Upper_Lim_Neutralizing<- PTS_Neutralizing + CI_Neutralizing
      CI_Upper_Lim_Neutralizing<- round(CI_Upper_Lim_Neutralizing, digits = 2)
      CI_Lower_Lim_Neutralizing<-PTS_Neutralizing - CI_Neutralizing
      CI_Lower_Lim_Neutralizing<- round(CI_Lower_Lim_Neutralizing, digits = 2)
      
      if(input$Select_CI == "2") {
        CI<- input$Man_CI
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Washing == "2") {
        CI_Washing<- input$Man_CI_Washing
        CI_Washing<- round(CI_Washing, digits = 2)
        CI_Upper_Lim_Washing<- Score_Washing + CI_Washing
        CI_Upper_Lim_Washing<- round(CI_Upper_Lim_Washing, digits = 2)
        CI_Lower_Lim_Washing<- Score_Washing - CI_Washing
        CI_Lower_Lim_Washing<- round(CI_Lower_Lim_Washing, digits = 2)
      }
      if(input$Select_CI_Checking == "2") {
        CI_Checking<- input$Man_CI_Checking
        CI_Checking<- round(CI_Checking, digits = 2)
        CI_Upper_Lim_Checking<- Score_Checking + CI_Checking
        CI_Upper_Lim_Checking<- round(CI_Upper_Lim_Checking, digits = 2)
        CI_Lower_Lim_Checking<- Score_Checking - CI_Checking
        CI_Lower_Lim_Checking<- round(CI_Lower_Lim_Checking, digits = 2)
      }
      if(input$Select_CI_Ordering == "2") {
        CI_Ordering<- input$Man_CI_Ordering
        CI_Ordering<- round(CI_Ordering, digits = 2)
        CI_Upper_Lim_Ordering<- Score_Ordering + CI_Ordering
        CI_Upper_Lim_Ordering<- round(CI_Upper_Lim_Ordering, digits = 2)
        CI_Lower_Lim_Ordering<- Score_Ordering - CI_Ordering
        CI_Lower_Lim_Ordering<- round(CI_Lower_Lim_Ordering, digits = 2)
      }
      if(input$Select_CI_Obsessing == "2") {
        CI_Obsessing<- input$Man_CI_Obsessing
        CI_Obsessing<- round(CI_Obsessing, digits = 2)
        CI_Upper_Lim_Obsessing<- Score_Obsessing + CI_Obsessing
        CI_Upper_Lim_Obsessing<- round(CI_Upper_Lim_Obsessing, digits = 2)
        CI_Lower_Lim_Obsessing<- Score_Obsessing - CI_Obsessing
        CI_Lower_Lim_Obsessing<- round(CI_Lower_Lim_Obsessing, digits = 2)
      }
      if(input$Select_CI_Hoarding == "2") {
        CI_Hoarding<- input$Man_CI_Hoarding
        CI_Hoarding<- round(CI_Hoarding, digits = 2)
        CI_Upper_Lim_Hoarding<- Score_Hoarding + CI_Hoarding
        CI_Upper_Lim_Hoarding<- round(CI_Upper_Lim_Hoarding, digits = 2)
        CI_Lower_Lim_Hoarding<- Score_Hoarding - CI_Hoarding
        CI_Lower_Lim_Hoarding<- round(CI_Lower_Lim_Hoarding, digits = 2)
      }
      if(input$Select_CI_Neutralizing == "2") {
        CI_Neutralizing<- input$Man_CI_Neutralizing
        CI_Neutralizing<- round(CI_Neutralizing, digits = 2)
        CI_Upper_Lim_Neutralizing<- Score_Neutralizing + CI_Neutralizing
        CI_Upper_Lim_Neutralizing<- round(CI_Upper_Lim_Neutralizing, digits = 2)
        CI_Lower_Lim_Neutralizing<- Score_Neutralizing - CI_Neutralizing
        CI_Lower_Lim_Neutralizing<- round(CI_Lower_Lim_Neutralizing, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Washing_1<- round(input$Cutoff_Washing_1, digits = 2)
      Cutoff_Score_Washing_2<- round(input$Cutoff_Washing_2, digits = 2)
      Cutoff_Score_Washing_3<- round(input$Cutoff_Washing_3, digits = 2)
      Cutoff_Score_Checking_1<- round(input$Cutoff_Checking_1, digits = 2)
      Cutoff_Score_Checking_2<- round(input$Cutoff_Checking_2, digits = 2)
      Cutoff_Score_Checking_3<- round(input$Cutoff_Checking_3, digits = 2)
      Cutoff_Score_Ordering_1<- round(input$Cutoff_Ordering_1, digits = 2)
      Cutoff_Score_Ordering_2<- round(input$Cutoff_Ordering_2, digits = 2)
      Cutoff_Score_Ordering_3<- round(input$Cutoff_Ordering_3, digits = 2)
      Cutoff_Score_Obsessing_1<- round(input$Cutoff_Obsessing_1, digits = 2)
      Cutoff_Score_Obsessing_2<- round(input$Cutoff_Obsessing_2, digits = 2)
      Cutoff_Score_Obsessing_3<- round(input$Cutoff_Obsessing_3, digits = 2)
      Cutoff_Score_Hoarding_1<- round(input$Cutoff_Hoarding_1, digits = 2)
      Cutoff_Score_Hoarding_2<- round(input$Cutoff_Hoarding_2, digits = 2)
      Cutoff_Score_Hoarding_3<- round(input$Cutoff_Hoarding_3, digits = 2)
      Cutoff_Score_Neutralizing_1<- round(input$Cutoff_Neutralizing_1, digits = 2)
      Cutoff_Score_Neutralizing_2<- round(input$Cutoff_Neutralizing_2, digits = 2)
      Cutoff_Score_Neutralizing_3<- round(input$Cutoff_Neutralizing_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Washing,Change_Washing,PTS_Washing, SE_Washing, CI_Upper_Lim_Washing, CI_Lower_Lim_Washing, Cutoff_Score_Washing_1,Cutoff_Score_Washing_2,Cutoff_Score_Washing_3,
                                      Score_Checking,Change_Checking, PTS_Checking, SE_Checking, CI_Upper_Lim_Checking, CI_Lower_Lim_Checking, Cutoff_Score_Checking_1,Cutoff_Score_Checking_2,Cutoff_Score_Checking_3, 
                                      Score_Ordering,Change_Ordering,PTS_Ordering, SE_Ordering, CI_Upper_Lim_Ordering, CI_Lower_Lim_Ordering, Cutoff_Score_Ordering_1,Cutoff_Score_Ordering_2,Cutoff_Score_Ordering_3, 
                                      Score_Obsessing,Change_Obsessing,PTS_Obsessing, SE_Obsessing, CI_Upper_Lim_Obsessing, CI_Lower_Lim_Obsessing, Cutoff_Score_Obsessing_1,Cutoff_Score_Obsessing_2,Cutoff_Score_Obsessing_3,
                                      Score_Hoarding,Change_Hoarding, PTS_Hoarding, SE_Hoarding, CI_Upper_Lim_Hoarding, CI_Lower_Lim_Hoarding, Cutoff_Score_Hoarding_1,Cutoff_Score_Hoarding_2,Cutoff_Score_Hoarding_3, 
                                      Score_Neutralizing,Change_Neutralizing,PTS_Neutralizing, SE_Neutralizing, CI_Upper_Lim_Neutralizing, CI_Lower_Lim_Neutralizing, Cutoff_Score_Neutralizing_1,Cutoff_Score_Neutralizing_2,Cutoff_Score_Neutralizing_3
                                      )
    } else if(input$Timepoint == "2") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date<- c(Date_1, Date_2)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Score_1<- sum(Score_1a, na.rm = TRUE)
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_2a), Score = Score_2a)
      Score_2<- sum(Score_2a, na.rm = TRUE)
      Score<- c(Score_1, Score_2)
      Score<- round(Score, digits = 2)
      Score_Washing_1<- sum(Score_1a[c(5,11,17)], na.rm = TRUE)
      Score_Washing_2<- sum(Score_2a[c(5,11,17)], na.rm = TRUE)
      Score_Washing<- c(Score_Washing_1, Score_Washing_2)
      Score_Washing<- round(Score_Washing, digits = 2)
      Score_Checking_1<- sum(Score_1a[c(2,8,14)], na.rm = TRUE)
      Score_Checking_2<- sum(Score_2a[c(2,8,14)], na.rm = TRUE)
      Score_Checking<- c(Score_Checking_1,Score_Checking_2)
      Score_Checking<- round(Score_Checking, digits = 2)
      Score_Ordering_1<- sum(Score_1a[c(3,9,15)], na.rm = TRUE)
      Score_Ordering_2<- sum(Score_2a[c(3,9,15)], na.rm = TRUE)
      Score_Ordering<- c(Score_Ordering_1, Score_Ordering_2)
      Score_Ordering<- round(Score_Ordering, digits = 2)
      Score_Obsessing_1<- sum(Score_1a[c(6,12,18)], na.rm = TRUE)
      Score_Obsessing_2<- sum(Score_2a[c(6,12,18)], na.rm = TRUE)
      Score_Obsessing<- c(Score_Obsessing_1, Score_Obsessing_2)
      Score_Obsessing<- round(Score_Obsessing, digits = 2)
      Score_Hoarding_1<- sum(Score_1a[c(1,7,13)], na.rm = TRUE)
      Score_Hoarding_2<- sum(Score_2a[c(1,7,13)], na.rm = TRUE)
      Score_Hoarding<- c(Score_Hoarding_1, Score_Hoarding_2)
      Score_Hoarding<- round(Score_Hoarding, digits = 2)
      Score_Neutralizing_1<- sum(Score_1a[c(4,10,16)], na.rm = TRUE)
      Score_Neutralizing_2<- sum(Score_2a[c(4,10,16)], na.rm = TRUE)
      Score_Neutralizing<- c(Score_Neutralizing_1, Score_Neutralizing_2)
      Score_Neutralizing<- round(Score_Neutralizing, digits = 2)
      Change<- c(0, (Score_2 - Score_1))
      Change<- round(Change, digits = 2)
      Change_Washing<- c(0, (Score_Washing_2 - Score_Washing_1))
      Change_Washing<- round(Change_Washing, digits = 2)
      Change_Checking<- c(0, (Score_Checking_2 - Score_Checking_1))
      Change_Checking<- round(Change_Checking, digits = 2)
      Change_Ordering<- c(0, (Score_Ordering_2 - Score_Ordering_1))
      Change_Ordering<- round(Change_Ordering, digits = 2)
      Change_Obsessing<- c(0, (Score_Obsessing_2 - Score_Obsessing_1))
      Change_Obsessing<- round(Change_Obsessing, digits = 2)
      Change_Hoarding<- c(0, (Score_Hoarding_2 - Score_Hoarding_1))
      Change_Hoarding<- round(Change_Hoarding, digits = 2)
      Change_Neutralizing<- c(0, (Score_Neutralizing_2 - Score_Neutralizing_1))
      Change_Neutralizing<- round(Change_Neutralizing, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2)
        PTS_Washing_1<- (Rel_Washing * Score_Washing_1) + (M_Washing * (1 - Rel_Washing))
        PTS_Washing_2<- (Rel_Washing * Score_Washing_2) + (M_Washing * (1 - Rel_Washing))
        PTS_Washing<- c(PTS_Washing_1, PTS_Washing_2)
        PTS_Checking_1<- (Rel_Checking * Score_Checking_1) + (M_Checking * (1 - Rel_Checking))
        PTS_Checking_2<- (Rel_Checking * Score_Checking_2) + (M_Checking * (1 - Rel_Checking))
        PTS_Checking<- c(PTS_Checking_1, PTS_Checking_2)
        PTS_Ordering_1<- (Rel_Ordering * Score_Ordering_1) + (M_Ordering * (1 - Rel_Ordering))
        PTS_Ordering_2<- (Rel_Ordering * Score_Ordering_2) + (M_Ordering * (1 - Rel_Ordering))
        PTS_Ordering<- c(PTS_Ordering_1, PTS_Ordering_2)
        PTS_Obsessing_1<- (Rel_Obsessing * Score_Obsessing_1) + (M_Obsessing * (1 - Rel_Obsessing))
        PTS_Obsessing_2<- (Rel_Obsessing * Score_Obsessing_2) + (M_Obsessing * (1 - Rel_Obsessing))
        PTS_Obsessing<- c(PTS_Obsessing_1, PTS_Obsessing_2)
        PTS_Hoarding_1<- (Rel_Hoarding * Score_Hoarding_1) + (M_Hoarding * (1 - Rel_Hoarding))
        PTS_Hoarding_2<- (Rel_Hoarding * Score_Hoarding_2) + (M_Hoarding * (1 - Rel_Hoarding))
        PTS_Hoarding<- c(PTS_Hoarding_1, PTS_Hoarding_2)
        PTS_Neutralizing_1<- (Rel_Neutralizing * Score_Neutralizing_1) + (M_Neutralizing * (1 - Rel_Neutralizing))
        PTS_Neutralizing_2<- (Rel_Neutralizing * Score_Neutralizing_2) + (M_Neutralizing * (1 - Rel_Neutralizing))
        PTS_Neutralizing<- c(PTS_Neutralizing_1, PTS_Neutralizing_2)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2)
        PTS_Washing_1<- Score_Washing_1 + (M_Retest_Washing - M_Washing)  
        PTS_Washing_2<- Score_Washing_2 + (M_Retest_Washing - M_Washing) 
        PTS_Washing<- c(PTS_Washing_1, PTS_Washing_2)
        PTS_Checking_1<- Score_Checking_1 + (M_Retest_Checking - M_Checking)  
        PTS_Checking_2<- Score_Checking_2 + (M_Retest_Checking - M_Checking) 
        PTS_Checking<- c(PTS_Checking_1, PTS_Checking_2)
        PTS_Ordering_1<- Score_Ordering_1 + (M_Retest_Ordering - M_Ordering)  
        PTS_Ordering_2<- Score_Ordering_2 + (M_Retest_Ordering - M_Ordering) 
        PTS_Ordering<- c(PTS_Ordering_1, PTS_Ordering_2)
        PTS_Obsessing_1<- Score_Obsessing_1 + (M_Retest_Obsessing - M_Obsessing)  
        PTS_Obsessing_2<- Score_Obsessing_2 + (M_Retest_Obsessing - M_Obsessing) 
        PTS_Obsessing<- c(PTS_Obsessing_1, PTS_Obsessing_2)
        PTS_Hoarding_1<- Score_Hoarding_1 + (M_Retest_Hoarding - M_Hoarding)  
        PTS_Hoarding_2<- Score_Hoarding_2 + (M_Retest_Hoarding - M_Hoarding) 
        PTS_Hoarding<- c(PTS_Hoarding_1, PTS_Hoarding_2)
        PTS_Neutralizing_1<- Score_Neutralizing_1 + (M_Retest_Neutralizing - M_Neutralizing)  
        PTS_Neutralizing_2<- Score_Neutralizing_2 + (M_Retest_Neutralizing - M_Neutralizing) 
        PTS_Neutralizing<- c(PTS_Neutralizing_1, PTS_Neutralizing_2)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Washing<- Score_Washing
        PTS_Checking<- Score_Checking
        PTS_Ordering<- Score_Ordering
        PTS_Obsessing<- Score_Obsessing
        PTS_Hoarding<- Score_Hoarding
        PTS_Neutralizing<- Score_Neutralizing
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Washing<- Rel_Washing * (SD_Retest_Washing/SD_Washing)
        A_Constant_Washing<- M_Retest_Washing - (B_Slope_Washing * M_Washing)
        B_Adj_Washing<- SD_Retest_Washing/SD_Washing
        A_Adj_Washing<- M_Retest_Washing - (B_Adj_Washing * M_Washing)
        PTS_Washing_1<- (B_Adj_Washing * Score_Washing_1) + A_Adj_Washing
        PTS_Washing_2<- (B_Adj_Washing * Score_Washing_2) + A_Adj_Washing
        PTS_Washing<- c(PTS_Washing_1,PTS_Washing_2)
        B_Slope_Checking<- Rel_Checking * (SD_Retest_Checking/SD_Checking)
        A_Constant_Checking<- M_Retest_Checking - (B_Slope_Checking * M_Checking)
        B_Adj_Checking<- SD_Retest_Checking/SD_Checking
        A_Adj_Checking<- M_Retest_Checking - (B_Adj_Checking * M_Checking)
        PTS_Checking_1<- (B_Adj_Checking * Score_Checking_1) + A_Adj_Checking
        PTS_Checking_2<- (B_Adj_Checking * Score_Checking_2) + A_Adj_Checking
        PTS_Checking<- c(PTS_Checking_1,PTS_Checking_2)
        B_Slope_Ordering<- Rel_Ordering * (SD_Retest_Ordering/SD_Ordering)
        A_Constant_Ordering<- M_Retest_Ordering - (B_Slope_Ordering * M_Ordering)
        B_Adj_Ordering<- SD_Retest_Ordering/SD_Ordering
        A_Adj_Ordering<- M_Retest_Ordering - (B_Adj_Ordering * M_Ordering)
        PTS_Ordering_1<- (B_Adj_Ordering * Score_Ordering_1) + A_Adj_Ordering
        PTS_Ordering_2<- (B_Adj_Ordering * Score_Ordering_2) + A_Adj_Ordering
        PTS_Ordering<- c(PTS_Ordering_1,PTS_Ordering_2)
        B_Slope_Obsessing<- Rel_Obsessing * (SD_Retest_Obsessing/SD_Obsessing)
        A_Constant_Obsessing<- M_Retest_Obsessing - (B_Slope_Obsessing * M_Obsessing)
        B_Adj_Obsessing<- SD_Retest_Obsessing/SD_Obsessing
        A_Adj_Obsessing<- M_Retest_Obsessing - (B_Adj_Obsessing * M_Obsessing)
        PTS_Obsessing_1<- (B_Adj_Obsessing * Score_Obsessing_1) + A_Adj_Obsessing
        PTS_Obsessing_2<- (B_Adj_Obsessing * Score_Obsessing_2) + A_Adj_Obsessing
        PTS_Obsessing<- c(PTS_Obsessing_1,PTS_Obsessing_2)
        B_Slope_Hoarding<- Rel_Hoarding * (SD_Retest_Hoarding/SD_Hoarding)
        A_Constant_Hoarding<- M_Retest_Hoarding - (B_Slope_Hoarding * M_Hoarding)
        B_Adj_Hoarding<- SD_Retest_Hoarding/SD_Hoarding
        A_Adj_Hoarding<- M_Retest_Hoarding - (B_Adj_Hoarding * M_Hoarding)
        PTS_Hoarding_1<- (B_Adj_Hoarding * Score_Hoarding_1) + A_Adj_Hoarding
        PTS_Hoarding_2<- (B_Adj_Hoarding * Score_Hoarding_2) + A_Adj_Hoarding
        PTS_Hoarding<- c(PTS_Hoarding_1, PTS_Hoarding_2)
        B_Slope_Neutralizing<- Rel_Neutralizing * (SD_Retest_Neutralizing/SD_Neutralizing)
        A_Constant_Neutralizing<- M_Retest_Neutralizing - (B_Slope_Neutralizing * M_Neutralizing)
        B_Adj_Neutralizing<- SD_Retest_Neutralizing/SD_Neutralizing
        A_Adj_Neutralizing<- M_Retest_Neutralizing - (B_Adj_Neutralizing * M_Neutralizing)
        PTS_Neutralizing_1<- (B_Adj_Neutralizing * Score_Neutralizing_1) + A_Adj_Neutralizing
        PTS_Neutralizing_2<- (B_Adj_Neutralizing * Score_Neutralizing_2) + A_Adj_Neutralizing
        PTS_Neutralizing<- c(PTS_Neutralizing_1,PTS_Neutralizing_2)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Washing<- Rel_Washing * (SD_Retest_Washing/SD_Washing)
        PTS_Washing_1<- B_Slope_Washing * Score_Washing_1
        PTS_Washing_2<- B_Slope_Washing * Score_Washing_2
        PTS_Washing<- c(PTS_Washing_1, PTS_Washing_2)
        B_Slope_Checking<- Rel_Checking * (SD_Retest_Checking/SD_Checking)
        PTS_Checking_1<- B_Slope_Checking * Score_Checking_1
        PTS_Checking_2<- B_Slope_Checking * Score_Checking_2
        PTS_Checking<- c(PTS_Checking_1, PTS_Checking_2)
        B_Slope_Ordering<- Rel_Ordering * (SD_Retest_Ordering/SD_Ordering)
        PTS_Ordering_1<- B_Slope_Ordering * Score_Ordering_1
        PTS_Ordering_2<- B_Slope_Ordering * Score_Ordering_2
        PTS_Ordering<- c(PTS_Ordering_1, PTS_Ordering_2)
        B_Slope_Obsessing<- Rel_Obsessing * (SD_Retest_Obsessing/SD_Obsessing)
        PTS_Obsessing_1<- B_Slope_Obsessing * Score_Obsessing_1
        PTS_Obsessing_2<- B_Slope_Obsessing * Score_Obsessing_2
        PTS_Obsessing<- c(PTS_Obsessing_1, PTS_Obsessing_2)
        B_Slope_Hoarding<- Rel_Hoarding * (SD_Retest_Hoarding/SD_Hoarding)
        PTS_Hoarding_1<- B_Slope_Hoarding * Score_Hoarding_1
        PTS_Hoarding_2<- B_Slope_Hoarding * Score_Hoarding_2
        PTS_Hoarding<- c(PTS_Hoarding_1, PTS_Hoarding_2)
        B_Slope_Neutralizing<- Rel_Neutralizing * (SD_Retest_Neutralizing/SD_Neutralizing)
        PTS_Neutralizing_1<- B_Slope_Neutralizing * Score_Neutralizing_1
        PTS_Neutralizing_2<- B_Slope_Neutralizing * Score_Neutralizing_2
        PTS_Neutralizing<- c(PTS_Neutralizing_1, PTS_Neutralizing_2) 
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2)
        PTS_Washing_1<- Score_Washing_1 + (M_Retest_Washing - M_Washing)
        PTS_Washing_2<- Score_Washing_2 + (M_Retest_Washing - M_Washing)
        PTS_Washing<- c(PTS_Washing_1, PTS_Washing_2)
        PTS_Checking_1<- Score_Checking_1 + (M_Retest_Checking - M_Checking)
        PTS_Checking_2<- Score_Checking_2 + (M_Retest_Checking - M_Checking)
        PTS_Checking<- c(PTS_Checking_1, PTS_Checking_2)
        PTS_Ordering_1<- Score_Ordering_1 + (M_Retest_Ordering - M_Ordering)
        PTS_Ordering_2<- Score_Ordering_2 + (M_Retest_Ordering - M_Ordering)
        PTS_Ordering<- c(PTS_Ordering_1, PTS_Ordering_2)
        PTS_Obsessing_1<- Score_Obsessing_1 + (M_Retest_Obsessing - M_Obsessing)
        PTS_Obsessing_2<- Score_Obsessing_2 + (M_Retest_Obsessing - M_Obsessing)
        PTS_Obsessing<- c(PTS_Obsessing_1, PTS_Obsessing_2)
        PTS_Hoarding_1<- Score_Hoarding_1 + (M_Retest_Hoarding - M_Hoarding)
        PTS_Hoarding_2<- Score_Hoarding_2 + (M_Retest_Hoarding - M_Hoarding)
        PTS_Hoarding<- c(PTS_Hoarding_1, PTS_Hoarding_2)
        PTS_Neutralizing_1<- Score_Neutralizing_1 + (M_Retest_Neutralizing - M_Neutralizing)
        PTS_Neutralizing_2<- Score_Neutralizing_2 + (M_Retest_Neutralizing - M_Neutralizing)
        PTS_Neutralizing<- c(PTS_Neutralizing_1, PTS_Neutralizing_2)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Washing<- round(PTS_Washing, digits = 2)
      PTS_Checking<- round(PTS_Checking, digits = 2)
      PTS_Ordering<- round(PTS_Ordering, digits = 2)
      PTS_Obsessing<- round(PTS_Obsessing, digits = 2)
      PTS_Hoarding<- round(PTS_Hoarding, digits = 2)
      PTS_Neutralizing<- round(PTS_Neutralizing, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2)
        SE_Washing_1<- McSweeny_SE_Washing*sqrt(1 + (1/SampleN) + ((Score_Washing_1 - M_Washing)^2/(SD_Washing^2*(SampleN-1))))
        SE_Washing_2<- McSweeny_SE_Washing*sqrt(1 + (1/SampleN) + ((Score_Washing_2 - M_Washing)^2/(SD_Washing^2*(SampleN-1))))
        SE_Washing<-c(SE_Washing_1, SE_Washing_2)
        SE_Checking_1<- McSweeny_SE_Checking*sqrt(1 + (1/SampleN) + ((Score_Checking_1 - M_Checking)^2/(SD_Checking^2*(SampleN-1))))
        SE_Checking_2<- McSweeny_SE_Checking*sqrt(1 + (1/SampleN) + ((Score_Checking_2 - M_Checking)^2/(SD_Checking^2*(SampleN-1))))
        SE_Checking<-c(SE_Checking_1, SE_Checking_2)
        SE_Ordering_1<- McSweeny_SE_Ordering*sqrt(1 + (1/SampleN) + ((Score_Ordering_1 - M_Ordering)^2/(SD_Ordering^2*(SampleN-1))))
        SE_Ordering_2<- McSweeny_SE_Ordering*sqrt(1 + (1/SampleN) + ((Score_Ordering_2 - M_Ordering)^2/(SD_Ordering^2*(SampleN-1))))
        SE_Ordering<-c(SE_Ordering_1, SE_Ordering_2)
        SE_Obsessing_1<- McSweeny_SE_Obsessing*sqrt(1 + (1/SampleN) + ((Score_Obsessing_1 - M_Obsessing)^2/(SD_Obsessing^2*(SampleN-1))))
        SE_Obsessing_2<- McSweeny_SE_Obsessing*sqrt(1 + (1/SampleN) + ((Score_Obsessing_2 - M_Obsessing)^2/(SD_Obsessing^2*(SampleN-1))))
        SE_Obsessing<-c(SE_Obsessing_1, SE_Obsessing_2)
        SE_Hoarding_1<- McSweeny_SE_Hoarding*sqrt(1 + (1/SampleN) + ((Score_Hoarding_1 - M_Hoarding)^2/(SD_Hoarding^2*(SampleN-1))))
        SE_Hoarding_2<- McSweeny_SE_Hoarding*sqrt(1 + (1/SampleN) + ((Score_Hoarding_2 - M_Hoarding)^2/(SD_Hoarding^2*(SampleN-1))))
        SE_Hoarding<-c(SE_Hoarding_1, SE_Hoarding_2)
        SE_Neutralizing_1<- McSweeny_SE_Neutralizing*sqrt(1 + (1/SampleN) + ((Score_Neutralizing_1 - M_Neutralizing)^2/(SD_Neutralizing^2*(SampleN-1))))
        SE_Neutralizing_2<- McSweeny_SE_Neutralizing*sqrt(1 + (1/SampleN) + ((Score_Neutralizing_2 - M_Neutralizing)^2/(SD_Neutralizing^2*(SampleN-1))))
        SE_Neutralizing<-c(SE_Neutralizing_1, SE_Neutralizing_2)
        SE<- round(SE, digits = 2)
        SE_Washing<- round(SE_Washing, digits = 2)
        SE_Checking<- round(SE_Checking, digits = 2)
        SE_Ordering<- round(SE_Ordering, digits = 2)
        SE_Obsessing<- round(SE_Obsessing, digits = 2)
        SE_Hoarding<- round(SE_Hoarding, digits = 2)
        SE_Neutralizing<- round(SE_Neutralizing, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2))
        CI<- round(CI, digits = 2)
        CI_Washing<- c((Conf*SE_Washing_1), (Conf*SE_Washing_2))
        CI_Washing<- round(CI_Washing, digits = 2)
        CI_Checking<- c((Conf*SE_Checking_1), (Conf*SE_Checking_2))
        CI_Checking<- round(CI_Checking, digits = 2)
        CI_Ordering<- c((Conf*SE_Ordering_1), (Conf*SE_Ordering_2))
        CI_Ordering<- round(CI_Ordering, digits = 2)
        CI_Obsessing<- c((Conf*SE_Obsessing_1), (Conf*SE_Obsessing_2))
        CI_Obsessing<- round(CI_Obsessing, digits = 2)
        CI_Hoarding<- c((Conf*SE_Hoarding_1), (Conf*SE_Hoarding_2))
        CI_Hoarding<- round(CI_Hoarding, digits = 2)
        CI_Neutralizing<- c((Conf*SE_Neutralizing_1), (Conf*SE_Neutralizing_2))
        CI_Neutralizing<- round(CI_Neutralizing, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Washing<- c((Conf*SE_Washing), (Conf*SE_Washing))
        CI_Washing<- round(CI_Washing, digits = 2)
        CI_Checking<- c((Conf*SE_Checking), (Conf*SE_Checking))
        CI_Checking<- round(CI_Checking, digits = 2)
        CI_Ordering<- c((Conf*SE_Ordering), (Conf*SE_Ordering))
        CI_Ordering<- round(CI_Ordering, digits = 2)
        CI_Obsessing<- c((Conf*SE_Obsessing), (Conf*SE_Obsessing))
        CI_Obsessing<- round(CI_Obsessing, digits = 2)
        CI_Hoarding<- c((Conf*SE_Hoarding), (Conf*SE_Hoarding))
        CI_Hoarding<- round(CI_Hoarding, digits = 2)
        CI_Neutralizing<- c((Conf*SE_Neutralizing), (Conf*SE_Neutralizing))
        CI_Neutralizing<- round(CI_Neutralizing, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Washing<- PTS_Washing + CI_Washing
      CI_Upper_Lim_Washing<- round(CI_Upper_Lim_Washing, digits = 2)
      CI_Lower_Lim_Washing<-PTS_Washing - CI_Washing
      CI_Lower_Lim_Washing<- round(CI_Lower_Lim_Washing, digits = 2)
      CI_Upper_Lim_Checking<- PTS_Checking + CI_Checking
      CI_Upper_Lim_Checking<- round(CI_Upper_Lim_Checking, digits = 2)
      CI_Lower_Lim_Checking<-PTS_Checking - CI_Checking
      CI_Lower_Lim_Checking<- round(CI_Lower_Lim_Checking, digits = 2)
      CI_Upper_Lim_Ordering<- PTS_Ordering + CI_Ordering
      CI_Upper_Lim_Ordering<- round(CI_Upper_Lim_Ordering, digits = 2)
      CI_Lower_Lim_Ordering<-PTS_Ordering - CI_Ordering
      CI_Lower_Lim_Ordering<- round(CI_Lower_Lim_Ordering, digits = 2)
      CI_Upper_Lim_Obsessing<- PTS_Obsessing + CI_Obsessing
      CI_Upper_Lim_Obsessing<- round(CI_Upper_Lim_Obsessing, digits = 2)
      CI_Lower_Lim_Obsessing<-PTS_Obsessing - CI_Obsessing
      CI_Lower_Lim_Obsessing<- round(CI_Lower_Lim_Obsessing, digits = 2)
      CI_Upper_Lim_Hoarding<- PTS_Hoarding + CI_Hoarding
      CI_Upper_Lim_Hoarding<- round(CI_Upper_Lim_Hoarding, digits = 2)
      CI_Lower_Lim_Hoarding<- PTS_Hoarding - CI_Hoarding
      CI_Lower_Lim_Hoarding<- round(CI_Lower_Lim_Hoarding, digits = 2)
      CI_Upper_Lim_Neutralizing<- PTS_Neutralizing + CI_Neutralizing
      CI_Upper_Lim_Neutralizing<- round(CI_Upper_Lim_Neutralizing, digits = 2)
      CI_Lower_Lim_Neutralizing<-PTS_Neutralizing - CI_Neutralizing
      CI_Lower_Lim_Neutralizing<- round(CI_Lower_Lim_Neutralizing, digits = 2)
  
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Washing == "2") {
        CI_Washing<- input$Man_CI_Washing
        CI_Washing<- c(CI_Washing, CI_Washing)
        CI_Washing<- round(CI_Washing, digits = 2)
        CI_Upper_Lim_Washing<- Score_Washing + CI_Washing
        CI_Upper_Lim_Washing<- round(CI_Upper_Lim_Washing, digits = 2)
        CI_Lower_Lim_Washing<- Score_Washing - CI_Washing
        CI_Lower_Lim_Washing<- round(CI_Lower_Lim_Washing, digits = 2)
      }
      if(input$Select_CI_Checking == "2") {
        CI_Checking<- input$Man_CI_Checking
        CI_Checking<- c(CI_Checking, CI_Checking)
        CI_Checking<- round(CI_Checking, digits = 2)
        CI_Upper_Lim_Checking<- Score_Checking + CI_Checking
        CI_Upper_Lim_Checking<- round(CI_Upper_Lim_Checking, digits = 2)
        CI_Lower_Lim_Checking<- Score_Checking - CI_Checking
        CI_Lower_Lim_Checking<- round(CI_Lower_Lim_Checking, digits = 2)
      }
      if(input$Select_CI_Ordering == "2") {
        CI_Ordering<- input$Man_CI_Ordering
        CI_Ordering<- c(CI_Ordering, CI_Ordering)
        CI_Ordering<- round(CI_Ordering, digits = 2)
        CI_Upper_Lim_Ordering<- Score_Ordering + CI_Ordering
        CI_Upper_Lim_Ordering<- round(CI_Upper_Lim_Ordering, digits = 2)
        CI_Lower_Lim_Ordering<- Score_Ordering - CI_Ordering
        CI_Lower_Lim_Ordering<- round(CI_Lower_Lim_Ordering, digits = 2)
      }
      if(input$Select_CI_Obsessing == "2") {
        CI_Obsessing<- input$Man_CI_Obsessing
        CI_Obsessing<- c(CI_Obsessing,  CI_Obsessing)
        CI_Obsessing<- round(CI_Obsessing, digits = 2)
        CI_Upper_Lim_Obsessing<- Score_Obsessing + CI_Obsessing
        CI_Upper_Lim_Obsessing<- round(CI_Upper_Lim_Obsessing, digits = 2)
        CI_Lower_Lim_Obsessing<- Score_Obsessing - CI_Obsessing
        CI_Lower_Lim_Obsessing<- round(CI_Lower_Lim_Obsessing, digits = 2)
      }
      if(input$Select_CI_Hoarding == "2") {
        CI_Hoarding<- input$Man_CI_Hoarding
        CI_Hoarding<- c(CI_Hoarding, CI_Hoarding)
        CI_Hoarding<- round(CI_Hoarding, digits = 2)
        CI_Upper_Lim_Hoarding<- Score_Hoarding + CI_Hoarding
        CI_Upper_Lim_Hoarding<- round(CI_Upper_Lim_Hoarding, digits = 2)
        CI_Lower_Lim_Hoarding<- Score_Hoarding - CI_Hoarding
        CI_Lower_Lim_Hoarding<- round(CI_Lower_Lim_Hoarding, digits = 2)
      }
      if(input$Select_CI_Neutralizing == "2") {
        CI_Neutralizing<- input$Man_CI_Neutralizing
        CI_Neutralizing<- c(CI_Neutralizing, CI_Neutralizing)
        CI_Neutralizing<- round(CI_Neutralizing, digits = 2)
        CI_Upper_Lim_Neutralizing<- Score_Neutralizing + CI_Neutralizing
        CI_Upper_Lim_Neutralizing<- round(CI_Upper_Lim_Neutralizing, digits = 2)
        CI_Lower_Lim_Neutralizing<- Score_Neutralizing - CI_Neutralizing
        CI_Lower_Lim_Neutralizing<- round(CI_Lower_Lim_Neutralizing, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Washing_1<- round(input$Cutoff_Washing_1, digits = 2)
      Cutoff_Score_Washing_2<- round(input$Cutoff_Washing_2, digits = 2)
      Cutoff_Score_Washing_3<- round(input$Cutoff_Washing_3, digits = 2)
      Cutoff_Score_Checking_1<- round(input$Cutoff_Checking_1, digits = 2)
      Cutoff_Score_Checking_2<- round(input$Cutoff_Checking_2, digits = 2)
      Cutoff_Score_Checking_3<- round(input$Cutoff_Checking_3, digits = 2)
      Cutoff_Score_Ordering_1<- round(input$Cutoff_Ordering_1, digits = 2)
      Cutoff_Score_Ordering_2<- round(input$Cutoff_Ordering_2, digits = 2)
      Cutoff_Score_Ordering_3<- round(input$Cutoff_Ordering_3, digits = 2)
      Cutoff_Score_Obsessing_1<- round(input$Cutoff_Obsessing_1, digits = 2)
      Cutoff_Score_Obsessing_2<- round(input$Cutoff_Obsessing_2, digits = 2)
      Cutoff_Score_Obsessing_3<- round(input$Cutoff_Obsessing_3, digits = 2)
      Cutoff_Score_Hoarding_1<- round(input$Cutoff_Hoarding_1, digits = 2)
      Cutoff_Score_Hoarding_2<- round(input$Cutoff_Hoarding_2, digits = 2)
      Cutoff_Score_Hoarding_3<- round(input$Cutoff_Hoarding_3, digits = 2)
      Cutoff_Score_Neutralizing_1<- round(input$Cutoff_Neutralizing_1, digits = 2)
      Cutoff_Score_Neutralizing_2<- round(input$Cutoff_Neutralizing_2, digits = 2)
      Cutoff_Score_Neutralizing_3<- round(input$Cutoff_Neutralizing_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Washing,Change_Washing,PTS_Washing, SE_Washing, CI_Upper_Lim_Washing, CI_Lower_Lim_Washing, Cutoff_Score_Washing_1,Cutoff_Score_Washing_2,Cutoff_Score_Washing_3,
                                      Score_Checking,Change_Checking, PTS_Checking, SE_Checking, CI_Upper_Lim_Checking, CI_Lower_Lim_Checking, Cutoff_Score_Checking_1,Cutoff_Score_Checking_2,Cutoff_Score_Checking_3, 
                                      Score_Ordering,Change_Ordering,PTS_Ordering, SE_Ordering, CI_Upper_Lim_Ordering, CI_Lower_Lim_Ordering, Cutoff_Score_Ordering_1,Cutoff_Score_Ordering_2,Cutoff_Score_Ordering_3, 
                                      Score_Obsessing,Change_Obsessing,PTS_Obsessing, SE_Obsessing, CI_Upper_Lim_Obsessing, CI_Lower_Lim_Obsessing, Cutoff_Score_Obsessing_1,Cutoff_Score_Obsessing_2,Cutoff_Score_Obsessing_3,
                                      Score_Hoarding,Change_Hoarding, PTS_Hoarding, SE_Hoarding, CI_Upper_Lim_Hoarding, CI_Lower_Lim_Hoarding, Cutoff_Score_Hoarding_1,Cutoff_Score_Hoarding_2,Cutoff_Score_Hoarding_3, 
                                      Score_Neutralizing,Change_Neutralizing,PTS_Neutralizing, SE_Neutralizing, CI_Upper_Lim_Neutralizing, CI_Lower_Lim_Neutralizing, Cutoff_Score_Neutralizing_1,Cutoff_Score_Neutralizing_2,Cutoff_Score_Neutralizing_3 
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
      Item_Df<<- data.frame(Item = 1:length(Score_3a), Score = Score_3a)
      Score_3<- sum(Score_3a, na.rm = TRUE)
      Score<- c(Score_1, Score_2, Score_3)
      Score<- round(Score, digits = 2)
      Score_Washing_1<- sum(Score_1a[c(5,11,17)], na.rm = TRUE)
      Score_Washing_2<- sum(Score_2a[c(5,11,17)], na.rm = TRUE)
      Score_Washing_3<- sum(Score_3a[c(5,11,17)], na.rm = TRUE)
      Score_Washing<- c(Score_Washing_1, Score_Washing_2, Score_Washing_3)
      Score_Washing<- round(Score_Washing, digits = 2)
      Score_Checking_1<- sum(Score_1a[c(2,8,14)], na.rm = TRUE)
      Score_Checking_2<- sum(Score_2a[c(2,8,14)], na.rm = TRUE)
      Score_Checking_3<- sum(Score_3a[c(2,8,14)], na.rm = TRUE)
      Score_Checking<- c(Score_Checking_1,Score_Checking_2, Score_Checking_3)
      Score_Checking<- round(Score_Checking, digits = 2)
      Score_Ordering_1<- sum(Score_1a[c(3,9,15)], na.rm = TRUE)
      Score_Ordering_2<- sum(Score_2a[c(3,9,15)], na.rm = TRUE)
      Score_Ordering_3<- sum(Score_3a[c(3,9,15)], na.rm = TRUE)
      Score_Ordering<- c(Score_Ordering_1, Score_Ordering_2, Score_Ordering_3)
      Score_Ordering<- round(Score_Ordering, digits = 2)
      Score_Obsessing_1<- sum(Score_1a[c(6,12,18)], na.rm = TRUE)
      Score_Obsessing_2<- sum(Score_2a[c(6,12,18)], na.rm = TRUE)
      Score_Obsessing_3<- sum(Score_3a[c(6,12,18)], na.rm = TRUE)
      Score_Obsessing<- c(Score_Obsessing_1, Score_Obsessing_2, Score_Obsessing_3)
      Score_Obsessing<- round(Score_Obsessing, digits = 2)
      Score_Hoarding_1<- sum(Score_1a[c(1,7,13)], na.rm = TRUE)
      Score_Hoarding_2<- sum(Score_2a[c(1,7,13)], na.rm = TRUE)
      Score_Hoarding_3<- sum(Score_3a[c(1,7,13)], na.rm = TRUE)
      Score_Hoarding<- c(Score_Hoarding_1, Score_Hoarding_2, Score_Hoarding_3)
      Score_Hoarding<- round(Score_Hoarding, digits = 2)
      Score_Neutralizing_1<- sum(Score_1a[c(4,10,16)], na.rm = TRUE)
      Score_Neutralizing_2<- sum(Score_2a[c(4,10,16)], na.rm = TRUE)
      Score_Neutralizing_3<- sum(Score_3a[c(4,10,16)], na.rm = TRUE)
      Score_Neutralizing<- c(Score_Neutralizing_1, Score_Neutralizing_2, Score_Neutralizing_3)
      Score_Neutralizing<- round(Score_Neutralizing, digits = 2)
      Change<- c(0, (Score_2 - Score_1), (Score_3 - Score_2))
      Change<- round(Change, digits = 2)
      Change_Washing<- c(0, (Score_Washing_2 - Score_Washing_1), (Score_Washing_3 - Score_Washing_2))
      Change_Washing<- round(Change_Washing, digits = 2)
      Change_Checking<- c(0, (Score_Checking_2 - Score_Checking_1), (Score_Checking_3 - Score_Checking_2))
      Change_Checking<- round(Change_Checking, digits = 2)
      Change_Ordering<- c(0, (Score_Ordering_2 - Score_Ordering_1),  (Score_Ordering_3 - Score_Ordering_2))
      Change_Ordering<- round(Change_Ordering, digits = 2)
      Change_Obsessing<- c(0, (Score_Obsessing_2 - Score_Obsessing_1), (Score_Obsessing_3 - Score_Obsessing_2))
      Change_Obsessing<- round(Change_Obsessing, digits = 2)
      Change_Hoarding<- c(0, (Score_Hoarding_2 - Score_Hoarding_1), (Score_Hoarding_3 - Score_Hoarding_2))
      Change_Hoarding<- round(Change_Hoarding, digits = 2)
      Change_Neutralizing<- c(0, (Score_Neutralizing_2 - Score_Neutralizing_1), (Score_Neutralizing_3 - Score_Neutralizing_2))
      Change_Neutralizing<- round(Change_Neutralizing, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS_3<- (Rel * Score_3) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Washing_1<- (Rel_Washing * Score_Washing_1) + (M_Washing * (1 - Rel_Washing))
        PTS_Washing_2<- (Rel_Washing * Score_Washing_2) + (M_Washing * (1 - Rel_Washing))
        PTS_Washing_3<- (Rel_Washing * Score_Washing_3) + (M_Washing * (1 - Rel_Washing))
        PTS_Washing<- c(PTS_Washing_1, PTS_Washing_2, PTS_Washing_3)
        PTS_Checking_1<- (Rel_Checking * Score_Checking_1) + (M_Checking * (1 - Rel_Checking))
        PTS_Checking_2<- (Rel_Checking * Score_Checking_2) + (M_Checking * (1 - Rel_Checking))
        PTS_Checking_3<- (Rel_Checking * Score_Checking_3) + (M_Checking * (1 - Rel_Checking))
        PTS_Checking<- c(PTS_Checking_1, PTS_Checking_2, PTS_Checking_3)
        PTS_Ordering_1<- (Rel_Ordering * Score_Ordering_1) + (M_Ordering * (1 - Rel_Ordering))
        PTS_Ordering_2<- (Rel_Ordering * Score_Ordering_2) + (M_Ordering * (1 - Rel_Ordering))
        PTS_Ordering_3<- (Rel_Ordering * Score_Ordering_3) + (M_Ordering * (1 - Rel_Ordering))
        PTS_Ordering<- c(PTS_Ordering_1, PTS_Ordering_2, PTS_Ordering_3)
        PTS_Obsessing_1<- (Rel_Obsessing * Score_Obsessing_1) + (M_Obsessing * (1 - Rel_Obsessing))
        PTS_Obsessing_2<- (Rel_Obsessing * Score_Obsessing_2) + (M_Obsessing * (1 - Rel_Obsessing))
        PTS_Obsessing_3<- (Rel_Obsessing * Score_Obsessing_3) + (M_Obsessing * (1 - Rel_Obsessing))
        PTS_Obsessing<- c(PTS_Obsessing_1, PTS_Obsessing_2, PTS_Obsessing_3)
        PTS_Hoarding_1<- (Rel_Hoarding * Score_Hoarding_1) + (M_Hoarding * (1 - Rel_Hoarding))
        PTS_Hoarding_2<- (Rel_Hoarding * Score_Hoarding_2) + (M_Hoarding * (1 - Rel_Hoarding))
        PTS_Hoarding_3<- (Rel_Hoarding * Score_Hoarding_3) + (M_Hoarding * (1 - Rel_Hoarding))
        PTS_Hoarding<- c(PTS_Hoarding_1, PTS_Hoarding_2, PTS_Hoarding_3)
        PTS_Neutralizing_1<- (Rel_Neutralizing * Score_Neutralizing_1) + (M_Neutralizing * (1 - Rel_Neutralizing))
        PTS_Neutralizing_2<- (Rel_Neutralizing * Score_Neutralizing_2) + (M_Neutralizing * (1 - Rel_Neutralizing))
        PTS_Neutralizing_3<- (Rel_Neutralizing * Score_Neutralizing_3) + (M_Neutralizing * (1 - Rel_Neutralizing))
        PTS_Neutralizing<- c(PTS_Neutralizing_1, PTS_Neutralizing_2, PTS_Neutralizing_3)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Washing_1<- Score_Washing_1 + (M_Retest_Washing - M_Washing)  
        PTS_Washing_2<- Score_Washing_2 + (M_Retest_Washing - M_Washing) 
        PTS_Washing_3<- Score_Washing_3 + (M_Retest_Washing - M_Washing) 
        PTS_Washing<- c(PTS_Washing_1, PTS_Washing_2, PTS_Washing_3)
        PTS_Checking_1<- Score_Checking_1 + (M_Retest_Checking - M_Checking)  
        PTS_Checking_2<- Score_Checking_2 + (M_Retest_Checking - M_Checking) 
        PTS_Checking_3<- Score_Checking_3 + (M_Retest_Checking - M_Checking) 
        PTS_Checking<- c(PTS_Checking_1, PTS_Checking_2, PTS_Checking_3)
        PTS_Ordering_1<- Score_Ordering_1 + (M_Retest_Ordering - M_Ordering)  
        PTS_Ordering_2<- Score_Ordering_2 + (M_Retest_Ordering - M_Ordering) 
        PTS_Ordering_3<- Score_Ordering_3 + (M_Retest_Ordering - M_Ordering) 
        PTS_Ordering<- c(PTS_Ordering_1, PTS_Ordering_2, PTS_Ordering_3)
        PTS_Obsessing_1<- Score_Obsessing_1 + (M_Retest_Obsessing - M_Obsessing)  
        PTS_Obsessing_2<- Score_Obsessing_2 + (M_Retest_Obsessing - M_Obsessing) 
        PTS_Obsessing_3<- Score_Obsessing_3 + (M_Retest_Obsessing - M_Obsessing) 
        PTS_Obsessing<- c(PTS_Obsessing_1, PTS_Obsessing_2, PTS_Obsessing_3)
        PTS_Hoarding_1<- Score_Hoarding_1 + (M_Retest_Hoarding - M_Hoarding)  
        PTS_Hoarding_2<- Score_Hoarding_2 + (M_Retest_Hoarding - M_Hoarding) 
        PTS_Hoarding_3<- Score_Hoarding_3 + (M_Retest_Hoarding - M_Hoarding) 
        PTS_Hoarding<- c(PTS_Hoarding_1, PTS_Hoarding_2, PTS_Hoarding_3)
        PTS_Neutralizing_1<- Score_Neutralizing_1 + (M_Retest_Neutralizing - M_Neutralizing)  
        PTS_Neutralizing_2<- Score_Neutralizing_2 + (M_Retest_Neutralizing - M_Neutralizing) 
        PTS_Neutralizing_3<- Score_Neutralizing_3 + (M_Retest_Neutralizing - M_Neutralizing) 
        PTS_Neutralizing<- c(PTS_Neutralizing_1, PTS_Neutralizing_2, PTS_Neutralizing_3)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Washing<- Score_Washing
        PTS_Checking<- Score_Checking
        PTS_Ordering<- Score_Ordering
        PTS_Obsessing<- Score_Obsessing
        PTS_Hoarding<- Score_Hoarding
        PTS_Neutralizing<- Score_Neutralizing
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS_3<- (B_Adj * Score_3) + A_Adj
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Washing<- Rel_Washing * (SD_Retest_Washing/SD_Washing)
        A_Constant_Washing<- M_Retest_Washing - (B_Slope_Washing * M_Washing)
        B_Adj_Washing<- SD_Retest_Washing/SD_Washing
        A_Adj_Washing<- M_Retest_Washing - (B_Adj_Washing * M_Washing)
        PTS_Washing_1<- (B_Adj_Washing * Score_Washing_1) + A_Adj_Washing
        PTS_Washing_2<- (B_Adj_Washing * Score_Washing_2) + A_Adj_Washing
        PTS_Washing_3<- (B_Adj_Washing * Score_Washing_3) + A_Adj_Washing
        PTS_Washing<- c(PTS_Washing_1,PTS_Washing_2, PTS_Washing_3)
        B_Slope_Checking<- Rel_Checking * (SD_Retest_Checking/SD_Checking)
        A_Constant_Checking<- M_Retest_Checking - (B_Slope_Checking * M_Checking)
        B_Adj_Checking<- SD_Retest_Checking/SD_Checking
        A_Adj_Checking<- M_Retest_Checking - (B_Adj_Checking * M_Checking)
        PTS_Checking_1<- (B_Adj_Checking * Score_Checking_1) + A_Adj_Checking
        PTS_Checking_2<- (B_Adj_Checking * Score_Checking_2) + A_Adj_Checking
        PTS_Checking_3<- (B_Adj_Checking * Score_Checking_3) + A_Adj_Checking
        PTS_Checking<- c(PTS_Checking_1,PTS_Checking_2, PTS_Checking_3)
        B_Slope_Ordering<- Rel_Ordering * (SD_Retest_Ordering/SD_Ordering)
        A_Constant_Ordering<- M_Retest_Ordering - (B_Slope_Ordering * M_Ordering)
        B_Adj_Ordering<- SD_Retest_Ordering/SD_Ordering
        A_Adj_Ordering<- M_Retest_Ordering - (B_Adj_Ordering * M_Ordering)
        PTS_Ordering_1<- (B_Adj_Ordering * Score_Ordering_1) + A_Adj_Ordering
        PTS_Ordering_2<- (B_Adj_Ordering * Score_Ordering_2) + A_Adj_Ordering
        PTS_Ordering_3<- (B_Adj_Ordering * Score_Ordering_3) + A_Adj_Ordering
        PTS_Ordering<- c(PTS_Ordering_1,PTS_Ordering_2, PTS_Ordering_3)
        B_Slope_Obsessing<- Rel_Obsessing * (SD_Retest_Obsessing/SD_Obsessing)
        A_Constant_Obsessing<- M_Retest_Obsessing - (B_Slope_Obsessing * M_Obsessing)
        B_Adj_Obsessing<- SD_Retest_Obsessing/SD_Obsessing
        A_Adj_Obsessing<- M_Retest_Obsessing - (B_Adj_Obsessing * M_Obsessing)
        PTS_Obsessing_1<- (B_Adj_Obsessing * Score_Obsessing_1) + A_Adj_Obsessing
        PTS_Obsessing_2<- (B_Adj_Obsessing * Score_Obsessing_2) + A_Adj_Obsessing
        PTS_Obsessing_3<- (B_Adj_Obsessing * Score_Obsessing_3) + A_Adj_Obsessing
        PTS_Obsessing<- c(PTS_Obsessing_1,PTS_Obsessing_2, PTS_Obsessing_3)
        B_Slope_Hoarding<- Rel_Hoarding * (SD_Retest_Hoarding/SD_Hoarding)
        A_Constant_Hoarding<- M_Retest_Hoarding - (B_Slope_Hoarding * M_Hoarding)
        B_Adj_Hoarding<- SD_Retest_Hoarding/SD_Hoarding
        A_Adj_Hoarding<- M_Retest_Hoarding - (B_Adj_Hoarding * M_Hoarding)
        PTS_Hoarding_1<- (B_Adj_Hoarding * Score_Hoarding_1) + A_Adj_Hoarding
        PTS_Hoarding_2<- (B_Adj_Hoarding * Score_Hoarding_2) + A_Adj_Hoarding
        PTS_Hoarding_3<- (B_Adj_Hoarding * Score_Hoarding_3) + A_Adj_Hoarding
        PTS_Hoarding<- c(PTS_Hoarding_1,PTS_Hoarding_2, PTS_Hoarding_3)
        B_Slope_Neutralizing<- Rel_Neutralizing * (SD_Retest_Neutralizing/SD_Neutralizing)
        A_Constant_Neutralizing<- M_Retest_Neutralizing - (B_Slope_Neutralizing * M_Neutralizing)
        B_Adj_Neutralizing<- SD_Retest_Neutralizing/SD_Neutralizing
        A_Adj_Neutralizing<- M_Retest_Neutralizing - (B_Adj_Neutralizing * M_Neutralizing)
        PTS_Neutralizing_1<- (B_Adj_Neutralizing * Score_Neutralizing_1) + A_Adj_Neutralizing
        PTS_Neutralizing_2<- (B_Adj_Neutralizing * Score_Neutralizing_2) + A_Adj_Neutralizing
        PTS_Neutralizing_3<- (B_Adj_Neutralizing * Score_Neutralizing_3) + A_Adj_Neutralizing
        PTS_Neutralizing<- c(PTS_Neutralizing_1,PTS_Neutralizing_2, PTS_Neutralizing_3)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS_3<- B_Slope*Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Washing<- Rel_Washing * (SD_Retest_Washing/SD_Washing)
        PTS_Washing_1<- B_Slope_Washing * Score_Washing_1
        PTS_Washing_2<- B_Slope_Washing * Score_Washing_2
        PTS_Washing_3<- B_Slope_Washing * Score_Washing_3
        PTS_Washing<- c(PTS_Washing_1, PTS_Washing_2, PTS_Washing_3)
        B_Slope_Checking<- Rel_Checking * (SD_Retest_Checking/SD_Checking)
        PTS_Checking_1<- B_Slope_Checking * Score_Checking_1
        PTS_Checking_2<- B_Slope_Checking * Score_Checking_2
        PTS_Checking_3<- B_Slope_Checking * Score_Checking_3
        PTS_Checking<- c(PTS_Checking_1, PTS_Checking_2, PTS_Checking_3)
        B_Slope_Ordering<- Rel_Ordering * (SD_Retest_Ordering/SD_Ordering)
        PTS_Ordering_1<- B_Slope_Ordering * Score_Ordering_1
        PTS_Ordering_2<- B_Slope_Ordering * Score_Ordering_2
        PTS_Ordering_3<- B_Slope_Ordering * Score_Ordering_3
        PTS_Ordering<- c(PTS_Ordering_1, PTS_Ordering_2, PTS_Ordering_3)
        B_Slope_Obsessing<- Rel_Obsessing * (SD_Retest_Obsessing/SD_Obsessing)
        PTS_Obsessing_1<- B_Slope_Obsessing * Score_Obsessing_1
        PTS_Obsessing_2<- B_Slope_Obsessing * Score_Obsessing_2
        PTS_Obsessing_3<- B_Slope_Obsessing * Score_Obsessing_3
        PTS_Obsessing<- c(PTS_Obsessing_1, PTS_Obsessing_2, PTS_Obsessing_3)
        B_Slope_Hoarding<- Rel_Hoarding * (SD_Retest_Hoarding/SD_Hoarding)
        PTS_Hoarding_1<- B_Slope_Hoarding * Score_Hoarding_1
        PTS_Hoarding_2<- B_Slope_Hoarding * Score_Hoarding_2
        PTS_Hoarding_3<- B_Slope_Hoarding * Score_Hoarding_3
        PTS_Hoarding<- c(PTS_Hoarding_1, PTS_Hoarding_2, PTS_Hoarding_3)
        B_Slope_Neutralizing<- Rel_Neutralizing * (SD_Retest_Neutralizing/SD_Neutralizing)
        PTS_Neutralizing_1<- B_Slope_Neutralizing * Score_Neutralizing_1
        PTS_Neutralizing_2<- B_Slope_Neutralizing * Score_Neutralizing_2
        PTS_Neutralizing_3<- B_Slope_Neutralizing * Score_Neutralizing_3
        PTS_Neutralizing<- c(PTS_Neutralizing_1, PTS_Neutralizing_2, PTS_Neutralizing_3) 
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Washing_1<- Score_Washing_1 + (M_Retest_Washing - M_Washing)
        PTS_Washing_2<- Score_Washing_2 + (M_Retest_Washing - M_Washing)
        PTS_Washing_3<- Score_Washing_3 + (M_Retest_Washing - M_Washing)
        PTS_Washing<- c(PTS_Washing_1, PTS_Washing_2, PTS_Washing_3)
        PTS_Checking_1<- Score_Checking_1 + (M_Retest_Checking - M_Checking)
        PTS_Checking_2<- Score_Checking_2 + (M_Retest_Checking - M_Checking)
        PTS_Checking_3<- Score_Checking_3 + (M_Retest_Checking - M_Checking)
        PTS_Checking<- c(PTS_Checking_1, PTS_Checking_2, PTS_Checking_3)
        PTS_Ordering_1<- Score_Ordering_1 + (M_Retest_Ordering - M_Ordering)
        PTS_Ordering_2<- Score_Ordering_2 + (M_Retest_Ordering - M_Ordering)
        PTS_Ordering_3<- Score_Ordering_3 + (M_Retest_Ordering - M_Ordering)
        PTS_Ordering<- c(PTS_Ordering_1, PTS_Ordering_2, PTS_Ordering_3)
        PTS_Obsessing_1<- Score_Obsessing_1 + (M_Retest_Obsessing - M_Obsessing)
        PTS_Obsessing_2<- Score_Obsessing_2 + (M_Retest_Obsessing - M_Obsessing)
        PTS_Obsessing_3<- Score_Obsessing_3 + (M_Retest_Obsessing - M_Obsessing)
        PTS_Obsessing<- c(PTS_Obsessing_1, PTS_Obsessing_2, PTS_Obsessing_3)
        PTS_Hoarding_1<- Score_Hoarding_1 + (M_Retest_Hoarding - M_Hoarding)
        PTS_Hoarding_2<- Score_Hoarding_2 + (M_Retest_Hoarding - M_Hoarding)
        PTS_Hoarding_3<- Score_Hoarding_3 + (M_Retest_Hoarding - M_Hoarding)
        PTS_Hoarding<- c(PTS_Hoarding_1, PTS_Hoarding_2, PTS_Hoarding_3)
        PTS_Neutralizing_1<- Score_Neutralizing_1 + (M_Retest_Neutralizing - M_Neutralizing)
        PTS_Neutralizing_2<- Score_Neutralizing_2 + (M_Retest_Neutralizing - M_Neutralizing)
        PTS_Neutralizing_3<- Score_Neutralizing_3 + (M_Retest_Neutralizing - M_Neutralizing)
        PTS_Neutralizing<- c(PTS_Neutralizing_1, PTS_Neutralizing_2, PTS_Neutralizing_3)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Washing<- round(PTS_Washing, digits = 2)
      PTS_Checking<- round(PTS_Checking, digits = 2)
      PTS_Ordering<- round(PTS_Ordering, digits = 2)
      PTS_Obsessing<- round(PTS_Obsessing, digits = 2)
      PTS_Hoarding<- round(PTS_Hoarding, digits = 2)
      PTS_Neutralizing<- round(PTS_Neutralizing, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE3<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_3 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2, SE3)
        SE_Washing_1<- McSweeny_SE_Washing*sqrt(1 + (1/SampleN) + ((Score_Washing_1 - M_Washing)^2/(SD_Washing^2*(SampleN-1))))
        SE_Washing_2<- McSweeny_SE_Washing*sqrt(1 + (1/SampleN) + ((Score_Washing_2 - M_Washing)^2/(SD_Washing^2*(SampleN-1))))
        SE_Washing_3<- McSweeny_SE_Washing*sqrt(1 + (1/SampleN) + ((Score_Washing_3 - M_Washing)^2/(SD_Washing^2*(SampleN-1))))
        SE_Washing<-c(SE_Washing_1, SE_Washing_2, SE_Washing_3)
        SE_Checking_1<- McSweeny_SE_Checking*sqrt(1 + (1/SampleN) + ((Score_Checking_1 - M_Checking)^2/(SD_Checking^2*(SampleN-1))))
        SE_Checking_2<- McSweeny_SE_Checking*sqrt(1 + (1/SampleN) + ((Score_Checking_2 - M_Checking)^2/(SD_Checking^2*(SampleN-1))))
        SE_Checking_3<- McSweeny_SE_Checking*sqrt(1 + (1/SampleN) + ((Score_Checking_3 - M_Checking)^2/(SD_Checking^2*(SampleN-1))))
        SE_Checking<-c(SE_Checking_1, SE_Checking_2, SE_Checking_3)
        SE_Ordering_1<- McSweeny_SE_Ordering*sqrt(1 + (1/SampleN) + ((Score_Ordering_1 - M_Ordering)^2/(SD_Ordering^2*(SampleN-1))))
        SE_Ordering_2<- McSweeny_SE_Ordering*sqrt(1 + (1/SampleN) + ((Score_Ordering_2 - M_Ordering)^2/(SD_Ordering^2*(SampleN-1))))
        SE_Ordering_3<- McSweeny_SE_Ordering*sqrt(1 + (1/SampleN) + ((Score_Ordering_3 - M_Ordering)^2/(SD_Ordering^2*(SampleN-1))))
        SE_Ordering<-c(SE_Ordering_1, SE_Ordering_2, SE_Ordering_3)
        SE_Obsessing_1<- McSweeny_SE_Obsessing*sqrt(1 + (1/SampleN) + ((Score_Obsessing_1 - M_Obsessing)^2/(SD_Obsessing^2*(SampleN-1))))
        SE_Obsessing_2<- McSweeny_SE_Obsessing*sqrt(1 + (1/SampleN) + ((Score_Obsessing_2 - M_Obsessing)^2/(SD_Obsessing^2*(SampleN-1))))
        SE_Obsessing_3<- McSweeny_SE_Obsessing*sqrt(1 + (1/SampleN) + ((Score_Obsessing_3 - M_Obsessing)^2/(SD_Obsessing^2*(SampleN-1))))
        SE_Obsessing<-c(SE_Obsessing_1, SE_Obsessing_2, SE_Obsessing_3)
        SE_Hoarding_1<- McSweeny_SE_Hoarding*sqrt(1 + (1/SampleN) + ((Score_Hoarding_1 - M_Hoarding)^2/(SD_Hoarding^2*(SampleN-1))))
        SE_Hoarding_2<- McSweeny_SE_Hoarding*sqrt(1 + (1/SampleN) + ((Score_Hoarding_2 - M_Hoarding)^2/(SD_Hoarding^2*(SampleN-1))))
        SE_Hoarding_3<- McSweeny_SE_Hoarding*sqrt(1 + (1/SampleN) + ((Score_Hoarding_3 - M_Hoarding)^2/(SD_Hoarding^2*(SampleN-1))))
        SE_Hoarding<-c(SE_Hoarding_1, SE_Hoarding_2, SE_Hoarding_3)
        SE_Neutralizing_1<- McSweeny_SE_Neutralizing*sqrt(1 + (1/SampleN) + ((Score_Neutralizing_1 - M_Neutralizing)^2/(SD_Neutralizing^2*(SampleN-1))))
        SE_Neutralizing_2<- McSweeny_SE_Neutralizing*sqrt(1 + (1/SampleN) + ((Score_Neutralizing_2 - M_Neutralizing)^2/(SD_Neutralizing^2*(SampleN-1))))
        SE_Neutralizing_3<- McSweeny_SE_Neutralizing*sqrt(1 + (1/SampleN) + ((Score_Neutralizing_3 - M_Neutralizing)^2/(SD_Neutralizing^2*(SampleN-1))))
        SE_Neutralizing<-c(SE_Neutralizing_1, SE_Neutralizing_2, SE_Neutralizing_3)
        SE<- round(SE, digits = 2)
        SE_Washing<- round(SE_Washing, digits = 2)
        SE_Checking<- round(SE_Checking, digits = 2)
        SE_Ordering<- round(SE_Ordering, digits = 2)
        SE_Obsessing<- round(SE_Obsessing, digits = 2)
        SE_Hoarding<- round(SE_Hoarding, digits = 2)
        SE_Neutralizing<- round(SE_Neutralizing, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2), (Conf*SE3))
        CI<- round(CI, digits = 2)
        CI_Washing<- c((Conf*SE_Washing_1), (Conf*SE_Washing_2), (Conf*SE_Washing_3))
        CI_Washing<- round(CI_Washing, digits = 2)
        CI_Checking<- c((Conf*SE_Checking_1), (Conf*SE_Checking_2), (Conf*SE_Checking_3))
        CI_Checking<- round(CI_Checking, digits = 2)
        CI_Ordering<- c((Conf*SE_Ordering_1), (Conf*SE_Ordering_2), (Conf*SE_Ordering_3))
        CI_Ordering<- round(CI_Ordering, digits = 2)
        CI_Obsessing<- c((Conf*SE_Obsessing_1), (Conf*SE_Obsessing_2), (Conf*SE_Obsessing_3))
        CI_Obsessing<- round(CI_Obsessing, digits = 2)
        CI_Hoarding<- c((Conf*SE_Hoarding_1), (Conf*SE_Hoarding_2), (Conf*SE_Hoarding_3))
        CI_Hoarding<- round(CI_Hoarding, digits = 2)
        CI_Neutralizing<- c((Conf*SE_Neutralizing_1), (Conf*SE_Neutralizing_2), (Conf*SE_Neutralizing_3))
        CI_Neutralizing<- round(CI_Neutralizing, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Washing<- c((Conf*SE_Washing), (Conf*SE_Washing), (Conf*SE_Washing))
        CI_Washing<- round(CI_Washing, digits = 2)
        CI_Checking<- c((Conf*SE_Checking), (Conf*SE_Checking), (Conf*SE_Checking))
        CI_Checking<- round(CI_Checking, digits = 2)
        CI_Ordering<- c((Conf*SE_Ordering), (Conf*SE_Ordering), (Conf*SE_Ordering))
        CI_Ordering<- round(CI_Ordering, digits = 2)
        CI_Obsessing<- c((Conf*SE_Obsessing), (Conf*SE_Obsessing), (Conf*SE_Obsessing))
        CI_Obsessing<- round(CI_Obsessing, digits = 2)
        CI_Hoarding<- c((Conf*SE_Hoarding), (Conf*SE_Hoarding), (Conf*SE_Hoarding))
        CI_Hoarding<- round(CI_Hoarding, digits = 2)
        CI_Neutralizing<- c((Conf*SE_Neutralizing), (Conf*SE_Neutralizing), (Conf*SE_Neutralizing))
        CI_Neutralizing<- round(CI_Neutralizing, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Washing<- PTS_Washing + CI_Washing
      CI_Upper_Lim_Washing<- round(CI_Upper_Lim_Washing, digits = 2)
      CI_Lower_Lim_Washing<-PTS_Washing - CI_Washing
      CI_Lower_Lim_Washing<- round(CI_Lower_Lim_Washing, digits = 2)
      CI_Upper_Lim_Checking<- PTS_Checking + CI_Checking
      CI_Upper_Lim_Checking<- round(CI_Upper_Lim_Checking, digits = 2)
      CI_Lower_Lim_Checking<-PTS_Checking - CI_Checking
      CI_Lower_Lim_Checking<- round(CI_Lower_Lim_Checking, digits = 2)
      CI_Upper_Lim_Ordering<- PTS_Ordering + CI_Ordering
      CI_Upper_Lim_Ordering<- round(CI_Upper_Lim_Ordering, digits = 2)
      CI_Lower_Lim_Ordering<-PTS_Ordering - CI_Ordering
      CI_Lower_Lim_Ordering<- round(CI_Lower_Lim_Ordering, digits = 2)
      CI_Upper_Lim_Obsessing<- PTS_Obsessing + CI_Obsessing
      CI_Upper_Lim_Obsessing<- round(CI_Upper_Lim_Obsessing, digits = 2)
      CI_Lower_Lim_Obsessing<-PTS_Obsessing - CI_Obsessing
      CI_Lower_Lim_Obsessing<- round(CI_Lower_Lim_Obsessing, digits = 2)
      CI_Upper_Lim_Hoarding<- PTS_Hoarding + CI_Hoarding
      CI_Upper_Lim_Hoarding<- round(CI_Upper_Lim_Hoarding, digits = 2)
      CI_Lower_Lim_Hoarding<-PTS_Hoarding - CI_Hoarding
      CI_Lower_Lim_Hoarding<- round(CI_Lower_Lim_Hoarding, digits = 2)
      CI_Upper_Lim_Neutralizing<- PTS_Neutralizing + CI_Neutralizing
      CI_Upper_Lim_Neutralizing<- round(CI_Upper_Lim_Neutralizing, digits = 2)
      CI_Lower_Lim_Neutralizing<-PTS_Neutralizing - CI_Neutralizing
      CI_Lower_Lim_Neutralizing<- round(CI_Lower_Lim_Neutralizing, digits = 2)
    
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Washing == "2") {
        CI_Washing<- input$Man_CI_Washing
        CI_Washing<- c(CI_Washing, CI_Washing, CI_Washing)
        CI_Washing<- round(CI_Washing, digits = 2)
        CI_Upper_Lim_Washing<- Score_Washing + CI_Washing
        CI_Upper_Lim_Washing<- round(CI_Upper_Lim_Washing, digits = 2)
        CI_Lower_Lim_Washing<- Score_Washing - CI_Washing
        CI_Lower_Lim_Washing<- round(CI_Lower_Lim_Washing, digits = 2)
      }
      if(input$Select_CI_Checking == "2") {
        CI_Checking<- input$Man_CI_Checking
        CI_Checking<- c(CI_Checking, CI_Checking, CI_Checking)
        CI_Checking<- round(CI_Checking, digits = 2)
        CI_Upper_Lim_Checking<- Score_Checking + CI_Checking
        CI_Upper_Lim_Checking<- round(CI_Upper_Lim_Checking, digits = 2)
        CI_Lower_Lim_Checking<- Score_Checking - CI_Checking
        CI_Lower_Lim_Checking<- round(CI_Lower_Lim_Checking, digits = 2)
      }
      if(input$Select_CI_Ordering == "2") {
        CI_Ordering<- input$Man_CI_Ordering
        CI_Ordering<- c(CI_Ordering, CI_Ordering, CI_Ordering)
        CI_Ordering<- round(CI_Ordering, digits = 2)
        CI_Upper_Lim_Ordering<- Score_Ordering + CI_Ordering
        CI_Upper_Lim_Ordering<- round(CI_Upper_Lim_Ordering, digits = 2)
        CI_Lower_Lim_Ordering<- Score_Ordering - CI_Ordering
        CI_Lower_Lim_Ordering<- round(CI_Lower_Lim_Ordering, digits = 2)
      }
      if(input$Select_CI_Obsessing == "2") {
        CI_Obsessing<- input$Man_CI_Obsessing
        CI_Obsessing<- c(CI_Obsessing,  CI_Obsessing, CI_Obsessing)
        CI_Obsessing<- round(CI_Obsessing, digits = 2)
        CI_Upper_Lim_Obsessing<- Score_Obsessing + CI_Obsessing
        CI_Upper_Lim_Obsessing<- round(CI_Upper_Lim_Obsessing, digits = 2)
        CI_Lower_Lim_Obsessing<- Score_Obsessing - CI_Obsessing
        CI_Lower_Lim_Obsessing<- round(CI_Lower_Lim_Obsessing, digits = 2)
      }
      if(input$Select_CI_Hoarding == "2") {
        CI_Hoarding<- input$Man_CI_Hoarding
        CI_Hoarding<- c(CI_Hoarding, CI_Hoarding, CI_Hoarding)
        CI_Hoarding<- round(CI_Hoarding, digits = 2)
        CI_Upper_Lim_Hoarding<- Score_Hoarding + CI_Hoarding
        CI_Upper_Lim_Hoarding<- round(CI_Upper_Lim_Hoarding, digits = 2)
        CI_Lower_Lim_Hoarding<- Score_Hoarding - CI_Hoarding
        CI_Lower_Lim_Hoarding<- round(CI_Lower_Lim_Hoarding, digits = 2)
      }
      if(input$Select_CI_Neutralizing == "2") {
        CI_Neutralizing<- input$Man_CI_Neutralizing
        CI_Neutralizing<- c(CI_Neutralizing, CI_Neutralizing, CI_Neutralizing)
        CI_Neutralizing<- round(CI_Neutralizing, digits = 2)
        CI_Upper_Lim_Neutralizing<- Score_Neutralizing + CI_Neutralizing
        CI_Upper_Lim_Neutralizing<- round(CI_Upper_Lim_Neutralizing, digits = 2)
        CI_Lower_Lim_Neutralizing<- Score_Neutralizing - CI_Neutralizing
        CI_Lower_Lim_Neutralizing<- round(CI_Lower_Lim_Neutralizing, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Washing_1<- round(input$Cutoff_Washing_1, digits = 2)
      Cutoff_Score_Washing_2<- round(input$Cutoff_Washing_2, digits = 2)
      Cutoff_Score_Washing_3<- round(input$Cutoff_Washing_3, digits = 2)
      Cutoff_Score_Checking_1<- round(input$Cutoff_Checking_1, digits = 2)
      Cutoff_Score_Checking_2<- round(input$Cutoff_Checking_2, digits = 2)
      Cutoff_Score_Checking_3<- round(input$Cutoff_Checking_3, digits = 2)
      Cutoff_Score_Ordering_1<- round(input$Cutoff_Ordering_1, digits = 2)
      Cutoff_Score_Ordering_2<- round(input$Cutoff_Ordering_2, digits = 2)
      Cutoff_Score_Ordering_3<- round(input$Cutoff_Ordering_3, digits = 2)
      Cutoff_Score_Obsessing_1<- round(input$Cutoff_Obsessing_1, digits = 2)
      Cutoff_Score_Obsessing_2<- round(input$Cutoff_Obsessing_2, digits = 2)
      Cutoff_Score_Obsessing_3<- round(input$Cutoff_Obsessing_3, digits = 2)
      Cutoff_Score_Hoarding_1<- round(input$Cutoff_Hoarding_1, digits = 2)
      Cutoff_Score_Hoarding_2<- round(input$Cutoff_Hoarding_2, digits = 2)
      Cutoff_Score_Hoarding_3<- round(input$Cutoff_Hoarding_3, digits = 2)
      Cutoff_Score_Neutralizing_1<- round(input$Cutoff_Neutralizing_1, digits = 2)
      Cutoff_Score_Neutralizing_2<- round(input$Cutoff_Neutralizing_2, digits = 2)
      Cutoff_Score_Neutralizing_3<- round(input$Cutoff_Neutralizing_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Washing,Change_Washing,PTS_Washing, SE_Washing, CI_Upper_Lim_Washing, CI_Lower_Lim_Washing, Cutoff_Score_Washing_1,Cutoff_Score_Washing_2,Cutoff_Score_Washing_3,
                                      Score_Checking,Change_Checking, PTS_Checking, SE_Checking, CI_Upper_Lim_Checking, CI_Lower_Lim_Checking, Cutoff_Score_Checking_1,Cutoff_Score_Checking_2,Cutoff_Score_Checking_3, 
                                      Score_Ordering,Change_Ordering,PTS_Ordering, SE_Ordering, CI_Upper_Lim_Ordering, CI_Lower_Lim_Ordering, Cutoff_Score_Ordering_1,Cutoff_Score_Ordering_2,Cutoff_Score_Ordering_3, 
                                      Score_Obsessing,Change_Obsessing,PTS_Obsessing, SE_Obsessing, CI_Upper_Lim_Obsessing, CI_Lower_Lim_Obsessing, Cutoff_Score_Obsessing_1,Cutoff_Score_Obsessing_2,Cutoff_Score_Obsessing_3,
                                      Score_Hoarding,Change_Hoarding, PTS_Hoarding, SE_Hoarding, CI_Upper_Lim_Hoarding, CI_Lower_Lim_Hoarding, Cutoff_Score_Hoarding_1,Cutoff_Score_Hoarding_2,Cutoff_Score_Hoarding_3, 
                                      Score_Neutralizing,Change_Neutralizing,PTS_Neutralizing, SE_Neutralizing, CI_Upper_Lim_Neutralizing, CI_Lower_Lim_Neutralizing, Cutoff_Score_Neutralizing_1,Cutoff_Score_Neutralizing_2,Cutoff_Score_Neutralizing_3 
                                      )
    }
    
    
    #Create dataframes to be displayed as tables in pdf report (statistical values used in reliable change analyses)
    
    if(input$RelChangeMethod == 'Nunnally & Bernstein (1994)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Washing<<- data.frame(Pop,  M_Washing, SD_Washing, RelChangeMethod, Rel_Washing, ConfInt)
      names(Stats_Table_Washing)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Checking<<- data.frame(Pop,  M_Checking, SD_Checking, RelChangeMethod, Rel_Checking, ConfInt)
      names(Stats_Table_Checking)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Ordering<<- data.frame(Pop,  M_Ordering, SD_Ordering, RelChangeMethod, Rel_Ordering, ConfInt)
      names(Stats_Table_Ordering)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Obsessing<<- data.frame(Pop,  M_Obsessing, SD_Obsessing, RelChangeMethod, Rel_Obsessing, ConfInt)
      names(Stats_Table_Obsessing)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Hoarding<<- data.frame(Pop,  M_Hoarding, SD_Hoarding, RelChangeMethod, Rel_Hoarding, ConfInt)
      names(Stats_Table_Hoarding)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Neutralizing<<- data.frame(Pop,  M_Neutralizing, SD_Neutralizing, RelChangeMethod, Rel_Neutralizing, ConfInt)
      names(Stats_Table_Neutralizing)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Chelune et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Washing<<- data.frame(Pop,  M_Washing, M_Retest_Washing, SD_Washing, RelChangeMethod, Rel_Washing, ConfInt)
      names(Stats_Table_Washing)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Checking<<- data.frame(Pop,  M_Checking, M_Retest_Checking, SD_Checking, RelChangeMethod, Rel_Checking, ConfInt)
      names(Stats_Table_Checking)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Ordering<<- data.frame(Pop,  M_Ordering, M_Retest_Ordering, SD_Ordering, RelChangeMethod, Rel_Ordering, ConfInt)
      names(Stats_Table_Ordering)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
     
      Stats_Table_Obsessing<<- data.frame(Pop,  M_Obsessing, M_Retest_Obsessing, SD_Obsessing, RelChangeMethod, Rel_Obsessing, ConfInt)
      names(Stats_Table_Obsessing)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Hoarding<<- data.frame(Pop,  M_Hoarding, M_Retest_Hoarding, SD_Hoarding, RelChangeMethod, Rel_Hoarding, ConfInt)
      names(Stats_Table_Hoarding)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Neutralizing<<- data.frame(Pop,  M_Neutralizing, M_Retest_Neutralizing, SD_Neutralizing, RelChangeMethod, Rel_Neutralizing, ConfInt)
      names(Stats_Table_Neutralizing)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    
    } else if (input$RelChangeMethod == 'Maassen et al. (2006)' | input$RelChangeMethod == 'McSweeny et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Washing<<- data.frame(Pop,  M_Washing, M_Retest_Washing, SD_Washing, SD_Retest_Washing, RelChangeMethod, Rel_Washing, ConfInt)
      names(Stats_Table_Washing)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Checking<<- data.frame(Pop,  M_Checking, M_Retest_Checking, SD_Checking, SD_Retest_Checking, RelChangeMethod, Rel_Checking, ConfInt)
      names(Stats_Table_Checking)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Ordering<<- data.frame(Pop,  M_Ordering, M_Retest_Ordering, SD_Ordering, SD_Retest_Ordering, RelChangeMethod, Rel_Ordering, ConfInt)
      names(Stats_Table_Ordering)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Obsessing<<- data.frame(Pop,  M_Obsessing, M_Retest_Obsessing, SD_Obsessing, SD_Retest_Obsessing, RelChangeMethod, Rel_Obsessing, ConfInt)
      names(Stats_Table_Obsessing)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Hoarding<<- data.frame(Pop,  M_Hoarding, M_Retest_Hoarding, SD_Hoarding, SD_Retest_Hoarding, RelChangeMethod, Rel_Hoarding, ConfInt)
      names(Stats_Table_Hoarding)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Neutralizing<<- data.frame(Pop,  M_Neutralizing, M_Retest_Neutralizing, SD_Neutralizing, SD_Retest_Neutralizing, RelChangeMethod, Rel_Neutralizing, ConfInt)
      names(Stats_Table_Neutralizing)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
    
    } else if (input$RelChangeMethod == 'Crawford & Howell (1998)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, SampleN, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Washing<<- data.frame(Pop,  M_Washing, M_Retest_Washing, SD_Washing, SD_Retest_Washing, RelChangeMethod, Rel_Washing, SampleN,ConfInt)
      names(Stats_Table_Washing)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Checking<<- data.frame(Pop,  M_Checking, M_Retest_Checking, SD_Checking, SD_Retest_Checking, RelChangeMethod, Rel_Checking, SampleN, ConfInt)
      names(Stats_Table_Checking)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Ordering<<- data.frame(Pop,  M_Ordering, M_Retest_Ordering, SD_Ordering, SD_Retest_Ordering, RelChangeMethod, Rel_Ordering, SampleN,ConfInt)
      names(Stats_Table_Ordering)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Obsessing<<- data.frame(Pop,  M_Obsessing, M_Retest_Obsessing, SD_Obsessing, SD_Retest_Obsessing, RelChangeMethod, Rel_Obsessing, SampleN,ConfInt)
      names(Stats_Table_Obsessing)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Hoarding<<- data.frame(Pop,  M_Hoarding, M_Retest_Hoarding, SD_Hoarding, SD_Retest_Hoarding, RelChangeMethod, Rel_Hoarding, SampleN, ConfInt)
      names(Stats_Table_Hoarding)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Neutralizing<<- data.frame(Pop,  M_Neutralizing, M_Retest_Neutralizing, SD_Neutralizing, SD_Retest_Neutralizing, RelChangeMethod, Rel_Neutralizing, SampleN,ConfInt)
      names(Stats_Table_Neutralizing)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
    } else if (input$RelChangeMethod == 'Jacobson & Truax (1991)' | input$RelChangeMethod == 'Speer (1992)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Washing<<- data.frame(Pop,  SD_Washing, RelChangeMethod, Rel_Washing, ConfInt)
      names(Stats_Table_Washing)<<- c("Reference Population","Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Checking<<- data.frame(Pop,  SD_Checking, RelChangeMethod, Rel_Checking, ConfInt)
      names(Stats_Table_Checking)<<- c("Reference Population","Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Ordering<<- data.frame(Pop,  SD_Ordering, RelChangeMethod, Rel_Ordering, ConfInt)
      names(Stats_Table_Ordering)<<- c("Reference Population","Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Obsessing<<- data.frame(Pop,  SD_Obsessing, RelChangeMethod, Rel_Obsessing, ConfInt)
      names(Stats_Table_Obsessing)<<- c("Reference Population","Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Hoarding<<- data.frame(Pop,  SD_Hoarding, RelChangeMethod, Rel_Hoarding, ConfInt)
      names(Stats_Table_Hoarding)<<- c("Reference Population","Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Neutralizing<<- data.frame(Pop,  SD_Neutralizing, RelChangeMethod, Rel_Neutralizing, ConfInt)
      names(Stats_Table_Neutralizing)<<- c("Reference Population","Sd", "Reliable Change Method", "Reliability", "Confidence")
  
    }
    
    #If custom confidence intervals options are selected, make sure that no values appear for PS and SE in pdf tables
    #Change the look of tables in report if custom confidence intervals options are selected
    
    if (input$Select_CI == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS = NA, SE = NA)
      Stats_Table_Fullscale<<- Stats_Table_Fullscale %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI[1])
    }
    if (input$Select_CI_Washing == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Washing = NA, SE_Washing = NA)
      Stats_Table_Washing<<- Stats_Table_Washing %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                    "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Washing[1])
    }
    if (input$Select_CI_Checking == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Checking = NA, SE_Checking = NA)
      Stats_Table_Checking<<- Stats_Table_Checking %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                                        "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Checking[1])
    }
    if (input$Select_CI_Ordering == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Ordering = NA, SE_Ordering = NA)
      Stats_Table_Ordering<<- Stats_Table_Ordering %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                    "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Ordering[1])
    }
    if (input$Select_CI_Obsessing == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Obsessing = NA, SE_Obsessing = NA)
      Stats_Table_Obsessing<<- Stats_Table_Obsessing %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                  "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Obsessing[1])
    }
    if (input$Select_CI_Hoarding == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Hoarding = NA, SE_Hoarding = NA)
      Stats_Table_Hoarding<<- Stats_Table_Hoarding %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                                  "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Hoarding[1])
    }
    if (input$Select_CI_Neutralizing == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Neutralizing = NA, SE_Neutralizing = NA)
      Stats_Table_Neutralizing<<- Stats_Table_Neutralizing %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Neutralizing[1])
    }
    
    
    
    #Create a dataframe to be used in the analytics spreadsheet.
    
    #Calculate variables for significant improvement
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$CI_Lower_Lim[1]) {
      OCIR.Fullscale.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$CI_Lower_Lim[1]) {
      OCIR.Fullscale.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$CI_Upper_Lim[1]) {
      OCIR.Fullscale.Sig.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$CI_Upper_Lim[1]) {
      OCIR.Fullscale.Sig.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Washing[length(Entered_Scores_Df$Score_Washing)] < Entered_Scores_Df$CI_Lower_Lim_Washing[1]) {
      OCIR.Washing.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Washing[length(Entered_Scores_Df$Score_Washing)] >= Entered_Scores_Df$CI_Lower_Lim_Washing[1]) {
      OCIR.Washing.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Washing[length(Entered_Scores_Df$Score_Washing)] > Entered_Scores_Df$CI_Upper_Lim_Washing[1]) {
      OCIR.Washing.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Washing[length(Entered_Scores_Df$Score_Washing)] <= Entered_Scores_Df$CI_Upper_Lim_Washing[1]) {
      OCIR.Washing.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Checking[length(Entered_Scores_Df$Score_Checking)] < Entered_Scores_Df$CI_Lower_Lim_Checking[1]) {
      OCIR.Checking.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Checking[length(Entered_Scores_Df$Score_Checking)] >= Entered_Scores_Df$CI_Lower_Lim_Checking[1]) {
      OCIR.Checking.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Checking[length(Entered_Scores_Df$Score_Checking)] > Entered_Scores_Df$CI_Upper_Lim_Checking[1]) {
      OCIR.Checking.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Checking[length(Entered_Scores_Df$Score_Checking)] <= Entered_Scores_Df$CI_Upper_Lim_Checking[1]) {
      OCIR.Checking.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Ordering[length(Entered_Scores_Df$Score_Ordering)] < Entered_Scores_Df$CI_Lower_Lim_Ordering[1]) {
      OCIR.Ordering.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Ordering[length(Entered_Scores_Df$Score_Ordering)] >= Entered_Scores_Df$CI_Lower_Lim_Ordering[1]) {
      OCIR.Ordering.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Ordering[length(Entered_Scores_Df$Score_Ordering)] > Entered_Scores_Df$CI_Upper_Lim_Ordering[1]) {
      OCIR.Ordering.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Ordering[length(Entered_Scores_Df$Score_Ordering)] <= Entered_Scores_Df$CI_Upper_Lim_Ordering[1]) {
      OCIR.Ordering.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Obsessing[length(Entered_Scores_Df$Score_Obsessing)] < Entered_Scores_Df$CI_Lower_Lim_Obsessing[1]) {
      OCIR.Obsessing.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Obsessing[length(Entered_Scores_Df$Score_Obsessing)] >= Entered_Scores_Df$CI_Lower_Lim_Obsessing[1]) {
      OCIR.Obsessing.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Obsessing[length(Entered_Scores_Df$Score_Obsessing)] > Entered_Scores_Df$CI_Upper_Lim_Obsessing[1]) {
      OCIR.Obsessing.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Obsessing[length(Entered_Scores_Df$Score_Obsessing)] <= Entered_Scores_Df$CI_Upper_Lim_Obsessing[1]) {
      OCIR.Obsessing.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Hoarding[length(Entered_Scores_Df$Score_Hoarding)] < Entered_Scores_Df$CI_Lower_Lim_Hoarding[1]) {
      OCIR.Hoarding.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Hoarding[length(Entered_Scores_Df$Score_Hoarding)] >= Entered_Scores_Df$CI_Lower_Lim_Hoarding[1]) {
      OCIR.Hoarding.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Hoarding[length(Entered_Scores_Df$Score_Hoarding)] > Entered_Scores_Df$CI_Upper_Lim_Hoarding[1]) {
      OCIR.Hoarding.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Hoarding[length(Entered_Scores_Df$Score_Hoarding)] <= Entered_Scores_Df$CI_Upper_Lim_Hoarding[1]) {
      OCIR.Hoarding.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Neutralizing[length(Entered_Scores_Df$Score_Neutralizing)] < Entered_Scores_Df$CI_Lower_Lim_Neutralizing[1]) {
      OCIR.Neutralizing.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Neutralizing[length(Entered_Scores_Df$Score_Neutralizing)] >= Entered_Scores_Df$CI_Lower_Lim_Neutralizing[1]) {
      OCIR.Neutralizing.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Neutralizing[length(Entered_Scores_Df$Score_Neutralizing)] > Entered_Scores_Df$CI_Upper_Lim_Neutralizing[1]) {
      OCIR.Neutralizing.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Neutralizing[length(Entered_Scores_Df$Score_Neutralizing)] <= Entered_Scores_Df$CI_Upper_Lim_Neutralizing[1]) {
      OCIR.Neutralizing.Sig.Deterioration<- "No"
    }
    
   
    #Calculate variables for improvement
    
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$Score[1]) {
      OCIR.Fullscale.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$Score[1]) {
      OCIR.Fullscale.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$Score[1]) {
      OCIR.Fullscale.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$Score[1]) {
      OCIR.Fullscale.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Washing[length(Entered_Scores_Df$Score_Washing)] < Entered_Scores_Df$Score_Washing[1]) {
      OCIR.Washing.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Washing[length(Entered_Scores_Df$Score_Washing)] >= Entered_Scores_Df$Score_Washing[1]) {
      OCIR.Washing.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Washing[length(Entered_Scores_Df$Score_Washing)] > Entered_Scores_Df$Score_Washing[1]) {
      OCIR.Washing.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Washing[length(Entered_Scores_Df$Score_Washing)] <= Entered_Scores_Df$Score_Washing[1]) {
      OCIR.Washing.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Checking[length(Entered_Scores_Df$Score_Checking)] < Entered_Scores_Df$Score_Checking[1]) {
      OCIR.Checking.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Checking[length(Entered_Scores_Df$Score_Checking)] >= Entered_Scores_Df$Score_Checking[1]) {
      OCIR.Checking.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Checking[length(Entered_Scores_Df$Score_Checking)] > Entered_Scores_Df$Score_Checking[1]) {
      OCIR.Checking.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Checking[length(Entered_Scores_Df$Score_Checking)] <= Entered_Scores_Df$Score_Checking[1]) {
      OCIR.Checking.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Ordering[length(Entered_Scores_Df$Score_Ordering)] < Entered_Scores_Df$Score_Ordering[1]) {
      OCIR.Ordering.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Ordering[length(Entered_Scores_Df$Score_Ordering)] >= Entered_Scores_Df$Score_Ordering[1]) {
      OCIR.Ordering.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Ordering[length(Entered_Scores_Df$Score_Ordering)] > Entered_Scores_Df$Score_Ordering[1]) {
      OCIR.Ordering.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Ordering[length(Entered_Scores_Df$Score_Ordering)] <= Entered_Scores_Df$Score_Ordering[1]) {
      OCIR.Ordering.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Obsessing[length(Entered_Scores_Df$Score_Obsessing)] < Entered_Scores_Df$Score_Obsessing[1]) {
      OCIR.Obsessing.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Obsessing[length(Entered_Scores_Df$Score_Obsessing)] >= Entered_Scores_Df$Score_Obsessing[1]) {
      OCIR.Obsessing.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Obsessing[length(Entered_Scores_Df$Score_Obsessing)] > Entered_Scores_Df$Score_Obsessing[1]) {
      OCIR.Obsessing.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Obsessing[length(Entered_Scores_Df$Score_Obsessing)] <= Entered_Scores_Df$Score_Obsessing[1]) {
      OCIR.Obsessing.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Hoarding[length(Entered_Scores_Df$Score_Hoarding)] < Entered_Scores_Df$Score_Hoarding[1]) {
      OCIR.Hoarding.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Hoarding[length(Entered_Scores_Df$Score_Hoarding)] >= Entered_Scores_Df$Score_Hoarding[1]) {
      OCIR.Hoarding.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Hoarding[length(Entered_Scores_Df$Score_Hoarding)] > Entered_Scores_Df$Score_Hoarding[1]) {
      OCIR.Hoarding.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Hoarding[length(Entered_Scores_Df$Score_Hoarding)] <= Entered_Scores_Df$Score_Hoarding[1]) {
      OCIR.Hoarding.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Neutralizing[length(Entered_Scores_Df$Score_Neutralizing)] < Entered_Scores_Df$Score_Neutralizing[1]) {
      OCIR.Neutralizing.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Neutralizing[length(Entered_Scores_Df$Score_Neutralizing)] >= Entered_Scores_Df$Score_Neutralizing[1]) {
      OCIR.Neutralizing.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Neutralizing[length(Entered_Scores_Df$Score_Neutralizing)] > Entered_Scores_Df$Score_Neutralizing[1]) {
      OCIR.Neutralizing.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Neutralizing[length(Entered_Scores_Df$Score_Neutralizing)] <= Entered_Scores_Df$Score_Neutralizing[1]) {
      OCIR.Neutralizing.Deterioration<- "No"
    }
    
   
    OCIR.Fullscale.Change<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] - Entered_Scores_Df$Score[1]
    OCIR.Washing.Change<- Entered_Scores_Df$Score_Washing[length(Entered_Scores_Df$Score_Washing)] - Entered_Scores_Df$Score_Washing[1]
    OCIR.Checking.Change<- Entered_Scores_Df$Score_Checking[length(Entered_Scores_Df$Score_Checking)] - Entered_Scores_Df$Score_Checking[1]
    OCIR.Ordering.Change<- Entered_Scores_Df$Score_Ordering[length(Entered_Scores_Df$Score_Ordering)] - Entered_Scores_Df$Score_Ordering[1]
    OCIR.Fullscale.Comparisons<- length(Entered_Scores_Df$Change) - 1
    OCIR.Washing.Comparisons<- length(Entered_Scores_Df$Change_Washing) - 1
    OCIR.Checking.Comparisons<- length(Entered_Scores_Df$Change_Checking) - 1
    OCIR.Ordering.Comparisons<- length(Entered_Scores_Df$Change_Ordering) - 1
    OCIR.Fullscale.First.Date<- Entered_Scores_Df$Date[1]
    OCIR.Washing.First.Date<- Entered_Scores_Df$Date[1]
    OCIR.Checking.First.Date<- Entered_Scores_Df$Date[1]
    OCIR.Ordering.First.Date<- Entered_Scores_Df$Date[1]
    OCIR.Fullscale.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    OCIR.Washing.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    OCIR.Checking.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    OCIR.Ordering.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    OCIR.Fullscale.First.Score<- Entered_Scores_Df$Score[1]
    OCIR.Washing.First.Score<- Entered_Scores_Df$Score_Washing[1]
    OCIR.Checking.First.Score<- Entered_Scores_Df$Score_Checking[1]
    OCIR.Ordering.First.Score<- Entered_Scores_Df$Score_Ordering[1]
    OCIR.Fullscale.Last.Score<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)]
    OCIR.Washing.Last.Score<- Entered_Scores_Df$Score_Washing[length(Entered_Scores_Df$Score_Washing)]
    OCIR.Checking.Last.Score<- Entered_Scores_Df$Score_Checking[length(Entered_Scores_Df$Score_Checking)]
    OCIR.Ordering.Last.Score<- Entered_Scores_Df$Score_Ordering[length(Entered_Scores_Df$Score_Ordering)]

    
    OCIR.Obsessing.Change<- Entered_Scores_Df$Score_Obsessing[length(Entered_Scores_Df$Score_Obsessing)] - Entered_Scores_Df$Score_Obsessing[1]
    OCIR.Hoarding.Change<- Entered_Scores_Df$Score_Hoarding[length(Entered_Scores_Df$Score_Hoarding)] - Entered_Scores_Df$Score_Hoarding[1]
    OCIR.Neutralizing.Change<- Entered_Scores_Df$Score_Neutralizing[length(Entered_Scores_Df$Score_Neutralizing)] - Entered_Scores_Df$Score_Neutralizing[1]
    OCIR.Obsessing.Comparisons<- length(Entered_Scores_Df$Change_Obsessing) - 1
    OCIR.Hoarding.Comparisons<- length(Entered_Scores_Df$Change_Hoarding) - 1
    OCIR.Neutralizing.Comparisons<- length(Entered_Scores_Df$Change_Neutralizing) - 1
    OCIR.Obsessing.First.Date<- Entered_Scores_Df$Date[1]
    OCIR.Hoarding.First.Date<- Entered_Scores_Df$Date[1]
    OCIR.Neutralizing.First.Date<- Entered_Scores_Df$Date[1]
    OCIR.Obsessing.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    OCIR.Hoarding.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    OCIR.Neutralizing.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    OCIR.Obsessing.First.Score<- Entered_Scores_Df$Score_Obsessing[1]
    OCIR.Hoarding.First.Score<- Entered_Scores_Df$Score_Hoarding[1]
    OCIR.Neutralizing.First.Score<- Entered_Scores_Df$Score_Neutralizing[1]
    OCIR.Obsessing.Last.Score<- Entered_Scores_Df$Score_Obsessing[length(Entered_Scores_Df$Score_Obsessing)]
    OCIR.Hoarding.Last.Score<- Entered_Scores_Df$Score_Hoarding[length(Entered_Scores_Df$Score_Hoarding)]
    OCIR.Neutralizing.Last.Score<- Entered_Scores_Df$Score_Neutralizing[length(Entered_Scores_Df$Score_Neutralizing)]
  
    
    
    Analytics_Df<<- data.frame(OCIR.Fullscale.First.Date, OCIR.Fullscale.First.Score, OCIR.Fullscale.Comparisons, OCIR.Fullscale.Change, OCIR.Fullscale.Last.Date, OCIR.Fullscale.Last.Score, OCIR.Fullscale.Improvement,OCIR.Fullscale.Sig.Improvement, OCIR.Fullscale.Deterioration, OCIR.Fullscale.Sig.Deterioration,
                               OCIR.Washing.First.Date, OCIR.Washing.First.Score, OCIR.Washing.Comparisons, OCIR.Washing.Change, OCIR.Washing.Last.Date, OCIR.Washing.Last.Score, OCIR.Washing.Improvement, OCIR.Washing.Sig.Improvement, OCIR.Washing.Deterioration, OCIR.Washing.Sig.Deterioration,
                               OCIR.Checking.First.Date, OCIR.Checking.First.Score, OCIR.Checking.Comparisons, OCIR.Checking.Change, OCIR.Checking.Last.Date, OCIR.Checking.Last.Score, OCIR.Checking.Improvement, OCIR.Checking.Sig.Improvement, OCIR.Checking.Deterioration, OCIR.Checking.Sig.Deterioration, 
                               OCIR.Ordering.First.Date, OCIR.Ordering.First.Score, OCIR.Ordering.Comparisons, OCIR.Ordering.Change, OCIR.Ordering.Last.Date, OCIR.Ordering.Last.Score, OCIR.Ordering.Improvement, OCIR.Ordering.Sig.Improvement, OCIR.Ordering.Deterioration, OCIR.Ordering.Sig.Deterioration, 
                               OCIR.Obsessing.First.Date, OCIR.Obsessing.First.Score, OCIR.Obsessing.Comparisons, OCIR.Obsessing.Change, OCIR.Obsessing.Last.Date, OCIR.Obsessing.Last.Score, OCIR.Obsessing.Improvement,OCIR.Obsessing.Sig.Improvement, OCIR.Obsessing.Deterioration, OCIR.Obsessing.Sig.Deterioration,
                               OCIR.Hoarding.First.Date, OCIR.Hoarding.First.Score, OCIR.Hoarding.Comparisons, OCIR.Hoarding.Change, OCIR.Hoarding.Last.Date, OCIR.Hoarding.Last.Score, OCIR.Hoarding.Improvement, OCIR.Hoarding.Sig.Improvement, OCIR.Hoarding.Deterioration, OCIR.Hoarding.Sig.Deterioration,
                               OCIR.Neutralizing.First.Date, OCIR.Neutralizing.First.Score, OCIR.Neutralizing.Comparisons, OCIR.Neutralizing.Change, OCIR.Neutralizing.Last.Date, OCIR.Neutralizing.Last.Score, OCIR.Neutralizing.Improvement, OCIR.Neutralizing.Sig.Improvement, OCIR.Neutralizing.Deterioration, OCIR.Neutralizing.Sig.Deterioration 
                               )
    
    
    
  })
  
  
  
  #Create an expression to activate the entered data & create notifications to indicate inaccurate data entry
  
  Entered_Scores_Notifications<- observeEvent(input$Action_Submit_Data, {
    
    Entered_Scores_Reac()
    
    if(length(Score_1a) < 18) {
      showNotification("The OCI-R is an 18-item scale. You have entered less than 18 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(length(Score_1a) > 18) {
      showNotification("The OCI-R is an 18-item scale. You have entered more than 18 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(input$Timepoint != "1") {
      if(length(Score_2a) < 18) {
        showNotification("The OCI-R is an 18-item scale. You have entered less than 18 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_2a) > 18) {
        showNotification("The OCI-R is an 18-item scale. You have entered more than 18 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
    if(input$Timepoint == "3") {
      if(length(Score_3a) < 18) {
        showNotification("The OCI-R is an 18-item scale. You have entered less than 18 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_3a) > 18) {
        showNotification("The OCI-R is an 18-item scale. You have entered more than 18 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
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
    
    Gap_Washing<- Entered_Scores_Df[1,11] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),11]
    Entered_Scores_Df[1,12]<- Gap_Washing
    
    Gap_Checking<- Entered_Scores_Df[1,20] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),20]
    Entered_Scores_Df[1,21]<- Gap_Checking
    
    Gap_Ordering<- Entered_Scores_Df[1,29] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),29]
    Entered_Scores_Df[1,30]<- Gap_Ordering
    
    Gap_Obsessing<- Entered_Scores_Df[1,38] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),38]
    Entered_Scores_Df[1,39]<- Gap_Obsessing
    
    Gap_Hoarding<- Entered_Scores_Df[1,47] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),47]
    Entered_Scores_Df[1,48]<- Gap_Hoarding
    
    Gap_Neutralizing<- Entered_Scores_Df[1,56] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),56]
    Entered_Scores_Df[1,57]<- Gap_Neutralizing
    
    
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
    
    filename = paste0(" OCI-R Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
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
        Stats_Table_Washing = Stats_Table_Washing,
        Stats_Table_Checking = Stats_Table_Checking,
        Stats_Table_Ordering = Stats_Table_Ordering,
        Stats_Table_Obsessing = Stats_Table_Obsessing,
        Stats_Table_Hoarding = Stats_Table_Hoarding,
        Stats_Table_Neutralizing = Stats_Table_Neutralizing,
        Cutoff_Names = Cutoff_Names,
        Item_Df = Item_Df
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
      paste(paste0(" OCI-R Data ", format(Sys.time(), '%d/%m/%y')), input$Output_Filetype1, sep = ".")
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













