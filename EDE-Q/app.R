
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
library(zoo)

source("Diagnoses.R")

Research_Table<- read_excel("ResearchTable.xlsx")

ui<- function(request) {
  sidebar <- dashboardSidebar(
    sidebarMenu(
      br(),
      menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "EDE-Q"),
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
      tags$footer(tags$a(href = "http://psychlytx.com.au", target = "_blank", HTML("<br><center>"), "PsychlytX", tags$sup(icon("registered")), br(),
                         "© Timothy Deitz 2018"))
    )
  )
  
  dashboardPage(
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| Eating Disorder Examination Questionnaire (EDE-Q)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 820),
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
                
                h3(tags$strong("References")), br(),br(),

                "Carter, J. C., Stewart, D. A., & Fairburn, C. G. (2001). Eating disorder examination questionnaire: Norms for young adolescent girls. Behaviour Research and Therapy, 39(5), 625-632.", br(), br(), 
                "Fairburn, C. G., & Beglin, S. J. (1994). Assessment of eating disorders: Interview or self‐report questionnaire? International Journal of Eating Disorders, 16(4), 363-370.", br(), br(),  
                "Jennings, K. M., & Phillips, K. E. (2017). Eating disorder Examination–Questionnaire (EDE–Q): Norms for a clinical sample of males. Archives of Psychiatric Nursing, 31(1), 73-76.", br(), br(),  
                "Luce, K. H., Crowther, J. H., & Pole, M. (2008). Eating disorder examination questionnaire (EDE‐Q): Norms for undergraduate women. International Journal of Eating Disorders, 41(3), 273-276.", br(), br(),  
                "Mond, J. M., Hay, P. J., Rodgers, B., & Owen, C. (2006). Eating disorder examination questionnaire (EDE-Q): Norms for young adult women. Behaviour Research and Therapy, 44(1), 53-62.", br(), br(),  
                "Mond, J., Hall, A., Bentley, C., Harrison, C., Gratwick‐Sarll, K., & Lewis, V. (2014). Eating‐disordered behavior in adolescent boys: Eating disorder examination questionnaire norms. International Journal of Eating Disorders, 47(4), 335-341.", br(), br(),  
                "Rand-Giovannetti, D., Cicero, D. C., Mond, J. M., & Latner, J. D. (2017). Psychometric properties of the eating disorder Examination–Questionnaire (EDE-Q): A confirmatory factor analysis and assessment of measurement invariance by sex. Assessment.", br(), br(), 
                "Rose, J. S., Vaewsorn, A., Rosselli-Navarra, F., Wilson, G. T., & Weissman, R. S. (2013). Test-retest reliability of the eating disorder examination-questionnaire (EDE-Q) in a college sample. Journal of Eating Disorders, 1(1), 42.", br(), br(),  
                "Smith, K. E., Mason, T. B., Murray, S. B., Griffiths, S., Leonard, R. C., Wetterneck, C. T., . . . Lavender, J. M. (2017). Male clinical norms and sex differences on the eating disorder inventory (EDI) and eating disorder examination questionnaire (EDE‐Q). International Journal of Eating Disorders, 50(7), 769-775."
 
        ),
        
        
        
        tabItem(tabName = "EDE-Q",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel("Scale",
                             wellPanel(style = "background-color: #ffffff; color: black",
                                       fluidRow(
                                         column(width = 12, offset = 4, h3(tags$strong("Eating Questionnaire")))
                                       ),
                                       fluidRow(
                                         column(width = 12,
                                                h4(tags$strong("Instructions: The following questions are concerned with the past four weeks (28 days) only.
                                                               Please read each question carefully. Please answer all of the questions.  Please only choose one answer for 
                                                               each question. Thank you.")), br(), h4(tags$strong("Questions 1 to 12: Please circle the appropriate number on the right. 
                                                                                                                  Remember that the questions only refer to the past four weeks (28 days) only.")) 
                                                )
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, h4(tags$strong("On how many of the past 28 days......"))),
                                         column(width = 1, h5(tags$strong("No", HTML('&emsp;'), "days"))),
                                         column(width = 1, h5(tags$strong("1-5", HTML('&emsp;'), "days"))),
                                         column(width = 1, h5(tags$strong("6-12 days"))),
                                         column(width = 1, h5(tags$strong("13-15 days"))),
                                         column(width = 1, h5(tags$strong("16-22 days"))), 
                                         column(width = 1, h5(tags$strong("23-27 days"))),
                                         column(width = 1, h5(tags$strong("Every day")))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, tags$div(h4("1 Have you been deliberately", tags$strong("trying"), "to limit the amount of food you eat to influence your shape or 
                                                              weight (whether or not you have succeeded)?"))),
                                         column(width = 8, radioButtons("Item_1", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, h4("2 Have you gone for long periods of time (8 waking hours or more) without eating anything at all in order to influence your shape or weight? ")),
                                         column(width = 8, radioButtons("Item_2", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, tags$div(h4("3 Have you", tags$strong("tried"), "to exclude from your diet any foods that you like in order to influence your shape or weight (whether or not you have succeeded)?"))),
                                         column(width = 8, radioButtons("Item_3", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, tags$div(h4("4 Have you", tags$strong("tried"), "to follow definite rules regarding your eating (for example, a calorie limit) in order to influence your shape or weight (whether or not you have succeeded)?"))),
                                         column(width = 8, radioButtons("Item_4", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, tags$div(h4("5 Have you had a definite desire to have an", tags$strong("empty"), "stomach with the aim of influencing your shape or weight?"))),
                                         column(width = 8, radioButtons("Item_5", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, tags$div(h4("6 Have you had a definite desire to have a", tags$strong("totally"), "flat stomach?"))),
                                         column(width = 8, radioButtons("Item_6", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, tags$div(h4("7 Has thinking about", tags$strong("food, eating or calories"), "made it very difficult to concentrate on things you are 
                                                              interested in (for example, working, following a conversation, or reading)?"))),
                                         column(width = 8, radioButtons("Item_7", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, tags$div(h4("8 Has thinking about", tags$strong("shape or weight"), "made it difficult to concentrate on things you are interested in
                                                              (for example, working, following a conversation, or reading)?"))),
                                         column(width = 8, radioButtons("Item_8", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, h4("9 Have you had a definite fear of losing control over eating?")),
                                         column(width = 8, radioButtons("Item_9", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, h4("10 Have you had a definite fear that you might gain weight?")),
                                         column(width = 8, radioButtons("Item_10", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, h4("11 Have you felt fat?")),
                                         column(width = 8, radioButtons("Item_11", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, h4("12 Have you had a strong desire to lose weight?")),
                                         column(width = 8, radioButtons("Item_12", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 12, 
                                                h4(tags$strong("Questions 13-18: Please fill in the appropriate number in the boxes on the right. Remember that the questions
                                                   only refer to the past four weeks (28 days).")),
                                                h4(tags$strong("Over the past four weeks (28 days)......."))
                                                )
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 10, tags$div(h4("13 Over the past 28 days, how many times have you eaten what other people would regard as 
                                                               an unusually", tags$strong("large amount of food"), "(given the circumstances)?"))),
                                         column(width = 2, textInput("Item_13", ""))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 10, h4("14 ....On how many of these times did you have a sense of having lost control over your eating 
                                                               (at the time that you were eating)?")),
                                         column(width = 2, textInput("Item_14", ""))
                                         ),
                                       hr(),
                                       fluidRow(
                                         column(width = 10, tags$div(h4("15 Over the past 28 days, on how many", tags$strong("DAYS"), "have such episodes of overeating occurred (i.e. you have eaten an unusually 
                                                               large amount of food and have had a sense of loss of control at the time)?"))),
                                         column(width = 2, textInput("Item_15", ""))
                                         ),
                                       hr(),
                                       fluidRow(
                                         column(width = 10, tags$div(h4("16 Over the past 28 days, how many", tags$strong("times"), "have you made yourself sick (vomit) as a means of controlling your shape or weight? "))),
                                         column(width = 2, textInput("Item_16", ""))
                                         ),
                                       hr(),
                                       fluidRow(
                                         column(width = 10, tags$div(h4("17 Over the past 28 days, how many", tags$strong("times"), "have you taken laxatives as a means of controlling your 
                                                               shape or weight?"))),
                                         column(width = 2, textInput("Item_17", ""))
                                         ),
                                       hr(),
                                       fluidRow(
                                         column(width = 10, tags$div(h4("18 Over the past 28 days, how many", tags$strong("times"), "have you exercised in a “driven” or “compulsive” 
                                                               way as a means of controlling your weight, shape or amount of fat or to burn off calories?"))),
                                         column(width = 2, textInput("Item_18", ""))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 12,
                                                h4(tags$strong("Questions 19-21: Please circle the appropriate number. Please note that for these questions the term “binge eating” means eating what others would regard as an unusually large amount 
                                                of food for the circumstances, accompanied by a sense of having lost control over eating."))
                                                )
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 1, offset = 4, h5(tags$strong("No", HTML('&emsp;'), "days"))),
                                         column(width = 1, h5(tags$strong("1-5", HTML('&emsp;'), "days"))),
                                         column(width = 1, h5(tags$strong("6-12 days"))),
                                         column(width = 1, h5(tags$strong("13-15 days"))),
                                         column(width = 1, h5(tags$strong("16-22 days"))), 
                                         column(width = 1, h5(tags$strong("23-27 days"))),
                                         column(width = 1, h5(tags$strong("Every day")))
                                       ),
                                       fluidRow(
                                         column(width = 4, h4("19 Over the past 28 days, on how many days have you eaten in secret (ie, furtively)"), h4("......Do not
                                                              count episodes of binge eating")),
                                         column(width = 8, radioButtons("Item_19", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 1, offset = 4, h5(tags$strong("None", HTML('&emsp;'), "of the times"))),
                                         column(width = 1, h5(tags$strong("A few", HTML('&emsp;'), "of the times"))),
                                         column(width = 1, h5(tags$strong("Less than half"))),
                                         column(width = 1, h5(tags$strong("Half of the times"))),
                                         column(width = 1, h5(tags$strong("More than half"))), 
                                         column(width = 1, h5(tags$strong("Most of the time"))),
                                         column(width = 1, h5(tags$strong("Every time")))
                                       ),
                                       fluidRow(
                                         column(width = 4, h4("20 On what proportion of the times that you hav eaten have you felt guilty (felt that you've done wrong) 
                                                              because of its effect on your shape or weight?"), h4("......Do not count episodes of binge eating")),
                                         column(width = 8, radioButtons("Item_20", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                         ),
                                      hr(),
                                      fluidRow(
                                        column(width = 1, offset = 4, h5(tags$strong("Not at all"))),
                                        column(offset = 1, width = 1, h5(tags$strong("Slightly"))),
                                        column(width = 2, h5(tags$strong("Moderately"))),
                                        column(offset = 1, width = 1, h5(tags$strong("Markedly")))
                                      ),
                                      fluidRow(
                                        column(width = 4, h4("21 Over the past 28 days, how concerned have you been about other people seeing you eat?"), h4("....Do not count episodes of binge eating")),
                                        column(width = 8, radioButtons("Item_21", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                      ),
                                      hr(),
                                      fluidRow(
                                       column(width = 12,
                                              h4(tags$strong("Questions 22-28: Please circle the appropriate number on the right. Remember that the questions only refer to the past four weeks (28 days) "))) 
                                      ), 
                                       hr(),
                                      fluidRow(
                                        column (width = 4, h4(tags$strong("On how many of the past 28 days......"))),
                                        column(width = 1, h5(tags$strong("Not at all"))),
                                        column(offset = 1, width = 1, h5(tags$strong("Slightly"))),
                                        column(width = 2, h5(tags$strong("Moderately"))),
                                        column(offset = 1, width = 1, h5(tags$strong("Markedly")))
                                      ),
                                      hr(),
                                      
                                      fluidRow(
                                        column(width = 4, tags$div(h4("22 Has your", tags$strong("weight"), "influenced how you think about (judge) yourself as a person"))),
                                        column(width = 8, radioButtons("Item_22", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                      ),
                                      hr(),
                                      fluidRow(
                                        column(width = 4, tags$div(h4("23 Has your", tags$strong("shape"), "influenced how you think about (judge) yourself as a person"))),
                                               column(width = 8, radioButtons("Item_23", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                        ),
                                        hr(),
                                        fluidRow(
                                          column(width = 4, h4("24 How much would it have upset you if you had been asked to weigh yourself once a week
                                                               (no more, or less, often) for the next four weeks?")),
                                                 column(width = 8, radioButtons("Item_24", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                          ),
                                          hr(),
                                          fluidRow(
                                            column(width = 4, tags$div(h4("25  How dissatisfied have you been with your", tags$strong("weight?")))),
                                                   column(width = 8, radioButtons("Item_25", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                            ),
                                            hr(),
                                            fluidRow(
                                              column(width = 4, tags$div(h4("26  How dissatisfied have you been with your", tags$strong("shape?")))),
                                              column(width = 8, radioButtons("Item_26", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                              ),
                                              hr(),
                                              fluidRow(
                                                column(width = 4, h4("27 How unfomfortable have you felt seeing your body (for example, seeing your shape in the mirror,
                                                                     in a shop window reflection, while undressing or taking a bath or shower?")),
                                                       column(width = 8, radioButtons("Item_27", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                                ),
                                                hr(),
                                                fluidRow(
                                                  column(width = 4, h4("28 How unfomfortable have you felt about others seeing your shape or figure (for example, in communal changing rooms, when swimming,
                                                                       or wearing tight clothes)?")),
                                                         column(width = 8, radioButtons("Item_28", label = NULL, choices = c("0", "1", "2", "3", "4", "5", "6"), inline = TRUE, selected = character(0)))
                                                  ),
                                      fluidRow(
                                        column(width = 9, h4("What is your weight at present? (Please give your best estimate)")),
                                        column(width = 3, textInput("Item_29", ""))
                                        ),
                                      fluidRow(
                                        column(width = 9, h4("What is your height? (Please give your best estimate)")),
                                        column(width = 3, textInput("Item_30", ""))
                                      ),
                                      fluidRow(
                                        column(width = 9, h4("If female: Over the past three-to-four months have you missed any menstrual periods?")),
                                        column(width = 3, textInput("Item_31", ""))
                                      ),
                                      fluidRow(
                                        column(width = 7, offset = 2, h4("If so, how many?")),
                                        column(width = 3, textInput("Item_32", ""))
                                      ),
                                      fluidRow(
                                        column(width = 7, offset = 2, h4("Have you been taking the pill?")),
                                        column(width = 3, textInput("Item_33", ""))
                                      ),
                                      br(),
                                      h3(tags$strong("THANK YOU")),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, dateInput("Q_Date", "Date",  format = "dd/mm/yyyy")),
                                         column(width = 4, textInput("Q_Name", "Name")),
                                         column(width = 4, textInput("Q_Clin_Name", "Clinician's Name"))
                                       ),
                                       fluidRow(
                                         column(width = 12, h5("Scale Source: Fairburn, C. G., & Beglin, S. J. (1994). Assessment of eating disorders: Interview or self-report questionnaire? 
                                                               International Journal of Eating Disorders, 16, 363–370."))
                                       )
                             )
                             
                    ),
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
                                                      selectInput("Pop", "", choices = c("General Population (Adult Female)", "University Student (Adult Male)", "Adolescent Female (12-14 years)", "Adolescent Female (12-18 years)", 
                                                                                         "Adolescent Male (12-18 years)", "Eating Disorder (Female)", "Eating Disorder (Male)"))
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
                                                               selectInput("Select_CI", label = "EDE-Q total scale",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI == '2'",
                                                                                numericInput("Man_CI", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Restraint", label = "Restraint",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Restraint == '2'",
                                                                                numericInput("Man_CI_Restraint", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Eating_Concern", label = "Eating Concern",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Eating_Concern == '2'",
                                                                                numericInput("Man_CI_Eating_Concern", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Shape_Concern", label = "Shape_Concern",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Shape_Concern == '2'",
                                                                                numericInput("Man_CI_Shape_Concern", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Weight_Concern", label = "Weight_Concern",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Weight_Concern == '2'",
                                                                                numericInput("Man_CI_Weight_Concern", "Specify the width of the confidence interval", value = 0))
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
                                                               uiOutput("Mean_Widg_Restraint")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Eating_Concern")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Shape_Concern")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Weight_Concern")
                                                        )
                                                      ),
                                                      
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Chelune et al. (1993)' || input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)' || input.RelChangeMethod == 'Speer (1992)'",
                                                                       
                                                                       h4(tags$strong("Enter a mean value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean", "EDE-Q total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Restraint", "Restraint", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Eating_Concern", "Eating Concern", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Shape_Concern", "Shape Concern", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Weight_Concern", "Weight Concern", value = 0)
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
                                                               uiOutput("Sd_Widg_Restraint")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Eating_Concern")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Shape_Concern")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Weight_Concern")
                                                        )
                                                      ),
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)'",
                                                                       
                                                                       h4(tags$strong("Enter a standard deviation value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd", "EDE-Q total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Restraint", "Restraint", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Eating_Concern", "Eating Concern", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Shape_Concern", "Shape Concern", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Weight_Concern", "Weight Concern", value = 0)
                                                                         )
                                                                       )
                                                                       
                                                      )
                                             ),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      h4(tags$strong("Enter test-retest reliability values")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability", "EDE-Q total scale", value = .92),
                                                               h6("Rose, Vaewsorn, Rosselli-Navarra, Wilson & Weissman (2013)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Restraint", "Restraint", value = .81),
                                                               h6("Rose, Vaewsorn, Rosselli-Navarra, Wilson & Weissman (2013)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Eating_Concern", "Eating Concern", value = .84),
                                                               h6("Rose, Vaewsorn, Rosselli-Navarra, Wilson & Weissman (2013)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Shape_Concern", "Shape Concern", value = .91),
                                                               h6("Rose, Vaewsorn, Rosselli-Navarra, Wilson & Weissman (2013)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Weight_Concern", "Weight Concern", value = .90),
                                                               h6("Rose, Vaewsorn, Rosselli-Navarra, Wilson & Weissman (2013)")
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
                                                               uiOutput("Cutoff_Widg_Restraint_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Eating_Concern_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Shape_Concern_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Weight_Concern_1") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Second cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Restraint_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Eating_Concern_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Shape_Concern_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Weight_Concern_2") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Third cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Restraint_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Eating_Concern_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Shape_Concern_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Weight_Concern_3") 
                                                        )
                                                        
                                                      )
                                                      , hr(), 
                                                    
                                                      h4(tags$strong("Fourth cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_4") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Restraint_4") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Eating_Concern_4") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Shape_Concern_4") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Weight_Concern_4") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Fifth cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_5") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Restraint_5") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Eating_Concern_5") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Shape_Concern_5") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Weight_Concern_5") 
                                                        )
                                                        
                                                      ), hr()
                                                      
                                             ),
                                             
                                             
                                             h4(tags$strong("Psychometric Properties of the EDE-Q Relevant to Assessing Reliable & Clinically Significant Change")),
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
                                              downloadButton("report", div(tags$strong("Step 3."), "Generate Patient Report"), class = "reportbutton", width = '270px'),
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
                                                       h4(tags$strong("Step 1.")), h4("Create a New nalytics Dataset or Add to an Existing One"),
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
                      input$Item_17, input$Item_18, input$Item_19, input$Item_20, input$Item_21, input$Item_22, input$Item_23, input$Item_24, input$Item_25,
                      input$Item_26, input$Item_27, input$Item_28, input$Item_29, input$Item_30, input$Item_31, input$Item_32, input$Item_33, sep = ",")
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
                    , pageLength = 150, dom = "t", scrollX = TRUE, fixedColumns = list(leftColumns = 3), autoWidth = TRUE, columnDefs = list(list(width = '180px', targets = c(1,9,10)), list(width= '140px', targets = c(0,2))))) %>% formatStyle('Authors', color = '#ffffff', backgroundColor = '#d35400')
    
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
    
    if(input$Pop == "General Population (Adult Female)") {
      Mean_Val<<- 1.52
      Sd_Val<<- 1.25
      Source_Mean<<- "Mond et al. (2006)"
      Cut_Val_1<<- 1.24
      Cut_Val_2<<- 1.83
      Cut_Val_3<<- 2.29
      Cut_Val_4<<- 3.36
      Cut_Val_5<<- 4
      Cut_Lab_1<<- "50th Percentile"
      Cut_Lab_2<<- "65th Percentile"
      Cut_Lab_3<<- "75th Percentile"
      Cut_Lab_4<<- "90th Percentile"
      Cut_Lab_5<<- "95th Percentile"
      Source_Cutoff<<- "Mond et al. (2006)"
      Mean_Val_Restraint<<- 1.3
      Sd_Val_Restraint<<- 1.4
      Source_Mean_Restraint<<- "Mond et al. (2006)"
      Cut_Val_Restraint_1<<-0.8
      Cut_Val_Restraint_2<<- 1.6
      Cut_Val_Restraint_3<<- 2.2
      Cut_Val_Restraint_4<<- 3.6
      Cut_Val_Restraint_5<<- 4
      Cut_Lab_1_Restraint<<- "50th Percentile"
      Cut_Lab_2_Restraint<<- "65th Percentile"
      Cut_Lab_3_Restraint<<- "75th Percentile"
      Cut_Lab_4_Restraint<<- "90th Percentile"
      Cut_Lab_5_Restraint<<- "95th Percentile"
      Source_Cutoff_Restraint<<- "Mond et al. (2006)"
      Mean_Val_Eating_Concern<<- 0.76
      Sd_Val_Eating_Concern<<- 1.06
      Source_Mean_Eating_Concern<<- "Mond et al. (2006)"
      Cut_Val_Eating_Concern_1<<- 0.2  
      Cut_Val_Eating_Concern_2<<- 0.6
      Cut_Val_Eating_Concern_3<<- 1
      Cut_Val_Eating_Concern_4<<- 2.4
      Cut_Val_Eating_Concern_5<<- 3.2
      Cut_Lab_1_Eating_Concern<<- "50th Percentile"
      Cut_Lab_2_Eating_Concern<<- "65th Percentile"
      Cut_Lab_3_Eating_Concern<<- "75th Percentile"
      Cut_Lab_4_Eating_Concern<<- "90th Percentile"
      Cut_Lab_5_Eating_Concern<<- "95th Percentile"
      Source_Cutoff_Eating_Concern<<- "Mond et al. (2006)"
      Mean_Val_Shape_Concern<<- 2.23
      Sd_Val_Shape_Concern<<-1.65
      Source_Mean_Shape_Concern<<- "Mond et al. (2006)"
      Cut_Val_Shape_Concern_1<<- 1.88
      Cut_Val_Shape_Concern_2<<- 2.75
      Cut_Val_Shape_Concern_3<<- 3.5
      Cut_Val_Shape_Concern_4<<- 4.75
      Cut_Val_Shape_Concern_5<<- 5.25
      Cut_Lab_1_Shape_Concern<<- "50th Percentile"
      Cut_Lab_2_Shape_Concern<<- "65th Percentile"
      Cut_Lab_3_Shape_Concern<<- "75th Percentile"
      Cut_Lab_4_Shape_Concern<<- "90th Percentile"
      Cut_Lab_5_Shape_Concern<<- "95th Percentile"
      Source_Cutoff_Shape_Concern<<-  "Mond et al. (2006)"
      Mean_Val_Weight_Concern<<- 1.79
      Sd_Val_Weight_Concern<<-1.51
      Source_Mean_Weight_Concern<<- "Mond et al. (2006)"
      Cut_Val_Weight_Concern_1<<- 1.4
      Cut_Val_Weight_Concern_2<<- 2.2
      Cut_Val_Weight_Concern_3<<- 2.8
      Cut_Val_Weight_Concern_4<<- 4
      Cut_Val_Weight_Concern_5<<- 4.6
      Cut_Lab_1_Weight_Concern<<- "50th Percentile"
      Cut_Lab_2_Weight_Concern<<- "65th Percentile"
      Cut_Lab_3_Weight_Concern<<- "75th Percentile"
      Cut_Lab_4_Weight_Concern<<- "90th Percentile"
      Cut_Lab_5_Weight_Concern<<- "95th Percentile"
      Source_Cutoff_Weight_Concern<<- "Mond et al. (2006)"
    } else if (input$Pop == "University Student (Adult Male)") {
        Mean_Val<<- 1.09
        Sd_Val<<- 1
        Source_Mean<<- "Lavendar, De Young & Anderson (2010)"
        Cut_Val_1<<- .84
        Cut_Val_2<<-1.22
        Cut_Val_3<<- 1.59
        Cut_Val_4<<- 2.55
        Cut_Val_5<<-3.16
        Cut_Lab_1<<- "50th Percentile"
        Cut_Lab_2<<- "65th Percentile"
        Cut_Lab_3<<- "75th Percentile"
        Cut_Lab_4<<- "90th Percentile"
        Cut_Lab_5<<- "95th Percentile"
        Source_Cutoff<<-"Lavendar, De Young & Anderson (2010)"
        Mean_Val_Restraint<<- 1.04
        Sd_Val_Restraint<<- 1.19
        Source_Mean_Restraint<<- "Lavendar, De Young & Anderson (2010)"
        Cut_Val_Restraint_1<<-0.6
          Cut_Val_Restraint_2<<- 1.2
          Cut_Val_Restraint_3<<- 1.6
          Cut_Val_Restraint_4<<- 2.8
          Cut_Val_Restraint_5<<- 3.6
          Cut_Lab_1_Restraint<<- "50th Percentile"
          Cut_Lab_2_Restraint<<- "65th Percentile"
          Cut_Lab_3_Restraint<<- "75th Percentile"
          Cut_Lab_4_Restraint<<- "90th Percentile"
          Cut_Lab_5_Restraint<<- "95th Percentile"
          Source_Cutoff_Restraint<<- "Lavendar, De Young & Anderson (2010)"
          Mean_Val_Eating_Concern<<- .43
        Sd_Val_Eating_Concern<<- .77
        Source_Mean_Eating_Concern<<- "Lavendar, De Young & Anderson (2010)"
        Cut_Val_Eating_Concern_1<<-  .2
          Cut_Val_Eating_Concern_2<<- .4
          Cut_Val_Eating_Concern_3<<- .8
          Cut_Val_Eating_Concern_4<<- 1.2
          Cut_Val_Eating_Concern_5<<- 2.2
          Cut_Lab_1_Eating_Concern<<- "55th Percentile"
          Cut_Lab_2_Eating_Concern<<- "70th Percentile"
          Cut_Lab_3_Eating_Concern<<- "80th Percentile"
          Cut_Lab_4_Eating_Concern<<- "90th Percentile"
          Cut_Lab_5_Eating_Concern<<- "95th Percentile"
          Source_Cutoff_Eating_Concern<<- "Lavendar, De Young & Anderson (2010)"
          Mean_Val_Shape_Concern<<- 1.59
        Sd_Val_Shape_Concern<<-1.38
        Source_Mean_Shape_Concern<<-"Lavendar, De Young & Anderson (2010)"
        Cut_Val_Shape_Concern_1<<-1.25
          Cut_Val_Shape_Concern_2<<- 1.75
          Cut_Val_Shape_Concern_3<<- 2.38
          Cut_Val_Shape_Concern_4<<- 3.63
          Cut_Val_Shape_Concern_5<<- 4.38
          Cut_Lab_1_Shape_Concern<<- "50th Percentile"
          Cut_Lab_2_Shape_Concern<<- "65th Percentile"
          Cut_Lab_3_Shape_Concern<<- "75th Percentile"
          Cut_Lab_4_Shape_Concern<<- "90th Percentile"
          Cut_Lab_5_Shape_Concern<<- "95th Percentile"
          Source_Cutoff_Shape_Concern<<- "Lavendar, De Young & Anderson (2010)"
          Mean_Val_Weight_Concern<<- 1.29
        Sd_Val_Weight_Concern<<-1.27
        Source_Mean_Weight_Concern<<- "Lavendar, De Young & Anderson (2010)"
        Cut_Val_Weight_Concern_1<<- 1
          Cut_Val_Weight_Concern_2<<- 1.4
          Cut_Val_Weight_Concern_3<<- 2
          Cut_Val_Weight_Concern_4<<- 3.2
          Cut_Val_Weight_Concern_5<<- 3.8
          Cut_Lab_1_Weight_Concern<<- "50th Percentile"
          Cut_Lab_2_Weight_Concern<<- "65th Percentile"
          Cut_Lab_3_Weight_Concern<<- "75th Percentile"
          Cut_Lab_4_Weight_Concern<<- "90th Percentile"
          Cut_Lab_5_Weight_Concern<<- "95th Percentile"
          Source_Cutoff_Weight_Concern<<- "Lavendar, De Young & Anderson (2010)"
    } else if (input$Pop == "Adolescent Female (12-14 years)") {
      Mean_Val<<- 1.6
        Sd_Val<<- 1.4
        Source_Mean<<- "Carter et al. (2001)"
        Cut_Val_1<<- 1.16
        Cut_Val_2<<-1.92
        Cut_Val_3<<- 2.51
        Cut_Val_4<<- 3.67
        Cut_Val_5<<-4.33
        Cut_Lab_1<<- "50th Percentile"
        Cut_Lab_2<<- "65th Percentile"
        Cut_Lab_3<<-"75th Percentile"
        Cut_Lab_4<<- "90th Percentile"
        Cut_Lab_5<<- "95th Percentile"
        Source_Cutoff<<- "Carter et al. (2001)"
        Mean_Val_Restraint<<- 1.4
        Sd_Val_Restraint<<- 1.5
        Source_Mean_Restraint<<- "Carter et al. (2001)"
        Cut_Val_Restraint_1<<-0.8
        Cut_Val_Restraint_2<<- 1.4
        Cut_Val_Restraint_3<<- 2
        Cut_Val_Restraint_4<<- 3.8
        Cut_Val_Restraint_5<<- 4.8
        Cut_Lab_1_Restraint<<- "50th Percentile"
        Cut_Lab_2_Restraint<<- "65th Percentile"
        Cut_Lab_3_Restraint<<- "75th Percentile"
        Cut_Lab_4_Restraint<<- "90th Percentile"
        Cut_Lab_5_Restraint<<- "95th Percentile"
        Source_Cutoff_Restraint<<- "Carter et al. (2001)"
        Mean_Val_Eating_Concern<<- 1
        Sd_Val_Eating_Concern<<- 1
        Source_Mean_Eating_Concern<<- "Carter et al. (2001)"
        Cut_Val_Eating_Concern_1<<- .5
        Cut_Val_Eating_Concern_2<<- 1
        Cut_Val_Eating_Concern_3<<- 1.6
        Cut_Val_Eating_Concern_4<<- 2.8
        Cut_Val_Eating_Concern_5<<- 3.2
        Cut_Lab_1_Eating_Concern<<- "50th Percentile"
        Cut_Lab_2_Eating_Concern<<- "65th Percentile"
        Cut_Lab_3_Eating_Concern<<- "75th Percentile"
        Cut_Lab_4_Eating_Concern<<- "90th Percentile"
        Cut_Lab_5_Eating_Concern<<- "95th Percentile"
        Source_Cutoff_Eating_Concern<<- "Carter et al. (2001)"
        Mean_Val_Shape_Concern<<- 2.2
        Sd_Val_Shape_Concern<<-1.7
        Source_Mean_Shape_Concern<<-"Carter et al. (2001)"
        Cut_Val_Shape_Concern_1<<-1.88
        Cut_Val_Shape_Concern_2<<- 2.75
        Cut_Val_Shape_Concern_3<<- 3.63
        Cut_Val_Shape_Concern_4<<- 4.88
        Cut_Val_Shape_Concern_5<<- 5.38
        Cut_Lab_1_Shape_Concern<<- "50th Percentile"
        Cut_Lab_2_Shape_Concern<<- "65th Percentile"
        Cut_Lab_3_Shape_Concern<<- "75th Percentile"
        Cut_Lab_4_Shape_Concern<<- "90th Percentile"
        Cut_Lab_5_Shape_Concern<<- "95th Percentile"
        Source_Cutoff_Shape_Concern<<- "Carter et al. (2001)"
        Mean_Val_Weight_Concern<<- 1.8
        Sd_Val_Weight_Concern<<- 1.7
        Source_Mean_Weight_Concern<<- "Carter et al. (2001)"
        Cut_Val_Weight_Concern_1<<- 1.2
        Cut_Val_Weight_Concern_2<<- 2.4
        Cut_Val_Weight_Concern_3<<- 3
        Cut_Val_Weight_Concern_4<<- 4.4
        Cut_Val_Weight_Concern_5<<- 5
        Cut_Lab_1_Weight_Concern<<- "50th Percentile"
        Cut_Lab_2_Weight_Concern<<- "65th Percentile"
        Cut_Lab_3_Weight_Concern<<- "75th Percentile"
        Cut_Lab_4_Weight_Concern<<- "90th Percentile"
        Cut_Lab_5_Weight_Concern<<- "95th Percentile"
        Source_Cutoff_Weight_Concern<<- "Carter et al. (2001)"
    } else if(input$Pop == "Adolescent Male (12-18 years)") {
      Mean_Val<<- .61
        Sd_Val<<- .86
        Source_Mean<<- "Mond et al. (2013)"
        Cut_Val_1<<- .28
        Cut_Val_2<<-.41
        Cut_Val_3<<- .61
        Cut_Val_4<<- .86
        Cut_Val_5<<-1.73
        Cut_Lab_1<<- "50th Percentile"
        Cut_Lab_2<<- "60th Percentile"
        Cut_Lab_3<<-"70th Percentile"
        Cut_Lab_4<<- "80th Percentile"
        Cut_Lab_5<<- "90th Percentile"
        Source_Cutoff<<- "Mond et al. (2013)"
        Mean_Val_Restraint<<- .54
        Sd_Val_Restraint<<- .97
        Source_Mean_Restraint<<- "Mond et al. (2013)"
        Cut_Val_Restraint_1<<- 0
        Cut_Val_Restraint_2<<- 0.2
        Cut_Val_Restraint_3<<- 0.4
        Cut_Val_Restraint_4<<- 1
        Cut_Val_Restraint_5<<- 1.8
        Cut_Lab_1_Restraint<<- "50th Percentile"
        Cut_Lab_2_Restraint<<- "60th Percentile"
        Cut_Lab_3_Restraint<<- "70th Percentile"
        Cut_Lab_4_Restraint<<- "80th Percentile"
        Cut_Lab_5_Restraint<<- "90th Percentile"
        Source_Cutoff_Restraint<<- "Mond et al. (2013)"
        Mean_Val_Eating_Concern<<- .4
        Sd_Val_Eating_Concern<<- .72
        Source_Mean_Eating_Concern<<- "Mond et al. (2013)"
        Cut_Val_Eating_Concern_1<<-  0
        Cut_Val_Eating_Concern_2<<- .2
        Cut_Val_Eating_Concern_3<<- .4
        Cut_Val_Eating_Concern_4<<- .6
        Cut_Val_Eating_Concern_5<<- 1.2
        Cut_Lab_1_Eating_Concern<<- "50th Percentile"
        Cut_Lab_2_Eating_Concern<<- "60th Percentile"
        Cut_Lab_3_Eating_Concern<<- "70th Percentile"
        Cut_Lab_4_Eating_Concern<<- "80th Percentile"
        Cut_Lab_5_Eating_Concern<<- "90th Percentile"
        Source_Cutoff_Eating_Concern<<- "Mond et al. (2013)"
        Mean_Val_Shape_Concern<<- .81
        Sd_Val_Shape_Concern<<-1.18
        Source_Mean_Shape_Concern<<-"Mond et al. (2013)"
        Cut_Val_Shape_Concern_1<<-.2
        Cut_Val_Shape_Concern_2<<- .4
        Cut_Val_Shape_Concern_3<<- .6
        Cut_Val_Shape_Concern_4<<- 1.2
        Cut_Val_Shape_Concern_5<<- 2.2
        Cut_Lab_1_Shape_Concern<<- "50th Percentile"
        Cut_Lab_2_Shape_Concern<<- "60th Percentile"
        Cut_Lab_3_Shape_Concern<<- "70th Percentile"
        Cut_Lab_4_Shape_Concern<<- "80th Percentile"
        Cut_Lab_5_Shape_Concern<<- "90th Percentile"
        Source_Cutoff_Shape_Concern<<- "Mond et al. (2013)"
        Mean_Val_Weight_Concern<<- .67
        Sd_Val_Weight_Concern<<-1.05
        Source_Mean_Weight_Concern<<- "Mond et al. (2013)"
        Cut_Val_Weight_Concern_1<<- .29
        Cut_Val_Weight_Concern_2<<- .5
        Cut_Val_Weight_Concern_3<<- .75
        Cut_Val_Weight_Concern_4<<- 1.25
        Cut_Val_Weight_Concern_5<<- 2.38
        Cut_Lab_1_Weight_Concern<<- "50th Percentile"
        Cut_Lab_2_Weight_Concern<<- "60th Percentile"
        Cut_Lab_3_Weight_Concern<<- "70th Percentile"
        Cut_Lab_4_Weight_Concern<<- "80th Percentile"
        Cut_Lab_5_Weight_Concern<<- "90th Percentile"
        Source_Cutoff_Weight_Concern<<- "Mond et al. (2013)"
    } else if(input$Pop == "Eating Disorder (Female)") {
      Mean_Val<<- 4
        Sd_Val<<- 1.44
        Source_Mean<<- "Smith et al. (2017)"
        Cut_Val_1<<- 4 - (2*1.44)
        Cut_Val_2<<- 4 - 1.44
        Cut_Val_3<<- 4
        Cut_Val_4<<- 4 + 1.44
        Cut_Val_5<<- 4 + (2*1.44)
        Cut_Lab_1<<- "Mean - 2 Sd"
        Cut_Lab_2<<- "Mean - 1 Sd"
        Cut_Lab_3<<- "Mean"
        Cut_Lab_4<<- "Mean + 1 Sd"
        Cut_Lab_5<<- "Mean + 2 Sd"
        Source_Cutoff<<- "Smith et al. (2017)"
        Mean_Val_Restraint<<- 3.61
        Sd_Val_Restraint<<- 1.82
        Source_Mean_Restraint<<- "Smith et al. (2017)"
        Cut_Val_Restraint_1<<- Mean_Val_Restraint - (2*Sd_Val_Restraint)
        Cut_Val_Restraint_2<<- Mean_Val_Restraint - Sd_Val_Restraint
        Cut_Val_Restraint_3<<- Mean_Val_Restraint 
        Cut_Val_Restraint_4<<- Mean_Val_Restraint + Sd_Val_Restraint
        Cut_Val_Restraint_5<<- Mean_Val_Restraint + (2*Sd_Val_Restraint)
        Cut_Lab_1_Restraint<<- "Mean - 2 Sd"
        Cut_Lab_2_Restraint<<- "Mean - 1 Sd"
        Cut_Lab_3_Restraint<<- "Mean"
        Cut_Lab_4_Restraint<<- "Mean + 1 Sd"
        Cut_Lab_5_Restraint<<- "Mean + 2 Sd"
        Source_Cutoff_Restraint<<- "Smith et al. (2017)"
        Mean_Val_Eating_Concern<<- 3.48
        Sd_Val_Eating_Concern<<- 1.47
        Source_Mean_Eating_Concern<<- "Smith et al. (2017)"
        Cut_Val_Eating_Concern_1<<-  Mean_Val_Eating_Concern - (2*Sd_Val_Eating_Concern)
        Cut_Val_Eating_Concern_2<<- Mean_Val_Eating_Concern - Sd_Val_Eating_Concern
        Cut_Val_Eating_Concern_3<<- Mean_Val_Eating_Concern
        Cut_Val_Eating_Concern_4<<- Mean_Val_Eating_Concern + Sd_Val_Eating_Concern
        Cut_Val_Eating_Concern_5<<- Mean_Val_Eating_Concern + (2*Sd_Val_Eating_Concern)
        Cut_Lab_1_Eating_Concern<<- "Mean - 2 Sd"
        Cut_Lab_2_Eating_Concern<<- "Mean - 1 Sd"
        Cut_Lab_3_Eating_Concern<<- "Mean"
        Cut_Lab_4_Eating_Concern<<- "Mean + 1 Sd"
        Cut_Lab_5_Eating_Concern<<- "Mean + 2 Sd"
        Source_Cutoff_Eating_Concern<<- "Smith et al. (2017)"
        Mean_Val_Shape_Concern<<- 4.68
        Sd_Val_Shape_Concern<<-1.51
        Source_Mean_Shape_Concern<<-"Smith et al. (2017)"
        Cut_Val_Shape_Concern_1<<-Mean_Val_Shape_Concern - (2*Sd_Val_Shape_Concern)
        Cut_Val_Shape_Concern_2<<- Mean_Val_Shape_Concern - Sd_Val_Shape_Concern
        Cut_Val_Shape_Concern_3<<- Mean_Val_Shape_Concern
        Cut_Val_Shape_Concern_4<<- Mean_Val_Shape_Concern + Sd_Val_Shape_Concern
        Cut_Val_Shape_Concern_5<<- Mean_Val_Shape_Concern + (2*Sd_Val_Shape_Concern)
        Cut_Lab_1_Shape_Concern<<- "Mean - 2 Sd"
        Cut_Lab_2_Shape_Concern<<- "Mean - 1 Sd"
        Cut_Lab_3_Shape_Concern<<- "Mean"
        Cut_Lab_4_Shape_Concern<<- "Mean + 1 Sd"
        Cut_Lab_5_Shape_Concern<<- "Mean + 2 Sd"
        Source_Cutoff_Shape_Concern<<- "Smith et al. (2017)"
        Mean_Val_Weight_Concern<<- 4.2
        Sd_Val_Weight_Concern<<-1.62
        Source_Mean_Weight_Concern<<- "Smith et al. (2017)"
        Cut_Val_Weight_Concern_1<<- Mean_Val_Weight_Concern - (2*Sd_Val_Weight_Concern)
        Cut_Val_Weight_Concern_2<<- Mean_Val_Weight_Concern - Sd_Val_Weight_Concern
        Cut_Val_Weight_Concern_3<<- Mean_Val_Weight_Concern
        Cut_Val_Weight_Concern_4<<- Mean_Val_Weight_Concern + Sd_Val_Weight_Concern
        Cut_Val_Weight_Concern_5<<- Mean_Val_Weight_Concern + (2*Sd_Val_Weight_Concern)
        Cut_Lab_1_Weight_Concern<<- "Mean - 2 Sd"
        Cut_Lab_2_Weight_Concern<<- "Mean - 1 Sd"
        Cut_Lab_3_Weight_Concern<<- "Mean"
        Cut_Lab_4_Weight_Concern<<- "Mean + 1 Sd"
        Cut_Lab_5_Weight_Concern<<- "Mean + 2 Sd"
        Source_Cutoff_Weight_Concern<<- "Smith et al. (2017)"
    } else if (input$Pop == "Eating Disorder (Male)") {
      Mean_Val<<- 3.01
        Sd_Val<<- 1.63
        Source_Mean<<- "Smith et al. (2017)"
        Cut_Val_1<<- Mean_Val - (2*Sd_Val)
        Cut_Val_2<<- Mean_Val - Sd_Val
        Cut_Val_3<<- Mean_Val
        Cut_Val_4<<- Mean_Val + Sd_Val
        Cut_Val_5<<- Mean_Val + (2*Sd_Val)
        Cut_Lab_1<<- "Mean - 2 Sd"
        Cut_Lab_2<<- "Mean - 1 Sd"
        Cut_Lab_3<<- "Mean"
        Cut_Lab_4<<- "Mean + 1 Sd"
        Cut_Lab_5<<- "Mean + 2 Sd"
        Source_Cutoff<<- "Smith et al. (2017)"
        Mean_Val_Restraint<<- 2.72
        Sd_Val_Restraint<<- 1.9
        Source_Mean_Restraint<<- "Smith et al. (2017)"
        Cut_Val_Restraint_1<<- Mean_Val_Restraint - (2*Sd_Val_Restraint)
        Cut_Val_Restraint_2<<- Mean_Val_Restraint - Sd_Val_Restraint
        Cut_Val_Restraint_3<<- Mean_Val_Restraint 
        Cut_Val_Restraint_4<<- Mean_Val_Restraint + Sd_Val_Restraint
        Cut_Val_Restraint_5<<- Mean_Val_Restraint + (2*Sd_Val_Restraint)
        Cut_Lab_1_Restraint<<- "Mean - 2 Sd"
        Cut_Lab_2_Restraint<<- "Mean - 1 Sd"
        Cut_Lab_3_Restraint<<- "Mean"
        Cut_Lab_4_Restraint<<- "Mean + 1 Sd"
        Cut_Lab_5_Restraint<<- "Mean + 2 Sd"
        Source_Cutoff_Restraint<<- "Smith et al. (2017)"
        Mean_Val_Eating_Concern<<- 2.58
        Sd_Val_Eating_Concern<<- 1.64
        Source_Mean_Eating_Concern<<- "Smith et al. (2017)" 
        Cut_Val_Eating_Concern_1<<-  Mean_Val_Eating_Concern - (2*Sd_Val_Eating_Concern)
        Cut_Val_Eating_Concern_2<<- Mean_Val_Eating_Concern - Sd_Val_Eating_Concern
        Cut_Val_Eating_Concern_3<<- Mean_Val_Eating_Concern
        Cut_Val_Eating_Concern_4<<- Mean_Val_Eating_Concern + Sd_Val_Eating_Concern
        Cut_Val_Eating_Concern_5<<- Mean_Val_Eating_Concern + (2*Sd_Val_Eating_Concern)
        Cut_Lab_1_Eating_Concern<<- "Mean - 2 Sd"
        Cut_Lab_2_Eating_Concern<<- "Mean - 1 Sd"
        Cut_Lab_3_Eating_Concern<<- "Mean"
        Cut_Lab_4_Eating_Concern<<- "Mean + 1 Sd"
        Cut_Lab_5_Eating_Concern<<- "Mean + 2 Sd"
        Source_Cutoff_Eating_Concern<<- "Smith et al. (2017)"
        Mean_Val_Shape_Concern<<- 3.66
        Sd_Val_Shape_Concern<<-1.88
        Source_Mean_Shape_Concern<<-"Smith et al. (2017)"
        Cut_Val_Shape_Concern_1<<-Mean_Val_Shape_Concern - (2*Sd_Val_Shape_Concern)
        Cut_Val_Shape_Concern_2<<- Mean_Val_Shape_Concern - Sd_Val_Shape_Concern
        Cut_Val_Shape_Concern_3<<- Mean_Val_Shape_Concern
        Cut_Val_Shape_Concern_4<<- Mean_Val_Shape_Concern + Sd_Val_Shape_Concern
        Cut_Val_Shape_Concern_5<<- Mean_Val_Shape_Concern + (2*Sd_Val_Shape_Concern)
        Cut_Lab_1_Shape_Concern<<- "Mean - 2 Sd"
        Cut_Lab_2_Shape_Concern<<- "Mean - 1 Sd"
        Cut_Lab_3_Shape_Concern<<- "Mean"
        Cut_Lab_4_Shape_Concern<<- "Mean + 1 Sd"
        Cut_Lab_5_Shape_Concern<<- "Mean + 2 Sd"
        Source_Cutoff_Shape_Concern<<- "Smith et al. (2017)"
        Mean_Val_Weight_Concern<<- 3.14
        Sd_Val_Weight_Concern<<- 1.87
        Source_Mean_Weight_Concern<<- "Smith et al. (2017)"
        Cut_Val_Weight_Concern_1<<- Mean_Val_Weight_Concern - (2*Sd_Val_Weight_Concern)
        Cut_Val_Weight_Concern_2<<- Mean_Val_Weight_Concern - Sd_Val_Weight_Concern
        Cut_Val_Weight_Concern_3<<- Mean_Val_Weight_Concern
        Cut_Val_Weight_Concern_4<<- Mean_Val_Weight_Concern + Sd_Val_Weight_Concern
        Cut_Val_Weight_Concern_5<<- Mean_Val_Weight_Concern + (2*Sd_Val_Weight_Concern)
        Cut_Lab_1_Weight_Concern<<- "Mean - 2 Sd"
        Cut_Lab_2_Weight_Concern<<- "Mean - 1 Sd"
        Cut_Lab_3_Weight_Concern<<- "Mean"
        Cut_Lab_4_Weight_Concern<<- "Mean + 1 Sd"
        Cut_Lab_5_Weight_Concern<<- "Mean + 2 Sd"
    } else if (input$Pop == "Adolescent Female (12-18 years)") {
      Mean_Val<<- 1.84
      Sd_Val<<- 1.54
      Source_Mean<<- "Mond et al. (2013)"
      Cut_Val_1<<- Mean_Val - (2*Sd_Val)
      Cut_Val_2<<- Mean_Val - Sd_Val
      Cut_Val_3<<- Mean_Val
      Cut_Val_4<<- Mean_Val + Sd_Val
      Cut_Val_5<<- Mean_Val + (2*Sd_Val)
      Cut_Lab_1<<- "Mean - 2 Sd"
      Cut_Lab_2<<- "Mean - 1 Sd"
      Cut_Lab_3<<-"Mean"
      Cut_Lab_4<<- "Mean + 1 Sd"
      Cut_Lab_5<<- "Mean + 2 Sd"
      Source_Cutoff<<- "Mond et al. (2013)"
      Mean_Val_Restraint<<- 1.48
      Sd_Val_Restraint<<- 1.57
      Source_Mean_Restraint<<- "Mond et al. (2013)"
      Cut_Val_Restraint_1<<- Mean_Val_Restraint - (2*Sd_Val_Restraint)
      Cut_Val_Restraint_2<<- Mean_Val_Restraint - Sd_Val_Restraint
      Cut_Val_Restraint_3<<- Mean_Val_Restraint
      Cut_Val_Restraint_4<<- Mean_Val_Restraint + Sd_Val_Restraint
      Cut_Val_Restraint_5<<- Mean_Val_Restraint + (2*Sd_Val_Restraint)
      Cut_Lab_1_Restraint<<- "Mean - 2 Sd"
      Cut_Lab_2_Restraint<<- "Mean - 1 Sd"
      Cut_Lab_3_Restraint<<- "Mean"
      Cut_Lab_4_Restraint<<- "Mean + 1 Sd"
      Cut_Lab_5_Restraint<<- "Mean + 2 Sd"
      Source_Cutoff_Restraint<<- "Mond et al. (2013)"
      Mean_Val_Eating_Concern<<- 1.21
      Sd_Val_Eating_Concern<<- 1.38
      Source_Mean_Eating_Concern<<- "Mond et al. (2013)"
      Cut_Val_Eating_Concern_1<<-  Mean_Val_Eating_Concern - (2*Sd_Val_Eating_Concern)
      Cut_Val_Eating_Concern_2<<- Mean_Val_Eating_Concern - Sd_Val_Eating_Concern
      Cut_Val_Eating_Concern_3<<- Mean_Val_Eating_Concern
      Cut_Val_Eating_Concern_4<<- Mean_Val_Eating_Concern + Sd_Val_Eating_Concern
      Cut_Val_Eating_Concern_5<<- Mean_Val_Eating_Concern + (2*Sd_Val_Eating_Concern)
      Cut_Lab_1_Eating_Concern<<- "Mean - 2 Sd"
      Cut_Lab_2_Eating_Concern<<- "Mean - 1 Sd"
      Cut_Lab_3_Eating_Concern<<- "Mean"
      Cut_Lab_4_Eating_Concern<<- "Mean + 1 Sd"
      Cut_Lab_5_Eating_Concern<<- "Mean + 2 Sd"
      Source_Cutoff_Eating_Concern<<- "Mond et al. (2013)"
      Mean_Val_Shape_Concern<<- 2.45
      Sd_Val_Shape_Concern<<-1.88
      Source_Mean_Shape_Concern<<-"Mond et al. (2013)"
      Cut_Val_Shape_Concern_1<<- Mean_Val_Shape_Concern - (2*Sd_Val_Shape_Concern)
      Cut_Val_Shape_Concern_2<<- Mean_Val_Shape_Concern - Sd_Val_Shape_Concern
      Cut_Val_Shape_Concern_3<<- Mean_Val_Shape_Concern
      Cut_Val_Shape_Concern_4<<- Mean_Val_Shape_Concern + Sd_Val_Shape_Concern
      Cut_Val_Shape_Concern_5<<- Mean_Val_Shape_Concern + (2*Sd_Val_Shape_Concern)
      Cut_Lab_1_Shape_Concern<<- "Mean - 2 Sd"
      Cut_Lab_2_Shape_Concern<<- "Mean - 1 Sd"
      Cut_Lab_3_Shape_Concern<<- "Mean"
      Cut_Lab_4_Shape_Concern<<- "Mean + 1 Sd"
      Cut_Lab_5_Shape_Concern<<- "Mean + 2 Sd"
      Source_Cutoff_Shape_Concern<<- "Mond et al. (2013)"
      Mean_Val_Weight_Concern<<- 2.2
      Sd_Val_Weight_Concern<<-1.82
      Source_Mean_Weight_Concern<<- "Mond et al. (2013)"
      Cut_Val_Weight_Concern_1<<- Mean_Val_Weight_Concern - (2*Sd_Val_Weight_Concern)
      Cut_Val_Weight_Concern_2<<- Mean_Val_Weight_Concern - Sd_Val_Weight_Concern
      Cut_Val_Weight_Concern_3<<- Mean_Val_Weight_Concern
      Cut_Val_Weight_Concern_4<<- Mean_Val_Weight_Concern + Sd_Val_Weight_Concern
      Cut_Val_Weight_Concern_5<<- Mean_Val_Weight_Concern + (2*Sd_Val_Weight_Concern)
      Cut_Lab_1_Weight_Concern<<- "Mean - 2 Sd"
      Cut_Lab_2_Weight_Concern<<- "Mean - 1 Sd"
      Cut_Lab_3_Weight_Concern<<- "Mean"
      Cut_Lab_4_Weight_Concern<<- "Mean + 1 Sd"
      Cut_Lab_5_Weight_Concern<<- "Mean + 2 Sd"
      Source_Cutoff_Weight_Concern<<- "Mond et al. (2013)"
    }
    
  })
  
  
  output$Mean_Widg<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean", "EDE-Q total scale", Mean_Val),
      h6(paste("Reference:", Source_Mean))
          )
  })
  outputOptions(output, "Mean_Widg", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Restraint<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Restraint", "Restraint", Mean_Val_Restraint),
      h6(paste("Reference:", Source_Mean_Restraint))
          )
  })
  outputOptions(output, "Mean_Widg_Restraint", suspendWhenHidden = FALSE)
  

  output$Mean_Widg_Eating_Concern<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Eating_Concern", "Eating Concern", Mean_Val_Eating_Concern),
      h6(paste("Reference:", Source_Mean_Eating_Concern))
          )
  })
  outputOptions(output, "Mean_Widg_Eating_Concern", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Shape_Concern<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Shape_Concern", "Shape Concern", Mean_Val_Shape_Concern),
      h6(paste("Reference:", Source_Mean_Shape_Concern))
          )
  })
  outputOptions(output, "Mean_Widg_Shape_Concern", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Weight_Concern<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Weight_Concern", "Weight Concern", Mean_Val_Weight_Concern),
      h6(paste("Reference:", Source_Mean_Weight_Concern))
           )
  })
  outputOptions(output, "Mean_Widg_Weight_Concern", suspendWhenHidden = FALSE) 
  
  
  output$Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd", "EDE-Q total scale", Sd_Val),
      h6(paste("Reference:", Source_Mean))
           )
  })
  outputOptions(output, "Sd_Widg", suspendWhenHidden = FALSE)
  

  output$Sd_Widg_Restraint<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Restraint", "Restraint", Sd_Val_Restraint),
      h6(paste("Reference:", Source_Mean_Restraint))
          )
  })
  outputOptions(output, "Sd_Widg_Restraint", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Eating_Concern<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Eating_Concern", "Eating Concern", Sd_Val_Eating_Concern),
      h6(paste("Reference:", Source_Mean_Eating_Concern))
          )
  })
  outputOptions(output, "Sd_Widg_Eating_Concern", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Shape_Concern<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Shape_Concern", "Shape Concern", Sd_Val_Shape_Concern),
      h6(paste("Reference:", Source_Mean_Shape_Concern))
          )
  })
  outputOptions(output, "Sd_Widg_Shape_Concern", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Weight_Concern<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Weight_Concern", "Weight Concern", Sd_Val_Weight_Concern),
      h6(paste("Reference:", Source_Mean_Weight_Concern))
          )
  })
  outputOptions(output, "Sd_Widg_Weight_Concern", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_1", "EDE-Q total scale", as.numeric(Cut_Val_1)),
      textInput("Cutoff_Text_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source_Cutoff))
          )
  })
  outputOptions(output, "Cutoff_Widg_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Restraint_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Restraint_1", "Restraint", as.numeric(Cut_Val_Restraint_1)),
      textInput("Cutoff_Text_Restraint_1", "Cut-Off Score Name", Cut_Lab_1_Restraint),
      h6(paste("Reference:", Source_Cutoff_Restraint))
          )
  })
  outputOptions(output, "Cutoff_Widg_Restraint_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Eating_Concern_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Eating_Concern_1", "Eating Concern", as.numeric(Cut_Val_Eating_Concern_1)),
      textInput("Cutoff_Text_Eating_Concern_1", "Cut-Off Score Name", Cut_Lab_1_Eating_Concern),
      h6(paste("Reference:", Source_Cutoff_Eating_Concern))
           )
  })
  outputOptions(output, "Cutoff_Widg_Eating_Concern_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Shape_Concern_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Shape_Concern_1", "Shape Concern", as.numeric(Cut_Val_Shape_Concern_1)),
      textInput("Cutoff_Text_Shape_Concern_1", "Cut-Off Score Name", Cut_Lab_1_Shape_Concern),
      h6(paste("Reference:", Source_Cutoff_Shape_Concern))
          )
  })
  outputOptions(output, "Cutoff_Widg_Shape_Concern_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Weight_Concern_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Weight_Concern_1", "Weight Concern", as.numeric(Cut_Val_Weight_Concern_1)),
      textInput("Cutoff_Text_Weight_Concern_1", "Cut-Off Score Name", Cut_Lab_1_Weight_Concern),
      h6(paste("Reference:", Source_Cutoff_Weight_Concern))
           )
  })
  outputOptions(output, "Cutoff_Widg_Weight_Concern_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_2", "EDE-Q total scale", as.numeric(Cut_Val_2)),
      textInput("Cutoff_Text_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source_Cutoff))
          )
  })
  outputOptions(output, "Cutoff_Widg_2", suspendWhenHidden = FALSE)
  

  output$Cutoff_Widg_Restraint_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Restraint_2", "Restraint", as.numeric(Cut_Val_Restraint_2)),
      textInput("Cutoff_Text_Restraint_2", "Cut-Off Score Name", Cut_Lab_2_Restraint),
      h6(paste("Reference:", Source_Cutoff_Restraint))
           )
  })
  outputOptions(output, "Cutoff_Widg_Restraint_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Eating_Concern_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Eating_Concern_2", "Eating Concern", as.numeric(Cut_Val_Eating_Concern_2)),
      textInput("Cutoff_Text_Eating_Concern_2", "Cut-Off Score Name", Cut_Lab_2_Eating_Concern),
      h6(paste("Reference:", Source_Cutoff_Eating_Concern))
    )
  })
  outputOptions(output, "Cutoff_Widg_Eating_Concern_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Shape_Concern_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Shape_Concern_2", "Shape Concern", as.numeric(Cut_Val_Shape_Concern_2)),
      textInput("Cutoff_Text_Shape_Concern_2", "Cut-Off Score Name", Cut_Lab_2_Shape_Concern),
      h6(paste("Reference:", Source_Cutoff_Shape_Concern))
          )
  })
  outputOptions(output, "Cutoff_Widg_Shape_Concern_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Weight_Concern_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Weight_Concern_2", "Weight Concern", as.numeric(Cut_Val_Weight_Concern_2)),
      textInput("Cutoff_Text_Weight_Concern_2", "Cut-Off Score Name", Cut_Lab_2_Weight_Concern),
      h6(paste("Reference:", Source_Cutoff_Weight_Concern))
          )
  })
  outputOptions(output, "Cutoff_Widg_Weight_Concern_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_3", "EDE-Q total scale", as.numeric(Cut_Val_3)),
      textInput("Cutoff_Text_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source_Cutoff))
          )
  })
  outputOptions(output, "Cutoff_Widg_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Restraint_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Restraint_3", "Restraint", as.numeric(Cut_Val_Restraint_3)),
      textInput("Cutoff_Text_Restraint_3", "Cut-Off Score Name", Cut_Lab_3_Restraint),
      h6(paste("Reference:", Source_Cutoff_Restraint))
          )
  })
  outputOptions(output, "Cutoff_Widg_Restraint_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Eating_Concern_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Eating_Concern_3", "Eating Concern", as.numeric(Cut_Val_Eating_Concern_3)),
      textInput("Cutoff_Text_Eating_Concern_3", "Cut-Off Score Name", Cut_Lab_3_Eating_Concern),
      h6(paste("Reference:", Source_Cutoff_Eating_Concern))
           )
  })
  outputOptions(output, "Cutoff_Widg_Eating_Concern_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Shape_Concern_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Shape_Concern_3", "Shape Concern", as.numeric(Cut_Val_Shape_Concern_3)),
      textInput("Cutoff_Text_Shape_Concern_3", "Cut-Off Score Name", Cut_Lab_3_Shape_Concern),
      h6(paste("Reference:", Source_Cutoff_Shape_Concern))
          )
  })
  outputOptions(output, "Cutoff_Widg_Shape_Concern_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Weight_Concern_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Weight_Concern_3", "Weight Concern", as.numeric(Cut_Val_Weight_Concern_3)),
      textInput("Cutoff_Text_Weight_Concern_3", "Cut-Off Score Name", Cut_Lab_3_Weight_Concern),
      h6(paste("Reference:", Source_Cutoff_Weight_Concern))
          )
  })
  outputOptions(output, "Cutoff_Widg_Weight_Concern_3", suspendWhenHidden = FALSE)
  

  output$Cutoff_Widg_4<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_4", "EDE-Q total scale", as.numeric(Cut_Val_4)),
      textInput("Cutoff_Text_4", "Cut-Off Score Name", Cut_Lab_4),
      h6(paste("Reference:", Source_Cutoff))
    )
  })
  outputOptions(output, "Cutoff_Widg_4", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Restraint_4<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Restraint_4", "Restraint", as.numeric(Cut_Val_Restraint_4)),
      textInput("Cutoff_Text_Restraint_4", "Cut-Off Score Name", Cut_Lab_4_Restraint),
      h6(paste("Reference:", Source_Cutoff_Restraint))
    )
  })
  outputOptions(output, "Cutoff_Widg_Restraint_4", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Eating_Concern_4<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Eating_Concern_4", "Eating Concern", as.numeric(Cut_Val_Eating_Concern_4)),
      textInput("Cutoff_Text_Eating_Concern_4", "Cut-Off Score Name", Cut_Lab_4_Eating_Concern),
      h6(paste("Reference:", Source_Cutoff_Eating_Concern))
    )
  })
  outputOptions(output, "Cutoff_Widg_Eating_Concern_4", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Shape_Concern_4<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Shape_Concern_4", "Shape Concern", as.numeric(Cut_Val_Shape_Concern_4)),
      textInput("Cutoff_Text_Shape_Concern_4", "Cut-Off Score Name", Cut_Lab_4_Shape_Concern),
      h6(paste("Reference:", Source_Cutoff_Shape_Concern))
    )
  })
  outputOptions(output, "Cutoff_Widg_Shape_Concern_4", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Weight_Concern_4<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Weight_Concern_4", "Weight Concern", as.numeric(Cut_Val_Weight_Concern_4)),
      textInput("Cutoff_Text_Weight_Concern_4", "Cut-Off Score Name", Cut_Lab_4_Weight_Concern),
      h6(paste("Reference:", Source_Cutoff_Weight_Concern))
    )
  })
  outputOptions(output, "Cutoff_Widg_Weight_Concern_4", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_5<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_5", "EDE-Q total scale", as.numeric(Cut_Val_5)),
      textInput("Cutoff_Text_5", "Cut-Off Score Name", Cut_Lab_5),
      h6(paste("Reference:", Source_Cutoff))
    )
  })
  outputOptions(output, "Cutoff_Widg_5", suspendWhenHidden = FALSE)
  

  
  output$Cutoff_Widg_Restraint_5<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Restraint_5", "Restraint", as.numeric(Cut_Val_Restraint_5)),
      textInput("Cutoff_Text_Restraint_5", "Cut-Off Score Name", Cut_Lab_5_Restraint),
      h6(paste("Reference:", Source_Cutoff_Restraint))
    )
  })
  outputOptions(output, "Cutoff_Widg_Restraint_5", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Eating_Concern_5<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Eating_Concern_5", "Eating Concern", as.numeric(Cut_Val_Eating_Concern_5)),
      textInput("Cutoff_Text_Eating_Concern_5", "Cut-Off Score Name", Cut_Lab_5_Eating_Concern),
      h6(paste("Reference:", Source_Cutoff_Eating_Concern))
    )
  })
  outputOptions(output, "Cutoff_Widg_Eating_Concern_5", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Shape_Concern_5<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Shape_Concern_5", "Shape Concern", as.numeric(Cut_Val_Shape_Concern_5)),
      textInput("Cutoff_Text_Shape_Concern_5", "Cut-Off Score Name", Cut_Lab_5_Shape_Concern),
      h6(paste("Reference:", Source_Cutoff_Shape_Concern))
    )
  })
  outputOptions(output, "Cutoff_Widg_Shape_Concern_5", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Weight_Concern_5<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Weight_Concern_5", "Weight Concern", as.numeric(Cut_Val_Weight_Concern_5)),
      textInput("Cutoff_Text_Weight_Concern_5", "Cut-Off Score Name", Cut_Lab_5_Weight_Concern),
      h6(paste("Reference:", Source_Cutoff_Weight_Concern))
    )
  })
  outputOptions(output, "Cutoff_Widg_Weight_Concern_5", suspendWhenHidden = FALSE)
  
  
  #Create a reactive expression defining entered values  
  
  Entered_Scores_Reac<- reactive({
    
    TP<- input$Timepoint
    
    PN<<- input$PatientName
    CN<<- input$ClinicianName
    
    Pop<- input$Pop
    
    RelChangeMethod<- input$RelChangeMethod
    
    Tab_Reference<<- Source_Mean
    
    Binge_Info<<- list(input$Item_13,input$Item_14,input$Item_15,input$Item_16,input$Item_17,input$Item_18)
    
    Basic_Info<<- list(input$Item_29, input$Item_30,input$Item_31,input$Item_32, input$Item_33)
    
    CI_Vals_Reac()
    
    M<- input$Pop_Mean
    SD<- input$Pop_Sd
    M_Restraint<- input$Pop_Mean_Restraint
    SD_Restraint<-input$Pop_Sd_Restraint
    M_Eating_Concern<- input$Pop_Mean_Eating_Concern
    SD_Eating_Concern<- input$Pop_Sd_Eating_Concern
    M_Shape_Concern<- input$Pop_Mean_Shape_Concern
    SD_Shape_Concern<- input$Pop_Sd_Shape_Concern
    M_Weight_Concern<- input$Pop_Mean_Weight_Concern
    SD_Weight_Concern<- input$Pop_Sd_Weight_Concern
    
    M_Retest<- input$Retest_Mean
    SD_Retest<- input$Retest_Sd
    M_Retest_Restraint<- input$Retest_Mean_Restraint
    SD_Retest_Restraint<- input$Retest_Sd_Restraint
    M_Retest_Eating_Concern<- input$Retest_Mean_Eating_Concern
    SD_Retest_Eating_Concern<- input$Retest_Sd_Eating_Concern
    M_Retest_Shape_Concern<- input$Retest_Mean_Shape_Concern
    SD_Retest_Shape_Concern<- input$Retest_Sd_Shape_Concern
    M_Retest_Weight_Concern<- input$Retest_Mean_Weight_Concern
    SD_Retest_Weight_Concern<- input$Retest_Sd_Weight_Concern
    
    SampleN<- input$SampleN
    
    Rel<- input$Reliability
    Rel_Restraint<- input$Reliability_Restraint
    Rel_Eating_Concern<- input$Reliability_Eating_Concern
    Rel_Shape_Concern<- input$Reliability_Shape_Concern
    Rel_Weight_Concern<- input$Reliability_Weight_Concern
    
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
      SE_Restraint<-SD_Restraint * sqrt(1 - Rel_Restraint^2)
      SE_Eating_Concern<-SD_Eating_Concern * sqrt(1 - Rel_Eating_Concern^2)
      SE_Shape_Concern<-SD_Shape_Concern * sqrt(1 - Rel_Shape_Concern^2)
      SE_Weight_Concern<-SD_Weight_Concern * sqrt(1 - Rel_Weight_Concern^2)
      SE<- round(SE, digits = 2)
      SE_Restraint<- round(SE_Restraint, digits = 2)
      SE_Eating_Concern<- round(SE_Eating_Concern, digits = 2)
      SE_Shape_Concern<- round(SE_Shape_Concern, digits = 2)
      SE_Weight_Concern<- round(SE_Weight_Concern, digits = 2)
    } else if (input$RelChangeMethod == "Chelune et al. (1993)" | input$RelChangeMethod == "Jacobson & Truax (1991)" | input$RelChangeMethod == "Speer (1992)") {
      SE<- sqrt((2*(SD^2))*(1-Rel))
      SE_Restraint<- sqrt((2*(SD_Restraint^2))*(1-Rel_Restraint))
      SE_Eating_Concern<- sqrt((2*(SD_Eating_Concern^2))*(1-Rel_Eating_Concern))
      SE_Shape_Concern<- sqrt((2*(SD_Shape_Concern^2))*(1-Rel_Shape_Concern))
      SE_Weight_Concern<- sqrt((2*(SD_Weight_Concern^2))*(1-Rel_Weight_Concern))
      SE<- round(SE, digits = 2)
      SE_Restraint<- round(SE_Restraint, digits = 2)
      SE_Eating_Concern<- round(SE_Eating_Concern, digits = 2)
      SE_Shape_Concern<- round(SE_Shape_Concern, digits = 2)
      SE_Weight_Concern<- round(SE_Weight_Concern, digits = 2)
    } else if (input$RelChangeMethod == "Maassen et al. (2006)") {
      SE<- sqrt((SD^2 + SD_Retest^2)*(1-Rel))
      SE_Restraint<- sqrt((SD_Restraint^2 + SD_Retest_Restraint^2)*(1-Rel_Restraint))
      SE_Eating_Concern<- sqrt((SD_Eating_Concern^2 + SD_Retest_Eating_Concern^2)*(1-Rel_Eating_Concern))
      SE_Shape_Concern<- sqrt((SD_Shape_Concern^2 + SD_Retest_Shape_Concern^2)*(1-Rel_Shape_Concern))
      SE_Weight_Concern<- sqrt((SD_Weight_Concern^2 + SD_Retest_Weight_Concern^2)*(1-Rel_Weight_Concern))
      SE<- round(SE, digits = 2)
      SE_Restraint<- round(SE_Restraint, digits = 2)
      SE_Eating_Concern<- round(SE_Eating_Concern, digits = 2)
      SE_Shape_Concern<- round(SE_Shape_Concern, digits = 2)
      SE_Weight_Concern<- round(SE_Weight_Concern, digits = 2)
    } else if (input$RelChangeMethod == "McSweeny et al. (1993)") {
      SE<- SD_Retest*sqrt(1 - Rel^2)
      SE_Restraint<- SD_Retest_Restraint*sqrt(1 - Rel_Restraint^2)
      SE_Eating_Concern<- SD_Retest_Eating_Concern*sqrt(1 - Rel_Eating_Concern^2)
      SE_Shape_Concern<- SD_Retest_Shape_Concern*sqrt(1 - Rel_Shape_Concern^2)
      SE_Weight_Concern<- SD_Retest_Weight_Concern*sqrt(1 - Rel_Weight_Concern^2)
      SE<- round(SE, digits = 2)
      SE_Restraint<- round(SE_Restraint, digits = 2)
      SE_Eating_Concern<- round(SE_Eating_Concern, digits = 2)
      SE_Shape_Concern<- round(SE_Shape_Concern, digits = 2)
      SE_Weight_Concern<- round(SE_Weight_Concern, digits = 2)
    }
    
    
    #Caclculate McSweeny standard error to be employed in deriving the Crawford & Howell standard error if selected
    
    McSweeny_SE<- SD_Retest*sqrt(1 - Rel^2)
    McSweeny_SE_Restraint<- SD_Retest_Restraint*sqrt(1 - Rel_Restraint^2)
    McSweeny_SE_Eating_Concern<- SD_Retest_Eating_Concern*sqrt(1 - Rel_Eating_Concern^2)
    McSweeny_SE_Shape_Concern<- SD_Retest_Shape_Concern*sqrt(1 - Rel_Shape_Concern^2)
    McSweeny_SE_Weight_Concern<- SD_Retest_Weight_Concern*sqrt(1 - Rel_Weight_Concern^2)
    
    #Generate object storing names of cut-off scores
    
    Cutoff_Name_1<- input$Cutoff_Text_1
    Cutoff_Name_2<- input$Cutoff_Text_2
    Cutoff_Name_3<- input$Cutoff_Text_3
    Cutoff_Name_4<- input$Cutoff_Text_4
    Cutoff_Name_5<- input$Cutoff_Text_5
    Cutoff_Name_Restraint_1<- input$Cutoff_Text_Restraint_1
    Cutoff_Name_Restraint_2<- input$Cutoff_Text_Restraint_2
    Cutoff_Name_Restraint_3<- input$Cutoff_Text_Restraint_3
    Cutoff_Name_Restraint_4<- input$Cutoff_Text_Restraint_4
    Cutoff_Name_Restraint_5<- input$Cutoff_Text_Restraint_5
    Cutoff_Name_Eating_Concern_1<- input$Cutoff_Text_Eating_Concern_1
    Cutoff_Name_Eating_Concern_2<- input$Cutoff_Text_Eating_Concern_2
    Cutoff_Name_Eating_Concern_3<- input$Cutoff_Text_Eating_Concern_3
    Cutoff_Name_Eating_Concern_4<- input$Cutoff_Text_Eating_Concern_4
    Cutoff_Name_Eating_Concern_5<- input$Cutoff_Text_Eating_Concern_5
    Cutoff_Name_Shape_Concern_1<- input$Cutoff_Text_Shape_Concern_1
    Cutoff_Name_Shape_Concern_2<- input$Cutoff_Text_Shape_Concern_2
    Cutoff_Name_Shape_Concern_3<- input$Cutoff_Text_Shape_Concern_3
    Cutoff_Name_Shape_Concern_4<- input$Cutoff_Text_Shape_Concern_4
    Cutoff_Name_Shape_Concern_5<- input$Cutoff_Text_Shape_Concern_5
    Cutoff_Name_Weight_Concern_1<- input$Cutoff_Text_Weight_Concern_1
    Cutoff_Name_Weight_Concern_2<- input$Cutoff_Text_Weight_Concern_2
    Cutoff_Name_Weight_Concern_3<- input$Cutoff_Text_Weight_Concern_3
    Cutoff_Name_Weight_Concern_4<- input$Cutoff_Text_Weight_Concern_4
    Cutoff_Name_Weight_Concern_5<- input$Cutoff_Text_Weight_Concern_5
    
    Cutoff_Names<<- data.frame(Cutoff_Name_1,Cutoff_Name_2,Cutoff_Name_3, Cutoff_Name_4,Cutoff_Name_5,
                               Cutoff_Name_Restraint_1,Cutoff_Name_Restraint_2,Cutoff_Name_Restraint_3, Cutoff_Name_Restraint_4,Cutoff_Name_Restraint_5,
                               Cutoff_Name_Eating_Concern_1, Cutoff_Name_Eating_Concern_2, Cutoff_Name_Eating_Concern_3, Cutoff_Name_Eating_Concern_4, Cutoff_Name_Eating_Concern_5,
                               Cutoff_Name_Shape_Concern_1, Cutoff_Name_Shape_Concern_2, Cutoff_Name_Shape_Concern_3, Cutoff_Name_Shape_Concern_4, Cutoff_Name_Shape_Concern_5, 
                               Cutoff_Name_Weight_Concern_1, Cutoff_Name_Weight_Concern_2, Cutoff_Name_Weight_Concern_3, Cutoff_Name_Weight_Concern_4, Cutoff_Name_Weight_Concern_5)
    
    #Define the data & calculate PS, SE, confidence intervals and cut-off scores
    
    if(input$Timepoint == "1") {
      Date<- input$Date_1
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_1a), Score = Score_1a)
      Score_Restraint<- mean(Score_1a[c(1,2,3,4,5)], na.rm = TRUE)
      Score_Restraint<- round(Score_Restraint, digits = 2)
      Score_Eating_Concern<- mean(Score_1a[c(7,9,19,20,21)], na.rm = TRUE)
      Score_Eating_Concern<- round(Score_Eating_Concern, digits = 2)
      Score_Shape_Concern<- mean(Score_1a[c(8,6,10,11,23,26,27,28)], na.rm = TRUE)
      Score_Shape_Concern<- round(Score_Shape_Concern, digits = 2)
      Score_Weight_Concern<- mean(Score_1a[c(8,12,22,24,25)], na.rm = TRUE)
      Score_Weight_Concern<- round(Score_Weight_Concern, digits = 2)
      Score<- mean(Score_Restraint, Score_Eating_Concern, Score_Shape_Concern, Score_Weight_Concern, na.rm = TRUE)
      Score<- round(Score, digits = 2)
      Change<- 0
      Change_Restraint<- 0
      Change_Eating_Concern<- 0
      Change_Shape_Concern<- 0
      Change_Weight_Concern<- 0
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS<- (Rel * Score) + (M * (1 - Rel))
        PTS_Restraint<- (Rel_Restraint * Score_Restraint) + (M_Restraint * (1 - Rel_Restraint))
        PTS_Eating_Concern<- (Rel_Eating_Concern * Score_Eating_Concern) + (M_Eating_Concern * (1 - Rel_Eating_Concern))
        PTS_Shape_Concern<- (Rel_Shape_Concern * Score_Shape_Concern) + (M_Shape_Concern * (1 - Rel_Shape_Concern))
        PTS_Weight_Concern<- (Rel_Weight_Concern * Score_Weight_Concern) + (M_Weight_Concern * (1 - Rel_Weight_Concern))
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS<- Score + (M_Retest - M)  
        PTS_Restraint<- Score_Restraint + (M_Retest_Restraint - M_Restraint)  
        PTS_Eating_Concern<- Score_Eating_Concern + (M_Retest_Eating_Concern - M_Eating_Concern)  
        PTS_Shape_Concern<- Score_Shape_Concern + (M_Retest_Shape_Concern - M_Shape_Concern) 
        PTS_Weight_Concern<- Score_Weight_Concern + (M_Retest_Weight_Concern - M_Weight_Concern) 
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Restraint<- Score_Restraint
        PTS_Eating_Concern<- Score_Eating_Concern
        PTS_Shape_Concern<- Score_Shape_Concern
        PTS_Weight_Concern<- Score_Weight_Concern
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS<- (B_Adj * Score) + A_Adj
        B_Slope_Restraint<- Rel_Restraint * (SD_Retest_Restraint/SD_Restraint)
        A_Constant_Restraint<- M_Retest_Restraint - (B_Slope_Restraint * M_Restraint)
        B_Adj_Restraint<- SD_Retest_Restraint/SD_Restraint
        A_Adj_Restraint<- M_Retest_Restraint - (B_Adj_Restraint * M_Restraint)
        PTS_Restraint<- (B_Adj_Restraint * Score_Restraint) + A_Adj_Restraint
        B_Slope_Eating_Concern<- Rel_Eating_Concern * (SD_Retest_Eating_Concern/SD_Eating_Concern)
        A_Constant_Eating_Concern<- M_Retest_Eating_Concern - (B_Slope_Eating_Concern * M_Eating_Concern)
        B_Adj_Eating_Concern<- SD_Retest_Eating_Concern/SD_Eating_Concern
        A_Adj_Eating_Concern<- M_Retest_Eating_Concern - (B_Adj_Eating_Concern * M_Eating_Concern)
        PTS_Eating_Concern<- (B_Adj_Eating_Concern * Score_Eating_Concern) + A_Adj_Eating_Concern
        B_Slope_Shape_Concern<- Rel_Shape_Concern * (SD_Retest_Shape_Concern/SD_Shape_Concern)
        A_Constant_Shape_Concern<- M_Retest_Shape_Concern - (B_Slope_Shape_Concern * M_Shape_Concern)
        B_Adj_Shape_Concern<- SD_Retest_Shape_Concern/SD_Shape_Concern
        A_Adj_Shape_Concern<- M_Retest_Shape_Concern - (B_Adj_Shape_Concern * M_Shape_Concern)
        PTS_Shape_Concern<- (B_Adj_Shape_Concern * Score_Shape_Concern) + A_Adj_Shape_Concern
        B_Slope_Weight_Concern<- Rel_Weight_Concern * (SD_Retest_Weight_Concern/SD_Weight_Concern)
        A_Constant_Weight_Concern<- M_Retest_Weight_Concern - (B_Slope_Weight_Concern * M_Weight_Concern)
        B_Adj_Weight_Concern<- SD_Retest_Weight_Concern/SD_Weight_Concern
        A_Adj_Weight_Concern<- M_Retest_Weight_Concern - (B_Adj_Weight_Concern * M_Weight_Concern)
        PTS_Weight_Concern<- (B_Adj_Weight_Concern * Score_Weight_Concern) + A_Adj_Weight_Concern
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS<- B_Slope * Score
        B_Slope_Restraint<- Rel_Restraint * (SD_Retest_Restraint/SD_Restraint)
        PTS_Restraint<- B_Slope_Restraint * Score_Restraint
        B_Slope_Eating_Concern<- Rel_Eating_Concern * (SD_Retest_Eating_Concern/SD_Eating_Concern)
        PTS_Eating_Concern<- B_Slope_Eating_Concern * Score_Eating_Concern
        B_Slope_Shape_Concern<- Rel_Shape_Concern * (SD_Retest_Shape_Concern/SD_Shape_Concern)
        PTS_Shape_Concern<- B_Slope_Shape_Concern * Score_Shape_Concern
        B_Slope_Weight_Concern<- Rel_Weight_Concern * (SD_Retest_Weight_Concern/SD_Weight_Concern)
        PTS_Weight_Concern<- B_Slope_Weight_Concern * Score_Weight_Concern
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS<- Score + (M_Retest - M)
        PTS_Restraint<- Score_Restraint + (M_Retest_Restraint - M_Restraint)
        PTS<- Eating_Concern<- Score_Eating_Concern + (M_Retest_Eating_Concern - M_Eating_Concern)
        PTS_Shape_Concern<- Score_Shape_Concern + (M_Retest_Shape_Concern - M_Shape_Concern)
        PTS_Weight_Concern<- Score_Weight_Concern + (M_Retest_Weight_Concern - M_Weight_Concern)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Restraint<- round(PTS_Restraint, digits = 2)
      PTS_Eating_Concern<- round(PTS_Eating_Concern, digits = 2)
      PTS_Shape_Concern<- round(PTS_Shape_Concern, digits = 2)
      PTS_Weight_Concern<- round(PTS_Weight_Concern, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score - M)^2/(SD^2*(SampleN-1))))
        SE_Restraint<- McSweeny_SE_Restraint*sqrt(1 + (1/SampleN) + ((Score_Restraint - M_Restraint)^2/(SD_Restraint^2*(SampleN-1))))
        SE_Eating_Concern<- McSweeny_SE_Eating_Concern*sqrt(1 + (1/SampleN) + ((Score_Eating_Concern - M_Eating_Concern)^2/(SD_Eating_Concern^2*(SampleN-1))))
        SE_Shape_Concern<- McSweeny_SE_Shape_Concern*sqrt(1 + (1/SampleN) + ((Score_Shape_Concern - M_Shape_Concern)^2/(SD_Shape_Concern^2*(SampleN-1))))
        SE_Weight_Concern<- McSweeny_SE_Weight_Concern*sqrt(1 + (1/SampleN) + ((Score_Weight_Concern - M_Weight_Concern)^2/(SD_Weight_Concern^2*(SampleN-1))))
        SE<- round(SE, digits = 2)
        SE_Restraint<- round(SE_Restraint, digits = 2)
        SE_Eating_Concern<- round(SE_Eating_Concern, digits = 2)
        SE_Shape_Concern<- round(SE_Shape_Concern, digits = 2)
        SE_Weight_Concern<- round(SE_Weight_Concern, digits = 2)
        CI<- (Conf*SE)
        CI<- round(CI, digits = 2)
        CI_Restraint<- (Conf*SE_Restraint)
        CI_Restraint<- round(CI_Restraint, digits = 2)
        CI_Eating_Concern<- (Conf*SE_Eating_Concern)
        CI_Eating_Concern<- round(CI_Eating_Concern, digits = 2)
        CI_Shape_Concern<- (Conf*SE_Shape_Concern)
        CI_Shape_Concern<- round(CI_Shape_Concern, digits = 2)
        CI_Weight_Concern<- (Conf*SE_Weight_Concern)
        CI_Weight_Concern<- round(CI_Weight_Concern, digits = 2)
      }
      CI<- (Conf*SE)
      CI<- round(CI, digits = 2)
      CI_Restraint<- (Conf*SE_Restraint)
      CI_Restraint<- round(CI_Restraint, digits = 2)
      CI_Eating_Concern<- (Conf*SE_Eating_Concern)
      CI_Eating_Concern<- round(CI_Eating_Concern, digits = 2)
      CI_Shape_Concern<- (Conf*SE_Shape_Concern)
      CI_Shape_Concern<- round(CI_Shape_Concern, digits = 2)
      CI_Weight_Concern<- (Conf*SE_Weight_Concern)
      CI_Weight_Concern<- round(CI_Weight_Concern, digits = 2)
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Restraint<- PTS_Restraint + CI_Restraint
      CI_Upper_Lim_Restraint<- round(CI_Upper_Lim_Restraint, digits = 2)
      CI_Lower_Lim_Restraint<-PTS_Restraint - CI_Restraint
      CI_Lower_Lim_Restraint<- round(CI_Lower_Lim_Restraint, digits = 2)
      CI_Upper_Lim_Eating_Concern<- PTS_Eating_Concern + CI_Eating_Concern
      CI_Upper_Lim_Eating_Concern<- round(CI_Upper_Lim_Eating_Concern, digits = 2)
      CI_Lower_Lim_Eating_Concern<-PTS_Eating_Concern - CI_Eating_Concern
      CI_Lower_Lim_Eating_Concern<- round(CI_Lower_Lim_Eating_Concern, digits = 2)
      CI_Upper_Lim_Shape_Concern<- PTS_Shape_Concern + CI_Shape_Concern
      CI_Upper_Lim_Shape_Concern<- round(CI_Upper_Lim_Shape_Concern, digits = 2)
      CI_Lower_Lim_Shape_Concern<-PTS_Shape_Concern - CI_Shape_Concern
      CI_Lower_Lim_Shape_Concern<- round(CI_Lower_Lim_Shape_Concern, digits = 2)
      CI_Upper_Lim_Weight_Concern<- PTS_Weight_Concern + CI_Weight_Concern
      CI_Upper_Lim_Weight_Concern<- round(CI_Upper_Lim_Weight_Concern, digits = 2)
      CI_Lower_Lim_Weight_Concern<-PTS_Weight_Concern - CI_Weight_Concern
      CI_Lower_Lim_Weight_Concern<- round(CI_Lower_Lim_Weight_Concern, digits = 2)
      if(input$Select_CI == "2") {
        CI<- input$Man_CI
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Restraint == "2") {
        CI_Restraint<- input$Man_CI_Restraint
        CI_Restraint<- round(CI_Restraint, digits = 2)
        CI_Upper_Lim_Restraint<- Score_Restraint + CI_Restraint
        CI_Upper_Lim_Restraint<- round(CI_Upper_Lim_Restraint, digits = 2)
        CI_Lower_Lim_Restraint<- Score_Restraint - CI_Restraint
        CI_Lower_Lim_Restraint<- round(CI_Lower_Lim_Restraint, digits = 2)
      }
      if(input$Select_CI_Eating_Concern == "2") {
        CI_Eating_Concern<- input$Man_CI_Eating_Concern
        CI_Eating_Concern<- round(CI_Eating_Concern, digits = 2)
        CI_Upper_Lim_Eating_Concern<- Score_Eating_Concern + CI_Eating_Concern
        CI_Upper_Lim_Eating_Concern<- round(CI_Upper_Lim_Eating_Concern, digits = 2)
        CI_Lower_Lim_Eating_Concern<- Score_Eating_Concern - CI_Eating_Concern
        CI_Lower_Lim_Eating_Concern<- round(CI_Lower_Lim_Eating_Concern, digits = 2)
      }
      if(input$Select_CI_Shape_Concern == "2") {
        CI_Shape_Concern<- input$Man_CI_Shape_Concern
        CI_Shape_Concern<- round(CI_Shape_Concern, digits = 2)
        CI_Upper_Lim_Shape_Concern<- Score_Shape_Concern + CI_Shape_Concern
        CI_Upper_Lim_Shape_Concern<- round(CI_Upper_Lim_Shape_Concern, digits = 2)
        CI_Lower_Lim_Shape_Concern<- Score_Shape_Concern - CI_Shape_Concern
        CI_Lower_Lim_Shape_Concern<- round(CI_Lower_Lim_Shape_Concern, digits = 2)
      }
      if(input$Select_CI_Weight_Concern == "2") {
        CI_Weight_Concern<- input$Man_CI_Weight_Concern
        CI_Weight_Concern<- round(CI_Weight_Concern, digits = 2)
        CI_Upper_Lim_Weight_Concern<- Score_Weight_Concern + CI_Weight_Concern
        CI_Upper_Lim_Weight_Concern<- round(CI_Upper_Lim_Weight_Concern, digits = 2)
        CI_Lower_Lim_Weight_Concern<- Score_Weight_Concern - CI_Weight_Concern
        CI_Lower_Lim_Weight_Concern<- round(CI_Lower_Lim_Weight_Concern, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_4<- round(input$Cutoff_4, digits = 2)
      Cutoff_Score_5<- round(input$Cutoff_5, digits = 2)
      Cutoff_Score_Restraint_1<- round(input$Cutoff_Restraint_1, digits = 2)
      Cutoff_Score_Restraint_2<- round(input$Cutoff_Restraint_2, digits = 2)
      Cutoff_Score_Restraint_3<- round(input$Cutoff_Restraint_3, digits = 2)
      Cutoff_Score_Restraint_4<- round(input$Cutoff_Restraint_4, digits = 2)
      Cutoff_Score_Restraint_5<- round(input$Cutoff_Restraint_5, digits = 2)
      Cutoff_Score_Eating_Concern_1<- round(input$Cutoff_Eating_Concern_1, digits = 2)
      Cutoff_Score_Eating_Concern_2<- round(input$Cutoff_Eating_Concern_2, digits = 2)
      Cutoff_Score_Eating_Concern_3<- round(input$Cutoff_Eating_Concern_3, digits = 2)
      Cutoff_Score_Eating_Concern_4<- round(input$Cutoff_Eating_Concern_4, digits = 2)
      Cutoff_Score_Eating_Concern_5<- round(input$Cutoff_Eating_Concern_5, digits = 2)
      Cutoff_Score_Shape_Concern_1<- round(input$Cutoff_Shape_Concern_1, digits = 2)
      Cutoff_Score_Shape_Concern_2<- round(input$Cutoff_Shape_Concern_2, digits = 2)
      Cutoff_Score_Shape_Concern_3<- round(input$Cutoff_Shape_Concern_3, digits = 2)
      Cutoff_Score_Shape_Concern_4<- round(input$Cutoff_Shape_Concern_4, digits = 2)
      Cutoff_Score_Shape_Concern_5<- round(input$Cutoff_Shape_Concern_5, digits = 2)
      Cutoff_Score_Weight_Concern_1<- round(input$Cutoff_Weight_Concern_1, digits = 2)
      Cutoff_Score_Weight_Concern_2<- round(input$Cutoff_Weight_Concern_2, digits = 2)
      Cutoff_Score_Weight_Concern_3<- round(input$Cutoff_Weight_Concern_3, digits = 2)
      Cutoff_Score_Weight_Concern_4<- round(input$Cutoff_Weight_Concern_4, digits = 2)
      Cutoff_Score_Weight_Concern_5<- round(input$Cutoff_Weight_Concern_5, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3, Cutoff_Score_4, Cutoff_Score_5,
                                      Score_Restraint,Change_Restraint,PTS_Restraint, SE_Restraint, CI_Upper_Lim_Restraint, CI_Lower_Lim_Restraint, Cutoff_Score_Restraint_1,Cutoff_Score_Restraint_2,Cutoff_Score_Restraint_3, Cutoff_Score_Restraint_4,Cutoff_Score_Restraint_5,
                                      Score_Eating_Concern,Change_Eating_Concern, PTS_Eating_Concern, SE_Eating_Concern, CI_Upper_Lim_Eating_Concern, CI_Lower_Lim_Eating_Concern, Cutoff_Score_Eating_Concern_1,Cutoff_Score_Eating_Concern_2,Cutoff_Score_Eating_Concern_3, Cutoff_Score_Eating_Concern_4,Cutoff_Score_Eating_Concern_5, 
                                      Score_Shape_Concern,Change_Shape_Concern,PTS_Shape_Concern, SE_Shape_Concern, CI_Upper_Lim_Shape_Concern, CI_Lower_Lim_Shape_Concern, Cutoff_Score_Shape_Concern_1,Cutoff_Score_Shape_Concern_2,Cutoff_Score_Shape_Concern_3, Cutoff_Score_Shape_Concern_4,Cutoff_Score_Shape_Concern_5, 
                                      Score_Weight_Concern,Change_Weight_Concern,PTS_Weight_Concern, SE_Weight_Concern, CI_Upper_Lim_Weight_Concern, CI_Lower_Lim_Weight_Concern, Cutoff_Score_Weight_Concern_1,Cutoff_Score_Weight_Concern_2,Cutoff_Score_Weight_Concern_3, Cutoff_Score_Weight_Concern_4,Cutoff_Score_Weight_Concern_5)
    } else if(input$Timepoint == "2") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date<- c(Date_1, Date_2)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<- as.numeric(unlist(strsplit(input$Text_1,",")))
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_2a), Score = Score_2a)
      Score_Restraint_1<- mean(Score_1a[c(1,2,3,4,5)], na.rm = TRUE)
      Score_Restraint_2<- mean(Score_2a[c(1,2,3,4,5)], na.rm = TRUE)
      Score_Restraint<- c(Score_Restraint_1,Score_Restraint_2)
      Score_Restraint<- round(Score_Restraint, digits = 2)
      Score_Eating_Concern_1<- mean(Score_1a[c(7,9,19,20,21)], na.rm = TRUE)
      Score_Eating_Concern_2<- mean(Score_2a[c(7,9,19,20,21)], na.rm = TRUE)
      Score_Eating_Concern<- c(Score_Eating_Concern_1,Score_Eating_Concern_2)
      Score_Eating_Concern<- round(Score_Eating_Concern, digits = 2)
      Score_Shape_Concern_1<- mean(Score_1a[c(8,6,10,11,23,26,27,28)], na.rm = TRUE)
      Score_Shape_Concern_2<- mean(Score_2a[c(8,6,10,11,23,26,27,28)], na.rm = TRUE)
      Score_Shape_Concern<- c(Score_Shape_Concern_1,Score_Shape_Concern_2)
      Score_Shape_Concern<- round(Score_Shape_Concern, digits = 2)
      Score_Weight_Concern_1<- mean(Score_1a[c(8,12,22,24,25)], na.rm = TRUE)
      Score_Weight_Concern_2<- mean(Score_2a[c(8,12,22,24,25)], na.rm = TRUE)
      Score_Weight_Concern<- c(Score_Weight_Concern_1,Score_Weight_Concern_2)
      Score_Weight_Concern<- round(Score_Weight_Concern, digits = 2)
      Score_1<- mean(Score_Restraint_1, Score_Eating_Concern_1, Score_Shape_Concern_1, Score_Weight_Concern_1, na.rm = TRUE)
      Score_2<- mean(Score_Restraint_2, Score_Eating_Concern_2, Score_Shape_Concern_2, Score_Weight_Concern_2, na.rm = TRUE)
      Score<- c(Score_1, Score_2)
      Score<- round(Score, digits = 2)
      Change<- c(0, (Score_2 - Score_1))
      Change<- round(Change, digits = 2)
      Change_Restraint<- c(0, (Score_Restraint_2 - Score_Restraint_1))
      Change_Restraint<- round(Change_Restraint, digits = 2)
      Change_Eating_Concern<- c(0, (Score_Eating_Concern_2 - Score_Eating_Concern_1))
      Change_Eating_Concern<- round(Change_Eating_Concern, digits = 2)
      Change_Shape_Concern<- c(0, (Score_Shape_Concern_2 - Score_Shape_Concern_1))
      Change_Shape_Concern<- round(Change_Shape_Concern, digits = 2)
      Change_Weight_Concern<- c(0, (Score_Weight_Concern_2 - Score_Weight_Concern_1))
      Change_Weight_Concern<- round(Change_Weight_Concern, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2)
        PTS_Restraint_1<- (Rel_Restraint * Score_Restraint_1) + (M_Restraint * (1 - Rel_Restraint))
        PTS_Restraint_2<- (Rel_Restraint * Score_Restraint_2) + (M_Restraint * (1 - Rel_Restraint))
        PTS_Restraint<-c(PTS_Restraint_1, PTS_Restraint_2)
        PTS_Eating_Concern_1<- (Rel_Eating_Concern * Score_Eating_Concern_1) + (M_Eating_Concern * (1 - Rel_Eating_Concern))
        PTS_Eating_Concern_2<- (Rel_Eating_Concern * Score_Eating_Concern_2) + (M_Eating_Concern * (1 - Rel_Eating_Concern))
        PTS_Eating_Concern<-c(PTS_Eating_Concern_1,PTS_Eating_Concern_2)
        PTS_Shape_Concern_1<- (Rel_Shape_Concern * Score_Shape_Concern_1) + (M_Shape_Concern * (1 - Rel_Shape_Concern))
        PTS_Shape_Concern_2<- (Rel_Shape_Concern * Score_Shape_Concern_2) + (M_Shape_Concern * (1 - Rel_Shape_Concern))
        PTS_Shape_Concern<-c(PTS_Shape_Concern_1, PTS_Shape_Concern_2)
        PTS_Weight_Concern_1<- (Rel_Weight_Concern * Score_Weight_Concern_1) + (M_Weight_Concern * (1 - Rel_Weight_Concern))
        PTS_Weight_Concern_2<- (Rel_Weight_Concern * Score_Weight_Concern_2) + (M_Weight_Concern * (1 - Rel_Weight_Concern))
        PTS_Weight_Concern<-c(PTS_Weight_Concern_1, PTS_Weight_Concern_2)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2)
        PTS_Restraint_1<- Score_Restraint_1 + (M_Retest_Restraint - M_Restraint)  
        PTS_Restraint_2<- Score_Restraint_2 + (M_Retest_Restraint - M_Restraint)
        PTS_Restraint<- c(PTS_Restraint_1, PTS_Restraint_2)
        PTS_Eating_Concern_1<- Score_Eating_Concern_1 + (M_Retest_Eating_Concern - M_Eating_Concern)  
        PTS_Eating_Concern_2<- Score_Eating_Concern_2 + (M_Retest_Eating_Concern - M_Eating_Concern)
        PTS_Eating_Concern<- c(PTS_Eating_Concern_1, PTS_Eating_Concern_2)
        PTS_Shape_Concern_1<- Score_Shape_Concern_1 + (M_Retest_Shape_Concern - M_Shape_Concern)  
        PTS_Shape_Concern_2<- Score_Shape_Concern_2 + (M_Retest_Shape_Concern - M_Shape_Concern)
        PTS_Shape_Concern<- c(PTS_Shape_Concern_1, PTS_Shape_Concern_2)
        PTS_Weight_Concern_1<- Score_Weight_Concern_1 + (M_Retest_Weight_Concern - M_Weight_Concern)  
        PTS_Weight_Concern_2<- Score_Weight_Concern_2 + (M_Retest_Weight_Concern - M_Weight_Concern)
        PTS_Weight_Concern<- c(PTS_Weight_Concern_1, PTS_Weight_Concern_2)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS<- c(PTS_1,PTS_2)
        PTS_Restraint_1<- Score_Restraint_1
        PTS_Restraint_2<- Score_Restraint_2
        PTS_Restraint<- c(PTS_Restraint_1, PTS_Restraint_2)
        PTS_Eating_Concern_1<- Score_Eating_Concern_1
        PTS_Eating_Concern_2<- Score_Eating_Concern_2
        PTS_Eating_Concern<- c(PTS_Eating_Concern_1, PTS_Eating_Concern_2)
        PTS_Shape_Concern_1<- Score_Shape_Concern_1
        PTS_Shape_Concern_2<- Score_Shape_Concern_2
        PTS_Shape_Concern<- c(PTS_Shape_Concern_1, PTS_Shape_Concern_2)
        PTS_Weight_Concern_1<- Score_Weight_Concern_1
        PTS_Weight_Concern_2<- Score_Weight_Concern_2
        PTS_Weight_Concern<- c(PTS_Weight_Concern_1, PTS_Weight_Concern_2)
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Restraint<- Rel_Restraint * (SD_Retest_Restraint/SD_Restraint)
        A_Constant_Restraint<- M_Retest_Restraint - (B_Slope_Restraint * M_Restraint)
        B_Adj_Restraint<- SD_Retest_Restraint/SD_Restraint
        A_Adj_Restraint<- M_Retest_Restraint - (B_Adj_Restraint * M_Restraint)
        PTS_Restraint_1<- (B_Adj_Restraint * Score_Restraint_1) + A_Adj_Restraint
        PTS_Restraint_2<- (B_Adj_Restraint * Score_Restraint_2) + A_Adj_Restraint
        PTS_Restraint<- c(PTS_Restraint_1,PTS_Restraint_2)
        B_Slope_Eating_Concern<- Rel_Eating_Concern * (SD_Retest_Eating_Concern/SD_Eating_Concern)
        A_Constant_Eating_Concern<- M_Retest_Eating_Concern - (B_Slope_Eating_Concern * M_Eating_Concern)
        B_Adj_Eating_Concern<- SD_Retest_Eating_Concern/SD_Eating_Concern
        A_Adj_Eating_Concern<- M_Retest_Eating_Concern - (B_Adj_Eating_Concern * M_Eating_Concern)
        PTS_Eating_Concern_1<- (B_Adj_Eating_Concern * Score_Eating_Concern_1) + A_Adj_Eating_Concern
        PTS_Eating_Concern_2<- (B_Adj_Eating_Concern * Score_Eating_Concern_2) + A_Adj_Eating_Concern
        PTS_Eating_Concern<- c(PTS_Eating_Concern_1,PTS_Eating_Concern_2)
        B_Slope_Shape_Concern<- Rel_Shape_Concern * (SD_Retest_Shape_Concern/SD_Shape_Concern)
        A_Constant_Shape_Concern<- M_Retest_Shape_Concern - (B_Slope_Shape_Concern * M_Shape_Concern)
        B_Adj_Shape_Concern<- SD_Retest_Shape_Concern/SD_Shape_Concern
        A_Adj_Shape_Concern<- M_Retest_Shape_Concern - (B_Adj_Shape_Concern * M_Shape_Concern)
        PTS_Shape_Concern_1<- (B_Adj_Shape_Concern * Score_Shape_Concern_1) + A_Adj_Shape_Concern
        PTS_Shape_Concern_2<- (B_Adj_Shape_Concern * Score_Shape_Concern_2) + A_Adj_Shape_Concern
        PTS_Shape_Concern<- c(PTS_Shape_Concern_1,PTS_Shape_Concern_2)
        B_Slope_Weight_Concern<- Rel_Weight_Concern * (SD_Retest_Weight_Concern/SD_Weight_Concern)
        A_Constant_Weight_Concern<- M_Retest_Weight_Concern - (B_Slope_Weight_Concern * M_Weight_Concern)
        B_Adj_Weight_Concern<- SD_Retest_Weight_Concern/SD_Weight_Concern
        A_Adj_Weight_Concern<- M_Retest_Weight_Concern - (B_Adj_Weight_Concern * M_Weight_Concern)
        PTS_Weight_Concern_1<- (B_Adj_Weight_Concern * Score_Weight_Concern_1) + A_Adj_Weight_Concern
        PTS_Weight_Concern_2<- (B_Adj_Weight_Concern * Score_Weight_Concern_2) + A_Adj_Weight_Concern
        PTS_Weight_Concern<- c(PTS_Weight_Concern_1,PTS_Weight_Concern_2)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Restraint<- Rel_Restraint * (SD_Retest_Restraint/SD_Restraint)
        PTS_Restraint_1<- B_Slope_Restraint * Score_Restraint_1
        PTS_Restraint_2<- B_Slope_Restraint * Score_Restraint_2
        PTS_Restraint<- c(PTS_Restraint_1, PTS_Restraint_2)
        B_Slope_Eating_Concern<- Rel_Eating_Concern * (SD_Retest_Eating_Concern/SD_Eating_Concern)
        PTS_Eating_Concern_1<- B_Slope_Eating_Concern * Score_Eating_Concern_1
        PTS_Eating_Concern_2<- B_Slope_Eating_Concern * Score_Eating_Concern_2
        PTS_Eating_Concern<- c(PTS_Eating_Concern_1, PTS_Eating_Concern_2)
        B_Slope_Shape_Concern<- Rel_Shape_Concern * (SD_Retest_Shape_Concern/SD_Shape_Concern)
        PTS_Shape_Concern_1<- B_Slope_Shape_Concern * Score_Shape_Concern_1
        PTS_Shape_Concern_2<- B_Slope_Shape_Concern * Score_Shape_Concern_2
        PTS_Shape_Concern<- c(PTS_Shape_Concern_1, PTS_Shape_Concern_2)
        B_Slope_Weight_Concern<- Rel_Weight_Concern * (SD_Retest_Weight_Concern/SD_Weight_Concern)
        PTS_Weight_Concern_1<- B_Slope_Weight_Concern * Score_Weight_Concern_1
        PTS_Weight_Concern_2<- B_Slope_Weight_Concern * Score_Weight_Concern_2
        PTS_Weight_Concern<- c(PTS_Weight_Concern_1, PTS_Weight_Concern_2)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2)
        PTS_Restraint_1<- Score_Restraint_1 + (M_Retest_Restraint - M_Restraint)
        PTS_Restraint_2<- Score_Restraint_2 + (M_Retest_Restraint - M_Restraint)
        PTS_Restraint<- c(PTS_Restraint_1, PTS_Restraint_2)
        PTS_Eating_Concern_1<- Score_Eating_Concern_1 + (M_Retest_Eating_Concern - M_Eating_Concern)
        PTS_Eating_Concern_2<- Score_Eating_Concern_2 + (M_Retest_Eating_Concern - M_Eating_Concern)
        PTS_Eating_Concern<- c(PTS_Eating_Concern_1, PTS_Eating_Concern_2)
        PTS_Shape_Concern_1<- Score_Shape_Concern_1 + (M_Retest_Shape_Concern - M_Shape_Concern)
        PTS_Shape_Concern_2<- Score_Shape_Concern_2 + (M_Retest_Shape_Concern - M_Shape_Concern)
        PTS_Shape_Concern<- c(PTS_Shape_Concern_1, PTS_Shape_Concern_2)
        PTS_Weight_Concern_1<- Score_Weight_Concern_1 + (M_Retest_Weight_Concern - M_Weight_Concern)
        PTS_Weight_Concern_2<- Score_Weight_Concern_2 + (M_Retest_Weight_Concern - M_Weight_Concern)
        PTS_Weight_Concern<- c(PTS_Weight_Concern_1, PTS_Weight_Concern_2)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Restraint<- round(PTS_Restraint, digits = 2)
      PTS_Eating_Concern<- round(PTS_Eating_Concern, digits = 2)
      PTS_Shape_Concern<- round(PTS_Shape_Concern, digits = 2)
      PTS_Weight_Concern<- round(PTS_Weight_Concern, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2)
        SE_Restraint_1<- McSweeny_SE_Restraint*sqrt(1 + (1/SampleN) + ((Score_Restraint_1 - M_Restraint)^2/(SD_Restraint^2*(SampleN-1))))
        SE_Restraint_2<- McSweeny_SE_Restraint*sqrt(1 + (1/SampleN) + ((Score_Restraint_2 - M_Restraint)^2/(SD_Restraint^2*(SampleN-1))))
        SE_Restraint<- c(SE_Restraint_1, SE_Restraint_2)
        SE_Eating_Concern_1<- McSweeny_SE_Eating_Concern*sqrt(1 + (1/SampleN) + ((Score_Eating_Concern_1 - M_Eating_Concern)^2/(SD_Eating_Concern^2*(SampleN-1))))
        SE_Eating_Concern_2<- McSweeny_SE_Eating_Concern*sqrt(1 + (1/SampleN) + ((Score_Eating_Concern_2 - M_Eating_Concern)^2/(SD_Eating_Concern^2*(SampleN-1))))
        SE_Eating_Concern<-c(SE_Eating_Concern_1, SE_Eating_Concern_2)
        SE_Shape_Concern_1<- McSweeny_SE_Shape_Concern*sqrt(1 + (1/SampleN) + ((Score_Shape_Concern_1 - M_Shape_Concern)^2/(SD_Shape_Concern^2*(SampleN-1))))
        SE_Shape_Concern_2<- McSweeny_SE_Shape_Concern*sqrt(1 + (1/SampleN) + ((Score_Shape_Concern_2 - M_Shape_Concern)^2/(SD_Shape_Concern^2*(SampleN-1))))
        SE_Shape_Concern<- c(SE_Shape_Concern_1, SE_Shape_Concern_2)
        SE_Weight_Concern_1<- McSweeny_SE_Weight_Concern*sqrt(1 + (1/SampleN) + ((Score_Weight_Concern_1 - M_Weight_Concern)^2/(SD_Weight_Concern^2*(SampleN-1))))
        SE_Weight_Concern_2<- McSweeny_SE_Weight_Concern*sqrt(1 + (1/SampleN) + ((Score_Weight_Concern_2 - M_Weight_Concern)^2/(SD_Weight_Concern^2*(SampleN-1))))
        SE_Weight_Concern<- c(SE_Weight_Concern_1, SE_Weight_Concern_2)
        SE<- round(SE, digits = 2)
        SE_Restraint<- round(SE_Restraint, digits = 2)
        SE_Eating_Concern<- round(SE_Eating_Concern, digits = 2)
        SE_Shape_Concern<- round(SE_Shape_Concern, digits = 2)
        SE_Weight_Concern<- round(SE_Weight_Concern, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2))
        CI<- round(CI, digits = 2)
        CI_Restraint<- c((Conf*SE_Restraint_1), (Conf*SE_Restraint_2))
        CI_Restraint<- round(CI_Restraint, digits = 2)
        CI_Eating_Concern<- c((Conf*SE_Eating_Concern_1), (Conf*SE_Eating_Concern_2))
        CI_Eating_Concern<- round(CI_Eating_Concern, digits = 2)
        CI_Shape_Concern<- c((Conf*SE_Shape_Concern_1), (Conf*SE_Shape_Concern_2))
        CI_Shape_Concern<- round(CI_Shape_Concern, digits = 2)
        CI_Weight_Concern<- c((Conf*SE_Weight_Concern_1), (Conf*SE_Weight_Concern_2))
        CI_Weight_Concern<- round(CI_Weight_Concern, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Restraint<- c((Conf*SE_Restraint), (Conf*SE_Restraint))
        CI_Restraint<- round(CI_Restraint, digits = 2)
        CI_Eating_Concern<- c((Conf*SE_Eating_Concern), (Conf*SE_Eating_Concern))
        CI_Eating_Concern<- round(CI_Eating_Concern, digits = 2)
        CI_Shape_Concern<- c((Conf*SE_Shape_Concern), (Conf*SE_Shape_Concern))
        CI_Shape_Concern<- round(CI_Shape_Concern, digits = 2)
        CI_Weight_Concern<- c((Conf*SE_Weight_Concern), (Conf*SE_Weight_Concern))
        CI_Weight_Concern<- round(CI_Weight_Concern, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Restraint<- PTS_Restraint + CI_Restraint
      CI_Upper_Lim_Restraint<- round(CI_Upper_Lim_Restraint, digits = 2)
      CI_Lower_Lim_Restraint<-PTS_Restraint - CI_Restraint
      CI_Lower_Lim_Restraint<- round(CI_Lower_Lim_Restraint, digits = 2)
      CI_Upper_Lim_Eating_Concern<- PTS_Eating_Concern + CI_Eating_Concern
      CI_Upper_Lim_Eating_Concern<- round(CI_Upper_Lim_Eating_Concern, digits = 2)
      CI_Lower_Lim_Eating_Concern<-PTS_Eating_Concern - CI_Eating_Concern
      CI_Lower_Lim_Eating_Concern<- round(CI_Lower_Lim_Eating_Concern, digits = 2)
      CI_Upper_Lim_Shape_Concern<- PTS_Shape_Concern + CI_Shape_Concern
      CI_Upper_Lim_Shape_Concern<- round(CI_Upper_Lim_Shape_Concern, digits = 2)
      CI_Lower_Lim_Shape_Concern<-PTS_Shape_Concern - CI_Shape_Concern
      CI_Lower_Lim_Shape_Concern<- round(CI_Lower_Lim_Shape_Concern, digits = 2)
      CI_Upper_Lim_Weight_Concern<- PTS_Weight_Concern + CI_Weight_Concern
      CI_Upper_Lim_Weight_Concern<- round(CI_Upper_Lim_Weight_Concern, digits = 2)
      CI_Lower_Lim_Weight_Concern<-PTS_Weight_Concern - CI_Weight_Concern
      CI_Lower_Lim_Weight_Concern<- round(CI_Lower_Lim_Weight_Concern, digits = 2)
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Restraint == "2") {
        CI_Restraint<- input$Man_CI_Restraint
        CI_Restraint<- c(CI_Restraint, CI_Restraint)
        CI_Restraint<- round(CI_Restraint, digits = 2)
        CI_Upper_Lim_Restraint<- Score_Restraint + CI_Restraint
        CI_Upper_Lim_Restraint<- round(CI_Upper_Lim_Restraint, digits = 2)
        CI_Lower_Lim_Restraint<- Score_Restraint - CI_Restraint
        CI_Lower_Lim_Restraint<- round(CI_Lower_Lim_Restraint, digits = 2)
      }
      if(input$Select_CI_Eating_Concern == "2") {
        CI_Eating_Concern<- input$Man_CI_Eating_Concern
        CI_Eating_Concern<- c(CI_Eating_Concern, CI_Eating_Concern)
        CI_Eating_Concern<- round(CI_Eating_Concern, digits = 2)
        CI_Upper_Lim_Eating_Concern<- Score_Eating_Concern + CI_Eating_Concern
        CI_Upper_Lim_Eating_Concern<- round(CI_Upper_Lim_Eating_Concern, digits = 2)
        CI_Lower_Lim_Eating_Concern<- Score_Eating_Concern - CI_Eating_Concern
        CI_Lower_Lim_Eating_Concern<- round(CI_Lower_Lim_Eating_Concern, digits = 2)
      } 
      if(input$Select_CI_Shape_Concern == "2") {
        CI_Shape_Concern<- input$Man_CI_Shape_Concern
        CI_Shape_Concern<- c(CI_Shape_Concern, CI_Shape_Concern)
        CI_Shape_Concern<- round(CI_Shape_Concern, digits = 2)
        CI_Upper_Lim_Shape_Concern<- Score_Shape_Concern + CI_Shape_Concern
        CI_Upper_Lim_Shape_Concern<- round(CI_Upper_Lim_Shape_Concern, digits = 2)
        CI_Lower_Lim_Shape_Concern<- Score_Shape_Concern - CI_Shape_Concern
        CI_Lower_Lim_Shape_Concern<- round(CI_Lower_Lim_Shape_Concern, digits = 2)
      } 
      if(input$Select_CI_Weight_Concern == "2") {
        CI_Weight_Concern<- input$Man_CI_Weight_Concern
        CI_Weight_Concern<- c(CI_Weight_Concern, CI_Weight_Concern)
        CI_Weight_Concern<- round(CI_Weight_Concern, digits = 2)
        CI_Upper_Lim_Weight_Concern<- Score_Weight_Concern + CI_Weight_Concern
        CI_Upper_Lim_Weight_Concern<- round(CI_Upper_Lim_Weight_Concern, digits = 2)
        CI_Lower_Lim_Weight_Concern<- Score_Weight_Concern - CI_Weight_Concern
        CI_Lower_Lim_Weight_Concern<- round(CI_Lower_Lim_Weight_Concern, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_4<- round(input$Cutoff_4, digits = 2)
      Cutoff_Score_5<- round(input$Cutoff_5, digits = 2)
      Cutoff_Score_Restraint_1<- round(input$Cutoff_Restraint_1, digits = 2)
      Cutoff_Score_Restraint_2<- round(input$Cutoff_Restraint_2, digits = 2)
      Cutoff_Score_Restraint_3<- round(input$Cutoff_Restraint_3, digits = 2)
      Cutoff_Score_Restraint_4<- round(input$Cutoff_Restraint_4, digits = 2)
      Cutoff_Score_Restraint_5<- round(input$Cutoff_Restraint_5, digits = 2)
      Cutoff_Score_Eating_Concern_1<- round(input$Cutoff_Eating_Concern_1, digits = 2)
      Cutoff_Score_Eating_Concern_2<- round(input$Cutoff_Eating_Concern_2, digits = 2)
      Cutoff_Score_Eating_Concern_3<- round(input$Cutoff_Eating_Concern_3, digits = 2)
      Cutoff_Score_Eating_Concern_4<- round(input$Cutoff_Eating_Concern_4, digits = 2)
      Cutoff_Score_Eating_Concern_5<- round(input$Cutoff_Eating_Concern_5, digits = 2)
      Cutoff_Score_Shape_Concern_1<- round(input$Cutoff_Shape_Concern_1, digits = 2)
      Cutoff_Score_Shape_Concern_2<- round(input$Cutoff_Shape_Concern_2, digits = 2)
      Cutoff_Score_Shape_Concern_3<- round(input$Cutoff_Shape_Concern_3, digits = 2)
      Cutoff_Score_Shape_Concern_4<- round(input$Cutoff_Shape_Concern_4, digits = 2)
      Cutoff_Score_Shape_Concern_5<- round(input$Cutoff_Shape_Concern_5, digits = 2)
      Cutoff_Score_Weight_Concern_1<- round(input$Cutoff_Weight_Concern_1, digits = 2)
      Cutoff_Score_Weight_Concern_2<- round(input$Cutoff_Weight_Concern_2, digits = 2)
      Cutoff_Score_Weight_Concern_3<- round(input$Cutoff_Weight_Concern_3, digits = 2)
      Cutoff_Score_Weight_Concern_4<- round(input$Cutoff_Weight_Concern_4, digits = 2)
      Cutoff_Score_Weight_Concern_5<- round(input$Cutoff_Weight_Concern_5, digits = 2)
      Cutoff_Score_1<- rep(Cutoff_Score_1, 2)
      Cutoff_Score_2<- rep(Cutoff_Score_2, 2)
      Cutoff_Score_3<- rep(Cutoff_Score_3, 2)
      Cutoff_Score_4<- rep(Cutoff_Score_4, 2)
      Cutoff_Score_5<- rep(Cutoff_Score_5, 2)
      Cutoff_Score_Restraint_1<- rep(Cutoff_Score_Restraint_1, 2)
      Cutoff_Score_Restraint_2<- rep(Cutoff_Score_Restraint_2, 2)
      Cutoff_Score_Restraint_3<- rep(Cutoff_Score_Restraint_3, 2)
      Cutoff_Score_Restraint_4<- rep(Cutoff_Score_Restraint_4, 2)
      Cutoff_Score_Restraint_5<- rep(Cutoff_Score_Restraint_5, 2)
      Cutoff_Score_Eating_Concern_1<- rep(Cutoff_Score_Eating_Concern_1, 2)
      Cutoff_Score_Eating_Concern_2<- rep(Cutoff_Score_Eating_Concern_2, 2)
      Cutoff_Score_Eating_Concern_3<- rep(Cutoff_Score_Eating_Concern_3, 2)
      Cutoff_Score_Eating_Concern_4<- rep(Cutoff_Score_Eating_Concern_4, 2)
      Cutoff_Score_Eating_Concern_5<- rep(Cutoff_Score_Eating_Concern_5, 2)
      Cutoff_Score_Shape_Concern_1<- rep(Cutoff_Score_Shape_Concern_1, 2)
      Cutoff_Score_Shape_Concern_2<- rep(Cutoff_Score_Shape_Concern_2, 2)
      Cutoff_Score_Shape_Concern_3<- rep(Cutoff_Score_Shape_Concern_3, 2)
      Cutoff_Score_Shape_Concern_4<- rep(Cutoff_Score_Shape_Concern_4, 2)
      Cutoff_Score_Shape_Concern_5<- rep(Cutoff_Score_Shape_Concern_5, 2)
      Cutoff_Score_Weight_Concern_1<- rep(Cutoff_Score_Weight_Concern_1, 2)
      Cutoff_Score_Weight_Concern_2<- rep(Cutoff_Score_Weight_Concern_2, 2)
      Cutoff_Score_Weight_Concern_3<- rep(Cutoff_Score_Weight_Concern_3, 2)
      Cutoff_Score_Weight_Concern_4<- rep(Cutoff_Score_Weight_Concern_4, 2)
      Cutoff_Score_Weight_Concern_5<- rep(Cutoff_Score_Weight_Concern_5, 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3, Cutoff_Score_4, Cutoff_Score_5,
                                      Score_Restraint,Change_Restraint,PTS_Restraint, SE_Restraint, CI_Upper_Lim_Restraint, CI_Lower_Lim_Restraint, Cutoff_Score_Restraint_1,Cutoff_Score_Restraint_2,Cutoff_Score_Restraint_3, Cutoff_Score_Restraint_4,Cutoff_Score_Restraint_5,
                                      Score_Eating_Concern,Change_Eating_Concern, PTS_Eating_Concern, SE_Eating_Concern, CI_Upper_Lim_Eating_Concern, CI_Lower_Lim_Eating_Concern, Cutoff_Score_Eating_Concern_1,Cutoff_Score_Eating_Concern_2,Cutoff_Score_Eating_Concern_3, Cutoff_Score_Eating_Concern_4,Cutoff_Score_Eating_Concern_5, 
                                      Score_Shape_Concern,Change_Shape_Concern,PTS_Shape_Concern, SE_Shape_Concern, CI_Upper_Lim_Shape_Concern, CI_Lower_Lim_Shape_Concern, Cutoff_Score_Shape_Concern_1,Cutoff_Score_Shape_Concern_2,Cutoff_Score_Shape_Concern_3, Cutoff_Score_Shape_Concern_4,Cutoff_Score_Shape_Concern_5, 
                                      Score_Weight_Concern,Change_Weight_Concern,PTS_Weight_Concern, SE_Weight_Concern, CI_Upper_Lim_Weight_Concern, CI_Lower_Lim_Weight_Concern, Cutoff_Score_Weight_Concern_1,Cutoff_Score_Weight_Concern_2,Cutoff_Score_Weight_Concern_3, Cutoff_Score_Weight_Concern_4,Cutoff_Score_Weight_Concern_5)
    } else if(input$Timepoint == "3") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date_3<- input$Date_3
      Date<- c(Date_1, Date_2, Date_3)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<- as.numeric(unlist(strsplit(input$Text_1,",")))
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Score_3a<<- as.numeric(unlist(strsplit(input$Text_3,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_3a), Score = Score_3a)
      Score_Restraint_1<- mean(Score_1a[c(1,2,3,4,5)], na.rm = TRUE)
      Score_Restraint_2<- mean(Score_2a[c(1,2,3,4,5)], na.rm = TRUE)
      Score_Restraint_3<- mean(Score_3a[c(1,2,3,4,5)], na.rm = TRUE)
      Score_Restraint<- c(Score_Restraint_1,Score_Restraint_2,Score_Restraint_3)
      Score_Restraint<- round(Score_Restraint, digits = 2)
      Score_Eating_Concern_1<- mean(Score_1a[c(7,9,19,20,21)], na.rm = TRUE)
      Score_Eating_Concern_2<- mean(Score_2a[c(7,9,19,20,21)], na.rm = TRUE)
      Score_Eating_Concern_3<- mean(Score_3a[c(7,9,19,20,21)], na.rm = TRUE)
      Score_Eating_Concern<- c(Score_Eating_Concern_1,Score_Eating_Concern_2, Score_Eating_Concern_3)
      Score_Eating_Concern<- round(Score_Eating_Concern, digits = 2)
      Score_Shape_Concern_1<- mean(Score_1a[c(8,6,10,11,23,26,27,28)], na.rm = TRUE)
      Score_Shape_Concern_2<- mean(Score_2a[c(8,6,10,11,23,26,27,28)], na.rm = TRUE)
      Score_Shape_Concern_3<- mean(Score_3a[c(8,6,10,11,23,26,27,28)], na.rm = TRUE)
      Score_Shape_Concern<- c(Score_Shape_Concern_1,Score_Shape_Concern_2,Score_Shape_Concern_3)
      Score_Shape_Concern<- round(Score_Shape_Concern, digits = 2)
      Score_Weight_Concern_1<- mean(Score_1a[c(8,12,22,24,25)], na.rm = TRUE)
      Score_Weight_Concern_2<- mean(Score_2a[c(8,12,22,24,25)], na.rm = TRUE)
      Score_Weight_Concern_3<- mean(Score_3a[c(8,12,22,24,25)], na.rm = TRUE)
      Score_Weight_Concern<- c(Score_Weight_Concern_1,Score_Weight_Concern_2,Score_Weight_Concern_3)
      Score_Weight_Concern<- round(Score_Weight_Concern, digits = 2)
      Score_1<- mean(Score_Restraint_1, Score_Eating_Concern_1, Score_Shape_Concern_1, Score_Weight_Concern_1, na.rm = TRUE)
      Score_2<- mean(Score_Restraint_2, Score_Eating_Concern_2, Score_Shape_Concern_2, Score_Weight_Concern_2, na.rm = TRUE)
      Score_3<- mean(Score_Restraint_3, Score_Eating_Concern_3, Score_Shape_Concern_3, Score_Weight_Concern_3, na.rm = TRUE)
      Score<- c(Score_1, Score_2, Score_3)
      Score<- round(Score, digits = 2)
      Change<- c(0, (Score_2 - Score_1), (Score_3 - Score_2))
      Change<- round(Change, digits = 2)
      Change_Restraint<- c(0, Score_Restraint_2 - Score_Restraint_1, Score_Restraint_3 - Score_Restraint_2)
      Change_Restraint<- round(Change_Restraint, digits = 2)
      Change_Eating_Concern<- c(0, Score_Eating_Concern_2 - Score_Eating_Concern_1, Score_Eating_Concern_3 - Score_Eating_Concern_2)
      Change_Eating_Concern<- round(Change_Eating_Concern, digits = 2)
      Change_Shape_Concern<- c(0, Score_Shape_Concern_2 - Score_Shape_Concern_1, Score_Shape_Concern_3 - Score_Shape_Concern_2)
      Change_Shape_Concern<- round(Change_Shape_Concern, digits = 2)
      Change_Weight_Concern<- c(0, Score_Weight_Concern_2 - Score_Weight_Concern_1, Score_Weight_Concern_3 - Score_Weight_Concern_2)
      Change_Weight_Concern<- round(Change_Weight_Concern, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS_3<- (Rel * Score_3) + (M * (1 - Rel))
        PTS<<- c(PTS_1, PTS_2, PTS_3)
        PTS_Restraint_1<- (Rel_Restraint * Score_Restraint_1) + (M_Restraint * (1 - Rel_Restraint))
        PTS_Restraint_2<- (Rel_Restraint * Score_Restraint_2) + (M_Restraint * (1 - Rel_Restraint))
        PTS_Restraint_3<- (Rel_Restraint * Score_Restraint_3) + (M_Restraint * (1 - Rel_Restraint))
        PTS_Restraint<<- c(PTS_Restraint_1, PTS_Restraint_2, PTS_Restraint_3)
        PTS_Eating_Concern_1<- (Rel_Eating_Concern * Score_Eating_Concern_1) + (M_Eating_Concern * (1 - Rel_Eating_Concern))
        PTS_Eating_Concern_2<- (Rel_Eating_Concern * Score_Eating_Concern_2) + (M_Eating_Concern * (1 - Rel_Eating_Concern))
        PTS_Eating_Concern_3<- (Rel_Eating_Concern * Score_Eating_Concern_3) + (M_Eating_Concern * (1 - Rel_Eating_Concern))
        PTS_Eating_Concern<- c(PTS_Eating_Concern_1,PTS_Eating_Concern_2, PTS_Eating_Concern_3)
        PTS_Shape_Concern_1<- (Rel_Shape_Concern * Score_Shape_Concern_1) + (M_Shape_Concern * (1 - Rel_Shape_Concern))
        PTS_Shape_Concern_2<- (Rel_Shape_Concern * Score_Shape_Concern_2) + (M_Shape_Concern * (1 - Rel_Shape_Concern))
        PTS_Shape_Concern_3<- (Rel_Shape_Concern * Score_Shape_Concern_3) + (M_Shape_Concern * (1 - Rel_Shape_Concern))
        PTS_Shape_Concern<<- c(PTS_Shape_Concern_1, PTS_Shape_Concern_2, PTS_Shape_Concern_3)
        PTS_Weight_Concern_1<- (Rel_Weight_Concern * Score_Weight_Concern_1) + (M_Weight_Concern * (1 - Rel_Weight_Concern))
        PTS_Weight_Concern_2<- (Rel_Weight_Concern * Score_Weight_Concern_2) + (M_Weight_Concern * (1 - Rel_Weight_Concern))
        PTS_Weight_Concern_3<- (Rel_Weight_Concern * Score_Weight_Concern_3) + (M_Weight_Concern * (1 - Rel_Weight_Concern))
        PTS_Weight_Concern<<- c(PTS_Weight_Concern_1, PTS_Weight_Concern_2, PTS_Weight_Concern_3)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Restraint_1<- Score_Restraint_1 + (M_Retest_Restraint - M_Restraint)  
        PTS_Restraint_2<- Score_Restraint_2 + (M_Retest_Restraint - M_Restraint)
        PTS_Restraint_3<- Score_Restraint_3 + (M_Retest_Restraint - M_Restraint)
        PTS_Restraint<- c(PTS_Restraint_1, PTS_Restraint_2, PTS_Restraint_3)
        PTS_Eating_Concern_1<- Score_Eating_Concern_1 + (M_Retest_Eating_Concern - M_Eating_Concern)  
        PTS_Eating_Concern_2<- Score_Eating_Concern_2 + (M_Retest_Eating_Concern - M_Eating_Concern)
        PTS_Eating_Concern_3<- Score_Eating_Concern_3 + (M_Retest_Eating_Concern - M_Eating_Concern)
        PTS_Eating_Concern<- c(PTS_Eating_Concern_1, PTS_Eating_Concern_2, PTS_Eating_Concern_3)
        PTS_Shape_Concern_1<- Score_Shape_Concern_1 + (M_Retest_Shape_Concern - M_Shape_Concern)  
        PTS_Shape_Concern_2<- Score_Shape_Concern_2 + (M_Retest_Shape_Concern - M_Shape_Concern)
        PTS_Shape_Concern_3<- Score_Shape_Concern_3 + (M_Retest_Shape_Concern - M_Shape_Concern)
        PTS_Shape_Concern<- c(PTS_Shape_Concern_1, PTS_Shape_Concern_2, PTS_Shape_Concern_3)
        PTS_Weight_Concern_1<- Score_Weight_Concern_1 + (M_Retest_Weight_Concern - M_Weight_Concern)  
        PTS_Weight_Concern_2<- Score_Weight_Concern_2 + (M_Retest_Weight_Concern - M_Weight_Concern)
        PTS_Weight_Concern_3<- Score_Weight_Concern_3 + (M_Retest_Weight_Concern - M_Weight_Concern)
        PTS_Weight_Concern<- c(PTS_Weight_Concern_1, PTS_Weight_Concern_2, PTS_Weight_Concern_3)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS_3<- Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Restraint_1<- Score_Restraint_1
        PTS_Restraint_2<- Score_Restraint_2
        PTS_Restraint_3<- Score_Restraint_3
        PTS_Restraint<- c(PTS_Restraint_1, PTS_Restraint_2, PTS_Restraint_3)
        PTS_Eating_Concern_1<- Score_Eating_Concern_1
        PTS_Eating_Concern_2<- Score_Eating_Concern_2
        PTS_Eating_Concern_3<- Score_Eating_Concern_3
        PTS_Eating_Concern<- c(PTS_Eating_Concern_1, PTS_Eating_Concern_2, PTS_Eating_Concern_3)
        PTS_Shape_Concern_1<- Score_Shape_Concern_1
        PTS_Shape_Concern_2<- Score_Shape_Concern_2
        PTS_Shape_Concern_3<- Score_Shape_Concern_3
        PTS_Shape_Concern<- c(PTS_Shape_Concern_1, PTS_Shape_Concern_2, PTS_Shape_Concern_3)
        PTS_Weight_Concern_1<- Score_Weight_Concern_1
        PTS_Weight_Concern_2<- Score_Weight_Concern_2
        PTS_Weight_Concern_3<- Score_Weight_Concern_3
        PTS_Weight_Concern<- c(PTS_Weight_Concern_1, PTS_Weight_Concern_2, PTS_Weight_Concern_3)
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS_3<- (B_Adj * Score_3) + A_Adj
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Restraint<- Rel_Restraint * (SD_Retest_Restraint/SD_Restraint)
        A_Constant_Restraint<- M_Retest_Restraint - (B_Slope_Restraint * M_Restraint)
        B_Adj_Restraint<- SD_Retest_Restraint/SD_Restraint
        A_Adj_Restraint<- M_Retest_Restraint - (B_Adj_Restraint * M_Restraint)
        PTS_Restraint_1<- (B_Adj_Restraint * Score_Restraint_1) + A_Adj_Restraint
        PTS_Restraint_2<- (B_Adj_Restraint * Score_Restraint_2) + A_Adj_Restraint
        PTS_Restraint_3<- (B_Adj_Restraint * Score_Restraint_3) + A_Adj_Restraint
        PTS_Restraint<- c(PTS_Restraint_1, PTS_Restraint_2, PTS_Restraint_3)
        B_Slope_Eating_Concern<- Rel_Eating_Concern * (SD_Retest_Eating_Concern/SD_Eating_Concern)
        A_Constant_Eating_Concern<- M_Retest_Eating_Concern - (B_Slope_Eating_Concern * M_Eating_Concern)
        B_Adj_Eating_Concern<- SD_Retest_Eating_Concern/SD_Eating_Concern
        A_Adj_Eating_Concern<- M_Retest_Eating_Concern - (B_Adj_Eating_Concern * M_Eating_Concern)
        PTS_Eating_Concern_1<- (B_Adj_Eating_Concern * Score_Eating_Concern_1) + A_Adj_Eating_Concern
        PTS_Eating_Concern_2<- (B_Adj_Eating_Concern * Score_Eating_Concern_2) + A_Adj_Eating_Concern
        PTS_Eating_Concern_3<- (B_Adj_Eating_Concern * Score_Eating_Concern_3) + A_Adj_Eating_Concern
        PTS_Eating_Concern<- c(PTS_Eating_Concern_1, PTS_Eating_Concern_2, PTS_Eating_Concern_3)
        B_Slope_Shape_Concern<- Rel_Shape_Concern * (SD_Retest_Shape_Concern/SD_Shape_Concern)
        A_Constant_Shape_Concern<- M_Retest_Shape_Concern - (B_Slope_Shape_Concern * M_Shape_Concern)
        B_Adj_Shape_Concern<- SD_Retest_Shape_Concern/SD_Shape_Concern
        A_Adj_Shape_Concern<- M_Retest_Shape_Concern - (B_Adj_Shape_Concern * M_Shape_Concern)
        PTS_Shape_Concern_1<- (B_Adj_Shape_Concern * Score_Shape_Concern_1) + A_Adj_Shape_Concern
        PTS_Shape_Concern_2<- (B_Adj_Shape_Concern * Score_Shape_Concern_2) + A_Adj_Shape_Concern
        PTS_Shape_Concern_3<- (B_Adj_Shape_Concern * Score_Shape_Concern_3) + A_Adj_Shape_Concern
        PTS_Shape_Concern<- c(PTS_Shape_Concern_1, PTS_Shape_Concern_2, PTS_Shape_Concern_3)
        B_Slope_Weight_Concern<- Rel_Weight_Concern * (SD_Retest_Weight_Concern/SD_Weight_Concern)
        A_Constant_Weight_Concern<- M_Retest_Weight_Concern - (B_Slope_Weight_Concern * M_Weight_Concern)
        B_Adj_Weight_Concern<- SD_Retest_Weight_Concern/SD_Weight_Concern
        A_Adj_Weight_Concern<- M_Retest_Weight_Concern - (B_Adj_Weight_Concern * M_Weight_Concern)
        PTS_Weight_Concern_1<- (B_Adj_Weight_Concern * Score_Weight_Concern_1) + A_Adj_Weight_Concern
        PTS_Weight_Concern_2<- (B_Adj_Weight_Concern * Score_Weight_Concern_2) + A_Adj_Weight_Concern
        PTS_Weight_Concern_3<- (B_Adj_Weight_Concern * Score_Weight_Concern_3) + A_Adj_Weight_Concern
        PTS_Weight_Concern<- c(PTS_Weight_Concern_1, PTS_Weight_Concern_2, PTS_Weight_Concern_3)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope * Score_1
        PTS_2<- B_Slope * Score_2
        PTS_3<- B_Slope * Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Restraint<- Rel_Restraint * (SD_Retest_Restraint/SD_Restraint)
        PTS_Restraint_1<- B_Slope_Restraint * Score_Restraint_1
        PTS_Restraint_2<- B_Slope_Restraint * Score_Restraint_2
        PTS_Restraint_3<- B_Slope_Restraint * Score_Restraint_3
        PTS_Restraint<- c(PTS_Restraint_1, PTS_Restraint_2, PTS_Restraint_3)
        B_Slope_Eating_Concern<- Rel_Eating_Concern * (SD_Retest_Eating_Concern/SD_Eating_Concern)
        PTS_Eating_Concern_1<- B_Slope_Eating_Concern * Score_Eating_Concern_1
        PTS_Eating_Concern_2<- B_Slope_Eating_Concern * Score_Eating_Concern_2
        PTS_Eating_Concern_3<- B_Slope_Eating_Concern * Score_Eating_Concern_3
        PTS_Eating_Concern<- c(PTS_Eating_Concern_1, PTS_Eating_Concern_2, PTS_Eating_Concern_3)
        B_Slope_Shape_Concern<- Rel_Shape_Concern * (SD_Retest_Shape_Concern/SD_Shape_Concern)
        PTS_Shape_Concern_1<- B_Slope_Shape_Concern * Score_Shape_Concern_1
        PTS_Shape_Concern_2<- B_Slope_Shape_Concern * Score_Shape_Concern_2
        PTS_Shape_Concern_3<- B_Slope_Shape_Concern * Score_Shape_Concern_3
        PTS_Shape_Concern<- c(PTS_Shape_Concern_1, PTS_Shape_Concern_2, PTS_Shape_Concern_3)
        B_Slope_Weight_Concern<- Rel_Weight_Concern * (SD_Retest_Weight_Concern/SD_Weight_Concern)
        PTS_Weight_Concern_1<- B_Slope_Weight_Concern * Score_Weight_Concern_1
        PTS_Weight_Concern_2<- B_Slope_Weight_Concern * Score_Weight_Concern_2
        PTS_Weight_Concern_3<- B_Slope_Weight_Concern * Score_Weight_Concern_3
        PTS_Weight_Concern<- c(PTS_Weight_Concern_1, PTS_Weight_Concern_2, PTS_Weight_Concern_3)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Restraint_1<- Score_Restraint_1 + (M_Retest_Restraint - M_Restraint)
        PTS_Restraint_2<- Score_Restraint_2 + (M_Retest_Restraint - M_Restraint)
        PTS_Restraint_3<- Score_Restraint_3 + (M_Retest_Restraint - M_Restraint)
        PTS_Restraint<- c(PTS_Restraint_1, PTS_Restraint_2, PTS_Restraint_3)
        PTS_Eating_Concern_1<- Score_Eating_Concern_1 + (M_Retest_Eating_Concern - M_Eating_Concern)
        PTS_Eating_Concern_2<- Score_Eating_Concern_2 + (M_Retest_Eating_Concern - M_Eating_Concern)
        PTS_Eating_Concern_3<- Score_Eating_Concern_3 + (M_Retest_Eating_Concern - M_Eating_Concern)
        PTS_Eating_Concern<- c(PTS_Eating_Concern_1, PTS_Eating_Concern_2, PTS_Eating_Concern_3)
        PTS_Shape_Concern_1<- Score_Shape_Concern_1 + (M_Retest_Shape_Concern - M_Shape_Concern)
        PTS_Shape_Concern_2<- Score_Shape_Concern_2 + (M_Retest_Shape_Concern - M_Shape_Concern)
        PTS_Shape_Concern_3<- Score_Shape_Concern_3 + (M_Retest_Shape_Concern - M_Shape_Concern)
        PTS_Shape_Concern<- c(PTS_Shape_Concern_1, PTS_Shape_Concern_2, PTS_Shape_Concern_3)
        PTS_Weight_Concern_1<- Score_Weight_Concern_1 + (M_Retest_Weight_Concern - M_Weight_Concern)
        PTS_Weight_Concern_2<- Score_Weight_Concern_2 + (M_Retest_Weight_Concern - M_Weight_Concern)
        PTS_Weight_Concern_3<- Score_Weight_Concern_3 + (M_Retest_Weight_Concern - M_Weight_Concern)
        PTS_Weight_Concern<- c(PTS_Weight_Concern_1, PTS_Weight_Concern_2, PTS_Weight_Concern_3)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Restraint<- round(PTS_Restraint, digits = 2)
      PTS_Eating_Concern<- round(PTS_Eating_Concern, digits = 2)
      PTS_Shape_Concern<- round(PTS_Shape_Concern, digits = 2)
      PTS_Weight_Concern<- round(PTS_Weight_Concern, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE3<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_3 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2, SE3)
        SE_Restraint_1<- McSweeny_SE_Restraint*sqrt(1 + (1/SampleN) + ((Score_Restraint_1 - M_Restraint)^2/(SD_Restraint^2*(SampleN-1))))
        SE_Restraint_2<- McSweeny_SE_Restraint*sqrt(1 + (1/SampleN) + ((Score_Restraint_2 - M_Restraint)^2/(SD_Restraint^2*(SampleN-1))))
        SE_Restraint_3<- McSweeny_SE_Restraint*sqrt(1 + (1/SampleN) + ((Score_Restraint_3 - M_Restraint)^2/(SD_Restraint^2*(SampleN-1))))
        SE_Restraint<- c(SE_Restraint_1, SE_Restraint_2, SE_Restraint_3)
        SE_Eating_Concern_1<- McSweeny_SE_Eating_Concern*sqrt(1 + (1/SampleN) + ((Score_Eating_Concern_1 - M_Eating_Concern)^2/(SD_Eating_Concern^2*(SampleN-1))))
        SE_Eating_Concern_2<- McSweeny_SE_Eating_Concern*sqrt(1 + (1/SampleN) + ((Score_Eating_Concern_2 - M_Eating_Concern)^2/(SD_Eating_Concern^2*(SampleN-1))))
        SE_Eating_Concern_3<- McSweeny_SE_Eating_Concern*sqrt(1 + (1/SampleN) + ((Score_Eating_Concern_3 - M_Eating_Concern)^2/(SD_Eating_Concern^2*(SampleN-1))))
        SE_Eating_Concern<- c(SE_Eating_Concern_1, SE_Eating_Concern_2, SE_Eating_Concern_3)
        SE_Shape_Concern_1<- McSweeny_SE_Shape_Concern*sqrt(1 + (1/SampleN) + ((Score_Shape_Concern_1 - M_Shape_Concern)^2/(SD_Shape_Concern^2*(SampleN-1))))
        SE_Shape_Concern_2<- McSweeny_SE_Shape_Concern*sqrt(1 + (1/SampleN) + ((Score_Shape_Concern_2 - M_Shape_Concern)^2/(SD_Shape_Concern^2*(SampleN-1))))
        SE_Shape_Concern_3<- McSweeny_SE_Shape_Concern*sqrt(1 + (1/SampleN) + ((Score_Shape_Concern_3 - M_Shape_Concern)^2/(SD_Shape_Concern^2*(SampleN-1))))
        SE_Shape_Concern<- c(SE_Shape_Concern_1, SE_Shape_Concern_2, SE_Shape_Concern_3)
        SE_Weight_Concern_1<- McSweeny_SE_Weight_Concern*sqrt(1 + (1/SampleN) + ((Score_Weight_Concern_1 - M_Weight_Concern)^2/(SD_Weight_Concern^2*(SampleN-1))))
        SE_Weight_Concern_2<- McSweeny_SE_Weight_Concern*sqrt(1 + (1/SampleN) + ((Score_Weight_Concern_2 - M_Weight_Concern)^2/(SD_Weight_Concern^2*(SampleN-1))))
        SE_Weight_Concern_3<- McSweeny_SE_Weight_Concern*sqrt(1 + (1/SampleN) + ((Score_Weight_Concern_3 - M_Weight_Concern)^2/(SD_Weight_Concern^2*(SampleN-1))))
        SE_Weight_Concern<- c(SE_Weight_Concern_1, SE_Weight_Concern_2, SE_Weight_Concern_3)
        SE<- round(SE, digits = 2)
        SE_Restraint<- round(SE_Restraint, digits = 2)
        SE_Eating_Concern<- round(SE_Eating_Concern, digits = 2)
        SE_Shape_Concern<- round(SE_Shape_Concern, digits = 2)
        SE_Weight_Concern<- round(SE_Weight_Concern, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2), (Conf*SE3))
        CI<- round(CI, digits = 2)
        CI_Restraint<- c((Conf*SE_Restraint_1), (Conf*SE_Restraint_2), (Conf*SE_Restraint_3))
        CI_Restraint<- round(CI_Restraint, digits = 2)
        CI_Eating_Concern<- c((Conf*SE_Eating_Concern_1), (Conf*SE_Eating_Concern_2), (Conf*SE_Eating_Concern_3))
        CI_Eating_Concern<- round(CI_Eating_Concern, digits = 2)
        CI_Shape_Concern<- c((Conf*SE_Shape_Concern_1), (Conf*SE_Shape_Concern_2), (Conf*SE_Shape_Concern_3))
        CI_Shape_Concern<- round(CI_Shape_Concern, digits = 2)
        CI_Weight_Concern<- c((Conf*SE_Weight_Concern_1), (Conf*SE_Weight_Concern_2), (Conf*SE_Weight_Concern_3))
        CI_Weight_Concern<- round(CI_Weight_Concern, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Restraint<- c((Conf*SE_Restraint), (Conf*SE_Restraint), (Conf*SE_Restraint))
        CI_Restraint<- round(CI_Restraint, digits = 2)
        CI_Eating_Concern<- c((Conf*SE_Eating_Concern), (Conf*SE_Eating_Concern), (Conf*SE_Eating_Concern))
        CI_Eating_Concern<- round(CI_Eating_Concern, digits = 2)
        CI_Shape_Concern<- c((Conf*SE_Shape_Concern), (Conf*SE_Shape_Concern), (Conf*SE_Shape_Concern))
        CI_Shape_Concern<- round(CI_Shape_Concern, digits = 2)
        CI_Weight_Concern<- c((Conf*SE_Weight_Concern), (Conf*SE_Weight_Concern), (Conf*SE_Weight_Concern))
        CI_Weight_Concern<- round(CI_Weight_Concern, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Restraint<- PTS_Restraint + CI_Restraint
      CI_Upper_Lim_Restraint<- round(CI_Upper_Lim_Restraint, digits = 2)
      CI_Lower_Lim_Restraint<-PTS_Restraint - CI_Restraint
      CI_Lower_Lim_Restraint<- round(CI_Lower_Lim_Restraint, digits = 2)
      CI_Upper_Lim_Eating_Concern<- PTS_Eating_Concern + CI_Eating_Concern
      CI_Upper_Lim_Eating_Concern<- round(CI_Upper_Lim_Eating_Concern, digits = 2)
      CI_Lower_Lim_Eating_Concern<-PTS_Eating_Concern - CI_Eating_Concern
      CI_Lower_Lim_Eating_Concern<- round(CI_Lower_Lim_Eating_Concern, digits = 2)
      CI_Upper_Lim_Shape_Concern<- PTS_Shape_Concern + CI_Shape_Concern
      CI_Upper_Lim_Shape_Concern<- round(CI_Upper_Lim_Shape_Concern, digits = 2)
      CI_Lower_Lim_Shape_Concern<-PTS_Shape_Concern - CI_Shape_Concern
      CI_Lower_Lim_Shape_Concern<- round(CI_Lower_Lim_Shape_Concern, digits = 2)
      CI_Upper_Lim_Weight_Concern<- PTS_Weight_Concern + CI_Weight_Concern
      CI_Upper_Lim_Weight_Concern<- round(CI_Upper_Lim_Weight_Concern, digits = 2)
      CI_Lower_Lim_Weight_Concern<-PTS_Weight_Concern - CI_Weight_Concern
      CI_Lower_Lim_Weight_Concern<- round(CI_Lower_Lim_Weight_Concern, digits = 2)
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Restraint == "2") {
        CI_Restraint<- input$Man_CI_Restraint
        CI_Restraint<- c(CI_Restraint, CI_Restraint, CI_Restraint)
        CI_Restraint<- round(CI_Restraint, digits = 2)
        CI_Upper_Lim_Restraint<- Score_Restraint + CI_Restraint
        CI_Upper_Lim_Restraint<- round(CI_Upper_Lim_Restraint, digits = 2)
        CI_Lower_Lim_Restraint<- Score_Restraint - CI_Restraint
        CI_Lower_Lim_Restraint<- round(CI_Lower_Lim_Restraint, digits = 2)
      }
      if(input$Select_CI_Eating_Concern == "2") {
        CI_Eating_Concern<- input$Man_CI_Eating_Concern
        CI_Eating_Concern<- c(CI_Eating_Concern, CI_Eating_Concern, CI_Eating_Concern)
        CI_Eating_Concern<- round(CI_Eating_Concern, digits = 2)
        CI_Upper_Lim_Eating_Concern<- Score_Eating_Concern + CI_Eating_Concern
        CI_Upper_Lim_Eating_Concern<- round(CI_Upper_Lim_Eating_Concern, digits = 2)
        CI_Lower_Lim_Eating_Concern<- Score_Eating_Concern - CI_Eating_Concern
        CI_Lower_Lim_Eating_Concern<- round(CI_Lower_Lim_Eating_Concern, digits = 2)
      }
      if(input$Select_CI_Shape_Concern == "2") {
        CI_Shape_Concern<- input$Man_CI_Shape_Concern
        CI_Shape_Concern<- c(CI_Shape_Concern, CI_Shape_Concern, CI_Shape_Concern)
        CI_Shape_Concern<- round(CI_Shape_Concern, digits = 2)
        CI_Upper_Lim_Shape_Concern<- Score_Shape_Concern + CI_Shape_Concern
        CI_Upper_Lim_Shape_Concern<- round(CI_Upper_Lim_Shape_Concern, digits = 2)
        CI_Lower_Lim_Shape_Concern<- Score_Shape_Concern - CI_Shape_Concern
        CI_Lower_Lim_Shape_Concern<- round(CI_Lower_Lim_Shape_Concern, digits = 2)
      }
      if(input$Select_CI_Weight_Concern == "2") {
        CI_Weight_Concern<- input$Man_CI_Weight_Concern
        CI_Weight_Concern<- c(CI_Weight_Concern, CI_Weight_Concern, CI_Weight_Concern)
        CI_Weight_Concern<- round(CI_Weight_Concern, digits = 2)
        CI_Upper_Lim_Weight_Concern<- Score_Weight_Concern + CI_Weight_Concern
        CI_Upper_Lim_Weight_Concern<- round(CI_Upper_Lim_Weight_Concern, digits = 2)
        CI_Lower_Lim_Weight_Concern<- Score_Weight_Concern - CI_Weight_Concern
        CI_Lower_Lim_Weight_Concern<- round(CI_Lower_Lim_Weight_Concern, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_4<- round(input$Cutoff_4, digits = 2)
      Cutoff_Score_5<- round(input$Cutoff_5, digits = 2)
      Cutoff_Score_Restraint_1<- round(input$Cutoff_Restraint_1, digits = 2)
      Cutoff_Score_Restraint_2<- round(input$Cutoff_Restraint_2, digits = 2)
      Cutoff_Score_Restraint_3<- round(input$Cutoff_Restraint_3, digits = 2)
      Cutoff_Score_Restraint_4<- round(input$Cutoff_Restraint_4, digits = 2)
      Cutoff_Score_Restraint_5<- round(input$Cutoff_Restraint_5, digits = 2)
      Cutoff_Score_Eating_Concern_1<- round(input$Cutoff_Eating_Concern_1, digits = 2)
      Cutoff_Score_Eating_Concern_2<- round(input$Cutoff_Eating_Concern_2, digits = 2)
      Cutoff_Score_Eating_Concern_3<- round(input$Cutoff_Eating_Concern_3, digits = 2)
      Cutoff_Score_Eating_Concern_4<- round(input$Cutoff_Eating_Concern_4, digits = 2)
      Cutoff_Score_Eating_Concern_5<- round(input$Cutoff_Eating_Concern_5, digits = 2)
      Cutoff_Score_Shape_Concern_1<- round(input$Cutoff_Shape_Concern_1, digits = 2)
      Cutoff_Score_Shape_Concern_2<- round(input$Cutoff_Shape_Concern_2, digits = 2)
      Cutoff_Score_Shape_Concern_3<- round(input$Cutoff_Shape_Concern_3, digits = 2)
      Cutoff_Score_Shape_Concern_4<- round(input$Cutoff_Shape_Concern_4, digits = 2)
      Cutoff_Score_Shape_Concern_5<- round(input$Cutoff_Shape_Concern_5, digits = 2)
      Cutoff_Score_Weight_Concern_1<- round(input$Cutoff_Weight_Concern_1, digits = 2)
      Cutoff_Score_Weight_Concern_2<- round(input$Cutoff_Weight_Concern_2, digits = 2)
      Cutoff_Score_Weight_Concern_3<- round(input$Cutoff_Weight_Concern_3, digits = 2)
      Cutoff_Score_Weight_Concern_4<- round(input$Cutoff_Weight_Concern_4, digits = 2)
      Cutoff_Score_Weight_Concern_5<- round(input$Cutoff_Weight_Concern_5, digits = 2)
      Cutoff_Score_1<- rep(Cutoff_Score_1, 3)
      Cutoff_Score_2<- rep(Cutoff_Score_2, 3)
      Cutoff_Score_3<- rep(Cutoff_Score_3, 3)
      Cutoff_Score_4<- rep(Cutoff_Score_4, 3)
      Cutoff_Score_5<- rep(Cutoff_Score_5, 3)
      Cutoff_Score_Restraint_1<- rep(Cutoff_Score_Restraint_1, 3)
      Cutoff_Score_Restraint_2<- rep(Cutoff_Score_Restraint_2, 3)
      Cutoff_Score_Restraint_3<- rep(Cutoff_Score_Restraint_3, 3)
      Cutoff_Score_Restraint_4<- rep(Cutoff_Score_Restraint_4, 3)
      Cutoff_Score_Restraint_5<- rep(Cutoff_Score_Restraint_5, 3)
      Cutoff_Score_Eating_Concern_1<- rep(Cutoff_Score_Eating_Concern_1, 3)
      Cutoff_Score_Eating_Concern_2<- rep(Cutoff_Score_Eating_Concern_2, 3)
      Cutoff_Score_Eating_Concern_3<- rep(Cutoff_Score_Eating_Concern_3, 3)
      Cutoff_Score_Eating_Concern_4<- rep(Cutoff_Score_Eating_Concern_4, 3)
      Cutoff_Score_Eating_Concern_5<- rep(Cutoff_Score_Eating_Concern_5, 3)
      Cutoff_Score_Shape_Concern_1<- rep(Cutoff_Score_Shape_Concern_1, 3)
      Cutoff_Score_Shape_Concern_2<- rep(Cutoff_Score_Shape_Concern_2, 3)
      Cutoff_Score_Shape_Concern_3<- rep(Cutoff_Score_Shape_Concern_3, 3)
      Cutoff_Score_Shape_Concern_4<- rep(Cutoff_Score_Shape_Concern_4, 3)
      Cutoff_Score_Shape_Concern_5<- rep(Cutoff_Score_Shape_Concern_5, 3)
      Cutoff_Score_Weight_Concern_1<- rep(Cutoff_Score_Weight_Concern_1, 3)
      Cutoff_Score_Weight_Concern_2<- rep(Cutoff_Score_Weight_Concern_2, 3)
      Cutoff_Score_Weight_Concern_3<- rep(Cutoff_Score_Weight_Concern_3, 3)
      Cutoff_Score_Weight_Concern_4<- rep(Cutoff_Score_Weight_Concern_4, 3)
      Cutoff_Score_Weight_Concern_5<- rep(Cutoff_Score_Weight_Concern_5, 3)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3, Cutoff_Score_4, Cutoff_Score_5,
                                      Score_Restraint,Change_Restraint,PTS_Restraint, SE_Restraint, CI_Upper_Lim_Restraint, CI_Lower_Lim_Restraint, Cutoff_Score_Restraint_1,Cutoff_Score_Restraint_2,Cutoff_Score_Restraint_3, Cutoff_Score_Restraint_4,Cutoff_Score_Restraint_5,
                                      Score_Eating_Concern,Change_Eating_Concern, PTS_Eating_Concern, SE_Eating_Concern, CI_Upper_Lim_Eating_Concern, CI_Lower_Lim_Eating_Concern, Cutoff_Score_Eating_Concern_1,Cutoff_Score_Eating_Concern_2,Cutoff_Score_Eating_Concern_3, Cutoff_Score_Eating_Concern_4,Cutoff_Score_Eating_Concern_5, 
                                      Score_Shape_Concern,Change_Shape_Concern,PTS_Shape_Concern, SE_Shape_Concern, CI_Upper_Lim_Shape_Concern, CI_Lower_Lim_Shape_Concern, Cutoff_Score_Shape_Concern_1,Cutoff_Score_Shape_Concern_2,Cutoff_Score_Shape_Concern_3, Cutoff_Score_Shape_Concern_4,Cutoff_Score_Shape_Concern_5, 
                                      Score_Weight_Concern,Change_Weight_Concern,PTS_Weight_Concern, SE_Weight_Concern, CI_Upper_Lim_Weight_Concern, CI_Lower_Lim_Weight_Concern, Cutoff_Score_Weight_Concern_1,Cutoff_Score_Weight_Concern_2,Cutoff_Score_Weight_Concern_3, Cutoff_Score_Weight_Concern_4,Cutoff_Score_Weight_Concern_5)
    }
    
  
    #Create dataframes to be displayed as tables in pdf report (statistical values used in reliable change analyses)
    
    if(input$RelChangeMethod == 'Nunnally & Bernstein (1994)') {
      Stats_Table_Fullscale<<- data.frame(Pop, M, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Restraint<<- data.frame(Pop, M_Restraint, SD_Restraint, RelChangeMethod, Rel_Restraint, ConfInt)
      names(Stats_Table_Restraint)<<- c("Reference Population", "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Eating_Concern<<- data.frame(Pop, M_Eating_Concern, SD_Eating_Concern, RelChangeMethod, Rel_Eating_Concern, ConfInt)
      names(Stats_Table_Eating_Concern)<<- c("Reference Population", "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Shape_Concern<<- data.frame(Pop, M_Shape_Concern, SD_Shape_Concern, RelChangeMethod, Rel_Shape_Concern, ConfInt)
      names(Stats_Table_Shape_Concern)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Weight_Concern<<- data.frame(Pop, M_Weight_Concern, SD_Weight_Concern, RelChangeMethod, Rel_Weight_Concern, ConfInt)
      names(Stats_Table_Weight_Concern)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      } else if (input$RelChangeMethod == 'Chelune et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop, M, M_Retest, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Restraint<<- data.frame(Pop, M_Restraint, M_Retest_Restraint, SD_Restraint, RelChangeMethod, Rel_Restraint, ConfInt)
      names(Stats_Table_Restraint)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Eating_Concern<<- data.frame(Pop, M_Eating_Concern, M_Retest_Eating_Concern, SD_Eating_Concern, RelChangeMethod, Rel_Eating_Concern, ConfInt)
      names(Stats_Table_Eating_Concern)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Shape_Concern<<- data.frame(Pop, M_Shape_Concern, M_Retest_Shape_Concern, SD_Shape_Concern, RelChangeMethod, Rel_Shape_Concern, ConfInt)
      names(Stats_Table_Shape_Concern)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Weight_Concern<<- data.frame(Pop, M_Weight_Concern, M_Retest_Weight_Concern, SD_Weight_Concern, RelChangeMethod, Rel_Weight_Concern, ConfInt)
      names(Stats_Table_Weight_Concern)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Maassen et al. (2006)' | input$RelChangeMethod == 'McSweeny et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop, M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Restraint<<- data.frame(Pop, M_Restraint, M_Retest_Restraint, SD_Restraint, SD_Retest_Restraint, RelChangeMethod, Rel_Restraint, ConfInt)
      names(Stats_Table_Restraint)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Eating_Concern<<- data.frame(Pop, M_Eating_Concern, M_Retest_Eating_Concern, SD_Eating_Concern, SD_Retest_Eating_Concern, RelChangeMethod, Rel_Eating_Concern, ConfInt)
      names(Stats_Table_Eating_Concern)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Shape_Concern<<- data.frame(Pop,  M_Shape_Concern, M_Retest_Shape_Concern, SD_Shape_Concern, SD_Retest_Shape_Concern, RelChangeMethod, Rel_Shape_Concern, ConfInt)
      names(Stats_Table_Shape_Concern)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Weight_Concern<<- data.frame(Pop, M_Weight_Concern, M_Retest_Weight_Concern, SD_Weight_Concern, SD_Retest_Weight_Concern, RelChangeMethod, Rel_Weight_Concern, ConfInt)
      names(Stats_Table_Weight_Concern)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Crawford & Howell (1998)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, SampleN, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Restraint<<- data.frame(Pop, M_Restraint, M_Retest_Restraint, SD_Restraint, SD_Retest_Restraint, RelChangeMethod, Rel_Restraint, SampleN,ConfInt)
      names(Stats_Table_Restraint)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Eating_Concern<<- data.frame(Pop, M_Eating_Concern, M_Retest_Eating_Concern, SD_Eating_Concern, SD_Retest_Eating_Concern, RelChangeMethod, Rel_Eating_Concern, SampleN, ConfInt)
      names(Stats_Table_Eating_Concern)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Shape_Concern<<- data.frame(Pop, M_Shape_Concern, M_Retest_Shape_Concern, SD_Shape_Concern, SD_Retest_Shape_Concern, RelChangeMethod, Rel_Shape_Concern, SampleN,ConfInt)
      names(Stats_Table_Shape_Concern)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Weight_Concern<<- data.frame(Pop, M_Weight_Concern, M_Retest_Weight_Concern, SD_Weight_Concern, SD_Retest_Weight_Concern, RelChangeMethod, Rel_Weight_Concern, SampleN,ConfInt)
      names(Stats_Table_Weight_Concern)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)","Reliable Change Method", "Reliability", "N", "Confidence")
    } else if (input$RelChangeMethod == 'Jacobson & Truax (1991)' | input$RelChangeMethod == 'Speer (1992)') {
      Stats_Table_Fullscale<<- data.frame(Pop, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Restraint<<- data.frame(Pop, SD_Restraint, RelChangeMethod, Rel_Restraint, ConfInt)
      names(Stats_Table_Restraint)<<- c("Reference Population", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Eating_Concern<<- data.frame(Pop, SD_Eating_Concern, RelChangeMethod, Rel_Eating_Concern, ConfInt)
      names(Stats_Table_Eating_Concern)<<- c("Reference Population", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Shape_Concern<<- data.frame(Pop, SD_Shape_Concern, RelChangeMethod, Rel_Shape_Concern, ConfInt)
      names(Stats_Table_Shape_Concern)<<- c("Reference Population", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Weight_Concern<<- data.frame(Pop, SD_Weight_Concern, RelChangeMethod, Rel_Weight_Concern, ConfInt)
      names(Stats_Table_Weight_Concern)<<- c("Reference Population", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    }
    
    #If custom confidence intervals options are selected, make sure that no values appear for PS and SE in pdf tables
    #Change the look of tables in report if custom confidence intervals options are selected
    
    if (input$Select_CI == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS = NA, SE = NA)
      Stats_Table_Fullscale<<- Stats_Table_Fullscale %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI[1])
    }
    if (input$Select_CI_Restraint == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Restraint = NA, SE_Restraint = NA)
      Stats_Table_Restraint<<- Stats_Table_Restraint %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Restraint[1])
    }
    if (input$Select_CI_Eating_Concern == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Eating_Concern = NA, SE_Eating_Concern = NA)
      Stats_Table_Eating_Concern<<- Stats_Table_Eating_Concern %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                      "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Eating_Concern[1])
    }
    if (input$Select_CI_Shape_Concern == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Shape_Concern = NA, SE_Shape_Concern = NA)
      Stats_Table_Shape_Concern<<- Stats_Table_Shape_Concern %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                      "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Shape_Concern[1])
    }
    if (input$Select_CI_Weight_Concern == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Weight_Concern = NA, SE_Weight_Concern = NA)
      Stats_Table_Weight_Concern<<- Stats_Table_Weight_Concern %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Weight_Concern[1])
    }
    
    #Create a dataframe to be used in the analytics spreadsheet.
    
    #Calculate variables for significant improvement
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$CI_Lower_Lim[1]) {
      EDEQ.Fullscale.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$CI_Lower_Lim[1]) {
      EDEQ.Fullscale.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$CI_Upper_Lim[1]) {
      EDEQ.Fullscale.Sig.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$CI_Upper_Lim[1]) {
      EDEQ.Fullscale.Sig.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Restraint[length(Entered_Scores_Df$Score_Restraint)] < Entered_Scores_Df$CI_Lower_Lim_Restraint[1]) {
      EDEQ.Restraint.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Restraint[length(Entered_Scores_Df$Score_Restraint)] >= Entered_Scores_Df$CI_Lower_Lim_Restraint[1]) {
      EDEQ.Restraint.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Restraint[length(Entered_Scores_Df$Score_Restraint)] > Entered_Scores_Df$CI_Upper_Lim_Restraint[1]) {
      EDEQ.Restraint.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Restraint[length(Entered_Scores_Df$Score_Restraint)] <= Entered_Scores_Df$CI_Upper_Lim_Restraint[1]) {
      EDEQ.Restraint.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Eating_Concern[length(Entered_Scores_Df$Score_Eating_Concern)] < Entered_Scores_Df$CI_Lower_Lim_Eating_Concern[1]) {
      EDEQ.Eating.Concern.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Eating_Concern[length(Entered_Scores_Df$Score_Eating_Concern)] >= Entered_Scores_Df$CI_Lower_Lim_Eating_Concern[1]) {
      EDEQ.Eating.Concern.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Eating_Concern[length(Entered_Scores_Df$Score_Eating_Concern)] > Entered_Scores_Df$CI_Upper_Lim_Eating_Concern[1]) {
      EDEQ.Eating.Concern.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Eating_Concern[length(Entered_Scores_Df$Score_Eating_Concern)] <= Entered_Scores_Df$CI_Upper_Lim_Eating_Concern[1]) {
      EDEQ.Eating.Concern.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Shape_Concern[length(Entered_Scores_Df$Score_Shape_Concern)] < Entered_Scores_Df$CI_Lower_Lim_Shape_Concern[1]) {
      EDEQ.Shape.Concern.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Shape_Concern[length(Entered_Scores_Df$Score_Shape_Concern)] >= Entered_Scores_Df$CI_Lower_Lim_Shape_Concern[1]) {
      EDEQ.Shape.Concern.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Shape_Concern[length(Entered_Scores_Df$Score_Shape_Concern)] > Entered_Scores_Df$CI_Upper_Lim_Shape_Concern[1]) {
      EDEQ.Shape.Concern.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Shape_Concern[length(Entered_Scores_Df$Score_Shape_Concern)] <= Entered_Scores_Df$CI_Upper_Lim_Shape_Concern[1]) {
      EDEQ.Shape.Concern.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Weight_Concern[length(Entered_Scores_Df$Score_Weight_Concern)] < Entered_Scores_Df$CI_Lower_Lim_Weight_Concern[1]) {
      EDEQ.Weight.Concern.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Weight_Concern[length(Entered_Scores_Df$Score_Weight_Concern)] >= Entered_Scores_Df$CI_Lower_Lim_Weight_Concern[1]) {
      EDEQ.Weight.Concern.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Weight_Concern[length(Entered_Scores_Df$Score_Weight_Concern)] > Entered_Scores_Df$CI_Upper_Lim_Weight_Concern[1]) {
      EDEQ.Weight.Concern.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Weight_Concern[length(Entered_Scores_Df$Score_Weight_Concern)] <= Entered_Scores_Df$CI_Upper_Lim_Weight_Concern[1]) {
      EDEQ.Weight.Concern.Sig.Deterioration<- "No"
    }
    
    
    #Calculate variables for improvement
    
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$Score[1]) {
      EDEQ.Fullscale.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$Score[1]) {
      EDEQ.Fullscale.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$Score[1]) {
      EDEQ.Fullscale.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$Score[1]) {
      EDEQ.Fullscale.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Restraint[length(Entered_Scores_Df$Score_Restraint)] < Entered_Scores_Df$Score_Restraint[1]) {
      EDEQ.Restraint.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Restraint[length(Entered_Scores_Df$Score_Restraint)] >= Entered_Scores_Df$Score_Restraint[1]) {
      EDEQ.Restraint.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Restraint[length(Entered_Scores_Df$Score_Restraint)] > Entered_Scores_Df$Score_Restraint[1]) {
      EDEQ.Restraint.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Restraint[length(Entered_Scores_Df$Score_Restraint)] <= Entered_Scores_Df$Score_Restraint[1]) {
      EDEQ.Restraint.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Eating_Concern[length(Entered_Scores_Df$Score_Eating_Concern)] < Entered_Scores_Df$Score_Eating_Concern[1]) {
      EDEQ.Eating.Concern.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Eating_Concern[length(Entered_Scores_Df$Score_Eating_Concern)] >= Entered_Scores_Df$Score_Eating_Concern[1]) {
      EDEQ.Eating.Concern.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Eating_Concern[length(Entered_Scores_Df$Score_Eating_Concern)] > Entered_Scores_Df$Score_Eating_Concern[1]) {
      EDEQ.Eating.Concern.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Eating_Concern[length(Entered_Scores_Df$Score_Eating_Concern)] <= Entered_Scores_Df$Score_Eating_Concern[1]) {
      EDEQ.Eating.Concern.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Shape_Concern[length(Entered_Scores_Df$Score_Shape_Concern)] < Entered_Scores_Df$Score_Shape_Concern[1]) {
      EDEQ.Shape.Concern.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Shape_Concern[length(Entered_Scores_Df$Score_Shape_Concern)] >= Entered_Scores_Df$Score_Shape_Concern[1]) {
      EDEQ.Shape.Concern.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Shape_Concern[length(Entered_Scores_Df$Score_Shape_Concern)] > Entered_Scores_Df$Score_Shape_Concern[1]) {
      EDEQ.Shape.Concern.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Shape_Concern[length(Entered_Scores_Df$Score_Shape_Concern)] <= Entered_Scores_Df$Score_Shape_Concern[1]) {
      EDEQ.Shape.Concern.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Weight_Concern[length(Entered_Scores_Df$Score_Weight_Concern)] < Entered_Scores_Df$Score_Weight_Concern[1]) {
      EDEQ.Weight.Concern.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Weight_Concern[length(Entered_Scores_Df$Score_Weight_Concern)] >= Entered_Scores_Df$Score_Weight_Concern[1]) {
      EDEQ.Weight.Concern.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Weight_Concern[length(Entered_Scores_Df$Score_Weight_Concern)] > Entered_Scores_Df$Score_Weight_Concern[1]) {
      EDEQ.Weight.Concern.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Weight_Concern[length(Entered_Scores_Df$Score_Weight_Concern)] <= Entered_Scores_Df$Score_Weight_Concern[1]) {
      EDEQ.Weight.Concern.Deterioration<- "No"
    }
    
    
    
    EDEQ.Fullscale.Change<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] - Entered_Scores_Df$Score[1]
    EDEQ.Restraint.Change<- Entered_Scores_Df$Score_Restraint[length(Entered_Scores_Df$Score_Restraint)] - Entered_Scores_Df$Score_Restraint[1]
    EDEQ.Eating.Concern.Change<- Entered_Scores_Df$Score_Eating_Concern[length(Entered_Scores_Df$Score_Eating_Concern)] - Entered_Scores_Df$Score_Eating_Concern[1]
    EDEQ.Shape.Concern.Change<- Entered_Scores_Df$Score_Shape_Concern[length(Entered_Scores_Df$Score_Shape_Concern)] - Entered_Scores_Df$Score_Shape_Concern[1]
    EDEQ.Weight.Concern.Change<- Entered_Scores_Df$Score_Weight_Concern[length(Entered_Scores_Df$Score_Weight_Concern)] - Entered_Scores_Df$Score_Weight_Concern[1]
    EDEQ.Fullscale.Comparisons<- length(Entered_Scores_Df$Change) - 1
    EDEQ.Restraint.Comparisons<- length(Entered_Scores_Df$Change_Restraint) - 1
    EDEQ.Eating.Concern.Comparisons<- length(Entered_Scores_Df$Change_Eating_Concern) - 1
    EDEQ.Shape.Concern.Comparisons<- length(Entered_Scores_Df$Change_Shape_Concern) - 1
    EDEQ.Weight.Concern.Comparisons<- length(Entered_Scores_Df$Change_Weight_Concern) - 1
    EDEQ.Fullscale.First.Date<- Entered_Scores_Df$Date[1]
    EDEQ.Restraint.First.Date<- Entered_Scores_Df$Date[1]
    EDEQ.Eating.Concern.First.Date<- Entered_Scores_Df$Date[1]
    EDEQ.Shape.Concern.First.Date<- Entered_Scores_Df$Date[1]
    EDEQ.Weight.Concern.First.Date<- Entered_Scores_Df$Date[1]
    EDEQ.Fullscale.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    EDEQ.Restraint.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    EDEQ.Eating.Concern.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    EDEQ.Shape.Concern.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    EDEQ.Weight.Concern.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    EDEQ.Fullscale.First.Score<- Entered_Scores_Df$Score[1]
    EDEQ.Restraint.First.Score<- Entered_Scores_Df$Score_Restraint[1]
    EDEQ.Eating.Concern.First.Score<- Entered_Scores_Df$Score_Eating_Concern[1]
    EDEQ.Shape.Concern.First.Score<- Entered_Scores_Df$Score_Shape_Concern[1]
    EDEQ.Weight.Concern.First.Score<- Entered_Scores_Df$Score_Weight_Concern[1]
    EDEQ.Fullscale.Last.Score<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)]
    EDEQ.Restraint.Last.Score<- Entered_Scores_Df$Score_Restraint[length(Entered_Scores_Df$Score_Restraint)]
    EDEQ.Eating.Concern.Last.Score<- Entered_Scores_Df$Score_Eating_Concern[length(Entered_Scores_Df$Score_Eating_Concern)]
    EDEQ.Shape.Concern.Last.Score<- Entered_Scores_Df$Score_Shape_Concern[length(Entered_Scores_Df$Score_Shape_Concern)]
    EDEQ.Weight.Concern.Last.Score<- Entered_Scores_Df$Score_Weight_Concern[length(Entered_Scores_Df$Score_Weight_Concern)]
    
    
    Analytics_Df<<- data.frame(EDEQ.Fullscale.First.Date, EDEQ.Fullscale.First.Score, EDEQ.Fullscale.Comparisons, EDEQ.Fullscale.Change, EDEQ.Fullscale.Last.Date, EDEQ.Fullscale.Last.Score, EDEQ.Fullscale.Improvement,EDEQ.Fullscale.Sig.Improvement, EDEQ.Fullscale.Deterioration, EDEQ.Fullscale.Sig.Deterioration,
                               EDEQ.Restraint.First.Date, EDEQ.Restraint.First.Score, EDEQ.Restraint.Comparisons, EDEQ.Restraint.Change, EDEQ.Restraint.Last.Date, EDEQ.Restraint.Last.Score, EDEQ.Restraint.Improvement, EDEQ.Restraint.Sig.Improvement, EDEQ.Restraint.Deterioration, EDEQ.Restraint.Sig.Deterioration,
                               EDEQ.Eating.Concern.First.Date, EDEQ.Eating.Concern.First.Score, EDEQ.Eating.Concern.Comparisons, EDEQ.Eating.Concern.Change, EDEQ.Eating.Concern.Last.Date, EDEQ.Eating.Concern.Last.Score, EDEQ.Eating.Concern.Improvement, EDEQ.Eating.Concern.Sig.Improvement, EDEQ.Eating.Concern.Deterioration, EDEQ.Eating.Concern.Sig.Deterioration, 
                               EDEQ.Shape.Concern.First.Date, EDEQ.Shape.Concern.First.Score, EDEQ.Shape.Concern.Comparisons, EDEQ.Shape.Concern.Change, EDEQ.Shape.Concern.Last.Date, EDEQ.Shape.Concern.Last.Score, EDEQ.Shape.Concern.Improvement, EDEQ.Shape.Concern.Sig.Improvement, EDEQ.Shape.Concern.Deterioration, EDEQ.Shape.Concern.Sig.Deterioration, 
                               EDEQ.Weight.Concern.First.Date, EDEQ.Weight.Concern.First.Score, EDEQ.Weight.Concern.Comparisons, EDEQ.Weight.Concern.Change, EDEQ.Weight.Concern.Last.Date, EDEQ.Weight.Concern.Last.Score, EDEQ.Weight.Concern.Improvement, EDEQ.Weight.Concern.Sig.Improvement, EDEQ.Weight.Concern.Deterioration, EDEQ.Weight.Concern.Sig.Deterioration)
    
    
    
  })
  
  
  
  #Create an expression to activate the entered data & create notifications to indicate inaccurate data entry
  
  Entered_Scores_Notifications<- observeEvent(input$Action_Submit_Data, {
    
    Entered_Scores_Reac()
    
    if(length(Score_1a) < 33) {
      showNotification("The EDE-Q has 28 scale items and 5 items requiring other basic information. You have entered fewer responses than items for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(length(Score_1a) > 33) {
      showNotification("The EDE-Q has 28 scale items and 5 items requiring other basic information. You have entered fewer responses than items for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(input$Timepoint != "1") {
      if(length(Score_2a) < 33) {
        showNotification("The EDE-Q has 28 scale items and 5 items requiring other basic information. You have entered fewer responses than items for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_2a) > 33) {
        showNotification("The EDE-Q has 28 scale items and 5 items requiring other basic information. You have entered fewer responses than items for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
    if(input$Timepoint == "3") {
      if(length(Score_3a) < 33) {
        showNotification("The EDE-Q has 28 scale items and 5 items requiring other basic information. You have entered fewer responses than items for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_3a) > 33) {
        showNotification("The EDE-Q has 28 scale items and 5 items requiring other basic information. You have entered fewer responses than items for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
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
    
    Gap_Restraint<- Entered_Scores_Df[1,13] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),13]
    Entered_Scores_Df[1,14]<- Gap_Restraint
    
    Gap_Eating_Concern<- Entered_Scores_Df[1,24] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),24]
    Entered_Scores_Df[1,25]<- Gap_Eating_Concern
    
    Gap_Shape_Concern<- Entered_Scores_Df[1,35] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),35]
    Entered_Scores_Df[1,36]<- Gap_Shape_Concern
    
    Gap_Weight_Concern<- Entered_Scores_Df[1,46] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),46]
    Entered_Scores_Df[1,47]<- Gap_Weight_Concern
    
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
    
    filename = paste0(" EDE-Q Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
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
        Binge_Info = Binge_Info, 
        Basic_Info = Basic_Info,
        Entered_Scores_Df = Entered_Scores_Df,
        Stats_Table_Fullscale = Stats_Table_Fullscale,
        Stats_Table_Restraint = Stats_Table_Restraint,
        Stats_Table_Eating_Concern = Stats_Table_Eating_Concern,
        Stats_Table_Shape_Concern = Stats_Table_Shape_Concern,
        Stats_Table_Weight_Concern = Stats_Table_Weight_Concern,
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
      paste(paste0(" EDE-Q Data ", format(Sys.time(), '%d/%m/%y')), input$Output_Filetype1, sep = ".")
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













