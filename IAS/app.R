
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
      menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "IAS"),
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
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| Illness Attitude Scales (IAS)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 580),
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
                "Ferguson, E., & Daniel, E. (1995). The illness attitudes scale (IAS): A psychometric evaluation on a non-clinical population. Personality and Individual Differences, 18(4), 463-469.", br(), br(), 
                "Hedman, E., Lekander, M., Ljótsson, B., Lindefors, N., Rück, C., Andersson, G., & Andersson, E. (2015). Optimal cut-off points on the health anxiety inventory, illness attitude scales and whiteley index to identify severe health anxiety. PLoS One, 10(4).", br(), br(), 
                "Hedman, E., Ljótsson, B., Andersson, E., Andersson, G., Lindefors, N., Rück, C., . . . Lekander, M. (2015). Psychometric properties of internet-administered measures of health anxiety: An investigation of the health anxiety inventory, the illness attitude scales, and the whiteley index. Journal of Anxiety Disorders, 31, 32-37.", br(), br(),  
                "Hiller, W., Rief, W., & Fichter, M. (2002). Dimensional and categorical approaches to hypochondriasis. Psychological Medicine, 32(4), 707-718.", br(), br(),  
                "Hofling, V., & Weck, F. (2013). Assessing bodily preoccupations is sufficient: Clinically effective screening for hypochondriasis. Journal of Psychosomatic Research, 75(6), 526-531.", br(), br(), 
                "Kellner, R., Abbott, P., Winslow, W. W., & Pathak, D. (1987). Fears, beliefs, and attitudes in DSM-III hypochondriasis. Journal of Nervous and Mental Disease.", br(), br(),  
                "Sirri, L., Grandi, S., & Fava, G. A. (2008). The illness attitude scales. A clinimetric index for assessing hypochondriacal fears and beliefs. Psychotherapy and Psychosomatics, 77(6), 337-350.", br(), br(), 
                "Weck, F., Bleichhardt, G., & Hiller, W. (2009). The factor structure of the illness attitude scales in a german population. International Journal of Behavioral Medicine, 16(2), 164-171.", br(), br(),  
                "Weck, F., Bleichhardt, G., & Hiller, W. (2010). Screening for hypochondriasis with the illness attitude scales. Journal of Personality Assessment, 92(3), 260-268." 
        ),
        
        
        
        tabItem(tabName = "IAS",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel("Scale",
                             wellPanel(style = "background-color: #ededed; color: black",
                                       fluidRow(
                                         column(width = 12, h3(tags$strong("Illness Attitude Scales")))
                                       ),
                                       hr(),
                                      fluidRow(style = "background-color: #ededed",
                                       column(width = 12, h4 (tags$strong("Please answer all questions which can be checked.")))
                                             ),
                                      fluidRow(style = "background-color: #ededed",
                                               column(width = 12, h4(tags$strong("Check one answer even if you cannot answer accurately.")))
                                      ),
                                      fluidRow(style = "background-color: #ededed",
                                               column(width = 12, h4(tags$strong("Answer the other few questions with a few words or sentences.")))
                                      ),
                                      fluidRow(style = "background-color: #ededed",
                                               column(width = 12, h4(tags$strong("Do not think long before answering.")))
                                      ),
                                      fluidRow(
                                        column(width = 12, h4(tags$strong("*Note to clinician:")), h4("If the patient has cancer, do not administer item 16. If the patient has heart disease, do not administer item 17. 
                                                              In either case, replace the missing value with the mean of whichever scores are available for items 16, 17 and 18 (i.e. the Disease Phobia subscale)"))
                                      ),
                                       hr(),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_1", h4(tags$strong("1. Do you worry about your health?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                              )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_2", h4(tags$strong(tags$strong("2. Are you worried that you may get a serious illness in the future?"))), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_3", h4(tags$strong("3. Does the thought of a serious illness scare you?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_4", h4(tags$strong("4. If you have a pain, do you worry that it may be caused by a serious illness?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_5", h4(tags$strong("5. If a pain lasts for a week or more, do you see a physician?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_6", h4(tags$strong("6. If a pain lasts a week or more, do you believe that you have a serious illness?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_7", h4(tags$strong("7. Do you avoid habits which may be harmful to you such as smoking?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_8", h4(tags$strong("8.Do you avoid foods which may not be healthy?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_9", h4(tags$strong("9.Do you examine your body to find whether there is something wrong?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_10", h4(tags$strong("10. Do you believe that you have a physical disease but the doctors have not diagnosed it correctly?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_11", h4(tags$strong("11. When your doctor tells you that you have no physical disease to account for your symptoms, do you refuse to believe him/her?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_12", h4(tags$strong("12. When you have been told by a doctor what he/she found, do you soon begin to believe that you may have developed a new illness?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_13", h4(tags$strong("13. Are you afraid of news which reminds you of death (such as funerals, obituary notices)?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )), 
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_14", h4(tags$strong("14. Does the thought of death scare you?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_15", h4(tags$strong("15. Are you afraid that you may die soon?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_15a", h4(tags$strong("15a. Has your doctor told you that you have an illness now?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 4, textInput("Illness_Text", "If yes, what illness?"))
                                      ),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_16", h4(tags$strong("16. Are you afraid that you may have cancer?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_17", h4(tags$strong("17. Are you afraid that you may have heart disease?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_18", h4(tags$strong("18. Are you afraid that you may have some other serious illness?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 4, textInput("Afraid_Illness_Text", "Which illness?"))
                                      ),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_19", h4(tags$strong("19. When you read or hear about an illness, do you get symptoms similar to those of the illness?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_20", h4(tags$strong("20. When you notice a sensation in your body, do you find it difficult to think of something else?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_21", h4(tags$strong("21. When you feel a sensation in your body do you worry about it?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_22", h4(tags$strong("22. How often do you see a doctor?")), choices = c(
                                          "Almost never" = "0", "Only very rarely" = "1", "About 4 times a year" = "2", "About once a month" = "3", "About once a week" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_23", h4(tags$strong("23. How many different doctors, chiropractors or other healers have you seen in the past year?")), choices = c(
                                          "None" = "0", "1" = "1", "2 or 3" = "2", "4 or 5" = "3", "6 or more" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_24", h4(tags$strong("24. How often have you been treated during the past year? (For example, drugs, change of drugs, surgery, etc.)")), choices = c(
                                          "Not at all" = "0", "Once" = "1", "2 or 3 times" = "2", "4 or 5 times" = "3", "6 or more times" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 8, textInput("Treatment_Text", "If yes, what were the treatments? (use a comma to separate your answers)"))
                                      ),
                                      fluidRow(
                                        column(width = 12, h4(tags$strong("The next three questions concern your bodily symptoms (for example, pain, aches, pressure in your body, breathing difficulties, tiredness, etc.).")))
                                      ), 
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_25", h4(tags$strong("25. Do your bodily symptoms stop you from working?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_26", h4(tags$strong("26. Do your bodily symptoms stop you from concentrating on what you are doing?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      fluidRow(
                                        column(width = 12, radioButtons("Item_27", h4(tags$strong("27. Do your bodily symptoms stop you from enjoying yourself?")), choices = c(
                                          "No" = "0", "Rarely" = "1", "Sometimes" = "2", "Often" = "3", "Most of the time" = "4"), inline = TRUE, selected = character(0))
                                        )),
                                      hr(),
                                      fluidRow(
                                        column(width = 4, dateInput("Q_Date", "Date",  format = "dd/mm/yyyy")),
                                        column(width = 4, textInput("Q_Name", "Name")),
                                        column(width = 4, textInput("Q_Clin_Name", "Clinician's Name"))
                                      ),
                                      fluidRow(
                                        column(width = 12, h5("Scale Source: Kellner, R. (1986). Somatization and hypochondriasis. New York: Praeger Publishers."))
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
                                                      selectInput("Pop", "", choices = c("Hypochondriasis", "General Population", "Anxiety Disorder"))
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
                                                               selectInput("Select_CI", label = "IAS total scale",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI == '2'",
                                                                                numericInput("Man_CI", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Worry_Illness", label = "Worry about Illness",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Worry_Illness == '2'",
                                                                                numericInput("Man_CI_Worry_Illness", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Concerns_Pain", label = "Concerns about Pain",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Concerns_Pain == '2'",
                                                                                numericInput("Man_CI_Concerns_Pain", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Health_Habits", label = "Health Habits",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Health_Habits == '2'",
                                                                                numericInput("Man_CI_Health_Habits", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 3,
                                                               selectInput("Select_CI_Hypochondriacal_Beliefs", label = "Hypochondriacal Beliefs",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Hypochondriacal_Beliefs == '2'",
                                                                                numericInput("Man_CI_Hypochondriacal_Beliefs", "Specify the width of the confidence interval", value = 0))
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               selectInput("Select_CI_Thanatophobia", label = "Thanatophobia",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Thanatophobia == '2'",
                                                                                numericInput("Man_CI_Thanatophobia", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Disease_Phobia", label = "Disease Phobia",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Disease_Phobia == '2'",
                                                                                numericInput("Man_CI_Disease_Phobia", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Bodily_Preoccupations", label = "Bodily Preoccupations",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Bodily_Preoccupations == '2'",
                                                                                numericInput("Man_CI_Bodily_Preoccupations", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Treatment_Experience", label = "Treatment Experience",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Treatment_Experience == '2'",
                                                                                numericInput("Man_CI_Treatment_Experience", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Effects_Symptoms", label = "Effects of Symptoms",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Effects_Symptoms == '2'",
                                                                                numericInput("Man_CI_Effects_Symptoms", "Specify the width of the confidence interval", value = 0))
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
                                                               uiOutput("Mean_Widg_Worry_Illness")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Concerns_Pain")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Health_Habits")
                                                        ),
                                                        column(width = 3,
                                                               uiOutput("Mean_Widg_Hypochondriacal_Beliefs")
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Thanatophobia")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Disease_Phobia")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Bodily_Preoccupations")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Treatment_Experience")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Effects_Symptoms")
                                                        )
                                                      ),
                                                      h5(tags$em("*Default mean values for the total scale will change according to the population you select.")),
                                                      h5(tags$em("*Default mean values for the IAS subscales are derived from a sample of university students and will not change according to the population you select.")),
                                                      
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Chelune et al. (1993)' || input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)' || input.RelChangeMethod == 'Speer (1992)'",
                                                                       
                                                                       h4(tags$strong("Enter a mean value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean", "IAS total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Worry_Illness", "Worry about Illness", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Concerns_Pain", "Concerns about Pain", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Health_Habits", "Health Habits", value = 0)
                                                                         ),
                                                                         column(width = 3,
                                                                                numericInput("Retest_Mean_Hypochondriacal_Beliefs", "Hypochondriacal Beliefs", value = 0)
                                                                         )
                                                                       ),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Thanatophobia", "Thanatophobia", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Disease_Phobia", "Disease Phobia", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Bodily_Preoccupations", "Bodily Preoccupations", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Treatment_Experience", "Treatment Experience", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Effects_Symptoms", "Effects of Symptoms", value = 0)
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
                                                               uiOutput("Sd_Widg_Worry_Illness")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Concerns_Pain")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Health_Habits")
                                                        ),
                                                        column(width = 3,
                                                               uiOutput("Sd_Widg_Hypochondriacal_Beliefs")
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Thanatophobia")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Disease_Phobia")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Bodily_Preoccupations")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Treatment_Experience")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Effects_Symptoms")
                                                        )
                                                      ),
                                                      h5(tags$em("*Default standard deviation values for the total scale will change according to the population you select.")),
                                                      h5(tags$em("*Default standard deviation values for the IAS subscales are derived from a sample of university students and will not change according to the population you select.")),
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)'",
                                                                       
                                                                       h4(tags$strong("Enter a standard deviation value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd", "IAS total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Worry_Illness", "Worry about Illness", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Concerns_Pain", "Concerns about Pain", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Health_Habits", "Health Habits", value = 0)
                                                                         ),
                                                                         column(width = 3,
                                                                                numericInput("Retest_Sd_Hypochondriacal_Beliefs", "Hypochondriacal Beliefs", value = 0)
                                                                         )
                                                                       ),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Thanatophobia", "Thanatophobia", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Disease_Phobia", "Disease Phobia", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Bodily_Preoccupations", "Bodily Preoccupations", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Treatment_Experience", "Treatment Experience", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Effects_Symptoms", "Effects of Symptoms", value = 0)
                                                                         )
                                                                       )
                                                                       
                                                      )
                                             ),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      h4(tags$strong("Enter test-retest reliability values")), 
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability", "IAS total scale", value = .89),
                                                               h6("Hiller & Janca (2003)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Worry_Illness", "Worry about Illness", value = .92),
                                                               h6("Hiller & Rief (2004)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Concerns_Pain", "Concerns about Pain", value = .74),
                                                               h6("Hiller & Rief (2004)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Health_Habits", "Health Habits", value = .93),
                                                               h6("Hiller & Rief (2004)")
                                                        ),
                                                        column(width = 3,
                                                               numericInput("Reliability_Hypochondriacal_Beliefs", "Hypochondriacal Beliefs", value = .71),
                                                               h6("Hiller & Rief (2004)")
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability_Thanatophobia", "Thanatophobia", value = .90),
                                                               h6("Hiller & Rief (2004)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Disease_Phobia", "Disease Phobia", value = .84),
                                                               h6("Hiller & Rief (2004)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Bodily_Preoccupations", "Bodily Preoccupations", value = .93),
                                                               h6("Hiller & Rief (2004)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Treatment_Experience", "Treatment Experience", value = .91),
                                                               h6("Hiller & Rief (2004)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Effects_Symptoms", "Effects of Symptoms", value = .83),
                                                               h6("Hiller & Rief (2004)")
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
                                                               uiOutput("Cutoff_Widg_Worry_Illness_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Concerns_Pain_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Health_Habits_1") 
                                                        ),
                                                        column(width = 3,
                                                               uiOutput("Cutoff_Widg_Hypochondriacal_Beliefs_1") 
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Thanatophobia_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Disease_Phobia_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Bodily_Preoccupations_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Treatment_Experience_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Effects_Symptoms_1") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Second cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Worry_Illness_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Concerns_Pain_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Health_Habits_2") 
                                                        ),
                                                        column(width = 3,
                                                               uiOutput("Cutoff_Widg_Hypochondriacal_Beliefs_2") 
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Thanatophobia_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Disease_Phobia_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Bodily_Preoccupations_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Treatment_Experience_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Effects_Symptoms_2") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Third cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Worry_Illness_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Concerns_Pain_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Health_Habits_3") 
                                                        ),
                                                        column(width = 3,
                                                               uiOutput("Cutoff_Widg_Hypochondriacal_Beliefs_3") 
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Thanatophobia_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Disease_Phobia_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Bodily_Preoccupations_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Treatment_Experience_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Effects_Symptoms_3") 
                                                        )
                                                        
                                                      )
                                                      , hr()
                                                      
                                             ),
                                             
                                             
                                             h4(tags$strong("Psychometric Properties of the IAS Relevant to Assessing Reliable & Clinically Significant Change")),
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
                      input$Item_17, input$Item_18, input$Item_19, input$Item_20, 
                      input$Item_21, input$Item_22, input$Item_23, input$Item_24, input$Item_25, input$Item_26, input$Item_27, sep = ",")
    
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
    
    if(input$Pop == "General Population") {
      Mean_Val<<-33.57
      Sd_Val<<- 16.15
      Source_Mean<<- "Weck, Blechhardt & Hiller (2010)"
      Source_Sd<<- "Weck, Blechhardt & Hiller (2010)"
      Cut_Val_1<<- Mean_Val
      Cut_Val_2<<- 47
      Cut_Val_3<<- Mean_Val + Sd_Val
      Cut_Lab_1<<- "Mean (General German Population)"
      Cut_Lab_2<<- "Severe Health Anxiety Cut-Off (Hedman et al. 2015)"
      Cut_Lab_3<<- "Mean + 1 Sd (General German Population)"
      Source_Cutoff_1<<- "Weck, Blechhardt & Hiller (2010)"
      Source_Cutoff_2<<- "Hedman, Lekander Ljótsson, Lindefors, Rück, Andersson & Andersson (2015)"
      Source_Cutoff_3<<- "Weck, Blechhardt & Hiller (2010)"
      Mean_Val_Worry_Illness<<-4.3
      Sd_Val_Worry_Illness<<-2.2
      Cut_Val_Worry_Illness_1<<- Mean_Val_Worry_Illness 
      Cut_Val_Worry_Illness_2<<- Mean_Val_Worry_Illness + Sd_Val_Worry_Illness
      Cut_Val_Worry_Illness_3<<- Mean_Val_Worry_Illness + (2*Sd_Val_Worry_Illness)
      Cut_Lab_Worry_Illness_1<<- "Mean (University Sample)"
      Cut_Lab_Worry_Illness_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Worry_Illness_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Worry_Illness_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Worry_Illness_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Worry_Illness_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Concerns_Pain<<- 3.7
      Sd_Val_Concerns_Pain<<- 2.5
      Cut_Val_Concerns_Pain_1<<- Mean_Val_Concerns_Pain 
      Cut_Val_Concerns_Pain_2<<- Mean_Val_Concerns_Pain + Sd_Val_Concerns_Pain
      Cut_Val_Concerns_Pain_3<<- Mean_Val_Concerns_Pain + (2*Sd_Val_Concerns_Pain)
      Cut_Lab_Concerns_Pain_1<<- "Mean (University Sample)"
      Cut_Lab_Concerns_Pain_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Concerns_Pain_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Concerns_Pain_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Concerns_Pain_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Concerns_Pain_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Health_Habits<<- 6
      Sd_Val_Health_Habits<<- 2.5
      Cut_Val_Health_Habits_1<<- Mean_Val_Health_Habits 
      Cut_Val_Health_Habits_2<<- Mean_Val_Health_Habits + Sd_Val_Health_Habits
      Cut_Val_Health_Habits_3<<- Mean_Val_Health_Habits + (2*Sd_Val_Health_Habits)
      Cut_Lab_Health_Habits_1<<- "Mean (University Sample)"
      Cut_Lab_Health_Habits_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Health_Habits_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Health_Habits_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Health_Habits_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Health_Habits_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Hypochondriacal_Beliefs<<- 0.9
      Sd_Val_Hypochondriacal_Beliefs<<- 1.6
      Cut_Val_Hypochondriacal_Beliefs_1<<- Mean_Val_Hypochondriacal_Beliefs 
      Cut_Val_Hypochondriacal_Beliefs_2<<- Mean_Val_Hypochondriacal_Beliefs + Sd_Val_Hypochondriacal_Beliefs
      Cut_Val_Hypochondriacal_Beliefs_3<<- Mean_Val_Hypochondriacal_Beliefs + (2*Sd_Val_Hypochondriacal_Beliefs)
      Cut_Lab_Hypochondriacal_Beliefs_1<<- "Mean (University Sample)"
      Cut_Lab_Hypochondriacal_Beliefs_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Hypochondriacal_Beliefs_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Hypochondriacal_Beliefs_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Hypochondriacal_Beliefs_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Hypochondriacal_Beliefs_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Thanatophobia<<- 2.8
      Sd_Val_Thanatophobia<<- 2.5
      Cut_Val_Thanatophobia_1<<- Mean_Val_Thanatophobia
      Cut_Val_Thanatophobia_2<<- Mean_Val_Thanatophobia + Sd_Val_Thanatophobia
      Cut_Val_Thanatophobia_3<<- Mean_Val_Thanatophobia + (2*Sd_Val_Thanatophobia)
      Cut_Lab_Thanatophobia_1<<- "Mean (University Sample)"
      Cut_Lab_Thanatophobia_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Thanatophobia_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Thanatophobia_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Thanatophobia_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Thanatophobia_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Disease_Phobia<<- 1.3
      Sd_Val_Disease_Phobia<<- 1.5
      Cut_Val_Disease_Phobia_1<<- Mean_Val_Disease_Phobia 
      Cut_Val_Disease_Phobia_2<<- Mean_Val_Disease_Phobia + Sd_Val_Disease_Phobia
      Cut_Val_Disease_Phobia_3<<- Mean_Val_Disease_Phobia + (2*Sd_Val_Disease_Phobia)
      Cut_Lab_Disease_Phobia_1<<- "Mean (University Sample)"
      Cut_Lab_Disease_Phobia_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Disease_Phobia_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Disease_Phobia_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Disease_Phobia_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Disease_Phobia_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Bodily_Preoccupations<<- 2.3
      Sd_Val_Bodily_Preoccupations<<- 1.7
      Cut_Val_Bodily_Preoccupations_1<<- Mean_Val_Bodily_Preoccupations 
      Cut_Val_Bodily_Preoccupations_2<<- Mean_Val_Bodily_Preoccupations + Sd_Val_Bodily_Preoccupations
      Cut_Val_Bodily_Preoccupations_3<<- Mean_Val_Bodily_Preoccupations + (2*Sd_Val_Bodily_Preoccupations)
      Cut_Lab_Bodily_Preoccupations_1<<- "Mean (University Sample)"
      Cut_Lab_Bodily_Preoccupations_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Bodily_Preoccupations_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Bodily_Preoccupations_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Bodily_Preoccupations_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Bodily_Preoccupations_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Treatment_Experience<<- 3.9
      Sd_Val_Treatment_Experience<<- 2.3
      Cut_Val_Treatment_Experience_1<<- Mean_Val_Treatment_Experience 
      Cut_Val_Treatment_Experience_2<<- Mean_Val_Treatment_Experience + Sd_Val_Treatment_Experience
      Cut_Val_Treatment_Experience_3<<- Mean_Val_Treatment_Experience + (2*Sd_Val_Treatment_Experience)
      Cut_Lab_Treatment_Experience_1<<- "Mean (University Sample)"
      Cut_Lab_Treatment_Experience_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Treatment_Experience_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Treatment_Experience_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Treatment_Experience_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Treatment_Experience_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Effects_Symptoms<<- 2.7
      Sd_Val_Effects_Symptoms<<- 2.3
      Cut_Val_Effects_Symptoms_1<<- Mean_Val_Effects_Symptoms 
      Cut_Val_Effects_Symptoms_2<<- Mean_Val_Effects_Symptoms + Sd_Val_Effects_Symptoms
      Cut_Val_Effects_Symptoms_3<<- Mean_Val_Effects_Symptoms + (2*Sd_Val_Effects_Symptoms)
      Cut_Lab_Effects_Symptoms_1<<- "Mean (University Sample)"
      Cut_Lab_Effects_Symptoms_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Effects_Symptoms_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Effects_Symptoms_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Effects_Symptoms_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Effects_Symptoms_3<<- "Ferguson & Daniel (1994)"
    } else if(input$Pop == "Hypochondriasis") {
      Mean_Val<<-64
      Sd_Val<<- 14.10
      Source_Mean<<- "Höfling & Weck (2013)"
      Source_Sd<<- "Höfling & Weck (2013)"
      Cut_Val_1<<- 47
      Cut_Val_2<<- Mean_Val
      Cut_Val_3<<- Mean_Val + Sd_Val
      Cut_Lab_1<<- "Severe Health Anxiety Cut-Off (Hedman et al. 2015)"
      Cut_Lab_2<<- "Mean (Hypochondriasis Sample)"
      Cut_Lab_3<<- "Mean + 1 Sd (Hypochondriasis Sample)"
      Source_Cutoff_1<<- "Hedman, Lekander Ljótsson, Lindefors, Rück, Andersson & Andersson (2015)"
      Source_Cutoff_2<<- "Höfling & Weck (2013)"
      Source_Cutoff_3<<- "Höfling & Weck (2013)"
      Mean_Val_Worry_Illness<<-4.3
      Sd_Val_Worry_Illness<<-2.2
      Cut_Val_Worry_Illness_1<<- Mean_Val_Worry_Illness 
      Cut_Val_Worry_Illness_2<<- Mean_Val_Worry_Illness + Sd_Val_Worry_Illness
      Cut_Val_Worry_Illness_3<<- Mean_Val_Worry_Illness + (2*Sd_Val_Worry_Illness)
      Cut_Lab_Worry_Illness_1<<- "Mean (University Sample)"
      Cut_Lab_Worry_Illness_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Worry_Illness_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Worry_Illness_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Worry_Illness_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Worry_Illness_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Concerns_Pain<<- 3.7
      Sd_Val_Concerns_Pain<<- 2.5
      Cut_Val_Concerns_Pain_1<<- Mean_Val_Concerns_Pain 
      Cut_Val_Concerns_Pain_2<<- Mean_Val_Concerns_Pain + Sd_Val_Concerns_Pain
      Cut_Val_Concerns_Pain_3<<- Mean_Val_Concerns_Pain + (2*Sd_Val_Concerns_Pain)
      Cut_Lab_Concerns_Pain_1<<- "Mean (University Sample)"
      Cut_Lab_Concerns_Pain_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Concerns_Pain_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Concerns_Pain_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Concerns_Pain_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Concerns_Pain_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Health_Habits<<- 6
      Sd_Val_Health_Habits<<- 2.5
      Cut_Val_Health_Habits_1<<- Mean_Val_Health_Habits 
      Cut_Val_Health_Habits_2<<- Mean_Val_Health_Habits + Sd_Val_Health_Habits
      Cut_Val_Health_Habits_3<<- Mean_Val_Health_Habits + (2*Sd_Val_Health_Habits)
      Cut_Lab_Health_Habits_1<<- "Mean (University Sample)"
      Cut_Lab_Health_Habits_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Health_Habits_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Health_Habits_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Health_Habits_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Health_Habits_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Hypochondriacal_Beliefs<<- 0.9
      Sd_Val_Hypochondriacal_Beliefs<<- 1.6
      Cut_Val_Hypochondriacal_Beliefs_1<<- Mean_Val_Hypochondriacal_Beliefs 
      Cut_Val_Hypochondriacal_Beliefs_2<<- Mean_Val_Hypochondriacal_Beliefs + Sd_Val_Hypochondriacal_Beliefs
      Cut_Val_Hypochondriacal_Beliefs_3<<- Mean_Val_Hypochondriacal_Beliefs + (2*Sd_Val_Hypochondriacal_Beliefs)
      Cut_Lab_Hypochondriacal_Beliefs_1<<- "Mean (University Sample)"
      Cut_Lab_Hypochondriacal_Beliefs_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Hypochondriacal_Beliefs_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Hypochondriacal_Beliefs_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Hypochondriacal_Beliefs_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Hypochondriacal_Beliefs_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Thanatophobia<<- 2.8
      Sd_Val_Thanatophobia<<- 2.5
      Cut_Val_Thanatophobia_1<<- Mean_Val_Thanatophobia
      Cut_Val_Thanatophobia_2<<- Mean_Val_Thanatophobia + Sd_Val_Thanatophobia
      Cut_Val_Thanatophobia_3<<- Mean_Val_Thanatophobia + (2*Sd_Val_Thanatophobia)
      Cut_Lab_Thanatophobia_1<<- "Mean (University Sample)"
      Cut_Lab_Thanatophobia_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Thanatophobia_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Thanatophobia_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Thanatophobia_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Thanatophobia_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Disease_Phobia<<- 1.3
      Sd_Val_Disease_Phobia<<- 1.5
      Cut_Val_Disease_Phobia_1<<- Mean_Val_Disease_Phobia 
      Cut_Val_Disease_Phobia_2<<- Mean_Val_Disease_Phobia + Sd_Val_Disease_Phobia
      Cut_Val_Disease_Phobia_3<<- Mean_Val_Disease_Phobia + (2*Sd_Val_Disease_Phobia)
      Cut_Lab_Disease_Phobia_1<<- "Mean (University Sample)"
      Cut_Lab_Disease_Phobia_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Disease_Phobia_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Disease_Phobia_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Disease_Phobia_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Disease_Phobia_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Bodily_Preoccupations<<- 2.3
      Sd_Val_Bodily_Preoccupations<<- 1.7
      Cut_Val_Bodily_Preoccupations_1<<- Mean_Val_Bodily_Preoccupations 
      Cut_Val_Bodily_Preoccupations_2<<- Mean_Val_Bodily_Preoccupations + Sd_Val_Bodily_Preoccupations
      Cut_Val_Bodily_Preoccupations_3<<- Mean_Val_Bodily_Preoccupations + (2*Sd_Val_Bodily_Preoccupations)
      Cut_Lab_Bodily_Preoccupations_1<<- "Mean (University Sample)"
      Cut_Lab_Bodily_Preoccupations_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Bodily_Preoccupations_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Bodily_Preoccupations_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Bodily_Preoccupations_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Bodily_Preoccupations_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Treatment_Experience<<- 3.9
      Sd_Val_Treatment_Experience<<- 2.3
      Cut_Val_Treatment_Experience_1<<- Mean_Val_Treatment_Experience 
      Cut_Val_Treatment_Experience_2<<- Mean_Val_Treatment_Experience + Sd_Val_Treatment_Experience
      Cut_Val_Treatment_Experience_3<<- Mean_Val_Treatment_Experience + (2*Sd_Val_Treatment_Experience)
      Cut_Lab_Treatment_Experience_1<<- "Mean (University Sample)"
      Cut_Lab_Treatment_Experience_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Treatment_Experience_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Treatment_Experience_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Treatment_Experience_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Treatment_Experience_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Effects_Symptoms<<- 2.7
      Sd_Val_Effects_Symptoms<<- 2.3
      Cut_Val_Effects_Symptoms_1<<- Mean_Val_Effects_Symptoms 
      Cut_Val_Effects_Symptoms_2<<- Mean_Val_Effects_Symptoms + Sd_Val_Effects_Symptoms
      Cut_Val_Effects_Symptoms_3<<- Mean_Val_Effects_Symptoms + (2*Sd_Val_Effects_Symptoms)
      Cut_Lab_Effects_Symptoms_1<<- "Mean (University Sample)"
      Cut_Lab_Effects_Symptoms_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Effects_Symptoms_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Effects_Symptoms_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Effects_Symptoms_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Effects_Symptoms_3<<- "Ferguson & Daniel (1994)"
    } else if(input$Pop == "Anxiety Disorder") {
      Mean_Val<<-31
      Sd_Val<<- 19.9
      Source_Mean<<- "Höfling & Weck (2013)"
      Source_Sd<<- "Höfling & Weck (2013)"
      Cut_Val_1<<- Mean_Val
      Cut_Val_2<<- 47
      Cut_Val_3<<- Mean_Val + Sd_Val
      Cut_Lab_1<<- "Mean (Anxiety Disorder Sample)"
      Cut_Lab_2<<- "Severe Health Anxiety Cut-Off (Hedman et al. 2015)"
      Cut_Lab_3<<- "Mean + 1 Sd (Anxiety Disorder Sample)"
      Source_Cutoff_1<<- "Höfling & Weck (2013)"
      Source_Cutoff_2<<- "Hedman, Lekander Ljótsson, Lindefors, Rück, Andersson & Andersson (2015)"
      Source_Cutoff_3<<- "Höfling & Weck (2013)"
      Mean_Val_Worry_Illness<<-4.3
      Sd_Val_Worry_Illness<<-2.2
      Cut_Val_Worry_Illness_1<<- Mean_Val_Worry_Illness 
      Cut_Val_Worry_Illness_2<<- Mean_Val_Worry_Illness + Sd_Val_Worry_Illness
      Cut_Val_Worry_Illness_3<<- Mean_Val_Worry_Illness + (2*Sd_Val_Worry_Illness)
      Cut_Lab_Worry_Illness_1<<- "Mean (University Sample)"
      Cut_Lab_Worry_Illness_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Worry_Illness_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Worry_Illness_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Worry_Illness_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Worry_Illness_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Concerns_Pain<<- 3.7
      Sd_Val_Concerns_Pain<<- 2.5
      Cut_Val_Concerns_Pain_1<<- Mean_Val_Concerns_Pain 
      Cut_Val_Concerns_Pain_2<<- Mean_Val_Concerns_Pain + Sd_Val_Concerns_Pain
      Cut_Val_Concerns_Pain_3<<- Mean_Val_Concerns_Pain + (2*Sd_Val_Concerns_Pain)
      Cut_Lab_Concerns_Pain_1<<- "Mean (University Sample)"
      Cut_Lab_Concerns_Pain_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Concerns_Pain_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Concerns_Pain_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Concerns_Pain_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Concerns_Pain_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Health_Habits<<- 6
      Sd_Val_Health_Habits<<- 2.5
      Cut_Val_Health_Habits_1<<- Mean_Val_Health_Habits 
      Cut_Val_Health_Habits_2<<- Mean_Val_Health_Habits + Sd_Val_Health_Habits
      Cut_Val_Health_Habits_3<<- Mean_Val_Health_Habits + (2*Sd_Val_Health_Habits)
      Cut_Lab_Health_Habits_1<<- "Mean (University Sample)"
      Cut_Lab_Health_Habits_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Health_Habits_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Health_Habits_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Health_Habits_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Health_Habits_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Hypochondriacal_Beliefs<<- 0.9
      Sd_Val_Hypochondriacal_Beliefs<<- 1.6
      Cut_Val_Hypochondriacal_Beliefs_1<<- Mean_Val_Hypochondriacal_Beliefs 
      Cut_Val_Hypochondriacal_Beliefs_2<<- Mean_Val_Hypochondriacal_Beliefs + Sd_Val_Hypochondriacal_Beliefs
      Cut_Val_Hypochondriacal_Beliefs_3<<- Mean_Val_Hypochondriacal_Beliefs + (2*Sd_Val_Hypochondriacal_Beliefs)
      Cut_Lab_Hypochondriacal_Beliefs_1<<- "Mean (University Sample)"
      Cut_Lab_Hypochondriacal_Beliefs_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Hypochondriacal_Beliefs_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Hypochondriacal_Beliefs_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Hypochondriacal_Beliefs_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Hypochondriacal_Beliefs_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Thanatophobia<<- 2.8
      Sd_Val_Thanatophobia<<- 2.5
      Cut_Val_Thanatophobia_1<<- Mean_Val_Thanatophobia
      Cut_Val_Thanatophobia_2<<- Mean_Val_Thanatophobia + Sd_Val_Thanatophobia
      Cut_Val_Thanatophobia_3<<- Mean_Val_Thanatophobia + (2*Sd_Val_Thanatophobia)
      Cut_Lab_Thanatophobia_1<<- "Mean (University Sample)"
      Cut_Lab_Thanatophobia_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Thanatophobia_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Thanatophobia_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Thanatophobia_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Thanatophobia_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Disease_Phobia<<- 1.3
      Sd_Val_Disease_Phobia<<- 1.5
      Cut_Val_Disease_Phobia_1<<- Mean_Val_Disease_Phobia 
      Cut_Val_Disease_Phobia_2<<- Mean_Val_Disease_Phobia + Sd_Val_Disease_Phobia
      Cut_Val_Disease_Phobia_3<<- Mean_Val_Disease_Phobia + (2*Sd_Val_Disease_Phobia)
      Cut_Lab_Disease_Phobia_1<<- "Mean (University Sample)"
      Cut_Lab_Disease_Phobia_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Disease_Phobia_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Disease_Phobia_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Disease_Phobia_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Disease_Phobia_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Bodily_Preoccupations<<- 2.3
      Sd_Val_Bodily_Preoccupations<<- 1.7
      Cut_Val_Bodily_Preoccupations_1<<- Mean_Val_Bodily_Preoccupations 
      Cut_Val_Bodily_Preoccupations_2<<- Mean_Val_Bodily_Preoccupations + Sd_Val_Bodily_Preoccupations
      Cut_Val_Bodily_Preoccupations_3<<- Mean_Val_Bodily_Preoccupations + (2*Sd_Val_Bodily_Preoccupations)
      Cut_Lab_Bodily_Preoccupations_1<<- "Mean (University Sample)"
      Cut_Lab_Bodily_Preoccupations_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Bodily_Preoccupations_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Bodily_Preoccupations_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Bodily_Preoccupations_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Bodily_Preoccupations_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Treatment_Experience<<- 3.9
      Sd_Val_Treatment_Experience<<- 2.3
      Cut_Val_Treatment_Experience_1<<- Mean_Val_Treatment_Experience 
      Cut_Val_Treatment_Experience_2<<- Mean_Val_Treatment_Experience + Sd_Val_Treatment_Experience
      Cut_Val_Treatment_Experience_3<<- Mean_Val_Treatment_Experience + (2*Sd_Val_Treatment_Experience)
      Cut_Lab_Treatment_Experience_1<<- "Mean (University Sample)"
      Cut_Lab_Treatment_Experience_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Treatment_Experience_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Treatment_Experience_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Treatment_Experience_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Treatment_Experience_3<<- "Ferguson & Daniel (1994)"
      Mean_Val_Effects_Symptoms<<- 2.7
      Sd_Val_Effects_Symptoms<<- 2.3
      Cut_Val_Effects_Symptoms_1<<- Mean_Val_Effects_Symptoms 
      Cut_Val_Effects_Symptoms_2<<- Mean_Val_Effects_Symptoms + Sd_Val_Effects_Symptoms
      Cut_Val_Effects_Symptoms_3<<- Mean_Val_Effects_Symptoms + (2*Sd_Val_Effects_Symptoms)
      Cut_Lab_Effects_Symptoms_1<<- "Mean (University Sample)"
      Cut_Lab_Effects_Symptoms_2<<- "Mean + 1 Sd (University Sample)"
      Cut_Lab_Effects_Symptoms_3<<- "Mean + 2 Sd (University Sample)"
      Source_Cutoff_Effects_Symptoms_1<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Effects_Symptoms_2<<- "Ferguson & Daniel (1994)"
      Source_Cutoff_Effects_Symptoms_3<<- "Ferguson & Daniel (1994)"
    }    
    
  })
  
  
  output$Mean_Widg<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean", "IAS total scale", Mean_Val),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Worry_Illness<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Worry_Illness", "Worry about Illness", Mean_Val_Worry_Illness),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Mean_Widg_Worry_Illness", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Concerns_Pain<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Concerns_Pain", "Concerns about Pain", Mean_Val_Concerns_Pain),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Mean_Widg_Concerns_Pain", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Health_Habits<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Health_Habits", "Health Habits", Mean_Val_Health_Habits),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Mean_Widg_Health_Habits", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Hypochondriacal_Beliefs<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Hypochondriacal_Beliefs", "Hypochondriacal Beliefs", Mean_Val_Hypochondriacal_Beliefs),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Mean_Widg_Hypochondriacal_Beliefs", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Thanatophobia<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Thanatophobia", "Thanatophobia", Mean_Val_Thanatophobia),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Mean_Widg_Thanatophobia", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Disease_Phobia<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Disease_Phobia", "Disease Phobia", Mean_Val_Disease_Phobia),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Mean_Widg_Disease_Phobia", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Bodily_Preoccupations<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Bodily_Preoccupations", "Bodily Preoccupations", Mean_Val_Bodily_Preoccupations),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Mean_Widg_Bodily_Preoccupations", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Treatment_Experience<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Treatment_Experience", "Treatment Experience", Mean_Val_Treatment_Experience),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Mean_Widg_Treatment_Experience", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Effects_Symptoms<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Effects_Symptoms", "Effects of Symptoms", Mean_Val_Effects_Symptoms),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Mean_Widg_Effects_Symptoms", suspendWhenHidden = FALSE) 
  
  

  output$Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd", "IAS total scale", Sd_Val),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Worry_Illness<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Worry_Illness", "Worry about Illness", Sd_Val_Worry_Illness),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Sd_Widg_Worry_Illness", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Concerns_Pain<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Concerns_Pain", "Concerns about Pain", Sd_Val_Concerns_Pain),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Sd_Widg_Concerns_Pain", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Health_Habits<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Health_Habits", "Health Habits", Sd_Val_Health_Habits),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Sd_Widg_Health_Habits", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Hypochondriacal_Beliefs<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Hypochondriacal_Beliefs", "Hypochondriacal Beliefs", Sd_Val_Hypochondriacal_Beliefs),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Sd_Widg_Hypochondriacal_Beliefs", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Thanatophobia<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Thanatophobia", "Thanatophobia", Sd_Val_Thanatophobia),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Sd_Widg_Thanatophobia", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Disease_Phobia<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Disease_Phobia", "Disease Phobia", Sd_Val_Disease_Phobia),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Sd_Widg_Disease_Phobia", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Bodily_Preoccupations<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Bodily_Preoccupations", "Bodily Preoccupations", Sd_Val_Bodily_Preoccupations),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Sd_Widg_Bodily_Preoccupations", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Treatment_Experience<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Treatment_Experience", "Treatment Experience", Sd_Val_Treatment_Experience),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Sd_Widg_Treatment_Experience", suspendWhenHidden = FALSE)
  
  output$Sd_Widg_Effects_Symptoms<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Effects_Symptoms", "Effects of Symptoms", Sd_Val_Effects_Symptoms),
      h6(paste("Reference: Ferguson & Daniel (1994)"))
    )
  })
  outputOptions(output, "Sd_Widg_Effects_Symptoms", suspendWhenHidden = FALSE)
  
  
  
  output$Cutoff_Widg_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_1", "IAS total scale", as.numeric(Cut_Val_1)),
      textInput("Cutoff_Text_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source_Cutoff_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Worry_Illness_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Worry_Illness_1", "Worry about Illness", as.numeric(Cut_Val_Worry_Illness_1)),
      textInput("Cutoff_Text_Worry_Illness_1", "Cut-Off Score Name", Cut_Lab_Worry_Illness_1),
      h6(paste("Reference:", Source_Cutoff_Worry_Illness_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Worry_Illness_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Concerns_Pain_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Concerns_Pain_1", "Concerns about Pain", as.numeric(Cut_Val_Concerns_Pain_1)),
      textInput("Cutoff_Text_Concerns_Pain_1", "Cut-Off Score Name", Cut_Lab_Concerns_Pain_1),
      h6(paste("Reference:", Source_Cutoff_Concerns_Pain_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Concerns_Pain_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Health_Habits_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Health_Habits_1", "Health Habits", as.numeric(Cut_Val_Health_Habits_1)),
      textInput("Cutoff_Text_Health_Habits_1", "Cut-Off Score Name", Cut_Lab_Health_Habits_1),
      h6(paste("Reference:", Source_Cutoff_Health_Habits_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Health_Habits_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Hypochondriacal_Beliefs_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Hypochondriacal_Beliefs_1", "Hypochondriacal Beliefs", as.numeric(Cut_Val_Hypochondriacal_Beliefs_1)),
      textInput("Cutoff_Text_Hypochondriacal_Beliefs_1", "Cut-Off Score Name", Cut_Lab_Hypochondriacal_Beliefs_1),
      h6(paste("Reference:", Source_Cutoff_Hypochondriacal_Beliefs_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Hypochondriacal_Beliefs_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Thanatophobia_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Thanatophobia_1", "Thanatophobia", as.numeric(Cut_Val_Thanatophobia_1)),
      textInput("Cutoff_Text_Thanatophobia_1", "Cut-Off Score Name", Cut_Lab_Thanatophobia_1),
      h6(paste("Reference:", Source_Cutoff_Thanatophobia_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Thanatophobia_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Disease_Phobia_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Disease_Phobia_1", "Disease Phobia", as.numeric(Cut_Val_Disease_Phobia_1)),
      textInput("Cutoff_Text_Disease_Phobia_1", "Cut-Off Score Name", Cut_Lab_Disease_Phobia_1),
      h6(paste("Reference:", Source_Cutoff_Disease_Phobia_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Disease_Phobia_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Bodily_Preoccupations_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Bodily_Preoccupations_1", "Bodily Preoccupations", as.numeric(Cut_Val_Bodily_Preoccupations_1)),
      textInput("Cutoff_Text_Bodily_Preoccupations_1", "Cut-Off Score Name", Cut_Lab_Bodily_Preoccupations_1),
      h6(paste("Reference:", Source_Cutoff_Bodily_Preoccupations_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Bodily_Preoccupations_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Treatment_Experience_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Treatment_Experience_1", "Treatment Experience", as.numeric(Cut_Val_Treatment_Experience_1)),
      textInput("Cutoff_Text_Treatment_Experience_1", "Cut-Off Score Name", Cut_Lab_Treatment_Experience_1),
      h6(paste("Reference:", Source_Cutoff_Treatment_Experience_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Treatment_Experience_1", suspendWhenHidden = FALSE)
  
  output$Cutoff_Widg_Effects_Symptoms_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Effects_Symptoms_1", "Effects of Symptoms", as.numeric(Cut_Val_Effects_Symptoms_1)),
      textInput("Cutoff_Text_Effects_Symptoms_1", "Cut-Off Score Name", Cut_Lab_Effects_Symptoms_1),
      h6(paste("Reference:", Source_Cutoff_Effects_Symptoms_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Effects_Symptoms_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_2", "IAS total scale", as.numeric(Cut_Val_2)),
      textInput("Cutoff_Text_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source_Cutoff_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Worry_Illness_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Worry_Illness_2", "Worry about Illness", as.numeric(Cut_Val_Worry_Illness_2)),
      textInput("Cutoff_Text_Worry_Illness_2", "Cut-Off Score Name", Cut_Lab_Worry_Illness_2),
      h6(paste("Reference:", Source_Cutoff_Worry_Illness_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Worry_Illness_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Concerns_Pain_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Concerns_Pain_2", "Concerns about Pain", as.numeric(Cut_Val_Concerns_Pain_2)),
      textInput("Cutoff_Text_Concerns_Pain_2", "Cut-Off Score Name", Cut_Lab_Concerns_Pain_2),
      h6(paste("Reference:", Source_Cutoff_Concerns_Pain_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Concerns_Pain_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Health_Habits_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Health_Habits_2", "Health Habits", as.numeric(Cut_Val_Health_Habits_2)),
      textInput("Cutoff_Text_Health_Habits_2", "Cut-Off Score Name", Cut_Lab_Health_Habits_2),
      h6(paste("Reference:", Source_Cutoff_Health_Habits_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Health_Habits_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Hypochondriacal_Beliefs_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Hypochondriacal_Beliefs_2", "Hypochondriacal Beliefs", as.numeric(Cut_Val_Hypochondriacal_Beliefs_2)),
      textInput("Cutoff_Text_Hypochondriacal_Beliefs_2", "Cut-Off Score Name", Cut_Lab_Hypochondriacal_Beliefs_2),
      h6(paste("Reference:", Source_Cutoff_Hypochondriacal_Beliefs_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Hypochondriacal_Beliefs_2", suspendWhenHidden = FALSE)
  

  output$Cutoff_Widg_Thanatophobia_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Thanatophobia_2", "Thanatophobia", as.numeric(Cut_Val_Thanatophobia_2)),
      textInput("Cutoff_Text_Thanatophobia_2", "Cut-Off Score Name", Cut_Lab_Thanatophobia_2),
      h6(paste("Reference:", Source_Cutoff_Thanatophobia_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Thanatophobia_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Disease_Phobia_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Disease_Phobia_2", "Disease Phobia", as.numeric(Cut_Val_Disease_Phobia_2)),
      textInput("Cutoff_Text_Disease_Phobia_2", "Cut-Off Score Name", Cut_Lab_Disease_Phobia_2),
      h6(paste("Reference:", Source_Cutoff_Disease_Phobia_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Disease_Phobia_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Bodily_Preoccupations_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Bodily_Preoccupations_2", "Bodily Preoccupations", as.numeric(Cut_Val_Bodily_Preoccupations_2)),
      textInput("Cutoff_Text_Bodily_Preoccupations_2", "Cut-Off Score Name", Cut_Lab_Bodily_Preoccupations_2),
      h6(paste("Reference:", Source_Cutoff_Bodily_Preoccupations_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Bodily_Preoccupations_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Treatment_Experience_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Treatment_Experience_2", "Treatment Experience", as.numeric(Cut_Val_Treatment_Experience_2)),
      textInput("Cutoff_Text_Treatment_Experience_2", "Cut-Off Score Name", Cut_Lab_Treatment_Experience_2),
      h6(paste("Reference:", Source_Cutoff_Treatment_Experience_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Treatment_Experience_2", suspendWhenHidden = FALSE)
  
  output$Cutoff_Widg_Effects_Symptoms_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Effects_Symptoms_2", "Effects of Symptoms", as.numeric(Cut_Val_Effects_Symptoms_2)),
      textInput("Cutoff_Text_Effects_Symptoms_2", "Cut-Off Score Name", Cut_Lab_Effects_Symptoms_2),
      h6(paste("Reference:", Source_Cutoff_Effects_Symptoms_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Effects_Symptoms_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_3", "IAS total scale", as.numeric(Cut_Val_3)),
      textInput("Cutoff_Text_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source_Cutoff_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Worry_Illness_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Worry_Illness_3", "Worry about Illness", as.numeric(Cut_Val_Worry_Illness_3)),
      textInput("Cutoff_Text_Worry_Illness_3", "Cut-Off Score Name", Cut_Lab_Worry_Illness_3),
      h6(paste("Reference:", Source_Cutoff_Worry_Illness_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Worry_Illness_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Concerns_Pain_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Concerns_Pain_3", "Concerns about Pain", as.numeric(Cut_Val_Concerns_Pain_3)),
      textInput("Cutoff_Text_Concerns_Pain_3", "Cut-Off Score Name", Cut_Lab_Concerns_Pain_3),
      h6(paste("Reference:", Source_Cutoff_Concerns_Pain_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Concerns_Pain_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Health_Habits_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Health_Habits_3", "Health Habits", as.numeric(Cut_Val_Health_Habits_3)),
      textInput("Cutoff_Text_Health_Habits_3", "Cut-Off Score Name", Cut_Lab_Health_Habits_3),
      h6(paste("Reference:", Source_Cutoff_Health_Habits_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Health_Habits_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Hypochondriacal_Beliefs_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Hypochondriacal_Beliefs_3", "Hypochondriacal Beliefs", as.numeric(Cut_Val_Hypochondriacal_Beliefs_3)),
      textInput("Cutoff_Text_Hypochondriacal_Beliefs_3", "Cut-Off Score Name", Cut_Lab_Hypochondriacal_Beliefs_3),
      h6(paste("Reference:", Source_Cutoff_Hypochondriacal_Beliefs_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Hypochondriacal_Beliefs_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Thanatophobia_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Thanatophobia_3", "Thanatophobia", as.numeric(Cut_Val_Thanatophobia_3)),
      textInput("Cutoff_Text_Thanatophobia_3", "Cut-Off Score Name", Cut_Lab_Thanatophobia_3),
      h6(paste("Reference:", Source_Cutoff_Thanatophobia_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Thanatophobia_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Disease_Phobia_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Disease_Phobia_3", "Disease Phobia", as.numeric(Cut_Val_Disease_Phobia_3)),
      textInput("Cutoff_Text_Disease_Phobia_3", "Cut-Off Score Name", Cut_Lab_Disease_Phobia_3),
      h6(paste("Reference:", Source_Cutoff_Disease_Phobia_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Disease_Phobia_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Bodily_Preoccupations_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Bodily_Preoccupations_3", "Bodily Preoccupations", as.numeric(Cut_Val_Bodily_Preoccupations_3)),
      textInput("Cutoff_Text_Bodily_Preoccupations_3", "Cut-Off Score Name", Cut_Lab_Bodily_Preoccupations_3),
      h6(paste("Reference:", Source_Cutoff_Bodily_Preoccupations_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Bodily_Preoccupations_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Treatment_Experience_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Treatment_Experience_3", "Treatment Experience", as.numeric(Cut_Val_Treatment_Experience_3)),
      textInput("Cutoff_Text_Treatment_Experience_3", "Cut-Off Score Name", Cut_Lab_Treatment_Experience_3),
      h6(paste("Reference:", Source_Cutoff_Treatment_Experience_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Treatment_Experience_3", suspendWhenHidden = FALSE)
  
  output$Cutoff_Widg_Effects_Symptoms_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Effects_Symptoms_3", "Effects of Symptoms", as.numeric(Cut_Val_Effects_Symptoms_3)),
      textInput("Cutoff_Text_Effects_Symptoms_3", "Cut-Off Score Name", Cut_Lab_Effects_Symptoms_3),
      h6(paste("Reference:", Source_Cutoff_Effects_Symptoms_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Effects_Symptoms_3", suspendWhenHidden = FALSE)
  
  

  #Create a reactive expression defining entered values  
  
  Entered_Scores_Reac<- reactive({
    
    TP<- input$Timepoint
    
    PN<<- input$PatientName
    CN<<- input$ClinicianName
    
    Pop<- input$Pop
    
    RelChangeMethod<- input$RelChangeMethod
    
    CI_Vals_Reac()
    
    M<- input$Pop_Mean
    SD<- input$Pop_Sd
    M_Worry_Illness<- input$Pop_Mean_Worry_Illness
    SD_Worry_Illness<-input$Pop_Sd_Worry_Illness
    M_Concerns_Pain<- input$Pop_Mean_Concerns_Pain
    SD_Concerns_Pain<- input$Pop_Sd_Concerns_Pain
    M_Health_Habits<- input$Pop_Mean_Health_Habits
    SD_Health_Habits<- input$Pop_Sd_Health_Habits
    M_Hypochondriacal_Beliefs<- input$Pop_Mean_Hypochondriacal_Beliefs
    SD_Hypochondriacal_Beliefs<- input$Pop_Sd_Hypochondriacal_Beliefs
    M_Thanatophobia<- input$Pop_Mean_Thanatophobia
    SD_Thanatophobia<-input$Pop_Sd_Thanatophobia
    M_Disease_Phobia<- input$Pop_Mean_Disease_Phobia
    SD_Disease_Phobia<- input$Pop_Sd_Disease_Phobia
    M_Bodily_Preoccupations<- input$Pop_Mean_Bodily_Preoccupations
    SD_Bodily_Preoccupations<- input$Pop_Sd_Bodily_Preoccupations
    M_Treatment_Experience<- input$Pop_Mean_Treatment_Experience
    SD_Treatment_Experience<- input$Pop_Sd_Treatment_Experience
    M_Effects_Symptoms<- input$Pop_Mean_Effects_Symptoms
    SD_Effects_Symptoms<- input$Pop_Sd_Effects_Symptoms
    
    
    M_Retest<- input$Retest_Mean
    SD_Retest<- input$Retest_Sd
    M_Retest_Worry_Illness<- input$Retest_Mean_Worry_Illness
    SD_Retest_Worry_Illness<- input$Retest_Sd_Worry_Illness
    M_Retest_Concerns_Pain<- input$Retest_Mean_Concerns_Pain
    SD_Retest_Concerns_Pain<- input$Retest_Sd_Concerns_Pain
    M_Retest_Health_Habits<- input$Retest_Mean_Health_Habits
    SD_Retest_Health_Habits<- input$Retest_Sd_Health_Habits
    M_Retest_Hypochondriacal_Beliefs<- input$Retest_Mean_Hypochondriacal_Beliefs
    SD_Retest_Hypochondriacal_Beliefs<- input$Retest_Sd_Hypochondriacal_Beliefs
    M_Retest_Thanatophobia<- input$Retest_Mean_Thanatophobia
    SD_Retest_Thanatophobia<- input$Retest_Sd_Thanatophobia
    M_Retest_Disease_Phobia<- input$Retest_Mean_Disease_Phobia
    SD_Retest_Disease_Phobia<- input$Retest_Sd_Disease_Phobia
    M_Retest_Bodily_Preoccupations<- input$Retest_Mean_Bodily_Preoccupations
    SD_Retest_Bodily_Preoccupations<- input$Retest_Sd_Bodily_Preoccupations
    M_Retest_Treatment_Experience<- input$Retest_Mean_Treatment_Experience
    SD_Retest_Treatment_Experience<- input$Retest_Sd_Treatment_Experience
    M_Retest_Effects_Symptoms<- input$Retest_Mean_Effects_Symptoms
    SD_Retest_Effects_Symptoms<- input$Retest_Sd_Effects_Symptoms
    
    
    SampleN<- input$SampleN
    
    Rel<- input$Reliability
    Rel_Worry_Illness<- input$Reliability_Worry_Illness
    Rel_Concerns_Pain<- input$Reliability_Concerns_Pain
    Rel_Health_Habits<- input$Reliability_Health_Habits
    Rel_Hypochondriacal_Beliefs<- input$Reliability_Hypochondriacal_Beliefs
    Rel_Thanatophobia<- input$Reliability_Thanatophobia
    Rel_Disease_Phobia<- input$Reliability_Disease_Phobia
    Rel_Bodily_Preoccupations<- input$Reliability_Bodily_Preoccupations
    Rel_Treatment_Experience<- input$Reliability_Treatment_Experience
    Rel_Effects_Symptoms<- input$Reliability_Effects_Symptoms
    
    Tab_Reference<<- Source_Mean
    
    
    Illness<<- input$Illness_Text
    Afraid_Illness<<- input$Afraid_Illness_Text
    Year_Treatments<<- input$Treatment_Text
    
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
      SE_Worry_Illness<-SD_Worry_Illness * sqrt(1 - Rel_Worry_Illness^2)
      SE_Concerns_Pain<-SD_Concerns_Pain * sqrt(1 - Rel_Concerns_Pain^2)
      SE_Health_Habits<-SD_Health_Habits * sqrt(1 - Rel_Health_Habits^2)
      SE_Hypochondriacal_Beliefs<-SD_Hypochondriacal_Beliefs * sqrt(1 - Rel_Hypochondriacal_Beliefs^2)
      SE<- round(SE, digits = 2)
      SE_Worry_Illness<- round(SE_Worry_Illness, digits = 2)
      SE_Concerns_Pain<- round(SE_Concerns_Pain, digits = 2)
      SE_Health_Habits<- round(SE_Health_Habits, digits = 2)
      SE_Hypochondriacal_Beliefs<- round(SE_Hypochondriacal_Beliefs, digits = 2)
      SE_Thanatophobia<-SD_Thanatophobia * sqrt(1 - Rel_Thanatophobia^2)
      SE_Disease_Phobia<-SD_Disease_Phobia * sqrt(1 - Rel_Disease_Phobia^2)
      SE_Bodily_Preoccupations<-SD_Bodily_Preoccupations * sqrt(1 - Rel_Bodily_Preoccupations^2)
      SE_Treatment_Experience<-SD_Treatment_Experience * sqrt(1 - Rel_Treatment_Experience^2)
      SE_Effects_Symptoms<- SD_Effects_Symptoms * sqrt(1 - Rel_Effects_Symptoms^2)
      SE_Thanatophobia<- round(SE_Thanatophobia, digits = 2)
      SE_Disease_Phobia<- round(SE_Disease_Phobia, digits = 2)
      SE_Bodily_Preoccupations<- round(SE_Bodily_Preoccupations, digits = 2)
      SE_Treatment_Experience<- round(SE_Treatment_Experience, digits = 2)
      SE_Effects_Symptoms<- round(SE_Effects_Symptoms, digits = 2)
    } else if (input$RelChangeMethod == "Chelune et al. (1993)" | input$RelChangeMethod == "Jacobson & Truax (1991)" | input$RelChangeMethod == "Speer (1992)") {
      SE<- sqrt((2*(SD^2))*(1-Rel))
      SE_Worry_Illness<- sqrt((2*(SD_Worry_Illness^2))*(1-Rel_Worry_Illness))
      SE_Concerns_Pain<- sqrt((2*(SD_Concerns_Pain^2))*(1-Rel_Concerns_Pain))
      SE_Health_Habits<- sqrt((2*(SD_Health_Habits^2))*(1-Rel_Health_Habits))
      SE_Hypochondriacal_Beliefs<- sqrt((2*(SD_Hypochondriacal_Beliefs^2))*(1-Rel_Hypochondriacal_Beliefs))
      SE_Thanatophobia<- sqrt((2*(SD_Thanatophobia^2))*(1-Rel_Thanatophobia))
      SE_Disease_Phobia<- sqrt((2*(SD_Disease_Phobia^2))*(1-Rel_Disease_Phobia))
      SE_Bodily_Preoccupations<- sqrt((2*(SD_Bodily_Preoccupations^2))*(1-Rel_Bodily_Preoccupations))
      SE_Treatment_Experience<- sqrt((2*(SD_Treatment_Experience^2))*(1-Rel_Treatment_Experience))
      SE_Effects_Symptoms<- sqrt((2*(SD_Effects_Symptoms^2))*(1-Rel_Effects_Symptoms))
      SE<- round(SE, digits = 2)
      SE_Worry_Illness<- round(SE_Worry_Illness, digits = 2)
      SE_Concerns_Pain<- round(SE_Concerns_Pain, digits = 2)
      SE_Health_Habits<- round(SE_Health_Habits, digits = 2)
      SE_Hypochondriacal_Beliefs<- round(SE_Hypochondriacal_Beliefs, digits = 2)
      SE_Thanatophobia<- round(SE_Thanatophobia, digits = 2)
      SE_Disease_Phobia<- round(SE_Disease_Phobia, digits = 2)
      SE_Bodily_Preoccupations<- round(SE_Bodily_Preoccupations, digits = 2)
      SE_Treatment_Experience<- round(SE_Treatment_Experience, digits = 2)
      SE_Effects_Symptoms<- round(SE_Effects_Symptoms, digits = 2)
    } else if (input$RelChangeMethod == "Maassen et al. (2006)") {
      SE<- sqrt((SD^2 + SD_Retest^2)*(1-Rel))
      SE_Worry_Illness<- sqrt((SD_Worry_Illness^2 + SD_Retest_Worry_Illness^2)*(1-Rel_Worry_Illness))
      SE_Concerns_Pain<- sqrt((SD_Concerns_Pain^2 + SD_Retest_Concerns_Pain^2)*(1-Rel_Concerns_Pain))
      SE_Health_Habits<- sqrt((SD_Health_Habits^2 + SD_Retest_Health_Habits^2)*(1-Rel_Health_Habits))
      SE_Hypochondriacal_Beliefs<- sqrt((SD_Hypochondriacal_Beliefs^2 + SD_Retest_Hypochondriacal_Beliefs^2)*(1-Rel_Hypochondriacal_Beliefs))
      SE<- round(SE, digits = 2)
      SE_Worry_Illness<- round(SE_Worry_Illness, digits = 2)
      SE_Concerns_Pain<- round(SE_Concerns_Pain, digits = 2)
      SE_Health_Habits<- round(SE_Health_Habits, digits = 2)
      SE_Hypochondriacal_Beliefs<- round(SE_Hypochondriacal_Beliefs, digits = 2)
      SE_Thanatophobia<- sqrt((SD_Thanatophobia^2 + SD_Retest_Thanatophobia^2)*(1-Rel_Thanatophobia))
      SE_Disease_Phobia<- sqrt((SD_Disease_Phobia^2 + SD_Retest_Disease_Phobia^2)*(1-Rel_Disease_Phobia))
      SE_Bodily_Preoccupations<- sqrt((SD_Bodily_Preoccupations^2 + SD_Retest_Bodily_Preoccupations^2)*(1-Rel_Bodily_Preoccupations))
      SE_Treatment_Experience<- sqrt((SD_Treatment_Experience^2 + SD_Retest_Treatment_Experience^2)*(1-Rel_Treatment_Experience))
      SE_Effects_Symptoms<- sqrt((SD_Effects_Symptoms^2 + SD_Retest_Effects_Symptoms^2)*(1-Rel_Effects_Symptoms))
      SE<- round(SE, digits = 2)
      SE_Thanatophobia<- round(SE_Thanatophobia, digits = 2)
      SE_Disease_Phobia<- round(SE_Disease_Phobia, digits = 2)
      SE_Bodily_Preoccupations<- round(SE_Bodily_Preoccupations, digits = 2)
      SE_Treatment_Experience<- round(SE_Treatment_Experience, digits = 2)
      SE_Effects_Symptoms<- round(SE_Effects_Symptoms, digits = 2)
    } else if (input$RelChangeMethod == "McSweeny et al. (1993)") {
      SE<- SD_Retest*sqrt(1 - Rel^2)
      SE_Worry_Illness<- SD_Retest_Worry_Illness*sqrt(1 - Rel_Worry_Illness^2)
      SE_Concerns_Pain<- SD_Retest_Concerns_Pain*sqrt(1 - Rel_Concerns_Pain^2)
      SE_Health_Habits<- SD_Retest_Health_Habits*sqrt(1 - Rel_Health_Habits^2)
      SE_Hypochondriacal_Beliefs<- SD_Retest_Hypochondriacal_Beliefs*sqrt(1 - Rel_Hypochondriacal_Beliefs^2)
      SE<- round(SE, digits = 2)
      SE_Worry_Illness<- round(SE_Worry_Illness, digits = 2)
      SE_Concerns_Pain<- round(SE_Concerns_Pain, digits = 2)
      SE_Health_Habits<- round(SE_Health_Habits, digits = 2)
      SE_Hypochondriacal_Beliefs<- round(SE_Hypochondriacal_Beliefs, digits = 2)
      SE_Thanatophobia<- SD_Retest_Thanatophobia*sqrt(1 - Rel_Thanatophobia^2)
      SE_Disease_Phobia<- SD_Retest_Disease_Phobia*sqrt(1 - Rel_Disease_Phobia^2)
      SE_Bodily_Preoccupations<- SD_Retest_Bodily_Preoccupations*sqrt(1 - Rel_Bodily_Preoccupations^2)
      SE_Treatment_Experience<- SD_Retest_Treatment_Experience*sqrt(1 - Rel_Treatment_Experience^2)
      SE_Effects_Symptoms<- SD_Retest_Effects_Symptoms*sqrt(1 - Rel_Effects_Symptoms^2)
      SE<- round(SE, digits = 2)
      SE_Thanatophobia<- round(SE_Thanatophobia, digits = 2)
      SE_Disease_Phobia<- round(SE_Disease_Phobia, digits = 2)
      SE_Bodily_Preoccupations<- round(SE_Bodily_Preoccupations, digits = 2)
      SE_Treatment_Experience<- round(SE_Treatment_Experience, digits = 2)
      SE_Effects_Symptoms<- round(SE_Effects_Symptoms, digits = 2)
    }
    
    #Caclculate McSweeny standard error to be employed in deriving the Crawford & Howell standard error if selected
    
    McSweeny_SE<- SD_Retest*sqrt(1 - Rel^2)
    McSweeny_SE_Worry_Illness<- SD_Retest_Worry_Illness*sqrt(1 - Rel_Worry_Illness^2)
    McSweeny_SE_Concerns_Pain<- SD_Retest_Concerns_Pain*sqrt(1 - Rel_Concerns_Pain^2)
    McSweeny_SE_Health_Habits<- SD_Retest_Health_Habits*sqrt(1 - Rel_Health_Habits^2)
    McSweeny_SE_Hypochondriacal_Beliefs<- SD_Retest_Hypochondriacal_Beliefs*sqrt(1 - Rel_Hypochondriacal_Beliefs^2)
    McSweeny_SE_Thanatophobia<- SD_Retest_Thanatophobia*sqrt(1 - Rel_Thanatophobia^2)
    McSweeny_SE_Disease_Phobia<- SD_Retest_Disease_Phobia*sqrt(1 - Rel_Disease_Phobia^2)
    McSweeny_SE_Bodily_Preoccupations<- SD_Retest_Bodily_Preoccupations*sqrt(1 - Rel_Bodily_Preoccupations^2)
    McSweeny_SE_Treatment_Experience<- SD_Retest_Treatment_Experience*sqrt(1 - Rel_Treatment_Experience^2)
    McSweeny_SE_Effects_Symptoms<- SD_Retest_Effects_Symptoms*sqrt(1 - Rel_Effects_Symptoms^2)
    
    #Generate object storing names of cut-off scores
    
    Cutoff_Name_1<- input$Cutoff_Text_1
    Cutoff_Name_2<- input$Cutoff_Text_2
    Cutoff_Name_3<- input$Cutoff_Text_3
    Cutoff_Name_Worry_Illness_1<- input$Cutoff_Text_Worry_Illness_1
    Cutoff_Name_Worry_Illness_2<- input$Cutoff_Text_Worry_Illness_2
    Cutoff_Name_Worry_Illness_3<- input$Cutoff_Text_Worry_Illness_3
    Cutoff_Name_Concerns_Pain_1<- input$Cutoff_Text_Concerns_Pain_1
    Cutoff_Name_Concerns_Pain_2<- input$Cutoff_Text_Concerns_Pain_2
    Cutoff_Name_Concerns_Pain_3<- input$Cutoff_Text_Concerns_Pain_3
    Cutoff_Name_Health_Habits_1<- input$Cutoff_Text_Health_Habits_1
    Cutoff_Name_Health_Habits_2<- input$Cutoff_Text_Health_Habits_2
    Cutoff_Name_Health_Habits_3<- input$Cutoff_Text_Health_Habits_3
    Cutoff_Name_Hypochondriacal_Beliefs_1<- input$Cutoff_Text_Hypochondriacal_Beliefs_1
    Cutoff_Name_Hypochondriacal_Beliefs_2<- input$Cutoff_Text_Hypochondriacal_Beliefs_2
    Cutoff_Name_Hypochondriacal_Beliefs_3<- input$Cutoff_Text_Hypochondriacal_Beliefs_3
    Cutoff_Name_Thanatophobia_1<- input$Cutoff_Text_Thanatophobia_1
    Cutoff_Name_Thanatophobia_2<- input$Cutoff_Text_Thanatophobia_2
    Cutoff_Name_Thanatophobia_3<- input$Cutoff_Text_Thanatophobia_3
    Cutoff_Name_Disease_Phobia_1<- input$Cutoff_Text_Disease_Phobia_1
    Cutoff_Name_Disease_Phobia_2<- input$Cutoff_Text_Disease_Phobia_2
    Cutoff_Name_Disease_Phobia_3<- input$Cutoff_Text_Disease_Phobia_3
    Cutoff_Name_Bodily_Preoccupations_1<- input$Cutoff_Text_Bodily_Preoccupations_1
    Cutoff_Name_Bodily_Preoccupations_2<- input$Cutoff_Text_Bodily_Preoccupations_2
    Cutoff_Name_Bodily_Preoccupations_3<- input$Cutoff_Text_Bodily_Preoccupations_3
    Cutoff_Name_Treatment_Experience_1<- input$Cutoff_Text_Treatment_Experience_1
    Cutoff_Name_Treatment_Experience_2<- input$Cutoff_Text_Treatment_Experience_2
    Cutoff_Name_Treatment_Experience_3<- input$Cutoff_Text_Treatment_Experience_3
    Cutoff_Name_Effects_Symptoms_1<- input$Cutoff_Text_Effects_Symptoms_1
    Cutoff_Name_Effects_Symptoms_2<- input$Cutoff_Text_Effects_Symptoms_2
    Cutoff_Name_Effects_Symptoms_3<- input$Cutoff_Text_Effects_Symptoms_3
    
    
    Cutoff_Names<<- data.frame(Cutoff_Name_1,Cutoff_Name_2,Cutoff_Name_3,Cutoff_Name_Worry_Illness_1,Cutoff_Name_Worry_Illness_2,Cutoff_Name_Worry_Illness_3,
                               Cutoff_Name_Concerns_Pain_1, Cutoff_Name_Concerns_Pain_2, Cutoff_Name_Concerns_Pain_3, Cutoff_Name_Health_Habits_1,
                               Cutoff_Name_Health_Habits_2, Cutoff_Name_Health_Habits_3, Cutoff_Name_Hypochondriacal_Beliefs_1, Cutoff_Name_Hypochondriacal_Beliefs_2, Cutoff_Name_Hypochondriacal_Beliefs_3,
                               Cutoff_Name_Thanatophobia_1,Cutoff_Name_Thanatophobia_2,Cutoff_Name_Thanatophobia_3,
                               Cutoff_Name_Disease_Phobia_1, Cutoff_Name_Disease_Phobia_2, Cutoff_Name_Disease_Phobia_3, Cutoff_Name_Bodily_Preoccupations_1,
                               Cutoff_Name_Bodily_Preoccupations_2, Cutoff_Name_Bodily_Preoccupations_3, Cutoff_Name_Treatment_Experience_1, Cutoff_Name_Treatment_Experience_2, Cutoff_Name_Treatment_Experience_3, 
                               Cutoff_Name_Effects_Symptoms_1, Cutoff_Name_Effects_Symptoms_2, Cutoff_Name_Effects_Symptoms_3 
                               )
    
    #Define the data & calculate PS, SE, confidence intervals and cut-off scores
    
    if(input$Timepoint == "1") {
      Date<- input$Date_1
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_1a), Score = Score_1a)
      Score<- sum(Score_1a, na.rm = TRUE)
      Score<- round(Score, digits = 2)
      Score_Worry_Illness<- sum(Score_1a[c(1,2,3)], na.rm = TRUE)
      Score_Concerns_Pain<- sum(Score_1a[c(4,5,6)], na.rm = TRUE)
      Score_Health_Habits<- sum(Score_1a[c(7,8,9)], na.rm = TRUE)
      Score_Hypochondriacal_Beliefs<- sum(Score_1a[c(10,11,12)], na.rm = TRUE)
      Score_Thanatophobia<- sum(Score_1a[c(13,14,15)], na.rm = TRUE)
      Score_Disease_Phobia<- sum(Score_1a[c(16,17,18)], na.rm = TRUE)
      Score_Bodily_Preoccupations<- sum(Score_1a[c(19,20,21)], na.rm = TRUE)
      Score_Treatment_Experience<- sum(Score_1a[c(22,23,24)], na.rm = TRUE)
      Score_Effects_Symptoms<- sum(Score_1a[c(25,26,27)], na.rm = TRUE) 
      Change<- 0
      Change_Worry_Illness<- 0
      Change_Concerns_Pain<- 0
      Change_Health_Habits<- 0
      Change_Hypochondriacal_Beliefs<- 0
      Change_Thanatophobia<- 0
      Change_Disease_Phobia<- 0
      Change_Bodily_Preoccupations<- 0
      Change_Treatment_Experience<- 0
      Change_Effects_Symptoms<- 0
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS<- (Rel * Score) + (M * (1 - Rel))
        PTS_Worry_Illness<- (Rel_Worry_Illness * Score_Worry_Illness) + (M_Worry_Illness * (1 - Rel_Worry_Illness))
        PTS_Concerns_Pain<- (Rel_Concerns_Pain * Score_Concerns_Pain) + (M_Concerns_Pain * (1 - Rel_Concerns_Pain))
        PTS_Health_Habits<- (Rel_Health_Habits * Score_Health_Habits) + (M_Health_Habits * (1 - Rel_Health_Habits))
        PTS_Hypochondriacal_Beliefs<- (Rel_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs) + (M_Hypochondriacal_Beliefs * (1 - Rel_Hypochondriacal_Beliefs))
        PTS_Thanatophobia<- (Rel_Thanatophobia * Score_Thanatophobia) + (M_Thanatophobia * (1 - Rel_Thanatophobia))
        PTS_Disease_Phobia<- (Rel_Disease_Phobia * Score_Disease_Phobia) + (M_Disease_Phobia * (1 - Rel_Disease_Phobia))
        PTS_Bodily_Preoccupations<- (Rel_Bodily_Preoccupations * Score_Bodily_Preoccupations) + (M_Bodily_Preoccupations * (1 - Rel_Bodily_Preoccupations))
        PTS_Treatment_Experience<- (Rel_Treatment_Experience * Score_Treatment_Experience) + (M_Treatment_Experience * (1 - Rel_Treatment_Experience))
        PTS_Effects_Symptoms<- (Rel_Effects_Symptoms * Score_Effects_Symptoms) + (M_Effects_Symptoms * (1 - Rel_Effects_Symptoms))
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS<- Score + (M_Retest - M)  
        PTS_Worry_Illness<- Score_Worry_Illness + (M_Retest_Worry_Illness - M_Worry_Illness)  
        PTS_Concerns_Pain<- Score_Concerns_Pain + (M_Retest_Concerns_Pain - M_Concerns_Pain)  
        PTS_Health_Habits<- Score_Health_Habits + (M_Retest_Health_Habits - M_Health_Habits) 
        PTS_Hypochondriacal_Beliefs<- Score_Hypochondriacal_Beliefs + (M_Retest_Hypochondriacal_Beliefs - M_Hypochondriacal_Beliefs)
        PTS_Thanatophobia<- Score_Thanatophobia + (M_Retest_Thanatophobia - M_Thanatophobia)  
        PTS_Disease_Phobia<- Score_Disease_Phobia + (M_Retest_Disease_Phobia - M_Disease_Phobia)  
        PTS_Bodily_Preoccupations<- Score_Bodily_Preoccupations + (M_Retest_Bodily_Preoccupations - M_Bodily_Preoccupations) 
        PTS_Treatment_Experience<- Score_Treatment_Experience + (M_Retest_Treatment_Experience - M_Treatment_Experience) 
        PTS_Effects_Symptoms<- Score_Effects_Symptoms + (M_Retest_Effects_Symptoms - M_Effects_Symptoms) 
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Worry_Illness<- Score_Worry_Illness
        PTS_Concerns_Pain<- Score_Concerns_Pain
        PTS_Health_Habits<- Score_Health_Habits
        PTS_Hypochondriacal_Beliefs<- Score_Hypochondriacal_Beliefs
        PTS_Thanatophobia<- Score_Thanatophobia
        PTS_Disease_Phobia<- Score_Disease_Phobia
        PTS_Bodily_Preoccupations<- Score_Bodily_Preoccupations
        PTS_Treatment_Experience<- Score_Treatment_Experience
        PTS_Effects_Symptoms<- Score_Effects_Symptoms
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS<- (B_Adj * Score) + A_Adj
        B_Slope_Worry_Illness<- Rel_Worry_Illness * (SD_Retest_Worry_Illness/SD_Worry_Illness)
        A_Constant_Worry_Illness<- M_Retest_Worry_Illness - (B_Slope_Worry_Illness * M_Worry_Illness)
        B_Adj_Worry_Illness<- SD_Retest_Worry_Illness/SD_Worry_Illness
        A_Adj_Worry_Illness<- M_Retest_Worry_Illness - (B_Adj_Worry_Illness * M_Worry_Illness)
        PTS_Worry_Illness<- (B_Adj_Worry_Illness * Score_Worry_Illness) + A_Adj_Worry_Illness
        B_Slope_Concerns_Pain<- Rel_Concerns_Pain * (SD_Retest_Concerns_Pain/SD_Concerns_Pain)
        A_Constant_Concerns_Pain<- M_Retest_Concerns_Pain - (B_Slope_Concerns_Pain * M_Concerns_Pain)
        B_Adj_Concerns_Pain<- SD_Retest_Concerns_Pain/SD_Concerns_Pain
        A_Adj_Concerns_Pain<- M_Retest_Concerns_Pain - (B_Adj_Concerns_Pain * M_Concerns_Pain)
        PTS_Concerns_Pain<- (B_Adj_Concerns_Pain * Score_Concerns_Pain) + A_Adj_Concerns_Pain
        B_Slope_Health_Habits<- Rel_Health_Habits * (SD_Retest_Health_Habits/SD_Health_Habits)
        A_Constant_Health_Habits<- M_Retest_Health_Habits - (B_Slope_Health_Habits * M_Health_Habits)
        B_Adj_Health_Habits<- SD_Retest_Health_Habits/SD_Health_Habits
        A_Adj_Health_Habits<- M_Retest_Health_Habits - (B_Adj_Health_Habits * M_Health_Habits)
        PTS_Health_Habits<- (B_Adj_Health_Habits * Score_Health_Habits) + A_Adj_Health_Habits
        B_Slope_Hypochondriacal_Beliefs<- Rel_Hypochondriacal_Beliefs * (SD_Retest_Hypochondriacal_Beliefs/SD_Hypochondriacal_Beliefs)
        A_Constant_Hypochondriacal_Beliefs<- M_Retest_Hypochondriacal_Beliefs - (B_Slope_Hypochondriacal_Beliefs * M_Hypochondriacal_Beliefs)
        B_Adj_Hypochondriacal_Beliefs<- SD_Retest_Hypochondriacal_Beliefs/SD_Hypochondriacal_Beliefs
        A_Adj_Hypochondriacal_Beliefs<- M_Retest_Hypochondriacal_Beliefs - (B_Adj_Hypochondriacal_Beliefs * M_Hypochondriacal_Beliefs)
        PTS_Hypochondriacal_Beliefs<- (B_Adj_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs) + A_Adj_Hypochondriacal_Beliefs
        B_Slope_Thanatophobia<- Rel_Thanatophobia * (SD_Retest_Thanatophobia/SD_Thanatophobia)
        A_Constant_Thanatophobia<- M_Retest_Thanatophobia - (B_Slope_Thanatophobia * M_Thanatophobia)
        B_Adj_Thanatophobia<- SD_Retest_Thanatophobia/SD_Thanatophobia
        A_Adj_Thanatophobia<- M_Retest_Thanatophobia - (B_Adj_Thanatophobia * M_Thanatophobia)
        PTS_Thanatophobia<- (B_Adj_Thanatophobia * Score_Thanatophobia) + A_Adj_Thanatophobia
        B_Slope_Disease_Phobia<- Rel_Disease_Phobia * (SD_Retest_Disease_Phobia/SD_Disease_Phobia)
        A_Constant_Disease_Phobia<- M_Retest_Disease_Phobia - (B_Slope_Disease_Phobia * M_Disease_Phobia)
        B_Adj_Disease_Phobia<- SD_Retest_Disease_Phobia/SD_Disease_Phobia
        A_Adj_Disease_Phobia<- M_Retest_Disease_Phobia - (B_Adj_Disease_Phobia * M_Disease_Phobia)
        PTS_Disease_Phobia<- (B_Adj_Disease_Phobia * Score_Disease_Phobia) + A_Adj_Disease_Phobia
        B_Slope_Bodily_Preoccupations<- Rel_Bodily_Preoccupations * (SD_Retest_Bodily_Preoccupations/SD_Bodily_Preoccupations)
        A_Constant_Bodily_Preoccupations<- M_Retest_Bodily_Preoccupations - (B_Slope_Bodily_Preoccupations * M_Bodily_Preoccupations)
        B_Adj_Bodily_Preoccupations<- SD_Retest_Bodily_Preoccupations/SD_Bodily_Preoccupations
        A_Adj_Bodily_Preoccupations<- M_Retest_Bodily_Preoccupations - (B_Adj_Bodily_Preoccupations * M_Bodily_Preoccupations)
        PTS_Bodily_Preoccupations<- (B_Adj_Bodily_Preoccupations * Score_Bodily_Preoccupations) + A_Adj_Bodily_Preoccupations
        B_Slope_Treatment_Experience<- Rel_Treatment_Experience * (SD_Retest_Treatment_Experience/SD_Treatment_Experience)
        A_Constant_Treatment_Experience<- M_Retest_Treatment_Experience - (B_Slope_Treatment_Experience * M_Treatment_Experience)
        B_Adj_Treatment_Experience<- SD_Retest_Treatment_Experience/SD_Treatment_Experience
        A_Adj_Treatment_Experience<- M_Retest_Treatment_Experience - (B_Adj_Treatment_Experience * M_Treatment_Experience)
        PTS_Treatment_Experience<- (B_Adj_Treatment_Experience * Score_Treatment_Experience) + A_Adj_Treatment_Experience
        B_Slope_Effects_Symptoms<- Rel_Effects_Symptoms * (SD_Retest_Effects_Symptoms/SD_Effects_Symptoms)
        A_Constant_Effects_Symptoms<- M_Retest_Effects_Symptoms - (B_Slope_Effects_Symptoms * M_Effects_Symptoms)
        B_Adj_Effects_Symptoms<- SD_Retest_Effects_Symptoms/SD_Effects_Symptoms
        A_Adj_Effects_Symptoms<- M_Retest_Effects_Symptoms - (B_Adj_Effects_Symptoms * M_Effects_Symptoms)
        PTS_Effects_Symptoms<- (B_Adj_Effects_Symptoms * Score_Effects_Symptoms) + A_Adj_Effects_Symptoms
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS<- B_Slope * Score
        B_Slope_Worry_Illness<- Rel_Worry_Illness * (SD_Retest_Worry_Illness/SD_Worry_Illness)
        PTS_Worry_Illness<- B_Slope_Worry_Illness * Score_Worry_Illness
        B_Slope_Concerns_Pain<- Rel_Concerns_Pain * (SD_Retest_Concerns_Pain/SD_Concerns_Pain)
        PTS_Concerns_Pain<- B_Slope_Concerns_Pain * Score_Concerns_Pain
        B_Slope_Health_Habits<- Rel_Health_Habits * (SD_Retest_Health_Habits/SD_Health_Habits)
        PTS_Health_Habits<- B_Slope_Health_Habits * Score_Health_Habits
        B_Slope_Hypochondriacal_Beliefs<- Rel_Hypochondriacal_Beliefs * (SD_Retest_Hypochondriacal_Beliefs/SD_Hypochondriacal_Beliefs)
        PTS_Hypochondriacal_Beliefs<- B_Slope_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs
        B_Slope_Thanatophobia<- Rel_Thanatophobia * (SD_Retest_Thanatophobia/SD_Thanatophobia)
        PTS_Thanatophobia<- B_Slope_Thanatophobia * Score_Thanatophobia
        B_Slope_Disease_Phobia<- Rel_Disease_Phobia * (SD_Retest_Disease_Phobia/SD_Disease_Phobia)
        PTS_Disease_Phobia<- B_Slope_Disease_Phobia * Score_Disease_Phobia
        B_Slope_Bodily_Preoccupations<- Rel_Bodily_Preoccupations * (SD_Retest_Bodily_Preoccupations/SD_Bodily_Preoccupations)
        PTS_Bodily_Preoccupations<- B_Slope_Bodily_Preoccupations * Score_Bodily_Preoccupations
        B_Slope_Treatment_Experience<- Rel_Treatment_Experience * (SD_Retest_Treatment_Experience/SD_Treatment_Experience)
        PTS_Treatment_Experience<- B_Slope_Treatment_Experience * Score_Treatment_Experience 
        B_Slope_Effects_Symptoms<- Rel_Effects_Symptoms * (SD_Retest_Effects_Symptoms/SD_Effects_Symptoms)
        PTS_Effects_Symptoms<- B_Slope_Effects_Symptoms * Score_Effects_Symptoms
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS<- Score + (M_Retest - M)
        PTS_Worry_Illness<- Score_Worry_Illness + (M_Retest_Worry_Illness - M_Worry_Illness)
        PTS_Concerns_Pain<- Score_Concerns_Pain + (M_Retest_Concerns_Pain - M_Concerns_Pain)
        PTS_Health_Habits<- Score_Health_Habits + (M_Retest_Health_Habits - M_Health_Habits)
        PTS_Hypochondriacal_Beliefs<- Score_Hypochondriacal_Beliefs + (M_Retest_Hypochondriacal_Beliefs - M_Hypochondriacal_Beliefs)
        PTS_Thanatophobia<- Score_Thanatophobia + (M_Retest_Thanatophobia - M_Thanatophobia)
        PTS_Disease_Phobia<- Score_Disease_Phobia + (M_Retest_Disease_Phobia - M_Disease_Phobia)
        PTS_Bodily_Preoccupations<- Score_Bodily_Preoccupations + (M_Retest_Bodily_Preoccupations - M_Bodily_Preoccupations)
        PTS_Treatment_Experience<- Score_Treatment_Experience + (M_Retest_Treatment_Experience - M_Treatment_Experience)
        PTS_Effects_Symptoms<- Score_Effects_Symptoms + (M_Retest_Effects_Symptoms - M_Effects_Symptoms)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Worry_Illness<- round(PTS_Worry_Illness, digits = 2)
      PTS_Concerns_Pain<- round(PTS_Concerns_Pain, digits = 2)
      PTS_Health_Habits<- round(PTS_Health_Habits, digits = 2)
      PTS_Hypochondriacal_Beliefs<- round(PTS_Hypochondriacal_Beliefs, digits = 2)
      PTS_Thanatophobia<- round(PTS_Thanatophobia, digits = 2)
      PTS_Disease_Phobia<- round(PTS_Disease_Phobia, digits = 2)
      PTS_Bodily_Preoccupations<- round(PTS_Bodily_Preoccupations, digits = 2)
      PTS_Treatment_Experience<- round(PTS_Treatment_Experience, digits = 2)
      PTS_Effects_Symptoms<- round(PTS_Effects_Symptoms, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score - M)^2/(SD^2*(SampleN-1))))
        SE_Worry_Illness<- McSweeny_SE_Worry_Illness*sqrt(1 + (1/SampleN) + ((Score_Worry_Illness - M_Worry_Illness)^2/(SD_Worry_Illness^2*(SampleN-1))))
        SE_Concerns_Pain<- McSweeny_SE_Concerns_Pain*sqrt(1 + (1/SampleN) + ((Score_Concerns_Pain - M_Concerns_Pain)^2/(SD_Concerns_Pain^2*(SampleN-1))))
        SE_Health_Habits<- McSweeny_SE_Health_Habits*sqrt(1 + (1/SampleN) + ((Score_Health_Habits - M_Health_Habits)^2/(SD_Health_Habits^2*(SampleN-1))))
        SE_Hypochondriacal_Beliefs<- McSweeny_SE_Hypochondriacal_Beliefs*sqrt(1 + (1/SampleN) + ((Score_Hypochondriacal_Beliefs - M_Hypochondriacal_Beliefs)^2/(SD_Hypochondriacal_Beliefs^2*(SampleN-1))))
        SE_Thanatophobia<- McSweeny_SE_Thanatophobia*sqrt(1 + (1/SampleN) + ((Score_Thanatophobia - M_Thanatophobia)^2/(SD_Thanatophobia^2*(SampleN-1))))
        SE_Disease_Phobia<- McSweeny_SE_Disease_Phobia*sqrt(1 + (1/SampleN) + ((Score_Disease_Phobia - M_Disease_Phobia)^2/(SD_Disease_Phobia^2*(SampleN-1))))
        SE_Bodily_Preoccupations<- McSweeny_SE_Bodily_Preoccupations*sqrt(1 + (1/SampleN) + ((Score_Bodily_Preoccupations - M_Bodily_Preoccupations)^2/(SD_Bodily_Preoccupations^2*(SampleN-1))))
        SE_Treatment_Experience<- McSweeny_SE_Treatment_Experience*sqrt(1 + (1/SampleN) + ((Score_Treatment_Experience - M_Treatment_Experience)^2/(SD_Treatment_Experience^2*(SampleN-1))))
        SE_Effects_Symptoms<- McSweeny_SE_Effects_Symptoms*sqrt(1 + (1/SampleN) + ((Score_Effects_Symptoms - M_Effects_Symptoms)^2/(SD_Effects_Symptoms^2*(SampleN-1))))
        SE<- round(SE, digits = 2)
        SE_Worry_Illness<- round(SE_Worry_Illness, digits = 2)
        SE_Concerns_Pain<- round(SE_Concerns_Pain, digits = 2)
        SE_Health_Habits<- round(SE_Health_Habits, digits = 2)
        SE_Hypochondriacal_Beliefs<- round(SE_Hypochondriacal_Beliefs, digits = 2)
        SE_Thanatophobia<- round(SE_Thanatophobia, digits = 2)
        SE_Disease_Phobia<- round(SE_Disease_Phobia, digits = 2)
        SE_Bodily_Preoccupations<- round(SE_Bodily_Preoccupations, digits = 2)
        SE_Treatment_Experience<- round(SE_Treatment_Experience, digits = 2)
        SE_Effects_Symptoms<- round(SE_Effects_Symptoms, digits = 2)
        CI<- (Conf*SE)
        CI<- round(CI, digits = 2)
        CI_Worry_Illness<- (Conf*SE_Worry_Illness)
        CI_Worry_Illness<- round(CI_Worry_Illness, digits = 2)
        CI_Concerns_Pain<- (Conf*SE_Concerns_Pain)
        CI_Concerns_Pain<- round(CI_Concerns_Pain, digits = 2)
        CI_Health_Habits<- (Conf*SE_Health_Habits)
        CI_Health_Habits<- round(CI_Health_Habits, digits = 2)
        CI_Hypochondriacal_Beliefs<- (Conf*SE_Hypochondriacal_Beliefs)
        CI_Hypochondriacal_Beliefs<- round(CI_Hypochondriacal_Beliefs, digits = 2)
        CI_Thanatophobia<- (Conf*SE_Thanatophobia)
        CI_Thanatophobia<- round(CI_Thanatophobia, digits = 2)
        CI_Disease_Phobia<- (Conf*SE_Disease_Phobia)
        CI_Disease_Phobia<- round(CI_Disease_Phobia, digits = 2)
        CI_Bodily_Preoccupations<- (Conf*SE_Bodily_Preoccupations)
        CI_Bodily_Preoccupations<- round(CI_Bodily_Preoccupations, digits = 2)
        CI_Treatment_Experience<- (Conf*SE_Treatment_Experience)
        CI_Treatment_Experience<- round(CI_Treatment_Experience, digits = 2)
        CI_Effects_Symptoms<- (Conf*SE_Effects_Symptoms)
        CI_Effects_Symptoms<- round(CI_Effects_Symptoms, digits = 2)
      }
      CI<- (Conf*SE)
      CI<- round(CI, digits = 2)
      CI<- round(CI, digits = 2)
      CI_Worry_Illness<- (Conf*SE_Worry_Illness)
      CI_Worry_Illness<- round(CI_Worry_Illness, digits = 2)
      CI_Concerns_Pain<- (Conf*SE_Concerns_Pain)
      CI_Concerns_Pain<- round(CI_Concerns_Pain, digits = 2)
      CI_Health_Habits<- (Conf*SE_Health_Habits)
      CI_Health_Habits<- round(CI_Health_Habits, digits = 2)
      CI_Hypochondriacal_Beliefs<- (Conf*SE_Hypochondriacal_Beliefs)
      CI_Hypochondriacal_Beliefs<- round(CI_Hypochondriacal_Beliefs, digits = 2)
      CI_Thanatophobia<- (Conf*SE_Thanatophobia)
      CI_Thanatophobia<- round(CI_Thanatophobia, digits = 2)
      CI_Disease_Phobia<- (Conf*SE_Disease_Phobia)
      CI_Disease_Phobia<- round(CI_Disease_Phobia, digits = 2)
      CI_Bodily_Preoccupations<- (Conf*SE_Bodily_Preoccupations)
      CI_Bodily_Preoccupations<- round(CI_Bodily_Preoccupations, digits = 2)
      CI_Treatment_Experience<- (Conf*SE_Treatment_Experience)
      CI_Treatment_Experience<- round(CI_Treatment_Experience, digits = 2)
      CI_Effects_Symptoms<- (Conf*SE_Effects_Symptoms)
      CI_Effects_Symptoms<- round(CI_Effects_Symptoms, digits = 2)
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Worry_Illness<- PTS_Worry_Illness + CI_Worry_Illness
      CI_Upper_Lim_Worry_Illness<- round(CI_Upper_Lim_Worry_Illness, digits = 2)
      CI_Lower_Lim_Worry_Illness<-PTS_Worry_Illness - CI_Worry_Illness
      CI_Lower_Lim_Worry_Illness<- round(CI_Lower_Lim_Worry_Illness, digits = 2)
      CI_Upper_Lim_Concerns_Pain<- PTS_Concerns_Pain + CI_Concerns_Pain
      CI_Upper_Lim_Concerns_Pain<- round(CI_Upper_Lim_Concerns_Pain, digits = 2)
      CI_Lower_Lim_Concerns_Pain<-PTS_Concerns_Pain - CI_Concerns_Pain
      CI_Lower_Lim_Concerns_Pain<- round(CI_Lower_Lim_Concerns_Pain, digits = 2)
      CI_Upper_Lim_Health_Habits<- PTS_Health_Habits + CI_Health_Habits
      CI_Upper_Lim_Health_Habits<- round(CI_Upper_Lim_Health_Habits, digits = 2)
      CI_Lower_Lim_Health_Habits<-PTS_Health_Habits - CI_Health_Habits
      CI_Lower_Lim_Health_Habits<- round(CI_Lower_Lim_Health_Habits, digits = 2)
      CI_Upper_Lim_Hypochondriacal_Beliefs<- PTS_Hypochondriacal_Beliefs + CI_Hypochondriacal_Beliefs
      CI_Upper_Lim_Hypochondriacal_Beliefs<- round(CI_Upper_Lim_Hypochondriacal_Beliefs, digits = 2)
      CI_Lower_Lim_Hypochondriacal_Beliefs<-PTS_Hypochondriacal_Beliefs - CI_Hypochondriacal_Beliefs
      CI_Lower_Lim_Hypochondriacal_Beliefs<- round(CI_Lower_Lim_Hypochondriacal_Beliefs, digits = 2)
      CI_Upper_Lim_Thanatophobia<- PTS_Thanatophobia + CI_Thanatophobia
      CI_Upper_Lim_Thanatophobia<- round(CI_Upper_Lim_Thanatophobia, digits = 2)
      CI_Lower_Lim_Thanatophobia<-PTS_Thanatophobia - CI_Thanatophobia
      CI_Lower_Lim_Thanatophobia<- round(CI_Lower_Lim_Thanatophobia, digits = 2)
      CI_Upper_Lim_Disease_Phobia<- PTS_Disease_Phobia + CI_Disease_Phobia
      CI_Upper_Lim_Disease_Phobia<- round(CI_Upper_Lim_Disease_Phobia, digits = 2)
      CI_Lower_Lim_Disease_Phobia<- PTS_Disease_Phobia - CI_Disease_Phobia
      CI_Lower_Lim_Disease_Phobia<- round(CI_Lower_Lim_Disease_Phobia, digits = 2)
      CI_Upper_Lim_Bodily_Preoccupations<- PTS_Bodily_Preoccupations + CI_Bodily_Preoccupations
      CI_Upper_Lim_Bodily_Preoccupations<- round(CI_Upper_Lim_Bodily_Preoccupations, digits = 2)
      CI_Lower_Lim_Bodily_Preoccupations<-PTS_Bodily_Preoccupations - CI_Bodily_Preoccupations
      CI_Lower_Lim_Bodily_Preoccupations<- round(CI_Lower_Lim_Bodily_Preoccupations, digits = 2)
      CI_Upper_Lim_Treatment_Experience<- PTS_Treatment_Experience + CI_Treatment_Experience
      CI_Upper_Lim_Treatment_Experience<- round(CI_Upper_Lim_Treatment_Experience, digits = 2)
      CI_Lower_Lim_Treatment_Experience<-PTS_Treatment_Experience - CI_Treatment_Experience
      CI_Lower_Lim_Treatment_Experience<- round(CI_Lower_Lim_Treatment_Experience, digits = 2)
      CI_Upper_Lim_Effects_Symptoms<- PTS_Effects_Symptoms + CI_Effects_Symptoms
      CI_Upper_Lim_Effects_Symptoms<- round(CI_Upper_Lim_Effects_Symptoms, digits = 2)
      CI_Lower_Lim_Effects_Symptoms<-PTS_Effects_Symptoms - CI_Effects_Symptoms
      CI_Lower_Lim_Effects_Symptoms<- round(CI_Lower_Lim_Effects_Symptoms, digits = 2)
      
      if(input$Select_CI == "2") {
        CI<- input$Man_CI
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Worry_Illness == "2") {
        CI_Worry_Illness<- input$Man_CI_Worry_Illness
        CI_Worry_Illness<- round(CI_Worry_Illness, digits = 2)
        CI_Upper_Lim_Worry_Illness<- Score_Worry_Illness + CI_Worry_Illness
        CI_Upper_Lim_Worry_Illness<- round(CI_Upper_Lim_Worry_Illness, digits = 2)
        CI_Lower_Lim_Worry_Illness<- Score_Worry_Illness - CI_Worry_Illness
        CI_Lower_Lim_Worry_Illness<- round(CI_Lower_Lim_Worry_Illness, digits = 2)
      }
      if(input$Select_CI_Concerns_Pain == "2") {
        CI_Concerns_Pain<- input$Man_CI_Concerns_Pain
        CI_Concerns_Pain<- round(CI_Concerns_Pain, digits = 2)
        CI_Upper_Lim_Concerns_Pain<- Score_Concerns_Pain + CI_Concerns_Pain
        CI_Upper_Lim_Concerns_Pain<- round(CI_Upper_Lim_Concerns_Pain, digits = 2)
        CI_Lower_Lim_Concerns_Pain<- Score_Concerns_Pain - CI_Concerns_Pain
        CI_Lower_Lim_Concerns_Pain<- round(CI_Lower_Lim_Concerns_Pain, digits = 2)
      }
      if(input$Select_CI_Health_Habits == "2") {
        CI_Health_Habits<- input$Man_CI_Health_Habits
        CI_Health_Habits<- round(CI_Health_Habits, digits = 2)
        CI_Upper_Lim_Health_Habits<- Score_Health_Habits + CI_Health_Habits
        CI_Upper_Lim_Health_Habits<- round(CI_Upper_Lim_Health_Habits, digits = 2)
        CI_Lower_Lim_Health_Habits<- Score_Health_Habits - CI_Health_Habits
        CI_Lower_Lim_Health_Habits<- round(CI_Lower_Lim_Health_Habits, digits = 2)
      }
      if(input$Select_CI_Hypochondriacal_Beliefs == "2") {
        CI_Hypochondriacal_Beliefs<- input$Man_CI_Hypochondriacal_Beliefs
        CI_Hypochondriacal_Beliefs<- round(CI_Hypochondriacal_Beliefs, digits = 2)
        CI_Upper_Lim_Hypochondriacal_Beliefs<- Score_Hypochondriacal_Beliefs + CI_Hypochondriacal_Beliefs
        CI_Upper_Lim_Hypochondriacal_Beliefs<- round(CI_Upper_Lim_Hypochondriacal_Beliefs, digits = 2)
        CI_Lower_Lim_Hypochondriacal_Beliefs<- Score_Hypochondriacal_Beliefs - CI_Hypochondriacal_Beliefs
        CI_Lower_Lim_Hypochondriacal_Beliefs<- round(CI_Lower_Lim_Hypochondriacal_Beliefs, digits = 2)
      }
      if(input$Select_CI_Thanatophobia == "2") {
        CI_Thanatophobia<- input$Man_CI_Thanatophobia
        CI_Thanatophobia<- round(CI_Thanatophobia, digits = 2)
        CI_Upper_Lim_Thanatophobia<- Score_Thanatophobia + CI_Thanatophobia
        CI_Upper_Lim_Thanatophobia<- round(CI_Upper_Lim_Thanatophobia, digits = 2)
        CI_Lower_Lim_Thanatophobia<- Score_Thanatophobia - CI_Thanatophobia
        CI_Lower_Lim_Thanatophobia<- round(CI_Lower_Lim_Thanatophobia, digits = 2)
      }
      if(input$Select_CI_Disease_Phobia == "2") {
        CI_Disease_Phobia<- input$Man_CI_Disease_Phobia
        CI_Disease_Phobia<- round(CI_Disease_Phobia, digits = 2)
        CI_Upper_Lim_Disease_Phobia<- Score_Disease_Phobia + CI_Disease_Phobia
        CI_Upper_Lim_Disease_Phobia<- round(CI_Upper_Lim_Disease_Phobia, digits = 2)
        CI_Lower_Lim_Disease_Phobia<- Score_Disease_Phobia - CI_Disease_Phobia
        CI_Lower_Lim_Disease_Phobia<- round(CI_Lower_Lim_Disease_Phobia, digits = 2)
      }
      if(input$Select_CI_Bodily_Preoccupations == "2") {
        CI_Bodily_Preoccupations<- input$Man_CI_Bodily_Preoccupations
        CI_Bodily_Preoccupations<- round(CI_Bodily_Preoccupations, digits = 2)
        CI_Upper_Lim_Bodily_Preoccupations<- Score_Bodily_Preoccupations + CI_Bodily_Preoccupations
        CI_Upper_Lim_Bodily_Preoccupations<- round(CI_Upper_Lim_Bodily_Preoccupations, digits = 2)
        CI_Lower_Lim_Bodily_Preoccupations<- Score_Bodily_Preoccupations - CI_Bodily_Preoccupations
        CI_Lower_Lim_Bodily_Preoccupations<- round(CI_Lower_Lim_Bodily_Preoccupations, digits = 2)
      }
      if(input$Select_CI_Treatment_Experience == "2") {
        CI_Treatment_Experience<- input$Man_CI_Treatment_Experience
        CI_Treatment_Experience<- round(CI_Treatment_Experience, digits = 2)
        CI_Upper_Lim_Treatment_Experience<- Score_Treatment_Experience + CI_Treatment_Experience
        CI_Upper_Lim_Treatment_Experience<- round(CI_Upper_Lim_Treatment_Experience, digits = 2)
        CI_Lower_Lim_Treatment_Experience<- Score_Treatment_Experience - CI_Treatment_Experience
        CI_Lower_Lim_Treatment_Experience<- round(CI_Lower_Lim_Treatment_Experience, digits = 2)
      }
      if(input$Select_CI_Effects_Symptoms == "2") {
        CI_Effects_Symptoms<- input$Man_CI_Effects_Symptoms
        CI_Effects_Symptoms<- round(CI_Effects_Symptoms, digits = 2)
        CI_Upper_Lim_Effects_Symptoms<- Score_Effects_Symptoms + CI_Effects_Symptoms
        CI_Upper_Lim_Effects_Symptoms<- round(CI_Upper_Lim_Effects_Symptoms, digits = 2)
        CI_Lower_Lim_Effects_Symptoms<- Score_Effects_Symptoms - CI_Effects_Symptoms
        CI_Lower_Lim_Effects_Symptoms<- round(CI_Lower_Lim_Effects_Symptoms, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Worry_Illness_1<- round(input$Cutoff_Worry_Illness_1, digits = 2)
      Cutoff_Score_Worry_Illness_2<- round(input$Cutoff_Worry_Illness_2, digits = 2)
      Cutoff_Score_Worry_Illness_3<- round(input$Cutoff_Worry_Illness_3, digits = 2)
      Cutoff_Score_Concerns_Pain_1<- round(input$Cutoff_Concerns_Pain_1, digits = 2)
      Cutoff_Score_Concerns_Pain_2<- round(input$Cutoff_Concerns_Pain_2, digits = 2)
      Cutoff_Score_Concerns_Pain_3<- round(input$Cutoff_Concerns_Pain_3, digits = 2)
      Cutoff_Score_Health_Habits_1<- round(input$Cutoff_Health_Habits_1, digits = 2)
      Cutoff_Score_Health_Habits_2<- round(input$Cutoff_Health_Habits_2, digits = 2)
      Cutoff_Score_Health_Habits_3<- round(input$Cutoff_Health_Habits_3, digits = 2)
      Cutoff_Score_Hypochondriacal_Beliefs_1<- round(input$Cutoff_Hypochondriacal_Beliefs_1, digits = 2)
      Cutoff_Score_Hypochondriacal_Beliefs_2<- round(input$Cutoff_Hypochondriacal_Beliefs_2, digits = 2)
      Cutoff_Score_Hypochondriacal_Beliefs_3<- round(input$Cutoff_Hypochondriacal_Beliefs_3, digits = 2)
      Cutoff_Score_Thanatophobia_1<- round(input$Cutoff_Thanatophobia_1, digits = 2)
      Cutoff_Score_Thanatophobia_2<- round(input$Cutoff_Thanatophobia_2, digits = 2)
      Cutoff_Score_Thanatophobia_3<- round(input$Cutoff_Thanatophobia_3, digits = 2)
      Cutoff_Score_Disease_Phobia_1<- round(input$Cutoff_Disease_Phobia_1, digits = 2)
      Cutoff_Score_Disease_Phobia_2<- round(input$Cutoff_Disease_Phobia_2, digits = 2)
      Cutoff_Score_Disease_Phobia_3<- round(input$Cutoff_Disease_Phobia_3, digits = 2)
      Cutoff_Score_Bodily_Preoccupations_1<- round(input$Cutoff_Bodily_Preoccupations_1, digits = 2)
      Cutoff_Score_Bodily_Preoccupations_2<- round(input$Cutoff_Bodily_Preoccupations_2, digits = 2)
      Cutoff_Score_Bodily_Preoccupations_3<- round(input$Cutoff_Bodily_Preoccupations_3, digits = 2)
      Cutoff_Score_Treatment_Experience_1<- round(input$Cutoff_Treatment_Experience_1, digits = 2)
      Cutoff_Score_Treatment_Experience_2<- round(input$Cutoff_Treatment_Experience_2, digits = 2)
      Cutoff_Score_Treatment_Experience_3<- round(input$Cutoff_Treatment_Experience_3, digits = 2)
      Cutoff_Score_Effects_Symptoms_1<- round(input$Cutoff_Effects_Symptoms_1, digits = 2)
      Cutoff_Score_Effects_Symptoms_2<- round(input$Cutoff_Effects_Symptoms_2, digits = 2)
      Cutoff_Score_Effects_Symptoms_3<- round(input$Cutoff_Effects_Symptoms_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Worry_Illness,Change_Worry_Illness,PTS_Worry_Illness, SE_Worry_Illness, CI_Upper_Lim_Worry_Illness, CI_Lower_Lim_Worry_Illness, Cutoff_Score_Worry_Illness_1,Cutoff_Score_Worry_Illness_2,Cutoff_Score_Worry_Illness_3,
                                      Score_Concerns_Pain,Change_Concerns_Pain, PTS_Concerns_Pain, SE_Concerns_Pain, CI_Upper_Lim_Concerns_Pain, CI_Lower_Lim_Concerns_Pain, Cutoff_Score_Concerns_Pain_1,Cutoff_Score_Concerns_Pain_2,Cutoff_Score_Concerns_Pain_3, 
                                      Score_Health_Habits,Change_Health_Habits,PTS_Health_Habits, SE_Health_Habits, CI_Upper_Lim_Health_Habits, CI_Lower_Lim_Health_Habits, Cutoff_Score_Health_Habits_1,Cutoff_Score_Health_Habits_2,Cutoff_Score_Health_Habits_3, 
                                      Score_Hypochondriacal_Beliefs,Change_Hypochondriacal_Beliefs,PTS_Hypochondriacal_Beliefs, SE_Hypochondriacal_Beliefs, CI_Upper_Lim_Hypochondriacal_Beliefs, CI_Lower_Lim_Hypochondriacal_Beliefs, Cutoff_Score_Hypochondriacal_Beliefs_1,Cutoff_Score_Hypochondriacal_Beliefs_2,Cutoff_Score_Hypochondriacal_Beliefs_3, 
                                      Score_Thanatophobia,Change_Thanatophobia,PTS_Thanatophobia, SE_Thanatophobia, CI_Upper_Lim_Thanatophobia, CI_Lower_Lim_Thanatophobia, Cutoff_Score_Thanatophobia_1,Cutoff_Score_Thanatophobia_2,Cutoff_Score_Thanatophobia_3,
                                      Score_Disease_Phobia,Change_Disease_Phobia, PTS_Disease_Phobia, SE_Disease_Phobia, CI_Upper_Lim_Disease_Phobia, CI_Lower_Lim_Disease_Phobia, Cutoff_Score_Disease_Phobia_1,Cutoff_Score_Disease_Phobia_2,Cutoff_Score_Disease_Phobia_3, 
                                      Score_Bodily_Preoccupations,Change_Bodily_Preoccupations,PTS_Bodily_Preoccupations, SE_Bodily_Preoccupations, CI_Upper_Lim_Bodily_Preoccupations, CI_Lower_Lim_Bodily_Preoccupations, Cutoff_Score_Bodily_Preoccupations_1,Cutoff_Score_Bodily_Preoccupations_2,Cutoff_Score_Bodily_Preoccupations_3, 
                                      Score_Treatment_Experience,Change_Treatment_Experience,PTS_Treatment_Experience, SE_Treatment_Experience, CI_Upper_Lim_Treatment_Experience, CI_Lower_Lim_Treatment_Experience, Cutoff_Score_Treatment_Experience_1,Cutoff_Score_Treatment_Experience_2,Cutoff_Score_Treatment_Experience_3, 
                                      Score_Effects_Symptoms,Change_Effects_Symptoms,PTS_Effects_Symptoms, SE_Effects_Symptoms, CI_Upper_Lim_Effects_Symptoms, CI_Lower_Lim_Effects_Symptoms, Cutoff_Score_Effects_Symptoms_1,Cutoff_Score_Effects_Symptoms_2,Cutoff_Score_Effects_Symptoms_3)
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
      Score_Worry_Illness_1<- sum(Score_1a[c(1,2,3)], na.rm = TRUE)
      Score_Worry_Illness_2<- sum(Score_2a[c(1,2,3)], na.rm = TRUE)
      Score_Worry_Illness<- c(Score_Worry_Illness_1, Score_Worry_Illness_2)
      Score_Worry_Illness<- round(Score_Worry_Illness, digits = 2)
      Score_Concerns_Pain_1<- sum(Score_1a[c(4,5,6)], na.rm = TRUE)
      Score_Concerns_Pain_2<- sum(Score_2a[c(4,5,6)], na.rm = TRUE)
      Score_Concerns_Pain<- c(Score_Concerns_Pain_1,Score_Concerns_Pain_2)
      Score_Concerns_Pain<- round(Score_Concerns_Pain, digits = 2)
      Score_Health_Habits_1<- sum(Score_1a[c(7,8,9)], na.rm = TRUE)
      Score_Health_Habits_2<- sum(Score_2a[c(7,8,9)], na.rm = TRUE)
      Score_Health_Habits<- c(Score_Health_Habits_1, Score_Health_Habits_2)
      Score_Health_Habits<- round(Score_Health_Habits, digits = 2)
      Score_Hypochondriacal_Beliefs_1<- sum(Score_1a[c(10,11,12)], na.rm = TRUE)
      Score_Hypochondriacal_Beliefs_2<- sum(Score_2a[c(10,11,12)], na.rm = TRUE)
      Score_Hypochondriacal_Beliefs<- c(Score_Hypochondriacal_Beliefs_1, Score_Hypochondriacal_Beliefs_2)
      Score_Hypochondriacal_Beliefs<- round(Score_Hypochondriacal_Beliefs, digits = 2)
      Score_Thanatophobia_1<- sum(Score_1a[c(13,14,15)], na.rm = TRUE)
      Score_Thanatophobia_2<- sum(Score_2a[c(13,14,15)], na.rm = TRUE)
      Score_Thanatophobia<- c(Score_Thanatophobia_1, Score_Thanatophobia_2)
      Score_Thanatophobia<- round(Score_Thanatophobia, digits = 2)
      Score_Disease_Phobia_1<- sum(Score_1a[c(16,17,18)], na.rm = TRUE)
      Score_Disease_Phobia_2<- sum(Score_2a[c(16,17,18)], na.rm = TRUE)
      Score_Disease_Phobia<- c(Score_Disease_Phobia_1, Score_Disease_Phobia_2)
      Score_Disease_Phobia<- round(Score_Disease_Phobia, digits = 2)
      Score_Bodily_Preoccupations_1<- sum(Score_1a[c(19,20,21)], na.rm = TRUE)
      Score_Bodily_Preoccupations_2<- sum(Score_2a[c(19,20,21)], na.rm = TRUE)
      Score_Bodily_Preoccupations<- c(Score_Bodily_Preoccupations_1, Score_Bodily_Preoccupations_2)
      Score_Bodily_Preoccupations<- round(Score_Bodily_Preoccupations, digits = 2)
      Score_Treatment_Experience_1<- sum(Score_1a[c(22,23,24)], na.rm = TRUE)
      Score_Treatment_Experience_2<- sum(Score_2a[c(22,23,24)], na.rm = TRUE)
      Score_Treatment_Experience<- c(Score_Treatment_Experience_1, Score_Treatment_Experience_2)
      Score_Treatment_Experience<- round(Score_Treatment_Experience, digits = 2)
      Score_Effects_Symptoms_1<- sum(Score_1a[c(25,26,27)], na.rm = TRUE)
      Score_Effects_Symptoms_2<- sum(Score_2a[c(25,26,27)], na.rm = TRUE)
      Score_Effects_Symptoms<- c(Score_Effects_Symptoms_1, Score_Effects_Symptoms_2)
      Score_Effects_Symptoms<- round(Score_Effects_Symptoms, digits = 2)
      Change<- c(0, (Score_2 - Score_1))
      Change<- round(Change, digits = 2)
      Change_Worry_Illness<- c(0, (Score_Worry_Illness_2 - Score_Worry_Illness_1))
      Change_Worry_Illness<- round(Change_Worry_Illness, digits = 2)
      Change_Concerns_Pain<- c(0, (Score_Concerns_Pain_2 - Score_Concerns_Pain_1))
      Change_Concerns_Pain<- round(Change_Concerns_Pain, digits = 2)
      Change_Health_Habits<- c(0, (Score_Health_Habits_2 - Score_Health_Habits_1))
      Change_Health_Habits<- round(Change_Health_Habits, digits = 2)
      Change_Hypochondriacal_Beliefs<- c(0, (Score_Hypochondriacal_Beliefs_2 - Score_Hypochondriacal_Beliefs_1))
      Change_Hypochondriacal_Beliefs<- round(Change_Hypochondriacal_Beliefs, digits = 2)
      Change_Thanatophobia<- c(0, (Score_Thanatophobia_2 - Score_Thanatophobia_1))
      Change_Thanatophobia<- round(Change_Thanatophobia, digits = 2)
      Change_Disease_Phobia<- c(0, (Score_Disease_Phobia_2 - Score_Disease_Phobia_1))
      Change_Disease_Phobia<- round(Change_Disease_Phobia, digits = 2)
      Change_Bodily_Preoccupations<- c(0, (Score_Bodily_Preoccupations_2 - Score_Bodily_Preoccupations_1))
      Change_Bodily_Preoccupations<- round(Change_Bodily_Preoccupations, digits = 2)
      Change_Treatment_Experience<- c(0, (Score_Treatment_Experience_2 - Score_Treatment_Experience_1))
      Change_Treatment_Experience<- round(Change_Treatment_Experience, digits = 2)
      Change_Effects_Symptoms<- c(0, (Score_Effects_Symptoms_2 - Score_Effects_Symptoms_1))
      Change_Effects_Symptoms<- round(Change_Effects_Symptoms, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2)
        PTS_Worry_Illness_1<- (Rel_Worry_Illness * Score_Worry_Illness_1) + (M_Worry_Illness * (1 - Rel_Worry_Illness))
        PTS_Worry_Illness_2<- (Rel_Worry_Illness * Score_Worry_Illness_2) + (M_Worry_Illness * (1 - Rel_Worry_Illness))
        PTS_Worry_Illness<- c(PTS_Worry_Illness_1, PTS_Worry_Illness_2)
        PTS_Concerns_Pain_1<- (Rel_Concerns_Pain * Score_Concerns_Pain_1) + (M_Concerns_Pain * (1 - Rel_Concerns_Pain))
        PTS_Concerns_Pain_2<- (Rel_Concerns_Pain * Score_Concerns_Pain_2) + (M_Concerns_Pain * (1 - Rel_Concerns_Pain))
        PTS_Concerns_Pain<- c(PTS_Concerns_Pain_1, PTS_Concerns_Pain_2)
        PTS_Health_Habits_1<- (Rel_Health_Habits * Score_Health_Habits_1) + (M_Health_Habits * (1 - Rel_Health_Habits))
        PTS_Health_Habits_2<- (Rel_Health_Habits * Score_Health_Habits_2) + (M_Health_Habits * (1 - Rel_Health_Habits))
        PTS_Health_Habits<- c(PTS_Health_Habits_1, PTS_Health_Habits_2)
        PTS_Hypochondriacal_Beliefs_1<- (Rel_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_1) + (M_Hypochondriacal_Beliefs * (1 - Rel_Hypochondriacal_Beliefs))
        PTS_Hypochondriacal_Beliefs_2<- (Rel_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_2) + (M_Hypochondriacal_Beliefs * (1 - Rel_Hypochondriacal_Beliefs))
        PTS_Hypochondriacal_Beliefs<- c(PTS_Hypochondriacal_Beliefs_1, PTS_Hypochondriacal_Beliefs_2)
        PTS_Thanatophobia_1<- (Rel_Thanatophobia * Score_Thanatophobia_1) + (M_Thanatophobia * (1 - Rel_Thanatophobia))
        PTS_Thanatophobia_2<- (Rel_Thanatophobia * Score_Thanatophobia_2) + (M_Thanatophobia * (1 - Rel_Thanatophobia))
        PTS_Thanatophobia<- c(PTS_Thanatophobia_1, PTS_Thanatophobia_2)
        PTS_Disease_Phobia_1<- (Rel_Disease_Phobia * Score_Disease_Phobia_1) + (M_Disease_Phobia * (1 - Rel_Disease_Phobia))
        PTS_Disease_Phobia_2<- (Rel_Disease_Phobia * Score_Disease_Phobia_2) + (M_Disease_Phobia * (1 - Rel_Disease_Phobia))
        PTS_Disease_Phobia<- c(PTS_Disease_Phobia_1, PTS_Disease_Phobia_2)
        PTS_Bodily_Preoccupations_1<- (Rel_Bodily_Preoccupations * Score_Bodily_Preoccupations_1) + (M_Bodily_Preoccupations * (1 - Rel_Bodily_Preoccupations))
        PTS_Bodily_Preoccupations_2<- (Rel_Bodily_Preoccupations * Score_Bodily_Preoccupations_2) + (M_Bodily_Preoccupations * (1 - Rel_Bodily_Preoccupations))
        PTS_Bodily_Preoccupations<- c(PTS_Bodily_Preoccupations_1, PTS_Bodily_Preoccupations_2)
        PTS_Treatment_Experience_1<- (Rel_Treatment_Experience * Score_Treatment_Experience_1) + (M_Treatment_Experience * (1 - Rel_Treatment_Experience))
        PTS_Treatment_Experience_2<- (Rel_Treatment_Experience * Score_Treatment_Experience_2) + (M_Treatment_Experience * (1 - Rel_Treatment_Experience))
        PTS_Treatment_Experience<- c(PTS_Treatment_Experience_1, PTS_Treatment_Experience_2)
        PTS_Effects_Symptoms_1<- (Rel_Effects_Symptoms * Score_Effects_Symptoms_1) + (M_Effects_Symptoms * (1 - Rel_Effects_Symptoms))
        PTS_Effects_Symptoms_2<- (Rel_Effects_Symptoms * Score_Effects_Symptoms_2) + (M_Effects_Symptoms * (1 - Rel_Effects_Symptoms))
        PTS_Effects_Symptoms<- c(PTS_Effects_Symptoms_1, PTS_Effects_Symptoms_2)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2)
        PTS_Worry_Illness_1<- Score_Worry_Illness_1 + (M_Retest_Worry_Illness - M_Worry_Illness)  
        PTS_Worry_Illness_2<- Score_Worry_Illness_2 + (M_Retest_Worry_Illness - M_Worry_Illness) 
        PTS_Worry_Illness<- c(PTS_Worry_Illness_1, PTS_Worry_Illness_2)
        PTS_Concerns_Pain_1<- Score_Concerns_Pain_1 + (M_Retest_Concerns_Pain - M_Concerns_Pain)  
        PTS_Concerns_Pain_2<- Score_Concerns_Pain_2 + (M_Retest_Concerns_Pain - M_Concerns_Pain) 
        PTS_Concerns_Pain<- c(PTS_Concerns_Pain_1, PTS_Concerns_Pain_2)
        PTS_Health_Habits_1<- Score_Health_Habits_1 + (M_Retest_Health_Habits - M_Health_Habits)  
        PTS_Health_Habits_2<- Score_Health_Habits_2 + (M_Retest_Health_Habits - M_Health_Habits) 
        PTS_Health_Habits<- c(PTS_Health_Habits_1, PTS_Health_Habits_2)
        PTS_Hypochondriacal_Beliefs_1<- Score_Hypochondriacal_Beliefs_1 + (M_Retest_Hypochondriacal_Beliefs - M_Hypochondriacal_Beliefs)  
        PTS_Hypochondriacal_Beliefs_2<- Score_Hypochondriacal_Beliefs_2 + (M_Retest_Hypochondriacal_Beliefs - M_Hypochondriacal_Beliefs) 
        PTS_Hypochondriacal_Beliefs<- c(PTS_Hypochondriacal_Beliefs_1, PTS_Hypochondriacal_Beliefs_2)
        PTS_Thanatophobia_1<- Score_Thanatophobia_1 + (M_Retest_Thanatophobia - M_Thanatophobia)  
        PTS_Thanatophobia_2<- Score_Thanatophobia_2 + (M_Retest_Thanatophobia - M_Thanatophobia) 
        PTS_Thanatophobia<- c(PTS_Thanatophobia_1, PTS_Thanatophobia_2)
        PTS_Disease_Phobia_1<- Score_Disease_Phobia_1 + (M_Retest_Disease_Phobia - M_Disease_Phobia)  
        PTS_Disease_Phobia_2<- Score_Disease_Phobia_2 + (M_Retest_Disease_Phobia - M_Disease_Phobia) 
        PTS_Disease_Phobia<- c(PTS_Disease_Phobia_1, PTS_Disease_Phobia_2)
        PTS_Bodily_Preoccupations_1<- Score_Bodily_Preoccupations_1 + (M_Retest_Bodily_Preoccupations - M_Bodily_Preoccupations)  
        PTS_Bodily_Preoccupations_2<- Score_Bodily_Preoccupations_2 + (M_Retest_Bodily_Preoccupations - M_Bodily_Preoccupations) 
        PTS_Bodily_Preoccupations<- c(PTS_Bodily_Preoccupations_1, PTS_Bodily_Preoccupations_2)
        PTS_Treatment_Experience_1<- Score_Treatment_Experience_1 + (M_Retest_Treatment_Experience - M_Treatment_Experience)  
        PTS_Treatment_Experience_2<- Score_Treatment_Experience_2 + (M_Retest_Treatment_Experience - M_Treatment_Experience) 
        PTS_Treatment_Experience<- c(PTS_Treatment_Experience_1, PTS_Treatment_Experience_2)
        PTS_Effects_Symptoms_1<- Score_Effects_Symptoms_1 + (M_Retest_Effects_Symptoms - M_Effects_Symptoms)  
        PTS_Effects_Symptoms_2<- Score_Effects_Symptoms_2 + (M_Retest_Effects_Symptoms - M_Effects_Symptoms) 
        PTS_Effects_Symptoms<- c(PTS_Effects_Symptoms_1, PTS_Effects_Symptoms_2)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Worry_Illness<- Score_Worry_Illness
        PTS_Concerns_Pain<- Score_Concerns_Pain
        PTS_Health_Habits<- Score_Health_Habits
        PTS_Hypochondriacal_Beliefs<- Score_Hypochondriacal_Beliefs
        PTS_Thanatophobia<- Score_Thanatophobia
        PTS_Disease_Phobia<- Score_Disease_Phobia
        PTS_Bodily_Preoccupations<- Score_Bodily_Preoccupations
        PTS_Treatment_Experience<- Score_Treatment_Experience
        PTS_Effects_Symptoms<- Score_Effects_Symptoms
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Worry_Illness<- Rel_Worry_Illness * (SD_Retest_Worry_Illness/SD_Worry_Illness)
        A_Constant_Worry_Illness<- M_Retest_Worry_Illness - (B_Slope_Worry_Illness * M_Worry_Illness)
        B_Adj_Worry_Illness<- SD_Retest_Worry_Illness/SD_Worry_Illness
        A_Adj_Worry_Illness<- M_Retest_Worry_Illness - (B_Adj_Worry_Illness * M_Worry_Illness)
        PTS_Worry_Illness_1<- (B_Adj_Worry_Illness * Score_Worry_Illness_1) + A_Adj_Worry_Illness
        PTS_Worry_Illness_2<- (B_Adj_Worry_Illness * Score_Worry_Illness_2) + A_Adj_Worry_Illness
        PTS_Worry_Illness<- c(PTS_Worry_Illness_1,PTS_Worry_Illness_2)
        B_Slope_Concerns_Pain<- Rel_Concerns_Pain * (SD_Retest_Concerns_Pain/SD_Concerns_Pain)
        A_Constant_Concerns_Pain<- M_Retest_Concerns_Pain - (B_Slope_Concerns_Pain * M_Concerns_Pain)
        B_Adj_Concerns_Pain<- SD_Retest_Concerns_Pain/SD_Concerns_Pain
        A_Adj_Concerns_Pain<- M_Retest_Concerns_Pain - (B_Adj_Concerns_Pain * M_Concerns_Pain)
        PTS_Concerns_Pain_1<- (B_Adj_Concerns_Pain * Score_Concerns_Pain_1) + A_Adj_Concerns_Pain
        PTS_Concerns_Pain_2<- (B_Adj_Concerns_Pain * Score_Concerns_Pain_2) + A_Adj_Concerns_Pain
        PTS_Concerns_Pain<- c(PTS_Concerns_Pain_1,PTS_Concerns_Pain_2)
        B_Slope_Health_Habits<- Rel_Health_Habits * (SD_Retest_Health_Habits/SD_Health_Habits)
        A_Constant_Health_Habits<- M_Retest_Health_Habits - (B_Slope_Health_Habits * M_Health_Habits)
        B_Adj_Health_Habits<- SD_Retest_Health_Habits/SD_Health_Habits
        A_Adj_Health_Habits<- M_Retest_Health_Habits - (B_Adj_Health_Habits * M_Health_Habits)
        PTS_Health_Habits_1<- (B_Adj_Health_Habits * Score_Health_Habits_1) + A_Adj_Health_Habits
        PTS_Health_Habits_2<- (B_Adj_Health_Habits * Score_Health_Habits_2) + A_Adj_Health_Habits
        PTS_Health_Habits<- c(PTS_Health_Habits_1,PTS_Health_Habits_2)
        B_Slope_Hypochondriacal_Beliefs<- Rel_Hypochondriacal_Beliefs * (SD_Retest_Hypochondriacal_Beliefs/SD_Hypochondriacal_Beliefs)
        A_Constant_Hypochondriacal_Beliefs<- M_Retest_Hypochondriacal_Beliefs - (B_Slope_Hypochondriacal_Beliefs * M_Hypochondriacal_Beliefs)
        B_Adj_Hypochondriacal_Beliefs<- SD_Retest_Hypochondriacal_Beliefs/SD_Hypochondriacal_Beliefs
        A_Adj_Hypochondriacal_Beliefs<- M_Retest_Hypochondriacal_Beliefs - (B_Adj_Hypochondriacal_Beliefs * M_Hypochondriacal_Beliefs)
        PTS_Hypochondriacal_Beliefs_1<- (B_Adj_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_1) + A_Adj_Hypochondriacal_Beliefs
        PTS_Hypochondriacal_Beliefs_2<- (B_Adj_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_2) + A_Adj_Hypochondriacal_Beliefs
        PTS_Hypochondriacal_Beliefs<- c(PTS_Hypochondriacal_Beliefs_1,PTS_Hypochondriacal_Beliefs_2)
        B_Slope_Thanatophobia<- Rel_Thanatophobia * (SD_Retest_Thanatophobia/SD_Thanatophobia)
        A_Constant_Thanatophobia<- M_Retest_Thanatophobia - (B_Slope_Thanatophobia * M_Thanatophobia)
        B_Adj_Thanatophobia<- SD_Retest_Thanatophobia/SD_Thanatophobia
        A_Adj_Thanatophobia<- M_Retest_Thanatophobia - (B_Adj_Thanatophobia * M_Thanatophobia)
        PTS_Thanatophobia_1<- (B_Adj_Thanatophobia * Score_Thanatophobia_1) + A_Adj_Thanatophobia
        PTS_Thanatophobia_2<- (B_Adj_Thanatophobia * Score_Thanatophobia_2) + A_Adj_Thanatophobia
        PTS_Thanatophobia<- c(PTS_Thanatophobia_1,PTS_Thanatophobia_2)
        B_Slope_Disease_Phobia<- Rel_Disease_Phobia * (SD_Retest_Disease_Phobia/SD_Disease_Phobia)
        A_Constant_Disease_Phobia<- M_Retest_Disease_Phobia - (B_Slope_Disease_Phobia * M_Disease_Phobia)
        B_Adj_Disease_Phobia<- SD_Retest_Disease_Phobia/SD_Disease_Phobia
        A_Adj_Disease_Phobia<- M_Retest_Disease_Phobia - (B_Adj_Disease_Phobia * M_Disease_Phobia)
        PTS_Disease_Phobia_1<- (B_Adj_Disease_Phobia * Score_Disease_Phobia_1) + A_Adj_Disease_Phobia
        PTS_Disease_Phobia_2<- (B_Adj_Disease_Phobia * Score_Disease_Phobia_2) + A_Adj_Disease_Phobia
        PTS_Disease_Phobia<- c(PTS_Disease_Phobia_1, PTS_Disease_Phobia_2)
        B_Slope_Bodily_Preoccupations<- Rel_Bodily_Preoccupations * (SD_Retest_Bodily_Preoccupations/SD_Bodily_Preoccupations)
        A_Constant_Bodily_Preoccupations<- M_Retest_Bodily_Preoccupations - (B_Slope_Bodily_Preoccupations * M_Bodily_Preoccupations)
        B_Adj_Bodily_Preoccupations<- SD_Retest_Bodily_Preoccupations/SD_Bodily_Preoccupations
        A_Adj_Bodily_Preoccupations<- M_Retest_Bodily_Preoccupations - (B_Adj_Bodily_Preoccupations * M_Bodily_Preoccupations)
        PTS_Bodily_Preoccupations_1<- (B_Adj_Bodily_Preoccupations * Score_Bodily_Preoccupations_1) + A_Adj_Bodily_Preoccupations
        PTS_Bodily_Preoccupations_2<- (B_Adj_Bodily_Preoccupations * Score_Bodily_Preoccupations_2) + A_Adj_Bodily_Preoccupations
        PTS_Bodily_Preoccupations<- c(PTS_Bodily_Preoccupations_1,PTS_Bodily_Preoccupations_2)
        B_Slope_Treatment_Experience<- Rel_Treatment_Experience * (SD_Retest_Treatment_Experience/SD_Treatment_Experience)
        A_Constant_Treatment_Experience<- M_Retest_Treatment_Experience - (B_Slope_Treatment_Experience * M_Treatment_Experience)
        B_Adj_Treatment_Experience<- SD_Retest_Treatment_Experience/SD_Treatment_Experience
        A_Adj_Treatment_Experience<- M_Retest_Treatment_Experience - (B_Adj_Treatment_Experience * M_Treatment_Experience)
        PTS_Treatment_Experience_1<- (B_Adj_Treatment_Experience * Score_Treatment_Experience_1) + A_Adj_Treatment_Experience
        PTS_Treatment_Experience_2<- (B_Adj_Treatment_Experience * Score_Treatment_Experience_2) + A_Adj_Treatment_Experience
        PTS_Treatment_Experience<- c(PTS_Treatment_Experience_1,PTS_Treatment_Experience_2)
        B_Slope_Effects_Symptoms<- Rel_Effects_Symptoms * (SD_Retest_Effects_Symptoms/SD_Effects_Symptoms)
        A_Constant_Effects_Symptoms<- M_Retest_Effects_Symptoms - (B_Slope_Effects_Symptoms * M_Effects_Symptoms)
        B_Adj_Effects_Symptoms<- SD_Retest_Effects_Symptoms/SD_Effects_Symptoms
        A_Adj_Effects_Symptoms<- M_Retest_Effects_Symptoms - (B_Adj_Effects_Symptoms * M_Effects_Symptoms)
        PTS_Effects_Symptoms_1<- (B_Adj_Effects_Symptoms * Score_Effects_Symptoms_1) + A_Adj_Effects_Symptoms
        PTS_Effects_Symptoms_2<- (B_Adj_Effects_Symptoms * Score_Effects_Symptoms_2) + A_Adj_Effects_Symptoms
        PTS_Effects_Symptoms<- c(PTS_Effects_Symptoms_1,PTS_Effects_Symptoms_2)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Worry_Illness<- Rel_Worry_Illness * (SD_Retest_Worry_Illness/SD_Worry_Illness)
        PTS_Worry_Illness_1<- B_Slope_Worry_Illness * Score_Worry_Illness_1
        PTS_Worry_Illness_2<- B_Slope_Worry_Illness * Score_Worry_Illness_2
        PTS_Worry_Illness<- c(PTS_Worry_Illness_1, PTS_Worry_Illness_2)
        B_Slope_Concerns_Pain<- Rel_Concerns_Pain * (SD_Retest_Concerns_Pain/SD_Concerns_Pain)
        PTS_Concerns_Pain_1<- B_Slope_Concerns_Pain * Score_Concerns_Pain_1
        PTS_Concerns_Pain_2<- B_Slope_Concerns_Pain * Score_Concerns_Pain_2
        PTS_Concerns_Pain<- c(PTS_Concerns_Pain_1, PTS_Concerns_Pain_2)
        B_Slope_Health_Habits<- Rel_Health_Habits * (SD_Retest_Health_Habits/SD_Health_Habits)
        PTS_Health_Habits_1<- B_Slope_Health_Habits * Score_Health_Habits_1
        PTS_Health_Habits_2<- B_Slope_Health_Habits * Score_Health_Habits_2
        PTS_Health_Habits<- c(PTS_Health_Habits_1, PTS_Health_Habits_2)
        B_Slope_Hypochondriacal_Beliefs<- Rel_Hypochondriacal_Beliefs * (SD_Retest_Hypochondriacal_Beliefs/SD_Hypochondriacal_Beliefs)
        PTS_Hypochondriacal_Beliefs_1<- B_Slope_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_1
        PTS_Hypochondriacal_Beliefs_2<- B_Slope_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_2
        PTS_Hypochondriacal_Beliefs<- c(PTS_Hypochondriacal_Beliefs_1, PTS_Hypochondriacal_Beliefs_2)
        B_Slope_Thanatophobia<- Rel_Thanatophobia * (SD_Retest_Thanatophobia/SD_Thanatophobia)
        PTS_Thanatophobia_1<- B_Slope_Thanatophobia * Score_Thanatophobia_1
        PTS_Thanatophobia_2<- B_Slope_Thanatophobia * Score_Thanatophobia_2
        PTS_Thanatophobia<- c(PTS_Thanatophobia_1, PTS_Thanatophobia_2)
        B_Slope_Disease_Phobia<- Rel_Disease_Phobia * (SD_Retest_Disease_Phobia/SD_Disease_Phobia)
        PTS_Disease_Phobia_1<- B_Slope_Disease_Phobia * Score_Disease_Phobia_1
        PTS_Disease_Phobia_2<- B_Slope_Disease_Phobia * Score_Disease_Phobia_2
        PTS_Disease_Phobia<- c(PTS_Disease_Phobia_1, PTS_Disease_Phobia_2)
        B_Slope_Bodily_Preoccupations<- Rel_Bodily_Preoccupations * (SD_Retest_Bodily_Preoccupations/SD_Bodily_Preoccupations)
        PTS_Bodily_Preoccupations_1<- B_Slope_Bodily_Preoccupations * Score_Bodily_Preoccupations_1
        PTS_Bodily_Preoccupations_2<- B_Slope_Bodily_Preoccupations * Score_Bodily_Preoccupations_2
        PTS_Bodily_Preoccupations<- c(PTS_Bodily_Preoccupations_1, PTS_Bodily_Preoccupations_2) 
        B_Slope_Treatment_Experience<- Rel_Treatment_Experience * (SD_Retest_Treatment_Experience/SD_Treatment_Experience)
        PTS_Treatment_Experience_1<- B_Slope_Treatment_Experience * Score_Treatment_Experience_1
        PTS_Treatment_Experience_2<- B_Slope_Treatment_Experience * Score_Treatment_Experience_2
        PTS_Treatment_Experience<- c(PTS_Treatment_Experience_1, PTS_Treatment_Experience_2)
        B_Slope_Effects_Symptoms<- Rel_Effects_Symptoms * (SD_Retest_Effects_Symptoms/SD_Effects_Symptoms)
        PTS_Effects_Symptoms_1<- B_Slope_Effects_Symptoms * Score_Effects_Symptoms_1
        PTS_Effects_Symptoms_2<- B_Slope_Effects_Symptoms * Score_Effects_Symptoms_2
        PTS_Effects_Symptoms<- c(PTS_Effects_Symptoms_1, PTS_Effects_Symptoms_2)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2)
        PTS_Worry_Illness_1<- Score_Worry_Illness_1 + (M_Retest_Worry_Illness - M_Worry_Illness)
        PTS_Worry_Illness_2<- Score_Worry_Illness_2 + (M_Retest_Worry_Illness - M_Worry_Illness)
        PTS_Worry_Illness<- c(PTS_Worry_Illness_1, PTS_Worry_Illness_2)
        PTS_Concerns_Pain_1<- Score_Concerns_Pain_1 + (M_Retest_Concerns_Pain - M_Concerns_Pain)
        PTS_Concerns_Pain_2<- Score_Concerns_Pain_2 + (M_Retest_Concerns_Pain - M_Concerns_Pain)
        PTS_Concerns_Pain<- c(PTS_Concerns_Pain_1, PTS_Concerns_Pain_2)
        PTS_Health_Habits_1<- Score_Health_Habits_1 + (M_Retest_Health_Habits - M_Health_Habits)
        PTS_Health_Habits_2<- Score_Health_Habits_2 + (M_Retest_Health_Habits - M_Health_Habits)
        PTS_Health_Habits<- c(PTS_Health_Habits_1, PTS_Health_Habits_2)
        PTS_Hypochondriacal_Beliefs_1<- Score_Hypochondriacal_Beliefs_1 + (M_Retest_Hypochondriacal_Beliefs - M_Hypochondriacal_Beliefs)
        PTS_Hypochondriacal_Beliefs_2<- Score_Hypochondriacal_Beliefs_2 + (M_Retest_Hypochondriacal_Beliefs - M_Hypochondriacal_Beliefs)
        PTS_Hypochondriacal_Beliefs<- c(PTS_Hypochondriacal_Beliefs_1, PTS_Hypochondriacal_Beliefs_2)
        PTS_Thanatophobia_1<- Score_Thanatophobia_1 + (M_Retest_Thanatophobia - M_Thanatophobia)
        PTS_Thanatophobia_2<- Score_Thanatophobia_2 + (M_Retest_Thanatophobia - M_Thanatophobia)
        PTS_Thanatophobia<- c(PTS_Thanatophobia_1, PTS_Thanatophobia_2)
        PTS_Disease_Phobia_1<- Score_Disease_Phobia_1 + (M_Retest_Disease_Phobia - M_Disease_Phobia)
        PTS_Disease_Phobia_2<- Score_Disease_Phobia_2 + (M_Retest_Disease_Phobia - M_Disease_Phobia)
        PTS_Disease_Phobia<- c(PTS_Disease_Phobia_1, PTS_Disease_Phobia_2)
        PTS_Bodily_Preoccupations_1<- Score_Bodily_Preoccupations_1 + (M_Retest_Bodily_Preoccupations - M_Bodily_Preoccupations)
        PTS_Bodily_Preoccupations_2<- Score_Bodily_Preoccupations_2 + (M_Retest_Bodily_Preoccupations - M_Bodily_Preoccupations)
        PTS_Bodily_Preoccupations<- c(PTS_Bodily_Preoccupations_1, PTS_Bodily_Preoccupations_2)
        PTS_Treatment_Experience_1<- Score_Treatment_Experience_1 + (M_Retest_Treatment_Experience - M_Treatment_Experience)
        PTS_Treatment_Experience_2<- Score_Treatment_Experience_2 + (M_Retest_Treatment_Experience - M_Treatment_Experience)
        PTS_Treatment_Experience<- c(PTS_Treatment_Experience_1, PTS_Treatment_Experience_2)
        PTS_Effects_Symptoms_1<- Score_Effects_Symptoms_1 + (M_Retest_Effects_Symptoms - M_Effects_Symptoms)
        PTS_Effects_Symptoms_2<- Score_Effects_Symptoms_2 + (M_Retest_Effects_Symptoms - M_Effects_Symptoms)
        PTS_Effects_Symptoms<- c(PTS_Effects_Symptoms_1, PTS_Effects_Symptoms_2)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Worry_Illness<- round(PTS_Worry_Illness, digits = 2)
      PTS_Concerns_Pain<- round(PTS_Concerns_Pain, digits = 2)
      PTS_Health_Habits<- round(PTS_Health_Habits, digits = 2)
      PTS_Hypochondriacal_Beliefs<- round(PTS_Hypochondriacal_Beliefs, digits = 2)
      PTS_Thanatophobia<- round(PTS_Thanatophobia, digits = 2)
      PTS_Disease_Phobia<- round(PTS_Disease_Phobia, digits = 2)
      PTS_Bodily_Preoccupations<- round(PTS_Bodily_Preoccupations, digits = 2)
      PTS_Treatment_Experience<- round(PTS_Treatment_Experience, digits = 2)
      PTS_Effects_Symptoms<- round(PTS_Effects_Symptoms, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2)
        SE_Worry_Illness_1<- McSweeny_SE_Worry_Illness*sqrt(1 + (1/SampleN) + ((Score_Worry_Illness_1 - M_Worry_Illness)^2/(SD_Worry_Illness^2*(SampleN-1))))
        SE_Worry_Illness_2<- McSweeny_SE_Worry_Illness*sqrt(1 + (1/SampleN) + ((Score_Worry_Illness_2 - M_Worry_Illness)^2/(SD_Worry_Illness^2*(SampleN-1))))
        SE_Worry_Illness<-c(SE_Worry_Illness_1, SE_Worry_Illness_2)
        SE_Concerns_Pain_1<- McSweeny_SE_Concerns_Pain*sqrt(1 + (1/SampleN) + ((Score_Concerns_Pain_1 - M_Concerns_Pain)^2/(SD_Concerns_Pain^2*(SampleN-1))))
        SE_Concerns_Pain_2<- McSweeny_SE_Concerns_Pain*sqrt(1 + (1/SampleN) + ((Score_Concerns_Pain_2 - M_Concerns_Pain)^2/(SD_Concerns_Pain^2*(SampleN-1))))
        SE_Concerns_Pain<-c(SE_Concerns_Pain_1, SE_Concerns_Pain_2)
        SE_Health_Habits_1<- McSweeny_SE_Health_Habits*sqrt(1 + (1/SampleN) + ((Score_Health_Habits_1 - M_Health_Habits)^2/(SD_Health_Habits^2*(SampleN-1))))
        SE_Health_Habits_2<- McSweeny_SE_Health_Habits*sqrt(1 + (1/SampleN) + ((Score_Health_Habits_2 - M_Health_Habits)^2/(SD_Health_Habits^2*(SampleN-1))))
        SE_Health_Habits<-c(SE_Health_Habits_1, SE_Health_Habits_2)
        SE_Hypochondriacal_Beliefs_1<- McSweeny_SE_Hypochondriacal_Beliefs*sqrt(1 + (1/SampleN) + ((Score_Hypochondriacal_Beliefs_1 - M_Hypochondriacal_Beliefs)^2/(SD_Hypochondriacal_Beliefs^2*(SampleN-1))))
        SE_Hypochondriacal_Beliefs_2<- McSweeny_SE_Hypochondriacal_Beliefs*sqrt(1 + (1/SampleN) + ((Score_Hypochondriacal_Beliefs_2 - M_Hypochondriacal_Beliefs)^2/(SD_Hypochondriacal_Beliefs^2*(SampleN-1))))
        SE_Hypochondriacal_Beliefs<-c(SE_Hypochondriacal_Beliefs_1, SE_Hypochondriacal_Beliefs_2)
        SE_Thanatophobia_1<- McSweeny_SE_Thanatophobia*sqrt(1 + (1/SampleN) + ((Score_Thanatophobia_1 - M_Thanatophobia)^2/(SD_Thanatophobia^2*(SampleN-1))))
        SE_Thanatophobia_2<- McSweeny_SE_Thanatophobia*sqrt(1 + (1/SampleN) + ((Score_Thanatophobia_2 - M_Thanatophobia)^2/(SD_Thanatophobia^2*(SampleN-1))))
        SE_Thanatophobia<-c(SE_Thanatophobia_1, SE_Thanatophobia_2)
        SE_Disease_Phobia_1<- McSweeny_SE_Disease_Phobia*sqrt(1 + (1/SampleN) + ((Score_Disease_Phobia_1 - M_Disease_Phobia)^2/(SD_Disease_Phobia^2*(SampleN-1))))
        SE_Disease_Phobia_2<- McSweeny_SE_Disease_Phobia*sqrt(1 + (1/SampleN) + ((Score_Disease_Phobia_2 - M_Disease_Phobia)^2/(SD_Disease_Phobia^2*(SampleN-1))))
        SE_Disease_Phobia<-c(SE_Disease_Phobia_1, SE_Disease_Phobia_2)
        SE_Bodily_Preoccupations_1<- McSweeny_SE_Bodily_Preoccupations*sqrt(1 + (1/SampleN) + ((Score_Bodily_Preoccupations_1 - M_Bodily_Preoccupations)^2/(SD_Bodily_Preoccupations^2*(SampleN-1))))
        SE_Bodily_Preoccupations_2<- McSweeny_SE_Bodily_Preoccupations*sqrt(1 + (1/SampleN) + ((Score_Bodily_Preoccupations_2 - M_Bodily_Preoccupations)^2/(SD_Bodily_Preoccupations^2*(SampleN-1))))
        SE_Bodily_Preoccupations<-c(SE_Bodily_Preoccupations_1, SE_Bodily_Preoccupations_2)
        SE_Treatment_Experience_1<- McSweeny_SE_Treatment_Experience*sqrt(1 + (1/SampleN) + ((Score_Treatment_Experience_1 - M_Treatment_Experience)^2/(SD_Treatment_Experience^2*(SampleN-1))))
        SE_Treatment_Experience_2<- McSweeny_SE_Treatment_Experience*sqrt(1 + (1/SampleN) + ((Score_Treatment_Experience_2 - M_Treatment_Experience)^2/(SD_Treatment_Experience^2*(SampleN-1))))
        SE_Treatment_Experience<-c(SE_Treatment_Experience_1, SE_Treatment_Experience_2)
        SE_Effects_Symptoms_1<- McSweeny_SE_Effects_Symptoms*sqrt(1 + (1/SampleN) + ((Score_Effects_Symptoms_1 - M_Effects_Symptoms)^2/(SD_Effects_Symptoms^2*(SampleN-1))))
        SE_Effects_Symptoms_2<- McSweeny_SE_Effects_Symptoms*sqrt(1 + (1/SampleN) + ((Score_Effects_Symptoms_2 - M_Effects_Symptoms)^2/(SD_Effects_Symptoms^2*(SampleN-1))))
        SE_Effects_Symptoms<-c(SE_Effects_Symptoms_1, SE_Effects_Symptoms_2)
        SE<- round(SE, digits = 2)
        SE_Worry_Illness<- round(SE_Worry_Illness, digits = 2)
        SE_Concerns_Pain<- round(SE_Concerns_Pain, digits = 2)
        SE_Health_Habits<- round(SE_Health_Habits, digits = 2)
        SE_Hypochondriacal_Beliefs<- round(SE_Hypochondriacal_Beliefs, digits = 2)
        SE_Thanatophobia<- round(SE_Thanatophobia, digits = 2)
        SE_Disease_Phobia<- round(SE_Disease_Phobia, digits = 2)
        SE_Bodily_Preoccupations<- round(SE_Bodily_Preoccupations, digits = 2)
        SE_Treatment_Experience<- round(SE_Treatment_Experience, digits = 2)
        SE_Effects_Symptoms<- round(SE_Effects_Symptoms, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2))
        CI<- round(CI, digits = 2)
        CI_Worry_Illness<- c((Conf*SE_Worry_Illness_1), (Conf*SE_Worry_Illness_2))
        CI_Worry_Illness<- round(CI_Worry_Illness, digits = 2)
        CI_Concerns_Pain<- c((Conf*SE_Concerns_Pain_1), (Conf*SE_Concerns_Pain_2))
        CI_Concerns_Pain<- round(CI_Concerns_Pain, digits = 2)
        CI_Health_Habits<- c((Conf*SE_Health_Habits_1), (Conf*SE_Health_Habits_2))
        CI_Health_Habits<- round(CI_Health_Habits, digits = 2)
        CI_Hypochondriacal_Beliefs<- c((Conf*SE_Hypochondriacal_Beliefs_1), (Conf*SE_Hypochondriacal_Beliefs_2))
        CI_Hypochondriacal_Beliefs<- round(CI_Hypochondriacal_Beliefs, digits = 2)
        CI_Thanatophobia<- c((Conf*SE_Thanatophobia_1), (Conf*SE_Thanatophobia_2))
        CI_Thanatophobia<- round(CI_Thanatophobia, digits = 2)
        CI_Disease_Phobia<- c((Conf*SE_Disease_Phobia_1), (Conf*SE_Disease_Phobia_2))
        CI_Disease_Phobia<- round(CI_Disease_Phobia, digits = 2)
        CI_Bodily_Preoccupations<- c((Conf*SE_Bodily_Preoccupations_1), (Conf*SE_Bodily_Preoccupations_2))
        CI_Bodily_Preoccupations<- round(CI_Bodily_Preoccupations, digits = 2)
        CI_Treatment_Experience<- c((Conf*SE_Treatment_Experience_1), (Conf*SE_Treatment_Experience_2))
        CI_Treatment_Experience<- round(CI_Treatment_Experience, digits = 2)
        CI_Effects_Symptoms<- c((Conf*SE_Effects_Symptoms_1), (Conf*SE_Effects_Symptoms_2))
        CI_Effects_Symptoms<- round(CI_Effects_Symptoms, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
      CI<- c((Conf*SE), (Conf*SE))
      CI<- round(CI, digits = 2)
      CI_Worry_Illness<- c((Conf*SE_Worry_Illness), (Conf*SE_Worry_Illness))
      CI_Worry_Illness<- round(CI_Worry_Illness, digits = 2)
      CI_Concerns_Pain<- c((Conf*SE_Concerns_Pain), (Conf*SE_Concerns_Pain))
      CI_Concerns_Pain<- round(CI_Concerns_Pain, digits = 2)
      CI_Health_Habits<- c((Conf*SE_Health_Habits), (Conf*SE_Health_Habits))
      CI_Health_Habits<- round(CI_Health_Habits, digits = 2)
      CI_Hypochondriacal_Beliefs<- c((Conf*SE_Hypochondriacal_Beliefs), (Conf*SE_Hypochondriacal_Beliefs))
      CI_Hypochondriacal_Beliefs<- round(CI_Hypochondriacal_Beliefs, digits = 2)
      CI_Thanatophobia<- c((Conf*SE_Thanatophobia), (Conf*SE_Thanatophobia))
      CI_Thanatophobia<- round(CI_Thanatophobia, digits = 2)
      CI_Disease_Phobia<- c((Conf*SE_Disease_Phobia), (Conf*SE_Disease_Phobia))
      CI_Disease_Phobia<- round(CI_Disease_Phobia, digits = 2)
      CI_Bodily_Preoccupations<- c((Conf*SE_Bodily_Preoccupations), (Conf*SE_Bodily_Preoccupations))
      CI_Bodily_Preoccupations<- round(CI_Bodily_Preoccupations, digits = 2)
      CI_Treatment_Experience<- c((Conf*SE_Treatment_Experience), (Conf*SE_Treatment_Experience))
      CI_Treatment_Experience<- round(CI_Treatment_Experience, digits = 2)
      CI_Effects_Symptoms<- c((Conf*SE_Effects_Symptoms), (Conf*SE_Effects_Symptoms))
      CI_Effects_Symptoms<- round(CI_Effects_Symptoms, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Worry_Illness<- PTS_Worry_Illness + CI_Worry_Illness
      CI_Upper_Lim_Worry_Illness<- round(CI_Upper_Lim_Worry_Illness, digits = 2)
      CI_Lower_Lim_Worry_Illness<-PTS_Worry_Illness - CI_Worry_Illness
      CI_Lower_Lim_Worry_Illness<- round(CI_Lower_Lim_Worry_Illness, digits = 2)
      CI_Upper_Lim_Concerns_Pain<- PTS_Concerns_Pain + CI_Concerns_Pain
      CI_Upper_Lim_Concerns_Pain<- round(CI_Upper_Lim_Concerns_Pain, digits = 2)
      CI_Lower_Lim_Concerns_Pain<-PTS_Concerns_Pain - CI_Concerns_Pain
      CI_Lower_Lim_Concerns_Pain<- round(CI_Lower_Lim_Concerns_Pain, digits = 2)
      CI_Upper_Lim_Health_Habits<- PTS_Health_Habits + CI_Health_Habits
      CI_Upper_Lim_Health_Habits<- round(CI_Upper_Lim_Health_Habits, digits = 2)
      CI_Lower_Lim_Health_Habits<-PTS_Health_Habits - CI_Health_Habits
      CI_Lower_Lim_Health_Habits<- round(CI_Lower_Lim_Health_Habits, digits = 2)
      CI_Upper_Lim_Hypochondriacal_Beliefs<- PTS_Hypochondriacal_Beliefs + CI_Hypochondriacal_Beliefs
      CI_Upper_Lim_Hypochondriacal_Beliefs<- round(CI_Upper_Lim_Hypochondriacal_Beliefs, digits = 2)
      CI_Lower_Lim_Hypochondriacal_Beliefs<-PTS_Hypochondriacal_Beliefs - CI_Hypochondriacal_Beliefs
      CI_Lower_Lim_Hypochondriacal_Beliefs<- round(CI_Lower_Lim_Hypochondriacal_Beliefs, digits = 2)
      CI_Upper_Lim_Thanatophobia<- PTS_Thanatophobia + CI_Thanatophobia
      CI_Upper_Lim_Thanatophobia<- round(CI_Upper_Lim_Thanatophobia, digits = 2)
      CI_Lower_Lim_Thanatophobia<-PTS_Thanatophobia - CI_Thanatophobia
      CI_Lower_Lim_Thanatophobia<- round(CI_Lower_Lim_Thanatophobia, digits = 2)
      CI_Upper_Lim_Disease_Phobia<- PTS_Disease_Phobia + CI_Disease_Phobia
      CI_Upper_Lim_Disease_Phobia<- round(CI_Upper_Lim_Disease_Phobia, digits = 2)
      CI_Lower_Lim_Disease_Phobia<- PTS_Disease_Phobia - CI_Disease_Phobia
      CI_Lower_Lim_Disease_Phobia<- round(CI_Lower_Lim_Disease_Phobia, digits = 2)
      CI_Upper_Lim_Bodily_Preoccupations<- PTS_Bodily_Preoccupations + CI_Bodily_Preoccupations
      CI_Upper_Lim_Bodily_Preoccupations<- round(CI_Upper_Lim_Bodily_Preoccupations, digits = 2)
      CI_Lower_Lim_Bodily_Preoccupations<-PTS_Bodily_Preoccupations - CI_Bodily_Preoccupations
      CI_Lower_Lim_Bodily_Preoccupations<- round(CI_Lower_Lim_Bodily_Preoccupations, digits = 2)
      CI_Upper_Lim_Treatment_Experience<- PTS_Treatment_Experience + CI_Treatment_Experience
      CI_Upper_Lim_Treatment_Experience<- round(CI_Upper_Lim_Treatment_Experience, digits = 2)
      CI_Lower_Lim_Treatment_Experience<-PTS_Treatment_Experience - CI_Treatment_Experience
      CI_Lower_Lim_Treatment_Experience<- round(CI_Lower_Lim_Treatment_Experience, digits = 2)
      CI_Upper_Lim_Effects_Symptoms<- PTS_Effects_Symptoms + CI_Effects_Symptoms
      CI_Upper_Lim_Effects_Symptoms<- round(CI_Upper_Lim_Effects_Symptoms, digits = 2)
      CI_Lower_Lim_Effects_Symptoms<-PTS_Effects_Symptoms - CI_Effects_Symptoms
      CI_Lower_Lim_Effects_Symptoms<- round(CI_Lower_Lim_Effects_Symptoms, digits = 2)
      
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Worry_Illness == "2") {
        CI_Worry_Illness<- input$Man_CI_Worry_Illness
        CI_Worry_Illness<- c(CI_Worry_Illness, CI_Worry_Illness)
        CI_Worry_Illness<- round(CI_Worry_Illness, digits = 2)
        CI_Upper_Lim_Worry_Illness<- Score_Worry_Illness + CI_Worry_Illness
        CI_Upper_Lim_Worry_Illness<- round(CI_Upper_Lim_Worry_Illness, digits = 2)
        CI_Lower_Lim_Worry_Illness<- Score_Worry_Illness - CI_Worry_Illness
        CI_Lower_Lim_Worry_Illness<- round(CI_Lower_Lim_Worry_Illness, digits = 2)
      }
      if(input$Select_CI_Concerns_Pain == "2") {
        CI_Concerns_Pain<- input$Man_CI_Concerns_Pain
        CI_Concerns_Pain<- c(CI_Concerns_Pain, CI_Concerns_Pain)
        CI_Concerns_Pain<- round(CI_Concerns_Pain, digits = 2)
        CI_Upper_Lim_Concerns_Pain<- Score_Concerns_Pain + CI_Concerns_Pain
        CI_Upper_Lim_Concerns_Pain<- round(CI_Upper_Lim_Concerns_Pain, digits = 2)
        CI_Lower_Lim_Concerns_Pain<- Score_Concerns_Pain - CI_Concerns_Pain
        CI_Lower_Lim_Concerns_Pain<- round(CI_Lower_Lim_Concerns_Pain, digits = 2)
      }
      if(input$Select_CI_Health_Habits == "2") {
        CI_Health_Habits<- input$Man_CI_Health_Habits
        CI_Health_Habits<- c(CI_Health_Habits, CI_Health_Habits)
        CI_Health_Habits<- round(CI_Health_Habits, digits = 2)
        CI_Upper_Lim_Health_Habits<- Score_Health_Habits + CI_Health_Habits
        CI_Upper_Lim_Health_Habits<- round(CI_Upper_Lim_Health_Habits, digits = 2)
        CI_Lower_Lim_Health_Habits<- Score_Health_Habits - CI_Health_Habits
        CI_Lower_Lim_Health_Habits<- round(CI_Lower_Lim_Health_Habits, digits = 2)
      }
      if(input$Select_CI_Hypochondriacal_Beliefs == "2") {
        CI_Hypochondriacal_Beliefs<- input$Man_CI_Hypochondriacal_Beliefs
        CI_Hypochondriacal_Beliefs<- c(CI_Hypochondriacal_Beliefs, CI_Hypochondriacal_Beliefs)
        CI_Hypochondriacal_Beliefs<- round(CI_Hypochondriacal_Beliefs, digits = 2)
        CI_Upper_Lim_Hypochondriacal_Beliefs<- Score_Hypochondriacal_Beliefs + CI_Hypochondriacal_Beliefs
        CI_Upper_Lim_Hypochondriacal_Beliefs<- round(CI_Upper_Lim_Hypochondriacal_Beliefs, digits = 2)
        CI_Lower_Lim_Hypochondriacal_Beliefs<- Score_Hypochondriacal_Beliefs - CI_Hypochondriacal_Beliefs
        CI_Lower_Lim_Hypochondriacal_Beliefs<- round(CI_Lower_Lim_Hypochondriacal_Beliefs, digits = 2)
      }
      if(input$Select_CI_Thanatophobia == "2") {
        CI_Thanatophobia<- input$Man_CI_Thanatophobia
        CI_Thanatophobia<- c(CI_Thanatophobia,  CI_Thanatophobia)
        CI_Thanatophobia<- round(CI_Thanatophobia, digits = 2)
        CI_Upper_Lim_Thanatophobia<- Score_Thanatophobia + CI_Thanatophobia
        CI_Upper_Lim_Thanatophobia<- round(CI_Upper_Lim_Thanatophobia, digits = 2)
        CI_Lower_Lim_Thanatophobia<- Score_Thanatophobia - CI_Thanatophobia
        CI_Lower_Lim_Thanatophobia<- round(CI_Lower_Lim_Thanatophobia, digits = 2)
      }
      if(input$Select_CI_Disease_Phobia == "2") {
        CI_Disease_Phobia<- input$Man_CI_Disease_Phobia
        CI_Disease_Phobia<- c(CI_Disease_Phobia, CI_Disease_Phobia)
        CI_Disease_Phobia<- round(CI_Disease_Phobia, digits = 2)
        CI_Upper_Lim_Disease_Phobia<- Score_Disease_Phobia + CI_Disease_Phobia
        CI_Upper_Lim_Disease_Phobia<- round(CI_Upper_Lim_Disease_Phobia, digits = 2)
        CI_Lower_Lim_Disease_Phobia<- Score_Disease_Phobia - CI_Disease_Phobia
        CI_Lower_Lim_Disease_Phobia<- round(CI_Lower_Lim_Disease_Phobia, digits = 2)
      }
      if(input$Select_CI_Bodily_Preoccupations == "2") {
        CI_Bodily_Preoccupations<- input$Man_CI_Bodily_Preoccupations
        CI_Bodily_Preoccupations<- c(CI_Bodily_Preoccupations, CI_Bodily_Preoccupations)
        CI_Bodily_Preoccupations<- round(CI_Bodily_Preoccupations, digits = 2)
        CI_Upper_Lim_Bodily_Preoccupations<- Score_Bodily_Preoccupations + CI_Bodily_Preoccupations
        CI_Upper_Lim_Bodily_Preoccupations<- round(CI_Upper_Lim_Bodily_Preoccupations, digits = 2)
        CI_Lower_Lim_Bodily_Preoccupations<- Score_Bodily_Preoccupations - CI_Bodily_Preoccupations
        CI_Lower_Lim_Bodily_Preoccupations<- round(CI_Lower_Lim_Bodily_Preoccupations, digits = 2)
      }
      if(input$Select_CI_Treatment_Experience == "2") {
        CI_Treatment_Experience<- input$Man_CI_Treatment_Experience
        CI_Treatment_Experience<- round(CI_Treatment_Experience, digits = 2)
        CI_Treatment_Experience<- c(CI_Treatment_Experience,  CI_Treatment_Experience)
        CI_Upper_Lim_Treatment_Experience<- Score_Treatment_Experience + CI_Treatment_Experience
        CI_Upper_Lim_Treatment_Experience<- round(CI_Upper_Lim_Treatment_Experience, digits = 2)
        CI_Lower_Lim_Treatment_Experience<- Score_Treatment_Experience - CI_Treatment_Experience
        CI_Lower_Lim_Treatment_Experience<- round(CI_Lower_Lim_Treatment_Experience, digits = 2)
      }
      if(input$Select_CI_Effects_Symptoms == "2") {
        CI_Effects_Symptoms<- input$Man_CI_Effects_Symptoms
        CI_Effects_Symptoms<- c(CI_Effects_Symptoms, CI_Effects_Symptoms)
        CI_Effects_Symptoms<- round(CI_Effects_Symptoms, digits = 2)
        CI_Upper_Lim_Effects_Symptoms<- Score_Effects_Symptoms + CI_Effects_Symptoms
        CI_Upper_Lim_Effects_Symptoms<- round(CI_Upper_Lim_Effects_Symptoms, digits = 2)
        CI_Lower_Lim_Effects_Symptoms<- Score_Effects_Symptoms - CI_Effects_Symptoms
        CI_Lower_Lim_Effects_Symptoms<- round(CI_Lower_Lim_Effects_Symptoms, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Worry_Illness_1<- round(input$Cutoff_Worry_Illness_1, digits = 2)
      Cutoff_Score_Worry_Illness_2<- round(input$Cutoff_Worry_Illness_2, digits = 2)
      Cutoff_Score_Worry_Illness_3<- round(input$Cutoff_Worry_Illness_3, digits = 2)
      Cutoff_Score_Concerns_Pain_1<- round(input$Cutoff_Concerns_Pain_1, digits = 2)
      Cutoff_Score_Concerns_Pain_2<- round(input$Cutoff_Concerns_Pain_2, digits = 2)
      Cutoff_Score_Concerns_Pain_3<- round(input$Cutoff_Concerns_Pain_3, digits = 2)
      Cutoff_Score_Health_Habits_1<- round(input$Cutoff_Health_Habits_1, digits = 2)
      Cutoff_Score_Health_Habits_2<- round(input$Cutoff_Health_Habits_2, digits = 2)
      Cutoff_Score_Health_Habits_3<- round(input$Cutoff_Health_Habits_3, digits = 2)
      Cutoff_Score_Hypochondriacal_Beliefs_1<- round(input$Cutoff_Hypochondriacal_Beliefs_1, digits = 2)
      Cutoff_Score_Hypochondriacal_Beliefs_2<- round(input$Cutoff_Hypochondriacal_Beliefs_2, digits = 2)
      Cutoff_Score_Hypochondriacal_Beliefs_3<- round(input$Cutoff_Hypochondriacal_Beliefs_3, digits = 2)
      Cutoff_Score_Thanatophobia_1<- round(input$Cutoff_Thanatophobia_1, digits = 2)
      Cutoff_Score_Thanatophobia_2<- round(input$Cutoff_Thanatophobia_2, digits = 2)
      Cutoff_Score_Thanatophobia_3<- round(input$Cutoff_Thanatophobia_3, digits = 2)
      Cutoff_Score_Disease_Phobia_1<- round(input$Cutoff_Disease_Phobia_1, digits = 2)
      Cutoff_Score_Disease_Phobia_2<- round(input$Cutoff_Disease_Phobia_2, digits = 2)
      Cutoff_Score_Disease_Phobia_3<- round(input$Cutoff_Disease_Phobia_3, digits = 2)
      Cutoff_Score_Bodily_Preoccupations_1<- round(input$Cutoff_Bodily_Preoccupations_1, digits = 2)
      Cutoff_Score_Bodily_Preoccupations_2<- round(input$Cutoff_Bodily_Preoccupations_2, digits = 2)
      Cutoff_Score_Bodily_Preoccupations_3<- round(input$Cutoff_Bodily_Preoccupations_3, digits = 2)
      Cutoff_Score_Treatment_Experience_1<- round(input$Cutoff_Treatment_Experience_1, digits = 2)
      Cutoff_Score_Treatment_Experience_2<- round(input$Cutoff_Treatment_Experience_2, digits = 2)
      Cutoff_Score_Treatment_Experience_3<- round(input$Cutoff_Treatment_Experience_3, digits = 2)
      Cutoff_Score_Effects_Symptoms_1<- round(input$Cutoff_Effects_Symptoms_1, digits = 2)
      Cutoff_Score_Effects_Symptoms_2<- round(input$Cutoff_Effects_Symptoms_2, digits = 2)
      Cutoff_Score_Effects_Symptoms_3<- round(input$Cutoff_Effects_Symptoms_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Worry_Illness,Change_Worry_Illness,PTS_Worry_Illness, SE_Worry_Illness, CI_Upper_Lim_Worry_Illness, CI_Lower_Lim_Worry_Illness, Cutoff_Score_Worry_Illness_1,Cutoff_Score_Worry_Illness_2,Cutoff_Score_Worry_Illness_3,
                                      Score_Concerns_Pain,Change_Concerns_Pain, PTS_Concerns_Pain, SE_Concerns_Pain, CI_Upper_Lim_Concerns_Pain, CI_Lower_Lim_Concerns_Pain, Cutoff_Score_Concerns_Pain_1,Cutoff_Score_Concerns_Pain_2,Cutoff_Score_Concerns_Pain_3, 
                                      Score_Health_Habits,Change_Health_Habits,PTS_Health_Habits, SE_Health_Habits, CI_Upper_Lim_Health_Habits, CI_Lower_Lim_Health_Habits, Cutoff_Score_Health_Habits_1,Cutoff_Score_Health_Habits_2,Cutoff_Score_Health_Habits_3, 
                                      Score_Hypochondriacal_Beliefs,Change_Hypochondriacal_Beliefs,PTS_Hypochondriacal_Beliefs, SE_Hypochondriacal_Beliefs, CI_Upper_Lim_Hypochondriacal_Beliefs, CI_Lower_Lim_Hypochondriacal_Beliefs, Cutoff_Score_Hypochondriacal_Beliefs_1,Cutoff_Score_Hypochondriacal_Beliefs_2,Cutoff_Score_Hypochondriacal_Beliefs_3, 
                                      Score_Thanatophobia,Change_Thanatophobia,PTS_Thanatophobia, SE_Thanatophobia, CI_Upper_Lim_Thanatophobia, CI_Lower_Lim_Thanatophobia, Cutoff_Score_Thanatophobia_1,Cutoff_Score_Thanatophobia_2,Cutoff_Score_Thanatophobia_3,
                                      Score_Disease_Phobia,Change_Disease_Phobia, PTS_Disease_Phobia, SE_Disease_Phobia, CI_Upper_Lim_Disease_Phobia, CI_Lower_Lim_Disease_Phobia, Cutoff_Score_Disease_Phobia_1,Cutoff_Score_Disease_Phobia_2,Cutoff_Score_Disease_Phobia_3, 
                                      Score_Bodily_Preoccupations,Change_Bodily_Preoccupations,PTS_Bodily_Preoccupations, SE_Bodily_Preoccupations, CI_Upper_Lim_Bodily_Preoccupations, CI_Lower_Lim_Bodily_Preoccupations, Cutoff_Score_Bodily_Preoccupations_1,Cutoff_Score_Bodily_Preoccupations_2,Cutoff_Score_Bodily_Preoccupations_3, 
                                      Score_Treatment_Experience,Change_Treatment_Experience,PTS_Treatment_Experience, SE_Treatment_Experience, CI_Upper_Lim_Treatment_Experience, CI_Lower_Lim_Treatment_Experience, Cutoff_Score_Treatment_Experience_1,Cutoff_Score_Treatment_Experience_2,Cutoff_Score_Treatment_Experience_3, 
                                      Score_Effects_Symptoms,Change_Effects_Symptoms,PTS_Effects_Symptoms, SE_Effects_Symptoms, CI_Upper_Lim_Effects_Symptoms, CI_Lower_Lim_Effects_Symptoms, Cutoff_Score_Effects_Symptoms_1,Cutoff_Score_Effects_Symptoms_2,Cutoff_Score_Effects_Symptoms_3)
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
      Score_Worry_Illness_1<- sum(Score_1a[c(1,2,3)], na.rm = TRUE)
      Score_Worry_Illness_2<- sum(Score_2a[c(1,2,3)], na.rm = TRUE)
      Score_Worry_Illness_3<- sum(Score_3a[c(1,2,3)], na.rm = TRUE)
      Score_Worry_Illness<- c(Score_Worry_Illness_1, Score_Worry_Illness_2, Score_Worry_Illness_3)
      Score_Worry_Illness<- round(Score_Worry_Illness, digits = 2)
      Score_Concerns_Pain_1<- sum(Score_1a[c(4,5,6)], na.rm = TRUE)
      Score_Concerns_Pain_2<- sum(Score_2a[c(4,5,6)], na.rm = TRUE)
      Score_Concerns_Pain_3<- sum(Score_3a[c(4,5,6)], na.rm = TRUE)
      Score_Concerns_Pain<- c(Score_Concerns_Pain_1,Score_Concerns_Pain_2, Score_Concerns_Pain_3)
      Score_Concerns_Pain<- round(Score_Concerns_Pain, digits = 2)
      Score_Health_Habits_1<- sum(Score_1a[c(7,8,9)], na.rm = TRUE)
      Score_Health_Habits_2<- sum(Score_2a[c(7,8,9)], na.rm = TRUE)
      Score_Health_Habits_3<- sum(Score_3a[c(7,8,9)], na.rm = TRUE)
      Score_Health_Habits<- c(Score_Health_Habits_1, Score_Health_Habits_2, Score_Health_Habits_3)
      Score_Health_Habits<- round(Score_Health_Habits, digits = 2)
      Score_Hypochondriacal_Beliefs_1<- sum(Score_1a[c(10,11,12)], na.rm = TRUE)
      Score_Hypochondriacal_Beliefs_2<- sum(Score_2a[c(10,11,12)], na.rm = TRUE)
      Score_Hypochondriacal_Beliefs_3<- sum(Score_3a[c(10,11,12)], na.rm = TRUE)
      Score_Hypochondriacal_Beliefs<- c(Score_Hypochondriacal_Beliefs_1, Score_Hypochondriacal_Beliefs_2, Score_Hypochondriacal_Beliefs_3)
      Score_Hypochondriacal_Beliefs<- round(Score_Hypochondriacal_Beliefs, digits = 2)
      Score_Thanatophobia_1<- sum(Score_1a[c(13,14,15)], na.rm = TRUE)
      Score_Thanatophobia_2<- sum(Score_2a[c(13,14,15)], na.rm = TRUE)
      Score_Thanatophobia_3<- sum(Score_3a[c(13,14,15)], na.rm = TRUE)
      Score_Thanatophobia<- c(Score_Thanatophobia_1, Score_Thanatophobia_2, Score_Thanatophobia_3)
      Score_Thanatophobia<- round(Score_Thanatophobia, digits = 2)
      Score_Disease_Phobia_1<- sum(Score_1a[c(16,17,18)], na.rm = TRUE)
      Score_Disease_Phobia_2<- sum(Score_2a[c(16,17,18)], na.rm = TRUE)
      Score_Disease_Phobia_3<- sum(Score_3a[c(16,17,18)], na.rm = TRUE)
      Score_Disease_Phobia<- c(Score_Disease_Phobia_1, Score_Disease_Phobia_2, Score_Disease_Phobia_3)
      Score_Disease_Phobia<- round(Score_Disease_Phobia, digits = 2)
      Score_Bodily_Preoccupations_1<- sum(Score_1a[c(19,20,21)], na.rm = TRUE)
      Score_Bodily_Preoccupations_2<- sum(Score_2a[c(19,20,21)], na.rm = TRUE)
      Score_Bodily_Preoccupations_3<- sum(Score_3a[c(19,20,21)], na.rm = TRUE)
      Score_Bodily_Preoccupations<- c(Score_Bodily_Preoccupations_1, Score_Bodily_Preoccupations_2, Score_Bodily_Preoccupations_3)
      Score_Bodily_Preoccupations<- round(Score_Bodily_Preoccupations, digits = 2)
      Score_Treatment_Experience_1<- sum(Score_1a[c(22,23,24)], na.rm = TRUE)
      Score_Treatment_Experience_2<- sum(Score_2a[c(22,23,24)], na.rm = TRUE)
      Score_Treatment_Experience_3<- sum(Score_3a[c(22,23,24)], na.rm = TRUE)
      Score_Treatment_Experience<- c(Score_Treatment_Experience_1, Score_Treatment_Experience_2, Score_Treatment_Experience_3)
      Score_Treatment_Experience<- round(Score_Treatment_Experience, digits = 2)
      Score_Effects_Symptoms_1<- sum(Score_1a[c(25,26,27)], na.rm = TRUE)
      Score_Effects_Symptoms_2<- sum(Score_2a[c(25,26,27)], na.rm = TRUE)
      Score_Effects_Symptoms_3<- sum(Score_3a[c(25,26,27)], na.rm = TRUE)
      Score_Effects_Symptoms<- c(Score_Effects_Symptoms_1, Score_Effects_Symptoms_2, Score_Effects_Symptoms_3)
      Score_Effects_Symptoms<- round(Score_Effects_Symptoms, digits = 2)
      Change<- c(0, (Score_2 - Score_1), (Score_3 - Score_2))
      Change_Worry_Illness<- round(Change, digits = 2)
      Change_Concerns_Pain<- c(0, (Score_Concerns_Pain_2 - Score_Concerns_Pain_1), (Score_Concerns_Pain_3 - Score_Concerns_Pain_2))
      Change_Concerns_Pain<- round(Change_Concerns_Pain, digits = 2)
      Change_Health_Habits<- c(0, (Score_Health_Habits_2 - Score_Health_Habits_1),  (Score_Health_Habits_3 - Score_Health_Habits_2))
      Change_Health_Habits<- round(Change_Health_Habits, digits = 2)
      Change_Hypochondriacal_Beliefs<- c(0, (Score_Hypochondriacal_Beliefs_2 - Score_Hypochondriacal_Beliefs_1), (Score_Hypochondriacal_Beliefs_3 - Score_Hypochondriacal_Beliefs_2))
      Change_Hypochondriacal_Beliefs<- round(Change_Hypochondriacal_Beliefs, digits = 2)
      Change_Thanatophobia<- c(0, (Score_Thanatophobia_2 - Score_Thanatophobia_1), (Score_Thanatophobia_3 - Score_Thanatophobia_2))
      Change_Thanatophobia<- round(Change_Thanatophobia, digits = 2)
      Change_Disease_Phobia<- c(0, (Score_Disease_Phobia_2 - Score_Disease_Phobia_1), (Score_Disease_Phobia_3 - Score_Disease_Phobia_2))
      Change_Disease_Phobia<- round(Change_Disease_Phobia, digits = 2)
      Change_Bodily_Preoccupations<- c(0, (Score_Bodily_Preoccupations_2 - Score_Bodily_Preoccupations_1), (Score_Bodily_Preoccupations_3 - Score_Bodily_Preoccupations_2))
      Change_Bodily_Preoccupations<- round(Change_Bodily_Preoccupations, digits = 2)
      Change_Treatment_Experience<- c(0, (Score_Treatment_Experience_2 - Score_Treatment_Experience_1), (Score_Treatment_Experience_3 - Score_Treatment_Experience_2))
      Change_Treatment_Experience<- round(Change_Treatment_Experience, digits = 2)
      Change_Effects_Symptoms<- c(0, (Score_Effects_Symptoms_2 - Score_Effects_Symptoms_1),  (Score_Effects_Symptoms_3 - Score_Effects_Symptoms_2))
      Change_Effects_Symptoms<- round(Change_Effects_Symptoms, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS_3<- (Rel * Score_3) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Worry_Illness_1<- (Rel_Worry_Illness * Score_Worry_Illness_1) + (M_Worry_Illness * (1 - Rel_Worry_Illness))
        PTS_Worry_Illness_2<- (Rel_Worry_Illness * Score_Worry_Illness_2) + (M_Worry_Illness * (1 - Rel_Worry_Illness))
        PTS_Worry_Illness_3<- (Rel_Worry_Illness * Score_Worry_Illness_3) + (M_Worry_Illness * (1 - Rel_Worry_Illness))
        PTS_Worry_Illness<- c(PTS_Worry_Illness_1, PTS_Worry_Illness_2, PTS_Worry_Illness_3)
        PTS_Concerns_Pain_1<- (Rel_Concerns_Pain * Score_Concerns_Pain_1) + (M_Concerns_Pain * (1 - Rel_Concerns_Pain))
        PTS_Concerns_Pain_2<- (Rel_Concerns_Pain * Score_Concerns_Pain_2) + (M_Concerns_Pain * (1 - Rel_Concerns_Pain))
        PTS_Concerns_Pain_3<- (Rel_Concerns_Pain * Score_Concerns_Pain_3) + (M_Concerns_Pain * (1 - Rel_Concerns_Pain))
        PTS_Concerns_Pain<- c(PTS_Concerns_Pain_1, PTS_Concerns_Pain_2, PTS_Concerns_Pain_3)
        PTS_Health_Habits_1<- (Rel_Health_Habits * Score_Health_Habits_1) + (M_Health_Habits * (1 - Rel_Health_Habits))
        PTS_Health_Habits_2<- (Rel_Health_Habits * Score_Health_Habits_2) + (M_Health_Habits * (1 - Rel_Health_Habits))
        PTS_Health_Habits_3<- (Rel_Health_Habits * Score_Health_Habits_3) + (M_Health_Habits * (1 - Rel_Health_Habits))
        PTS_Health_Habits<- c(PTS_Health_Habits_1, PTS_Health_Habits_2, PTS_Health_Habits_3)
        PTS_Hypochondriacal_Beliefs_1<- (Rel_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_1) + (M_Hypochondriacal_Beliefs * (1 - Rel_Hypochondriacal_Beliefs))
        PTS_Hypochondriacal_Beliefs_2<- (Rel_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_2) + (M_Hypochondriacal_Beliefs * (1 - Rel_Hypochondriacal_Beliefs))
        PTS_Hypochondriacal_Beliefs_3<- (Rel_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_3) + (M_Hypochondriacal_Beliefs * (1 - Rel_Hypochondriacal_Beliefs))
        PTS_Hypochondriacal_Beliefs<- c(PTS_Hypochondriacal_Beliefs_1, PTS_Hypochondriacal_Beliefs_2,  PTS_Hypochondriacal_Beliefs_3)
        PTS_Thanatophobia_1<- (Rel_Thanatophobia * Score_Thanatophobia_1) + (M_Thanatophobia * (1 - Rel_Thanatophobia))
        PTS_Thanatophobia_2<- (Rel_Thanatophobia * Score_Thanatophobia_2) + (M_Thanatophobia * (1 - Rel_Thanatophobia))
        PTS_Thanatophobia_3<- (Rel_Thanatophobia * Score_Thanatophobia_3) + (M_Thanatophobia * (1 - Rel_Thanatophobia))
        PTS_Thanatophobia<- c(PTS_Thanatophobia_1, PTS_Thanatophobia_2, PTS_Thanatophobia_3)
        PTS_Disease_Phobia_1<- (Rel_Disease_Phobia * Score_Disease_Phobia_1) + (M_Disease_Phobia * (1 - Rel_Disease_Phobia))
        PTS_Disease_Phobia_2<- (Rel_Disease_Phobia * Score_Disease_Phobia_2) + (M_Disease_Phobia * (1 - Rel_Disease_Phobia))
        PTS_Disease_Phobia_3<- (Rel_Disease_Phobia * Score_Disease_Phobia_3) + (M_Disease_Phobia * (1 - Rel_Disease_Phobia))
        PTS_Disease_Phobia<- c(PTS_Disease_Phobia_1, PTS_Disease_Phobia_2, PTS_Disease_Phobia_3)
        PTS_Bodily_Preoccupations_1<- (Rel_Bodily_Preoccupations * Score_Bodily_Preoccupations_1) + (M_Bodily_Preoccupations * (1 - Rel_Bodily_Preoccupations))
        PTS_Bodily_Preoccupations_2<- (Rel_Bodily_Preoccupations * Score_Bodily_Preoccupations_2) + (M_Bodily_Preoccupations * (1 - Rel_Bodily_Preoccupations))
        PTS_Bodily_Preoccupations_3<- (Rel_Bodily_Preoccupations * Score_Bodily_Preoccupations_3) + (M_Bodily_Preoccupations * (1 - Rel_Bodily_Preoccupations))
        PTS_Bodily_Preoccupations<- c(PTS_Bodily_Preoccupations_1, PTS_Bodily_Preoccupations_2, PTS_Bodily_Preoccupations_3)
        PTS_Treatment_Experience_1<- (Rel_Treatment_Experience * Score_Treatment_Experience_1) + (M_Treatment_Experience * (1 - Rel_Treatment_Experience))
        PTS_Treatment_Experience_2<- (Rel_Treatment_Experience * Score_Treatment_Experience_2) + (M_Treatment_Experience * (1 - Rel_Treatment_Experience))
        PTS_Treatment_Experience_3<- (Rel_Treatment_Experience * Score_Treatment_Experience_3) + (M_Treatment_Experience * (1 - Rel_Treatment_Experience))
        PTS_Treatment_Experience<- c(PTS_Treatment_Experience_1, PTS_Treatment_Experience_2, PTS_Treatment_Experience_3)
        PTS_Effects_Symptoms_1<- (Rel_Effects_Symptoms * Score_Effects_Symptoms_1) + (M_Effects_Symptoms * (1 - Rel_Effects_Symptoms))
        PTS_Effects_Symptoms_2<- (Rel_Effects_Symptoms * Score_Effects_Symptoms_2) + (M_Effects_Symptoms * (1 - Rel_Effects_Symptoms))
        PTS_Effects_Symptoms_3<- (Rel_Effects_Symptoms * Score_Effects_Symptoms_3) + (M_Effects_Symptoms * (1 - Rel_Effects_Symptoms))
        PTS_Effects_Symptoms<- c(PTS_Effects_Symptoms_1, PTS_Effects_Symptoms_2, PTS_Effects_Symptoms_3)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Worry_Illness_1<- Score_Worry_Illness_1 + (M_Retest_Worry_Illness - M_Worry_Illness)  
        PTS_Worry_Illness_2<- Score_Worry_Illness_2 + (M_Retest_Worry_Illness - M_Worry_Illness) 
        PTS_Worry_Illness_3<- Score_Worry_Illness_3 + (M_Retest_Worry_Illness - M_Worry_Illness) 
        PTS_Worry_Illness<- c(PTS_Worry_Illness_1, PTS_Worry_Illness_2, PTS_Worry_Illness_3)
        PTS_Concerns_Pain_1<- Score_Concerns_Pain_1 + (M_Retest_Concerns_Pain - M_Concerns_Pain)  
        PTS_Concerns_Pain_2<- Score_Concerns_Pain_2 + (M_Retest_Concerns_Pain - M_Concerns_Pain) 
        PTS_Concerns_Pain_3<- Score_Concerns_Pain_3 + (M_Retest_Concerns_Pain - M_Concerns_Pain) 
        PTS_Concerns_Pain<- c(PTS_Concerns_Pain_1, PTS_Concerns_Pain_2, PTS_Concerns_Pain_3)
        PTS_Health_Habits_1<- Score_Health_Habits_1 + (M_Retest_Health_Habits - M_Health_Habits)  
        PTS_Health_Habits_2<- Score_Health_Habits_2 + (M_Retest_Health_Habits - M_Health_Habits) 
        PTS_Health_Habits_3<- Score_Health_Habits_3 + (M_Retest_Health_Habits - M_Health_Habits) 
        PTS_Health_Habits<- c(PTS_Health_Habits_1, PTS_Health_Habits_2, PTS_Health_Habits_3)
        PTS_Hypochondriacal_Beliefs_1<- Score_Hypochondriacal_Beliefs_1 + (M_Retest_Hypochondriacal_Beliefs - M_Hypochondriacal_Beliefs)  
        PTS_Hypochondriacal_Beliefs_2<- Score_Hypochondriacal_Beliefs_2 + (M_Retest_Hypochondriacal_Beliefs - M_Hypochondriacal_Beliefs) 
        PTS_Hypochondriacal_Beliefs_3<- Score_Hypochondriacal_Beliefs_3 + (M_Retest_Hypochondriacal_Beliefs - M_Hypochondriacal_Beliefs) 
        PTS_Hypochondriacal_Beliefs<- c(PTS_Hypochondriacal_Beliefs_1, PTS_Hypochondriacal_Beliefs_2, PTS_Hypochondriacal_Beliefs_3)
        PTS_Thanatophobia_1<- Score_Thanatophobia_1 + (M_Retest_Thanatophobia - M_Thanatophobia)  
        PTS_Thanatophobia_2<- Score_Thanatophobia_2 + (M_Retest_Thanatophobia - M_Thanatophobia) 
        PTS_Thanatophobia_3<- Score_Thanatophobia_3 + (M_Retest_Thanatophobia - M_Thanatophobia) 
        PTS_Thanatophobia<- c(PTS_Thanatophobia_1, PTS_Thanatophobia_2, PTS_Thanatophobia_3)
        PTS_Disease_Phobia_1<- Score_Disease_Phobia_1 + (M_Retest_Disease_Phobia - M_Disease_Phobia)  
        PTS_Disease_Phobia_2<- Score_Disease_Phobia_2 + (M_Retest_Disease_Phobia - M_Disease_Phobia) 
        PTS_Disease_Phobia_3<- Score_Disease_Phobia_3 + (M_Retest_Disease_Phobia - M_Disease_Phobia) 
        PTS_Disease_Phobia<- c(PTS_Disease_Phobia_1, PTS_Disease_Phobia_2, PTS_Disease_Phobia_3)
        PTS_Bodily_Preoccupations_1<- Score_Bodily_Preoccupations_1 + (M_Retest_Bodily_Preoccupations - M_Bodily_Preoccupations)  
        PTS_Bodily_Preoccupations_2<- Score_Bodily_Preoccupations_2 + (M_Retest_Bodily_Preoccupations - M_Bodily_Preoccupations) 
        PTS_Bodily_Preoccupations_3<- Score_Bodily_Preoccupations_3 + (M_Retest_Bodily_Preoccupations - M_Bodily_Preoccupations) 
        PTS_Bodily_Preoccupations<- c(PTS_Bodily_Preoccupations_1, PTS_Bodily_Preoccupations_2, PTS_Bodily_Preoccupations_3)
        PTS_Treatment_Experience_1<- Score_Treatment_Experience_1 + (M_Retest_Treatment_Experience - M_Treatment_Experience)  
        PTS_Treatment_Experience_2<- Score_Treatment_Experience_2 + (M_Retest_Treatment_Experience - M_Treatment_Experience) 
        PTS_Treatment_Experience_3<- Score_Treatment_Experience_3 + (M_Retest_Treatment_Experience - M_Treatment_Experience) 
        PTS_Treatment_Experience<- c(PTS_Treatment_Experience_1, PTS_Treatment_Experience_2, PTS_Treatment_Experience_3)
        PTS_Effects_Symptoms_1<- Score_Effects_Symptoms_1 + (M_Retest_Effects_Symptoms - M_Effects_Symptoms)  
        PTS_Effects_Symptoms_2<- Score_Effects_Symptoms_2 + (M_Retest_Effects_Symptoms - M_Effects_Symptoms) 
        PTS_Effects_Symptoms_3<- Score_Effects_Symptoms_3 + (M_Retest_Effects_Symptoms - M_Effects_Symptoms) 
        PTS_Effects_Symptoms<- c(PTS_Effects_Symptoms_1, PTS_Effects_Symptoms_2, PTS_Effects_Symptoms_3)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Worry_Illness<- Score_Worry_Illness
        PTS_Concerns_Pain<- Score_Concerns_Pain
        PTS_Health_Habits<- Score_Health_Habits
        PTS_Hypochondriacal_Beliefs<- Score_Hypochondriacal_Beliefs
        PTS_Thanatophobia<- Score_Thanatophobia
        PTS_Disease_Phobia<- Score_Disease_Phobia
        PTS_Bodily_Preoccupations<- Score_Bodily_Preoccupations
        PTS_Treatment_Experience<- Score_Treatment_Experience
        PTS_Effects_Symptoms<- Score_Effects_Symptoms
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS_3<- (B_Adj * Score_3) + A_Adj
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Worry_Illness<- Rel_Worry_Illness * (SD_Retest_Worry_Illness/SD_Worry_Illness)
        A_Constant_Worry_Illness<- M_Retest_Worry_Illness - (B_Slope_Worry_Illness * M_Worry_Illness)
        B_Adj_Worry_Illness<- SD_Retest_Worry_Illness/SD_Worry_Illness
        A_Adj_Worry_Illness<- M_Retest_Worry_Illness - (B_Adj_Worry_Illness * M_Worry_Illness)
        PTS_Worry_Illness_1<- (B_Adj_Worry_Illness * Score_Worry_Illness_1) + A_Adj_Worry_Illness
        PTS_Worry_Illness_2<- (B_Adj_Worry_Illness * Score_Worry_Illness_2) + A_Adj_Worry_Illness
        PTS_Worry_Illness_3<- (B_Adj_Worry_Illness * Score_Worry_Illness_3) + A_Adj_Worry_Illness
        PTS_Worry_Illness<- c(PTS_Worry_Illness_1,PTS_Worry_Illness_2, PTS_Worry_Illness_3)
        B_Slope_Concerns_Pain<- Rel_Concerns_Pain * (SD_Retest_Concerns_Pain/SD_Concerns_Pain)
        A_Constant_Concerns_Pain<- M_Retest_Concerns_Pain - (B_Slope_Concerns_Pain * M_Concerns_Pain)
        B_Adj_Concerns_Pain<- SD_Retest_Concerns_Pain/SD_Concerns_Pain
        A_Adj_Concerns_Pain<- M_Retest_Concerns_Pain - (B_Adj_Concerns_Pain * M_Concerns_Pain)
        PTS_Concerns_Pain_1<- (B_Adj_Concerns_Pain * Score_Concerns_Pain_1) + A_Adj_Concerns_Pain
        PTS_Concerns_Pain_2<- (B_Adj_Concerns_Pain * Score_Concerns_Pain_2) + A_Adj_Concerns_Pain
        PTS_Concerns_Pain_3<- (B_Adj_Concerns_Pain * Score_Concerns_Pain_3) + A_Adj_Concerns_Pain
        PTS_Concerns_Pain<- c(PTS_Concerns_Pain_1,PTS_Concerns_Pain_2, PTS_Concerns_Pain_3)
        B_Slope_Health_Habits<- Rel_Health_Habits * (SD_Retest_Health_Habits/SD_Health_Habits)
        A_Constant_Health_Habits<- M_Retest_Health_Habits - (B_Slope_Health_Habits * M_Health_Habits)
        B_Adj_Health_Habits<- SD_Retest_Health_Habits/SD_Health_Habits
        A_Adj_Health_Habits<- M_Retest_Health_Habits - (B_Adj_Health_Habits * M_Health_Habits)
        PTS_Health_Habits_1<- (B_Adj_Health_Habits * Score_Health_Habits_1) + A_Adj_Health_Habits
        PTS_Health_Habits_2<- (B_Adj_Health_Habits * Score_Health_Habits_2) + A_Adj_Health_Habits
        PTS_Health_Habits_3<- (B_Adj_Health_Habits * Score_Health_Habits_3) + A_Adj_Health_Habits
        PTS_Health_Habits<- c(PTS_Health_Habits_1,PTS_Health_Habits_2, PTS_Health_Habits_3)
        B_Slope_Hypochondriacal_Beliefs<- Rel_Hypochondriacal_Beliefs * (SD_Retest_Hypochondriacal_Beliefs/SD_Hypochondriacal_Beliefs)
        A_Constant_Hypochondriacal_Beliefs<- M_Retest_Hypochondriacal_Beliefs - (B_Slope_Hypochondriacal_Beliefs * M_Hypochondriacal_Beliefs)
        B_Adj_Hypochondriacal_Beliefs<- SD_Retest_Hypochondriacal_Beliefs/SD_Hypochondriacal_Beliefs
        A_Adj_Hypochondriacal_Beliefs<- M_Retest_Hypochondriacal_Beliefs - (B_Adj_Hypochondriacal_Beliefs * M_Hypochondriacal_Beliefs)
        PTS_Hypochondriacal_Beliefs_1<- (B_Adj_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_1) + A_Adj_Hypochondriacal_Beliefs
        PTS_Hypochondriacal_Beliefs_2<- (B_Adj_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_2) + A_Adj_Hypochondriacal_Beliefs
        PTS_Hypochondriacal_Beliefs_3<- (B_Adj_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_3) + A_Adj_Hypochondriacal_Beliefs
        PTS_Hypochondriacal_Beliefs<- c(PTS_Hypochondriacal_Beliefs_1,PTS_Hypochondriacal_Beliefs_2, PTS_Hypochondriacal_Beliefs_3)
        B_Slope_Thanatophobia<- Rel_Thanatophobia * (SD_Retest_Thanatophobia/SD_Thanatophobia)
        A_Constant_Thanatophobia<- M_Retest_Thanatophobia - (B_Slope_Thanatophobia * M_Thanatophobia)
        B_Adj_Thanatophobia<- SD_Retest_Thanatophobia/SD_Thanatophobia
        A_Adj_Thanatophobia<- M_Retest_Thanatophobia - (B_Adj_Thanatophobia * M_Thanatophobia)
        PTS_Thanatophobia_1<- (B_Adj_Thanatophobia * Score_Thanatophobia_1) + A_Adj_Thanatophobia
        PTS_Thanatophobia_2<- (B_Adj_Thanatophobia * Score_Thanatophobia_2) + A_Adj_Thanatophobia
        PTS_Thanatophobia_3<- (B_Adj_Thanatophobia * Score_Thanatophobia_3) + A_Adj_Thanatophobia
        PTS_Thanatophobia<- c(PTS_Thanatophobia_1,PTS_Thanatophobia_2, PTS_Thanatophobia_3)
        B_Slope_Disease_Phobia<- Rel_Disease_Phobia * (SD_Retest_Disease_Phobia/SD_Disease_Phobia)
        A_Constant_Disease_Phobia<- M_Retest_Disease_Phobia - (B_Slope_Disease_Phobia * M_Disease_Phobia)
        B_Adj_Disease_Phobia<- SD_Retest_Disease_Phobia/SD_Disease_Phobia
        A_Adj_Disease_Phobia<- M_Retest_Disease_Phobia - (B_Adj_Disease_Phobia * M_Disease_Phobia)
        PTS_Disease_Phobia_1<- (B_Adj_Disease_Phobia * Score_Disease_Phobia_1) + A_Adj_Disease_Phobia
        PTS_Disease_Phobia_2<- (B_Adj_Disease_Phobia * Score_Disease_Phobia_2) + A_Adj_Disease_Phobia
        PTS_Disease_Phobia_3<- (B_Adj_Disease_Phobia * Score_Disease_Phobia_3) + A_Adj_Disease_Phobia
        PTS_Disease_Phobia<- c(PTS_Disease_Phobia_1,PTS_Disease_Phobia_2, PTS_Disease_Phobia_3)
        B_Slope_Bodily_Preoccupations<- Rel_Bodily_Preoccupations * (SD_Retest_Bodily_Preoccupations/SD_Bodily_Preoccupations)
        A_Constant_Bodily_Preoccupations<- M_Retest_Bodily_Preoccupations - (B_Slope_Bodily_Preoccupations * M_Bodily_Preoccupations)
        B_Adj_Bodily_Preoccupations<- SD_Retest_Bodily_Preoccupations/SD_Bodily_Preoccupations
        A_Adj_Bodily_Preoccupations<- M_Retest_Bodily_Preoccupations - (B_Adj_Bodily_Preoccupations * M_Bodily_Preoccupations)
        PTS_Bodily_Preoccupations_1<- (B_Adj_Bodily_Preoccupations * Score_Bodily_Preoccupations_1) + A_Adj_Bodily_Preoccupations
        PTS_Bodily_Preoccupations_2<- (B_Adj_Bodily_Preoccupations * Score_Bodily_Preoccupations_2) + A_Adj_Bodily_Preoccupations
        PTS_Bodily_Preoccupations_3<- (B_Adj_Bodily_Preoccupations * Score_Bodily_Preoccupations_3) + A_Adj_Bodily_Preoccupations
        PTS_Bodily_Preoccupations<- c(PTS_Bodily_Preoccupations_1,PTS_Bodily_Preoccupations_2, PTS_Bodily_Preoccupations_3)
        B_Slope_Treatment_Experience<- Rel_Treatment_Experience * (SD_Retest_Treatment_Experience/SD_Treatment_Experience)
        A_Constant_Treatment_Experience<- M_Retest_Treatment_Experience - (B_Slope_Treatment_Experience * M_Treatment_Experience)
        B_Adj_Treatment_Experience<- SD_Retest_Treatment_Experience/SD_Treatment_Experience
        A_Adj_Treatment_Experience<- M_Retest_Treatment_Experience - (B_Adj_Treatment_Experience * M_Treatment_Experience)
        PTS_Treatment_Experience_1<- (B_Adj_Treatment_Experience * Score_Treatment_Experience_1) + A_Adj_Treatment_Experience
        PTS_Treatment_Experience_2<- (B_Adj_Treatment_Experience * Score_Treatment_Experience_2) + A_Adj_Treatment_Experience
        PTS_Treatment_Experience_3<- (B_Adj_Treatment_Experience * Score_Treatment_Experience_3) + A_Adj_Treatment_Experience
        PTS_Treatment_Experience<- c(PTS_Treatment_Experience_1,PTS_Treatment_Experience_2, PTS_Treatment_Experience_3)
        B_Slope_Effects_Symptoms<- Rel_Effects_Symptoms * (SD_Retest_Effects_Symptoms/SD_Effects_Symptoms)
        A_Constant_Effects_Symptoms<- M_Retest_Effects_Symptoms - (B_Slope_Effects_Symptoms * M_Effects_Symptoms)
        B_Adj_Effects_Symptoms<- SD_Retest_Effects_Symptoms/SD_Effects_Symptoms
        A_Adj_Effects_Symptoms<- M_Retest_Effects_Symptoms - (B_Adj_Effects_Symptoms * M_Effects_Symptoms)
        PTS_Effects_Symptoms_1<- (B_Adj_Effects_Symptoms * Score_Effects_Symptoms_1) + A_Adj_Effects_Symptoms
        PTS_Effects_Symptoms_2<- (B_Adj_Effects_Symptoms * Score_Effects_Symptoms_2) + A_Adj_Effects_Symptoms
        PTS_Effects_Symptoms_3<- (B_Adj_Effects_Symptoms * Score_Effects_Symptoms_3) + A_Adj_Effects_Symptoms
        PTS_Effects_Symptoms<- c(PTS_Effects_Symptoms_1,PTS_Effects_Symptoms_2, PTS_Effects_Symptoms_3)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS_3<- B_Slope*Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Worry_Illness<- Rel_Worry_Illness * (SD_Retest_Worry_Illness/SD_Worry_Illness)
        PTS_Worry_Illness_1<- B_Slope_Worry_Illness * Score_Worry_Illness_1
        PTS_Worry_Illness_2<- B_Slope_Worry_Illness * Score_Worry_Illness_2
        PTS_Worry_Illness_3<- B_Slope_Worry_Illness * Score_Worry_Illness_3
        PTS_Worry_Illness<- c(PTS_Worry_Illness_1, PTS_Worry_Illness_2, PTS_Worry_Illness_3)
        B_Slope_Concerns_Pain<- Rel_Concerns_Pain * (SD_Retest_Concerns_Pain/SD_Concerns_Pain)
        PTS_Concerns_Pain_1<- B_Slope_Concerns_Pain * Score_Concerns_Pain_1
        PTS_Concerns_Pain_2<- B_Slope_Concerns_Pain * Score_Concerns_Pain_2
        PTS_Concerns_Pain_3<- B_Slope_Concerns_Pain * Score_Concerns_Pain_3
        PTS_Concerns_Pain<- c(PTS_Concerns_Pain_1, PTS_Concerns_Pain_2, PTS_Concerns_Pain_3)
        B_Slope_Health_Habits<- Rel_Health_Habits * (SD_Retest_Health_Habits/SD_Health_Habits)
        PTS_Health_Habits_1<- B_Slope_Health_Habits * Score_Health_Habits_1
        PTS_Health_Habits_2<- B_Slope_Health_Habits * Score_Health_Habits_2
        PTS_Health_Habits_3<- B_Slope_Health_Habits * Score_Health_Habits_3
        PTS_Health_Habits<- c(PTS_Health_Habits_1, PTS_Health_Habits_2, PTS_Health_Habits_3)
        B_Slope_Hypochondriacal_Beliefs<- Rel_Hypochondriacal_Beliefs * (SD_Retest_Hypochondriacal_Beliefs/SD_Hypochondriacal_Beliefs)
        PTS_Hypochondriacal_Beliefs_1<- B_Slope_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_1
        PTS_Hypochondriacal_Beliefs_2<- B_Slope_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_2
        PTS_Hypochondriacal_Beliefs_3<- B_Slope_Hypochondriacal_Beliefs * Score_Hypochondriacal_Beliefs_3
        PTS_Hypochondriacal_Beliefs<- c(PTS_Hypochondriacal_Beliefs_1, PTS_Hypochondriacal_Beliefs_2, PTS_Hypochondriacal_Beliefs_3)
        B_Slope_Thanatophobia<- Rel_Thanatophobia * (SD_Retest_Thanatophobia/SD_Thanatophobia)
        PTS_Thanatophobia_1<- B_Slope_Thanatophobia * Score_Thanatophobia_1
        PTS_Thanatophobia_2<- B_Slope_Thanatophobia * Score_Thanatophobia_2
        PTS_Thanatophobia_3<- B_Slope_Thanatophobia * Score_Thanatophobia_3
        PTS_Thanatophobia<- c(PTS_Thanatophobia_1, PTS_Thanatophobia_2, PTS_Thanatophobia_3)
        B_Slope_Disease_Phobia<- Rel_Disease_Phobia * (SD_Retest_Disease_Phobia/SD_Disease_Phobia)
        PTS_Disease_Phobia_1<- B_Slope_Disease_Phobia * Score_Disease_Phobia_1
        PTS_Disease_Phobia_2<- B_Slope_Disease_Phobia * Score_Disease_Phobia_2
        PTS_Disease_Phobia_3<- B_Slope_Disease_Phobia * Score_Disease_Phobia_3
        PTS_Disease_Phobia<- c(PTS_Disease_Phobia_1, PTS_Disease_Phobia_2, PTS_Disease_Phobia_3)
        B_Slope_Bodily_Preoccupations<- Rel_Bodily_Preoccupations * (SD_Retest_Bodily_Preoccupations/SD_Bodily_Preoccupations)
        PTS_Bodily_Preoccupations_1<- B_Slope_Bodily_Preoccupations * Score_Bodily_Preoccupations_1
        PTS_Bodily_Preoccupations_2<- B_Slope_Bodily_Preoccupations * Score_Bodily_Preoccupations_2
        PTS_Bodily_Preoccupations_3<- B_Slope_Bodily_Preoccupations * Score_Bodily_Preoccupations_3
        PTS_Bodily_Preoccupations<- c(PTS_Bodily_Preoccupations_1, PTS_Bodily_Preoccupations_2, PTS_Bodily_Preoccupations_3) 
        B_Slope_Treatment_Experience<- Rel_Treatment_Experience * (SD_Retest_Treatment_Experience/SD_Treatment_Experience)
        PTS_Treatment_Experience_1<- B_Slope_Treatment_Experience * Score_Treatment_Experience_1
        PTS_Treatment_Experience_2<- B_Slope_Treatment_Experience * Score_Treatment_Experience_2
        PTS_Treatment_Experience_3<- B_Slope_Treatment_Experience * Score_Treatment_Experience_3
        PTS_Treatment_Experience<- c(PTS_Treatment_Experience_1, PTS_Treatment_Experience_2, PTS_Treatment_Experience_3)
        B_Slope_Effects_Symptoms<- Rel_Effects_Symptoms * (SD_Retest_Effects_Symptoms/SD_Effects_Symptoms)
        PTS_Effects_Symptoms_1<- B_Slope_Effects_Symptoms * Score_Effects_Symptoms_1
        PTS_Effects_Symptoms_2<- B_Slope_Effects_Symptoms * Score_Effects_Symptoms_2
        PTS_Effects_Symptoms_3<- B_Slope_Effects_Symptoms * Score_Effects_Symptoms_3
        PTS_Effects_Symptoms<- c(PTS_Effects_Symptoms_1, PTS_Effects_Symptoms_2, PTS_Effects_Symptoms_3)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Worry_Illness_1<- Score_Worry_Illness_1 + (M_Retest_Worry_Illness - M_Worry_Illness)
        PTS_Worry_Illness_2<- Score_Worry_Illness_2 + (M_Retest_Worry_Illness - M_Worry_Illness)
        PTS_Worry_Illness_3<- Score_Worry_Illness_3 + (M_Retest_Worry_Illness - M_Worry_Illness)
        PTS_Worry_Illness<- c(PTS_Worry_Illness_1, PTS_Worry_Illness_2, PTS_Worry_Illness_3)
        PTS_Concerns_Pain_1<- Score_Concerns_Pain_1 + (M_Retest_Concerns_Pain - M_Concerns_Pain)
        PTS_Concerns_Pain_2<- Score_Concerns_Pain_2 + (M_Retest_Concerns_Pain - M_Concerns_Pain)
        PTS_Concerns_Pain_3<- Score_Concerns_Pain_3 + (M_Retest_Concerns_Pain - M_Concerns_Pain)
        PTS_Concerns_Pain<- c(PTS_Concerns_Pain_1, PTS_Concerns_Pain_2, PTS_Concerns_Pain_3)
        PTS_Health_Habits_1<- Score_Health_Habits_1 + (M_Retest_Health_Habits - M_Health_Habits)
        PTS_Health_Habits_2<- Score_Health_Habits_2 + (M_Retest_Health_Habits - M_Health_Habits)
        PTS_Health_Habits_3<- Score_Health_Habits_3 + (M_Retest_Health_Habits - M_Health_Habits)
        PTS_Health_Habits<- c(PTS_Health_Habits_1, PTS_Health_Habits_2, PTS_Health_Habits_3)
        PTS_Hypochondriacal_Beliefs_1<- Score_Hypochondriacal_Beliefs_1 + (M_Retest_Hypochondriacal_Beliefs - M_Hypochondriacal_Beliefs)
        PTS_Hypochondriacal_Beliefs_2<- Score_Hypochondriacal_Beliefs_2 + (M_Retest_Hypochondriacal_Beliefs - M_Hypochondriacal_Beliefs)
        PTS_Hypochondriacal_Beliefs_3<- Score_Hypochondriacal_Beliefs_3 + (M_Retest_Hypochondriacal_Beliefs - M_Hypochondriacal_Beliefs)
        PTS_Hypochondriacal_Beliefs<- c(PTS_Hypochondriacal_Beliefs_1, PTS_Hypochondriacal_Beliefs_2, PTS_Hypochondriacal_Beliefs_3)
        PTS_Thanatophobia_1<- Score_Thanatophobia_1 + (M_Retest_Thanatophobia - M_Thanatophobia)
        PTS_Thanatophobia_2<- Score_Thanatophobia_2 + (M_Retest_Thanatophobia - M_Thanatophobia)
        PTS_Thanatophobia_3<- Score_Thanatophobia_3 + (M_Retest_Thanatophobia - M_Thanatophobia)
        PTS_Thanatophobia<- c(PTS_Thanatophobia_1, PTS_Thanatophobia_2, PTS_Thanatophobia_3)
        PTS_Disease_Phobia_1<- Score_Disease_Phobia_1 + (M_Retest_Disease_Phobia - M_Disease_Phobia)
        PTS_Disease_Phobia_2<- Score_Disease_Phobia_2 + (M_Retest_Disease_Phobia - M_Disease_Phobia)
        PTS_Disease_Phobia_3<- Score_Disease_Phobia_3 + (M_Retest_Disease_Phobia - M_Disease_Phobia)
        PTS_Disease_Phobia<- c(PTS_Disease_Phobia_1, PTS_Disease_Phobia_2, PTS_Disease_Phobia_3)
        PTS_Bodily_Preoccupations_1<- Score_Bodily_Preoccupations_1 + (M_Retest_Bodily_Preoccupations - M_Bodily_Preoccupations)
        PTS_Bodily_Preoccupations_2<- Score_Bodily_Preoccupations_2 + (M_Retest_Bodily_Preoccupations - M_Bodily_Preoccupations)
        PTS_Bodily_Preoccupations_3<- Score_Bodily_Preoccupations_3 + (M_Retest_Bodily_Preoccupations - M_Bodily_Preoccupations)
        PTS_Bodily_Preoccupations<- c(PTS_Bodily_Preoccupations_1, PTS_Bodily_Preoccupations_2, PTS_Bodily_Preoccupations_3)
        PTS_Treatment_Experience_1<- Score_Treatment_Experience_1 + (M_Retest_Treatment_Experience - M_Treatment_Experience)
        PTS_Treatment_Experience_2<- Score_Treatment_Experience_2 + (M_Retest_Treatment_Experience - M_Treatment_Experience)
        PTS_Treatment_Experience_3<- Score_Treatment_Experience_3 + (M_Retest_Treatment_Experience - M_Treatment_Experience)
        PTS_Treatment_Experience<- c(PTS_Treatment_Experience_1, PTS_Treatment_Experience_2, PTS_Treatment_Experience_3)
        PTS_Effects_Symptoms_1<- Score_Effects_Symptoms_1 + (M_Retest_Effects_Symptoms - M_Effects_Symptoms)
        PTS_Effects_Symptoms_2<- Score_Effects_Symptoms_2 + (M_Retest_Effects_Symptoms - M_Effects_Symptoms)
        PTS_Effects_Symptoms_3<- Score_Effects_Symptoms_3 + (M_Retest_Effects_Symptoms - M_Effects_Symptoms)
        PTS_Effects_Symptoms<- c(PTS_Effects_Symptoms_1, PTS_Effects_Symptoms_2, PTS_Effects_Symptoms_3)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Worry_Illness<- round(PTS_Worry_Illness, digits = 2)
      PTS_Concerns_Pain<- round(PTS_Concerns_Pain, digits = 2)
      PTS_Health_Habits<- round(PTS_Health_Habits, digits = 2)
      PTS_Hypochondriacal_Beliefs<- round(PTS_Hypochondriacal_Beliefs, digits = 2)
      PTS_Thanatophobia<- round(PTS_Thanatophobia, digits = 2)
      PTS_Disease_Phobia<- round(PTS_Disease_Phobia, digits = 2)
      PTS_Bodily_Preoccupations<- round(PTS_Bodily_Preoccupations, digits = 2)
      PTS_Treatment_Experience<- round(PTS_Treatment_Experience, digits = 2)
      PTS_Effects_Symptoms<- round(PTS_Effects_Symptoms, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE3<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_3 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2, SE3)
        SE_Worry_Illness_1<- McSweeny_SE_Worry_Illness*sqrt(1 + (1/SampleN) + ((Score_Worry_Illness_1 - M_Worry_Illness)^2/(SD_Worry_Illness^2*(SampleN-1))))
        SE_Worry_Illness_2<- McSweeny_SE_Worry_Illness*sqrt(1 + (1/SampleN) + ((Score_Worry_Illness_2 - M_Worry_Illness)^2/(SD_Worry_Illness^2*(SampleN-1))))
        SE_Worry_Illness_3<- McSweeny_SE_Worry_Illness*sqrt(1 + (1/SampleN) + ((Score_Worry_Illness_3 - M_Worry_Illness)^2/(SD_Worry_Illness^2*(SampleN-1))))
        SE_Worry_Illness<-c(SE_Worry_Illness_1, SE_Worry_Illness_2, SE_Worry_Illness_3)
        SE_Concerns_Pain_1<- McSweeny_SE_Concerns_Pain*sqrt(1 + (1/SampleN) + ((Score_Concerns_Pain_1 - M_Concerns_Pain)^2/(SD_Concerns_Pain^2*(SampleN-1))))
        SE_Concerns_Pain_2<- McSweeny_SE_Concerns_Pain*sqrt(1 + (1/SampleN) + ((Score_Concerns_Pain_2 - M_Concerns_Pain)^2/(SD_Concerns_Pain^2*(SampleN-1))))
        SE_Concerns_Pain_3<- McSweeny_SE_Concerns_Pain*sqrt(1 + (1/SampleN) + ((Score_Concerns_Pain_3 - M_Concerns_Pain)^2/(SD_Concerns_Pain^2*(SampleN-1))))
        SE_Concerns_Pain<-c(SE_Concerns_Pain_1, SE_Concerns_Pain_2, SE_Concerns_Pain_3)
        SE_Health_Habits_1<- McSweeny_SE_Health_Habits*sqrt(1 + (1/SampleN) + ((Score_Health_Habits_1 - M_Health_Habits)^2/(SD_Health_Habits^2*(SampleN-1))))
        SE_Health_Habits_2<- McSweeny_SE_Health_Habits*sqrt(1 + (1/SampleN) + ((Score_Health_Habits_2 - M_Health_Habits)^2/(SD_Health_Habits^2*(SampleN-1))))
        SE_Health_Habits_3<- McSweeny_SE_Health_Habits*sqrt(1 + (1/SampleN) + ((Score_Health_Habits_3 - M_Health_Habits)^2/(SD_Health_Habits^2*(SampleN-1))))
        SE_Health_Habits<-c(SE_Health_Habits_1, SE_Health_Habits_2, SE_Health_Habits_3)
        SE_Hypochondriacal_Beliefs_1<- McSweeny_SE_Hypochondriacal_Beliefs*sqrt(1 + (1/SampleN) + ((Score_Hypochondriacal_Beliefs_1 - M_Hypochondriacal_Beliefs)^2/(SD_Hypochondriacal_Beliefs^2*(SampleN-1))))
        SE_Hypochondriacal_Beliefs_2<- McSweeny_SE_Hypochondriacal_Beliefs*sqrt(1 + (1/SampleN) + ((Score_Hypochondriacal_Beliefs_2 - M_Hypochondriacal_Beliefs)^2/(SD_Hypochondriacal_Beliefs^2*(SampleN-1))))
        SE_Hypochondriacal_Beliefs_3<- McSweeny_SE_Hypochondriacal_Beliefs*sqrt(1 + (1/SampleN) + ((Score_Hypochondriacal_Beliefs_3 - M_Hypochondriacal_Beliefs)^2/(SD_Hypochondriacal_Beliefs^2*(SampleN-1))))
        SE_Hypochondriacal_Beliefs<-c(SE_Hypochondriacal_Beliefs_1, SE_Hypochondriacal_Beliefs_2, SE_Hypochondriacal_Beliefs_3)
        SE_Thanatophobia_1<- McSweeny_SE_Thanatophobia*sqrt(1 + (1/SampleN) + ((Score_Thanatophobia_1 - M_Thanatophobia)^2/(SD_Thanatophobia^2*(SampleN-1))))
        SE_Thanatophobia_2<- McSweeny_SE_Thanatophobia*sqrt(1 + (1/SampleN) + ((Score_Thanatophobia_2 - M_Thanatophobia)^2/(SD_Thanatophobia^2*(SampleN-1))))
        SE_Thanatophobia_3<- McSweeny_SE_Thanatophobia*sqrt(1 + (1/SampleN) + ((Score_Thanatophobia_3 - M_Thanatophobia)^2/(SD_Thanatophobia^2*(SampleN-1))))
        SE_Thanatophobia<-c(SE_Thanatophobia_1, SE_Thanatophobia_2, SE_Thanatophobia_3)
        SE_Disease_Phobia_1<- McSweeny_SE_Disease_Phobia*sqrt(1 + (1/SampleN) + ((Score_Disease_Phobia_1 - M_Disease_Phobia)^2/(SD_Disease_Phobia^2*(SampleN-1))))
        SE_Disease_Phobia_2<- McSweeny_SE_Disease_Phobia*sqrt(1 + (1/SampleN) + ((Score_Disease_Phobia_2 - M_Disease_Phobia)^2/(SD_Disease_Phobia^2*(SampleN-1))))
        SE_Disease_Phobia_3<- McSweeny_SE_Disease_Phobia*sqrt(1 + (1/SampleN) + ((Score_Disease_Phobia_3 - M_Disease_Phobia)^2/(SD_Disease_Phobia^2*(SampleN-1))))
        SE_Disease_Phobia<-c(SE_Disease_Phobia_1, SE_Disease_Phobia_2, SE_Disease_Phobia_3)
        SE_Bodily_Preoccupations_1<- McSweeny_SE_Bodily_Preoccupations*sqrt(1 + (1/SampleN) + ((Score_Bodily_Preoccupations_1 - M_Bodily_Preoccupations)^2/(SD_Bodily_Preoccupations^2*(SampleN-1))))
        SE_Bodily_Preoccupations_2<- McSweeny_SE_Bodily_Preoccupations*sqrt(1 + (1/SampleN) + ((Score_Bodily_Preoccupations_2 - M_Bodily_Preoccupations)^2/(SD_Bodily_Preoccupations^2*(SampleN-1))))
        SE_Bodily_Preoccupations_3<- McSweeny_SE_Bodily_Preoccupations*sqrt(1 + (1/SampleN) + ((Score_Bodily_Preoccupations_3 - M_Bodily_Preoccupations)^2/(SD_Bodily_Preoccupations^2*(SampleN-1))))
        SE_Bodily_Preoccupations<-c(SE_Bodily_Preoccupations_1, SE_Bodily_Preoccupations_2, SE_Bodily_Preoccupations_3)
        SE_Treatment_Experience_1<- McSweeny_SE_Treatment_Experience*sqrt(1 + (1/SampleN) + ((Score_Treatment_Experience_1 - M_Treatment_Experience)^2/(SD_Treatment_Experience^2*(SampleN-1))))
        SE_Treatment_Experience_2<- McSweeny_SE_Treatment_Experience*sqrt(1 + (1/SampleN) + ((Score_Treatment_Experience_2 - M_Treatment_Experience)^2/(SD_Treatment_Experience^2*(SampleN-1))))
        SE_Treatment_Experience_3<- McSweeny_SE_Treatment_Experience*sqrt(1 + (1/SampleN) + ((Score_Treatment_Experience_3 - M_Treatment_Experience)^2/(SD_Treatment_Experience^2*(SampleN-1))))
        SE_Treatment_Experience<-c(SE_Treatment_Experience_1, SE_Treatment_Experience_2, SE_Treatment_Experience_3)
        SE_Effects_Symptoms_1<- McSweeny_SE_Effects_Symptoms*sqrt(1 + (1/SampleN) + ((Score_Effects_Symptoms_1 - M_Effects_Symptoms)^2/(SD_Effects_Symptoms^2*(SampleN-1))))
        SE_Effects_Symptoms_2<- McSweeny_SE_Effects_Symptoms*sqrt(1 + (1/SampleN) + ((Score_Effects_Symptoms_2 - M_Effects_Symptoms)^2/(SD_Effects_Symptoms^2*(SampleN-1))))
        SE_Effects_Symptoms_3<- McSweeny_SE_Effects_Symptoms*sqrt(1 + (1/SampleN) + ((Score_Effects_Symptoms_3 - M_Effects_Symptoms)^2/(SD_Effects_Symptoms^2*(SampleN-1))))
        SE_Effects_Symptoms<-c(SE_Effects_Symptoms_1, SE_Effects_Symptoms_2, SE_Effects_Symptoms_3)
        SE<- round(SE, digits = 2)
        SE_Worry_Illness<- round(SE_Worry_Illness, digits = 2)
        SE_Concerns_Pain<- round(SE_Concerns_Pain, digits = 2)
        SE_Health_Habits<- round(SE_Health_Habits, digits = 2)
        SE_Hypochondriacal_Beliefs<- round(SE_Hypochondriacal_Beliefs, digits = 2)
        SE_Thanatophobia<- round(SE_Thanatophobia, digits = 2)
        SE_Disease_Phobia<- round(SE_Disease_Phobia, digits = 2)
        SE_Bodily_Preoccupations<- round(SE_Bodily_Preoccupations, digits = 2)
        SE_Treatment_Experience<- round(SE_Treatment_Experience, digits = 2)
        SE_Effects_Symptoms<- round(SE_Effects_Symptoms, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2), (Conf*SE3))
        CI<- round(CI, digits = 2)
        CI_Worry_Illness<- c((Conf*SE_Worry_Illness_1), (Conf*SE_Worry_Illness_2), (Conf*SE_Worry_Illness_3))
        CI_Worry_Illness<- round(CI_Worry_Illness, digits = 2)
        CI_Concerns_Pain<- c((Conf*SE_Concerns_Pain_1), (Conf*SE_Concerns_Pain_2), (Conf*SE_Concerns_Pain_3))
        CI_Concerns_Pain<- round(CI_Concerns_Pain, digits = 2)
        CI_Health_Habits<- c((Conf*SE_Health_Habits_1), (Conf*SE_Health_Habits_2), (Conf*SE_Health_Habits_3))
        CI_Health_Habits<- round(CI_Health_Habits, digits = 2)
        CI_Hypochondriacal_Beliefs<- c((Conf*SE_Hypochondriacal_Beliefs_1), (Conf*SE_Hypochondriacal_Beliefs_2), (Conf*SE_Hypochondriacal_Beliefs_3))
        CI_Hypochondriacal_Beliefs<- round(CI_Hypochondriacal_Beliefs, digits = 2)
        CI_Thanatophobia<- c((Conf*SE_Thanatophobia_1), (Conf*SE_Thanatophobia_2), (Conf*SE_Thanatophobia_3))
        CI_Thanatophobia<- round(CI_Thanatophobia, digits = 2)
        CI_Disease_Phobia<- c((Conf*SE_Disease_Phobia_1), (Conf*SE_Disease_Phobia_2), (Conf*SE_Disease_Phobia_3))
        CI_Disease_Phobia<- round(CI_Disease_Phobia, digits = 2)
        CI_Bodily_Preoccupations<- c((Conf*SE_Bodily_Preoccupations_1), (Conf*SE_Bodily_Preoccupations_2), (Conf*SE_Bodily_Preoccupations_3))
        CI_Bodily_Preoccupations<- round(CI_Bodily_Preoccupations, digits = 2)
        CI_Treatment_Experience<- c((Conf*SE_Treatment_Experience_1), (Conf*SE_Treatment_Experience_2), (Conf*SE_Treatment_Experience_3))
        CI_Treatment_Experience<- round(CI_Treatment_Experience, digits = 2)
        CI_Effects_Symptoms<- c((Conf*SE_Effects_Symptoms_1), (Conf*SE_Effects_Symptoms_2), (Conf*SE_Effects_Symptoms_3))
        CI_Effects_Symptoms<- round(CI_Effects_Symptoms, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Worry_Illness<- c((Conf*SE_Worry_Illness), (Conf*SE_Worry_Illness), (Conf*SE_Worry_Illness))
        CI_Worry_Illness<- round(CI_Worry_Illness, digits = 2)
        CI_Concerns_Pain<- c((Conf*SE_Concerns_Pain), (Conf*SE_Concerns_Pain), (Conf*SE_Concerns_Pain))
        CI_Concerns_Pain<- round(CI_Concerns_Pain, digits = 2)
        CI_Health_Habits<- c((Conf*SE_Health_Habits), (Conf*SE_Health_Habits), (Conf*SE_Health_Habits))
        CI_Health_Habits<- round(CI_Health_Habits, digits = 2)
        CI_Hypochondriacal_Beliefs<- c((Conf*SE_Hypochondriacal_Beliefs), (Conf*SE_Hypochondriacal_Beliefs), (Conf*SE_Hypochondriacal_Beliefs))
        CI_Hypochondriacal_Beliefs<- round(CI_Hypochondriacal_Beliefs, digits = 2)
        CI_Thanatophobia<- c((Conf*SE_Thanatophobia), (Conf*SE_Thanatophobia), (Conf*SE_Thanatophobia))
        CI_Thanatophobia<- round(CI_Thanatophobia, digits = 2)
        CI_Disease_Phobia<- c((Conf*SE_Disease_Phobia), (Conf*SE_Disease_Phobia), (Conf*SE_Disease_Phobia))
        CI_Disease_Phobia<- round(CI_Disease_Phobia, digits = 2)
        CI_Bodily_Preoccupations<- c((Conf*SE_Bodily_Preoccupations), (Conf*SE_Bodily_Preoccupations), (Conf*SE_Bodily_Preoccupations))
        CI_Bodily_Preoccupations<- round(CI_Bodily_Preoccupations, digits = 2)
        CI_Treatment_Experience<- c((Conf*SE_Treatment_Experience), (Conf*SE_Treatment_Experience))
        CI_Treatment_Experience<- round(CI_Treatment_Experience, digits = 2)
        CI_Effects_Symptoms<- c((Conf*SE_Effects_Symptoms), (Conf*SE_Effects_Symptoms), (Conf*SE_Effects_Symptoms))
        CI_Effects_Symptoms<- round(CI_Effects_Symptoms, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Worry_Illness<- PTS_Worry_Illness + CI_Worry_Illness
      CI_Upper_Lim_Worry_Illness<- round(CI_Upper_Lim_Worry_Illness, digits = 2)
      CI_Lower_Lim_Worry_Illness<-PTS_Worry_Illness - CI_Worry_Illness
      CI_Lower_Lim_Worry_Illness<- round(CI_Lower_Lim_Worry_Illness, digits = 2)
      CI_Upper_Lim_Concerns_Pain<- PTS_Concerns_Pain + CI_Concerns_Pain
      CI_Upper_Lim_Concerns_Pain<- round(CI_Upper_Lim_Concerns_Pain, digits = 2)
      CI_Lower_Lim_Concerns_Pain<-PTS_Concerns_Pain - CI_Concerns_Pain
      CI_Lower_Lim_Concerns_Pain<- round(CI_Lower_Lim_Concerns_Pain, digits = 2)
      CI_Upper_Lim_Health_Habits<- PTS_Health_Habits + CI_Health_Habits
      CI_Upper_Lim_Health_Habits<- round(CI_Upper_Lim_Health_Habits, digits = 2)
      CI_Lower_Lim_Health_Habits<-PTS_Health_Habits - CI_Health_Habits
      CI_Lower_Lim_Health_Habits<- round(CI_Lower_Lim_Health_Habits, digits = 2)
      CI_Upper_Lim_Hypochondriacal_Beliefs<- PTS_Hypochondriacal_Beliefs + CI_Hypochondriacal_Beliefs
      CI_Upper_Lim_Hypochondriacal_Beliefs<- round(CI_Upper_Lim_Hypochondriacal_Beliefs, digits = 2)
      CI_Lower_Lim_Hypochondriacal_Beliefs<-PTS_Hypochondriacal_Beliefs - CI_Hypochondriacal_Beliefs
      CI_Lower_Lim_Hypochondriacal_Beliefs<- round(CI_Lower_Lim_Hypochondriacal_Beliefs, digits = 2)
      CI_Upper_Lim_Thanatophobia<- PTS_Thanatophobia + CI_Thanatophobia
      CI_Upper_Lim_Thanatophobia<- round(CI_Upper_Lim_Thanatophobia, digits = 2)
      CI_Lower_Lim_Thanatophobia<-PTS_Thanatophobia - CI_Thanatophobia
      CI_Lower_Lim_Thanatophobia<- round(CI_Lower_Lim_Thanatophobia, digits = 2)
      CI_Upper_Lim_Disease_Phobia<- PTS_Disease_Phobia + CI_Disease_Phobia
      CI_Upper_Lim_Disease_Phobia<- round(CI_Upper_Lim_Disease_Phobia, digits = 2)
      CI_Lower_Lim_Disease_Phobia<-PTS_Disease_Phobia - CI_Disease_Phobia
      CI_Lower_Lim_Disease_Phobia<- round(CI_Lower_Lim_Disease_Phobia, digits = 2)
      CI_Upper_Lim_Bodily_Preoccupations<- PTS_Bodily_Preoccupations + CI_Bodily_Preoccupations
      CI_Upper_Lim_Bodily_Preoccupations<- round(CI_Upper_Lim_Bodily_Preoccupations, digits = 2)
      CI_Lower_Lim_Bodily_Preoccupations<-PTS_Bodily_Preoccupations - CI_Bodily_Preoccupations
      CI_Lower_Lim_Bodily_Preoccupations<- round(CI_Lower_Lim_Bodily_Preoccupations, digits = 2)
      CI_Upper_Lim_Treatment_Experience<- PTS_Treatment_Experience + CI_Treatment_Experience
      CI_Upper_Lim_Treatment_Experience<- round(CI_Upper_Lim_Treatment_Experience, digits = 2)
      CI_Lower_Lim_Treatment_Experience<-PTS_Treatment_Experience - CI_Treatment_Experience
      CI_Lower_Lim_Treatment_Experience<- round(CI_Lower_Lim_Treatment_Experience, digits = 2)
      CI_Upper_Lim_Effects_Symptoms<- PTS_Effects_Symptoms + CI_Effects_Symptoms
      CI_Upper_Lim_Effects_Symptoms<- round(CI_Upper_Lim_Effects_Symptoms, digits = 2)
      CI_Lower_Lim_Effects_Symptoms<-PTS_Effects_Symptoms - CI_Effects_Symptoms
      CI_Lower_Lim_Effects_Symptoms<- round(CI_Lower_Lim_Effects_Symptoms, digits = 2)
      
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Worry_Illness == "2") {
        CI_Worry_Illness<- input$Man_CI_Worry_Illness
        CI_Worry_Illness<- c(CI_Worry_Illness, CI_Worry_Illness, CI_Worry_Illness)
        CI_Worry_Illness<- round(CI_Worry_Illness, digits = 2)
        CI_Upper_Lim_Worry_Illness<- Score_Worry_Illness + CI_Worry_Illness
        CI_Upper_Lim_Worry_Illness<- round(CI_Upper_Lim_Worry_Illness, digits = 2)
        CI_Lower_Lim_Worry_Illness<- Score_Worry_Illness - CI_Worry_Illness
        CI_Lower_Lim_Worry_Illness<- round(CI_Lower_Lim_Worry_Illness, digits = 2)
      }
      if(input$Select_CI_Concerns_Pain == "2") {
        CI_Concerns_Pain<- input$Man_CI_Concerns_Pain
        CI_Concerns_Pain<- c(CI_Concerns_Pain, CI_Concerns_Pain, CI_Concerns_Pain)
        CI_Concerns_Pain<- round(CI_Concerns_Pain, digits = 2)
        CI_Upper_Lim_Concerns_Pain<- Score_Concerns_Pain + CI_Concerns_Pain
        CI_Upper_Lim_Concerns_Pain<- round(CI_Upper_Lim_Concerns_Pain, digits = 2)
        CI_Lower_Lim_Concerns_Pain<- Score_Concerns_Pain - CI_Concerns_Pain
        CI_Lower_Lim_Concerns_Pain<- round(CI_Lower_Lim_Concerns_Pain, digits = 2)
      }
      if(input$Select_CI_Health_Habits == "2") {
        CI_Health_Habits<- input$Man_CI_Health_Habits
        CI_Health_Habits<- c(CI_Health_Habits, CI_Health_Habits, CI_Health_Habits)
        CI_Health_Habits<- round(CI_Health_Habits, digits = 2)
        CI_Upper_Lim_Health_Habits<- Score_Health_Habits + CI_Health_Habits
        CI_Upper_Lim_Health_Habits<- round(CI_Upper_Lim_Health_Habits, digits = 2)
        CI_Lower_Lim_Health_Habits<- Score_Health_Habits - CI_Health_Habits
        CI_Lower_Lim_Health_Habits<- round(CI_Lower_Lim_Health_Habits, digits = 2)
      }
      if(input$Select_CI_Hypochondriacal_Beliefs == "2") {
        CI_Hypochondriacal_Beliefs<- input$Man_CI_Hypochondriacal_Beliefs
        CI_Hypochondriacal_Beliefs<- c(CI_Hypochondriacal_Beliefs, CI_Hypochondriacal_Beliefs, CI_Hypochondriacal_Beliefs)
        CI_Hypochondriacal_Beliefs<- round(CI_Hypochondriacal_Beliefs, digits = 2)
        CI_Upper_Lim_Hypochondriacal_Beliefs<- Score_Hypochondriacal_Beliefs + CI_Hypochondriacal_Beliefs
        CI_Upper_Lim_Hypochondriacal_Beliefs<- round(CI_Upper_Lim_Hypochondriacal_Beliefs, digits = 2)
        CI_Lower_Lim_Hypochondriacal_Beliefs<- Score_Hypochondriacal_Beliefs - CI_Hypochondriacal_Beliefs
        CI_Lower_Lim_Hypochondriacal_Beliefs<- round(CI_Lower_Lim_Hypochondriacal_Beliefs, digits = 2)
      }
      if(input$Select_CI_Thanatophobia == "2") {
        CI_Thanatophobia<- input$Man_CI_Thanatophobia
        CI_Thanatophobia<- c(CI_Thanatophobia,  CI_Thanatophobia, CI_Thanatophobia)
        CI_Thanatophobia<- round(CI_Thanatophobia, digits = 2)
        CI_Upper_Lim_Thanatophobia<- Score_Thanatophobia + CI_Thanatophobia
        CI_Upper_Lim_Thanatophobia<- round(CI_Upper_Lim_Thanatophobia, digits = 2)
        CI_Lower_Lim_Thanatophobia<- Score_Thanatophobia - CI_Thanatophobia
        CI_Lower_Lim_Thanatophobia<- round(CI_Lower_Lim_Thanatophobia, digits = 2)
      }
      if(input$Select_CI_Disease_Phobia == "2") {
        CI_Disease_Phobia<- input$Man_CI_Disease_Phobia
        CI_Disease_Phobia<- c(CI_Disease_Phobia, CI_Disease_Phobia, CI_Disease_Phobia)
        CI_Disease_Phobia<- round(CI_Disease_Phobia, digits = 2)
        CI_Upper_Lim_Disease_Phobia<- Score_Disease_Phobia + CI_Disease_Phobia
        CI_Upper_Lim_Disease_Phobia<- round(CI_Upper_Lim_Disease_Phobia, digits = 2)
        CI_Lower_Lim_Disease_Phobia<- Score_Disease_Phobia - CI_Disease_Phobia
        CI_Lower_Lim_Disease_Phobia<- round(CI_Lower_Lim_Disease_Phobia, digits = 2)
      }
      if(input$Select_CI_Bodily_Preoccupations == "2") {
        CI_Bodily_Preoccupations<- input$Man_CI_Bodily_Preoccupations
        CI_Bodily_Preoccupations<- c(CI_Bodily_Preoccupations, CI_Bodily_Preoccupations, CI_Bodily_Preoccupations)
        CI_Bodily_Preoccupations<- round(CI_Bodily_Preoccupations, digits = 2)
        CI_Upper_Lim_Bodily_Preoccupations<- Score_Bodily_Preoccupations + CI_Bodily_Preoccupations
        CI_Upper_Lim_Bodily_Preoccupations<- round(CI_Upper_Lim_Bodily_Preoccupations, digits = 2)
        CI_Lower_Lim_Bodily_Preoccupations<- Score_Bodily_Preoccupations - CI_Bodily_Preoccupations
        CI_Lower_Lim_Bodily_Preoccupations<- round(CI_Lower_Lim_Bodily_Preoccupations, digits = 2)
      }
      if(input$Select_CI_Treatment_Experience == "2") {
        CI_Treatment_Experience<- input$Man_CI_Treatment_Experience
        CI_Treatment_Experience<- round(CI_Treatment_Experience, digits = 2)
        CI_Treatment_Experience<- c(CI_Treatment_Experience,  CI_Treatment_Experience, CI_Treatment_Experience)
        CI_Upper_Lim_Treatment_Experience<- Score_Treatment_Experience + CI_Treatment_Experience
        CI_Upper_Lim_Treatment_Experience<- round(CI_Upper_Lim_Treatment_Experience, digits = 2)
        CI_Lower_Lim_Treatment_Experience<- Score_Treatment_Experience - CI_Treatment_Experience
        CI_Lower_Lim_Treatment_Experience<- round(CI_Lower_Lim_Treatment_Experience, digits = 2)
      }
      if(input$Select_CI_Effects_Symptoms == "2") {
        CI_Effects_Symptoms<- input$Man_CI_Effects_Symptoms
        CI_Effects_Symptoms<- c(CI_Effects_Symptoms, CI_Effects_Symptoms, CI_Effects_Symptoms)
        CI_Effects_Symptoms<- round(CI_Effects_Symptoms, digits = 2)
        CI_Upper_Lim_Effects_Symptoms<- Score_Effects_Symptoms + CI_Effects_Symptoms
        CI_Upper_Lim_Effects_Symptoms<- round(CI_Upper_Lim_Effects_Symptoms, digits = 2)
        CI_Lower_Lim_Effects_Symptoms<- Score_Effects_Symptoms - CI_Effects_Symptoms
        CI_Lower_Lim_Effects_Symptoms<- round(CI_Lower_Lim_Effects_Symptoms, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Worry_Illness_1<- round(input$Cutoff_Worry_Illness_1, digits = 2)
      Cutoff_Score_Worry_Illness_2<- round(input$Cutoff_Worry_Illness_2, digits = 2)
      Cutoff_Score_Worry_Illness_3<- round(input$Cutoff_Worry_Illness_3, digits = 2)
      Cutoff_Score_Concerns_Pain_1<- round(input$Cutoff_Concerns_Pain_1, digits = 2)
      Cutoff_Score_Concerns_Pain_2<- round(input$Cutoff_Concerns_Pain_2, digits = 2)
      Cutoff_Score_Concerns_Pain_3<- round(input$Cutoff_Concerns_Pain_3, digits = 2)
      Cutoff_Score_Health_Habits_1<- round(input$Cutoff_Health_Habits_1, digits = 2)
      Cutoff_Score_Health_Habits_2<- round(input$Cutoff_Health_Habits_2, digits = 2)
      Cutoff_Score_Health_Habits_3<- round(input$Cutoff_Health_Habits_3, digits = 2)
      Cutoff_Score_Hypochondriacal_Beliefs_1<- round(input$Cutoff_Hypochondriacal_Beliefs_1, digits = 2)
      Cutoff_Score_Hypochondriacal_Beliefs_2<- round(input$Cutoff_Hypochondriacal_Beliefs_2, digits = 2)
      Cutoff_Score_Hypochondriacal_Beliefs_3<- round(input$Cutoff_Hypochondriacal_Beliefs_3, digits = 2)
      Cutoff_Score_Thanatophobia_1<- round(input$Cutoff_Thanatophobia_1, digits = 2)
      Cutoff_Score_Thanatophobia_2<- round(input$Cutoff_Thanatophobia_2, digits = 2)
      Cutoff_Score_Thanatophobia_3<- round(input$Cutoff_Thanatophobia_3, digits = 2)
      Cutoff_Score_Disease_Phobia_1<- round(input$Cutoff_Disease_Phobia_1, digits = 2)
      Cutoff_Score_Disease_Phobia_2<- round(input$Cutoff_Disease_Phobia_2, digits = 2)
      Cutoff_Score_Disease_Phobia_3<- round(input$Cutoff_Disease_Phobia_3, digits = 2)
      Cutoff_Score_Bodily_Preoccupations_1<- round(input$Cutoff_Bodily_Preoccupations_1, digits = 2)
      Cutoff_Score_Bodily_Preoccupations_2<- round(input$Cutoff_Bodily_Preoccupations_2, digits = 2)
      Cutoff_Score_Bodily_Preoccupations_3<- round(input$Cutoff_Bodily_Preoccupations_3, digits = 2)
      Cutoff_Score_Treatment_Experience_1<- round(input$Cutoff_Treatment_Experience_1, digits = 2)
      Cutoff_Score_Treatment_Experience_2<- round(input$Cutoff_Treatment_Experience_2, digits = 2)
      Cutoff_Score_Treatment_Experience_3<- round(input$Cutoff_Treatment_Experience_3, digits = 2)
      Cutoff_Score_Effects_Symptoms_1<- round(input$Cutoff_Effects_Symptoms_1, digits = 2)
      Cutoff_Score_Effects_Symptoms_2<- round(input$Cutoff_Effects_Symptoms_2, digits = 2)
      Cutoff_Score_Effects_Symptoms_3<- round(input$Cutoff_Effects_Symptoms_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Worry_Illness,Change_Worry_Illness,PTS_Worry_Illness, SE_Worry_Illness, CI_Upper_Lim_Worry_Illness, CI_Lower_Lim_Worry_Illness, Cutoff_Score_Worry_Illness_1,Cutoff_Score_Worry_Illness_2,Cutoff_Score_Worry_Illness_3,
                                      Score_Concerns_Pain,Change_Concerns_Pain, PTS_Concerns_Pain, SE_Concerns_Pain, CI_Upper_Lim_Concerns_Pain, CI_Lower_Lim_Concerns_Pain, Cutoff_Score_Concerns_Pain_1,Cutoff_Score_Concerns_Pain_2,Cutoff_Score_Concerns_Pain_3, 
                                      Score_Health_Habits,Change_Health_Habits,PTS_Health_Habits, SE_Health_Habits, CI_Upper_Lim_Health_Habits, CI_Lower_Lim_Health_Habits, Cutoff_Score_Health_Habits_1,Cutoff_Score_Health_Habits_2,Cutoff_Score_Health_Habits_3, 
                                      Score_Hypochondriacal_Beliefs,Change_Hypochondriacal_Beliefs,PTS_Hypochondriacal_Beliefs, SE_Hypochondriacal_Beliefs, CI_Upper_Lim_Hypochondriacal_Beliefs, CI_Lower_Lim_Hypochondriacal_Beliefs, Cutoff_Score_Hypochondriacal_Beliefs_1,Cutoff_Score_Hypochondriacal_Beliefs_2,Cutoff_Score_Hypochondriacal_Beliefs_3, 
                                      Score_Thanatophobia,Change_Thanatophobia,PTS_Thanatophobia, SE_Thanatophobia, CI_Upper_Lim_Thanatophobia, CI_Lower_Lim_Thanatophobia, Cutoff_Score_Thanatophobia_1,Cutoff_Score_Thanatophobia_2,Cutoff_Score_Thanatophobia_3,
                                      Score_Disease_Phobia,Change_Disease_Phobia, PTS_Disease_Phobia, SE_Disease_Phobia, CI_Upper_Lim_Disease_Phobia, CI_Lower_Lim_Disease_Phobia, Cutoff_Score_Disease_Phobia_1,Cutoff_Score_Disease_Phobia_2,Cutoff_Score_Disease_Phobia_3, 
                                      Score_Bodily_Preoccupations,Change_Bodily_Preoccupations,PTS_Bodily_Preoccupations, SE_Bodily_Preoccupations, CI_Upper_Lim_Bodily_Preoccupations, CI_Lower_Lim_Bodily_Preoccupations, Cutoff_Score_Bodily_Preoccupations_1,Cutoff_Score_Bodily_Preoccupations_2,Cutoff_Score_Bodily_Preoccupations_3, 
                                      Score_Treatment_Experience,Change_Treatment_Experience,PTS_Treatment_Experience, SE_Treatment_Experience, CI_Upper_Lim_Treatment_Experience, CI_Lower_Lim_Treatment_Experience, Cutoff_Score_Treatment_Experience_1,Cutoff_Score_Treatment_Experience_2,Cutoff_Score_Treatment_Experience_3, 
                                      Score_Effects_Symptoms,Change_Effects_Symptoms,PTS_Effects_Symptoms, SE_Effects_Symptoms, CI_Upper_Lim_Effects_Symptoms, CI_Lower_Lim_Effects_Symptoms, Cutoff_Score_Effects_Symptoms_1,Cutoff_Score_Effects_Symptoms_2,Cutoff_Score_Effects_Symptoms_3)
    }
    
    
    #Create dataframes to be displayed as tables in pdf report (statistical values used in reliable change analyses)
    
    if(input$RelChangeMethod == 'Nunnally & Bernstein (1994)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Worry_Illness<<- data.frame(Pop,  M_Worry_Illness, SD_Worry_Illness, RelChangeMethod, Rel_Worry_Illness, ConfInt)
      names(Stats_Table_Worry_Illness)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Concerns_Pain<<- data.frame(Pop,  M_Concerns_Pain, SD_Concerns_Pain, RelChangeMethod, Rel_Concerns_Pain, ConfInt)
      names(Stats_Table_Concerns_Pain)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Health_Habits<<- data.frame(Pop,  M_Health_Habits, SD_Health_Habits, RelChangeMethod, Rel_Health_Habits, ConfInt)
      names(Stats_Table_Health_Habits)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Hypochondriacal_Beliefs<<- data.frame(Pop,  M_Hypochondriacal_Beliefs, SD_Hypochondriacal_Beliefs, RelChangeMethod, Rel_Hypochondriacal_Beliefs, ConfInt)
      names(Stats_Table_Hypochondriacal_Beliefs)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Thanatophobia<<- data.frame(Pop,  M_Thanatophobia, SD_Thanatophobia, RelChangeMethod, Rel_Thanatophobia, ConfInt)
      names(Stats_Table_Thanatophobia)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Disease_Phobia<<- data.frame(Pop,  M_Disease_Phobia, SD_Disease_Phobia, RelChangeMethod, Rel_Disease_Phobia, ConfInt)
      names(Stats_Table_Disease_Phobia)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Bodily_Preoccupations<<- data.frame(Pop,  M_Bodily_Preoccupations, SD_Bodily_Preoccupations, RelChangeMethod, Rel_Bodily_Preoccupations, ConfInt)
      names(Stats_Table_Bodily_Preoccupations)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Treatment_Experience<<- data.frame(Pop,  M_Treatment_Experience, SD_Treatment_Experience, RelChangeMethod, Rel_Treatment_Experience, ConfInt)
      names(Stats_Table_Treatment_Experience)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Effects_Symptoms<<- data.frame(Pop,  M_Effects_Symptoms, SD_Effects_Symptoms, RelChangeMethod, Rel_Effects_Symptoms, ConfInt)
      names(Stats_Table_Effects_Symptoms)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Chelune et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Worry_Illness<<- data.frame(Pop,  M_Worry_Illness, M_Retest_Worry_Illness, SD_Worry_Illness, RelChangeMethod, Rel_Worry_Illness, ConfInt)
      names(Stats_Table_Worry_Illness)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Concerns_Pain<<- data.frame(Pop,  M_Concerns_Pain, M_Retest_Concerns_Pain, SD_Concerns_Pain, RelChangeMethod, Rel_Concerns_Pain, ConfInt)
      names(Stats_Table_Concerns_Pain)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Health_Habits<<- data.frame(Pop,  M_Health_Habits, M_Retest_Health_Habits, SD_Health_Habits, RelChangeMethod, Rel_Health_Habits, ConfInt)
      names(Stats_Table_Health_Habits)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Hypochondriacal_Beliefs<<- data.frame(Pop,  M_Hypochondriacal_Beliefs, M_Retest_Hypochondriacal_Beliefs, SD_Hypochondriacal_Beliefs, RelChangeMethod, Rel_Hypochondriacal_Beliefs, ConfInt)
      names(Stats_Table_Hypochondriacal_Beliefs)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      
      Stats_Table_Thanatophobia<<- data.frame(Pop,  M_Thanatophobia, M_Retest_Thanatophobia, SD_Thanatophobia, RelChangeMethod, Rel_Thanatophobia, ConfInt)
      names(Stats_Table_Thanatophobia)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Disease_Phobia<<- data.frame(Pop,  M_Disease_Phobia, M_Retest_Disease_Phobia, SD_Disease_Phobia, RelChangeMethod, Rel_Disease_Phobia, ConfInt)
      names(Stats_Table_Disease_Phobia)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Bodily_Preoccupations<<- data.frame(Pop,  M_Bodily_Preoccupations, M_Retest_Bodily_Preoccupations, SD_Bodily_Preoccupations, RelChangeMethod, Rel_Bodily_Preoccupations, ConfInt)
      names(Stats_Table_Bodily_Preoccupations)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Treatment_Experience<<- data.frame(Pop,  M_Treatment_Experience, M_Retest_Treatment_Experience, SD_Treatment_Experience, RelChangeMethod, Rel_Treatment_Experience, ConfInt)
      names(Stats_Table_Treatment_Experience)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Effects_Symptoms<<- data.frame(Pop,  M_Effects_Symptoms, M_Retest_Effects_Symptoms, SD_Effects_Symptoms, RelChangeMethod, Rel_Effects_Symptoms, ConfInt)
      names(Stats_Table_Effects_Symptoms)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Maassen et al. (2006)' | input$RelChangeMethod == 'McSweeny et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Worry_Illness<<- data.frame(Pop,  M_Worry_Illness, M_Retest_Worry_Illness, SD_Worry_Illness, SD_Retest_Worry_Illness, RelChangeMethod, Rel_Worry_Illness, ConfInt)
      names(Stats_Table_Worry_Illness)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Concerns_Pain<<- data.frame(Pop,  M_Concerns_Pain, M_Retest_Concerns_Pain, SD_Concerns_Pain, SD_Retest_Concerns_Pain, RelChangeMethod, Rel_Concerns_Pain, ConfInt)
      names(Stats_Table_Concerns_Pain)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Health_Habits<<- data.frame(Pop,  M_Health_Habits, M_Retest_Health_Habits, SD_Health_Habits, SD_Retest_Health_Habits, RelChangeMethod, Rel_Health_Habits, ConfInt)
      names(Stats_Table_Health_Habits)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Hypochondriacal_Beliefs<<- data.frame(Pop,  M_Hypochondriacal_Beliefs, M_Retest_Hypochondriacal_Beliefs, SD_Hypochondriacal_Beliefs, SD_Retest_Hypochondriacal_Beliefs, RelChangeMethod, Rel_Hypochondriacal_Beliefs, ConfInt)
      names(Stats_Table_Hypochondriacal_Beliefs)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Thanatophobia<<- data.frame(Pop,  M_Thanatophobia, M_Retest_Thanatophobia, SD_Thanatophobia, SD_Retest_Thanatophobia, RelChangeMethod, Rel_Thanatophobia, ConfInt)
      names(Stats_Table_Thanatophobia)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Disease_Phobia<<- data.frame(Pop,  M_Disease_Phobia, M_Retest_Disease_Phobia, SD_Disease_Phobia, SD_Retest_Disease_Phobia, RelChangeMethod, Rel_Disease_Phobia, ConfInt)
      names(Stats_Table_Disease_Phobia)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Bodily_Preoccupations<<- data.frame(Pop,  M_Bodily_Preoccupations, M_Retest_Bodily_Preoccupations, SD_Bodily_Preoccupations, SD_Retest_Bodily_Preoccupations, RelChangeMethod, Rel_Bodily_Preoccupations, ConfInt)
      names(Stats_Table_Bodily_Preoccupations)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Treatment_Experience<<- data.frame(Pop,  M_Treatment_Experience, M_Retest_Treatment_Experience, SD_Treatment_Experience, SD_Retest_Treatment_Experience, RelChangeMethod, Rel_Treatment_Experience, ConfInt)
      names(Stats_Table_Treatment_Experience)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Effects_Symptoms<<- data.frame(Pop,  M_Effects_Symptoms, M_Retest_Effects_Symptoms, SD_Effects_Symptoms, SD_Retest_Effects_Symptoms, RelChangeMethod, Rel_Effects_Symptoms, ConfInt)
      names(Stats_Table_Effects_Symptoms)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      
    } else if (input$RelChangeMethod == 'Crawford & Howell (1998)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, SampleN, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Worry_Illness<<- data.frame(Pop,  M_Worry_Illness, M_Retest_Worry_Illness, SD_Worry_Illness, SD_Retest_Worry_Illness, RelChangeMethod, Rel_Worry_Illness, SampleN,ConfInt)
      names(Stats_Table_Worry_Illness)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Concerns_Pain<<- data.frame(Pop,  M_Concerns_Pain, M_Retest_Concerns_Pain, SD_Concerns_Pain, SD_Retest_Concerns_Pain, RelChangeMethod, Rel_Concerns_Pain, SampleN, ConfInt)
      names(Stats_Table_Concerns_Pain)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Health_Habits<<- data.frame(Pop,  M_Health_Habits, M_Retest_Health_Habits, SD_Health_Habits, SD_Retest_Health_Habits, RelChangeMethod, Rel_Health_Habits, SampleN,ConfInt)
      names(Stats_Table_Health_Habits)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Hypochondriacal_Beliefs<<- data.frame(Pop,  M_Hypochondriacal_Beliefs, M_Retest_Hypochondriacal_Beliefs, SD_Hypochondriacal_Beliefs, SD_Retest_Hypochondriacal_Beliefs, RelChangeMethod, Rel_Hypochondriacal_Beliefs, SampleN,ConfInt)
      names(Stats_Table_Hypochondriacal_Beliefs)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Thanatophobia<<- data.frame(Pop,  M_Thanatophobia, M_Retest_Thanatophobia, SD_Thanatophobia, SD_Retest_Thanatophobia, RelChangeMethod, Rel_Thanatophobia, SampleN,ConfInt)
      names(Stats_Table_Thanatophobia)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Disease_Phobia<<- data.frame(Pop,  M_Disease_Phobia, M_Retest_Disease_Phobia, SD_Disease_Phobia, SD_Retest_Disease_Phobia, RelChangeMethod, Rel_Disease_Phobia, SampleN, ConfInt)
      names(Stats_Table_Disease_Phobia)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Bodily_Preoccupations<<- data.frame(Pop,  M_Bodily_Preoccupations, M_Retest_Bodily_Preoccupations, SD_Bodily_Preoccupations, SD_Retest_Bodily_Preoccupations, RelChangeMethod, Rel_Bodily_Preoccupations, SampleN,ConfInt)
      names(Stats_Table_Bodily_Preoccupations)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Treatment_Experience<<- data.frame(Pop,  M_Treatment_Experience, M_Retest_Treatment_Experience, SD_Treatment_Experience, SD_Retest_Treatment_Experience, RelChangeMethod, Rel_Treatment_Experience, SampleN,ConfInt)
      names(Stats_Table_Treatment_Experience)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Effects_Symptoms<<- data.frame(Pop,  M_Effects_Symptoms, M_Retest_Effects_Symptoms, SD_Effects_Symptoms, SD_Retest_Effects_Symptoms, RelChangeMethod, Rel_Effects_Symptoms, SampleN,ConfInt)
      names(Stats_Table_Effects_Symptoms)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
    } else if (input$RelChangeMethod == 'Jacobson & Truax (1991)' | input$RelChangeMethod == 'Speer (1992)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Worry_Illness<<- data.frame(Pop,  SD_Worry_Illness, RelChangeMethod, Rel_Worry_Illness, ConfInt)
      names(Stats_Table_Worry_Illness)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Concerns_Pain<<- data.frame(Pop,  SD_Concerns_Pain, RelChangeMethod, Rel_Concerns_Pain, ConfInt)
      names(Stats_Table_Concerns_Pain)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Health_Habits<<- data.frame(Pop,  SD_Health_Habits, RelChangeMethod, Rel_Health_Habits, ConfInt)
      names(Stats_Table_Health_Habits)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Hypochondriacal_Beliefs<<- data.frame(Pop,  SD_Hypochondriacal_Beliefs, RelChangeMethod, Rel_Hypochondriacal_Beliefs, ConfInt)
      names(Stats_Table_Hypochondriacal_Beliefs)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Thanatophobia<<- data.frame(Pop,  SD_Thanatophobia, RelChangeMethod, Rel_Thanatophobia, ConfInt)
      names(Stats_Table_Thanatophobia)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Disease_Phobia<<- data.frame(Pop,  SD_Disease_Phobia, RelChangeMethod, Rel_Disease_Phobia, ConfInt)
      names(Stats_Table_Disease_Phobia)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Bodily_Preoccupations<<- data.frame(Pop,  SD_Bodily_Preoccupations, RelChangeMethod, Rel_Bodily_Preoccupations, ConfInt)
      names(Stats_Table_Bodily_Preoccupations)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Treatment_Experience<<- data.frame(Pop,  SD_Treatment_Experience, RelChangeMethod, Rel_Treatment_Experience, ConfInt)
      names(Stats_Table_Treatment_Experience)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Effects_Symptoms<<- data.frame(Pop,  SD_Effects_Symptoms, RelChangeMethod, Rel_Effects_Symptoms, ConfInt)
      names(Stats_Table_Effects_Symptoms)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      
    }
    
    #If custom confidence intervals options are selected, make sure that no values appear for PS and SE in pdf tables
    #Change the look of tables in report if custom confidence intervals options are selected
    
    if (input$Select_CI == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS = NA, SE = NA)
      Stats_Table_Fullscale<<- Stats_Table_Fullscale %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI[1])
    }
    if (input$Select_CI_Worry_Illness == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Worry_Illness = NA, SE_Worry_Illness = NA)
      Stats_Table_Worry_Illness<<- Stats_Table_Worry_Illness %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Worry_Illness[1])
    }
    if (input$Select_CI_Concerns_Pain == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Concerns_Pain = NA, SE_Concerns_Pain = NA)
      Stats_Table_Concerns_Pain<<- Stats_Table_Concerns_Pain %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                      "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Concerns_Pain[1])
    }
    if (input$Select_CI_Health_Habits == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Health_Habits = NA, SE_Health_Habits = NA)
      Stats_Table_Health_Habits<<- Stats_Table_Health_Habits %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                      "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Health_Habits[1])
    }
    if (input$Select_CI_Hypochondriacal_Beliefs == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Hypochondriacal_Beliefs = NA, SE_Hypochondriacal_Beliefs = NA)
      Stats_Table_Hypochondriacal_Beliefs<<- Stats_Table_Hypochondriacal_Beliefs %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Hypochondriacal_Beliefs[1])
    }
    if (input$Select_CI_Thanatophobia == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Thanatophobia = NA, SE_Thanatophobia = NA)
      Stats_Table_Thanatophobia<<- Stats_Table_Thanatophobia %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                    "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Thanatophobia[1])
    }
    if (input$Select_CI_Disease_Phobia == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Disease_Phobia = NA, SE_Disease_Phobia = NA)
      Stats_Table_Disease_Phobia<<- Stats_Table_Disease_Phobia %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                                        "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Disease_Phobia[1])
    }
    if (input$Select_CI_Bodily_Preoccupations == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Bodily_Preoccupations = NA, SE_Bodily_Preoccupations = NA)
      Stats_Table_Bodily_Preoccupations<<- Stats_Table_Bodily_Preoccupations %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                    "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Bodily_Preoccupations[1])
    }
    if (input$Select_CI_Treatment_Experience == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Treatment_Experience = NA, SE_Treatment_Experience = NA)
      Stats_Table_Treatment_Experience<<- Stats_Table_Treatment_Experience %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                        "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Treatment_Experience[1])
    }
    if (input$Select_CI_Effects_Symptoms == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Effects_Symptoms = NA, SE_Effects_Symptoms = NA)
      Stats_Table_Effects_Symptoms<<- Stats_Table_Effects_Symptoms %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Effects_Symptoms[1])
    }
    
    
    
    
    #Create a dataframe to be used in the analytics spreadsheet.
    
    #Calculate variables for significant improvement
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$CI_Lower_Lim[1]) {
      IAS.Fullscale.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$CI_Lower_Lim[1]) {
      IAS.Fullscale.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$CI_Upper_Lim[1]) {
      IAS.Fullscale.Sig.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$CI_Upper_Lim[1]) {
      IAS.Fullscale.Sig.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Worry_Illness[length(Entered_Scores_Df$Score_Worry_Illness)] < Entered_Scores_Df$CI_Lower_Lim_Worry_Illness[1]) {
      IAS.Worry.Illness.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Worry_Illness[length(Entered_Scores_Df$Score_Worry_Illness)] >= Entered_Scores_Df$CI_Lower_Lim_Worry_Illness[1]) {
      IAS.Worry.Illness.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Worry_Illness[length(Entered_Scores_Df$Score_Worry_Illness)] > Entered_Scores_Df$CI_Upper_Lim_Worry_Illness[1]) {
      IAS.Worry.Illness.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Worry_Illness[length(Entered_Scores_Df$Score_Worry_Illness)] <= Entered_Scores_Df$CI_Upper_Lim_Worry_Illness[1]) {
      IAS.Worry.Illness.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Concerns_Pain[length(Entered_Scores_Df$Score_Concerns_Pain)] < Entered_Scores_Df$CI_Lower_Lim_Concerns_Pain[1]) {
      IAS.Concerns.Pain.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Concerns_Pain[length(Entered_Scores_Df$Score_Concerns_Pain)] >= Entered_Scores_Df$CI_Lower_Lim_Concerns_Pain[1]) {
      IAS.Concerns.Pain.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Concerns_Pain[length(Entered_Scores_Df$Score_Concerns_Pain)] > Entered_Scores_Df$CI_Upper_Lim_Concerns_Pain[1]) {
      IAS.Concerns.Pain.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Concerns_Pain[length(Entered_Scores_Df$Score_Concerns_Pain)] <= Entered_Scores_Df$CI_Upper_Lim_Concerns_Pain[1]) {
      IAS.Concerns.Pain.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Health_Habits[length(Entered_Scores_Df$Score_Health_Habits)] < Entered_Scores_Df$CI_Lower_Lim_Health_Habits[1]) {
      IAS.Health.Habits.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Health_Habits[length(Entered_Scores_Df$Score_Health_Habits)] >= Entered_Scores_Df$CI_Lower_Lim_Health_Habits[1]) {
      IAS.Health.Habits.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Health_Habits[length(Entered_Scores_Df$Score_Health_Habits)] > Entered_Scores_Df$CI_Upper_Lim_Health_Habits[1]) {
      IAS.Health.Habits.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Health_Habits[length(Entered_Scores_Df$Score_Health_Habits)] <= Entered_Scores_Df$CI_Upper_Lim_Health_Habits[1]) {
      IAS.Health.Habits.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Hypochondriacal_Beliefs[length(Entered_Scores_Df$Score_Hypochondriacal_Beliefs)] < Entered_Scores_Df$CI_Lower_Lim_Hypochondriacal_Beliefs[1]) {
      IAS.Hypochondriacal.Beliefs.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Hypochondriacal_Beliefs[length(Entered_Scores_Df$Score_Hypochondriacal_Beliefs)] >= Entered_Scores_Df$CI_Lower_Lim_Hypochondriacal_Beliefs[1]) {
      IAS.Hypochondriacal.Beliefs.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Hypochondriacal_Beliefs[length(Entered_Scores_Df$Score_Hypochondriacal_Beliefs)] > Entered_Scores_Df$CI_Upper_Lim_Hypochondriacal_Beliefs[1]) {
      IAS.Hypochondriacal.Beliefs.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Hypochondriacal_Beliefs[length(Entered_Scores_Df$Score_Hypochondriacal_Beliefs)] <= Entered_Scores_Df$CI_Upper_Lim_Hypochondriacal_Beliefs[1]) {
      IAS.Hypochondriacal.Beliefs.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Thanatophobia[length(Entered_Scores_Df$Score_Thanatophobia)] < Entered_Scores_Df$CI_Lower_Lim_Thanatophobia[1]) {
      IAS.Thanatophobia.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Thanatophobia[length(Entered_Scores_Df$Score_Thanatophobia)] >= Entered_Scores_Df$CI_Lower_Lim_Thanatophobia[1]) {
      IAS.Thanatophobia.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Thanatophobia[length(Entered_Scores_Df$Score_Thanatophobia)] > Entered_Scores_Df$CI_Upper_Lim_Thanatophobia[1]) {
      IAS.Thanatophobia.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Thanatophobia[length(Entered_Scores_Df$Score_Thanatophobia)] <= Entered_Scores_Df$CI_Upper_Lim_Thanatophobia[1]) {
      IAS.Thanatophobia.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Disease_Phobia[length(Entered_Scores_Df$Score_Disease_Phobia)] < Entered_Scores_Df$CI_Lower_Lim_Disease_Phobia[1]) {
      IAS.Disease.Phobia.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Disease_Phobia[length(Entered_Scores_Df$Score_Disease_Phobia)] >= Entered_Scores_Df$CI_Lower_Lim_Disease_Phobia[1]) {
      IAS.Disease.Phobia.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Disease_Phobia[length(Entered_Scores_Df$Score_Disease_Phobia)] > Entered_Scores_Df$CI_Upper_Lim_Disease_Phobia[1]) {
      IAS.Disease.Phobia.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Disease_Phobia[length(Entered_Scores_Df$Score_Disease_Phobia)] <= Entered_Scores_Df$CI_Upper_Lim_Disease_Phobia[1]) {
      IAS.Disease.Phobia.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Bodily_Preoccupations[length(Entered_Scores_Df$Score_Bodily_Preoccupations)] < Entered_Scores_Df$CI_Lower_Lim_Bodily_Preoccupations[1]) {
      IAS.Bodily.Preoccupations.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Bodily_Preoccupations[length(Entered_Scores_Df$Score_Bodily_Preoccupations)] >= Entered_Scores_Df$CI_Lower_Lim_Bodily_Preoccupations[1]) {
      IAS.Bodily.Preoccupations.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Bodily_Preoccupations[length(Entered_Scores_Df$Score_Bodily_Preoccupations)] > Entered_Scores_Df$CI_Upper_Lim_Bodily_Preoccupations[1]) {
      IAS.Bodily.Preoccupations.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Bodily_Preoccupations[length(Entered_Scores_Df$Score_Bodily_Preoccupations)] <= Entered_Scores_Df$CI_Upper_Lim_Bodily_Preoccupations[1]) {
      IAS.Bodily.Preoccupations.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Treatment_Experience[length(Entered_Scores_Df$Score_Treatment_Experience)] < Entered_Scores_Df$CI_Lower_Lim_Treatment_Experience[1]) {
      IAS.Treatment.Experience.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Treatment_Experience[length(Entered_Scores_Df$Score_Treatment_Experience)] >= Entered_Scores_Df$CI_Lower_Lim_Treatment_Experience[1]) {
      IAS.Treatment.Experience.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Treatment_Experience[length(Entered_Scores_Df$Score_Treatment_Experience)] > Entered_Scores_Df$CI_Upper_Lim_Treatment_Experience[1]) {
      IAS.Treatment.Experience.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Treatment_Experience[length(Entered_Scores_Df$Score_Treatment_Experience)] <= Entered_Scores_Df$CI_Upper_Lim_Treatment_Experience[1]) {
      IAS.Treatment.Experience.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Effects_Symptoms[length(Entered_Scores_Df$Score_Effects_Symptoms)] < Entered_Scores_Df$CI_Lower_Lim_Effects_Symptoms[1]) {
      IAS.Effects.Symptoms.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Effects_Symptoms[length(Entered_Scores_Df$Score_Effects_Symptoms)] >= Entered_Scores_Df$CI_Lower_Lim_Effects_Symptoms[1]) {
      IAS.Effects.Symptoms.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Effects_Symptoms[length(Entered_Scores_Df$Score_Effects_Symptoms)] > Entered_Scores_Df$CI_Upper_Lim_Effects_Symptoms[1]) {
      IAS.Effects.Symptoms.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Effects_Symptoms[length(Entered_Scores_Df$Score_Effects_Symptoms)] <= Entered_Scores_Df$CI_Upper_Lim_Effects_Symptoms[1]) {
      IAS.Effects.Symptoms.Sig.Deterioration<- "No"
    }
    
    
    #Calculate variables for improvement
    
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$Score[1]) {
      IAS.Fullscale.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$Score[1]) {
      IAS.Fullscale.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$Score[1]) {
      IAS.Fullscale.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$Score[1]) {
      IAS.Fullscale.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Worry_Illness[length(Entered_Scores_Df$Score_Worry_Illness)] < Entered_Scores_Df$Score_Worry_Illness[1]) {
      IAS.Worry.Illness.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Worry_Illness[length(Entered_Scores_Df$Score_Worry_Illness)] >= Entered_Scores_Df$Score_Worry_Illness[1]) {
      IAS.Worry.Illness.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Worry_Illness[length(Entered_Scores_Df$Score_Worry_Illness)] > Entered_Scores_Df$Score_Worry_Illness[1]) {
      IAS.Worry.Illness.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Worry_Illness[length(Entered_Scores_Df$Score_Worry_Illness)] <= Entered_Scores_Df$Score_Worry_Illness[1]) {
      IAS.Worry.Illness.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Concerns_Pain[length(Entered_Scores_Df$Score_Concerns_Pain)] < Entered_Scores_Df$Score_Concerns_Pain[1]) {
      IAS.Concerns.Pain.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Concerns_Pain[length(Entered_Scores_Df$Score_Concerns_Pain)] >= Entered_Scores_Df$Score_Concerns_Pain[1]) {
      IAS.Concerns.Pain.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Concerns_Pain[length(Entered_Scores_Df$Score_Concerns_Pain)] > Entered_Scores_Df$Score_Concerns_Pain[1]) {
      IAS.Concerns.Pain.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Concerns_Pain[length(Entered_Scores_Df$Score_Concerns_Pain)] <= Entered_Scores_Df$Score_Concerns_Pain[1]) {
      IAS.Concerns.Pain.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Health_Habits[length(Entered_Scores_Df$Score_Health_Habits)] < Entered_Scores_Df$Score_Health_Habits[1]) {
      IAS.Health.Habits.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Health_Habits[length(Entered_Scores_Df$Score_Health_Habits)] >= Entered_Scores_Df$Score_Health_Habits[1]) {
      IAS.Health.Habits.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Health_Habits[length(Entered_Scores_Df$Score_Health_Habits)] > Entered_Scores_Df$Score_Health_Habits[1]) {
      IAS.Health.Habits.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Health_Habits[length(Entered_Scores_Df$Score_Health_Habits)] <= Entered_Scores_Df$Score_Health_Habits[1]) {
      IAS.Health.Habits.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Hypochondriacal_Beliefs[length(Entered_Scores_Df$Score_Hypochondriacal_Beliefs)] < Entered_Scores_Df$Score_Hypochondriacal_Beliefs[1]) {
      IAS.Hypochondriacal.Beliefs.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Hypochondriacal_Beliefs[length(Entered_Scores_Df$Score_Hypochondriacal_Beliefs)] >= Entered_Scores_Df$Score_Hypochondriacal_Beliefs[1]) {
      IAS.Hypochondriacal.Beliefs.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Hypochondriacal_Beliefs[length(Entered_Scores_Df$Score_Hypochondriacal_Beliefs)] > Entered_Scores_Df$Score_Hypochondriacal_Beliefs[1]) {
      IAS.Hypochondriacal.Beliefs.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Hypochondriacal_Beliefs[length(Entered_Scores_Df$Score_Hypochondriacal_Beliefs)] <= Entered_Scores_Df$Score_Hypochondriacal_Beliefs[1]) {
      IAS.Hypochondriacal.Beliefs.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Thanatophobia[length(Entered_Scores_Df$Score_Thanatophobia)] < Entered_Scores_Df$Score_Thanatophobia[1]) {
      IAS.Thanatophobia.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Thanatophobia[length(Entered_Scores_Df$Score_Thanatophobia)] >= Entered_Scores_Df$Score_Thanatophobia[1]) {
      IAS.Thanatophobia.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Thanatophobia[length(Entered_Scores_Df$Score_Thanatophobia)] > Entered_Scores_Df$Score_Thanatophobia[1]) {
      IAS.Thanatophobia.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Thanatophobia[length(Entered_Scores_Df$Score_Thanatophobia)] <= Entered_Scores_Df$Score_Thanatophobia[1]) {
      IAS.Thanatophobia.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Disease_Phobia[length(Entered_Scores_Df$Score_Disease_Phobia)] < Entered_Scores_Df$Score_Disease_Phobia[1]) {
      IAS.Disease.Phobia.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Disease_Phobia[length(Entered_Scores_Df$Score_Disease_Phobia)] >= Entered_Scores_Df$Score_Disease_Phobia[1]) {
      IAS.Disease.Phobia.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Disease_Phobia[length(Entered_Scores_Df$Score_Disease_Phobia)] > Entered_Scores_Df$Score_Disease_Phobia[1]) {
      IAS.Disease.Phobia.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Disease_Phobia[length(Entered_Scores_Df$Score_Disease_Phobia)] <= Entered_Scores_Df$Score_Disease_Phobia[1]) {
      IAS.Disease.Phobia.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Bodily_Preoccupations[length(Entered_Scores_Df$Score_Bodily_Preoccupations)] < Entered_Scores_Df$Score_Bodily_Preoccupations[1]) {
      IAS.Bodily.Preoccupations.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Bodily_Preoccupations[length(Entered_Scores_Df$Score_Bodily_Preoccupations)] >= Entered_Scores_Df$Score_Bodily_Preoccupations[1]) {
      IAS.Bodily.Preoccupations.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Bodily_Preoccupations[length(Entered_Scores_Df$Score_Bodily_Preoccupations)] > Entered_Scores_Df$Score_Bodily_Preoccupations[1]) {
      IAS.Bodily.Preoccupations.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Bodily_Preoccupations[length(Entered_Scores_Df$Score_Bodily_Preoccupations)] <= Entered_Scores_Df$Score_Bodily_Preoccupations[1]) {
      IAS.Bodily.Preoccupations.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Treatment_Experience[length(Entered_Scores_Df$Score_Treatment_Experience)] < Entered_Scores_Df$Score_Treatment_Experience[1]) {
      IAS.Treatment.Experience.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Treatment_Experience[length(Entered_Scores_Df$Score_Treatment_Experience)] >= Entered_Scores_Df$Score_Treatment_Experience[1]) {
      IAS.Treatment.Experience.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Treatment_Experience[length(Entered_Scores_Df$Score_Treatment_Experience)] > Entered_Scores_Df$Score_Treatment_Experience[1]) {
      IAS.Treatment.Experience.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Treatment_Experience[length(Entered_Scores_Df$Score_Treatment_Experience)] <= Entered_Scores_Df$Score_Treatment_Experience[1]) {
      IAS.Treatment.Experience.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Effects_Symptoms[length(Entered_Scores_Df$Score_Effects_Symptoms)] < Entered_Scores_Df$Score_Effects_Symptoms[1]) {
      IAS.Effects.Symptoms.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Effects_Symptoms[length(Entered_Scores_Df$Score_Effects_Symptoms)] >= Entered_Scores_Df$Score_Effects_Symptoms[1]) {
      IAS.Effects.Symptoms.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Effects_Symptoms[length(Entered_Scores_Df$Score_Effects_Symptoms)] > Entered_Scores_Df$Score_Effects_Symptoms[1]) {
      IAS.Effects.Symptoms.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Effects_Symptoms[length(Entered_Scores_Df$Score_Effects_Symptoms)] <= Entered_Scores_Df$Score_Effects_Symptoms[1]) {
      IAS.Effects.Symptoms.Deterioration<- "No"
    }
    
    
    IAS.Fullscale.Change<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] - Entered_Scores_Df$Score[1]
    IAS.Worry.Illness.Change<- Entered_Scores_Df$Score_Worry_Illness[length(Entered_Scores_Df$Score_Worry_Illness)] - Entered_Scores_Df$Score_Worry_Illness[1]
    IAS.Concerns.Pain.Change<- Entered_Scores_Df$Score_Concerns_Pain[length(Entered_Scores_Df$Score_Concerns_Pain)] - Entered_Scores_Df$Score_Concerns_Pain[1]
    IAS.Health.Habits.Change<- Entered_Scores_Df$Score_Health_Habits[length(Entered_Scores_Df$Score_Health_Habits)] - Entered_Scores_Df$Score_Health_Habits[1]
    IAS.Hypochondriacal.Beliefs.Change<- Entered_Scores_Df$Score_Hypochondriacal_Beliefs[length(Entered_Scores_Df$Score_Hypochondriacal_Beliefs)] - Entered_Scores_Df$Score_Hypochondriacal_Beliefs[1]
    IAS.Fullscale.Comparisons<- length(Entered_Scores_Df$Change) - 1
    IAS.Worry.Illness.Comparisons<- length(Entered_Scores_Df$Change_Worry_Illness) - 1
    IAS.Concerns.Pain.Comparisons<- length(Entered_Scores_Df$Change_Concerns_Pain) - 1
    IAS.Health.Habits.Comparisons<- length(Entered_Scores_Df$Change_Health_Habits) - 1
    IAS.Hypochondriacal.Beliefs.Comparisons<- length(Entered_Scores_Df$Change_Hypochondriacal_Beliefs) - 1
    IAS.Fullscale.First.Date<- Entered_Scores_Df$Date[1]
    IAS.Worry.Illness.First.Date<- Entered_Scores_Df$Date[1]
    IAS.Concerns.Pain.First.Date<- Entered_Scores_Df$Date[1]
    IAS.Health.Habits.First.Date<- Entered_Scores_Df$Date[1]
    IAS.Hypochondriacal.Beliefs.First.Date<- Entered_Scores_Df$Date[1]
    IAS.Fullscale.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    IAS.Worry.Illness.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    IAS.Concerns.Pain.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    IAS.Health.Habits.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    IAS.Hypochondriacal.Beliefs.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    IAS.Fullscale.First.Score<- Entered_Scores_Df$Score[1]
    IAS.Worry.Illness.First.Score<- Entered_Scores_Df$Score_Worry_Illness[1]
    IAS.Concerns.Pain.First.Score<- Entered_Scores_Df$Score_Concerns_Pain[1]
    IAS.Health.Habits.First.Score<- Entered_Scores_Df$Score_Health_Habits[1]
    IAS.Hypochondriacal.Beliefs.First.Score<- Entered_Scores_Df$Score_Hypochondriacal_Beliefs[1]
    IAS.Fullscale.Last.Score<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)]
    IAS.Worry.Illness.Last.Score<- Entered_Scores_Df$Score_Worry_Illness[length(Entered_Scores_Df$Score_Worry_Illness)]
    IAS.Concerns.Pain.Last.Score<- Entered_Scores_Df$Score_Concerns_Pain[length(Entered_Scores_Df$Score_Concerns_Pain)]
    IAS.Health.Habits.Last.Score<- Entered_Scores_Df$Score_Health_Habits[length(Entered_Scores_Df$Score_Health_Habits)]
    IAS.Hypochondriacal.Beliefs.Last.Score<- Entered_Scores_Df$Score_Hypochondriacal_Beliefs[length(Entered_Scores_Df$Score_Hypochondriacal_Beliefs)]
    
    
    IAS.Thanatophobia.Change<- Entered_Scores_Df$Score_Thanatophobia[length(Entered_Scores_Df$Score_Thanatophobia)] - Entered_Scores_Df$Score_Thanatophobia[1]
    IAS.Disease.Phobia.Change<- Entered_Scores_Df$Score_Disease_Phobia[length(Entered_Scores_Df$Score_Disease_Phobia)] - Entered_Scores_Df$Score_Disease_Phobia[1]
    IAS.Bodily.Preoccupations.Change<- Entered_Scores_Df$Score_Bodily_Preoccupations[length(Entered_Scores_Df$Score_Bodily_Preoccupations)] - Entered_Scores_Df$Score_Bodily_Preoccupations[1]
    IAS.Treatment.Experience.Change<- Entered_Scores_Df$Score_Treatment_Experience[length(Entered_Scores_Df$Score_Treatment_Experience)] - Entered_Scores_Df$Score_Treatment_Experience[1]
    IAS.Effects.Symptoms.Change<- Entered_Scores_Df$Score_Effects_Symptoms[length(Entered_Scores_Df$Score_Effects_Symptoms)] - Entered_Scores_Df$Score_Effects_Symptoms[1]
    IAS.Thanatophobia.Comparisons<- length(Entered_Scores_Df$Change) - 1
    IAS.Disease.Phobia.Comparisons<- length(Entered_Scores_Df$Change_Disease_Phobia) - 1
    IAS.Bodily.Preoccupations.Comparisons<- length(Entered_Scores_Df$Change_Bodily_Preoccupations) - 1
    IAS.Treatment.Experience.Comparisons<- length(Entered_Scores_Df$Change_Treatment_Experience) - 1
    IAS.Effects.Symptoms.Comparisons<- length(Entered_Scores_Df$Change_Effects_Symptoms) - 1
    IAS.Thanatophobia.First.Date<- Entered_Scores_Df$Date[1]
    IAS.Disease.Phobia.First.Date<- Entered_Scores_Df$Date[1]
    IAS.Bodily.Preoccupations.First.Date<- Entered_Scores_Df$Date[1]
    IAS.Treatment.Experience.First.Date<- Entered_Scores_Df$Date[1]
    IAS.Effects.Symptoms.First.Date<- Entered_Scores_Df$Date[1]
    IAS.Thanatophobia.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    IAS.Disease.Phobia.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    IAS.Bodily.Preoccupations.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    IAS.Treatment.Experience.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    IAS.Effects.Symptoms.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    IAS.Thanatophobia.First.Score<- Entered_Scores_Df$Score_Thanatophobia[1]
    IAS.Disease.Phobia.First.Score<- Entered_Scores_Df$Score_Disease_Phobia[1]
    IAS.Bodily.Preoccupations.First.Score<- Entered_Scores_Df$Score_Bodily_Preoccupations[1]
    IAS.Treatment.Experience.First.Score<- Entered_Scores_Df$Score_Treatment_Experience[1]
    IAS.Effects.Symptoms.First.Score<- Entered_Scores_Df$Score_Effects_Symptoms[1]
    IAS.Thanatophobia.Last.Score<- Entered_Scores_Df$Score_Thanatophobia[length(Entered_Scores_Df$Score_Thanatophobia)]
    IAS.Disease.Phobia.Last.Score<- Entered_Scores_Df$Score_Disease_Phobia[length(Entered_Scores_Df$Score_Disease_Phobia)]
    IAS.Bodily.Preoccupations.Last.Score<- Entered_Scores_Df$Score_Bodily_Preoccupations[length(Entered_Scores_Df$Score_Bodily_Preoccupations)]
    IAS.Treatment.Experience.Last.Score<- Entered_Scores_Df$Score_Treatment_Experience[length(Entered_Scores_Df$Score_Treatment_Experience)]
    IAS.Effects.Symptoms.Last.Score<- Entered_Scores_Df$Score_Effects_Symptoms[length(Entered_Scores_Df$Score_Effects_Symptoms)]
    
    
    
    
    
    
    Analytics_Df<<- data.frame(IAS.Fullscale.First.Date, IAS.Fullscale.First.Score, IAS.Fullscale.Comparisons, IAS.Fullscale.Change, IAS.Fullscale.Last.Date, IAS.Fullscale.Last.Score, IAS.Fullscale.Improvement,IAS.Fullscale.Sig.Improvement, IAS.Fullscale.Deterioration, IAS.Fullscale.Sig.Deterioration,
                               IAS.Worry.Illness.First.Date, IAS.Worry.Illness.First.Score, IAS.Worry.Illness.Comparisons, IAS.Worry.Illness.Change, IAS.Worry.Illness.Last.Date, IAS.Worry.Illness.Last.Score, IAS.Worry.Illness.Improvement, IAS.Worry.Illness.Sig.Improvement, IAS.Worry.Illness.Deterioration, IAS.Worry.Illness.Sig.Deterioration,
                               IAS.Concerns.Pain.First.Date, IAS.Concerns.Pain.First.Score, IAS.Concerns.Pain.Comparisons, IAS.Concerns.Pain.Change, IAS.Concerns.Pain.Last.Date, IAS.Concerns.Pain.Last.Score, IAS.Concerns.Pain.Improvement, IAS.Concerns.Pain.Sig.Improvement, IAS.Concerns.Pain.Deterioration, IAS.Concerns.Pain.Sig.Deterioration, 
                               IAS.Health.Habits.First.Date, IAS.Health.Habits.First.Score, IAS.Health.Habits.Comparisons, IAS.Health.Habits.Change, IAS.Health.Habits.Last.Date, IAS.Health.Habits.Last.Score, IAS.Health.Habits.Improvement, IAS.Health.Habits.Sig.Improvement, IAS.Health.Habits.Deterioration, IAS.Health.Habits.Sig.Deterioration, 
                               IAS.Hypochondriacal.Beliefs.First.Date, IAS.Hypochondriacal.Beliefs.First.Score, IAS.Hypochondriacal.Beliefs.Comparisons, IAS.Hypochondriacal.Beliefs.Change, IAS.Hypochondriacal.Beliefs.Last.Date, IAS.Hypochondriacal.Beliefs.Last.Score, IAS.Hypochondriacal.Beliefs.Improvement, IAS.Hypochondriacal.Beliefs.Sig.Improvement, IAS.Hypochondriacal.Beliefs.Deterioration, IAS.Hypochondriacal.Beliefs.Sig.Deterioration,
                               IAS.Thanatophobia.First.Date, IAS.Thanatophobia.First.Score, IAS.Thanatophobia.Comparisons, IAS.Thanatophobia.Change, IAS.Thanatophobia.Last.Date, IAS.Thanatophobia.Last.Score, IAS.Thanatophobia.Improvement,IAS.Thanatophobia.Sig.Improvement, IAS.Thanatophobia.Deterioration, IAS.Thanatophobia.Sig.Deterioration,
                               IAS.Disease.Phobia.First.Date, IAS.Disease.Phobia.First.Score, IAS.Disease.Phobia.Comparisons, IAS.Disease.Phobia.Change, IAS.Disease.Phobia.Last.Date, IAS.Disease.Phobia.Last.Score, IAS.Disease.Phobia.Improvement, IAS.Disease.Phobia.Sig.Improvement, IAS.Disease.Phobia.Deterioration, IAS.Disease.Phobia.Sig.Deterioration,
                               IAS.Bodily.Preoccupations.First.Date, IAS.Bodily.Preoccupations.First.Score, IAS.Bodily.Preoccupations.Comparisons, IAS.Bodily.Preoccupations.Change, IAS.Bodily.Preoccupations.Last.Date, IAS.Bodily.Preoccupations.Last.Score, IAS.Bodily.Preoccupations.Improvement, IAS.Bodily.Preoccupations.Sig.Improvement, IAS.Bodily.Preoccupations.Deterioration, IAS.Bodily.Preoccupations.Sig.Deterioration, 
                               IAS.Treatment.Experience.First.Date, IAS.Treatment.Experience.First.Score, IAS.Treatment.Experience.Comparisons, IAS.Treatment.Experience.Change, IAS.Treatment.Experience.Last.Date, IAS.Treatment.Experience.Last.Score, IAS.Treatment.Experience.Improvement, IAS.Treatment.Experience.Sig.Improvement, IAS.Treatment.Experience.Deterioration, IAS.Treatment.Experience.Sig.Deterioration, 
                               IAS.Effects.Symptoms.First.Date, IAS.Effects.Symptoms.First.Score, IAS.Effects.Symptoms.Comparisons, IAS.Effects.Symptoms.Change, IAS.Effects.Symptoms.Last.Date, IAS.Effects.Symptoms.Last.Score, IAS.Effects.Symptoms.Improvement, IAS.Effects.Symptoms.Sig.Improvement, IAS.Effects.Symptoms.Deterioration, IAS.Effects.Symptoms.Sig.Deterioration)
    
    
    
  })
  
  
  
  #Create an expression to activate the entered data & create notifications to indicate inaccurate data entry
  
  Entered_Scores_Notifications<- observeEvent(input$Action_Submit_Data, {
    
    Entered_Scores_Reac()
    
    if(length(Score_1a) < 27) {
      showNotification("The IAS is a 27-item scale. You have entered less than 27 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(length(Score_1a) > 27) {
      showNotification("The IAS is a 27-item scale. You have entered more than 27 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(input$Timepoint != "1") {
      if(length(Score_2a) < 27) {
        showNotification("The IAS is a 27-item scale. You have entered less than 27 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_2a) > 27) {
        showNotification("The IAS is a 27-item scale. You have entered more than 27 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
    if(input$Timepoint == "3") {
      if(length(Score_3a) < 27) {
        showNotification("The IAS is a 27-item scale. You have entered less than 27 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_3a) > 27) {
        showNotification("The IAS is a 27-item scale. You have entered more than 27 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
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
    
    Gap_Worry_Illness<- Entered_Scores_Df[1,11] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),11]
    Entered_Scores_Df[1,12]<- Gap_Worry_Illness
    
    Gap_Concerns_Pain<- Entered_Scores_Df[1,20] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),20]
    Entered_Scores_Df[1,21]<- Gap_Concerns_Pain
    
    Gap_Health_Habits<- Entered_Scores_Df[1,29] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),29]
    Entered_Scores_Df[1,30]<- Gap_Health_Habits
    
    Gap_Hypochondriacal_Beliefs<- Entered_Scores_Df[1,38] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),38]
    Entered_Scores_Df[1,39]<- Gap_Hypochondriacal_Beliefs
    
    Gap_Thanatophobia<- Entered_Scores_Df[1,47] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),47]
    Entered_Scores_Df[1,48]<- Gap_Thanatophobia
    
    Gap_Disease_Phobia<- Entered_Scores_Df[1,56] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),56]
    Entered_Scores_Df[1,57]<- Gap_Disease_Phobia
    
    Gap_Bodily_Preoccupations<- Entered_Scores_Df[1,65] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),65]
    Entered_Scores_Df[1,66]<- Gap_Bodily_Preoccupations
    
    Gap_Treatment_Experience<- Entered_Scores_Df[1,74] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),74]
    Entered_Scores_Df[1,75]<- Gap_Treatment_Experience
    
    Gap_Effects_Symptoms<- Entered_Scores_Df[1,83] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),83]
    Entered_Scores_Df[1,84]<- Gap_Effects_Symptoms
    

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
    
    filename = paste0(" IAS Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
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
        Illness = Illness,
        Afraid_Illness = Afraid_Illness,
        Year_Treatments = Year_Treatments,
        Entered_Scores_Df = Entered_Scores_Df,
        Tab_Reference = Tab_Reference,
        Stats_Table_Fullscale = Stats_Table_Fullscale,
        Stats_Table_Worry_Illness = Stats_Table_Worry_Illness,
        Stats_Table_Concerns_Pain = Stats_Table_Concerns_Pain,
        Stats_Table_Health_Habits = Stats_Table_Health_Habits,
        Stats_Table_Hypochondriacal_Beliefs = Stats_Table_Hypochondriacal_Beliefs,
        Stats_Table_Thanatophobia = Stats_Table_Thanatophobia,
        Stats_Table_Disease_Phobia = Stats_Table_Disease_Phobia,
        Stats_Table_Bodily_Preoccupations = Stats_Table_Bodily_Preoccupations,
        Stats_Table_Treatment_Experience = Stats_Table_Treatment_Experience,
        Stats_Table_Effects_Symptoms = Stats_Table_Effects_Symptoms,
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
      paste(paste0(" IAS Data ", format(Sys.time(), '%d/%m/%y')), input$Output_Filetype1, sep = ".")
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













