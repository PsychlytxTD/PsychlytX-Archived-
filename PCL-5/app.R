
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
      menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "PCL-5"),
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
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| PTSD Checklist for DSM-5 (PCL-5)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 650),
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
                "Armour, C., Contractor, A., Shea, T., Elhai, J. D., & Pietrzak, R. H. (2016). Factor structure of the PTSD checklist for DSM-5: Relationships among symptom clusters, anger, and impulsivity. The Journal of Nervous and Mental Disease, 204(2), 108-115. 10.1097/NMD.0000000000000430.", br(), br(),
                "Ashbaugh, A. R., Houle-Johnson, S., Herbert, C., El-Hage, W., & Brunet, A. (2016). Psychometric validation of the english and french versions of the posttraumatic stress disorder checklist for DSM-5 (PCL-5). PloS One, 11(10).", br(), br(),
                "Blevins, C. A., Weathers, F. W., Davis, M. T., Witte, T. K., & Domino, J. L. (2015). The posttraumatic stress disorder checklist for DSM‐5 (PCL‐5): Development and initial psychometric evaluation. Journal of Traumatic Stress, 28(6), 489-498.", br(), br(), 
                "Bovin, M. J., Marx, B. P., Weathers, F. W., Gallagher, M. W., Rodriguez, P., Schnurr, P. P., & Keane, T. M. (2016). Psychometric properties of the PTSD checklist for diagnostic and statistical manual of mental Disorders–Fifth edition (PCL-5) in veterans. Psychological Assessment, 28(11), 1379.", br(), br(), 
                "Finkelman, M. D., Lowe, S. R., Kim, W., Gruebner, O., Smits, N., & Galea, S. (2017). Customized computer-based administration of the PCL-5 for the efficient assessment of PTSD: A proof-of-principle study. Psychological Trauma: Theory, Research, Practice, and Policy, 9(3), 379.", br(), br(), 
                "Keane, T. M., Rubin, A., Lachowicz, M., Brief, D., Enggasser, J. L., Roy, M., . . . Rosenbloom, D. (2014). Temporal stability of DSM–5 posttraumatic stress disorder criteria in a problem-drinking sample. Psychological Assessment, 26(4), 1138.", br(), br(), 
                "Krüger-Gottschalk, A., Knaevelsrud, C., Rau, H., Dyer, A., Schäfer, I., Schellong, J., & Ehring, T. (2017). The german version of the posttraumatic stress disorder checklist for DSM-5 (PCL-5): Psychometric properties and diagnostic utility. BMC Psychiatry, 17(1), 379.", br(), br(), 
                "Lueger-Schuster, B., Knefel, M., Glück, T. M., Jagsch, R., Kantor, V., & Weindl, D. (2018). Child abuse and neglect in institutional settings, cumulative lifetime traumatization, and psychopathological long-term correlates in adult survivors: The vienna institutional abuse study. Child Abuse & Neglect, 76, 488-501.", br(), br(), 
                "McDonald, S. D., & Calhoun, P. S. (2010). The diagnostic accuracy of the PTSD checklist: A critical review. Clinical Psychology Review, 30(8), 976-987.", br(), br(),
                "Seligowski, A. V., & Orcutt, H. K. (2016). Support for the 7-factor hybrid model of PTSD in a community sample. Psychological Trauma: Theory, Research, Practice, and Policy, 8(2), 218.", br(), br(), 
                "Stanley, I. H., Hom, M. A., Spencer-Thomas, S., & Joiner, T. E. (2017). Examining anxiety sensitivity as a mediator of the association between PTSD symptoms and suicide risk among women firefighters. Journal of Anxiety Disorders, 50, 94-102.", br(), br(), 
                "Vujanovic, A. A., Dutcher, C. D., & Berenz, E. C. (2017). Multimodal examination of distress tolerance and posttraumatic stress disorder symptoms in acute-care psychiatric inpatients. Journal of Anxiety Disorders, 48, 45-53.", br(), br(), 
                "Weathers, F. W., Litz, B. T., Keane, T. M., Palmieri, P. A., Marx, B. P., & Schnurr, P. P. (2013). The ptsd checklist for dsm-5 (pcl-5). Scale Available from the National Center for PTSD at Www.Ptsd.Va.Gov.", br(), br(),
                "Wortmann, J. H., Jordan, A. H., Weathers, F. W., Resick, P. A., Dondanville, K. A., Hall-Clark, B., . . . Hembree, E. A. (2016). Psychometric analysis of the PTSD checklist-5 (PCL-5) among treatment-seeking military service members. Psychological Assessment, 28(11), 1392."
        ),
        
        
        
        tabItem(tabName = "PCL-5",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel("Scale",
                             wellPanel(style = "background-color: #ffffff; color: black",
                                       fluidRow(
                                         column(width = 12, offset = 5, h3(tags$strong("PCL-5")))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 6, h4(tags$strong("Over the past 2 weeks, how often have you been bothered by any of the following problems?"))),
                                         column(width = 1, h5(tags$strong("Not at all"))),
                                         column(width = 1, h5(tags$strong("A little bit"))),
                                         column(width = 1, h5(tags$strong("Moderately"))),
                                         column(width = 1, h5(tags$strong(HTML('&emsp;'),"Quite", HTML('&emsp;'), "a bit"))),
                                         column(width = 1, h5(tags$strong("Extremely")))
                                       ),
                                       
                                       fluidRow(
                                         column(width = 6, h4("1. Repeated, disturbing, and unwanted memories of the stressful experience?")),
                                         column(width = 6, radioButtons("Item_1", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("2. Repeated, disturbing dreams of the stressful experience?")),
                                         column(width = 6, radioButtons("Item_2", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("3. Suddenly feeling or acting as if the stressful experience were actually happening again (as if you were actually back there reliving it)?")),
                                         column(width = 6, radioButtons("Item_3", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("4. Feeling very upset when something reminded you of the stressful experience?")),
                                         column(width = 6, radioButtons("Item_4", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("5. Having strong physical reactions when something reminded you of the stressful experience (for example, heart pounding, trouble breathing, sweating)?")),
                                         column(width = 6, radioButtons("Item_5", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("6. Avoiding memories, thoughts, or feelings related to the stressful experience?")),
                                         column(width = 6, radioButtons("Item_6", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("7. Avoiding external reminders of the stressful experience (for example, people, places, conversations, activities, objects, or situations)?")),
                                         column(width = 6, radioButtons("Item_7", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("8. Trouble remembering important parts of the stressful experience?")),
                                         column(width = 6, radioButtons("Item_8", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("9. Having strong negative beliefs about yourself, other people, or the world (for example, having thoughts such as: I am bad, there is something seriously wrong with me, no one can be trusted, the world is completely dangerous)?")),
                                         column(width = 6, radioButtons("Item_9", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("10. Blaming yourself or someone else for the stressful experience or what happened after it?")),
                                         column(width = 6, radioButtons("Item_10", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("11. Having strong negative feelings such as fear, horror, anger, guilt, or shame?")),
                                         column(width = 6, radioButtons("Item_11", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("12. Loss of interest in activities that you used to enjoy?")),
                                         column(width = 6, radioButtons("Item_12", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("13. Feeling distant or cut of from other people?")),
                                         column(width = 6, radioButtons("Item_13", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("14. Trouble experiencing positive feelings (for example, being unable to feel happiness or have loving feelings for people close to you)?")),
                                         column(width = 6, radioButtons("Item_14", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("15. Irritable behavior, angry outbursts, or acting aggressively?")),
                                         column(width = 6, radioButtons("Item_15", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("16. Taking too many risks or doing things that could cause you harm?")),
                                         column(width = 6, radioButtons("Item_16", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("17. Being “superalert” or watchful or on guard?")),
                                         column(width = 6, radioButtons("Item_17", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("18. Feeling jumpy or easily startled?")),
                                         column(width = 6, radioButtons("Item_18", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("19. Having difculty concentrating?")),
                                         column(width = 6, radioButtons("Item_19", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 6, h4("20. Trouble falling or staying asleep?")),
                                         column(width = 6, radioButtons("Item_20", label = NULL, choices = c("0", "1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, dateInput("Q_Date", "Date",  format = "dd/mm/yyyy")),
                                         column(width = 4, textInput("Q_Name", "Name")),
                                         column(width = 4, textInput("Q_Clin_Name", "Clinician's Name"))
                                       ),
                                       fluidRow(
                                         column(width = 12, h5("Scale Source: Weathers, Litz, Keane, Palmieri, Marx & Schnurr (2013)"))
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
                                                      selectInput("Pop", "", choices = c("Active Military Service", "Veteran", "Psychiatric Inpatient", "Institutional Abuse", "University Student"))
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
                                                               selectInput("Select_CI", label = "PCL-5 total scale",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI == '2'",
                                                                                numericInput("Man_CI", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Intrusion", label = "Intrusion",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Intrusion == '2'",
                                                                                numericInput("Man_CI_Intrusion", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_NACM", label = "Cognition Mood",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_NACM == '2'",
                                                                                numericInput("Man_CI_NACM", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Hyperarousal", label = "Hyperarousal",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Hyperarousal == '2'",
                                                                                numericInput("Man_CI_Hyperarousal", "Specify the width of the confidence interval", value = 0))
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
                                                               uiOutput("Mean_Widg_Intrusion")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_NACM")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Hyperarousal")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Avoidance")
                                                        )
                                                      ),
                                                      
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Chelune et al. (1993)' || input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)' || input.RelChangeMethod == 'Speer (1992)'",
                                                                       
                                                                       h4(tags$strong("Enter a mean value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean", "PCL-5 total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Intrusion", "Intrusion", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_NACM", "Cognition Mood", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Hyperarousal", "Hyperarousal", value = 0)
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
                                                               uiOutput("Sd_Widg_Intrusion")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_NACM")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Hyperarousal")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Avoidance")
                                                        )
                                                      ),
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)'",
                                                                       
                                                                       h4(tags$strong("Enter a standard deviation value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd", "PCL-5 total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Intrusion", "Intrusion", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_NACM", "Cognition Mood", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Hyperarousal", "Hyperarousal", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Avoidance", "Avoidance", value = 0)
                                                                         )
                                                                       )
                                                                       
                                                      )
                                             ),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      h4(tags$strong("Enter test-retest reliability values")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability", "PCL-5 total scale", value = .89),
                                                               h6("Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Intrusion", "Intrusion", value = .80),
                                                               h6("Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_NACM", "Cognition Mood", value = .92),
                                                               h6("Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Hyperarousal", "Hyperarousal", value = .78),
                                                               h6("Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Avoidance", "Avoidance", value = .66),
                                                               h6("Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)")
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
                                                               uiOutput("Cutoff_Widg_Intrusion_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_NACM_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Hyperarousal_1") 
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
                                                               uiOutput("Cutoff_Widg_Intrusion_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_NACM_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Hyperarousal_2") 
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
                                                               uiOutput("Cutoff_Widg_Intrusion_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_NACM_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Hyperarousal_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Avoidance_3") 
                                                        )
                                                        
                                                      )
                                                      , hr()
                                                      
                                             ),
                                             
                                             
                                             h4(tags$strong("Psychometric Properties of the PCL-5 Relevant to Assessing Reliable & Clinically Significant Change")),
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
                                              actionButton("Action_Submit_Data", div(tags$strong("Step 1."), "Submit Newly-Entered Data"),  width = '270px'),
                                              hr(),
                                              br(),
                                              actionButton("Action_Combine", div(tags$strong("Step 2."), "Combine with Imported Data"),  width = '270px'),
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
    
    if(input$Pop == "Psychiatric Inpatient") {
      Mean_Val<<- 39.98
      Sd_Val<<-21.11
      Source_Mean<<- "Vujanovic, Dutcher, & Berenz (2016)"
      Source_Sd<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Cut_Val_1<<- 33
        Cut_Val_2<<- 38
        Cut_Val_3<<- 39.98
        Cut_Lab_1<<- "PTSD Cut-Off: Bovin et al (2015)"
        Cut_Lab_2<<- "PTSD Cut-Off: Scale Developers"
        Cut_Lab_3<<- "Psychiatric Inpatient Mean"
        Source_Cutoff_1<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
        Source_Cutoff_2<<- "Weathers, Litz, Keane, Palmieri, Marx et al (2013)"
        Source_Cutoff_3<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Mean_Val_Intrusion<<- 9.93
        Sd_Val_Intrusion<<-5.99
        Source_Mean_Intrusion<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Source_Sd_Intrusion<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Cut_Val_Intrusion_1<<- Mean_Val_Intrusion 
        Cut_Val_Intrusion_2<<- Mean_Val_Intrusion + Sd_Val_Intrusion
        Cut_Val_Intrusion_3<<- Mean_Val_Intrusion + (2*Sd_Val_Intrusion)
        Cut_Lab_1_Intrusion<<- "Psychiatric Inpatient Mean"
        Cut_Lab_2_Intrusion<<- "Psychiatric Inpatient Mean + 1Sd"
        Cut_Lab_3_Intrusion<<- "Psychiatric Inpatient Mean + 2Sd"
        Source_Cutoff_Intrusion_1<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Source_Cutoff_Intrusion_2<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Source_Cutoff_Intrusion_3<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Mean_Val_NACM<<- 13.61
        Sd_Val_NACM<<-8.71
        Source_Mean_NACM<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Source_Sd_NACM<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Cut_Val_NACM_1<<- Mean_Val_NACM 
        Cut_Val_NACM_2<<- Mean_Val_NACM + Sd_Val_NACM
        Cut_Val_NACM_3<<- Mean_Val_NACM + (2*Sd_Val_NACM)
        Cut_Lab_1_NACM<<- "Psychiatric Inpatient Mean"
        Cut_Lab_2_NACM<<- "Psychiatric Inpatient Mean + 1Sd"
        Cut_Lab_3_NACM<<- "Psychiatric Inpatient Mean + 2Sd"
        Source_Cutoff_NACM_1<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Source_Cutoff_NACM_2<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Source_Cutoff_NACM_3<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Mean_Val_Hyperarousal<<- 12.3
        Sd_Val_Hyperarousal<<-6.42
        Source_Mean_Hyperarousal<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Source_Sd_Hyperarousal<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Cut_Val_Hyperarousal_1<<- Mean_Val_Hyperarousal 
        Cut_Val_Hyperarousal_2<<- Mean_Val_Hyperarousal + Sd_Val_Hyperarousal
        Cut_Val_Hyperarousal_3<<- Mean_Val_Hyperarousal + (2*Sd_Val_Hyperarousal)
        Cut_Lab_1_Hyperarousal<<- "Psychiatric Inpatient Mean"
        Cut_Lab_2_Hyperarousal<<- "Psychiatric Inpatient Mean + 1Sd"
        Cut_Lab_3_Hyperarousal<<- "Psychiatric Inpatient Mean + 2Sd"
        Source_Cutoff_Hyperarousal_1<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Source_Cutoff_Hyperarousal_2<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Source_Cutoff_Hyperarousal_3<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Mean_Val_Avoidance<<- 4.13
        Sd_Val_Avoidance<<-2.63
        Source_Mean_Avoidance<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Source_Sd_Avoidance<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Cut_Val_Avoidance_1<<- Mean_Val_Avoidance 
        Cut_Val_Avoidance_2<<- Mean_Val_Avoidance + Sd_Val_Avoidance
        Cut_Val_Avoidance_3<<- Mean_Val_Avoidance + (2*Sd_Val_Avoidance)
        Cut_Lab_1_Avoidance<<- "Psychiatric Inpatient Mean"
        Cut_Lab_2_Avoidance<<- "Psychiatric Inpatient Mean + 1Sd"
        Cut_Lab_3_Avoidance<<- "Psychiatric Inpatient Mean + 2Sd"
        Source_Cutoff_Avoidance_1<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Source_Cutoff_Avoidance_2<<- "Vujanovic, Dutcher, & Berenz (2016)"
        Source_Cutoff_Avoidance_3<<- "Vujanovic, Dutcher, & Berenz (2016)"
    } else if(input$Pop == "University Student") {
      Mean_Val<<- 20.9
      Sd_Val<<-17.7
      Source_Mean<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Sd<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Cut_Val_1<<- Mean_Val
      Cut_Val_2<<- 33
      Cut_Val_3<<- 38
      Cut_Lab_1<<- "University Student Mean"
      Cut_Lab_2<<- "PTSD Cut-Off: Bovin et al (2015)"
      Cut_Lab_3<<- "PTSD Cut-Off: Scale Developers"
      Source_Cutoff_1<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Cutoff_2<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Cutoff_3<<- "Weathers, Litz, Keane, Palmieri, Marx et al (2013)"
      Mean_Val_Intrusion<<- 5.6
      Sd_Val_Intrusion<<-4.9
      Source_Mean_Intrusion<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Sd_Intrusion<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Cut_Val_Intrusion_1<<- Mean_Val_Intrusion 
      Cut_Val_Intrusion_2<<- Mean_Val_Intrusion + Sd_Val_Intrusion
      Cut_Val_Intrusion_3<<- Mean_Val_Intrusion + (2*Sd_Val_Intrusion)
      Cut_Lab_1_Intrusion<<- "University Student Mean"
      Cut_Lab_2_Intrusion<<- "University Student Mean + 1Sd"
      Cut_Lab_3_Intrusion<<- "University Student Mean + 2Sd"
      Source_Cutoff_Intrusion_1<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Cutoff_Intrusion_2<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Cutoff_Intrusion_3<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Mean_Val_NACM<<- 7.1
      Sd_Val_NACM<<- 6.9
      Source_Mean_NACM<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Sd_NACM<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Cut_Val_NACM_1<<- Mean_Val_NACM 
      Cut_Val_NACM_2<<- Mean_Val_NACM + Sd_Val_NACM
      Cut_Val_NACM_3<<- Mean_Val_NACM + (2*Sd_Val_NACM)
      Cut_Lab_1_NACM<<- "University Student Mean"
      Cut_Lab_2_NACM<<- "University Student Mean + 1Sd"
      Cut_Lab_3_NACM<<- "University Student Mean + 2Sd"
      Source_Cutoff_NACM_1<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Cutoff_NACM_2<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Cutoff_NACM_3<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Mean_Val_Hyperarousal<<- 5.5
      Sd_Val_Hyperarousal<<- 5.3
      Source_Mean_Hyperarousal<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Sd_Hyperarousal<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Cut_Val_Hyperarousal_1<<- Mean_Val_Hyperarousal 
      Cut_Val_Hyperarousal_2<<- Mean_Val_Hyperarousal + Sd_Val_Hyperarousal
      Cut_Val_Hyperarousal_3<<- Mean_Val_Hyperarousal + (2*Sd_Val_Hyperarousal)
      Cut_Lab_1_Hyperarousal<<- "University Student Mean"
      Cut_Lab_2_Hyperarousal<<- "University Student Mean + 1Sd"
      Cut_Lab_3_Hyperarousal<<- "University Student Mean + 2Sd"
      Source_Cutoff_Hyperarousal_1<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Cutoff_Hyperarousal_2<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Cutoff_Hyperarousal_3<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Mean_Val_Avoidance<<- 2.7
      Sd_Val_Avoidance<<- 2.4
      Source_Mean_Avoidance<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Sd_Avoidance<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Cut_Val_Avoidance_1<<- Mean_Val_Avoidance 
      Cut_Val_Avoidance_2<<- Mean_Val_Avoidance + Sd_Val_Avoidance
      Cut_Val_Avoidance_3<<- Mean_Val_Avoidance + (2*Sd_Val_Avoidance)
      Cut_Lab_1_Avoidance<<- "University Student Mean"
      Cut_Lab_2_Avoidance<<- "University Student Mean + 1Sd"
      Cut_Lab_3_Avoidance<<- "University Student Mean + 2Sd"
      Source_Cutoff_Avoidance_1<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Cutoff_Avoidance_2<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
      Source_Cutoff_Avoidance_3<<- "Ashbaugh, Houle-Johnson, El-Hage & Brunet (2016)"
    } else if(input$Pop == "Active Military Service") {
      Mean_Val<<- 42.41
      Sd_Val<<- 15.06
      Source_Mean<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Sd<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Cut_Val_1<<- 33
      Cut_Val_2<<- 38
      Cut_Val_3<<- Mean_Val
      Cut_Lab_1<<- "PTSD Cut-Off: Bovin et al (2015)"
      Cut_Lab_2<<- "PTSD Cut-Off: Scale Developers"
      Cut_Lab_3<<- "Active Military Service Mean"
      Source_Cutoff_1<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Cutoff_2<<- "Weathers, Litz, Keane, Palmieri, Marx et al (2013)"
      Source_Cutoff_3<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Mean_Val_Intrusion<<- 10.56
      Sd_Val_Intrusion<<- 4.45
      Source_Mean_Intrusion<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Sd_Intrusion<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Cut_Val_Intrusion_1<<- Mean_Val_Intrusion 
      Cut_Val_Intrusion_2<<- Mean_Val_Intrusion + Sd_Val_Intrusion
      Cut_Val_Intrusion_3<<- Mean_Val_Intrusion + (2*Sd_Val_Intrusion)
      Cut_Lab_1_Intrusion<<- "Active Military Service Mean"
      Cut_Lab_2_Intrusion<<- "Active Military Service Mean + 1Sd"
      Cut_Lab_3_Intrusion<<- "Active Military Service Mean + 2Sd"
      Source_Cutoff_Intrusion_1<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Cutoff_Intrusion_2<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Cutoff_Intrusion_3<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Mean_Val_NACM<<- 12.91
      Sd_Val_NACM<<- 6.59
      Source_Mean_NACM<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Sd_NACM<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Cut_Val_NACM_1<<- Mean_Val_NACM 
      Cut_Val_NACM_2<<- Mean_Val_NACM + Sd_Val_NACM
      Cut_Val_NACM_3<<- Mean_Val_NACM + (2*Sd_Val_NACM)
      Cut_Lab_1_NACM<<- "Active Military Service Mean"
      Cut_Lab_2_NACM<<- "Active Military Service Mean + 1Sd"
      Cut_Lab_3_NACM<<- "Active Military Service Mean + 2Sd"
      Source_Cutoff_NACM_1<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Cutoff_NACM_2<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Cutoff_NACM_3<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Mean_Val_Hyperarousal<<- 14.07
      Sd_Val_Hyperarousal<<- 4.68
      Source_Mean_Hyperarousal<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Sd_Hyperarousal<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Cut_Val_Hyperarousal_1<<- Mean_Val_Hyperarousal 
      Cut_Val_Hyperarousal_2<<- Mean_Val_Hyperarousal + Sd_Val_Hyperarousal
      Cut_Val_Hyperarousal_3<<- Mean_Val_Hyperarousal + (2*Sd_Val_Hyperarousal)
      Cut_Lab_1_Hyperarousal<<- "Active Military Service Mean"
      Cut_Lab_2_Hyperarousal<<- "Active Military Service Mean + 1Sd"
      Cut_Lab_3_Hyperarousal<<- "Active Military Service Mean + 2Sd"
      Source_Cutoff_Hyperarousal_1<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Cutoff_Hyperarousal_2<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Cutoff_Hyperarousal_3<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Mean_Val_Avoidance<<- 4.88
      Sd_Val_Avoidance<<- 2.35
      Source_Mean_Avoidance<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Sd_Avoidance<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Cut_Val_Avoidance_1<<- Mean_Val_Avoidance 
      Cut_Val_Avoidance_2<<- Mean_Val_Avoidance + Sd_Val_Avoidance
      Cut_Val_Avoidance_3<<- Mean_Val_Avoidance + (2*Sd_Val_Avoidance)
      Cut_Lab_1_Avoidance<<- "Active Military Service Mean"
      Cut_Lab_2_Avoidance<<- "Active Military Service Mean + 1Sd"
      Cut_Lab_3_Avoidance<<- "Active Military Service Mean + 2Sd"
      Source_Cutoff_Avoidance_1<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Cutoff_Avoidance_2<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Cutoff_Avoidance_3<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
    } else if(input$Pop == "Veteran") {
      Mean_Val<<- 36.97
      Sd_Val<<- 21.16
      Source_Mean<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Sd<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Cut_Val_1<<- 33
      Cut_Val_2<<- 36.97
      Cut_Val_3<<- 38
      Cut_Lab_1<<- "PTSD Cut-Off: Bovin et al (2015)"
      Cut_Lab_2<<- "Veteran Mean"
      Cut_Lab_3<<- "PTSD Cut-Off: Scale Developers"
      Source_Cutoff_1<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Cutoff_2<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Cutoff_3<<-  "Weathers, Litz, Keane, Palmieri, Marx et al (2013)"
      Mean_Val_Intrusion<<- 9.28
      Sd_Val_Intrusion<<- 5.87
      Source_Mean_Intrusion<<-"Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Sd_Intrusion<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Cut_Val_Intrusion_1<<- Mean_Val_Intrusion 
      Cut_Val_Intrusion_2<<- Mean_Val_Intrusion + Sd_Val_Intrusion
      Cut_Val_Intrusion_3<<- Mean_Val_Intrusion + (2*Sd_Val_Intrusion)
      Cut_Lab_1_Intrusion<<- "Veteran Mean"
      Cut_Lab_2_Intrusion<<- "Veteran Mean + 1Sd"
      Cut_Lab_3_Intrusion<<- "Veteran Mean + 2Sd"
      Source_Cutoff_Intrusion_1<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Cutoff_Intrusion_2<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Cutoff_Intrusion_3<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Mean_Val_NACM<<- 12.54
      Sd_Val_NACM<<- 8.15
      Source_Mean_NACM<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)" 
      Source_Sd_NACM<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Cut_Val_NACM_1<<- Mean_Val_NACM 
      Cut_Val_NACM_2<<- Mean_Val_NACM + Sd_Val_NACM
      Cut_Val_NACM_3<<- Mean_Val_NACM + (2*Sd_Val_NACM)
      Cut_Lab_1_NACM<<- "Veteran Mean"
      Cut_Lab_2_NACM<<- "Veteran Mean + 1Sd"
      Cut_Lab_3_NACM<<- "Veteran Mean + 2Sd"
      Source_Cutoff_NACM_1<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Cutoff_NACM_2<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Cutoff_NACM_3<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Mean_Val_Hyperarousal<<- 11.09
      Sd_Val_Hyperarousal<<- 6.75
      Source_Mean_Hyperarousal<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Sd_Hyperarousal<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Cut_Val_Hyperarousal_1<<- Mean_Val_Hyperarousal 
      Cut_Val_Hyperarousal_2<<- Mean_Val_Hyperarousal + Sd_Val_Hyperarousal
      Cut_Val_Hyperarousal_3<<- Mean_Val_Hyperarousal + (2*Sd_Val_Hyperarousal)
      Cut_Lab_1_Hyperarousal<<- "Veteran Mean"
      Cut_Lab_2_Hyperarousal<<- "Veteran Mean + 1Sd"
      Cut_Lab_3_Hyperarousal<<- "Veteran Mean + 2Sd"
      Source_Cutoff_Hyperarousal_1<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Cutoff_Hyperarousal_2<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Cutoff_Hyperarousal_3<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Mean_Val_Avoidance<<- 4.06
      Sd_Val_Avoidance<<- 2.6
      Source_Mean_Avoidance<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Source_Sd_Avoidance<<- "Wortmann, Jordan, Resick, Foa, Yarvis et al (2016)"
      Cut_Val_Avoidance_1<<- Mean_Val_Avoidance 
      Cut_Val_Avoidance_2<<- Mean_Val_Avoidance + Sd_Val_Avoidance
      Cut_Val_Avoidance_3<<- Mean_Val_Avoidance + (2*Sd_Val_Avoidance)
      Cut_Lab_1_Avoidance<<- "Veteran Mean"
      Cut_Lab_2_Avoidance<<- "Veteran Mean + 1Sd"
      Cut_Lab_3_Avoidance<<- "Veteran Mean + 2Sd"
      Source_Cutoff_Avoidance_1<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Cutoff_Avoidance_2<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Cutoff_Avoidance_3<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
    } else if(input$Pop == "Institutional Abuse") {
      Mean_Val<<- 28.28
      Sd_Val<<- 18.19
      Source_Mean<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Sd<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Cut_Val_1<<- Mean_Val
      Cut_Val_2<<- 33
      Cut_Val_3<<- 38
      Cut_Lab_1<<- "Institutional Abuse Mean"
      Cut_Lab_2<<- "PTSD Cut-Off: Bovin et al (2015)"
      Cut_Lab_3<<- "PTSD Cut-Off: Scale Developers"
      Source_Cutoff_1<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Cutoff_2<<- "Bovin, Marx, Gallagher, Schnurr, Weathers et al (2015)"
      Source_Cutoff_3<<- "Weathers, Litz, Keane, Palmieri, Marx et al (2013)"
      Mean_Val_Intrusion<<- 7.92
      Sd_Val_Intrusion<<- 6.35
      Source_Mean_Intrusion<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Sd_Intrusion<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Cut_Val_Intrusion_1<<- Mean_Val_Intrusion 
      Cut_Val_Intrusion_2<<- Mean_Val_Intrusion + Sd_Val_Intrusion
      Cut_Val_Intrusion_3<<- Mean_Val_Intrusion + (2*Sd_Val_Intrusion)
      Cut_Lab_1_Intrusion<<- "Institutional Abuse Mean"
      Cut_Lab_2_Intrusion<<- "Institutional Abuse Mean + 1Sd"
      Cut_Lab_3_Intrusion<<- "Institutional Abuse Mean + 2Sd"
      Source_Cutoff_Intrusion_1<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Cutoff_Intrusion_2<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Cutoff_Intrusion_3<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Mean_Val_NACM<<- 8.26
      Sd_Val_NACM<<- 6.8
      Source_Mean_NACM<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Sd_NACM<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Cut_Val_NACM_1<<- Mean_Val_NACM 
      Cut_Val_NACM_2<<- Mean_Val_NACM + Sd_Val_NACM
      Cut_Val_NACM_3<<- Mean_Val_NACM + (2*Sd_Val_NACM)
      Cut_Lab_1_NACM<<- "Institutional Abuse Mean"
      Cut_Lab_2_NACM<<- "Institutional Abuse Mean + 1Sd"
      Cut_Lab_3_NACM<<- "Institutional Abuse Mean + 2Sd"
      Source_Cutoff_NACM_1<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Cutoff_NACM_2<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Cutoff_NACM_3<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Mean_Val_Hyperarousal<<- 8.91
      Sd_Val_Hyperarousal<<- 5.43
      Source_Mean_Hyperarousal<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Sd_Hyperarousal<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Cut_Val_Hyperarousal_1<<- Mean_Val_Hyperarousal 
      Cut_Val_Hyperarousal_2<<- Mean_Val_Hyperarousal + Sd_Val_Hyperarousal
      Cut_Val_Hyperarousal_3<<- Mean_Val_Hyperarousal + (2*Sd_Val_Hyperarousal)
      Cut_Lab_1_Hyperarousal<<- "Institutional Abuse Mean"
      Cut_Lab_2_Hyperarousal<<- "Institutional Abuse Mean + 1Sd"
      Cut_Lab_3_Hyperarousal<<- "Institutional Abuse Mean + 2Sd"
      Source_Cutoff_Hyperarousal_1<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Cutoff_Hyperarousal_2<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Cutoff_Hyperarousal_3<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Mean_Val_Avoidance<<- 3.19
      Sd_Val_Avoidance<<- 2.72
      Source_Mean_Avoidance<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Sd_Avoidance<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Cut_Val_Avoidance_1<<- Mean_Val_Avoidance 
      Cut_Val_Avoidance_2<<- Mean_Val_Avoidance + Sd_Val_Avoidance
      Cut_Val_Avoidance_3<<- Mean_Val_Avoidance + (2*Sd_Val_Avoidance)
      Cut_Lab_1_Avoidance<<- "Institutional Abuse Mean"
      Cut_Lab_2_Avoidance<<- "Institutional Abuse Mean + 1Sd"
      Cut_Lab_3_Avoidance<<- "Institutional Abuse Mean + 2Sd"
      Source_Cutoff_Avoidance_1<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Cutoff_Avoidance_2<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
      Source_Cutoff_Avoidance_3<<- "Lueger-Schuster, Knefel, Glück, Jagsch, Kantor et al (2018)"
    }
    
  })
  
  
  output$Mean_Widg<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean", "PCL-5 total scale", Mean_Val),
      h6(paste("Reference:", Source_Mean))
          )
  })
  outputOptions(output, "Mean_Widg", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Intrusion<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Intrusion", "Intrusion", Mean_Val_Intrusion),
      h6(paste("Reference:", Source_Mean_Intrusion))
          )
  })
  outputOptions(output, "Mean_Widg_Intrusion", suspendWhenHidden = FALSE)
  

  output$Mean_Widg_NACM<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_NACM", "Cognition Mood", Mean_Val_NACM),
      h6(paste("Reference:", Source_Mean_NACM))
          )
  })
  outputOptions(output, "Mean_Widg_NACM", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Hyperarousal<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Hyperarousal", "Hyperarousal", Mean_Val_Hyperarousal),
      h6(paste("Reference:", Source_Mean_Hyperarousal))
          )
  })
  outputOptions(output, "Mean_Widg_Hyperarousal", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Avoidance<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Avoidance", "Avoidance", Mean_Val_Avoidance),
      h6(paste("Reference:", Source_Mean_Avoidance))
           )
  })
  outputOptions(output, "Mean_Widg_Avoidance", suspendWhenHidden = FALSE) 
  
  
  output$Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd", "PCL-5 total scale", Sd_Val),
      h6(paste("Reference:", Source_Sd))
           )
  })
  outputOptions(output, "Sd_Widg", suspendWhenHidden = FALSE)
  

  output$Sd_Widg_Intrusion<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Intrusion", "Intrusion", Sd_Val_Intrusion),
      h6(paste("Reference:", Source_Sd_Intrusion))
          )
  })
  outputOptions(output, "Sd_Widg_Intrusion", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_NACM<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_NACM", "Cognition Mood", Sd_Val_NACM),
      h6(paste("Reference:", Source_Sd_NACM))
          )
  })
  outputOptions(output, "Sd_Widg_NACM", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Hyperarousal<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Hyperarousal", "Hyperarousal", Sd_Val_Hyperarousal),
      h6(paste("Reference:", Source_Sd_Hyperarousal))
          )
  })
  outputOptions(output, "Sd_Widg_Hyperarousal", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Avoidance<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Avoidance", "Avoidance", Sd_Val_Avoidance),
      h6(paste("Reference:", Source_Sd_Avoidance))
          )
  })
  outputOptions(output, "Sd_Widg_Avoidance", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_1", "PCL-5 total scale", as.numeric(Cut_Val_1)),
      textInput("Cutoff_Text_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source_Cutoff_1))
          )
  })
  outputOptions(output, "Cutoff_Widg_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Intrusion_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Intrusion_1", "Intrusion", as.numeric(Cut_Val_Intrusion_1)),
      textInput("Cutoff_Text_Intrusion_1", "Cut-Off Score Name", Cut_Lab_1_Intrusion),
      h6(paste("Reference:", Source_Cutoff_Intrusion_1))
          )
  })
  outputOptions(output, "Cutoff_Widg_Intrusion_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_NACM_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_NACM_1", "Cognition Mood", as.numeric(Cut_Val_NACM_1)),
      textInput("Cutoff_Text_NACM_1", "Cut-Off Score Name", Cut_Lab_1_NACM),
      h6(paste("Reference:", Source_Cutoff_NACM_1))
           )
  })
  outputOptions(output, "Cutoff_Widg_NACM_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Hyperarousal_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Hyperarousal_1", "Hyperarousal", as.numeric(Cut_Val_Hyperarousal_1)),
      textInput("Cutoff_Text_Hyperarousal_1", "Cut-Off Score Name", Cut_Lab_1_Hyperarousal),
      h6(paste("Reference:", Source_Cutoff_Hyperarousal_1))
          )
  })
  outputOptions(output, "Cutoff_Widg_Hyperarousal_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Avoidance_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Avoidance_1", "Avoidance", as.numeric(Cut_Val_Avoidance_1)),
      textInput("Cutoff_Text_Avoidance_1", "Cut-Off Score Name", Cut_Lab_1_Avoidance),
      h6(paste("Reference:", Source_Cutoff_Avoidance_1))
           )
  })
  outputOptions(output, "Cutoff_Widg_Avoidance_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_2", "PCL-5 total scale", as.numeric(Cut_Val_2)),
      textInput("Cutoff_Text_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source_Cutoff_2))
          )
  })
  outputOptions(output, "Cutoff_Widg_2", suspendWhenHidden = FALSE)
  

  output$Cutoff_Widg_Intrusion_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Intrusion_2", "Intrusion", as.numeric(Cut_Val_Intrusion_2)),
      textInput("Cutoff_Text_Intrusion_2", "Cut-Off Score Name", Cut_Lab_2_Intrusion),
      h6(paste("Reference:", Source_Cutoff_Intrusion_2))
           )
  })
  outputOptions(output, "Cutoff_Widg_Intrusion_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_NACM_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_NACM_2", "Cognition Mood", as.numeric(Cut_Val_NACM_2)),
      textInput("Cutoff_Text_NACM_2", "Cut-Off Score Name", Cut_Lab_2_NACM),
      h6(paste("Reference:", Source_Cutoff_NACM_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_NACM_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Hyperarousal_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Hyperarousal_2", "Hyperarousal", as.numeric(Cut_Val_Hyperarousal_2)),
      textInput("Cutoff_Text_Hyperarousal_2", "Cut-Off Score Name", Cut_Lab_2_Hyperarousal),
      h6(paste("Reference:", Source_Cutoff_Hyperarousal_2))
          )
  })
  outputOptions(output, "Cutoff_Widg_Hyperarousal_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Avoidance_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Avoidance_2", "Avoidance", as.numeric(Cut_Val_Avoidance_2)),
      textInput("Cutoff_Text_Avoidance_2", "Cut-Off Score Name", Cut_Lab_2_Avoidance),
      h6(paste("Reference:", Source_Cutoff_Avoidance_2))
          )
  })
  outputOptions(output, "Cutoff_Widg_Avoidance_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_3", "PCL-5 total scale", as.numeric(Cut_Val_3)),
      textInput("Cutoff_Text_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source_Cutoff_3))
          )
  })
  outputOptions(output, "Cutoff_Widg_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Intrusion_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Intrusion_3", "Intrusion", as.numeric(Cut_Val_Intrusion_3)),
      textInput("Cutoff_Text_Intrusion_3", "Cut-Off Score Name", Cut_Lab_3_Intrusion),
      h6(paste("Reference:", Source_Cutoff_Intrusion_3))
          )
  })
  outputOptions(output, "Cutoff_Widg_Intrusion_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_NACM_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_NACM_3", "Cognition Mood", as.numeric(Cut_Val_NACM_3)),
      textInput("Cutoff_Text_NACM_3", "Cut-Off Score Name", Cut_Lab_3_NACM),
      h6(paste("Reference:", Source_Cutoff_NACM_3))
           )
  })
  outputOptions(output, "Cutoff_Widg_NACM_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Hyperarousal_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Hyperarousal_3", "Hyperarousal", as.numeric(Cut_Val_Hyperarousal_3)),
      textInput("Cutoff_Text_Hyperarousal_3", "Cut-Off Score Name", Cut_Lab_3_Hyperarousal),
      h6(paste("Reference:", Source_Cutoff_Hyperarousal_3))
          )
  })
  outputOptions(output, "Cutoff_Widg_Hyperarousal_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Avoidance_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Avoidance_3", "Avoidance", as.numeric(Cut_Val_Avoidance_3)),
      textInput("Cutoff_Text_Avoidance_3", "Cut-Off Score Name", Cut_Lab_3_Avoidance),
      h6(paste("Reference:", Source_Cutoff_Avoidance_3))
          )
  })
  outputOptions(output, "Cutoff_Widg_Avoidance_3", suspendWhenHidden = FALSE)
  
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
    M_Intrusion<- input$Pop_Mean_Intrusion
    SD_Intrusion<-input$Pop_Sd_Intrusion
    M_NACM<- input$Pop_Mean_NACM
    SD_NACM<- input$Pop_Sd_NACM
    M_Hyperarousal<- input$Pop_Mean_Hyperarousal
    SD_Hyperarousal<- input$Pop_Sd_Hyperarousal
    M_Avoidance<- input$Pop_Mean_Avoidance
    SD_Avoidance<- input$Pop_Sd_Avoidance
    
    M_Retest<- input$Retest_Mean
    SD_Retest<- input$Retest_Sd
    M_Retest_Intrusion<- input$Retest_Mean_Intrusion
    SD_Retest_Intrusion<- input$Retest_Sd_Intrusion
    M_Retest_NACM<- input$Retest_Mean_NACM
    SD_Retest_NACM<- input$Retest_Sd_NACM
    M_Retest_Hyperarousal<- input$Retest_Mean_Hyperarousal
    SD_Retest_Hyperarousal<- input$Retest_Sd_Hyperarousal
    M_Retest_Avoidance<- input$Retest_Mean_Avoidance
    SD_Retest_Avoidance<- input$Retest_Sd_Avoidance
    
    SampleN<- input$SampleN
    
    Rel<- input$Reliability
    Rel_Intrusion<- input$Reliability_Intrusion
    Rel_NACM<- input$Reliability_NACM
    Rel_Hyperarousal<- input$Reliability_Hyperarousal
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
      SE_Intrusion<-SD_Intrusion * sqrt(1 - Rel_Intrusion^2)
      SE_NACM<-SD_NACM * sqrt(1 - Rel_NACM^2)
      SE_Hyperarousal<-SD_Hyperarousal * sqrt(1 - Rel_Hyperarousal^2)
      SE_Avoidance<-SD_Avoidance * sqrt(1 - Rel_Avoidance^2)
      SE<- round(SE, digits = 2)
      SE_Intrusion<- round(SE_Intrusion, digits = 2)
      SE_NACM<- round(SE_NACM, digits = 2)
      SE_Hyperarousal<- round(SE_Hyperarousal, digits = 2)
      SE_Avoidance<- round(SE_Avoidance, digits = 2)
    } else if (input$RelChangeMethod == "Chelune et al. (1993)" | input$RelChangeMethod == "Jacobson & Truax (1991)" | input$RelChangeMethod == "Speer (1992)") {
      SE<- sqrt((2*(SD^2))*(1-Rel))
      SE_Intrusion<- sqrt((2*(SD_Intrusion^2))*(1-Rel_Intrusion))
      SE_NACM<- sqrt((2*(SD_NACM^2))*(1-Rel_NACM))
      SE_Hyperarousal<- sqrt((2*(SD_Hyperarousal^2))*(1-Rel_Hyperarousal))
      SE_Avoidance<- sqrt((2*(SD_Avoidance^2))*(1-Rel_Avoidance))
      SE<- round(SE, digits = 2)
      SE_Intrusion<- round(SE_Intrusion, digits = 2)
      SE_NACM<- round(SE_NACM, digits = 2)
      SE_Hyperarousal<- round(SE_Hyperarousal, digits = 2)
      SE_Avoidance<- round(SE_Avoidance, digits = 2)
    } else if (input$RelChangeMethod == "Maassen et al. (2006)") {
      SE<- sqrt((SD^2 + SD_Retest^2)*(1-Rel))
      SE_Intrusion<- sqrt((SD_Intrusion^2 + SD_Retest_Intrusion^2)*(1-Rel_Intrusion))
      SE_NACM<- sqrt((SD_NACM^2 + SD_Retest_NACM^2)*(1-Rel_NACM))
      SE_Hyperarousal<- sqrt((SD_Hyperarousal^2 + SD_Retest_Hyperarousal^2)*(1-Rel_Hyperarousal))
      SE_Avoidance<- sqrt((SD_Avoidance^2 + SD_Retest_Avoidance^2)*(1-Rel_Avoidance))
      SE<- round(SE, digits = 2)
      SE_Intrusion<- round(SE_Intrusion, digits = 2)
      SE_NACM<- round(SE_NACM, digits = 2)
      SE_Hyperarousal<- round(SE_Hyperarousal, digits = 2)
      SE_Avoidance<- round(SE_Avoidance, digits = 2)
    } else if (input$RelChangeMethod == "McSweeny et al. (1993)") {
      SE<- SD_Retest*sqrt(1 - Rel^2)
      SE_Intrusion<- SD_Retest_Intrusion*sqrt(1 - Rel_Intrusion^2)
      SE_NACM<- SD_Retest_NACM*sqrt(1 - Rel_NACM^2)
      SE_Hyperarousal<- SD_Retest_Hyperarousal*sqrt(1 - Rel_Hyperarousal^2)
      SE_Avoidance<- SD_Retest_Avoidance*sqrt(1 - Rel_Avoidance^2)
      SE<- round(SE, digits = 2)
      SE_Intrusion<- round(SE_Intrusion, digits = 2)
      SE_NACM<- round(SE_NACM, digits = 2)
      SE_Hyperarousal<- round(SE_Hyperarousal, digits = 2)
      SE_Avoidance<- round(SE_Avoidance, digits = 2)
    }
    
    
    #Caclculate McSweeny standard error to be employed in deriving the Crawford & Howell standard error if selected
    
    McSweeny_SE<- SD_Retest*sqrt(1 - Rel^2)
    McSweeny_SE_Intrusion<- SD_Retest_Intrusion*sqrt(1 - Rel_Intrusion^2)
    McSweeny_SE_NACM<- SD_Retest_NACM*sqrt(1 - Rel_NACM^2)
    McSweeny_SE_Hyperarousal<- SD_Retest_Hyperarousal*sqrt(1 - Rel_Hyperarousal^2)
    McSweeny_SE_Avoidance<- SD_Retest_Avoidance*sqrt(1 - Rel_Avoidance^2)
    
    #Generate object storing names of cut-off scores
    
    Cutoff_Name_1<- input$Cutoff_Text_1
    Cutoff_Name_2<- input$Cutoff_Text_2
    Cutoff_Name_3<- input$Cutoff_Text_3
    Cutoff_Name_Intrusion_1<- input$Cutoff_Text_Intrusion_1
    Cutoff_Name_Intrusion_2<- input$Cutoff_Text_Intrusion_2
    Cutoff_Name_Intrusion_3<- input$Cutoff_Text_Intrusion_3
    Cutoff_Name_NACM_1<- input$Cutoff_Text_NACM_1
    Cutoff_Name_NACM_2<- input$Cutoff_Text_NACM_2
    Cutoff_Name_NACM_3<- input$Cutoff_Text_NACM_3
    Cutoff_Name_Hyperarousal_1<- input$Cutoff_Text_Hyperarousal_1
    Cutoff_Name_Hyperarousal_2<- input$Cutoff_Text_Hyperarousal_2
    Cutoff_Name_Hyperarousal_3<- input$Cutoff_Text_Hyperarousal_3
    Cutoff_Name_Avoidance_1<- input$Cutoff_Text_Avoidance_1
    Cutoff_Name_Avoidance_2<- input$Cutoff_Text_Avoidance_2
    Cutoff_Name_Avoidance_3<- input$Cutoff_Text_Avoidance_3
    Cutoff_Names<<- data.frame(Cutoff_Name_1,Cutoff_Name_2,Cutoff_Name_3,Cutoff_Name_Intrusion_1,Cutoff_Name_Intrusion_2,Cutoff_Name_Intrusion_3,
                               Cutoff_Name_NACM_1, Cutoff_Name_NACM_2, Cutoff_Name_NACM_3, Cutoff_Name_Hyperarousal_1,
                               Cutoff_Name_Hyperarousal_2, Cutoff_Name_Hyperarousal_3, Cutoff_Name_Avoidance_1, Cutoff_Name_Avoidance_2, Cutoff_Name_Avoidance_3)
    
    #Define the data & calculate PS, SE, confidence intervals and cut-off scores
    
    if(input$Timepoint == "1") {
      Date<- input$Date_1
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_1a), Score = Score_1a)
      Score<- sum(Score_1a, na.rm = TRUE)
      Score<- round(Score, digits = 2)
      Score_Intrusion<- sum(Score_1a[c(1,2,3,4,5)], na.rm = TRUE)
      Score_NACM<- sum(Score_1a[c(8,9,10,11,12,13,14)], na.rm = TRUE)
      Score_Hyperarousal<- sum(Score_1a[c(15,16,17,18,19,20)], na.rm = TRUE)
      Score_Avoidance<- sum(Score_1a[c(6,7)], na.rm = TRUE) 
      Change<- 0
      Change<- round(Change, digits = 2)
      Change_Intrusion<- 0
      Change_NACM<- 0
      Change_Hyperarousal<- 0
      Change_Avoidance<- 0
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS<- (Rel * Score) + (M * (1 - Rel))
        PTS_Intrusion<- (Rel_Intrusion * Score_Intrusion) + (M_Intrusion * (1 - Rel_Intrusion))
        PTS_NACM<- (Rel_NACM * Score_NACM) + (M_NACM * (1 - Rel_NACM))
        PTS_Hyperarousal<- (Rel_Hyperarousal * Score_Hyperarousal) + (M_Hyperarousal * (1 - Rel_Hyperarousal))
        PTS_Avoidance<- (Rel_Avoidance * Score_Avoidance) + (M_Avoidance * (1 - Rel_Avoidance))
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS<- Score + (M_Retest - M)  
        PTS_Intrusion<- Score_Intrusion + (M_Retest_Intrusion - M_Intrusion)  
        PTS_NACM<- Score_NACM + (M_Retest_NACM - M_NACM)  
        PTS_Hyperarousal<- Score_Hyperarousal + (M_Retest_Hyperarousal - M_Hyperarousal) 
        PTS_Avoidance<- Score_Avoidance + (M_Retest_Avoidance - M_Avoidance) 
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Intrusion<- Score_Intrusion
        PTS_NACM<- Score_NACM
        PTS_Hyperarousal<- Score_Hyperarousal
        PTS_Avoidance<- Score_Avoidance
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS<- (B_Adj * Score) + A_Adj
        B_Slope_Intrusion<- Rel_Intrusion * (SD_Retest_Intrusion/SD_Intrusion)
        A_Constant_Intrusion<- M_Retest_Intrusion - (B_Slope_Intrusion * M_Intrusion)
        B_Adj_Intrusion<- SD_Retest_Intrusion/SD_Intrusion
        A_Adj_Intrusion<- M_Retest_Intrusion - (B_Adj_Intrusion * M_Intrusion)
        PTS_Intrusion<- (B_Adj_Intrusion * Score_Intrusion) + A_Adj_Intrusion
        B_Slope_NACM<- Rel_NACM * (SD_Retest_NACM/SD_NACM)
        A_Constant_NACM<- M_Retest_NACM - (B_Slope_NACM * M_NACM)
        B_Adj_NACM<- SD_Retest_NACM/SD_NACM
        A_Adj_NACM<- M_Retest_NACM - (B_Adj_NACM * M_NACM)
        PTS_NACM<- (B_Adj_NACM * Score_NACM) + A_Adj_NACM
        B_Slope_Hyperarousal<- Rel_Hyperarousal * (SD_Retest_Hyperarousal/SD_Hyperarousal)
        A_Constant_Hyperarousal<- M_Retest_Hyperarousal - (B_Slope_Hyperarousal * M_Hyperarousal)
        B_Adj_Hyperarousal<- SD_Retest_Hyperarousal/SD_Hyperarousal
        A_Adj_Hyperarousal<- M_Retest_Hyperarousal - (B_Adj_Hyperarousal * M_Hyperarousal)
        PTS_Hyperarousal<- (B_Adj_Hyperarousal * Score_Hyperarousal) + A_Adj_Hyperarousal
        B_Slope_Avoidance<- Rel_Avoidance * (SD_Retest_Avoidance/SD_Avoidance)
        A_Constant_Avoidance<- M_Retest_Avoidance - (B_Slope_Avoidance * M_Avoidance)
        B_Adj_Avoidance<- SD_Retest_Avoidance/SD_Avoidance
        A_Adj_Avoidance<- M_Retest_Avoidance - (B_Adj_Avoidance * M_Avoidance)
        PTS_Avoidance<- (B_Adj_Avoidance * Score_Avoidance) + A_Adj_Avoidance
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS<- B_Slope * Score
        B_Slope_Intrusion<- Rel_Intrusion * (SD_Retest_Intrusion/SD_Intrusion)
        PTS_Intrusion<- B_Slope_Intrusion * Score_Intrusion
        B_Slope_NACM<- Rel_NACM * (SD_Retest_NACM/SD_NACM)
        PTS_NACM<- B_Slope_NACM * Score_NACM
        B_Slope_Hyperarousal<- Rel_Hyperarousal * (SD_Retest_Hyperarousal/SD_Hyperarousal)
        PTS_Hyperarousal<- B_Slope_Hyperarousal * Score_Hyperarousal
        B_Slope_Avoidance<- Rel_Avoidance * (SD_Retest_Avoidance/SD_Avoidance)
        PTS_Avoidance<- B_Slope_Avoidance * Score_Avoidance
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS<- Score + (M_Retest - M)
        PTS_Intrusion<- Score_Intrusion + (M_Retest_Intrusion - M_Intrusion)
        PTS<- NACM<- Score_NACM + (M_Retest_NACM - M_NACM)
        PTS_Hyperarousal<- Score_Hyperarousal + (M_Retest_Hyperarousal - M_Hyperarousal)
        PTS_Avoidance<- Score_Avoidance + (M_Retest_Avoidance - M_Avoidance)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Intrusion<- round(PTS_Intrusion, digits = 2)
      PTS_NACM<- round(PTS_NACM, digits = 2)
      PTS_Hyperarousal<- round(PTS_Hyperarousal, digits = 2)
      PTS_Avoidance<- round(PTS_Avoidance, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score - M)^2/(SD^2*(SampleN-1))))
        SE_Intrusion<- McSweeny_SE_Intrusion*sqrt(1 + (1/SampleN) + ((Score_Intrusion - M_Intrusion)^2/(SD_Intrusion^2*(SampleN-1))))
        SE_NACM<- McSweeny_SE_NACM*sqrt(1 + (1/SampleN) + ((Score_NACM - M_NACM)^2/(SD_NACM^2*(SampleN-1))))
        SE_Hyperarousal<- McSweeny_SE_Hyperarousal*sqrt(1 + (1/SampleN) + ((Score_Hyperarousal - M_Hyperarousal)^2/(SD_Hyperarousal^2*(SampleN-1))))
        SE_Avoidance<- McSweeny_SE_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Avoidance - M_Avoidance)^2/(SD_Avoidance^2*(SampleN-1))))
        SE<- round(SE, digits = 2)
        SE_Intrusion<- round(SE_Intrusion, digits = 2)
        SE_NACM<- round(SE_NACM, digits = 2)
        SE_Hyperarousal<- round(SE_Hyperarousal, digits = 2)
        SE_Avoidance<- round(SE_Avoidance, digits = 2)
        CI<- (Conf*SE)
        CI<- round(CI, digits = 2)
        CI_Intrusion<- (Conf*SE_Intrusion)
        CI_Intrusion<- round(CI_Intrusion, digits = 2)
        CI_NACM<- (Conf*SE_NACM)
        CI_NACM<- round(CI_NACM, digits = 2)
        CI_Hyperarousal<- (Conf*SE_Hyperarousal)
        CI_Hyperarousal<- round(CI_Hyperarousal, digits = 2)
        CI_Avoidance<- (Conf*SE_Avoidance)
        CI_Avoidance<- round(CI_Avoidance, digits = 2)
      }
      CI<- (Conf*SE)
      CI<- round(CI, digits = 2)
      CI_Intrusion<- (Conf*SE_Intrusion)
      CI_Intrusion<- round(CI_Intrusion, digits = 2)
      CI_NACM<- (Conf*SE_NACM)
      CI_NACM<- round(CI_NACM, digits = 2)
      CI_Hyperarousal<- (Conf*SE_Hyperarousal)
      CI_Hyperarousal<- round(CI_Hyperarousal, digits = 2)
      CI_Avoidance<- (Conf*SE_Avoidance)
      CI_Avoidance<- round(CI_Avoidance, digits = 2)
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Intrusion<- PTS_Intrusion + CI_Intrusion
      CI_Upper_Lim_Intrusion<- round(CI_Upper_Lim_Intrusion, digits = 2)
      CI_Lower_Lim_Intrusion<-PTS_Intrusion - CI_Intrusion
      CI_Lower_Lim_Intrusion<- round(CI_Lower_Lim_Intrusion, digits = 2)
      CI_Upper_Lim_NACM<- PTS_NACM + CI_NACM
      CI_Upper_Lim_NACM<- round(CI_Upper_Lim_NACM, digits = 2)
      CI_Lower_Lim_NACM<-PTS_NACM - CI_NACM
      CI_Lower_Lim_NACM<- round(CI_Lower_Lim_NACM, digits = 2)
      CI_Upper_Lim_Hyperarousal<- PTS_Hyperarousal + CI_Hyperarousal
      CI_Upper_Lim_Hyperarousal<- round(CI_Upper_Lim_Hyperarousal, digits = 2)
      CI_Lower_Lim_Hyperarousal<-PTS_Hyperarousal - CI_Hyperarousal
      CI_Lower_Lim_Hyperarousal<- round(CI_Lower_Lim_Hyperarousal, digits = 2)
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
      if(input$Select_CI_Intrusion == "2") {
        CI_Intrusion<- input$Man_CI_Intrusion
        CI_Intrusion<- round(CI_Intrusion, digits = 2)
        CI_Upper_Lim_Intrusion<- Score_Intrusion + CI_Intrusion
        CI_Upper_Lim_Intrusion<- round(CI_Upper_Lim_Intrusion, digits = 2)
        CI_Lower_Lim_Intrusion<- Score_Intrusion - CI_Intrusion
        CI_Lower_Lim_Intrusion<- round(CI_Lower_Lim_Intrusion, digits = 2)
      }
      if(input$Select_CI_NACM == "2") {
        CI_NACM<- input$Man_CI_NACM
        CI_NACM<- round(CI_NACM, digits = 2)
        CI_Upper_Lim_NACM<- Score_NACM + CI_NACM
        CI_Upper_Lim_NACM<- round(CI_Upper_Lim_NACM, digits = 2)
        CI_Lower_Lim_NACM<- Score_NACM - CI_NACM
        CI_Lower_Lim_NACM<- round(CI_Lower_Lim_NACM, digits = 2)
      }
      if(input$Select_CI_Hyperarousal == "2") {
        CI_Hyperarousal<- input$Man_CI_Hyperarousal
        CI_Hyperarousal<- round(CI_Hyperarousal, digits = 2)
        CI_Upper_Lim_Hyperarousal<- Score_Hyperarousal + CI_Hyperarousal
        CI_Upper_Lim_Hyperarousal<- round(CI_Upper_Lim_Hyperarousal, digits = 2)
        CI_Lower_Lim_Hyperarousal<- Score_Hyperarousal - CI_Hyperarousal
        CI_Lower_Lim_Hyperarousal<- round(CI_Lower_Lim_Hyperarousal, digits = 2)
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
      Cutoff_Score_Intrusion_1<- round(input$Cutoff_Intrusion_1, digits = 2)
      Cutoff_Score_Intrusion_2<- round(input$Cutoff_Intrusion_2, digits = 2)
      Cutoff_Score_Intrusion_3<- round(input$Cutoff_Intrusion_3, digits = 2)
      Cutoff_Score_NACM_1<- round(input$Cutoff_NACM_1, digits = 2)
      Cutoff_Score_NACM_2<- round(input$Cutoff_NACM_2, digits = 2)
      Cutoff_Score_NACM_3<- round(input$Cutoff_NACM_3, digits = 2)
      Cutoff_Score_Hyperarousal_1<- round(input$Cutoff_Hyperarousal_1, digits = 2)
      Cutoff_Score_Hyperarousal_2<- round(input$Cutoff_Hyperarousal_2, digits = 2)
      Cutoff_Score_Hyperarousal_3<- round(input$Cutoff_Hyperarousal_3, digits = 2)
      Cutoff_Score_Avoidance_1<- round(input$Cutoff_Avoidance_1, digits = 2)
      Cutoff_Score_Avoidance_2<- round(input$Cutoff_Avoidance_2, digits = 2)
      Cutoff_Score_Avoidance_3<- round(input$Cutoff_Avoidance_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Intrusion,Change_Intrusion,PTS_Intrusion, SE_Intrusion, CI_Upper_Lim_Intrusion, CI_Lower_Lim_Intrusion, Cutoff_Score_Intrusion_1,Cutoff_Score_Intrusion_2,Cutoff_Score_Intrusion_3,
                                      Score_NACM,Change_NACM, PTS_NACM, SE_NACM, CI_Upper_Lim_NACM, CI_Lower_Lim_NACM, Cutoff_Score_NACM_1,Cutoff_Score_NACM_2,Cutoff_Score_NACM_3, Score_Hyperarousal,Change_Hyperarousal,PTS_Hyperarousal, 
                                      SE_Hyperarousal, CI_Upper_Lim_Hyperarousal, CI_Lower_Lim_Hyperarousal, Cutoff_Score_Hyperarousal_1,Cutoff_Score_Hyperarousal_2,Cutoff_Score_Hyperarousal_3, 
                                      Score_Avoidance,Change_Avoidance,PTS_Avoidance, SE_Avoidance, CI_Upper_Lim_Avoidance, CI_Lower_Lim_Avoidance, Cutoff_Score_Avoidance_1,Cutoff_Score_Avoidance_2,Cutoff_Score_Avoidance_3)
    } else if(input$Timepoint == "2") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date<- c(Date_1, Date_2)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<- as.numeric(unlist(strsplit(input$Text_1,",")))
      Score_1<- sum(Score_1a, na.rm = TRUE)
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_2a), Score = Score_2a)
      Score_2<- sum(Score_2a, na.rm = TRUE)
      Score<- c(Score_1, Score_2)
      Score<- round(Score, digits = 2)
      Score_Intrusion_1<- sum(Score_1a[c(1,2,3,4,5)], na.rm = TRUE)
      Score_Intrusion_2<- sum(Score_2a[c(1,2,3,4,5)], na.rm = TRUE)
      Score_Intrusion<- c(Score_Intrusion_1,Score_Intrusion_2)
      Score_Intrusion<- round(Score_Intrusion, digits = 2)
      Score_NACM_1<- sum(Score_1a[c(8,9,10,11,12,13,14)], na.rm = TRUE)
      Score_NACM_2<- sum(Score_2a[c(8,9,10,11,12,13,14)], na.rm = TRUE)
      Score_NACM<- c(Score_NACM_1,Score_NACM_2)
      Score_NACM<- round(Score_NACM, digits = 2)
      Score_Hyperarousal_1<- sum(Score_1a[c(15,16,17,18,19,20)], na.rm = TRUE)
      Score_Hyperarousal_2<- sum(Score_2a[c(15,16,17,18,19,20)], na.rm = TRUE)
      Score_Hyperarousal<- c(Score_Hyperarousal_1,Score_Hyperarousal_2)
      Score_Hyperarousal<- round(Score_Hyperarousal, digits = 2)
      Score_Avoidance_1<- sum(Score_1a[c(6,7)], na.rm = TRUE)
      Score_Avoidance_2<- sum(Score_2a[c(6,7)], na.rm = TRUE)
      Score_Avoidance<- c(Score_Avoidance_1,Score_Avoidance_2)
      Score_Avoidance<- round(Score_Avoidance, digits = 2)
      Change<- c(0, (Score_2 - Score_1))
      Change<- round(Change, digits = 2)
      Change_Intrusion<- c(0, (Score_Intrusion_2 - Score_Intrusion_1))
      Change_Intrusion<- round(Change_Intrusion, digits = 2)
      Change_NACM<- c(0, (Score_NACM_2 - Score_NACM_1))
      Change_NACM<- round(Change_NACM, digits = 2)
      Change_Hyperarousal<- c(0, (Score_Hyperarousal_2 - Score_Hyperarousal_1))
      Change_Hyperarousal<- round(Change_Hyperarousal, digits = 2)
      Change_Avoidance<- c(0, (Score_Avoidance_2 - Score_Avoidance_1))
      Change_Avoidance<- round(Change_Avoidance, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2)
        PTS_Intrusion_1<- (Rel_Intrusion * Score_Intrusion_1) + (M_Intrusion * (1 - Rel_Intrusion))
        PTS_Intrusion_2<- (Rel_Intrusion * Score_Intrusion_2) + (M_Intrusion * (1 - Rel_Intrusion))
        PTS_Intrusion<-c(PTS_Intrusion_1, PTS_Intrusion_2)
        PTS_NACM_1<- (Rel_NACM * Score_NACM_1) + (M_NACM * (1 - Rel_NACM))
        PTS_NACM_2<- (Rel_NACM * Score_NACM_2) + (M_NACM * (1 - Rel_NACM))
        PTS_NACM<-c(PTS_NACM_1,PTS_NACM_2)
        PTS_Hyperarousal_1<- (Rel_Hyperarousal * Score_Hyperarousal_1) + (M_Hyperarousal * (1 - Rel_Hyperarousal))
        PTS_Hyperarousal_2<- (Rel_Hyperarousal * Score_Hyperarousal_2) + (M_Hyperarousal * (1 - Rel_Hyperarousal))
        PTS_Hyperarousal<-c(PTS_Hyperarousal_1, PTS_Hyperarousal_2)
        PTS_Avoidance_1<- (Rel_Avoidance * Score_Avoidance_1) + (M_Avoidance * (1 - Rel_Avoidance))
        PTS_Avoidance_2<- (Rel_Avoidance * Score_Avoidance_2) + (M_Avoidance * (1 - Rel_Avoidance))
        PTS_Avoidance<-c(PTS_Avoidance_1, PTS_Avoidance_2)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2)
        PTS_Intrusion_1<- Score_Intrusion_1 + (M_Retest_Intrusion - M_Intrusion)  
        PTS_Intrusion_2<- Score_Intrusion_2 + (M_Retest_Intrusion - M_Intrusion)
        PTS_Intrusion<- c(PTS_Intrusion_1, PTS_Intrusion_2)
        PTS_NACM_1<- Score_NACM_1 + (M_Retest_NACM - M_NACM)  
        PTS_NACM_2<- Score_NACM_2 + (M_Retest_NACM - M_NACM)
        PTS_NACM<- c(PTS_NACM_1, PTS_NACM_2)
        PTS_Hyperarousal_1<- Score_Hyperarousal_1 + (M_Retest_Hyperarousal - M_Hyperarousal)  
        PTS_Hyperarousal_2<- Score_Hyperarousal_2 + (M_Retest_Hyperarousal - M_Hyperarousal)
        PTS_Hyperarousal<- c(PTS_Hyperarousal_1, PTS_Hyperarousal_2)
        PTS_Avoidance_1<- Score_Avoidance_1 + (M_Retest_Avoidance - M_Avoidance)  
        PTS_Avoidance_2<- Score_Avoidance_2 + (M_Retest_Avoidance - M_Avoidance)
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS<- c(PTS_1,PTS_2)
        PTS_Intrusion_1<- Score_Intrusion_1
        PTS_Intrusion_2<- Score_Intrusion_2
        PTS_Intrusion<- c(PTS_Intrusion_1, PTS_Intrusion_2)
        PTS_NACM_1<- Score_NACM_1
        PTS_NACM_2<- Score_NACM_2
        PTS_NACM<- c(PTS_NACM_1, PTS_NACM_2)
        PTS_Hyperarousal_1<- Score_Hyperarousal_1
        PTS_Hyperarousal_2<- Score_Hyperarousal_2
        PTS_Hyperarousal<- c(PTS_Hyperarousal_1, PTS_Hyperarousal_2)
        PTS_Avoidance_1<- Score_Avoidance_1
        PTS_Avoidance_2<- Score_Avoidance_2
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2)
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Intrusion<- Rel_Intrusion * (SD_Retest_Intrusion/SD_Intrusion)
        A_Constant_Intrusion<- M_Retest_Intrusion - (B_Slope_Intrusion * M_Intrusion)
        B_Adj_Intrusion<- SD_Retest_Intrusion/SD_Intrusion
        A_Adj_Intrusion<- M_Retest_Intrusion - (B_Adj_Intrusion * M_Intrusion)
        PTS_Intrusion_1<- (B_Adj_Intrusion * Score_Intrusion_1) + A_Adj_Intrusion
        PTS_Intrusion_2<- (B_Adj_Intrusion * Score_Intrusion_2) + A_Adj_Intrusion
        PTS_Intrusion<- c(PTS_Intrusion_1,PTS_Intrusion_2)
        B_Slope_NACM<- Rel_NACM * (SD_Retest_NACM/SD_NACM)
        A_Constant_NACM<- M_Retest_NACM - (B_Slope_NACM * M_NACM)
        B_Adj_NACM<- SD_Retest_NACM/SD_NACM
        A_Adj_NACM<- M_Retest_NACM - (B_Adj_NACM * M_NACM)
        PTS_NACM_1<- (B_Adj_NACM * Score_NACM_1) + A_Adj_NACM
        PTS_NACM_2<- (B_Adj_NACM * Score_NACM_2) + A_Adj_NACM
        PTS_NACM<- c(PTS_NACM_1,PTS_NACM_2)
        B_Slope_Hyperarousal<- Rel_Hyperarousal * (SD_Retest_Hyperarousal/SD_Hyperarousal)
        A_Constant_Hyperarousal<- M_Retest_Hyperarousal - (B_Slope_Hyperarousal * M_Hyperarousal)
        B_Adj_Hyperarousal<- SD_Retest_Hyperarousal/SD_Hyperarousal
        A_Adj_Hyperarousal<- M_Retest_Hyperarousal - (B_Adj_Hyperarousal * M_Hyperarousal)
        PTS_Hyperarousal_1<- (B_Adj_Hyperarousal * Score_Hyperarousal_1) + A_Adj_Hyperarousal
        PTS_Hyperarousal_2<- (B_Adj_Hyperarousal * Score_Hyperarousal_2) + A_Adj_Hyperarousal
        PTS_Hyperarousal<- c(PTS_Hyperarousal_1,PTS_Hyperarousal_2)
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
        B_Slope_Intrusion<- Rel_Intrusion * (SD_Retest_Intrusion/SD_Intrusion)
        PTS_Intrusion_1<- B_Slope_Intrusion * Score_Intrusion_1
        PTS_Intrusion_2<- B_Slope_Intrusion * Score_Intrusion_2
        PTS_Intrusion<- c(PTS_Intrusion_1, PTS_Intrusion_2)
        B_Slope_NACM<- Rel_NACM * (SD_Retest_NACM/SD_NACM)
        PTS_NACM_1<- B_Slope_NACM * Score_NACM_1
        PTS_NACM_2<- B_Slope_NACM * Score_NACM_2
        PTS_NACM<- c(PTS_NACM_1, PTS_NACM_2)
        B_Slope_Hyperarousal<- Rel_Hyperarousal * (SD_Retest_Hyperarousal/SD_Hyperarousal)
        PTS_Hyperarousal_1<- B_Slope_Hyperarousal * Score_Hyperarousal_1
        PTS_Hyperarousal_2<- B_Slope_Hyperarousal * Score_Hyperarousal_2
        PTS_Hyperarousal<- c(PTS_Hyperarousal_1, PTS_Hyperarousal_2)
        B_Slope_Avoidance<- Rel_Avoidance * (SD_Retest_Avoidance/SD_Avoidance)
        PTS_Avoidance_1<- B_Slope_Avoidance * Score_Avoidance_1
        PTS_Avoidance_2<- B_Slope_Avoidance * Score_Avoidance_2
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2)
        PTS_Intrusion_1<- Score_Intrusion_1 + (M_Retest_Intrusion - M_Intrusion)
        PTS_Intrusion_2<- Score_Intrusion_2 + (M_Retest_Intrusion - M_Intrusion)
        PTS_Intrusion<- c(PTS_Intrusion_1, PTS_Intrusion_2)
        PTS_NACM_1<- Score_NACM_1 + (M_Retest_NACM - M_NACM)
        PTS_NACM_2<- Score_NACM_2 + (M_Retest_NACM - M_NACM)
        PTS_NACM<- c(PTS_NACM_1, PTS_NACM_2)
        PTS_Hyperarousal_1<- Score_Hyperarousal_1 + (M_Retest_Hyperarousal - M_Hyperarousal)
        PTS_Hyperarousal_2<- Score_Hyperarousal_2 + (M_Retest_Hyperarousal - M_Hyperarousal)
        PTS_Hyperarousal<- c(PTS_Hyperarousal_1, PTS_Hyperarousal_2)
        PTS_Avoidance_1<- Score_Avoidance_1 + (M_Retest_Avoidance - M_Avoidance)
        PTS_Avoidance_2<- Score_Avoidance_2 + (M_Retest_Avoidance - M_Avoidance)
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Intrusion<- round(PTS_Intrusion, digits = 2)
      PTS_NACM<- round(PTS_NACM, digits = 2)
      PTS_Hyperarousal<- round(PTS_Hyperarousal, digits = 2)
      PTS_Avoidance<- round(PTS_Avoidance, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2)
        SE_Intrusion_1<- McSweeny_SE_Intrusion*sqrt(1 + (1/SampleN) + ((Score_Intrusion_1 - M_Intrusion)^2/(SD_Intrusion^2*(SampleN-1))))
        SE_Intrusion_2<- McSweeny_SE_Intrusion*sqrt(1 + (1/SampleN) + ((Score_Intrusion_2 - M_Intrusion)^2/(SD_Intrusion^2*(SampleN-1))))
        SE_Intrusion<- c(SE_Intrusion_1, SE_Intrusion_2)
        SE_NACM_1<- McSweeny_SE_NACM*sqrt(1 + (1/SampleN) + ((Score_NACM_1 - M_NACM)^2/(SD_NACM^2*(SampleN-1))))
        SE_NACM_2<- McSweeny_SE_NACM*sqrt(1 + (1/SampleN) + ((Score_NACM_2 - M_NACM)^2/(SD_NACM^2*(SampleN-1))))
        SE_NACM<-c(SE_NACM_1, SE_NACM_2)
        SE_Hyperarousal_1<- McSweeny_SE_Hyperarousal*sqrt(1 + (1/SampleN) + ((Score_Hyperarousal_1 - M_Hyperarousal)^2/(SD_Hyperarousal^2*(SampleN-1))))
        SE_Hyperarousal_2<- McSweeny_SE_Hyperarousal*sqrt(1 + (1/SampleN) + ((Score_Hyperarousal_2 - M_Hyperarousal)^2/(SD_Hyperarousal^2*(SampleN-1))))
        SE_Hyperarousal<- c(SE_Hyperarousal_1, SE_Hyperarousal_2)
        SE_Avoidance_1<- McSweeny_SE_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Avoidance_1 - M_Avoidance)^2/(SD_Avoidance^2*(SampleN-1))))
        SE_Avoidance_2<- McSweeny_SE_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Avoidance_2 - M_Avoidance)^2/(SD_Avoidance^2*(SampleN-1))))
        SE_Avoidance<- c(SE_Avoidance_1, SE_Avoidance_2)
        SE<- round(SE, digits = 2)
        SE_Intrusion<- round(SE_Intrusion, digits = 2)
        SE_NACM<- round(SE_NACM, digits = 2)
        SE_Hyperarousal<- round(SE_Hyperarousal, digits = 2)
        SE_Avoidance<- round(SE_Avoidance, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2))
        CI<- round(CI, digits = 2)
        CI_Intrusion<- c((Conf*SE_Intrusion_1), (Conf*SE_Intrusion_2))
        CI_Intrusion<- round(CI_Intrusion, digits = 2)
        CI_NACM<- c((Conf*SE_NACM_1), (Conf*SE_NACM_2))
        CI_NACM<- round(CI_NACM, digits = 2)
        CI_Hyperarousal<- c((Conf*SE_Hyperarousal_1), (Conf*SE_Hyperarousal_2))
        CI_Hyperarousal<- round(CI_Hyperarousal, digits = 2)
        CI_Avoidance<- c((Conf*SE_Avoidance_1), (Conf*SE_Avoidance_2))
        CI_Avoidance<- round(CI_Avoidance, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Intrusion<- c((Conf*SE_Intrusion), (Conf*SE_Intrusion))
        CI_Intrusion<- round(CI_Intrusion, digits = 2)
        CI_NACM<- c((Conf*SE_NACM), (Conf*SE_NACM))
        CI_NACM<- round(CI_NACM, digits = 2)
        CI_Hyperarousal<- c((Conf*SE_Hyperarousal), (Conf*SE_Hyperarousal))
        CI_Hyperarousal<- round(CI_Hyperarousal, digits = 2)
        CI_Avoidance<- c((Conf*SE_Avoidance), (Conf*SE_Avoidance))
        CI_Avoidance<- round(CI_Avoidance, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Intrusion<- PTS_Intrusion + CI_Intrusion
      CI_Upper_Lim_Intrusion<- round(CI_Upper_Lim_Intrusion, digits = 2)
      CI_Lower_Lim_Intrusion<-PTS_Intrusion - CI_Intrusion
      CI_Lower_Lim_Intrusion<- round(CI_Lower_Lim_Intrusion, digits = 2)
      CI_Upper_Lim_NACM<- PTS_NACM + CI_NACM
      CI_Upper_Lim_NACM<- round(CI_Upper_Lim_NACM, digits = 2)
      CI_Lower_Lim_NACM<-PTS_NACM - CI_NACM
      CI_Lower_Lim_NACM<- round(CI_Lower_Lim_NACM, digits = 2)
      CI_Upper_Lim_Hyperarousal<- PTS_Hyperarousal + CI_Hyperarousal
      CI_Upper_Lim_Hyperarousal<- round(CI_Upper_Lim_Hyperarousal, digits = 2)
      CI_Lower_Lim_Hyperarousal<-PTS_Hyperarousal - CI_Hyperarousal
      CI_Lower_Lim_Hyperarousal<- round(CI_Lower_Lim_Hyperarousal, digits = 2)
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
      if(input$Select_CI_Intrusion == "2") {
        CI_Intrusion<- input$Man_CI_Intrusion
        CI_Intrusion<- c(CI_Intrusion, CI_Intrusion)
        CI_Intrusion<- round(CI_Intrusion, digits = 2)
        CI_Upper_Lim_Intrusion<- Score_Intrusion + CI_Intrusion
        CI_Upper_Lim_Intrusion<- round(CI_Upper_Lim_Intrusion, digits = 2)
        CI_Lower_Lim_Intrusion<- Score_Intrusion - CI_Intrusion
        CI_Lower_Lim_Intrusion<- round(CI_Lower_Lim_Intrusion, digits = 2)
      }
      if(input$Select_CI_NACM == "2") {
        CI_NACM<- input$Man_CI_NACM
        CI_NACM<- c(CI_NACM, CI_NACM)
        CI_NACM<- round(CI_NACM, digits = 2)
        CI_Upper_Lim_NACM<- Score_NACM + CI_NACM
        CI_Upper_Lim_NACM<- round(CI_Upper_Lim_NACM, digits = 2)
        CI_Lower_Lim_NACM<- Score_NACM - CI_NACM
        CI_Lower_Lim_NACM<- round(CI_Lower_Lim_NACM, digits = 2)
      } 
      if(input$Select_CI_Hyperarousal == "2") {
        CI_Hyperarousal<- input$Man_CI_Hyperarousal
        CI_Hyperarousal<- c(CI_Hyperarousal, CI_Hyperarousal)
        CI_Hyperarousal<- round(CI_Hyperarousal, digits = 2)
        CI_Upper_Lim_Hyperarousal<- Score_Hyperarousal + CI_Hyperarousal
        CI_Upper_Lim_Hyperarousal<- round(CI_Upper_Lim_Hyperarousal, digits = 2)
        CI_Lower_Lim_Hyperarousal<- Score_Hyperarousal - CI_Hyperarousal
        CI_Lower_Lim_Hyperarousal<- round(CI_Lower_Lim_Hyperarousal, digits = 2)
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
      Cutoff_Score_1<- rep(Cutoff_Score_1, 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_2<- rep(Cutoff_Score_2, 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_3<- rep(Cutoff_Score_3, 2)
      Cutoff_Score_Intrusion_1<- round(input$Cutoff_Intrusion_1, digits = 2)
      Cutoff_Score_Intrusion_1<- rep(Cutoff_Score_Intrusion_1, 2)
      Cutoff_Score_Intrusion_2<- round(input$Cutoff_Intrusion_2, digits = 2)
      Cutoff_Score_Intrusion_2<- rep(Cutoff_Score_Intrusion_2, 2)
      Cutoff_Score_Intrusion_3<- round(input$Cutoff_Intrusion_3, digits = 2)
      Cutoff_Score_Intrusion_3<- rep(Cutoff_Score_Intrusion_3, 2)
      Cutoff_Score_NACM_1<- round(input$Cutoff_NACM_1, digits = 2)
      Cutoff_Score_NACM_1<- rep(Cutoff_Score_NACM_1, 2)
      Cutoff_Score_NACM_2<- round(input$Cutoff_NACM_2, digits = 2)
      Cutoff_Score_NACM_2<- rep(Cutoff_Score_NACM_2, 2)
      Cutoff_Score_NACM_3<- round(input$Cutoff_NACM_3, digits = 2)
      Cutoff_Score_NACM_3<- rep(Cutoff_Score_NACM_3, 2)
      Cutoff_Score_Hyperarousal_1<- round(input$Cutoff_Hyperarousal_1, digits = 2)
      Cutoff_Score_Hyperarousal_1<- rep(Cutoff_Score_Hyperarousal_1, 2)
      Cutoff_Score_Hyperarousal_2<- round(input$Cutoff_Hyperarousal_2, digits = 2)
      Cutoff_Score_Hyperarousal_2<- rep(Cutoff_Score_Hyperarousal_2, 2)
      Cutoff_Score_Hyperarousal_3<- round(input$Cutoff_Hyperarousal_3, digits = 2)
      Cutoff_Score_Hyperarousal_3<- rep(Cutoff_Score_Hyperarousal_3, 2)
      Cutoff_Score_Avoidance_1<- round(input$Cutoff_Avoidance_1, digits = 2)
      Cutoff_Score_Avoidance_1<- rep(Cutoff_Score_Avoidance_1, 2)
      Cutoff_Score_Avoidance_2<- round(input$Cutoff_Avoidance_2, digits = 2)
      Cutoff_Score_Avoidance_2<- rep(Cutoff_Score_Avoidance_2, 2)
      Cutoff_Score_Avoidance_3<- round(input$Cutoff_Avoidance_3, digits = 2)
      Cutoff_Score_Avoidance_3<- rep(Cutoff_Score_Avoidance_3, 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Intrusion,Change_Intrusion,PTS_Intrusion, SE_Intrusion, CI_Upper_Lim_Intrusion, CI_Lower_Lim_Intrusion, Cutoff_Score_Intrusion_1,Cutoff_Score_Intrusion_2,Cutoff_Score_Intrusion_3,
                                      Score_NACM,Change_NACM, PTS_NACM, SE_NACM, CI_Upper_Lim_NACM, CI_Lower_Lim_NACM, Cutoff_Score_NACM_1,Cutoff_Score_NACM_2,Cutoff_Score_NACM_3, Score_Hyperarousal,Change_Hyperarousal,PTS_Hyperarousal, 
                                      SE_Hyperarousal, CI_Upper_Lim_Hyperarousal, CI_Lower_Lim_Hyperarousal, Cutoff_Score_Hyperarousal_1,Cutoff_Score_Hyperarousal_2,Cutoff_Score_Hyperarousal_3, 
                                      Score_Avoidance,Change_Avoidance,PTS_Avoidance, SE_Avoidance, CI_Upper_Lim_Avoidance, CI_Lower_Lim_Avoidance, Cutoff_Score_Avoidance_1,Cutoff_Score_Avoidance_2,Cutoff_Score_Avoidance_3)
    } else if(input$Timepoint == "3") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date_3<- input$Date_3
      Date<- c(Date_1, Date_2, Date_3)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<- as.numeric(unlist(strsplit(input$Text_1,",")))
      Score_1<- sum(Score_1a, na.rm = TRUE)
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Score_2<- sum(Score_2a, na.rm = TRUE)
      Score_3a<<- as.numeric(unlist(strsplit(input$Text_3,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_3a), Score = Score_3a)
      Score_3<- sum(Score_3a, na.rm = TRUE)
      Score<- c(Score_1, Score_2, Score_3)
      Score<- round(Score, digits = 2)
      Score_Intrusion_1<- sum(Score_1a[c(1,2,3,4,5)], na.rm = TRUE)
      Score_Intrusion_2<- sum(Score_2a[c(1,2,3,4,5)], na.rm = TRUE)
      Score_Intrusion_3<- sum(Score_3a[c(1,2,3,4,5)], na.rm = TRUE)
      Score_Intrusion<- c(Score_Intrusion_1,Score_Intrusion_2,Score_Intrusion_3)
      Score_Intrusion<- round(Score_Intrusion, digits = 2)
      Score_NACM_1<- sum(Score_1a[c(8,9,10,11,12,13,14)], na.rm = TRUE)
      Score_NACM_2<- sum(Score_2a[c(8,9,10,11,12,13,14)], na.rm = TRUE)
      Score_NACM_3<- sum(Score_3a[c(8,9,10,11,12,13,14)], na.rm = TRUE)
      Score_NACM<- c(Score_NACM_1,Score_NACM_2, Score_NACM_3)
      Score_NACM<- round(Score_NACM, digits = 2)
      Score_Hyperarousal_1<- sum(Score_1a[c(15,16,17,18,19,20)], na.rm = TRUE)
      Score_Hyperarousal_2<- sum(Score_2a[c(15,16,17,18,19,20)], na.rm = TRUE)
      Score_Hyperarousal_3<- sum(Score_3a[c(15,16,17,18,19,20)], na.rm = TRUE)
      Score_Hyperarousal<- c(Score_Hyperarousal_1,Score_Hyperarousal_2,Score_Hyperarousal_3)
      Score_Hyperarousal<- round(Score_Hyperarousal, digits = 2)
      Score_Avoidance_1<- sum(Score_1a[c(6,7)], na.rm = TRUE)
      Score_Avoidance_2<- sum(Score_2a[c(6,7)], na.rm = TRUE)
      Score_Avoidance_3<- sum(Score_3a[c(6,7)], na.rm = TRUE)
      Score_Avoidance<- c(Score_Avoidance_1,Score_Avoidance_2,Score_Avoidance_3)
      Score_Avoidance<- round(Score_Avoidance, digits = 2)
      Change<- c(0, (Score_2 - Score_1), (Score_3 - Score_2))
      Change<- round(Change, digits = 2)
      Change_Intrusion<- c(0, Score_Intrusion_2 - Score_Intrusion_1, Score_Intrusion_3 - Score_Intrusion_2)
      Change_Intrusion<- round(Change_Intrusion, digits = 2)
      Change_NACM<- c(0, Score_NACM_2 - Score_NACM_1, Score_NACM_3 - Score_NACM_2)
      Change_NACM<- round(Change_NACM, digits = 2)
      Change_Hyperarousal<- c(0, Score_Hyperarousal_2 - Score_Hyperarousal_1, Score_Hyperarousal_3 - Score_Hyperarousal_2)
      Change_Hyperarousal<- round(Change_Hyperarousal, digits = 2)
      Change_Avoidance<- c(0, Score_Avoidance_2 - Score_Avoidance_1, Score_Avoidance_3 - Score_Avoidance_2)
      Change_Avoidance<- round(Change_Avoidance, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS_3<- (Rel * Score_3) + (M * (1 - Rel))
        PTS<<- c(PTS_1, PTS_2, PTS_3)
        PTS_Intrusion_1<- (Rel_Intrusion * Score_Intrusion_1) + (M_Intrusion * (1 - Rel_Intrusion))
        PTS_Intrusion_2<- (Rel_Intrusion * Score_Intrusion_2) + (M_Intrusion * (1 - Rel_Intrusion))
        PTS_Intrusion_3<- (Rel_Intrusion * Score_Intrusion_3) + (M_Intrusion * (1 - Rel_Intrusion))
        PTS_Intrusion<<- c(PTS_Intrusion_1, PTS_Intrusion_2, PTS_Intrusion_3)
        PTS_NACM_1<- (Rel_NACM * Score_NACM_1) + (M_NACM * (1 - Rel_NACM))
        PTS_NACM_2<- (Rel_NACM * Score_NACM_2) + (M_NACM * (1 - Rel_NACM))
        PTS_NACM_3<- (Rel_NACM * Score_NACM_3) + (M_NACM * (1 - Rel_NACM))
        PTS_NACM<- c(PTS_NACM_1,PTS_NACM_2, PTS_NACM_3)
        PTS_Hyperarousal_1<- (Rel_Hyperarousal * Score_Hyperarousal_1) + (M_Hyperarousal * (1 - Rel_Hyperarousal))
        PTS_Hyperarousal_2<- (Rel_Hyperarousal * Score_Hyperarousal_2) + (M_Hyperarousal * (1 - Rel_Hyperarousal))
        PTS_Hyperarousal_3<- (Rel_Hyperarousal * Score_Hyperarousal_3) + (M_Hyperarousal * (1 - Rel_Hyperarousal))
        PTS_Hyperarousal<<- c(PTS_Hyperarousal_1, PTS_Hyperarousal_2, PTS_Hyperarousal_3)
        PTS_Avoidance_1<- (Rel_Avoidance * Score_Avoidance_1) + (M_Avoidance * (1 - Rel_Avoidance))
        PTS_Avoidance_2<- (Rel_Avoidance * Score_Avoidance_2) + (M_Avoidance * (1 - Rel_Avoidance))
        PTS_Avoidance_3<- (Rel_Avoidance * Score_Avoidance_3) + (M_Avoidance * (1 - Rel_Avoidance))
        PTS_Avoidance<<- c(PTS_Avoidance_1, PTS_Avoidance_2, PTS_Avoidance_3)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Intrusion_1<- Score_Intrusion_1 + (M_Retest_Intrusion - M_Intrusion)  
        PTS_Intrusion_2<- Score_Intrusion_2 + (M_Retest_Intrusion - M_Intrusion)
        PTS_Intrusion_3<- Score_Intrusion_3 + (M_Retest_Intrusion - M_Intrusion)
        PTS_Intrusion<- c(PTS_Intrusion_1, PTS_Intrusion_2, PTS_Intrusion_3)
        PTS_NACM_1<- Score_NACM_1 + (M_Retest_NACM - M_NACM)  
        PTS_NACM_2<- Score_NACM_2 + (M_Retest_NACM - M_NACM)
        PTS_NACM_3<- Score_NACM_3 + (M_Retest_NACM - M_NACM)
        PTS_NACM<- c(PTS_NACM_1, PTS_NACM_2, PTS_NACM_3)
        PTS_Hyperarousal_1<- Score_Hyperarousal_1 + (M_Retest_Hyperarousal - M_Hyperarousal)  
        PTS_Hyperarousal_2<- Score_Hyperarousal_2 + (M_Retest_Hyperarousal - M_Hyperarousal)
        PTS_Hyperarousal_3<- Score_Hyperarousal_3 + (M_Retest_Hyperarousal - M_Hyperarousal)
        PTS_Hyperarousal<- c(PTS_Hyperarousal_1, PTS_Hyperarousal_2, PTS_Hyperarousal_3)
        PTS_Avoidance_1<- Score_Avoidance_1 + (M_Retest_Avoidance - M_Avoidance)  
        PTS_Avoidance_2<- Score_Avoidance_2 + (M_Retest_Avoidance - M_Avoidance)
        PTS_Avoidance_3<- Score_Avoidance_3 + (M_Retest_Avoidance - M_Avoidance)
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2, PTS_Avoidance_3)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS_3<- Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Intrusion_1<- Score_Intrusion_1
        PTS_Intrusion_2<- Score_Intrusion_2
        PTS_Intrusion_3<- Score_Intrusion_3
        PTS_Intrusion<- c(PTS_Intrusion_1, PTS_Intrusion_2, PTS_Intrusion_3)
        PTS_NACM_1<- Score_NACM_1
        PTS_NACM_2<- Score_NACM_2
        PTS_NACM_3<- Score_NACM_3
        PTS_NACM<- c(PTS_NACM_1, PTS_NACM_2, PTS_NACM_3)
        PTS_Hyperarousal_1<- Score_Hyperarousal_1
        PTS_Hyperarousal_2<- Score_Hyperarousal_2
        PTS_Hyperarousal_3<- Score_Hyperarousal_3
        PTS_Hyperarousal<- c(PTS_Hyperarousal_1, PTS_Hyperarousal_2, PTS_Hyperarousal_3)
        PTS_Avoidance_1<- Score_Avoidance_1
        PTS_Avoidance_2<- Score_Avoidance_2
        PTS_Avoidance_3<- Score_Avoidance_3
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2, PTS_Avoidance_3)
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS_3<- (B_Adj * Score_3) + A_Adj
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Intrusion<- Rel_Intrusion * (SD_Retest_Intrusion/SD_Intrusion)
        A_Constant_Intrusion<- M_Retest_Intrusion - (B_Slope_Intrusion * M_Intrusion)
        B_Adj_Intrusion<- SD_Retest_Intrusion/SD_Intrusion
        A_Adj_Intrusion<- M_Retest_Intrusion - (B_Adj_Intrusion * M_Intrusion)
        PTS_Intrusion_1<- (B_Adj_Intrusion * Score_Intrusion_1) + A_Adj_Intrusion
        PTS_Intrusion_2<- (B_Adj_Intrusion * Score_Intrusion_2) + A_Adj_Intrusion
        PTS_Intrusion_3<- (B_Adj_Intrusion * Score_Intrusion_3) + A_Adj_Intrusion
        PTS_Intrusion<- c(PTS_Intrusion_1, PTS_Intrusion_2, PTS_Intrusion_3)
        B_Slope_NACM<- Rel_NACM * (SD_Retest_NACM/SD_NACM)
        A_Constant_NACM<- M_Retest_NACM - (B_Slope_NACM * M_NACM)
        B_Adj_NACM<- SD_Retest_NACM/SD_NACM
        A_Adj_NACM<- M_Retest_NACM - (B_Adj_NACM * M_NACM)
        PTS_NACM_1<- (B_Adj_NACM * Score_NACM_1) + A_Adj_NACM
        PTS_NACM_2<- (B_Adj_NACM * Score_NACM_2) + A_Adj_NACM
        PTS_NACM_3<- (B_Adj_NACM * Score_NACM_3) + A_Adj_NACM
        PTS_NACM<- c(PTS_NACM_1, PTS_NACM_2, PTS_NACM_3)
        B_Slope_Hyperarousal<- Rel_Hyperarousal * (SD_Retest_Hyperarousal/SD_Hyperarousal)
        A_Constant_Hyperarousal<- M_Retest_Hyperarousal - (B_Slope_Hyperarousal * M_Hyperarousal)
        B_Adj_Hyperarousal<- SD_Retest_Hyperarousal/SD_Hyperarousal
        A_Adj_Hyperarousal<- M_Retest_Hyperarousal - (B_Adj_Hyperarousal * M_Hyperarousal)
        PTS_Hyperarousal_1<- (B_Adj_Hyperarousal * Score_Hyperarousal_1) + A_Adj_Hyperarousal
        PTS_Hyperarousal_2<- (B_Adj_Hyperarousal * Score_Hyperarousal_2) + A_Adj_Hyperarousal
        PTS_Hyperarousal_3<- (B_Adj_Hyperarousal * Score_Hyperarousal_3) + A_Adj_Hyperarousal
        PTS_Hyperarousal<- c(PTS_Hyperarousal_1, PTS_Hyperarousal_2, PTS_Hyperarousal_3)
        B_Slope_Avoidance<- Rel_Avoidance * (SD_Retest_Avoidance/SD_Avoidance)
        A_Constant_Avoidance<- M_Retest_Avoidance - (B_Slope_Avoidance * M_Avoidance)
        B_Adj_Avoidance<- SD_Retest_Avoidance/SD_Avoidance
        A_Adj_Avoidance<- M_Retest_Avoidance - (B_Adj_Avoidance * M_Avoidance)
        PTS_Avoidance_1<- (B_Adj_Avoidance * Score_Avoidance_1) + A_Adj_Avoidance
        PTS_Avoidance_2<- (B_Adj_Avoidance * Score_Avoidance_2) + A_Adj_Avoidance
        PTS_Avoidance_3<- (B_Adj_Avoidance * Score_Avoidance_3) + A_Adj_Avoidance
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2, PTS_Avoidance_3)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope * Score_1
        PTS_2<- B_Slope * Score_2
        PTS_3<- B_Slope * Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Intrusion<- Rel_Intrusion * (SD_Retest_Intrusion/SD_Intrusion)
        PTS_Intrusion_1<- B_Slope_Intrusion * Score_Intrusion_1
        PTS_Intrusion_2<- B_Slope_Intrusion * Score_Intrusion_2
        PTS_Intrusion_3<- B_Slope_Intrusion * Score_Intrusion_3
        PTS_Intrusion<- c(PTS_Intrusion_1, PTS_Intrusion_2, PTS_Intrusion_3)
        B_Slope_NACM<- Rel_NACM * (SD_Retest_NACM/SD_NACM)
        PTS_NACM_1<- B_Slope_NACM * Score_NACM_1
        PTS_NACM_2<- B_Slope_NACM * Score_NACM_2
        PTS_NACM_3<- B_Slope_NACM * Score_NACM_3
        PTS_NACM<- c(PTS_NACM_1, PTS_NACM_2, PTS_NACM_3)
        B_Slope_Hyperarousal<- Rel_Hyperarousal * (SD_Retest_Hyperarousal/SD_Hyperarousal)
        PTS_Hyperarousal_1<- B_Slope_Hyperarousal * Score_Hyperarousal_1
        PTS_Hyperarousal_2<- B_Slope_Hyperarousal * Score_Hyperarousal_2
        PTS_Hyperarousal_3<- B_Slope_Hyperarousal * Score_Hyperarousal_3
        PTS_Hyperarousal<- c(PTS_Hyperarousal_1, PTS_Hyperarousal_2, PTS_Hyperarousal_3)
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
        PTS_Intrusion_1<- Score_Intrusion_1 + (M_Retest_Intrusion - M_Intrusion)
        PTS_Intrusion_2<- Score_Intrusion_2 + (M_Retest_Intrusion - M_Intrusion)
        PTS_Intrusion_3<- Score_Intrusion_3 + (M_Retest_Intrusion - M_Intrusion)
        PTS_Intrusion<- c(PTS_Intrusion_1, PTS_Intrusion_2, PTS_Intrusion_3)
        PTS_NACM_1<- Score_NACM_1 + (M_Retest_NACM - M_NACM)
        PTS_NACM_2<- Score_NACM_2 + (M_Retest_NACM - M_NACM)
        PTS_NACM_3<- Score_NACM_3 + (M_Retest_NACM - M_NACM)
        PTS_NACM<- c(PTS_NACM_1, PTS_NACM_2, PTS_NACM_3)
        PTS_Hyperarousal_1<- Score_Hyperarousal_1 + (M_Retest_Hyperarousal - M_Hyperarousal)
        PTS_Hyperarousal_2<- Score_Hyperarousal_2 + (M_Retest_Hyperarousal - M_Hyperarousal)
        PTS_Hyperarousal_3<- Score_Hyperarousal_3 + (M_Retest_Hyperarousal - M_Hyperarousal)
        PTS_Hyperarousal<- c(PTS_Hyperarousal_1, PTS_Hyperarousal_2, PTS_Hyperarousal_3)
        PTS_Avoidance_1<- Score_Avoidance_1 + (M_Retest_Avoidance - M_Avoidance)
        PTS_Avoidance_2<- Score_Avoidance_2 + (M_Retest_Avoidance - M_Avoidance)
        PTS_Avoidance_3<- Score_Avoidance_3 + (M_Retest_Avoidance - M_Avoidance)
        PTS_Avoidance<- c(PTS_Avoidance_1, PTS_Avoidance_2, PTS_Avoidance_3)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Intrusion<- round(PTS_Intrusion, digits = 2)
      PTS_NACM<- round(PTS_NACM, digits = 2)
      PTS_Hyperarousal<- round(PTS_Hyperarousal, digits = 2)
      PTS_Avoidance<- round(PTS_Avoidance, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE3<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_3 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2, SE3)
        SE_Intrusion_1<- McSweeny_SE_Intrusion*sqrt(1 + (1/SampleN) + ((Score_Intrusion_1 - M_Intrusion)^2/(SD_Intrusion^2*(SampleN-1))))
        SE_Intrusion_2<- McSweeny_SE_Intrusion*sqrt(1 + (1/SampleN) + ((Score_Intrusion_2 - M_Intrusion)^2/(SD_Intrusion^2*(SampleN-1))))
        SE_Intrusion_3<- McSweeny_SE_Intrusion*sqrt(1 + (1/SampleN) + ((Score_Intrusion_3 - M_Intrusion)^2/(SD_Intrusion^2*(SampleN-1))))
        SE_Intrusion<- c(SE_Intrusion_1, SE_Intrusion_2, SE_Intrusion_3)
        SE_NACM_1<- McSweeny_SE_NACM*sqrt(1 + (1/SampleN) + ((Score_NACM_1 - M_NACM)^2/(SD_NACM^2*(SampleN-1))))
        SE_NACM_2<- McSweeny_SE_NACM*sqrt(1 + (1/SampleN) + ((Score_NACM_2 - M_NACM)^2/(SD_NACM^2*(SampleN-1))))
        SE_NACM_3<- McSweeny_SE_NACM*sqrt(1 + (1/SampleN) + ((Score_NACM_3 - M_NACM)^2/(SD_NACM^2*(SampleN-1))))
        SE_NACM<- c(SE_NACM_1, SE_NACM_2, SE_NACM_3)
        SE_Hyperarousal_1<- McSweeny_SE_Hyperarousal*sqrt(1 + (1/SampleN) + ((Score_Hyperarousal_1 - M_Hyperarousal)^2/(SD_Hyperarousal^2*(SampleN-1))))
        SE_Hyperarousal_2<- McSweeny_SE_Hyperarousal*sqrt(1 + (1/SampleN) + ((Score_Hyperarousal_2 - M_Hyperarousal)^2/(SD_Hyperarousal^2*(SampleN-1))))
        SE_Hyperarousal_3<- McSweeny_SE_Hyperarousal*sqrt(1 + (1/SampleN) + ((Score_Hyperarousal_3 - M_Hyperarousal)^2/(SD_Hyperarousal^2*(SampleN-1))))
        SE_Hyperarousal<- c(SE_Hyperarousal_1, SE_Hyperarousal_2, SE_Hyperarousal_3)
        SE_Avoidance_1<- McSweeny_SE_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Avoidance_1 - M_Avoidance)^2/(SD_Avoidance^2*(SampleN-1))))
        SE_Avoidance_2<- McSweeny_SE_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Avoidance_2 - M_Avoidance)^2/(SD_Avoidance^2*(SampleN-1))))
        SE_Avoidance_3<- McSweeny_SE_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Avoidance_3 - M_Avoidance)^2/(SD_Avoidance^2*(SampleN-1))))
        SE_Avoidance<- c(SE_Avoidance_1, SE_Avoidance_2, SE_Avoidance_3)
        SE<- round(SE, digits = 2)
        SE_Intrusion<- round(SE_Intrusion, digits = 2)
        SE_NACM<- round(SE_NACM, digits = 2)
        SE_Hyperarousal<- round(SE_Hyperarousal, digits = 2)
        SE_Avoidance<- round(SE_Avoidance, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2), (Conf*SE3))
        CI<- round(CI, digits = 2)
        CI_Intrusion<- c((Conf*SE_Intrusion_1), (Conf*SE_Intrusion_2), (Conf*SE_Intrusion_3))
        CI_Intrusion<- round(CI_Intrusion, digits = 2)
        CI_NACM<- c((Conf*SE_NACM_1), (Conf*SE_NACM_2), (Conf*SE_NACM_3))
        CI_NACM<- round(CI_NACM, digits = 2)
        CI_Hyperarousal<- c((Conf*SE_Hyperarousal_1), (Conf*SE_Hyperarousal_2), (Conf*SE_Hyperarousal_3))
        CI_Hyperarousal<- round(CI_Hyperarousal, digits = 2)
        CI_Avoidance<- c((Conf*SE_Avoidance_1), (Conf*SE_Avoidance_2), (Conf*SE_Avoidance_3))
        CI_Avoidance<- round(CI_Avoidance, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Intrusion<- c((Conf*SE_Intrusion), (Conf*SE_Intrusion), (Conf*SE_Intrusion))
        CI_Intrusion<- round(CI_Intrusion, digits = 2)
        CI_NACM<- c((Conf*SE_NACM), (Conf*SE_NACM), (Conf*SE_NACM))
        CI_NACM<- round(CI_NACM, digits = 2)
        CI_Hyperarousal<- c((Conf*SE_Hyperarousal), (Conf*SE_Hyperarousal), (Conf*SE_Hyperarousal))
        CI_Hyperarousal<- round(CI_Hyperarousal, digits = 2)
        CI_Avoidance<- c((Conf*SE_Avoidance), (Conf*SE_Avoidance), (Conf*SE_Avoidance))
        CI_Avoidance<- round(CI_Avoidance, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Intrusion<- PTS_Intrusion + CI_Intrusion
      CI_Upper_Lim_Intrusion<- round(CI_Upper_Lim_Intrusion, digits = 2)
      CI_Lower_Lim_Intrusion<-PTS_Intrusion - CI_Intrusion
      CI_Lower_Lim_Intrusion<- round(CI_Lower_Lim_Intrusion, digits = 2)
      CI_Upper_Lim_NACM<- PTS_NACM + CI_NACM
      CI_Upper_Lim_NACM<- round(CI_Upper_Lim_NACM, digits = 2)
      CI_Lower_Lim_NACM<-PTS_NACM - CI_NACM
      CI_Lower_Lim_NACM<- round(CI_Lower_Lim_NACM, digits = 2)
      CI_Upper_Lim_Hyperarousal<- PTS_Hyperarousal + CI_Hyperarousal
      CI_Upper_Lim_Hyperarousal<- round(CI_Upper_Lim_Hyperarousal, digits = 2)
      CI_Lower_Lim_Hyperarousal<-PTS_Hyperarousal - CI_Hyperarousal
      CI_Lower_Lim_Hyperarousal<- round(CI_Lower_Lim_Hyperarousal, digits = 2)
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
      if(input$Select_CI_Intrusion == "2") {
        CI_Intrusion<- input$Man_CI_Intrusion
        CI_Intrusion<- c(CI_Intrusion, CI_Intrusion, CI_Intrusion)
        CI_Intrusion<- round(CI_Intrusion, digits = 2)
        CI_Upper_Lim_Intrusion<- Score_Intrusion + CI_Intrusion
        CI_Upper_Lim_Intrusion<- round(CI_Upper_Lim_Intrusion, digits = 2)
        CI_Lower_Lim_Intrusion<- Score_Intrusion - CI_Intrusion
        CI_Lower_Lim_Intrusion<- round(CI_Lower_Lim_Intrusion, digits = 2)
      }
      if(input$Select_CI_NACM == "2") {
        CI_NACM<- input$Man_CI_NACM
        CI_NACM<- c(CI_NACM, CI_NACM, CI_NACM)
        CI_NACM<- round(CI_NACM, digits = 2)
        CI_Upper_Lim_NACM<- Score_NACM + CI_NACM
        CI_Upper_Lim_NACM<- round(CI_Upper_Lim_NACM, digits = 2)
        CI_Lower_Lim_NACM<- Score_NACM - CI_NACM
        CI_Lower_Lim_NACM<- round(CI_Lower_Lim_NACM, digits = 2)
      }
      if(input$Select_CI_Hyperarousal == "2") {
        CI_Hyperarousal<- input$Man_CI_Hyperarousal
        CI_Hyperarousal<- c(CI_Hyperarousal, CI_Hyperarousal, CI_Hyperarousal)
        CI_Hyperarousal<- round(CI_Hyperarousal, digits = 2)
        CI_Upper_Lim_Hyperarousal<- Score_Hyperarousal + CI_Hyperarousal
        CI_Upper_Lim_Hyperarousal<- round(CI_Upper_Lim_Hyperarousal, digits = 2)
        CI_Lower_Lim_Hyperarousal<- Score_Hyperarousal - CI_Hyperarousal
        CI_Lower_Lim_Hyperarousal<- round(CI_Lower_Lim_Hyperarousal, digits = 2)
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
      Cutoff_Score_1<- rep(Cutoff_Score_1, 3)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_2<- rep(Cutoff_Score_2, 3)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_3<- rep(Cutoff_Score_3, 3)
      Cutoff_Score_Intrusion_1<- round(input$Cutoff_Intrusion_1, digits = 2)
      Cutoff_Score_Intrusion_1<- rep(Cutoff_Score_Intrusion_1, 3)
      Cutoff_Score_Intrusion_2<- round(input$Cutoff_Intrusion_2, digits = 2)
      Cutoff_Score_Intrusion_2<- rep(Cutoff_Score_Intrusion_2, 3)
      Cutoff_Score_Intrusion_3<- round(input$Cutoff_Intrusion_3, digits = 2)
      Cutoff_Score_Intrusion_3<- rep(Cutoff_Score_Intrusion_3, 3)
      Cutoff_Score_NACM_1<- round(input$Cutoff_NACM_1, digits = 2)
      Cutoff_Score_NACM_1<- rep(Cutoff_Score_NACM_1, 3)
      Cutoff_Score_NACM_2<- round(input$Cutoff_NACM_2, digits = 2)
      Cutoff_Score_NACM_2<- rep(Cutoff_Score_NACM_2, 3)
      Cutoff_Score_NACM_3<- round(input$Cutoff_NACM_3, digits = 2)
      Cutoff_Score_NACM_3<- rep(Cutoff_Score_NACM_3, 3)
      Cutoff_Score_Hyperarousal_1<- round(input$Cutoff_Hyperarousal_1, digits = 2)
      Cutoff_Score_Hyperarousal_1<- rep(Cutoff_Score_Hyperarousal_1, 3)
      Cutoff_Score_Hyperarousal_2<- round(input$Cutoff_Hyperarousal_2, digits = 2)
      Cutoff_Score_Hyperarousal_2<- rep(Cutoff_Score_Hyperarousal_2, 3)
      Cutoff_Score_Hyperarousal_3<- round(input$Cutoff_Hyperarousal_3, digits = 2)
      Cutoff_Score_Hyperarousal_3<- rep(Cutoff_Score_Hyperarousal_3, 3)
      Cutoff_Score_Avoidance_1<- round(input$Cutoff_Avoidance_1, digits = 2)
      Cutoff_Score_Avoidance_1<- rep(Cutoff_Score_Avoidance_1, 3)
      Cutoff_Score_Avoidance_2<- round(input$Cutoff_Avoidance_2, digits = 2)
      Cutoff_Score_Avoidance_2<- rep(Cutoff_Score_Avoidance_2, 3)
      Cutoff_Score_Avoidance_3<- round(input$Cutoff_Avoidance_3, digits = 2)
      Cutoff_Score_Avoidance_3<- rep(Cutoff_Score_Avoidance_3, 3)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Intrusion,Change_Intrusion,PTS_Intrusion, SE_Intrusion, CI_Upper_Lim_Intrusion, CI_Lower_Lim_Intrusion, Cutoff_Score_Intrusion_1,Cutoff_Score_Intrusion_2,Cutoff_Score_Intrusion_3,
                                      Score_NACM,Change_NACM, PTS_NACM, SE_NACM, CI_Upper_Lim_NACM, CI_Lower_Lim_NACM, Cutoff_Score_NACM_1,Cutoff_Score_NACM_2,Cutoff_Score_NACM_3, Score_Hyperarousal,Change_Hyperarousal,PTS_Hyperarousal, 
                                      SE_Hyperarousal, CI_Upper_Lim_Hyperarousal, CI_Lower_Lim_Hyperarousal, Cutoff_Score_Hyperarousal_1,Cutoff_Score_Hyperarousal_2,Cutoff_Score_Hyperarousal_3, 
                                      Score_Avoidance,Change_Avoidance,PTS_Avoidance, SE_Avoidance, CI_Upper_Lim_Avoidance, CI_Lower_Lim_Avoidance, Cutoff_Score_Avoidance_1,Cutoff_Score_Avoidance_2,Cutoff_Score_Avoidance_3)
    }
    
  
    #Create dataframes to be displayed as tables in pdf report (statistical values used in reliable change analyses)
    
    if(input$RelChangeMethod == 'Nunnally & Bernstein (1994)') {
      Stats_Table_Fullscale<<- data.frame(Pop, M, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Intrusion<<- data.frame(Pop, M_Intrusion, SD_Intrusion, RelChangeMethod, Rel_Intrusion, ConfInt)
      names(Stats_Table_Intrusion)<<- c("Reference Population", "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_NACM<<- data.frame(Pop, M_NACM, SD_NACM, RelChangeMethod, Rel_NACM, ConfInt)
      names(Stats_Table_NACM)<<- c("Reference Population", "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Hyperarousal<<- data.frame(Pop, M_Hyperarousal, SD_Hyperarousal, RelChangeMethod, Rel_Hyperarousal, ConfInt)
      names(Stats_Table_Hyperarousal)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Avoidance<<- data.frame(Pop, M_Avoidance, SD_Avoidance, RelChangeMethod, Rel_Avoidance, ConfInt)
      names(Stats_Table_Avoidance)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      } else if (input$RelChangeMethod == 'Chelune et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop, M, M_Retest, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Intrusion<<- data.frame(Pop, M_Intrusion, M_Retest_Intrusion, SD_Intrusion, RelChangeMethod, Rel_Intrusion, ConfInt)
      names(Stats_Table_Intrusion)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_NACM<<- data.frame(Pop, M_NACM, M_Retest_NACM, SD_NACM, RelChangeMethod, Rel_NACM, ConfInt)
      names(Stats_Table_NACM)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Hyperarousal<<- data.frame(Pop, M_Hyperarousal, M_Retest_Hyperarousal, SD_Hyperarousal, RelChangeMethod, Rel_Hyperarousal, ConfInt)
      names(Stats_Table_Hyperarousal)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Avoidance<<- data.frame(Pop, M_Avoidance, M_Retest_Avoidance, SD_Avoidance, RelChangeMethod, Rel_Avoidance, ConfInt)
      names(Stats_Table_Avoidance)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Maassen et al. (2006)' | input$RelChangeMethod == 'McSweeny et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop, M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Intrusion<<- data.frame(Pop, M_Intrusion, M_Retest_Intrusion, SD_Intrusion, SD_Retest_Intrusion, RelChangeMethod, Rel_Intrusion, ConfInt)
      names(Stats_Table_Intrusion)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_NACM<<- data.frame(Pop, M_NACM, M_Retest_NACM, SD_NACM, SD_Retest_NACM, RelChangeMethod, Rel_NACM, ConfInt)
      names(Stats_Table_NACM)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Hyperarousal<<- data.frame(Pop,  M_Hyperarousal, M_Retest_Hyperarousal, SD_Hyperarousal, SD_Retest_Hyperarousal, RelChangeMethod, Rel_Hyperarousal, ConfInt)
      names(Stats_Table_Hyperarousal)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Avoidance<<- data.frame(Pop, M_Avoidance, M_Retest_Avoidance, SD_Avoidance, SD_Retest_Avoidance, RelChangeMethod, Rel_Avoidance, ConfInt)
      names(Stats_Table_Avoidance)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Crawford & Howell (1998)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, SampleN, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Intrusion<<- data.frame(Pop, M_Intrusion, M_Retest_Intrusion, SD_Intrusion, SD_Retest_Intrusion, RelChangeMethod, Rel_Intrusion, SampleN,ConfInt)
      names(Stats_Table_Intrusion)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_NACM<<- data.frame(Pop, M_NACM, M_Retest_NACM, SD_NACM, SD_Retest_NACM, RelChangeMethod, Rel_NACM, SampleN, ConfInt)
      names(Stats_Table_NACM)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Hyperarousal<<- data.frame(Pop, M_Hyperarousal, M_Retest_Hyperarousal, SD_Hyperarousal, SD_Retest_Hyperarousal, RelChangeMethod, Rel_Hyperarousal, SampleN,ConfInt)
      names(Stats_Table_Hyperarousal)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Avoidance<<- data.frame(Pop, M_Avoidance, M_Retest_Avoidance, SD_Avoidance, SD_Retest_Avoidance, RelChangeMethod, Rel_Avoidance, SampleN,ConfInt)
      names(Stats_Table_Avoidance)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)","Reliable Change Method", "Reliability", "N", "Confidence")
    } else if (input$RelChangeMethod == 'Jacobson & Truax (1991)' | input$RelChangeMethod == 'Speer (1992)') {
      Stats_Table_Fullscale<<- data.frame(Pop, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Intrusion<<- data.frame(Pop, SD_Intrusion, RelChangeMethod, Rel_Intrusion, ConfInt)
      names(Stats_Table_Intrusion)<<- c("Reference Population", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_NACM<<- data.frame(Pop, SD_NACM, RelChangeMethod, Rel_NACM, ConfInt)
      names(Stats_Table_NACM)<<- c("Reference Population", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Hyperarousal<<- data.frame(Pop, SD_Hyperarousal, RelChangeMethod, Rel_Hyperarousal, ConfInt)
      names(Stats_Table_Hyperarousal)<<- c("Reference Population", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Avoidance<<- data.frame(Pop, SD_Avoidance, RelChangeMethod, Rel_Avoidance, ConfInt)
      names(Stats_Table_Avoidance)<<- c("Reference Population", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    }
    
    #If custom confidence intervals options are selected, make sure that no values appear for PS and SE in pdf tables
    #Change the look of tables in report if custom confidence intervals options are selected
    
    if (input$Select_CI == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS = NA, SE = NA)
      Stats_Table_Fullscale<<- Stats_Table_Fullscale %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI[1])
    }
    if (input$Select_CI_Intrusion == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Intrusion = NA, SE_Intrusion = NA)
      Stats_Table_Intrusion<<- Stats_Table_Intrusion %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Intrusion[1])
    }
    if (input$Select_CI_NACM == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_NACM = NA, SE_NACM = NA)
      Stats_Table_NACM<<- Stats_Table_NACM %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                      "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_NACM[1])
    }
    if (input$Select_CI_Hyperarousal == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Hyperarousal = NA, SE_Hyperarousal = NA)
      Stats_Table_Hyperarousal<<- Stats_Table_Hyperarousal %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                      "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Hyperarousal[1])
    }
    if (input$Select_CI_Avoidance == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Avoidance = NA, SE_Avoidance = NA)
      Stats_Table_Avoidance<<- Stats_Table_Avoidance %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Avoidance[1])
    }
    
    #Create a dataframe to be used in the analytics spreadsheet.
    
    #Calculate variables for significant improvement
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$CI_Lower_Lim[1]) {
      PCL5.Fullscale.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$CI_Lower_Lim[1]) {
      PCL5.Fullscale.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$CI_Upper_Lim[1]) {
      PCL5.Fullscale.Sig.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$CI_Upper_Lim[1]) {
      PCL5.Fullscale.Sig.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Intrusion[length(Entered_Scores_Df$Score_Intrusion)] < Entered_Scores_Df$CI_Lower_Lim_Intrusion[1]) {
      PCL5.Intrusion.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Intrusion[length(Entered_Scores_Df$Score_Intrusion)] >= Entered_Scores_Df$CI_Lower_Lim_Intrusion[1]) {
      PCL5.Intrusion.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Intrusion[length(Entered_Scores_Df$Score_Intrusion)] > Entered_Scores_Df$CI_Upper_Lim_Intrusion[1]) {
      PCL5.Intrusion.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Intrusion[length(Entered_Scores_Df$Score_Intrusion)] <= Entered_Scores_Df$CI_Upper_Lim_Intrusion[1]) {
      PCL5.Intrusion.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_NACM[length(Entered_Scores_Df$Score_NACM)] < Entered_Scores_Df$CI_Lower_Lim_NACM[1]) {
      PCL5.NACM.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_NACM[length(Entered_Scores_Df$Score_NACM)] >= Entered_Scores_Df$CI_Lower_Lim_NACM[1]) {
      PCL5.NACM.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_NACM[length(Entered_Scores_Df$Score_NACM)] > Entered_Scores_Df$CI_Upper_Lim_NACM[1]) {
      PCL5.NACM.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_NACM[length(Entered_Scores_Df$Score_NACM)] <= Entered_Scores_Df$CI_Upper_Lim_NACM[1]) {
      PCL5.NACM.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Hyperarousal[length(Entered_Scores_Df$Score_Hyperarousal)] < Entered_Scores_Df$CI_Lower_Lim_Hyperarousal[1]) {
      PCL5.Hyperarousal.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Hyperarousal[length(Entered_Scores_Df$Score_Hyperarousal)] >= Entered_Scores_Df$CI_Lower_Lim_Hyperarousal[1]) {
      PCL5.Hyperarousal.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Hyperarousal[length(Entered_Scores_Df$Score_Hyperarousal)] > Entered_Scores_Df$CI_Upper_Lim_Hyperarousal[1]) {
      PCL5.Hyperarousal.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Hyperarousal[length(Entered_Scores_Df$Score_Hyperarousal)] <= Entered_Scores_Df$CI_Upper_Lim_Hyperarousal[1]) {
      PCL5.Hyperarousal.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] < Entered_Scores_Df$CI_Lower_Lim_Avoidance[1]) {
      PCL5.Avoidance.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] >= Entered_Scores_Df$CI_Lower_Lim_Avoidance[1]) {
      PCL5.Avoidance.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] > Entered_Scores_Df$CI_Upper_Lim_Avoidance[1]) {
      PCL5.Avoidance.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] <= Entered_Scores_Df$CI_Upper_Lim_Avoidance[1]) {
      PCL5.Avoidance.Sig.Deterioration<- "No"
    }
    
    
    #Calculate variables for improvement
    
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$Score[1]) {
      PCL5.Fullscale.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$Score[1]) {
      PCL5.Fullscale.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$Score[1]) {
      PCL5.Fullscale.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$Score[1]) {
      PCL5.Fullscale.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Intrusion[length(Entered_Scores_Df$Score_Intrusion)] < Entered_Scores_Df$Score_Intrusion[1]) {
      PCL5.Intrusion.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Intrusion[length(Entered_Scores_Df$Score_Intrusion)] >= Entered_Scores_Df$Score_Intrusion[1]) {
      PCL5.Intrusion.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Intrusion[length(Entered_Scores_Df$Score_Intrusion)] > Entered_Scores_Df$Score_Intrusion[1]) {
      PCL5.Intrusion.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Intrusion[length(Entered_Scores_Df$Score_Intrusion)] <= Entered_Scores_Df$Score_Intrusion[1]) {
      PCL5.Intrusion.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_NACM[length(Entered_Scores_Df$Score_NACM)] < Entered_Scores_Df$Score_NACM[1]) {
      PCL5.NACM.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_NACM[length(Entered_Scores_Df$Score_NACM)] >= Entered_Scores_Df$Score_NACM[1]) {
      PCL5.NACM.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_NACM[length(Entered_Scores_Df$Score_NACM)] > Entered_Scores_Df$Score_NACM[1]) {
      PCL5.NACM.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_NACM[length(Entered_Scores_Df$Score_NACM)] <= Entered_Scores_Df$Score_NACM[1]) {
      PCL5.NACM.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Hyperarousal[length(Entered_Scores_Df$Score_Hyperarousal)] < Entered_Scores_Df$Score_Hyperarousal[1]) {
      PCL5.Hyperarousal.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Hyperarousal[length(Entered_Scores_Df$Score_Hyperarousal)] >= Entered_Scores_Df$Score_Hyperarousal[1]) {
      PCL5.Hyperarousal.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Hyperarousal[length(Entered_Scores_Df$Score_Hyperarousal)] > Entered_Scores_Df$Score_Hyperarousal[1]) {
      PCL5.Hyperarousal.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Hyperarousal[length(Entered_Scores_Df$Score_Hyperarousal)] <= Entered_Scores_Df$Score_Hyperarousal[1]) {
      PCL5.Hyperarousal.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] < Entered_Scores_Df$Score_Avoidance[1]) {
      PCL5.Avoidance.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] >= Entered_Scores_Df$Score_Avoidance[1]) {
      PCL5.Avoidance.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] > Entered_Scores_Df$Score_Avoidance[1]) {
      PCL5.Avoidance.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] <= Entered_Scores_Df$Score_Avoidance[1]) {
      PCL5.Avoidance.Deterioration<- "No"
    }
    
    
    
    PCL5.Fullscale.Change<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] - Entered_Scores_Df$Score[1]
    PCL5.Intrusion.Change<- Entered_Scores_Df$Score_Intrusion[length(Entered_Scores_Df$Score_Intrusion)] - Entered_Scores_Df$Score_Intrusion[1]
    PCL5.NACM.Change<- Entered_Scores_Df$Score_NACM[length(Entered_Scores_Df$Score_NACM)] - Entered_Scores_Df$Score_NACM[1]
    PCL5.Hyperarousal.Change<- Entered_Scores_Df$Score_Hyperarousal[length(Entered_Scores_Df$Score_Hyperarousal)] - Entered_Scores_Df$Score_Hyperarousal[1]
    PCL5.Avoidance.Change<- Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)] - Entered_Scores_Df$Score_Avoidance[1]
    PCL5.Fullscale.Comparisons<- length(Entered_Scores_Df$Change) - 1
    PCL5.Intrusion.Comparisons<- length(Entered_Scores_Df$Change_Intrusion) - 1
    PCL5.NACM.Comparisons<- length(Entered_Scores_Df$Change_NACM) - 1
    PCL5.Hyperarousal.Comparisons<- length(Entered_Scores_Df$Change_Hyperarousal) - 1
    PCL5.Avoidance.Comparisons<- length(Entered_Scores_Df$Change_Avoidance) - 1
    PCL5.Fullscale.First.Date<- Entered_Scores_Df$Date[1]
    PCL5.Intrusion.First.Date<- Entered_Scores_Df$Date[1]
    PCL5.NACM.First.Date<- Entered_Scores_Df$Date[1]
    PCL5.Hyperarousal.First.Date<- Entered_Scores_Df$Date[1]
    PCL5.Avoidance.First.Date<- Entered_Scores_Df$Date[1]
    PCL5.Fullscale.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    PCL5.Intrusion.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    PCL5.NACM.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    PCL5.Hyperarousal.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    PCL5.Avoidance.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    PCL5.Fullscale.First.Score<- Entered_Scores_Df$Score[1]
    PCL5.Intrusion.First.Score<- Entered_Scores_Df$Score_Intrusion[1]
    PCL5.NACM.First.Score<- Entered_Scores_Df$Score_NACM[1]
    PCL5.Hyperarousal.First.Score<- Entered_Scores_Df$Score_Hyperarousal[1]
    PCL5.Avoidance.First.Score<- Entered_Scores_Df$Score_Avoidance[1]
    PCL5.Fullscale.Last.Score<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)]
    PCL5.Intrusion.Last.Score<- Entered_Scores_Df$Score_Intrusion[length(Entered_Scores_Df$Score_Intrusion)]
    PCL5.NACM.Last.Score<- Entered_Scores_Df$Score_NACM[length(Entered_Scores_Df$Score_NACM)]
    PCL5.Hyperarousal.Last.Score<- Entered_Scores_Df$Score_Hyperarousal[length(Entered_Scores_Df$Score_Hyperarousal)]
    PCL5.Avoidance.Last.Score<- Entered_Scores_Df$Score_Avoidance[length(Entered_Scores_Df$Score_Avoidance)]
    
    
    Analytics_Df<<- data.frame(PCL5.Fullscale.First.Date, PCL5.Fullscale.First.Score, PCL5.Fullscale.Comparisons, PCL5.Fullscale.Change, PCL5.Fullscale.Last.Date, PCL5.Fullscale.Last.Score, PCL5.Fullscale.Improvement,PCL5.Fullscale.Sig.Improvement, PCL5.Fullscale.Deterioration, PCL5.Fullscale.Sig.Deterioration,
                               PCL5.Intrusion.First.Date, PCL5.Intrusion.First.Score, PCL5.Intrusion.Comparisons, PCL5.Intrusion.Change, PCL5.Intrusion.Last.Date, PCL5.Intrusion.Last.Score, PCL5.Intrusion.Improvement, PCL5.Intrusion.Sig.Improvement, PCL5.Intrusion.Deterioration, PCL5.Intrusion.Sig.Deterioration,
                               PCL5.NACM.First.Date, PCL5.NACM.First.Score, PCL5.NACM.Comparisons, PCL5.NACM.Change, PCL5.NACM.Last.Date, PCL5.NACM.Last.Score, PCL5.NACM.Improvement, PCL5.NACM.Sig.Improvement, PCL5.NACM.Deterioration, PCL5.NACM.Sig.Deterioration, 
                               PCL5.Hyperarousal.First.Date, PCL5.Hyperarousal.First.Score, PCL5.Hyperarousal.Comparisons, PCL5.Hyperarousal.Change, PCL5.Hyperarousal.Last.Date, PCL5.Hyperarousal.Last.Score, PCL5.Hyperarousal.Improvement, PCL5.Hyperarousal.Sig.Improvement, PCL5.Hyperarousal.Deterioration, PCL5.Hyperarousal.Sig.Deterioration, 
                               PCL5.Avoidance.First.Date, PCL5.Avoidance.First.Score, PCL5.Avoidance.Comparisons, PCL5.Avoidance.Change, PCL5.Avoidance.Last.Date, PCL5.Avoidance.Last.Score, PCL5.Avoidance.Improvement, PCL5.Avoidance.Sig.Improvement, PCL5.Avoidance.Deterioration, PCL5.Avoidance.Sig.Deterioration)
    
    
    
  })
  
  
  
  #Create an expression to activate the entered data & create notifications to indicate inaccurate data entry
  
  Entered_Scores_Notifications<- observeEvent(input$Action_Submit_Data, {
    
    Entered_Scores_Reac()
    
    if(length(Score_1a) < 20) {
      showNotification("The PCL-5 is a 20-item scale. You have entered less than 20 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(length(Score_1a) > 20) {
      showNotification("The PCL-5 is a 20-item scale. You have entered more than 20 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(input$Timepoint != "1") {
      if(length(Score_2a) < 20) {
        showNotification("The PCL-5 is a 20-item scale. You have entered less than 20 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_2a) > 20) {
        showNotification("The PCL-5 is a 20-item scale. You have entered more than 20 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
    if(input$Timepoint == "3") {
      if(length(Score_3a) < 20) {
        showNotification("The PCL-5 is a 20-item scale. You have entered less than 20 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_3a) > 20) {
        showNotification("The PCL-5 is a 20-item scale. You have entered more than 20 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
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
    
    Gap_Intrusion<- Entered_Scores_Df[1,11] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),11]
    Entered_Scores_Df[1,12]<- Gap_Intrusion
    
    Gap_NACM<- Entered_Scores_Df[1,20] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),20]
    Entered_Scores_Df[1,21]<- Gap_NACM
    
    Gap_Hyperarousal<- Entered_Scores_Df[1,29] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),29]
    Entered_Scores_Df[1,30]<- Gap_Hyperarousal
    
    Gap_Avoidance<- Entered_Scores_Df[1,38] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),38]
    Entered_Scores_Df[1,39]<- Gap_Avoidance
    
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
    
    filename = paste0(" PCL-5 Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
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
        Stats_Table_Intrusion = Stats_Table_Intrusion,
        Stats_Table_NACM = Stats_Table_NACM,
        Stats_Table_Hyperarousal = Stats_Table_Hyperarousal,
        Stats_Table_Avoidance = Stats_Table_Avoidance,
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
      paste(paste0(" PCL-5 Data ", format(Sys.time(), '%d/%m/%y')), input$Output_Filetype1, sep = ".")
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













