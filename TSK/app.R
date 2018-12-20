
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

source("Diagnoses.R")

Research_Table<- read_excel("ResearchTable.xlsx")

ui<- function(request) {
  sidebar <- dashboardSidebar(
    sidebarMenu(
      br(),
      menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "TSK-11"),
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
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| Tampa Scale of Kinesiophobia (TSK)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 680),
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
                
                h3(tags$strong("References")), br(), br(),
                
                "Cleland, J. A., Fritz, J. M., & Childs, J. D. (2008). Psychometric properties of the fear-avoidance beliefs questionnaire and tampa scale of kinesiophobia in patients with neck pain. American Journal of Physical Medicine & Rehabilitation, 87(2), 109-117. doi:10.1097/PHM.0b013e31815b61f1", br(),br(),
                "Hapidou, E. G., O'Brien, M. A., Pierrynowski, M. R., de las Heras, E., Patel, M., & Patla, T. (2012). Fear and avoidance of movement in people with chronic pain: Psychometric properties of the 11-item tampa scale for kinesiophobia (TSK-11). Physiotherapy Canada, 64(3), 235-241.", br(),br(), 
                "Larsson, C., Hansson, E. E., Sundquist, K., & Jakobsson, U. (2016). Kinesiophobia and its relation to pain characteristics and cognitive affective variables in older adults with chronic pain. BMC Geriatrics, 16(1), 128.", br(),br(), 
                "Lundberg, M., Styf, J., & Jansson, B. (2009). On what patients does the tampa scale for kinesiophobia fit? Physiotherapy Theory and Practice, 25(7), 495-506.", br(),br(), 
                "Lüning Bergsten, C., Lundberg, M., Lindberg, P., & Elfving, B. (2012). Change in kinesiophobia and its relation to activity limitation after multidisciplinary rehabilitation in patients with chronic back pain. Disability and Rehabilitation, 34(10), 852-858.", br(),br(), 
                "Neblett, R., Hartzell, M., Mayer, T., Bradford, E., & Gatchel, R. (2016). Establishing clinically meaningful severity levels for the tampa scale for kinesiophobia (TSK‐13). European Journal of Pain, 20(5), 701-710.", br(),br(), 
                "Roelofs, J., Sluiter, J. K., Frings-Dresen, M. H., Goossens, M., Thibault, P., Boersma, K., & Vlaeyen, J. W. (2007). Fear of movement and (re) injury in chronic musculoskeletal pain: Evidence for an invariant two-factor model of the tampa scale for kinesiophobia across pain diagnoses and dutch, swedish, and canadian samples. Pain, 131(1-2), 181-190.", br(),br(), 
                "Tkachuk, G. A., & Harris, C. A. (2012). Psychometric properties of the tampa scale for kinesiophobia-11 (TSK-11). The Journal of Pain : Official Journal of the American Pain Society, 13(10), 970-977. doi:10.1016/j.jpain.2012.07.001", br(),br(),
                "Woby, S. R., Roach, N. K., Urmston, M., & Watson, P. J. (2005). Psychometric properties of the TSK-11: A shortened version of the tampa scale for kinesiophobia. Pain, 117(1-2), 137-144." 

               ),
        
        
        
        tabItem(tabName = "TSK-11",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel("Scale",
                             wellPanel(style = "background-color: #ffffff; color: black",
                                       fluidRow(
                                         column(width = 12, offset = 5, h3(tags$strong("TSK")))
                                       ),
                                       fluidRow(
                                         column(width = 12, h4(tags$strong("Here are some of the things which other patients have told us about their pain. For each statement please circle any number from 1 to 4 to 
                                                                          signify whether you agree or disagree with the statement.")))
                                               ),
                                       br(),
                                       br(),
                                       fluidRow(
                                         column(width = 7),
                                         column(width = 1, h5(tags$strong("Highly Disagree"))),
                                         column(width = 1, style='padding-left:20px;', h5(tags$strong("Somewhat Disagree"))),
                                         column(width = 1, style='padding-left:34px;', h5(tags$strong("Somewhat Agree"))),
                                         column(width = 1, style='padding-left:40px;', h5(tags$strong("Highly Agree")))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("1. I’m afraid that I might injure myself if I exercise")),
                                         column(width = 5,  radioButtons("Item_1", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("2. If I were to try to overcome it, my pain would increase")),
                                         column(width = 5, radioButtons("Item_2", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("3. My body is telling me I have something dangerously wrong")),
                                         column(width = 5, radioButtons("Item_3", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("4. My pain would probably be relieved if I were to exercise")),
                                         column(width = 5, radioButtons("Item_4", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("5. People aren’t taking my medical condition seriously enough")),
                                         column(width = 5, radioButtons("Item_5", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("6. My accident has put my body at risk for the rest of my life")),
                                         column(width = 5, radioButtons("Item_6", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("7. Pain always means I have injured my body")),
                                         column(width = 5, radioButtons("Item_7", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("8. Just because something aggravates my pain does not mean it is dangerous")),
                                         column(width = 5, radioButtons("Item_8", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("9. I am afraid that I might injure myself accidentally")),
                                         column(width = 5, radioButtons("Item_9", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("10. Simply being careful that I do not make any unnecessary movements is the safest thing I can do to prevent my pain from worsening")),
                                         column(width = 5, radioButtons("Item_10", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("11. I wouldn’t have this much pain if there weren’t something potentially dangerous going on in my body")),
                                         column(width = 5, radioButtons("Item_11", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                               ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("12. Although my condition is painful, I would be better off if I were physically active")),
                                         column(width = 5, radioButtons("Item_12", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("13. Pain lets me know when to stop exercising so that I don’t injure myself")),
                                         column(width = 5, radioButtons("Item_13", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("14. It’s really not safe for a person with a condition like mine to be physically active")),
                                         column(width = 5, radioButtons("Item_14", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("15. I can’t do all the things normal people do because it’s too easy for me to get injured")),
                                         column(width = 5, radioButtons("Item_15", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("16. Even though something is causing me a lot of pain, I don’t think it’s actually dangerous")),
                                         column(width = 5, radioButtons("Item_16", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 7, h4("17. No one should have to exercise when he/she is in pain")),
                                         column(width = 5, radioButtons("Item_17", label = NULL, choices = c("1", "2", "3", "4"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, dateInput("Q_Date", "Date",  format = "dd/mm/yyyy")),
                                         column(width = 4, textInput("Q_Name", "Name")),
                                         column(width = 4, textInput("Q_Clin_Name", "Clinician's Name"))
                                       ), 
                                       fluidRow(
                                         column(width = 12, h5("Scale Source: Miller R. P, Kori S. H & Todd D. D. (1991). The Tampa Scale. Unpublished."))
                                       )
                             )
                             
                    ),
                    tabPanel("Enter Data",
                             fluidRow(
                               column(width = 12,
                                      titlePanel(span(tagList(icon("edit", lib = "font-awesome")), h4(tags$b("Enter Scores for Each Timepoint")),
                                                      tags$ul(
                                                        tags$li(helpText(h5(tags$em(tags$b("Use commas to separate scores. Enter 17 scores in order, from the first to the last item of the total scale.", style = "color:black")))))
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
                                                      br(),
                                                      selectInput("Version", h4(tags$strong("Select Version of Scale")), choices = c("13 items (TSK-13)","11 items (TSK-11)/17 items (TSK-17)")),
                                                      conditionalPanel(condition = "input.Version == '11 items (TSK-11)/17 items (TSK-17)'",
                                                                       helpText("Selecting this option means that the total scale will be calculated with all 17 items. The Somatic Focus subscale will be calculated with 5 items, and 
                                                                                the Activity Avoidance subscale will be calculated with 6 items.")),
                                                      h4(tags$strong("Select the population")),
                                                      conditionalPanel(condition = "input.Version == '11 items (TSK-11)/17 items (TSK-17)'",
                                                      selectInput("Pop_11", "", choices = c("Chronic Low Back Pain", "Musculoskeletal Pain", "Fibromyalgia", "Upper Extremity Disorders", "Osteoarthritis", "Chronic Pain 65 Years +"))
                                                                      ),
                                                      conditionalPanel(condition = "input.Version == '13 items (TSK-13)'",
                                                      selectInput("Pop_13", "", choices = c("Chronic Low Back Pain", "Fibromyalgia"))
                                                                     )
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
                                                               uiOutput("Manual_CI_Widg"),
                                                               conditionalPanel(condition = "input.Select_CI == '2'",
                                                                                numericInput("Man_CI", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Somatic_Focus", label = "Somatic Focus",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Somatic_Focus == '2'",
                                                                                numericInput("Man_CI_Somatic_Focus", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Activity_Avoidance", label = "Activity Avoidance",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Activity_Avoidance == '2'",
                                                                                numericInput("Man_CI_Activity_Avoidance", "Specify the width of the confidence interval", value = 0))
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
                                                               uiOutput("Mean_Widg_Somatic_Focus")
                                                        ),
                                                        column(width = 3,
                                                               uiOutput("Mean_Widg_Activity_Avoidance")
                                                        )
                                                      ),
                                                      
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Chelune et al. (1993)' || input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)' || input.RelChangeMethod == 'Speer (1992)'",
                                                                       
                                                                       h4(tags$strong("Enter a mean value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                               uiOutput("Retest_M_Widg")
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Somatic_Focus", "Somatic Focus", value = 0)
                                                                         ),
                                                                         column(width = 3,
                                                                               uiOutput("Retest_M_AA_Widg")
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
                                                               uiOutput("Sd_Widg_Somatic_Focus")
                                                        ),
                                                        column(width = 3,
                                                               uiOutput("Sd_Widg_Activity_Avoidance")
                                                        )
                                                      ),
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)'",
                                                                       
                                                                       h4(tags$strong("Enter a standard deviation value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                uiOutput("Retest_Sd_Widg")
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Somatic_Focus", "Somatic Focus", value = 0)
                                                                         ),
                                                                         column(width = 3,
                                                                                uiOutput("Retest_Sd_AA_Widg")
                                                                         )
                                                                       )
                                                                       
                                                      )
                                             ),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      h4(tags$strong("Enter test-retest reliability values")),
                                                      uiOutput("Retest_Widg"),
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
                                                               uiOutput("Cutoff_Widg_Somatic_Focus_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Activity_Avoidance_1") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Second cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Somatic_Focus_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Activity_Avoidance_2") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Third cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Somatic_Focus_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Activity_Avoidance_3") 
                                                        )
                                                        
                                                      )
                                                      , hr()
                                                      
                                             ),
                                             
                                             
                                             h4(tags$strong("Psychometric Properties of the TSK Relevant to Assessing Reliable & Clinically Significant Change")),
                                             h6(em("*Values marked with an asterix represent internal consistency, not test-retest reliability.")),
                                             DT::dataTableOutput("Research_Table"),
                                             br(),
                                             h4("Add Your Own Research Sources For Future Reference"),
                                             h6(em("Remember to click 'Save Settings' under the 'Download Report' tab")),
                                             rHandsontableOutput("User_Research", width = 900)
                                             
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
                                              helpText(h5(tags$em("Allow a few moments for the report to load.", style = "color:white"))),
                                              hr(),
                                              br(),
                                              bookmarkButton(label = "Step 4 (Optional). Save Settings", title = "Maintain the program in its current state for future use (i.e. retain the data & values you entered)."),
                                              br(),
                                              helpText(h5(tags$em("Hover to learn more.", style = "color:white")))
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
                      input$Item_8, input$Item_9, input$Item_10, input$Item_11, input$Item_12, input$Item_13, input$Item_14,
                      input$Item_15, input$Item_16, input$Item_17, sep = ",")
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
                    , pageLength = 50, dom = "t", scrollX = TRUE, fixedColumns = list(leftColumns = 3), autoWidth = TRUE, columnDefs = list(list(width = '180px', targets = c(1,9,10)), list(width= '140px', targets = c(0,2))))) %>% formatStyle('Authors', color = '#ffffff', backgroundColor = '#d35400')
    
  })
  
  #Create modifiable research table for user to enter additional research sources
  
  cache_tbl = NULL
  
  onRestore(function(state) {
    tmp = state$input$User_Research
    tmp$data = jsonlite::fromJSON(
      jsonlite::toJSON(tmp$data), simplifyVector = FALSE)
    cache_tbl <<- tmp
  })
  
  data = reactive({
    if (!is.null(input$User_Research)) {
      User_Df = hot_to_r(input$User_Research)
    } else if (!is.null(cache_tbl)) {
      User_Df = hot_to_r(cache_tbl)
      cache_tbl <<- NULL
    } else {
      User_Df = data.frame(Authors = rep("", 10), N = rep("", 10), Scale = rep("", 10), Test.Retest.Reliability = rep("", 10), Retest.Interval = rep("", 10),
                           Mean = rep("", 10), Retest.Mean = rep("", 10), Sd = rep("", 10), Retest.Sd = rep("", 10), Cutoff.Score.Information = rep("", 10),
                           Notes = rep("", 10), stringsAsFactors = F)
    }
    User_Df
  })
  
  output$User_Research <- renderRHandsontable({
    User_Df = data()
    if (!is.null(User_Df))
      rhandsontable(User_Df, stretchH = "all")
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
    if(input$Version == "13 items (TSK-13)") {
      #data field lab
      TSK_Lab<<- "TSK-13 total scale"
      AA_Lab<<- "Activity Avoidance (8 items)"
      #Graph info
      Graph_Up_Lim<<- 62
      Y_Vals<<- c(0,8,16,24,32,40,48,52)
      Y_Vals_Labs<<- c("0 Min","8","16","24","32","40","48","52 Max")
      Graph_Title<<- "Total TSK-13 Score"
      #Graph info AA
      Graph_Up_Lim_AA<<- 42
      Y_Vals_AA<<- c(0,8,16,24,32)
      Y_Vals_Labs_AA<<- c("0 Min","8","16","24","32 Max")
    if(input$Pop_13 == "Chronic Low Back Pain") {
      Mean_Val<<- 33.8
      Sd_Val<<- 7.6
      Source_Mean<<- "Roelofs et al. (2004)"
      Source_Sd<<- "Roelofs et al. (2004)"
      Cut_Val_1<<- 23
      Cut_Val_2<<-33
      Cut_Val_3<<- 43
      Cut_Lab_1<<- "Mild"
      Cut_Lab_2<<- "Moderate"
      Cut_Lab_3<<- "Severe"
      Source_Cutoff_1<<- "Neblett, Hartzell, Mayer, Bradford & Gatchel (2015)"
      Source_Cutoff_2<<- "Neblett, Hartzell, Mayer, Bradford & Gatchel (2015)"
      Source_Cutoff_3<<- "Neblett, Hartzell, Mayer, Bradford & Gatchel (2015)"
      Mean_Val_Somatic_Focus<<- 12.4
      Sd_Val_Somatic_Focus<<-5.2
      Source_Mean_Somatic_Focus<<- "Roelofs et al. (2004)"
      Source_Sd_Somatic_Focus<<- "Roelofs et al. (2004)"
      Cut_Val_Somatic_Focus_1<<- Mean_Val_Somatic_Focus 
      Cut_Val_Somatic_Focus_2<<- Mean_Val_Somatic_Focus + Sd_Val_Somatic_Focus
      Cut_Val_Somatic_Focus_3<<- Mean_Val_Somatic_Focus + (2*Sd_Val_Somatic_Focus)
      Cut_Lab_1_Somatic_Focus<<- "Mean"
      Cut_Lab_2_Somatic_Focus<<- "Mean + 1 Sd"
      Cut_Lab_3_Somatic_Focus<<- "Mean + 2 Sd"
      Source_Cutoff_Somatic_Focus_1<<- "Roelofs et al. (2004)"
      Source_Cutoff_Somatic_Focus_2<<- "Roelofs et al. (2004)"
      Source_Cutoff_Somatic_Focus_3<<- "Roelofs et al. (2004)"
      Mean_Val_Activity_Avoidance<<- 21.4
      Sd_Val_Activity_Avoidance<<- 3.6
      Source_Mean_Activity_Avoidance<<- "Roelofs et al. (2004)"
      Source_Sd_Activity_Avoidance<<- "Roelofs et al. (2004)"
      Cut_Val_Activity_Avoidance_1<<- Mean_Val_Activity_Avoidance 
      Cut_Val_Activity_Avoidance_2<<- Mean_Val_Activity_Avoidance + Sd_Val_Activity_Avoidance
      Cut_Val_Activity_Avoidance_3<<- Mean_Val_Activity_Avoidance + (2*Sd_Val_Activity_Avoidance)
      Cut_Lab_1_Activity_Avoidance<<- "Mean"
      Cut_Lab_2_Activity_Avoidance<<- "Mean + 1 Sd"
      Cut_Lab_3_Activity_Avoidance<<- "Mean + 2 Sd"
      Source_Cutoff_Activity_Avoidance_1<<- "Roelofs et al. (2004)"
      Source_Cutoff_Activity_Avoidance_2<<- "Roelofs et al. (2004)"
      Source_Cutoff_Activity_Avoidance_3<<- "Roelofs et al. (2004)"
    } else if(input$Pop_13 == "Fibromyalgia") {
      Mean_Val<<- 28.2
      Sd_Val<<- 7.1
      Source_Mean<<- "Roelofs et al. (2004)"
      Source_Sd<<- "Roelofs et al. (2004)"
      Cut_Val_1<<- 23
      Cut_Val_2<<-33
      Cut_Val_3<<- 43
      Cut_Lab_1<<- "Mild"
      Cut_Lab_2<<- "Moderate"
      Cut_Lab_3<<- "Severe"
      Source_Cutoff_1<<- "Neblett, Hartzell, Mayer, Bradford & Gatchel (2015)"
      Source_Cutoff_2<<- "Neblett, Hartzell, Mayer, Bradford & Gatchel (2015)"
      Source_Cutoff_3<<- "Neblett, Hartzell, Mayer, Bradford & Gatchel (2015)"
      Mean_Val_Somatic_Focus<<- 10.4
      Sd_Val_Somatic_Focus<<- 3.3
      Source_Mean_Somatic_Focus<<- "Roelofs et al. (2004)"
      Source_Sd_Somatic_Focus<<- "Roelofs et al. (2004)"
      Cut_Val_Somatic_Focus_1<<- Mean_Val_Somatic_Focus 
      Cut_Val_Somatic_Focus_2<<- Mean_Val_Somatic_Focus + Sd_Val_Somatic_Focus
      Cut_Val_Somatic_Focus_3<<- Mean_Val_Somatic_Focus + (2*Sd_Val_Somatic_Focus)
      Cut_Lab_1_Somatic_Focus<<- "Mean"
      Cut_Lab_2_Somatic_Focus<<- "Mean + 1 Sd"
      Cut_Lab_3_Somatic_Focus<<- "Mean + 2 Sd"
      Source_Cutoff_Somatic_Focus_1<<- "Roelofs et al. (2004)"
      Source_Cutoff_Somatic_Focus_2<<- "Roelofs et al. (2004)"
      Source_Cutoff_Somatic_Focus_3<<- "Roelofs et al. (2004)"
      Mean_Val_Activity_Avoidance<<- 17.9
      Sd_Val_Activity_Avoidance<<- 5
      Source_Mean_Activity_Avoidance<<- "Roelofs et al. (2004)"
      Source_Sd_Activity_Avoidance<<- "Roelofs et al. (2004)"
      Cut_Val_Activity_Avoidance_1<<- Mean_Val_Activity_Avoidance  
      Cut_Val_Activity_Avoidance_2<<- Mean_Val_Activity_Avoidance + Sd_Val_Activity_Avoidance
      Cut_Val_Activity_Avoidance_3<<- Mean_Val_Activity_Avoidance + (2*Sd_Val_Activity_Avoidance)
      Cut_Lab_1_Activity_Avoidance<<- "Mean"
      Cut_Lab_2_Activity_Avoidance<<- "Mean + 1 Sd"
      Cut_Lab_3_Activity_Avoidance<<- "Mean + 2 Sd"
      Source_Cutoff_Activity_Avoidance_1<<- "Roelofs et al. (2004)"
      Source_Cutoff_Activity_Avoidance_2<<- "Roelofs et al. (2004)"
      Source_Cutoff_Activity_Avoidance_3<<- "Roelofs et al. (2004)"
    } 
    } else if(input$Version == "11 items (TSK-11)/17 items (TSK-17)") {  
      #data field lab
      TSK_Lab<<- "TSK-17 total scale"
      AA_Lab<<- "Activity Avoidance (6 items)"
      #Graph info
      Graph_Up_Lim<<- 78
      Y_Vals<<- c(0,4,12,20,28,36,44,52,60,68)
      Y_Vals_Labs<<- c("0 Min","4","12","20","28","36","44","52","60","68 Max")
      Graph_Title<<- "Total TSK-17 Score"
      #Graph info AA
      Graph_Up_Lim_AA<<- 34
      Y_Vals_AA<<- c(0,8,16,24)
      Y_Vals_Labs_AA<<- c("0 Min","8","16","24 Max")
      if(input$Pop_11 == "Upper Extremity Disorders") {
      Mean_Val<<- 37.8
      Sd_Val<<- 7.6
      Source_Mean<<- "Roelofs et al. (2011)"
      Source_Sd<<- "Roelofs et al. (2011)"
      Cut_Val_1<<-  Mean_Val 
      Cut_Val_2<<-  Mean_Val + Sd_Val 
      Cut_Val_3<<- Mean_Val + (2*Sd_Val)
      Cut_Lab_1<<- "Mean"
      Cut_Lab_2<<- "Mean + 1 Sd"
      Cut_Lab_3<<- "Mean + 2 Sd"
      Source_Cutoff_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_3<<- "Roelofs et al. (2011)"
      Mean_Val_Somatic_Focus<<- 11.3
      Sd_Val_Somatic_Focus<<-3.2
      Source_Mean_Somatic_Focus<<- "Roelofs et al. (2011)"
      Source_Sd_Somatic_Focus<<- "Roelofs et al. (2011)"
      Cut_Val_Somatic_Focus_1<<- Mean_Val_Somatic_Focus 
      Cut_Val_Somatic_Focus_2<<- Mean_Val_Somatic_Focus + Sd_Val_Somatic_Focus
      Cut_Val_Somatic_Focus_3<<- Mean_Val_Somatic_Focus + (2*Sd_Val_Somatic_Focus)
      Cut_Lab_1_Somatic_Focus<<- "Mean"
      Cut_Lab_2_Somatic_Focus<<- "Mean + 1 Sd"
      Cut_Lab_3_Somatic_Focus<<- "Mean + 2 Sd"
      Source_Cutoff_Somatic_Focus_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_Somatic_Focus_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_Somatic_Focus_3<<- "Roelofs et al. (2011)"
      Mean_Val_Activity_Avoidance<<- 14.3
      Sd_Val_Activity_Avoidance<<- 3.6
      Source_Mean_Activity_Avoidance<<- "Roelofs et al. (2011)"
      Source_Sd_Activity_Avoidance<<- "Roelofs et al. (2011)"
      Cut_Val_Activity_Avoidance_1<<- Mean_Val_Activity_Avoidance
      Cut_Val_Activity_Avoidance_2<<- Mean_Val_Activity_Avoidance + Sd_Val_Activity_Avoidance 
      Cut_Val_Activity_Avoidance_3<<- Mean_Val_Activity_Avoidance + (2*Sd_Val_Activity_Avoidance)
      Cut_Lab_1_Activity_Avoidance<<- "Mean"
      Cut_Lab_2_Activity_Avoidance<<- "Mean + 1 Sd"
      Cut_Lab_3_Activity_Avoidance<<- "Mean + 2 Sd"
      Source_Cutoff_Activity_Avoidance_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_Activity_Avoidance_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_Activity_Avoidance_3<<- "Roelofs et al. (2011)"
    } else if(input$Pop_11 == "Chronic Low Back Pain") {
      Mean_Val<<- 43.2
      Sd_Val<<- 8.4
      Source_Mean<<- "Roelofs et al. (2011)"
      Source_Sd<<- "Roelofs et al. (2011)"
      Cut_Val_1<<- Mean_Val 
      Cut_Val_2<<- Mean_Val + Sd_Val
      Cut_Val_3<<- Mean_Val + (2*Sd_Val)
      Cut_Lab_1<<- "Mean"
      Cut_Lab_2<<- "Mean + 1 Sd"
      Cut_Lab_3<<- "Mean + 2 Sd"
      Source_Cutoff_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_3<<- "Roelofs et al. (2011)"
      Mean_Val_Somatic_Focus<<- 12.1
      Sd_Val_Somatic_Focus<<-3.6
      Source_Mean_Somatic_Focus<<- "Roelofs et al. (2011)"
      Source_Sd_Somatic_Focus<<- "Roelofs et al. (2011)"
      Cut_Val_Somatic_Focus_1<<- Mean_Val_Somatic_Focus 
      Cut_Val_Somatic_Focus_2<<- Mean_Val_Somatic_Focus + Sd_Val_Somatic_Focus 
      Cut_Val_Somatic_Focus_3<<- Mean_Val_Somatic_Focus + (2*Sd_Val_Somatic_Focus)
      Cut_Lab_1_Somatic_Focus<<- "Mean"
      Cut_Lab_2_Somatic_Focus<<- "Mean + 1 Sd"
      Cut_Lab_3_Somatic_Focus<<- "Mean + 2 Sd"
      Source_Cutoff_Somatic_Focus_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_Somatic_Focus_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_Somatic_Focus_3<<- "Roelofs et al. (2011)"
      Mean_Val_Activity_Avoidance<<- 16.1
      Sd_Val_Activity_Avoidance<<- 4.3
      Source_Mean_Activity_Avoidance<<- "Roelofs et al. (2011)"
      Source_Sd_Activity_Avoidance<<- "Roelofs et al. (2011)"
      Cut_Val_Activity_Avoidance_1<<- Mean_Val_Activity_Avoidance 
      Cut_Val_Activity_Avoidance_2<<- Mean_Val_Activity_Avoidance + Sd_Val_Activity_Avoidance
      Cut_Val_Activity_Avoidance_3<<- Mean_Val_Activity_Avoidance + (2*Sd_Val_Activity_Avoidance)
      Cut_Lab_1_Activity_Avoidance<<- "Mean"
      Cut_Lab_2_Activity_Avoidance<<- "Mean + 1 Sd"
      Cut_Lab_3_Activity_Avoidance<<- "Mean + 2 Sd"
      Source_Cutoff_Activity_Avoidance_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_Activity_Avoidance_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_Activity_Avoidance_3<<- "Roelofs et al. (2011)"
    } else if(input$Pop_11 == "Fibromyalgia") {
      Mean_Val<<- 36.6
      Sd_Val<<- 8.4
      Source_Mean<<- "Roelofs et al. (2011)"
      Source_Sd<<- "Roelofs et al. (2011)"
      Cut_Val_1<<- Mean_Val
      Cut_Val_2<<- Mean_Val + Sd_Val
      Cut_Val_3<<- Mean_Val + (2*Sd_Val)
      Cut_Lab_1<<- "Mean"
      Cut_Lab_2<<- "Mean + 1 Sd"
      Cut_Lab_3<<- "Mean + 2 Sd"
      Source_Cutoff_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_3<<- "Roelofs et al. (2011)"
      Mean_Val_Somatic_Focus<<- 10.4
      Sd_Val_Somatic_Focus<<- 3.3
      Source_Mean_Somatic_Focus<<- "Roelofs et al. (2011)"
      Source_Sd_Somatic_Focus<<- "Roelofs et al. (2011)"
      Cut_Val_Somatic_Focus_1<<- Mean_Val_Somatic_Focus
      Cut_Val_Somatic_Focus_2<<- Mean_Val_Somatic_Focus + Sd_Val_Somatic_Focus
      Cut_Val_Somatic_Focus_3<<- Mean_Val_Somatic_Focus + (2*Sd_Val_Somatic_Focus)
      Cut_Lab_1_Somatic_Focus<<- "Mean"
      Cut_Lab_2_Somatic_Focus<<- "Mean + 1 Sd"
      Cut_Lab_3_Somatic_Focus<<- "Mean + 2 Sd"
      Source_Cutoff_Somatic_Focus_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_Somatic_Focus_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_Somatic_Focus_3<<- "Roelofs et al. (2011)"
      Mean_Val_Activity_Avoidance<<- 14
      Sd_Val_Activity_Avoidance<<-3.8
      Source_Mean_Activity_Avoidance<<- "Roelofs et al. (2011)"
      Source_Sd_Activity_Avoidance<<- "Roelofs et al. (2011)"
      Cut_Val_Activity_Avoidance_1<<- Mean_Val_Activity_Avoidance  
      Cut_Val_Activity_Avoidance_2<<- Mean_Val_Activity_Avoidance + Sd_Val_Activity_Avoidance  
      Cut_Val_Activity_Avoidance_3<<- Mean_Val_Activity_Avoidance + (2*Sd_Val_Activity_Avoidance)
      Cut_Lab_1_Activity_Avoidance<<- "Mean"
      Cut_Lab_2_Activity_Avoidance<<- "Mean + 1 Sd"
      Cut_Lab_3_Activity_Avoidance<<- "Mean + 2 Sd"
      Source_Cutoff_Activity_Avoidance_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_Activity_Avoidance_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_Activity_Avoidance_3<<- "Roelofs et al. (2011)"
    } else if(input$Pop_11 == "Osteoarthritis") {
      Mean_Val<<- 24.5
      Sd_Val<<- 6
      Source_Mean<<- "Roelofs et al. (2011)"
      Source_Sd<<- "Roelofs et al. (2011)"
      Cut_Val_1<<- Mean_Val 
      Cut_Val_2<<- Mean_Val + Sd_Val
      Cut_Val_3<<- Mean_Val + (2*Sd_Val)
      Cut_Lab_1<<- "Mean"
      Cut_Lab_2<<- "Mean + 1 Sd"
      Cut_Lab_3<<- "Mean + 2 Sd"
      Source_Cutoff_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_3<<-  "Roelofs et al. (2011)"
      Mean_Val_Somatic_Focus<<- 10.6
      Sd_Val_Somatic_Focus<<- 3.2
      Source_Mean_Somatic_Focus<<-"Roelofs et al. (2011)"
      Source_Sd_Somatic_Focus<<- "Roelofs et al. (2011)"
      Cut_Val_Somatic_Focus_1<<- Mean_Val_Somatic_Focus
      Cut_Val_Somatic_Focus_2<<- Mean_Val_Somatic_Focus + Sd_Val_Somatic_Focus 
      Cut_Val_Somatic_Focus_3<<- Mean_Val_Somatic_Focus + (2*Sd_Val_Somatic_Focus)
      Cut_Lab_1_Somatic_Focus<<- "Mean"
      Cut_Lab_2_Somatic_Focus<<- "Mean + 1 Sd"
      Cut_Lab_3_Somatic_Focus<<- "Mean + 2 Sd"
      Source_Cutoff_Somatic_Focus_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_Somatic_Focus_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_Somatic_Focus_3<<- "Roelofs et al. (2011)"
      Mean_Val_Activity_Avoidance<<- 13.9
      Sd_Val_Activity_Avoidance<<- 3.7
      Source_Mean_Activity_Avoidance<<- "Roelofs et al. (2011)" 
      Source_Sd_Activity_Avoidance<<- "Roelofs et al. (2011)"
      Cut_Val_Activity_Avoidance_1<<- Mean_Val_Activity_Avoidance 
      Cut_Val_Activity_Avoidance_2<<- Mean_Val_Activity_Avoidance + Sd_Val_Activity_Avoidance 
      Cut_Val_Activity_Avoidance_3<<- Mean_Val_Activity_Avoidance + (2*Sd_Val_Activity_Avoidance)
      Cut_Lab_1_Activity_Avoidance<<- "Mean"
      Cut_Lab_2_Activity_Avoidance<<- "Mean + 1 Sd"
      Cut_Lab_3_Activity_Avoidance<<- "Mean + 2 Sd"
      Source_Cutoff_Activity_Avoidance_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_Activity_Avoidance_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_Activity_Avoidance_3<<- "Roelofs et al. (2011)"
    } else if(input$Pop_11 == "Musculoskeletal Pain") {
      Mean_Val<<- 42
      Sd_Val<<- 8.2
      Source_Mean<<- "Roelofs et al. (2011)"
      Source_Sd<<- "Roelofs et al. (2011)"
      Cut_Val_1<<- Mean_Val 
      Cut_Val_2<<- Mean_Val + Sd_Val
      Cut_Val_3<<- Mean_Val + (2*Sd_Val)
      Cut_Lab_1<<- "Mean"
      Cut_Lab_2<<- "Mean + 1 Sd"
      Cut_Lab_3<<- "Mean + 2 Sd"
      Source_Cutoff_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_3<<- "Roelofs et al. (2011)"
      Mean_Val_Somatic_Focus<<- 13
      Sd_Val_Somatic_Focus<<- 3.5
      Source_Mean_Somatic_Focus<<- "Roelofs et al. (2011)"
      Source_Sd_Somatic_Focus<<- "Roelofs et al. (2011)"
      Cut_Val_Somatic_Focus_1<<- Mean_Val_Somatic_Focus 
      Cut_Val_Somatic_Focus_2<<- Mean_Val_Somatic_Focus + Sd_Val_Somatic_Focus
      Cut_Val_Somatic_Focus_3<<- Mean_Val_Somatic_Focus + (2*Sd_Val_Somatic_Focus)
      Cut_Lab_1_Somatic_Focus<<- "Mean"
      Cut_Lab_2_Somatic_Focus<<- "Mean + 1 Sd"
      Cut_Lab_3_Somatic_Focus<<- "Mean + 2 Sd"
      Source_Cutoff_Somatic_Focus_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_Somatic_Focus_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_Somatic_Focus_3<<- "Roelofs et al. (2011)"
      Mean_Val_Activity_Avoidance<<- 15.5
      Sd_Val_Activity_Avoidance<<- 3.5
      Source_Mean_Activity_Avoidance<<- "Roelofs et al. (2011)"
      Source_Sd_Activity_Avoidance<<- "Roelofs et al. (2011)"
      Cut_Val_Activity_Avoidance_1<<- Mean_Val_Activity_Avoidance 
      Cut_Val_Activity_Avoidance_2<<- Mean_Val_Activity_Avoidance + Sd_Val_Activity_Avoidance
      Cut_Val_Activity_Avoidance_3<<- Mean_Val_Activity_Avoidance + (2*Sd_Val_Activity_Avoidance)
      Cut_Lab_1_Activity_Avoidance<<- "Mean"
      Cut_Lab_2_Activity_Avoidance<<- "Mean + 1 Sd"
      Cut_Lab_3_Activity_Avoidance<<- "Mean + 2 Sd"
      Source_Cutoff_Activity_Avoidance_1<<- "Roelofs et al. (2011)"
      Source_Cutoff_Activity_Avoidance_2<<- "Roelofs et al. (2011)"
      Source_Cutoff_Activity_Avoidance_3<<- "Roelofs et al. (2011)"
    } else if(input$Pop_11 == "Chronic Pain 65 Years +") {
      Mean_Val<<- 22.8
      Sd_Val<<- 8.3
      Source_Mean<<- "Larsson, Hansson, Sundquist & Jakobsson (2016)"
      Source_Sd<<- "Larsson, Hansson, Sundquist & Jakobsson (2016)"
      Cut_Val_1<<- Mean_Val 
      Cut_Val_2<<- Mean_Val + Sd_Val
      Cut_Val_3<<- Mean_Val + (2*Sd_Val)
      Cut_Lab_1<<- "Mean"
      Cut_Lab_2<<- "Mean + 1 Sd"
      Cut_Lab_3<<- "Mean + 2 Sd"
      Source_Cutoff_1<<- "Larsson, Hansson, Sundquist & Jakobsson (2016)"
      Source_Cutoff_2<<- "Larsson, Hansson, Sundquist & Jakobsson (2016)"
      Source_Cutoff_3<<-  "Larsson, Hansson, Sundquist & Jakobsson (2016)"
      Mean_Val_Somatic_Focus<<- 9.5
      Sd_Val_Somatic_Focus<<- 3.7
      Source_Mean_Somatic_Focus<<-"Larsson, Hansson, Sundquist & Jakobsson (2016)"
      Source_Sd_Somatic_Focus<<- "Larsson, Hansson, Sundquist & Jakobsson (2016)"
      Cut_Val_Somatic_Focus_1<<- Mean_Val_Somatic_Focus 
      Cut_Val_Somatic_Focus_2<<- Mean_Val_Somatic_Focus + Sd_Val_Somatic_Focus
      Cut_Val_Somatic_Focus_3<<- Mean_Val_Somatic_Focus + (2*Sd_Val_Somatic_Focus)
      Cut_Lab_1_Somatic_Focus<<- "Mean"
      Cut_Lab_2_Somatic_Focus<<- "Mean + 1 Sd"
      Cut_Lab_3_Somatic_Focus<<- "Mean + 2 Sd"
      Source_Cutoff_Somatic_Focus_1<<- "Larsson, Hansson, Sundquist & Jakobsson (2016)"
      Source_Cutoff_Somatic_Focus_2<<- "Larsson, Hansson, Sundquist & Jakobsson (2016)"
      Source_Cutoff_Somatic_Focus_3<<- "Larsson, Hansson, Sundquist & Jakobsson (2016)"
      Mean_Val_Activity_Avoidance<<- 13.4
      Sd_Val_Activity_Avoidance<<- 5.4
      Source_Mean_Activity_Avoidance<<- "Larsson, Hansson, Sundquist & Jakobsson (2016)" 
      Source_Sd_Activity_Avoidance<<- "Larsson, Hansson, Sundquist & Jakobsson (2016)"
      Cut_Val_Activity_Avoidance_1<<- Mean_Val_Activity_Avoidance 
      Cut_Val_Activity_Avoidance_2<<- Mean_Val_Activity_Avoidance + Sd_Val_Activity_Avoidance
      Cut_Val_Activity_Avoidance_3<<- Mean_Val_Activity_Avoidance + (2*Sd_Val_Activity_Avoidance)
      Cut_Lab_1_Activity_Avoidance<<- "Mean"
      Cut_Lab_2_Activity_Avoidance<<- "Mean + 1 Sd"
      Cut_Lab_3_Activity_Avoidance<<- "Mean + 2 Sd"
      Source_Cutoff_Activity_Avoidance_1<<- "Larsson, Hansson, Sundquist & Jakobsson (2016)"
      Source_Cutoff_Activity_Avoidance_2<<- "Larsson, Hansson, Sundquist & Jakobsson (2016)"
      Source_Cutoff_Activity_Avoidance_3<<- "Larsson, Hansson, Sundquist & Jakobsson (2016)"
    }
    }
  })
  
  output$Manual_CI_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
  selectInput("Select_CI", label = paste0(TSK_Lab),
              choices = list("No" = 1, "Yes" = 2),
              selected = 1)
    )
  })
  outputOptions(output, "Manual_CI_Widg", suspendWhenHidden = FALSE)
  
  output$Mean_Widg<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean", paste0(TSK_Lab), Mean_Val),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Somatic_Focus<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Somatic_Focus", "Somatic Focus", Mean_Val_Somatic_Focus),
      h6(paste("Reference:", Source_Mean_Somatic_Focus))
    )
  })
  outputOptions(output, "Mean_Widg_Somatic_Focus", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Activity_Avoidance<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Activity_Avoidance", paste0(AA_Lab), Mean_Val_Activity_Avoidance),
      h6(paste("Reference:", Source_Mean_Activity_Avoidance))
    )
  })
  outputOptions(output, "Mean_Widg_Activity_Avoidance", suspendWhenHidden = FALSE)
  

  output$Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd",  paste0(TSK_Lab), Sd_Val),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Somatic_Focus<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Somatic_Focus", "Somatic Focus", Sd_Val_Somatic_Focus),
      h6(paste("Reference:", Source_Sd_Somatic_Focus))
    )
  })
  outputOptions(output, "Sd_Widg_Somatic_Focus", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Activity_Avoidance<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Activity_Avoidance", paste0(AA_Lab), Sd_Val_Activity_Avoidance),
      h6(paste("Reference:", Source_Sd_Activity_Avoidance))
    )
  })
  outputOptions(output, "Sd_Widg_Activity_Avoidance", suspendWhenHidden = FALSE)
  
  
  output$Retest_M_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
          numericInput("Retest_Mean", paste0(TSK_Lab), value = 0)
    )
  })
  outputOptions(output, "Retest_M_Widg", suspendWhenHidden = FALSE)
  
  
  output$Retest_Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Retest_Sd", paste0(TSK_Lab), value = 0)
    )
  })
  outputOptions(output, "Retest_Sd_Widg", suspendWhenHidden = FALSE)
  
  
  output$Retest_M_AA_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Retest_Mean", paste0(AA_Lab), value = 0)
    )
  })
  
  
  output$Retest_Sd_AA_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Retest_Sd", paste0(AA_Lab), value = 0)
    )
  })
  outputOptions(output, "Retest_Sd_AA_Widg", suspendWhenHidden = FALSE)
  
  
 output$Retest_Widg<- renderUI({ 
   CI_Vals_Reac()
   tagList(
   fluidRow(
     column(width = 2,
            numericInput("Reliability", paste0(TSK_Lab), value = .81),
            h6("Woby, Roach, Urmston & Watson (2005)")
     ),
     column(width = 2,
            numericInput("Reliability_Somatic_Focus", "Somatic Focus", value = .76),
            h6("Roelofs et al. (2011). *Value represents internal consistency of scale in Upper Extremity Disorders subsample.")
     ),
     column(width = 5,
            numericInput("Reliability_Activity_Avoidance", paste0(AA_Lab), value = .67),
            h6("Roelofs et al. (2011). *Value represents internal consistency of scale in Upper Extremity Disorders subsample.")
     )
   )
   )
   })
 outputOptions(output, "Retest_Widg", suspendWhenHidden = FALSE)
  

  output$Cutoff_Widg_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_1",  paste0(TSK_Lab), as.numeric(Cut_Val_1)),
      textInput("Cutoff_Text_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source_Cutoff_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Somatic_Focus_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Somatic_Focus_1", "Somatic Focus", as.numeric(Cut_Val_Somatic_Focus_1)),
      textInput("Cutoff_Text_Somatic_Focus_1", "Cut-Off Score Name", Cut_Lab_1_Somatic_Focus),
      h6(paste("Reference:", Source_Cutoff_Somatic_Focus_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Somatic_Focus_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Activity_Avoidance_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Activity_Avoidance_1", "Activity Avoidance", as.numeric(Cut_Val_Activity_Avoidance_1)),
      textInput("Cutoff_Text_Activity_Avoidance_1", "Cut-Off Score Name", Cut_Lab_1_Activity_Avoidance),
      h6(paste("Reference:", Source_Cutoff_Activity_Avoidance_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Activity_Avoidance_1", suspendWhenHidden = FALSE)

  
  output$Cutoff_Widg_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_2",  paste0(TSK_Lab), as.numeric(Cut_Val_2)),
      textInput("Cutoff_Text_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source_Cutoff_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Somatic_Focus_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Somatic_Focus_2", "Somatic Focus", as.numeric(Cut_Val_Somatic_Focus_2)),
      textInput("Cutoff_Text_Somatic_Focus_2", "Cut-Off Score Name", Cut_Lab_2_Somatic_Focus),
      h6(paste("Reference:", Source_Cutoff_Somatic_Focus_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Somatic_Focus_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Activity_Avoidance_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Activity_Avoidance_2", "Activity Avoidance", as.numeric(Cut_Val_Activity_Avoidance_2)),
      textInput("Cutoff_Text_Activity_Avoidance_2", "Cut-Off Score Name", Cut_Lab_2_Activity_Avoidance),
      h6(paste("Reference:", Source_Cutoff_Activity_Avoidance_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Activity_Avoidance_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_3", paste0(TSK_Lab), as.numeric(Cut_Val_3)),
      textInput("Cutoff_Text_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source_Cutoff_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Somatic_Focus_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Somatic_Focus_3", "Somatic Focus", as.numeric(Cut_Val_Somatic_Focus_3)),
      textInput("Cutoff_Text_Somatic_Focus_3", "Cut-Off Score Name", Cut_Lab_3_Somatic_Focus),
      h6(paste("Reference:", Source_Cutoff_Somatic_Focus_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Somatic_Focus_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Activity_Avoidance_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Activity_Avoidance_3", "Activity Avoidance", as.numeric(Cut_Val_Activity_Avoidance_3)),
      textInput("Cutoff_Text_Activity_Avoidance_3", "Cut-Off Score Name", Cut_Lab_3_Activity_Avoidance),
      h6(paste("Reference:", Source_Cutoff_Activity_Avoidance_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Activity_Avoidance_3", suspendWhenHidden = FALSE)
  

  #Create a reactive expression defining entered values  
  
  Entered_Scores_Reac<- reactive({
    
    TP<- input$Timepoint
    
    PN<<- input$PatientName
    CN<<- input$ClinicianName
    
    if(input$Version == "13 items (TSK-13)") {
    Pop<- input$Pop_13
    } else if(input$Version == "11 items (TSK-11)/17 items (TSK-17)") {
    Pop<- input$Pop_11
    }
    
    RelChangeMethod<- input$RelChangeMethod
    
    CI_Vals_Reac()
    
    M<- input$Pop_Mean
    SD<- input$Pop_Sd
    M_Somatic_Focus<- input$Pop_Mean_Somatic_Focus
    SD_Somatic_Focus<-input$Pop_Sd_Somatic_Focus
    M_Activity_Avoidance<- input$Pop_Mean_Activity_Avoidance
    SD_Activity_Avoidance<- input$Pop_Sd_Activity_Avoidance
    
    
    M_Retest<- input$Retest_Mean
    SD_Retest<- input$Retest_Sd
    M_Retest_Somatic_Focus<- input$Retest_Mean_Somatic_Focus
    SD_Retest_Somatic_Focus<- input$Retest_Sd_Somatic_Focus
    M_Retest_Activity_Avoidance<- input$Retest_Mean_Activity_Avoidance
    SD_Retest_Activity_Avoidance<- input$Retest_Sd_Activity_Avoidance
  
    
    SampleN<- input$SampleN
    
    Rel<- input$Reliability
    Rel_Somatic_Focus<- input$Reliability_Somatic_Focus
    Rel_Activity_Avoidance<- input$Reliability_Activity_Avoidance
   
    Tab_Reference<<- Source_Mean
    
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
      SE_Somatic_Focus<-SD_Somatic_Focus * sqrt(1 - Rel_Somatic_Focus^2)
      SE_Activity_Avoidance<-SD_Activity_Avoidance * sqrt(1 - Rel_Activity_Avoidance^2)
      SE<- round(SE, digits = 2)
      SE_Somatic_Focus<- round(SE_Somatic_Focus, digits = 2)
      SE_Activity_Avoidance<- round(SE_Activity_Avoidance, digits = 2)
    } else if (input$RelChangeMethod == "Chelune et al. (1993)" | input$RelChangeMethod == "Jacobson & Truax (1991)" | input$RelChangeMethod == "Speer (1992)") {
      SE<- sqrt((2*(SD^2))*(1-Rel))
      SE_Somatic_Focus<- sqrt((2*(SD_Somatic_Focus^2))*(1-Rel_Somatic_Focus))
      SE_Activity_Avoidance<- sqrt((2*(SD_Activity_Avoidance^2))*(1-Rel_Activity_Avoidance))
      SE<- round(SE, digits = 2)
      SE_Somatic_Focus<- round(SE_Somatic_Focus, digits = 2)
      SE_Activity_Avoidance<- round(SE_Activity_Avoidance, digits = 2)
    } else if (input$RelChangeMethod == "Maassen et al. (2006)") {
      SE<- sqrt((SD^2 + SD_Retest^2)*(1-Rel))
      SE_Somatic_Focus<- sqrt((SD_Somatic_Focus^2 + SD_Retest_Somatic_Focus^2)*(1-Rel_Somatic_Focus))
      SE_Activity_Avoidance<- sqrt((SD_Activity_Avoidance^2 + SD_Retest_Activity_Avoidance^2)*(1-Rel_Activity_Avoidance))
      SE<- round(SE, digits = 2)
      SE_Somatic_Focus<- round(SE_Somatic_Focus, digits = 2)
      SE_Activity_Avoidance<- round(SE_Activity_Avoidance, digits = 2)
    } else if (input$RelChangeMethod == "McSweeny et al. (1993)") {
      SE<- SD_Retest*sqrt(1 - Rel^2)
      SE_Somatic_Focus<- SD_Retest_Somatic_Focus*sqrt(1 - Rel_Somatic_Focus^2)
      SE_Activity_Avoidance<- SD_Retest_Activity_Avoidance*sqrt(1 - Rel_Activity_Avoidance^2)
      SE<- round(SE, digits = 2)
      SE_Somatic_Focus<- round(SE_Somatic_Focus, digits = 2)
      SE_Activity_Avoidance<- round(SE_Activity_Avoidance, digits = 2)
    }
    
    
    #Caclculate McSweeny standard error to be employed in deriving the Crawford & Howell standard error if selected
    
    McSweeny_SE<- SD_Retest*sqrt(1 - Rel^2)
    McSweeny_SE_Somatic_Focus<- SD_Retest_Somatic_Focus*sqrt(1 - Rel_Somatic_Focus^2)
    McSweeny_SE_Activity_Avoidance<- SD_Retest_Activity_Avoidance*sqrt(1 - Rel_Activity_Avoidance^2)

    #Generate object storing names of cut-off scores
    
    Cutoff_Name_1<- input$Cutoff_Text_1
    Cutoff_Name_2<- input$Cutoff_Text_2
    Cutoff_Name_3<- input$Cutoff_Text_3
    Cutoff_Name_Somatic_Focus_1<- input$Cutoff_Text_Somatic_Focus_1
    Cutoff_Name_Somatic_Focus_2<- input$Cutoff_Text_Somatic_Focus_2
    Cutoff_Name_Somatic_Focus_3<- input$Cutoff_Text_Somatic_Focus_3
    Cutoff_Name_Activity_Avoidance_1<- input$Cutoff_Text_Activity_Avoidance_1
    Cutoff_Name_Activity_Avoidance_2<- input$Cutoff_Text_Activity_Avoidance_2
    Cutoff_Name_Activity_Avoidance_3<- input$Cutoff_Text_Activity_Avoidance_3
    Cutoff_Names<<- data.frame(Cutoff_Name_1,Cutoff_Name_2,Cutoff_Name_3,Cutoff_Name_Somatic_Focus_1,Cutoff_Name_Somatic_Focus_2,Cutoff_Name_Somatic_Focus_3,
                               Cutoff_Name_Activity_Avoidance_1, Cutoff_Name_Activity_Avoidance_2, Cutoff_Name_Activity_Avoidance_3)
    
    #Define the data & calculate PS, SE, confidence intervals and cut-off scores
    
    if(input$Timepoint == "1") {
      Date<- input$Date_1
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Recode<- car::recode(Score_1a[c(4,8,12,16)],'1=4; 2=3; 3=2; 4=1')
      Score_1a[c(4,8,12,16)]<- Recode
      if(input$Version == "11 items (TSK-11)/17 items (TSK-17)") {
      Item_Df<<- data.frame(Item = 1:length(Score_1a[c(-4,-8,-12,-16,-9,-14)]), Score = Score_1a[c(-4,-8,-12,-16,-9,-14)])
      Score<- sum(Score_1a, na.rm = TRUE)
      Score_Somatic_Focus<- sum(Score_1a[c(3,5,6,7,11)], na.rm = TRUE)
      Score_Activity_Avoidance<- sum(Score_1a[c(1,2,10,13,15,17)], na.rm = TRUE)
      } 
      if(input$Version == "13 items (TSK-13)") {
      Item_Df<<- data.frame(Item = 1:length(Score_1a[c(-4,-8,-12,-16)]), Score = Score_1a[c(-4,-8,-12,-16)])
      Score<- sum(Score_1a, na.rm = TRUE)
      Score_Somatic_Focus<- sum(Score_1a[c(3,5,6,7,11)], na.rm = TRUE)
      Score_Activity_Avoidance<- sum(Score_1a[c(1,2,9,10,13,14,15,17)], na.rm = TRUE)
      }
      Change<- 0
      Change_Somatic_Focus<- 0
      Change_Activity_Avoidance<- 0
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS<- (Rel * Score) + (M * (1 - Rel))
        PTS_Somatic_Focus<- (Rel_Somatic_Focus * Score_Somatic_Focus) + (M_Somatic_Focus * (1 - Rel_Somatic_Focus))
        PTS_Activity_Avoidance<- (Rel_Activity_Avoidance * Score_Activity_Avoidance) + (M_Activity_Avoidance * (1 - Rel_Activity_Avoidance))
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS<- Score + (M_Retest - M)  
        PTS_Somatic_Focus<- Score_Somatic_Focus + (M_Retest_Somatic_Focus - M_Somatic_Focus)  
        PTS_Activity_Avoidance<- Score_Activity_Avoidance + (M_Retest_Activity_Avoidance - M_Activity_Avoidance)  
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Somatic_Focus<- Score_Somatic_Focus
        PTS_Activity_Avoidance<- Score_Activity_Avoidance
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS<- (B_Adj * Score) + A_Adj
        B_Slope_Somatic_Focus<- Rel_Somatic_Focus * (SD_Retest_Somatic_Focus/SD_Somatic_Focus)
        A_Constant_Somatic_Focus<- M_Retest_Somatic_Focus - (B_Slope_Somatic_Focus * M_Somatic_Focus)
        B_Adj_Somatic_Focus<- SD_Retest_Somatic_Focus/SD_Somatic_Focus
        A_Adj_Somatic_Focus<- M_Retest_Somatic_Focus - (B_Adj_Somatic_Focus * M_Somatic_Focus)
        PTS_Somatic_Focus<- (B_Adj_Somatic_Focus * Score_Somatic_Focus) + A_Adj_Somatic_Focus
        B_Slope_Activity_Avoidance<- Rel_Activity_Avoidance * (SD_Retest_Activity_Avoidance/SD_Activity_Avoidance)
        A_Constant_Activity_Avoidance<- M_Retest_Activity_Avoidance - (B_Slope_Activity_Avoidance * M_Activity_Avoidance)
        B_Adj_Activity_Avoidance<- SD_Retest_Activity_Avoidance/SD_Activity_Avoidance
        A_Adj_Activity_Avoidance<- M_Retest_Activity_Avoidance - (B_Adj_Activity_Avoidance * M_Activity_Avoidance)
        PTS_Activity_Avoidance<- (B_Adj_Activity_Avoidance * Score_Activity_Avoidance) + A_Adj_Activity_Avoidance
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS<- B_Slope * Score
        B_Slope_Somatic_Focus<- Rel_Somatic_Focus * (SD_Retest_Somatic_Focus/SD_Somatic_Focus)
        PTS_Somatic_Focus<- B_Slope_Somatic_Focus * Score_Somatic_Focus
        B_Slope_Activity_Avoidance<- Rel_Activity_Avoidance * (SD_Retest_Activity_Avoidance/SD_Activity_Avoidance)
        PTS_Activity_Avoidance<- B_Slope_Activity_Avoidance * Score_Activity_Avoidance
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS<- Score + (M_Retest - M)
        PTS_Somatic_Focus<- Score_Somatic_Focus + (M_Retest_Somatic_Focus - M_Somatic_Focus)
        PTS<- Activity_Avoidance<- Score_Activity_Avoidance + (M_Retest_Activity_Avoidance - M_Activity_Avoidance)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Somatic_Focus<- round(PTS_Somatic_Focus, digits = 2)
      PTS_Activity_Avoidance<- round(PTS_Activity_Avoidance, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score - M)^2/(SD^2*(SampleN-1))))
        SE_Somatic_Focus<- McSweeny_SE_Somatic_Focus*sqrt(1 + (1/SampleN) + ((Score_Somatic_Focus - M_Somatic_Focus)^2/(SD_Somatic_Focus^2*(SampleN-1))))
        SE_Activity_Avoidance<- McSweeny_SE_Activity_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Activity_Avoidance - M_Activity_Avoidance)^2/(SD_Activity_Avoidance^2*(SampleN-1))))
        SE<- round(SE, digits = 2)
        SE_Somatic_Focus<- round(SE_Somatic_Focus, digits = 2)
        SE_Activity_Avoidance<- round(SE_Activity_Avoidance, digits = 2)
        CI<- (Conf*SE)
        CI<- round(CI, digits = 2)
        CI_Somatic_Focus<- (Conf*SE_Somatic_Focus)
        CI_Somatic_Focus<- round(CI_Somatic_Focus, digits = 2)
        CI_Activity_Avoidance<- (Conf*SE_Activity_Avoidance)
        CI_Activity_Avoidance<- round(CI_Activity_Avoidance, digits = 2)
      }
      CI<- (Conf*SE)
      CI<- round(CI, digits = 2)
      CI_Somatic_Focus<- (Conf*SE_Somatic_Focus)
      CI_Somatic_Focus<- round(CI_Somatic_Focus, digits = 2)
      CI_Activity_Avoidance<- (Conf*SE_Activity_Avoidance)
      CI_Activity_Avoidance<- round(CI_Activity_Avoidance, digits = 2)
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Somatic_Focus<- PTS_Somatic_Focus + CI_Somatic_Focus
      CI_Upper_Lim_Somatic_Focus<- round(CI_Upper_Lim_Somatic_Focus, digits = 2)
      CI_Lower_Lim_Somatic_Focus<-PTS_Somatic_Focus - CI_Somatic_Focus
      CI_Lower_Lim_Somatic_Focus<- round(CI_Lower_Lim_Somatic_Focus, digits = 2)
      CI_Upper_Lim_Activity_Avoidance<- PTS_Activity_Avoidance + CI_Activity_Avoidance
      CI_Upper_Lim_Activity_Avoidance<- round(CI_Upper_Lim_Activity_Avoidance, digits = 2)
      CI_Lower_Lim_Activity_Avoidance<-PTS_Activity_Avoidance - CI_Activity_Avoidance
      CI_Lower_Lim_Activity_Avoidance<- round(CI_Lower_Lim_Activity_Avoidance, digits = 2)
      if(input$Select_CI == "2") {
        CI<- input$Man_CI
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Somatic_Focus == "2") {
        CI_Somatic_Focus<- input$Man_CI_Somatic_Focus
        CI_Somatic_Focus<- round(CI_Somatic_Focus, digits = 2)
        CI_Upper_Lim_Somatic_Focus<- Score_Somatic_Focus + CI_Somatic_Focus
        CI_Upper_Lim_Somatic_Focus<- round(CI_Upper_Lim_Somatic_Focus, digits = 2)
        CI_Lower_Lim_Somatic_Focus<- Score_Somatic_Focus - CI_Somatic_Focus
        CI_Lower_Lim_Somatic_Focus<- round(CI_Lower_Lim_Somatic_Focus, digits = 2)
      }
      if(input$Select_CI_Activity_Avoidance == "2") {
        CI_Activity_Avoidance<- input$Man_CI_Activity_Avoidance
        CI_Activity_Avoidance<- round(CI_Activity_Avoidance, digits = 2)
        CI_Upper_Lim_Activity_Avoidance<- Score_Activity_Avoidance + CI_Activity_Avoidance
        CI_Upper_Lim_Activity_Avoidance<- round(CI_Upper_Lim_Activity_Avoidance, digits = 2)
        CI_Lower_Lim_Activity_Avoidance<- Score_Activity_Avoidance - CI_Activity_Avoidance
        CI_Lower_Lim_Activity_Avoidance<- round(CI_Lower_Lim_Activity_Avoidance, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Somatic_Focus_1<- round(input$Cutoff_Somatic_Focus_1, digits = 2)
      Cutoff_Score_Somatic_Focus_2<- round(input$Cutoff_Somatic_Focus_2, digits = 2)
      Cutoff_Score_Somatic_Focus_3<- round(input$Cutoff_Somatic_Focus_3, digits = 2)
      Cutoff_Score_Activity_Avoidance_1<- round(input$Cutoff_Activity_Avoidance_1, digits = 2)
      Cutoff_Score_Activity_Avoidance_2<- round(input$Cutoff_Activity_Avoidance_2, digits = 2)
      Cutoff_Score_Activity_Avoidance_3<- round(input$Cutoff_Activity_Avoidance_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Somatic_Focus,Change_Somatic_Focus,PTS_Somatic_Focus, SE_Somatic_Focus, CI_Upper_Lim_Somatic_Focus, CI_Lower_Lim_Somatic_Focus, Cutoff_Score_Somatic_Focus_1,Cutoff_Score_Somatic_Focus_2,Cutoff_Score_Somatic_Focus_3,
                                      Score_Activity_Avoidance,Change_Activity_Avoidance, PTS_Activity_Avoidance, SE_Activity_Avoidance, CI_Upper_Lim_Activity_Avoidance, CI_Lower_Lim_Activity_Avoidance, Cutoff_Score_Activity_Avoidance_1,Cutoff_Score_Activity_Avoidance_2,Cutoff_Score_Activity_Avoidance_3)
    } else if(input$Timepoint == "2") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date<- c(Date_1, Date_2)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<- as.numeric(unlist(strsplit(input$Text_1,",")))
      Recode_1<- car::recode(Score_1a[c(4,8,12,16)],'1=4; 2=3; 3=2; 4=1')
      Score_1a[c(4,8,12,16)]<- Recode_1
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Recode_2<- car::recode(Score_2a[c(4,8,12,16)],'1=4; 2=3; 3=2; 4=1')
      Score_2a[c(4,8,12,16)]<- Recode_2
      if(input$Version == "11 items (TSK-11)/17 items (TSK-17)") {
        Score_1<- sum(Score_1a, na.rm = TRUE)
        Score_Somatic_Focus_1<- sum(Score_1a[c(3,5,6,7,11)], na.rm = TRUE)
        Score_Activity_Avoidance_1<- sum(Score_1a[c(1,2,10,13,15,17)], na.rm = TRUE)
        Item_Df<<- data.frame(Item = 1:length(Score_2a[c(-4,-8,-12,-16,-9,-14)]), Score = Score_2a[c(-4,-8,-12,-16,-9,-14)])
        Score_2<- sum(Score_2a, na.rm = TRUE)
        Score_Somatic_Focus_2<- sum(Score_2a[c(3,5,6,7,11)], na.rm = TRUE)
        Score_Activity_Avoidance_2<- sum(Score_2a[c(1,2,10,13,15,17)], na.rm = TRUE)
      } else if(input$Version == "13 items (TSK-13)") {
        Score_1b<<- Score_1a[c(-4,-8,-12,-16)]
        Score_1<- sum(Score_1b, na.rm = TRUE)
        Score_Somatic_Focus_1<- sum(Score_1a[c(3,5,6,7,11)], na.rm = TRUE)
        Score_Activity_Avoidance_1<- sum(Score_1a[c(1,2,9,10,13,14,15,17)], na.rm = TRUE)
        Score_2b<<- Score_2a[c(-4,-8,-12,-16)]
        Item_Df<<- data.frame(Item = 1:length(Score_2a[c(-4,-8,-12,-16)]), Score = Score_2b)
        Score_2<- sum(Score_2b, na.rm = TRUE)
        Score_Somatic_Focus_2<- sum(Score_2a[c(3,5,6,7,11)], na.rm = TRUE)
        Score_Activity_Avoidance_2<- sum(Score_2a[c(1,2,9,10,13,14,15,17)], na.rm = TRUE)
      }
      Score<- c(Score_1, Score_2)
      Score_Somatic_Focus<- c(Score_Somatic_Focus_1,Score_Somatic_Focus_2)
      Score_Activity_Avoidance<- c(Score_Activity_Avoidance_1,Score_Activity_Avoidance_2)
      Change<- c(0, (Score_2 - Score_1))
      Change_Somatic_Focus<- c(0, (Score_Somatic_Focus_2 - Score_Somatic_Focus_1))
      Change_Activity_Avoidance<- c(0, (Score_Activity_Avoidance_2 - Score_Activity_Avoidance_1))
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2)
        PTS_Somatic_Focus_1<- (Rel_Somatic_Focus * Score_Somatic_Focus_1) + (M_Somatic_Focus * (1 - Rel_Somatic_Focus))
        PTS_Somatic_Focus_2<- (Rel_Somatic_Focus * Score_Somatic_Focus_2) + (M_Somatic_Focus * (1 - Rel_Somatic_Focus))
        PTS_Somatic_Focus<-c(PTS_Somatic_Focus_1, PTS_Somatic_Focus_2)
        PTS_Activity_Avoidance_1<- (Rel_Activity_Avoidance * Score_Activity_Avoidance_1) + (M_Activity_Avoidance * (1 - Rel_Activity_Avoidance))
        PTS_Activity_Avoidance_2<- (Rel_Activity_Avoidance * Score_Activity_Avoidance_2) + (M_Activity_Avoidance * (1 - Rel_Activity_Avoidance))
        PTS_Activity_Avoidance<-c(PTS_Activity_Avoidance_1,PTS_Activity_Avoidance_2)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2)
        PTS_Somatic_Focus_1<- Score_Somatic_Focus_1 + (M_Retest_Somatic_Focus - M_Somatic_Focus)  
        PTS_Somatic_Focus_2<- Score_Somatic_Focus_2 + (M_Retest_Somatic_Focus - M_Somatic_Focus)
        PTS_Somatic_Focus<- c(PTS_Somatic_Focus_1, PTS_Somatic_Focus_2)
        PTS_Activity_Avoidance_1<- Score_Activity_Avoidance_1 + (M_Retest_Activity_Avoidance - M_Activity_Avoidance)  
        PTS_Activity_Avoidance_2<- Score_Activity_Avoidance_2 + (M_Retest_Activity_Avoidance - M_Activity_Avoidance)
        PTS_Activity_Avoidance<- c(PTS_Activity_Avoidance_1, PTS_Activity_Avoidance_2)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS<- c(PTS_1,PTS_2)
        PTS_Somatic_Focus_1<- Score_Somatic_Focus_1
        PTS_Somatic_Focus_2<- Score_Somatic_Focus_2
        PTS_Somatic_Focus<- c(PTS_Somatic_Focus_1, PTS_Somatic_Focus_2)
        PTS_Activity_Avoidance_1<- Score_Activity_Avoidance_1
        PTS_Activity_Avoidance_2<- Score_Activity_Avoidance_2
        PTS_Activity_Avoidance<- c(PTS_Activity_Avoidance_1, PTS_Activity_Avoidance_2)
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Somatic_Focus<- Rel_Somatic_Focus * (SD_Retest_Somatic_Focus/SD_Somatic_Focus)
        A_Constant_Somatic_Focus<- M_Retest_Somatic_Focus - (B_Slope_Somatic_Focus * M_Somatic_Focus)
        B_Adj_Somatic_Focus<- SD_Retest_Somatic_Focus/SD_Somatic_Focus
        A_Adj_Somatic_Focus<- M_Retest_Somatic_Focus - (B_Adj_Somatic_Focus * M_Somatic_Focus)
        PTS_Somatic_Focus_1<- (B_Adj_Somatic_Focus * Score_Somatic_Focus_1) + A_Adj_Somatic_Focus
        PTS_Somatic_Focus_2<- (B_Adj_Somatic_Focus * Score_Somatic_Focus_2) + A_Adj_Somatic_Focus
        PTS_Somatic_Focus<- c(PTS_Somatic_Focus_1,PTS_Somatic_Focus_2)
        B_Slope_Activity_Avoidance<- Rel_Activity_Avoidance * (SD_Retest_Activity_Avoidance/SD_Activity_Avoidance)
        A_Constant_Activity_Avoidance<- M_Retest_Activity_Avoidance - (B_Slope_Activity_Avoidance * M_Activity_Avoidance)
        B_Adj_Activity_Avoidance<- SD_Retest_Activity_Avoidance/SD_Activity_Avoidance
        A_Adj_Activity_Avoidance<- M_Retest_Activity_Avoidance - (B_Adj_Activity_Avoidance * M_Activity_Avoidance)
        PTS_Activity_Avoidance_1<- (B_Adj_Activity_Avoidance * Score_Activity_Avoidance_1) + A_Adj_Activity_Avoidance
        PTS_Activity_Avoidance_2<- (B_Adj_Activity_Avoidance * Score_Activity_Avoidance_2) + A_Adj_Activity_Avoidance
        PTS_Activity_Avoidance<- c(PTS_Activity_Avoidance_1,PTS_Activity_Avoidance_2)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Somatic_Focus<- Rel_Somatic_Focus * (SD_Retest_Somatic_Focus/SD_Somatic_Focus)
        PTS_Somatic_Focus_1<- B_Slope_Somatic_Focus * Score_Somatic_Focus_1
        PTS_Somatic_Focus_2<- B_Slope_Somatic_Focus * Score_Somatic_Focus_2
        PTS_Somatic_Focus<- c(PTS_Somatic_Focus_1, PTS_Somatic_Focus_2)
        B_Slope_Activity_Avoidance<- Rel_Activity_Avoidance * (SD_Retest_Activity_Avoidance/SD_Activity_Avoidance)
        PTS_Activity_Avoidance_1<- B_Slope_Activity_Avoidance * Score_Activity_Avoidance_1
        PTS_Activity_Avoidance_2<- B_Slope_Activity_Avoidance * Score_Activity_Avoidance_2
        PTS_Activity_Avoidance<- c(PTS_Activity_Avoidance_1, PTS_Activity_Avoidance_2)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2)
        PTS_Somatic_Focus_1<- Score_Somatic_Focus_1 + (M_Retest_Somatic_Focus - M_Somatic_Focus)
        PTS_Somatic_Focus_2<- Score_Somatic_Focus_2 + (M_Retest_Somatic_Focus - M_Somatic_Focus)
        PTS_Somatic_Focus<- c(PTS_Somatic_Focus_1, PTS_Somatic_Focus_2)
        PTS_Activity_Avoidance_1<- Score_Activity_Avoidance_1 + (M_Retest_Activity_Avoidance - M_Activity_Avoidance)
        PTS_Activity_Avoidance_2<- Score_Activity_Avoidance_2 + (M_Retest_Activity_Avoidance - M_Activity_Avoidance)
        PTS_Activity_Avoidance<- c(PTS_Activity_Avoidance_1, PTS_Activity_Avoidance_2)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Somatic_Focus<- round(PTS_Somatic_Focus, digits = 2)
      PTS_Activity_Avoidance<- round(PTS_Activity_Avoidance, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2)
        SE_Somatic_Focus_1<- McSweeny_SE_Somatic_Focus*sqrt(1 + (1/SampleN) + ((Score_Somatic_Focus_1 - M_Somatic_Focus)^2/(SD_Somatic_Focus^2*(SampleN-1))))
        SE_Somatic_Focus_2<- McSweeny_SE_Somatic_Focus*sqrt(1 + (1/SampleN) + ((Score_Somatic_Focus_2 - M_Somatic_Focus)^2/(SD_Somatic_Focus^2*(SampleN-1))))
        SE_Somatic_Focus<- c(SE_Somatic_Focus_1, SE_Somatic_Focus_2)
        SE_Activity_Avoidance_1<- McSweeny_SE_Activity_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Activity_Avoidance_1 - M_Activity_Avoidance)^2/(SD_Activity_Avoidance^2*(SampleN-1))))
        SE_Activity_Avoidance_2<- McSweeny_SE_Activity_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Activity_Avoidance_2 - M_Activity_Avoidance)^2/(SD_Activity_Avoidance^2*(SampleN-1))))
        SE_Activity_Avoidance<-c(SE_Activity_Avoidance_1, SE_Activity_Avoidance_2)
        SE<- round(SE, digits = 2)
        SE_Somatic_Focus<- round(SE_Somatic_Focus, digits = 2)
        SE_Activity_Avoidance<- round(SE_Activity_Avoidance, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2))
        CI<- round(CI, digits = 2)
        CI_Somatic_Focus<- c((Conf*SE_Somatic_Focus_1), (Conf*SE_Somatic_Focus_2))
        CI_Somatic_Focus<- round(CI_Somatic_Focus, digits = 2)
        CI_Activity_Avoidance<- c((Conf*SE_Activity_Avoidance_1), (Conf*SE_Activity_Avoidance_2))
        CI_Activity_Avoidance<- round(CI_Activity_Avoidance, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Somatic_Focus<- c((Conf*SE_Somatic_Focus), (Conf*SE_Somatic_Focus))
        CI_Somatic_Focus<- round(CI_Somatic_Focus, digits = 2)
        CI_Activity_Avoidance<- c((Conf*SE_Activity_Avoidance), (Conf*SE_Activity_Avoidance))
        CI_Activity_Avoidance<- round(CI_Activity_Avoidance, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Somatic_Focus<- PTS_Somatic_Focus + CI_Somatic_Focus
      CI_Upper_Lim_Somatic_Focus<- round(CI_Upper_Lim_Somatic_Focus, digits = 2)
      CI_Lower_Lim_Somatic_Focus<-PTS_Somatic_Focus - CI_Somatic_Focus
      CI_Lower_Lim_Somatic_Focus<- round(CI_Lower_Lim_Somatic_Focus, digits = 2)
      CI_Upper_Lim_Activity_Avoidance<- PTS_Activity_Avoidance + CI_Activity_Avoidance
      CI_Upper_Lim_Activity_Avoidance<- round(CI_Upper_Lim_Activity_Avoidance, digits = 2)
      CI_Lower_Lim_Activity_Avoidance<-PTS_Activity_Avoidance - CI_Activity_Avoidance
      CI_Lower_Lim_Activity_Avoidance<- round(CI_Lower_Lim_Activity_Avoidance, digits = 2)
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Somatic_Focus == "2") {
        CI_Somatic_Focus<- input$Man_CI_Somatic_Focus
        CI_Somatic_Focus<- c(CI_Somatic_Focus, CI_Somatic_Focus)
        CI_Somatic_Focus<- round(CI_Somatic_Focus, digits = 2)
        CI_Upper_Lim_Somatic_Focus<- Score_Somatic_Focus + CI_Somatic_Focus
        CI_Upper_Lim_Somatic_Focus<- round(CI_Upper_Lim_Somatic_Focus, digits = 2)
        CI_Lower_Lim_Somatic_Focus<- Score_Somatic_Focus - CI_Somatic_Focus
        CI_Lower_Lim_Somatic_Focus<- round(CI_Lower_Lim_Somatic_Focus, digits = 2)
      }
      if(input$Select_CI_Activity_Avoidance == "2") {
        CI_Activity_Avoidance<- input$Man_CI_Activity_Avoidance
        CI_Activity_Avoidance<- c(CI_Activity_Avoidance, CI_Activity_Avoidance)
        CI_Activity_Avoidance<- round(CI_Activity_Avoidance, digits = 2)
        CI_Upper_Lim_Activity_Avoidance<- Score_Activity_Avoidance + CI_Activity_Avoidance
        CI_Upper_Lim_Activity_Avoidance<- round(CI_Upper_Lim_Activity_Avoidance, digits = 2)
        CI_Lower_Lim_Activity_Avoidance<- Score_Activity_Avoidance - CI_Activity_Avoidance
        CI_Lower_Lim_Activity_Avoidance<- round(CI_Lower_Lim_Activity_Avoidance, digits = 2)
      } 
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_1<- rep(Cutoff_Score_1, 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_2<- rep(Cutoff_Score_2, 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_3<- rep(Cutoff_Score_3, 2)
      Cutoff_Score_Somatic_Focus_1<- round(input$Cutoff_Somatic_Focus_1, digits = 2)
      Cutoff_Score_Somatic_Focus_1<- rep(Cutoff_Score_Somatic_Focus_1, 2)
      Cutoff_Score_Somatic_Focus_2<- round(input$Cutoff_Somatic_Focus_2, digits = 2)
      Cutoff_Score_Somatic_Focus_2<- rep(Cutoff_Score_Somatic_Focus_2, 2)
      Cutoff_Score_Somatic_Focus_3<- round(input$Cutoff_Somatic_Focus_3, digits = 2)
      Cutoff_Score_Somatic_Focus_3<- rep(Cutoff_Score_Somatic_Focus_3, 2)
      Cutoff_Score_Activity_Avoidance_1<- round(input$Cutoff_Activity_Avoidance_1, digits = 2)
      Cutoff_Score_Activity_Avoidance_1<- rep(Cutoff_Score_Activity_Avoidance_1, 2)
      Cutoff_Score_Activity_Avoidance_2<- round(input$Cutoff_Activity_Avoidance_2, digits = 2)
      Cutoff_Score_Activity_Avoidance_2<- rep(Cutoff_Score_Activity_Avoidance_2, 2)
      Cutoff_Score_Activity_Avoidance_3<- round(input$Cutoff_Activity_Avoidance_3, digits = 2)
      Cutoff_Score_Activity_Avoidance_3<- rep(Cutoff_Score_Activity_Avoidance_3, 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Somatic_Focus,Change_Somatic_Focus,PTS_Somatic_Focus, SE_Somatic_Focus, CI_Upper_Lim_Somatic_Focus, CI_Lower_Lim_Somatic_Focus, Cutoff_Score_Somatic_Focus_1,Cutoff_Score_Somatic_Focus_2,Cutoff_Score_Somatic_Focus_3,
                                      Score_Activity_Avoidance,Change_Activity_Avoidance, PTS_Activity_Avoidance, SE_Activity_Avoidance, CI_Upper_Lim_Activity_Avoidance, CI_Lower_Lim_Activity_Avoidance, Cutoff_Score_Activity_Avoidance_1,Cutoff_Score_Activity_Avoidance_2,Cutoff_Score_Activity_Avoidance_3)
    } else if(input$Timepoint == "3") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date_3<- input$Date_3
      Date<- c(Date_1, Date_2, Date_3)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<- as.numeric(unlist(strsplit(input$Text_1,",")))
      Recode_1<- car::recode(Score_1a[c(4,8,12,16)],'1=4; 2=3; 3=2; 4=1')
      Score_1a[c(4,8,12,16)]<- Recode_1
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Recode_2<- car::recode(Score_2a[c(4,8,12,16)],'1=4; 2=3; 3=2; 4=1')
      Score_2a[c(4,8,12,16)]<- Recode_2
      Score_3a<<- as.numeric(unlist(strsplit(input$Text_3,",")))
      Recode_3<- car::recode(Score_3a[c(4,8,12,16)],'1=4; 2=3; 3=2; 4=1')
      Score_3a[c(4,8,12,16)]<- Recode_3
      if(input$Version == "11 items (TSK-11)/17 items (TSK-17)") {
        Score_1<- sum(Score_1a, na.rm = TRUE)
        Score_Somatic_Focus_1<- sum(Score_1a[c(3,5,6,7,11)], na.rm = TRUE)
        Score_Activity_Avoidance_1<- sum(Score_1a[c(1,2,10,13,15,17)], na.rm = TRUE)
        Score_2<- sum(Score_2a, na.rm = TRUE)
        Score_Somatic_Focus_2<- sum(Score_2a[c(3,5,6,7,11)], na.rm = TRUE)
        Score_Activity_Avoidance_2<- sum(Score_2a[c(1,2,10,13,15,17)], na.rm = TRUE)
        Item_Df<<- data.frame(Item = 1:length(Score_3a[c(-4,-8,-12,-16,-9,-14)]), Score = Score_3a[c(-4,-8,-12,-16,-9,-14)])
        Score_3<- sum(Score_3a, na.rm = TRUE)
        Score_Somatic_Focus_3<- sum(Score_3a[c(3,5,6,7,11)], na.rm = TRUE)
        Score_Activity_Avoidance_3<- sum(Score_3a[c(1,2,10,13,15,17)], na.rm = TRUE)
      } else if(input$Version == "13 items (TSK-13)") {
        Score_1b<<- Score_1a[c(-4,-8,-12,-16)]
        Score_1<- sum(Score_1b, na.rm = TRUE)
        Score_Somatic_Focus_1<- sum(Score_1a[c(3,5,6,7,11)], na.rm = TRUE)
        Score_Activity_Avoidance_1<- sum(Score_1a[c(1,2,9,10,13,14,15,17)], na.rm = TRUE)
        Score_2b<<- Score_2a[c(-4,-8,-12,-16)]
        Score_2<- sum(Score_2b, na.rm = TRUE)
        Score_Somatic_Focus_2<- sum(Score_2a[c(3,5,6,7,11)], na.rm = TRUE)
        Score_Activity_Avoidance_2<- sum(Score_2a[c(1,2,9,10,13,14,15,17)], na.rm = TRUE)
        Score_3b<<- Score_3a[c(-4,-8,-12,-16)]
        Item_Df<<- data.frame(Item = 1:length(Score_3a[c(-4,-8,-12,-16)]), Score = Score_3b)
        Score_3<- sum(Score_3b, na.rm = TRUE)
        Score_3<- round(Score_3, digits = 2)
        Score_Somatic_Focus_3<- sum(Score_3a[c(3,5,6,7,11)], na.rm = TRUE)
        Score_Activity_Avoidance_3<- sum(Score_3a[c(1,2,9,10,13,14,15,17)], na.rm = TRUE)
      }
      Score<- c(Score_1, Score_2, Score_3)
      Score<- round(Score, digits = 2)
      Score_Somatic_Focus<- c(Score_Somatic_Focus_1,Score_Somatic_Focus_2, Score_Somatic_Focus_3)
      Score_Somatic_Focus<- round(Score_Somatic_Focus, digits = 2)
      Score_Activity_Avoidance<- c(Score_Activity_Avoidance_1,Score_Activity_Avoidance_2, Score_Activity_Avoidance_3)
      Score_Activity_Avoidance<- round(Score_Activity_Avoidance, digits = 2)
      Change<- c(0, (Score_2 - Score_1), (Score_3 - Score_2))
      Change<- round(Change, digits = 2)
      Change_Somatic_Focus<- c(0, Score_Somatic_Focus_2 - Score_Somatic_Focus_1, Score_Somatic_Focus_3 - Score_Somatic_Focus_2)
      Change_Somatic_Focus<- round(Change_Somatic_Focus, digits = 2)
      Change_Activity_Avoidance<- c(0, Score_Activity_Avoidance_2 - Score_Activity_Avoidance_1, Score_Activity_Avoidance_3 - Score_Activity_Avoidance_2)
      Change_Activity_Avoidance<- round(Change_Activity_Avoidance, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS_3<- (Rel * Score_3) + (M * (1 - Rel))
        PTS<<- c(PTS_1, PTS_2, PTS_3)
        PTS_Somatic_Focus_1<- (Rel_Somatic_Focus * Score_Somatic_Focus_1) + (M_Somatic_Focus * (1 - Rel_Somatic_Focus))
        PTS_Somatic_Focus_2<- (Rel_Somatic_Focus * Score_Somatic_Focus_2) + (M_Somatic_Focus * (1 - Rel_Somatic_Focus))
        PTS_Somatic_Focus_3<- (Rel_Somatic_Focus * Score_Somatic_Focus_3) + (M_Somatic_Focus * (1 - Rel_Somatic_Focus))
        PTS_Somatic_Focus<<- c(PTS_Somatic_Focus_1, PTS_Somatic_Focus_2, PTS_Somatic_Focus_3)
        PTS_Activity_Avoidance_1<- (Rel_Activity_Avoidance * Score_Activity_Avoidance_1) + (M_Activity_Avoidance * (1 - Rel_Activity_Avoidance))
        PTS_Activity_Avoidance_2<- (Rel_Activity_Avoidance * Score_Activity_Avoidance_2) + (M_Activity_Avoidance * (1 - Rel_Activity_Avoidance))
        PTS_Activity_Avoidance_3<- (Rel_Activity_Avoidance * Score_Activity_Avoidance_3) + (M_Activity_Avoidance * (1 - Rel_Activity_Avoidance))
        PTS_Activity_Avoidance<- c(PTS_Activity_Avoidance_1,PTS_Activity_Avoidance_2, PTS_Activity_Avoidance_3)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Somatic_Focus_1<- Score_Somatic_Focus_1 + (M_Retest_Somatic_Focus - M_Somatic_Focus)  
        PTS_Somatic_Focus_2<- Score_Somatic_Focus_2 + (M_Retest_Somatic_Focus - M_Somatic_Focus)
        PTS_Somatic_Focus_3<- Score_Somatic_Focus_3 + (M_Retest_Somatic_Focus - M_Somatic_Focus)
        PTS_Somatic_Focus<- c(PTS_Somatic_Focus_1, PTS_Somatic_Focus_2, PTS_Somatic_Focus_3)
        PTS_Activity_Avoidance_1<- Score_Activity_Avoidance_1 + (M_Retest_Activity_Avoidance - M_Activity_Avoidance)  
        PTS_Activity_Avoidance_2<- Score_Activity_Avoidance_2 + (M_Retest_Activity_Avoidance - M_Activity_Avoidance)
        PTS_Activity_Avoidance_3<- Score_Activity_Avoidance_3 + (M_Retest_Activity_Avoidance - M_Activity_Avoidance)
        PTS_Activity_Avoidance<- c(PTS_Activity_Avoidance_1, PTS_Activity_Avoidance_2, PTS_Activity_Avoidance_3)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS_3<- Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Somatic_Focus_1<- Score_Somatic_Focus_1
        PTS_Somatic_Focus_2<- Score_Somatic_Focus_2
        PTS_Somatic_Focus_3<- Score_Somatic_Focus_3
        PTS_Somatic_Focus<- c(PTS_Somatic_Focus_1, PTS_Somatic_Focus_2, PTS_Somatic_Focus_3)
        PTS_Activity_Avoidance_1<- Score_Activity_Avoidance_1
        PTS_Activity_Avoidance_2<- Score_Activity_Avoidance_2
        PTS_Activity_Avoidance_3<- Score_Activity_Avoidance_3
        PTS_Activity_Avoidance<- c(PTS_Activity_Avoidance_1, PTS_Activity_Avoidance_2, PTS_Activity_Avoidance_3)
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS_3<- (B_Adj * Score_3) + A_Adj
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Somatic_Focus<- Rel_Somatic_Focus * (SD_Retest_Somatic_Focus/SD_Somatic_Focus)
        A_Constant_Somatic_Focus<- M_Retest_Somatic_Focus - (B_Slope_Somatic_Focus * M_Somatic_Focus)
        B_Adj_Somatic_Focus<- SD_Retest_Somatic_Focus/SD_Somatic_Focus
        A_Adj_Somatic_Focus<- M_Retest_Somatic_Focus - (B_Adj_Somatic_Focus * M_Somatic_Focus)
        PTS_Somatic_Focus_1<- (B_Adj_Somatic_Focus * Score_Somatic_Focus_1) + A_Adj_Somatic_Focus
        PTS_Somatic_Focus_2<- (B_Adj_Somatic_Focus * Score_Somatic_Focus_2) + A_Adj_Somatic_Focus
        PTS_Somatic_Focus_3<- (B_Adj_Somatic_Focus * Score_Somatic_Focus_3) + A_Adj_Somatic_Focus
        PTS_Somatic_Focus<- c(PTS_Somatic_Focus_1, PTS_Somatic_Focus_2, PTS_Somatic_Focus_3)
        B_Slope_Activity_Avoidance<- Rel_Activity_Avoidance * (SD_Retest_Activity_Avoidance/SD_Activity_Avoidance)
        A_Constant_Activity_Avoidance<- M_Retest_Activity_Avoidance - (B_Slope_Activity_Avoidance * M_Activity_Avoidance)
        B_Adj_Activity_Avoidance<- SD_Retest_Activity_Avoidance/SD_Activity_Avoidance
        A_Adj_Activity_Avoidance<- M_Retest_Activity_Avoidance - (B_Adj_Activity_Avoidance * M_Activity_Avoidance)
        PTS_Activity_Avoidance_1<- (B_Adj_Activity_Avoidance * Score_Activity_Avoidance_1) + A_Adj_Activity_Avoidance
        PTS_Activity_Avoidance_2<- (B_Adj_Activity_Avoidance * Score_Activity_Avoidance_2) + A_Adj_Activity_Avoidance
        PTS_Activity_Avoidance_3<- (B_Adj_Activity_Avoidance * Score_Activity_Avoidance_3) + A_Adj_Activity_Avoidance
        PTS_Activity_Avoidance<- c(PTS_Activity_Avoidance_1, PTS_Activity_Avoidance_2, PTS_Activity_Avoidance_3)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope * Score_1
        PTS_2<- B_Slope * Score_2
        PTS_3<- B_Slope * Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Somatic_Focus<- Rel_Somatic_Focus * (SD_Retest_Somatic_Focus/SD_Somatic_Focus)
        PTS_Somatic_Focus_1<- B_Slope_Somatic_Focus * Score_Somatic_Focus_1
        PTS_Somatic_Focus_2<- B_Slope_Somatic_Focus * Score_Somatic_Focus_2
        PTS_Somatic_Focus_3<- B_Slope_Somatic_Focus * Score_Somatic_Focus_3
        PTS_Somatic_Focus<- c(PTS_Somatic_Focus_1, PTS_Somatic_Focus_2, PTS_Somatic_Focus_3)
        B_Slope_Activity_Avoidance<- Rel_Activity_Avoidance * (SD_Retest_Activity_Avoidance/SD_Activity_Avoidance)
        PTS_Activity_Avoidance_1<- B_Slope_Activity_Avoidance * Score_Activity_Avoidance_1
        PTS_Activity_Avoidance_2<- B_Slope_Activity_Avoidance * Score_Activity_Avoidance_2
        PTS_Activity_Avoidance_3<- B_Slope_Activity_Avoidance * Score_Activity_Avoidance_3
        PTS_Activity_Avoidance<- c(PTS_Activity_Avoidance_1, PTS_Activity_Avoidance_2, PTS_Activity_Avoidance_3)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Somatic_Focus_1<- Score_Somatic_Focus_1 + (M_Retest_Somatic_Focus - M_Somatic_Focus)
        PTS_Somatic_Focus_2<- Score_Somatic_Focus_2 + (M_Retest_Somatic_Focus - M_Somatic_Focus)
        PTS_Somatic_Focus_3<- Score_Somatic_Focus_3 + (M_Retest_Somatic_Focus - M_Somatic_Focus)
        PTS_Somatic_Focus<- c(PTS_Somatic_Focus_1, PTS_Somatic_Focus_2, PTS_Somatic_Focus_3)
        PTS_Activity_Avoidance_1<- Score_Activity_Avoidance_1 + (M_Retest_Activity_Avoidance - M_Activity_Avoidance)
        PTS_Activity_Avoidance_2<- Score_Activity_Avoidance_2 + (M_Retest_Activity_Avoidance - M_Activity_Avoidance)
        PTS_Activity_Avoidance_3<- Score_Activity_Avoidance_3 + (M_Retest_Activity_Avoidance - M_Activity_Avoidance)
        PTS_Activity_Avoidance<- c(PTS_Activity_Avoidance_1, PTS_Activity_Avoidance_2, PTS_Activity_Avoidance_3)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Somatic_Focus<- round(PTS_Somatic_Focus, digits = 2)
      PTS_Activity_Avoidance<- round(PTS_Activity_Avoidance, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE3<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_3 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2, SE3)
        SE_Somatic_Focus_1<- McSweeny_SE_Somatic_Focus*sqrt(1 + (1/SampleN) + ((Score_Somatic_Focus_1 - M_Somatic_Focus)^2/(SD_Somatic_Focus^2*(SampleN-1))))
        SE_Somatic_Focus_2<- McSweeny_SE_Somatic_Focus*sqrt(1 + (1/SampleN) + ((Score_Somatic_Focus_2 - M_Somatic_Focus)^2/(SD_Somatic_Focus^2*(SampleN-1))))
        SE_Somatic_Focus_3<- McSweeny_SE_Somatic_Focus*sqrt(1 + (1/SampleN) + ((Score_Somatic_Focus_3 - M_Somatic_Focus)^2/(SD_Somatic_Focus^2*(SampleN-1))))
        SE_Somatic_Focus<- c(SE_Somatic_Focus_1, SE_Somatic_Focus_2, SE_Somatic_Focus_3)
        SE_Activity_Avoidance_1<- McSweeny_SE_Activity_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Activity_Avoidance_1 - M_Activity_Avoidance)^2/(SD_Activity_Avoidance^2*(SampleN-1))))
        SE_Activity_Avoidance_2<- McSweeny_SE_Activity_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Activity_Avoidance_2 - M_Activity_Avoidance)^2/(SD_Activity_Avoidance^2*(SampleN-1))))
        SE_Activity_Avoidance_3<- McSweeny_SE_Activity_Avoidance*sqrt(1 + (1/SampleN) + ((Score_Activity_Avoidance_3 - M_Activity_Avoidance)^2/(SD_Activity_Avoidance^2*(SampleN-1))))
        SE_Activity_Avoidance<- c(SE_Activity_Avoidance_1, SE_Activity_Avoidance_2, SE_Activity_Avoidance_3)
        SE<- round(SE, digits = 2)
        SE_Somatic_Focus<- round(SE_Somatic_Focus, digits = 2)
        SE_Activity_Avoidance<- round(SE_Activity_Avoidance, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2), (Conf*SE3))
        CI<- round(CI, digits = 2)
        CI_Somatic_Focus<- c((Conf*SE_Somatic_Focus_1), (Conf*SE_Somatic_Focus_2), (Conf*SE_Somatic_Focus_3))
        CI_Somatic_Focus<- round(CI_Somatic_Focus, digits = 2)
        CI_Activity_Avoidance<- c((Conf*SE_Activity_Avoidance_1), (Conf*SE_Activity_Avoidance_2), (Conf*SE_Activity_Avoidance_3))
        CI_Activity_Avoidance<- round(CI_Activity_Avoidance, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Somatic_Focus<- c((Conf*SE_Somatic_Focus), (Conf*SE_Somatic_Focus), (Conf*SE_Somatic_Focus))
        CI_Somatic_Focus<- round(CI_Somatic_Focus, digits = 2)
        CI_Activity_Avoidance<- c((Conf*SE_Activity_Avoidance), (Conf*SE_Activity_Avoidance), (Conf*SE_Activity_Avoidance))
        CI_Activity_Avoidance<- round(CI_Activity_Avoidance, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Somatic_Focus<- PTS_Somatic_Focus + CI_Somatic_Focus
      CI_Upper_Lim_Somatic_Focus<- round(CI_Upper_Lim_Somatic_Focus, digits = 2)
      CI_Lower_Lim_Somatic_Focus<-PTS_Somatic_Focus - CI_Somatic_Focus
      CI_Lower_Lim_Somatic_Focus<- round(CI_Lower_Lim_Somatic_Focus, digits = 2)
      CI_Upper_Lim_Activity_Avoidance<- PTS_Activity_Avoidance + CI_Activity_Avoidance
      CI_Upper_Lim_Activity_Avoidance<- round(CI_Upper_Lim_Activity_Avoidance, digits = 2)
      CI_Lower_Lim_Activity_Avoidance<-PTS_Activity_Avoidance - CI_Activity_Avoidance
      CI_Lower_Lim_Activity_Avoidance<- round(CI_Lower_Lim_Activity_Avoidance, digits = 2)
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Somatic_Focus == "2") {
        CI_Somatic_Focus<- input$Man_CI_Somatic_Focus
        CI_Somatic_Focus<- c(CI_Somatic_Focus, CI_Somatic_Focus, CI_Somatic_Focus)
        CI_Somatic_Focus<- round(CI_Somatic_Focus, digits = 2)
        CI_Upper_Lim_Somatic_Focus<- Score_Somatic_Focus + CI_Somatic_Focus
        CI_Upper_Lim_Somatic_Focus<- round(CI_Upper_Lim_Somatic_Focus, digits = 2)
        CI_Lower_Lim_Somatic_Focus<- Score_Somatic_Focus - CI_Somatic_Focus
        CI_Lower_Lim_Somatic_Focus<- round(CI_Lower_Lim_Somatic_Focus, digits = 2)
      }
      if(input$Select_CI_Activity_Avoidance == "2") {
        CI_Activity_Avoidance<- input$Man_CI_Activity_Avoidance
        CI_Activity_Avoidance<- c(CI_Activity_Avoidance, CI_Activity_Avoidance, CI_Activity_Avoidance)
        CI_Activity_Avoidance<- round(CI_Activity_Avoidance, digits = 2)
        CI_Upper_Lim_Activity_Avoidance<- Score_Activity_Avoidance + CI_Activity_Avoidance
        CI_Upper_Lim_Activity_Avoidance<- round(CI_Upper_Lim_Activity_Avoidance, digits = 2)
        CI_Lower_Lim_Activity_Avoidance<- Score_Activity_Avoidance - CI_Activity_Avoidance
        CI_Lower_Lim_Activity_Avoidance<- round(CI_Lower_Lim_Activity_Avoidance, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_1<- rep(Cutoff_Score_1, 3)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_2<- rep(Cutoff_Score_2, 3)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_3<- rep(Cutoff_Score_3, 3)
      Cutoff_Score_Somatic_Focus_1<- round(input$Cutoff_Somatic_Focus_1, digits = 2)
      Cutoff_Score_Somatic_Focus_1<- rep(Cutoff_Score_Somatic_Focus_1, 3)
      Cutoff_Score_Somatic_Focus_2<- round(input$Cutoff_Somatic_Focus_2, digits = 2)
      Cutoff_Score_Somatic_Focus_2<- rep(Cutoff_Score_Somatic_Focus_2, 3)
      Cutoff_Score_Somatic_Focus_3<- round(input$Cutoff_Somatic_Focus_3, digits = 2)
      Cutoff_Score_Somatic_Focus_3<- rep(Cutoff_Score_Somatic_Focus_3, 3)
      Cutoff_Score_Activity_Avoidance_1<- round(input$Cutoff_Activity_Avoidance_1, digits = 2)
      Cutoff_Score_Activity_Avoidance_1<- rep(Cutoff_Score_Activity_Avoidance_1, 3)
      Cutoff_Score_Activity_Avoidance_2<- round(input$Cutoff_Activity_Avoidance_2, digits = 2)
      Cutoff_Score_Activity_Avoidance_2<- rep(Cutoff_Score_Activity_Avoidance_2, 3)
      Cutoff_Score_Activity_Avoidance_3<- round(input$Cutoff_Activity_Avoidance_3, digits = 2)
      Cutoff_Score_Activity_Avoidance_3<- rep(Cutoff_Score_Activity_Avoidance_3, 3)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Somatic_Focus,Change_Somatic_Focus,PTS_Somatic_Focus, SE_Somatic_Focus, CI_Upper_Lim_Somatic_Focus, CI_Lower_Lim_Somatic_Focus, Cutoff_Score_Somatic_Focus_1,Cutoff_Score_Somatic_Focus_2,Cutoff_Score_Somatic_Focus_3,
                                      Score_Activity_Avoidance,Change_Activity_Avoidance, PTS_Activity_Avoidance, SE_Activity_Avoidance, CI_Upper_Lim_Activity_Avoidance, CI_Lower_Lim_Activity_Avoidance, Cutoff_Score_Activity_Avoidance_1,Cutoff_Score_Activity_Avoidance_2,Cutoff_Score_Activity_Avoidance_3)
    }
    
    
    #Create dataframes to be displayed as tables in pdf report (statistical values used in reliable change analyses)
    
    if(input$RelChangeMethod == 'Nunnally & Bernstein (1994)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Somatic_Focus<<- data.frame(Pop,  M_Somatic_Focus, SD_Somatic_Focus, RelChangeMethod, Rel_Somatic_Focus, ConfInt)
      names(Stats_Table_Somatic_Focus)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Activity_Avoidance<<- data.frame(Pop,  M_Activity_Avoidance, SD_Activity_Avoidance, RelChangeMethod, Rel_Activity_Avoidance, ConfInt)
      names(Stats_Table_Activity_Avoidance)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Chelune et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Somatic_Focus<<- data.frame(Pop,  M_Somatic_Focus, M_Retest_Somatic_Focus, SD_Somatic_Focus, RelChangeMethod, Rel_Somatic_Focus, ConfInt)
      names(Stats_Table_Somatic_Focus)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Activity_Avoidance<<- data.frame(Pop,  M_Activity_Avoidance, M_Retest_Activity_Avoidance, SD_Activity_Avoidance, RelChangeMethod, Rel_Activity_Avoidance, ConfInt)
      names(Stats_Table_Activity_Avoidance)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Maassen et al. (2006)' | input$RelChangeMethod == 'McSweeny et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Somatic_Focus<<- data.frame(Pop,  M_Somatic_Focus, M_Retest_Somatic_Focus, SD_Somatic_Focus, SD_Retest_Somatic_Focus, RelChangeMethod, Rel_Somatic_Focus, ConfInt)
      names(Stats_Table_Somatic_Focus)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Activity_Avoidance<<- data.frame(Pop,  M_Activity_Avoidance, M_Retest_Activity_Avoidance, SD_Activity_Avoidance, SD_Retest_Activity_Avoidance, RelChangeMethod, Rel_Activity_Avoidance, ConfInt)
      names(Stats_Table_Activity_Avoidance)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Crawford & Howell (1998)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, SampleN, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Somatic_Focus<<- data.frame(Pop,  M_Somatic_Focus, M_Retest_Somatic_Focus, SD_Somatic_Focus, SD_Retest_Somatic_Focus, RelChangeMethod, Rel_Somatic_Focus, SampleN,ConfInt)
      names(Stats_Table_Somatic_Focus)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Activity_Avoidance<<- data.frame(Pop,  M_Activity_Avoidance, M_Retest_Activity_Avoidance, SD_Activity_Avoidance, SD_Retest_Activity_Avoidance, RelChangeMethod, Rel_Activity_Avoidance, SampleN, ConfInt)
      names(Stats_Table_Activity_Avoidance)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
    } else if (input$RelChangeMethod == 'Jacobson & Truax (1991)' | input$RelChangeMethod == 'Speer (1992)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Somatic_Focus<<- data.frame(Pop,  SD_Somatic_Focus, RelChangeMethod, Rel_Somatic_Focus, ConfInt)
      names(Stats_Table_Somatic_Focus)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Activity_Avoidance<<- data.frame(Pop,  SD_Activity_Avoidance, RelChangeMethod, Rel_Activity_Avoidance, ConfInt)
      names(Stats_Table_Activity_Avoidance)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
    }
    
    #If custom confidence intervals options are selected, make sure that no values appear for PS and SE in pdf tables
    #Change the look of tables in report if custom confidence intervals options are selected
    
    if (input$Select_CI == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS = NA, SE = NA)
      Stats_Table_Fullscale<<- Stats_Table_Fullscale %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI[1])
    }
    if (input$Select_CI_Somatic_Focus == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Somatic_Focus = NA, SE_Somatic_Focus = NA)
      Stats_Table_Somatic_Focus<<- Stats_Table_Somatic_Focus %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                                    "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Somatic_Focus[1])
    }
    if (input$Select_CI_Activity_Avoidance == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Activity_Avoidance = NA, SE_Activity_Avoidance = NA)
      Stats_Table_Activity_Avoidance<<- Stats_Table_Activity_Avoidance %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                              "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Activity_Avoidance[1])
    }
    
  
    #Create a dataframe to be used in the analytics spreadsheet.
    
    #Calculate variables for significant improvement
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$CI_Lower_Lim[1]) {
      TSK.Fullscale.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$CI_Lower_Lim[1]) {
      TSK.Fullscale.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$CI_Upper_Lim[1]) {
      TSK.Fullscale.Sig.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$CI_Upper_Lim[1]) {
      TSK.Fullscale.Sig.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Somatic_Focus[length(Entered_Scores_Df$Score_Somatic_Focus)] < Entered_Scores_Df$CI_Lower_Lim_Somatic_Focus[1]) {
      TSK.Somatic.Focus.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Somatic_Focus[length(Entered_Scores_Df$Score_Somatic_Focus)] >= Entered_Scores_Df$CI_Lower_Lim_Somatic_Focus[1]) {
      TSK.Somatic.Focus.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Somatic_Focus[length(Entered_Scores_Df$Score_Somatic_Focus)] > Entered_Scores_Df$CI_Upper_Lim_Somatic_Focus[1]) {
      TSK.Somatic.Focus.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Somatic_Focus[length(Entered_Scores_Df$Score_Somatic_Focus)] <= Entered_Scores_Df$CI_Upper_Lim_Somatic_Focus[1]) {
      TSK.Somatic.Focus.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Activity_Avoidance[length(Entered_Scores_Df$Score_Activity_Avoidance)] < Entered_Scores_Df$CI_Lower_Lim_Activity_Avoidance[1]) {
      TSK.Activity.Avoidance.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Activity_Avoidance[length(Entered_Scores_Df$Score_Activity_Avoidance)] >= Entered_Scores_Df$CI_Lower_Lim_Activity_Avoidance[1]) {
      TSK.Activity.Avoidance.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Activity_Avoidance[length(Entered_Scores_Df$Score_Activity_Avoidance)] > Entered_Scores_Df$CI_Upper_Lim_Activity_Avoidance[1]) {
      TSK.Activity.Avoidance.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Activity_Avoidance[length(Entered_Scores_Df$Score_Activity_Avoidance)] <= Entered_Scores_Df$CI_Upper_Lim_Activity_Avoidance[1]) {
      TSK.Activity.Avoidance.Sig.Deterioration<- "No"
    }
    
    #Calculate variables for improvement
    
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$Score[1]) {
      TSK.Fullscale.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$Score[1]) {
      TSK.Fullscale.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$Score[1]) {
      TSK.Fullscale.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$Score[1]) {
      TSK.Fullscale.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Somatic_Focus[length(Entered_Scores_Df$Score_Somatic_Focus)] < Entered_Scores_Df$Score_Somatic_Focus[1]) {
      TSK.Somatic.Focus.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Somatic_Focus[length(Entered_Scores_Df$Score_Somatic_Focus)] >= Entered_Scores_Df$Score_Somatic_Focus[1]) {
      TSK.Somatic.Focus.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Somatic_Focus[length(Entered_Scores_Df$Score_Somatic_Focus)] > Entered_Scores_Df$Score_Somatic_Focus[1]) {
      TSK.Somatic.Focus.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Somatic_Focus[length(Entered_Scores_Df$Score_Somatic_Focus)] <= Entered_Scores_Df$Score_Somatic_Focus[1]) {
      TSK.Somatic.Focus.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Activity_Avoidance[length(Entered_Scores_Df$Score_Activity_Avoidance)] < Entered_Scores_Df$Score_Activity_Avoidance[1]) {
      TSK.Activity.Avoidance.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Activity_Avoidance[length(Entered_Scores_Df$Score_Activity_Avoidance)] >= Entered_Scores_Df$Score_Activity_Avoidance[1]) {
      TSK.Activity.Avoidance.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Activity_Avoidance[length(Entered_Scores_Df$Score_Activity_Avoidance)] > Entered_Scores_Df$Score_Activity_Avoidance[1]) {
      TSK.Activity.Avoidance.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Activity_Avoidance[length(Entered_Scores_Df$Score_Activity_Avoidance)] <= Entered_Scores_Df$Score_Activity_Avoidance[1]) {
      TSK.Activity.Avoidance.Deterioration<- "No"
    }
    

    TSK.Fullscale.Change<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] - Entered_Scores_Df$Score[1]
    TSK.Somatic.Focus.Change<- Entered_Scores_Df$Score_Somatic_Focus[length(Entered_Scores_Df$Score_Somatic_Focus)] - Entered_Scores_Df$Score_Somatic_Focus[1]
    TSK.Activity.Avoidance.Change<- Entered_Scores_Df$Score_Activity_Avoidance[length(Entered_Scores_Df$Score_Activity_Avoidance)] - Entered_Scores_Df$Score_Activity_Avoidance[1]
    TSK.Fullscale.Comparisons<- length(Entered_Scores_Df$Change) - 1
    TSK.Somatic.Focus.Comparisons<- length(Entered_Scores_Df$Change_Somatic_Focus) - 1
    TSK.Activity.Avoidance.Comparisons<- length(Entered_Scores_Df$Change_Activity_Avoidance) - 1
    TSK.Fullscale.First.Date<- Entered_Scores_Df$Date[1]
    TSK.Somatic.Focus.First.Date<- Entered_Scores_Df$Date[1]
    TSK.Activity.Avoidance.First.Date<- Entered_Scores_Df$Date[1]
    TSK.Fullscale.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    TSK.Somatic.Focus.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    TSK.Activity.Avoidance.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    TSK.Fullscale.First.Score<- Entered_Scores_Df$Score[1]
    TSK.Somatic.Focus.First.Score<- Entered_Scores_Df$Score_Somatic_Focus[1]
    TSK.Activity.Avoidance.First.Score<- Entered_Scores_Df$Score_Activity_Avoidance[1]
    TSK.Fullscale.Last.Score<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)]
    TSK.Somatic.Focus.Last.Score<- Entered_Scores_Df$Score_Somatic_Focus[length(Entered_Scores_Df$Score_Somatic_Focus)]
    TSK.Activity.Avoidance.Last.Score<- Entered_Scores_Df$Score_Activity_Avoidance[length(Entered_Scores_Df$Score_Activity_Avoidance)]
  
    
    Analytics_Df<<- data.frame(TSK.Fullscale.First.Date, TSK.Fullscale.First.Score, TSK.Fullscale.Comparisons, TSK.Fullscale.Change, TSK.Fullscale.Last.Date, TSK.Fullscale.Last.Score, TSK.Fullscale.Improvement,TSK.Fullscale.Sig.Improvement, TSK.Fullscale.Deterioration, TSK.Fullscale.Sig.Deterioration,
                               TSK.Somatic.Focus.First.Date, TSK.Somatic.Focus.First.Score, TSK.Somatic.Focus.Comparisons, TSK.Somatic.Focus.Change, TSK.Somatic.Focus.Last.Date, TSK.Somatic.Focus.Last.Score, TSK.Somatic.Focus.Improvement, TSK.Somatic.Focus.Sig.Improvement, TSK.Somatic.Focus.Deterioration, TSK.Somatic.Focus.Sig.Deterioration,
                               TSK.Activity.Avoidance.First.Date, TSK.Activity.Avoidance.First.Score, TSK.Activity.Avoidance.Comparisons, TSK.Activity.Avoidance.Change, TSK.Activity.Avoidance.Last.Date, TSK.Activity.Avoidance.Last.Score, TSK.Activity.Avoidance.Improvement, TSK.Activity.Avoidance.Sig.Improvement, TSK.Activity.Avoidance.Deterioration, TSK.Activity.Avoidance.Sig.Deterioration)
    
    
    
  })
  
  
  
  #Create an expression to activate the entered data & create notifications to indicate inaccurate data entry
  
  Entered_Scores_Notifications<- observeEvent(input$Action_Submit_Data, {
    
    Entered_Scores_Reac()
    
    if(length(Score_1a) < 17) {
      showNotification("The TSK is a 17-item scale. You have entered less than 17 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(length(Score_1a) > 17) {
      showNotification("The TSK is a 17-item scale. You have entered more than 17 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(input$Timepoint != "1") {
      if(length(Score_2a) < 17) {
        showNotification("The TSK is a 17-item scale. You have entered less than 17 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_2a) > 17) {
        showNotification("The TSK is a 17-item scale. You have entered more than 17 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
    if(input$Timepoint == "3") {
      if(length(Score_3a) < 17) {
        showNotification("The TSK is a 17-item scale. You have entered less than 17 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_3a) > 17) {
        showNotification("The TSK is a 17-item scale. You have entered more than 17 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
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
    
    Gap_Somatic_Focus<- Entered_Scores_Df[1,11] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),11]
    Entered_Scores_Df[1,12]<- Gap_Somatic_Focus
    
    Gap_Activity_Avoidance<- Entered_Scores_Df[1,20] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),20]
    Entered_Scores_Df[1,21]<- Gap_Activity_Avoidance
    
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
    
    filename = paste0(" TSK Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "TSK.Rmd")
      file.copy("TSK.Rmd", tempReport, overwrite = TRUE)
      
      # Pass data objects to Rmd document
      params <- list(
        PN = PN,
        CN = CN,
        Tab_Reference = Tab_Reference,
        Entered_Scores_Df = Entered_Scores_Df,
        Stats_Table_Fullscale = Stats_Table_Fullscale,
        Stats_Table_Somatic_Focus = Stats_Table_Somatic_Focus,
        Stats_Table_Activity_Avoidance = Stats_Table_Activity_Avoidance,
        Cutoff_Names = Cutoff_Names,
        Item_Df = Item_Df,
        Graph_Up_Lim = Graph_Up_Lim,
        Y_Vals = Y_Vals,
        Y_Vals_Labs = Y_Vals_Labs,
        Graph_Title =  Graph_Title,
        Graph_Up_Lim_AA = Graph_Up_Lim_AA,
        Y_Vals_AA = Y_Vals_AA,
        Y_Vals_Labs_AA = Y_Vals_Labs_AA
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
      paste(paste0(" TSK Data ", format(Sys.time(), '%d/%m/%y')), input$Output_Filetype1, sep = ".")
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
        Patient_Row<- Old_Analytics_CSV %>% filter(Name == input$ControlName)
        Patient_Row<- as.data.frame(Patient_Row)
        Patient_Row<- bind_cols(Patient_Row, Analytics_Df)
        Old_Analytics_CSV[Old_Analytics_CSV$Name == input$ControlName,]<- rep(NA, ncol(Old_Analytics_CSV))
        List2<- list(Old_Analytics_CSV, Patient_Row)
        Same_Patient_OD<- rbindlist(List2, use.names = TRUE, fill = TRUE)
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













