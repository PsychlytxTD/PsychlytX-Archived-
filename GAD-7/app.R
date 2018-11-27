
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
      menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "GAD7"),
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
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| General Anxiety Disorder 7-Item Scale (GAD-7)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 750),
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
                "Bair, M. J., Wu, J., Damush, T. M., Sutherland, J. M., & Kroenke, K. (2008). Association of depression and anxiety alone and in combination with chronic musculoskeletal pain in primary care patients. Psychosomatic Medicine, 70(8), 890-897. doi:10.1097/PSY.0b013e318185c510", br(), br(),
                "Beard, C., & Björgvinsson, T. (2014). Beyond generalized anxiety disorder: Psychometric properties of the GAD-7 in a heterogeneous psychiatric sample. Journal of Anxiety Disorders, 28(6), 547-552.", br(), br(), 
                "Coventry, P., Lovell, K., Dickens, C., Bower, P., Chew-Graham, C., McElvenny, D., . . . Gask, L. (2015). Integrated primary care for patients with mental and physical multimorbidity: Cluster randomised controlled trial of collaborative care for patients with depression comorbid with diabetes or cardiovascular disease. BMJ (Clinical Research Ed.), 350, h638. doi:10.1136/bmj.h638", br(), br(),
                "Dear, B. F., Titov, N., Sunderland, M., McMillan, D., Anderson, T., Lorian, C., & Robinson, E. (2011). Psychometric comparison of the generalized anxiety disorder scale-7 and the penn state worry questionnaire for measuring response during treatment of generalised anxiety disorder. Cognitive Behaviour Therapy, 40(3), 216-227.", br(), br(), 
                "Fenwick, E. K., Rees, G., Holmes-Truscott, E., Browne, J. L., Pouwer, F., & Speight, J. (2016). What is the best measure for assessing diabetes distress? A comparison of the problem areas in diabetes and diabetes distress scale: Results from diabetes MILES–Australia. Journal of Health Psychology, , 1359105316642006.", br(), br(), 
                "Hinz, A., Klein, A. M., Brähler, E., Glaesmer, H., Luck, T., Riedel-Heller, S. G., . . . Hilbert, A. (2017). Psychometric evaluation of the generalized anxiety disorder screener GAD-7, based on a large german general population sample. Journal of Affective Disorders, 210, 338-344.", br(), br(), 
                "Jordan, P., Shedden-Mora, M. C., & Löwe, B. (2017). Psychometric analysis of the generalized anxiety disorder scale (GAD-7) in primary care using modern item response theory. PloS One, 12(8), e0182162.", br(), br(), 
                "Plummer, F., Manea, L., Trepel, D., & McMillan, D. (2016). Screening for anxiety disorders with the GAD-7 and GAD-2: A systematic review and diagnostic metaanalysis. General Hospital Psychiatry, 39, 24-31.", br(), br(), 
                "Schmid, A. A., Arnold, S. E., Jones, V. A., Ritter, M. J., Sapp, S. A., & Van Puymbroeck, M. (2015). Fear of falling in people with chronic stroke. American Journal of Occupational Therapy, 69(3), 6903350020p1-6903350020p5.", br(), br(), 
                "Spitzer, R. L., Kroenke, K., Williams, J. B., & Löwe, B. (2006). A brief measure for assessing generalized anxiety disorder: The GAD-7. Archives of Internal Medicine, 166(10), 1092-1097.", br(), br(), 
                "Wild, B., Eckl, A., Herzog, W., Niehoff, D., Lechner, S., Maatouk, I., . . . Löwe, B. (2014). Assessing generalized anxiety disorder in elderly people using the GAD-7 and GAD-2 scales: Results of a validation study. The American Journal of Geriatric Psychiatry, 22(10), 1029-1038." 
 
              ),
        
        
        
        tabItem(tabName = "GAD7",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel("Scale",
                             wellPanel(style = "background-color: #ffffff; color: black",
                               fluidRow(
                                 column(width = 12, offset = 5, h3(tags$strong("GAD-7 ")))
                                       ),
                               hr(),
                               fluidRow(
                               column(width = 6, h4(tags$strong("Over the past 2 weeks, how often have you been bothered by the following problems?"))),
                               column(width = 1, offset = 1, h5(tags$strong("Not at all"))),
                               column(width = 1, h5(tags$strong("Several days"))),
                               column(width = 1, h5(tags$strong("More than half the days"))),
                               column(width = 1, h5(tags$strong("Nearly every day")))
                                      ),
                             
                             fluidRow(
                               column(width = 7, h4("Feeling nervous, anxious or on edge")),
                               column(width = 5, radioButtons("Item_1", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, , selected = character(0)))
                                     ),
                             fluidRow(
                               column(width = 7, h4("Not being able to stop or control worrying")),
                               column(width = 5, radioButtons("Item_2", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                             ),
                             fluidRow(
                               column(width = 7, h4("Worrying too much about different things")),
                               column(width = 5, radioButtons("Item_3", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                             ),
                             fluidRow(
                               column(width = 7, h4("Trouble relaxing")),
                               column(width = 5, radioButtons("Item_4", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                            ),
                            fluidRow(
                              column(width = 7, h4("Being so restless it is hard to sit still")),
                              column(width = 5, radioButtons("Item_5", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                            ),
                            fluidRow(
                              column(width = 7, h4("Becoming easily annoyed or irritable")),
                              column(width = 5, radioButtons("Item_6", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                            ),
                            fluidRow(
                              column(width = 7, h4("Feeling afraid as if something awful might happen")),
                              column(width = 5, radioButtons("Item_7", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                            ),
                            hr(),
                            fluidRow(
                              column(width = 4, dateInput("Q_Date", "Date",  format = "dd/mm/yyyy")),
                              column(width = 4, textInput("Q_Name", "Name")),
                              column(width = 4, textInput("Q_Clin_Name", "Clinician's Name"))
                            ),
                            fluidRow(
                              column(width = 12, h5("Scale Source: Spitzer, R. L., Kroenke, K., Williams, J. B., & Lowe, B. (2006). A brief measure for assessing Generalized Anxiety Disorder: the GAD-7. Archives of Internal Medicine, 166(10), 1092–1097."))
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
                                      textInput("ClinicianName", "Enter the name of the Clinician"),
                                      helpText(tags$em("*Your patient's name cannot be seen or used by PsychlytX. If provided, it will appear in the patient report and analytics dataset (if created), accessible only to you.
                                                       This field is optional when generating the patient report, but mandatory if you wish to store and analyse an analytics dataset."))
                                      
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
                                          tags$li(helpText(h4(tags$em("Select a population that matches the characteristics of your patient.", style = "color:black")))),
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
                                                      selectInput("Pop", "", choices = c("Male: General Population", "Female: General Population", "Older Adult", "Primary Care", "Psychiatric", "Generalized Anxiety Disorder", 
                                                                                         "Chronic Musculoskeletal Pain", "Type 1 Diabetes", "Type 2 Diabetes", "Coronary Heart Disease", "Chronic Stroke"))
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
                                                        column(width = 4,
                                                               selectInput("Select_CI", label = "GAD-7 total scale",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI == '2'",
                                                                                numericInput("Man_CI", "Specify the width of the confidence interval", value = 0))
                                                        ))),
                                             
                                             
                                             
                                             tabPanel("Mean", width = 12,
                                                      h4(tags$strong("Enter a mean value")),
                                                      fluidRow(
                                                        column(width = 8,
                                                               uiOutput("Mean_Widg")
                                                               
                                                               
                                                        )),
                                                      
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Chelune et al. (1993)' || input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)' || input.RelChangeMethod == 'Speer (1992)'",
                                                                       
                                                                       h4(tags$strong("Enter a mean value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 4,
                                                                                numericInput("Retest_Mean_1", "GAD-7 total scale", value = 0)
                                                                                
                                                                         )))),                               
                                             
                                             tabPanel("Sd", width = 12,
                                                      h4(tags$strong("Enter a standard deviation value")),
                                                      fluidRow(
                                                        column(width = 8,
                                                               uiOutput("Sd_Widg")
                                                        )),
                                                      
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)'",
                                                                       
                                                                       h4(tags$strong("Enter a standard deviation value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 4,
                                                                                numericInput("Retest_Sd_1", "GAD-7 total scale", value = 0)
                                                                         )))),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      h4(tags$strong("Enter a test-retest reliability value")),
                                                      fluidRow(
                                                        column(width = 6,
                                                               numericInput("Reliability", "GAD-7 total scale", value = .83),
                                                               h6("Reference: Spitzer, Kroenke, Williams & Löwe (2006)")
                                                        )),
                                                      checkboxInput("Rel_Calc_Checkbox", tags$strong("Calculate the test-retest reliability value from research statistics."), width = '100%'),
                                                      uiOutput("Rel_Calc_Panel")
     
                                             ),
                                                              
                                             
                                             tabPanel("Confidence Level", width = 12,
                                                      
                                                      fluidRow(
                                                        column(width = 12, offset = 3,
                                                               radioButtons("Confidence", label = h4(tags$strong(HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), "Select the level of confidence for intervals")),
                                                                            choices = list("99%" = 1, "95%" = 2, "90%" = 3),
                                                                            selected = 2, inline = T)
                                                        )
                                                             )
                                                      
                                                      
                                                      
                                             ),
                                             tabPanel("User-Defined Cut-Off Scores", width = 12,
                                                      h4(tags$strong("Define cut-off scores")),
                                                      checkboxInput("Dev_Cutoffs", "Use cut-off scores recommended by the scale developers", TRUE, width = '100%'),
                                                      hr(),
                                                      h4(tags$strong("First cut-off score")),
                                                      fluidRow(
                                                        column(width = 8,
                                                               uiOutput("Cutoff_Widg_1"),
                                                               verbatimTextOutput("testing")
                                                               
                                                        )),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Second cut-off score")),
                                                      fluidRow(
                                                        column(width = 8,
                                                               uiOutput("Cutoff_Widg_2")
                                                               
                                                        )),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Third cut-off score")),
                                                      fluidRow(
                                                        column(width = 8,
                                                               uiOutput("Cutoff_Widg_3")
                                                               
                                                               
                                                               
                                                        )), hr()
                                                      
                                             ),
                                             
                                             
                                             h4(tags$strong("Psychometric Properties of the GAD-7 Relevant to Assessing Reliable & Clinically Significant Change")),
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
  
 
  
  
  observe({
    
    Q_Scores <- paste(input$Item_1, input$Item_2, input$Item_3, input$Item_4, input$Item_5, input$Item_6, input$Item_7, sep = ",")
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
  
  
  
  #Make default values vary depending on population selection
  
  CI_Vals_Reac<- reactive({
    
    
    if(input$Pop == "Male: General Population") {
      Mean_Val<<- 3.01
      Sd_Val<<-3.12
      Source_Mean<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
      Source_Sd<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
      if(input$Dev_Cutoffs == FALSE) { 
        Cut_Val_1<<- Mean_Val
        Cut_Val_2<<- Mean_Val + Sd_Val 
        Cut_Val_3<<- Mean_Val + (2*Sd_Val)
        Cut_Lab_1<<- "Normative Mean: Males"
        Cut_Lab_2<<- "Normative Mean: Males + 1Sd"
        Cut_Lab_3<<- "Normative Mean: Males + 2Sd"
        Source_Cutoff_1<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)" 
        Source_Cutoff_2<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
        Source_Cutoff_3<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
      } else {
        Cut_Val_1<<- 5
        Cut_Val_2<<- 10
        Cut_Val_3<<- 15
        Cut_Lab_1<<- "Mild"
        Cut_Lab_2<<- "Moderate: For Further Evaluation"
        Cut_Lab_3<<- "Severe"
        Source_Cutoff_1<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_2<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_3<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
      }
    } else if(input$Pop == "Female: General Population") {
      Mean_Val<<-  4.07
      Sd_Val<<-  3.53
      Source_Mean<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
      Source_Sd<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
      if(input$Dev_Cutoffs == FALSE) {
        Cut_Val_1<<-  Mean_Val
        Cut_Val_2<<-  Mean_Val + Sd_Val 
        Cut_Val_3<<-  Mean_Val + (2*Sd_Val)
        Cut_Lab_1<<-  "Normative Mean: Females"
        Cut_Lab_2<<-  "Normative Mean: Females + 1Sd"
        Cut_Lab_3<<-  "Normative Mean: Females + 2Sd"
        Source_Cutoff_1<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
        Source_Cutoff_2<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
        Source_Cutoff_3<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
      } else {
        Cut_Val_1<<-  5
        Cut_Val_2<<-  10
        Cut_Val_3<<-  15
        Cut_Lab_1<<-  "Mild"
        Cut_Lab_2<<-  "Moderate: For Further Evaluation"
        Cut_Lab_3<<-  "Severe"
        Source_Cutoff_1<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_2<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_3<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
      }
    } else if(input$Pop == "Older Adult") {
      Mean_Val<<-  2
      Sd_Val<<-  2.88
      Source_Mean<<- "Wild, Eckl, Herzog, Niehoff et al (2012)"
      Source_Sd<<- "Wild, Eckl, Herzog, Niehoff et al (2012)"
      if(input$Dev_Cutoffs == FALSE) {
        Cut_Val_1<<- 3.57
        Cut_Val_2<<-  Mean_Val + Sd_Val 
        Cut_Val_3<<-  Mean_Val + (2*Sd_Val)
        Cut_Lab_1<<-  "Normative Mean"
        Cut_Lab_2<<-  "Older Adult Mean + 1Sd"
        Cut_Lab_3<<-  "Older Adult Mean + 2Sd"
        Source_Cutoff_1<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
        Source_Cutoff_2<<- "Wild, Eckl, Herzog, Niehoff et al (2012)" 
        Source_Cutoff_3<<- "Wild, Eckl, Herzog, Niehoff et al (2012)" 
      } else {
        Cut_Val_1<<-  5
        Cut_Val_2<<-  10
        Cut_Val_3<<-  15
        Cut_Lab_1<<-  "Mild"
        Cut_Lab_2<<-  "Moderate: For Further Evaluation"
        Cut_Lab_3<<-  "Severe"
        Source_Cutoff_1<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_2<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_3<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
      }
    } else if(input$Pop == "Primary Care") {
      Mean_Val<<-  4.75
      Sd_Val<<- 4.76
      Source_Mean<<- "Jordan, Shedden-Mora & Löwe (2017)"
      Source_Sd<<- "Jordan, Shedden-Mora & Löwe (2017)"
      if(input$Dev_Cutoffs == FALSE) {
        Cut_Val_1<<-  3.57
        Cut_Val_2<<-  Mean_Val + Sd_Val 
        Cut_Val_3<<-  Mean_Val + (2*Sd_Val)
        Cut_Lab_1<<-  "Normative Mean"
        Cut_Lab_2<<-  "Primary Care Mean + 1Sd"
        Cut_Lab_3<<-  "Primary Care Mean + 2Sd"
        Source_Cutoff_1<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
        Source_Cutoff_2<<- "Jordan, Shedden-Mora & Löwe (2017)"
        Source_Cutoff_3<<- "Jordan, Shedden-Mora & Löwe (2017)"
      } else {
        Cut_Val_1<<-  5
        Cut_Val_2<<-  10
        Cut_Val_3<<-  15
        Cut_Lab_1<<-  "Mild"
        Cut_Lab_2<<-  "Moderate: For Further Evaluation"
        Cut_Lab_3<<-  "Severe"
        Source_Cutoff_1<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_2<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_3<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
      }
    } else if(input$Pop == "Psychiatric") {
      Mean_Val<<-  10.86
      Sd_Val<<-  5.62
      Source_Mean<<- "Beard & Björgvinsson (2014)"
      Source_Sd<<- "Beard & Björgvinsson (2014)"
      if(input$Dev_Cutoffs == FALSE) {
        Cut_Val_1<<-  3.57
        Cut_Val_2<<-  Mean_Val + Sd_Val 
        Cut_Val_3<<-  Mean_Val + (2*Sd_Val)
        Cut_Lab_1<<-  "Normative Mean"
        Cut_Lab_2<<-  "Psychiatric Mean + 1Sd"
        Cut_Lab_3<<-  "Psychiatric Mean + 2Sd"
        Source_Cutoff_1<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
        Source_Cutoff_2<<- "Beard & Björgvinsson (2014)"
        Source_Cutoff_3<<- "Beard & Björgvinsson (2014)"
      } else {
        Cut_Val_1<<-  5
        Cut_Val_2<<-  10
        Cut_Val_3<<-  15
        Cut_Lab_1<<-  "Mild"
        Cut_Lab_2<<-  "Moderate: For Further Evaluation"
        Cut_Lab_3<<-  "Severe"
        Source_Cutoff_1<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_2<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_3<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
      }
    } else if(input$Pop == "Generalized Anxiety Disorder") {
      Mean_Val<<-  12.59
      Sd_Val<<-  3.96
      Source_Mean<<- "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)"
      Source_Sd<<- "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)"
      if(input$Dev_Cutoffs == FALSE) {
        Cut_Val_1<<-  3.57
        Cut_Val_2<<-  Mean_Val + Sd_Val 
        Cut_Val_3<<-  Mean_Val + (2*Sd_Val)
        Cut_Lab_1<<-  "Normative Mean"
        Cut_Lab_2<<-  "Generalized Anxiety Disorder Mean + 1Sd"
        Cut_Lab_3<<-  "Generalized Anxiety Disorder Mean + 2Sd"
        Source_Cutoff_1<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
        Source_Cutoff_2<<- "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)"
        Source_Cutoff_3<<- "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)"
      } else {
        Cut_Val_1<<-  5
        Cut_Val_2<<-  10
        Cut_Val_3<<-  15
        Cut_Lab_1<<-  "Mild"
        Cut_Lab_2<<-  "Moderate: For Further Evaluation"
        Cut_Lab_3<<-  "Severe"
        Source_Cutoff_1<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_2<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_3<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
      }
    } else if(input$Pop == "Chronic Musculoskeletal Pain") {
      Mean_Val<<-  2.6
      Sd_Val<<-  2.3
      Source_Mean<<- "Bair, Wu, Damush, Sutherland & Kroenke (2008)"
      Source_Sd<<- "Bair, Wu, Damush, Sutherland & Kroenke (2008)"
      if(input$Dev_Cutoffs == FALSE) {
        Cut_Val_1<<-  3.57
        Cut_Val_2<<-  Mean_Val + Sd_Val 
        Cut_Val_3<<-  Mean_Val + (2*Sd_Val)
        Cut_Lab_1<<-  "Normative Mean"
        Cut_Lab_2<<-  "Musculoskeletal Pain Mean + 1Sd"
        Cut_Lab_3<<-  "Musculoskeletal Pain Mean + 2Sd"
        Source_Cutoff_1<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
        Source_Cutoff_2<<- "Bair, Wu, Damush, Sutherland & Kroenke (2008)"
        Source_Cutoff_3<<- "Bair, Wu, Damush, Sutherland & Kroenke (2008)"
      } else {
        Cut_Val_1<<-  5
        Cut_Val_2<<-  10
        Cut_Val_3<<-  15
        Cut_Lab_1<<-  "Mild"
        Cut_Lab_2<<-  "Moderate: For Further Evaluation"
        Cut_Lab_3<<-  "Severe"
        Source_Cutoff_1<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_2<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_3<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
      }
    } else if(input$Pop == "Coronary Heart Disease") {
      Mean_Val<<-  11.9
      Sd_Val<<-  5.3
      Source_Mean<<- "Conventry, Lovell, Dickens, Bower et al (2015)"
      Source_Sd<<- "Conventry, Lovell, Dickens, Bower et al (2015)"
      if(input$Dev_Cutoffs == FALSE) {
        Cut_Val_1<<-  3.57
        Cut_Val_2<<-  Mean_Val 
        Cut_Val_3<<-  Mean_Val + (2*Sd_Val)
        Cut_Lab_1<<-  "Normative Mean"
        Cut_Lab_2<<-  "Coronary Heart Disease Mean"
        Cut_Lab_3<<-  "Coronary Heart Disease Mean + 2Sd"
        Source_Cutoff_1<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
        Source_Cutoff_2<<- "Conventry, Lovell, Dickens, Bower et al (2015)"
        Source_Cutoff_3<<- "Conventry, Lovell, Dickens, Bower et al (2015)"
      } else {
        Cut_Val_1<<-  5
        Cut_Val_2<<-  10
        Cut_Val_3<<-  15
        Cut_Lab_1<<-  "Mild"
        Cut_Lab_2<<-  "Moderate: For Further Evaluation"
        Cut_Lab_3<<-  "Severe"
        Source_Cutoff_1<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_2<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_3<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
      }
    } else if(input$Pop == "Type 1 Diabetes") {
      Mean_Val<<-  4.7
      Sd_Val<<-  4.6
      Source_Mean<<- "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)"
      Source_Sd<<- "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)"
      if(input$Dev_Cutoffs == FALSE) {
        Cut_Val_1<<-  3.57 
        Cut_Val_2<<-  Mean_Val
        Cut_Val_3<<-  Mean_Val + (2*Sd_Val)
        Cut_Lab_1<<-  "Normative Mean"
        Cut_Lab_2<<-  "Type 1 Diabetes Mean"
        Cut_Lab_3<<-  "Type 1 Diabetes Mean + 2Sd"
        Source_Cutoff_1<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
        Source_Cutoff_2<<- "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)"
        Source_Cutoff_3<<- "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)"
      } else {
        Cut_Val_1<<-  5
        Cut_Val_2<<-  10
        Cut_Val_3<<-  15
        Cut_Lab_1<<-  "Mild"
        Cut_Lab_2<<-  "Moderate: For Further Evaluation"
        Cut_Lab_3<<-  "Severe"
        Source_Cutoff_1<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_2<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_3<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
      }
    } else if(input$Pop == "Type 2 Diabetes") {
      Mean_Val<<-  4.5
      Sd_Val<<-  4.9
      Source_Mean<<- "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)"
      Source_Sd<<- "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)" 
      if(input$Dev_Cutoffs == FALSE) {
        Cut_Val_1<<-  3.57 
        Cut_Val_2<<-  Mean_Val 
        Cut_Val_3<<-  Mean_Val + (2*Sd_Val)
        Cut_Lab_1<<-  "Normative Mean"
        Cut_Lab_2<<-  "Type 2 Diabetes Mean"
        Cut_Lab_3<<-  "Type 2 Diabetes Mean + 2Sd"
        Source_Cutoff_1<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
        Source_Cutoff_2<<- "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)" 
        Source_Cutoff_3<<- "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)" 
      } else {
        Cut_Val_1<<-  5
        Cut_Val_2<<-  10
        Cut_Val_3<<-  15
        Cut_Lab_1<<-  "Mild"
        Cut_Lab_2<<-  "Moderate: For Further Evaluation"
        Cut_Lab_3<<-  "Severe"
        Source_Cutoff_1<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_2<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_3<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
      }
    } else if(input$Pop == "Stroke") {
      Mean_Val<<- 3.87  
      Sd_Val<<-  4.52
      Source_Mean<<- "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)"
      Source_Sd<<- "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)"
      if(input$Dev_Cutoffs == FALSE) {
        Cut_Val_1<<-  3.57
        Cut_Val_2<<-  Mean_Val 
        Cut_Val_3<<-  Mean_Val + (2*Sd_Val)
        Cut_Lab_1<<-  "Normative Mean"
        Cut_Lab_2<<-  "Stroke Mean"
        Cut_Lab_3<<-  "Stroke Mean + 2Sd"
        Source_Cutoff_1<<- "Hinz, Klein, Brähler, Glaesmer et al (2017)"
        Source_Cutoff_2<<- "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)"
        Source_Cutoff_3<<- "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)"
      } else {
        Cut_Val_1<<-  5
        Cut_Val_2<<-  10
        Cut_Val_3<<-  15
        Cut_Lab_1<<-  "Mild"
        Cut_Lab_2<<-  "Moderate: For Further Evaluation"
        Cut_Lab_3<<-  "Severe"
        Source_Cutoff_1<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_2<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
        Source_Cutoff_3<<- "Spitzer, Kroenke, Williams & Löwe (2006)"
      }
    } 
    
    
  })
  
 
  output$Mean_Widg<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_1", "GAD-7 total scale", Mean_Val),
      h6(paste("Reference:", Source_Mean))
          )
  })
  outputOptions(output, "Mean_Widg", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_1", "GAD-7 total scale", Sd_Val),
      h6(paste("Reference:", Source_Sd))
           )
  })
  outputOptions(output, "Sd_Widg", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_1<- renderUI({ 
    CI_Vals_Reac()
     tagList(
      numericInput("Cutoff_1", "GAD-7 total scale", as.numeric(Cut_Val_1)),
      textInput("Cutoff_Text_1", "Name this cut-off score", Cut_Lab_1),
      h6(paste("Reference:", Source_Cutoff_1))
       )
    
    
  })
  outputOptions(output, "Cutoff_Widg_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_2", "GAD-7 total scale", as.numeric(Cut_Val_2)),
      textInput("Cutoff_Text_2", "Name this cut-off score", Cut_Lab_2),
      h6(paste("Reference:", Source_Cutoff_2))
          )
  })
  outputOptions(output, "Cutoff_Widg_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_3", "GAD-7 total scale", as.numeric(Cut_Val_3)),
      textInput("Cutoff_Text_3", "Name this cut-off score", Cut_Lab_3),
      h6(paste("Reference:", Source_Cutoff_3))
          )
  })
  outputOptions(output, "Cutoff_Widg_3", suspendWhenHidden = FALSE)




  #Create a reactive expression defining entered values  
  
  Entered_Scores_Reac<- reactive({
    
    
    TP<- input$Timepoint
    
    PN<<- input$PatientName
    CN<<- input$ClinicianName
    
    Pop<- input$Pop
    
    RelChangeMethod<- input$RelChangeMethod
    
    CI_Vals_Reac()
    
    M<- input$Pop_Mean_1
    SD<- input$Pop_Sd_1
    M_Retest<- input$Retest_Mean_1
    SD_Retest<- input$Retest_Sd_1
    SampleN<- input$SampleN
    Rel<- input$Reliability
    
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
      SE<- round(SE, digits = 2)
    } else if (input$RelChangeMethod == "Chelune et al. (1993)" | input$RelChangeMethod == "Jacobson & Truax (1991)" | input$RelChangeMethod == "Speer (1992)") {
      SE<- sqrt((2*(SD^2))*(1-Rel))
      SE<- round(SE, digits = 2)
    } else if (input$RelChangeMethod == "Maassen et al. (2006)") {
      SE<- sqrt((SD^2 + SD_Retest^2)*(1-Rel))
      SE<- round(SE, digits = 2)
    } else if (input$RelChangeMethod == "McSweeny et al. (1993)") {
      SE<- SD_Retest*sqrt(1 - Rel^2)
      SE<- round(SE, digits = 2)
    }
    
    
    #Caclculate McSweeny standard error to be employed in deriving the Crawford & Howell standard error if selected
    
    McSweeny_SE<- SD_Retest*sqrt(1 - Rel^2)
    
    #Generate object storing names of cut-off scores
    
    Cutoff_Name_1<- input$Cutoff_Text_1
    Cutoff_Name_2<- input$Cutoff_Text_2
    Cutoff_Name_3<- input$Cutoff_Text_3
    
    Cutoff_Names<<- data.frame(Cutoff_Name_1,Cutoff_Name_2,Cutoff_Name_3)
    
    #Define the data & calculate PS, SE, confidence intervals and cut-off scores
    
    if(input$Timepoint == "1") {
      Date<- input$Date_1
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_1a), Score = Score_1a)
      Score<- sum(Score_1a, na.rm = TRUE)
      Score<- round(Score, digits = 2)
      Change<- 0
      Change<- round(Change, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS<- (Rel * Score) + (M * (1 - Rel))
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS<- Score + (M_Retest - M)  
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS<- (B_Adj * Score) + A_Adj
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS<- B_Slope * Score
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS<- Score + (M_Retest - M)
      }
      PTS<- round(PTS, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score - M)^2/(SD^2*(SampleN-1))))
        SE<- round(SE, digits = 2)
        CI<- (Conf*SE)
        CI<- round(CI, digits = 2)
      }
      CI<- (Conf*SE)
      CI<- round(CI, digits = 2)
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      if(input$Select_CI == "2") {
        CI<- input$Man_CI
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3)
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
      Change<- c(0, (Score_2 - Score_1))
      Change<- round(Change, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS<- c(PTS_1,PTS_2)
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS<- c(PTS_1,PTS_2)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS<- c(PTS_1,PTS_2)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2)
      }
      PTS<- round(PTS, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2)
        SE<- round(SE, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2))
        CI<- round(CI, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_1<- rep(Cutoff_Score_1, 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_2<- rep(Cutoff_Score_2, 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_3<- rep(Cutoff_Score_3, 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3)
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
      Change<- c(0, (Score_2 - Score_1), (Score_3 - Score_2))
      Change<- round(Change, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS_3<- (Rel * Score_3) + (M * (1 - Rel))
        PTS<<- c(PTS_1, PTS_2, PTS_3)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2, PTS_3)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS_3<- Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS_3<- (B_Adj * Score_3) + A_Adj
        PTS<- c(PTS_1,PTS_2, PTS_3)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope * Score_1
        PTS_2<- B_Slope * Score_2
        PTS_3<- B_Slope * Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2, PTS_3)
      }
      PTS<- round(PTS, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE3<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_3 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2, SE3)
        SE<- round(SE, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2), (Conf*SE3))
        CI<- round(CI, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_1<- rep(Cutoff_Score_1, 3)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_2<- rep(Cutoff_Score_2, 3)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_3<- rep(Cutoff_Score_3, 3)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3)
    }
    
  
    #Create dataframes to be displayed as tables in pdf report (statistical values used in reliable change analyses)
    
    if(input$RelChangeMethod == 'Nunnally & Bernstein (1994)') {
      Stats_Table_Fullscale<<- data.frame(Pop, M, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Chelune et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop, M, M_Retest, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Maassen et al. (2006)' | input$RelChangeMethod == 'McSweeny et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop, M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Crawford & Howell (1998)') {
      Stats_Table_Fullscale<<- data.frame(Pop, M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, SampleN, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
    } else if (input$RelChangeMethod == 'Jacobson & Truax (1991)' | input$RelChangeMethod == 'Speer (1992)') {
      Stats_Table_Fullscale<<- data.frame(Pop, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
    }
    
    #If custom confidence intervals options are selected, make sure that no values appear for PS and SE in pdf tables
    #Change the look of Param_Df tables in report if custom confidence intervals options are selected
    
    if (input$Select_CI == '2') {
      
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS = NA, SE = NA)
      Stats_Table_Fullscale<<- Stats_Table_Fullscale %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI[1])
    }
    
    #Create a dataframe to be used in the analytics dataset.
    
    #Calculate variables for significant improvement
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$CI_Lower_Lim[1]) {
      GAD7.Fullscale.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$CI_Lower_Lim[1]) {
      GAD7.Fullscale.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$CI_Upper_Lim[1]) {
      GAD7.Fullscale.Sig.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$CI_Upper_Lim[1]) {
      GAD7.Fullscale.Sig.Deterioration<- "No"
    }
    
    
    #Calculate variables for improvement
    
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$Score[1]) {
      GAD7.Fullscale.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$Score[1]) {
      GAD7.Fullscale.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$Score[1]) {
      GAD7.Fullscale.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$Score[1]) {
      GAD7.Fullscale.Deterioration<- "No"
    }
    
    
    
    GAD7.Fullscale.Change<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] - Entered_Scores_Df$Score[1]
    GAD7.Fullscale.Comparisons<- length(Entered_Scores_Df$Change) - 1
    GAD7.Fullscale.First.Date<- Entered_Scores_Df$Date[1]
    GAD7.Fullscale.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    GAD7.Fullscale.First.Score<- Entered_Scores_Df$Score[1]
    GAD7.Fullscale.Last.Score<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)]
    
    
    Analytics_Df<<- data.frame(GAD7.Fullscale.First.Date, GAD7.Fullscale.First.Score, GAD7.Fullscale.Comparisons, GAD7.Fullscale.Change, GAD7.Fullscale.Last.Date, GAD7.Fullscale.Last.Score, GAD7.Fullscale.Improvement,GAD7.Fullscale.Sig.Improvement, GAD7.Fullscale.Deterioration, GAD7.Fullscale.Sig.Deterioration)
    
    
    
  })
  
  
  
  #Create an expression to activate the entered data & create notifications to indicate inaccurate data entry
  
  Entered_Scores_Notifications<- observeEvent(input$Action_Submit_Data, {
    
    
    Entered_Scores_Reac()
    
    if(length(Score_1a) < 7) {
      showNotification("The GAD-7 is a 7-item scale. You have entered less than 7 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(length(Score_1a) > 7) {
      showNotification("The GAD-7 is a 7-item scale. You have entered more than 7 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(input$Timepoint != "1") {
      if(length(Score_2a) < 7) {
        showNotification("The GAD-7 is a 7-item scale. You have entered less than 7 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_2a) > 7) {
        showNotification("The GAD-7 is a 7-item scale. You have entered more than 7 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
    if(input$Timepoint == "3") {
      if(length(Score_3a) < 7) {
        showNotification("The GAD-7 is a 7-item scale. You have entered less than 7 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_3a) > 7) {
        showNotification("The GAD-7 is a 7-item scale. You have entered more than 7 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
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
    
    
    Entered_Scores_Df<<- data.frame(rbind(Imported_Scores_CSV_ToCombine, Entered_Scores_Df))
    
  })
  
  
  #Create an expression to activate the imported data
  
  Combine_Data_Warning<- observeEvent(input$Action_Combine, {
    
    Combine_Data_Reac()
    
    showModal(modalDialog(
      title = "Important Information About Importing & Combining Scores", footer = modalButton("Okay"),
      "When combining imported and newly-entered scores, ensure that you have used the same method for calculating confidence intervals.", br(), br(),
      "Also make sure that the total number of timepoints does not exceed 5. The patient's report cannot be generated if there is data for more than 5 timepoints."
    ))
    
    
  })
  
  
  observe({
    DL_Name<<- input$PatientName
  })
  
  #Create pdf download functionality
  

  output$report <- downloadHandler(
    
    filename = paste0(" GAD-7 Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
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
  
  
  
  #Set up functionality to export data to dataset for storage
  
  
  output$Display_Export_Data <- DT::renderDataTable({
    DT::datatable(Export_Reac(), extensions = 'FixedColumns', rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE, fixedColumns = list(leftColumns = 2)))
  })
  

  
  output$Download_Export_Data <- downloadHandler(
    
    
    filename = function() {
      paste(paste0(" GAD-7 Data ", format(Sys.time(), '%d/%m/%y')), input$Output_Filetype1, sep = ".")
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
      title = "Analytics Information", footer = modalButton("Okay"), "When using the PsychlytX web applications, you have the option of creating an analytics dataset. By using this dataset, you can gain
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
  
  
  #Display the updated analytics dataset
  
  output$Display_New_Analytics <- DT::renderDataTable({
    DT::datatable(tail(Custom_Analytics_Reac()), extensions = 'FixedColumns', rownames = FALSE, 
                  options = list(dom = "t", scrollX = TRUE, fixedColumns = list(leftColumns = 2)))
  })
  
  
  #Create download functionality for updated analytics dataset  
  
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













