
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
      menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "CPAQ"),
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
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| Chronic Pain Acceptance Questionnaire (CPAQ)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 750),
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
                "Baranoff, J., Hanrahan, S. J., Kapur, D., & Connor, J. P. (2014). Validation of the chronic pain acceptance questionnaire-8 in an Australian pain clinic sample. International Journal of Behavioral Medicine, 21(1), 177-185.", br(), br(), 
                "Fish, R. A., McGuire, B., Hogan, M., Morrison, T. G., & Stewart, I. (2010). Validation of the chronic pain acceptance questionnaire (CPAQ) in an internet sample and development and preliminary validation of the CPAQ-8. Pain, 149(3), 435-443.", br(), br(),  
                "Liu, Y., Wang, L., Wei, Y., Wang, X., Xu, T., & Sun, J. (2016). Validation of a Chinese version of the chronic pain acceptance questionnaire (CAPQ) and CPAQ-8 in chronic pain patients. Medicine, 95(33), e4339. doi:10.1097/MD.0000000000004339.", br(), br(), 
                "Rovner, G., Vowles, K. E., Gerdle, B., & Gillanders, D. (2015). Latent class analysis of the short and long forms of the chronic pain acceptance questionnaire: Further examination of patient subgroups. The Journal of Pain, 16(11), 1095-1105.", br(), br(), 
                "Vowles, K. E., McCracken, L. M., McLeod, C., & Eccleston, C. (2008). The chronic pain acceptance questionnaire: Confirmatory factor analysis and identification of patient subgroups. Pain, 140(2), 284-291." 
                 ),
        
        
        
        tabItem(tabName = "CPAQ",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel("Scale",
                             selectInput("Q_Version", h4(tags$strong("Select Version of Scale")), choices = c("CPAQ-8","CPAQ-20")),
                             wellPanel(style = "background-color: #ffffff; color: black",
                                       fluidRow(
                                         column(width = 12, h3(tags$strong("Chronic Pain Acceptance Questionnaire")), br(), br(),
                                         h4("Below you will find a list of statements. Please rate the truth of each statement as it applies to you. 
                                                        Use the following rating scale to make your choices. For instance, if you believe a statement is ‘Always True,’ 
                                                        you would write a 6 in the blank next to that statement."))
                                       ),
                                       fluidRow(
                                         h4(tags$strong(HTML('&emsp;'), HTML('&emsp;'), "0", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), 
                                                        "1", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), 
                                                        "2", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                                                        "3", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                                                        "4", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                                                        "5", HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                                                        "6"))
                                       ),
                                       fluidRow(
                                         div(
                                           h4(tags$strong(HTML('&emsp;'), "Never true", HTML('&emsp;'),
                                                          "Very rarely true", HTML('&emsp;'),
                                                          "Seldom true", HTML('&emsp;'),
                                                          "Sometimes true", HTML('&emsp;'),
                                                          "Often true", HTML('&emsp;'),
                                                          "Almost always true", HTML('&emsp;'),
                                                          "Always true"
                                           )))), br(), br(),
                                       

                                       fluidRow(
                                         uiOutput("CPAQ_Scale"),
                                         hr(),
                                         fluidRow(
                                           column(width = 4, dateInput("Q_Date", "Date",  format = "dd/mm/yyyy")),
                                           column(width = 4, textInput("Q_Name", "Name")),
                                           column(width = 4, textInput("Q_Clin_Name", "Clinician's Name"))
                                         )
                                       )
                                       
                             )
                             
                             ),
                    tabPanel("Enter Data",
                             selectInput("Version", h4(tags$strong("Select Version of Scale")), choices = c("CPAQ-8","CPAQ-20")),
                             fluidRow(
                               column(width = 12,
                                      titlePanel(span(tagList(icon("edit", lib = "font-awesome")), h4(tags$b("Enter Scores for Each Timepoint")),
                                                      conditionalPanel(condition = "input.Version == 'CPAQ-8'",
                                                      tags$ul(
                                                        tags$li(helpText(h5(tags$em(tags$b("Use commas to separate scores. Enter 8 scores in order, from the first to the last item of the total scale.", style = "color:black")))))
                                                      )
                                                                      ),
                                                      conditionalPanel(condition = "input.Version == 'CPAQ-20'",
                                                                       tags$ul(
                                                                         tags$li(helpText(h5(tags$em(tags$b("Use commas to separate scores. Enter 20 scores in order, from the first to the last item of the total scale.", style = "color:black")))))
                                                                       )
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
                                                      h4(tags$strong("Select the population")),
                                                      conditionalPanel(condition = "input.Version == 'CPAQ-20'",
                                                                       selectInput("Pop_20", "", choices = c("Chronic Pain"))
                                                      ),
                                                      conditionalPanel(condition = "input.Version == 'CPAQ-8'",
                                                                       selectInput("Pop_8", "", choices = c("Chronic Pain"))
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
                                                               selectInput("Select_CI_Activity_Engagement", label = "Activity Engagement",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Activity_Engagement == '2'",
                                                                                numericInput("Man_CI_Activity_Engagement", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Pain_Willingness", label = "Pain Willingness",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Pain_Willingness == '2'",
                                                                                numericInput("Man_CI_Pain_Willingness", "Specify the width of the confidence interval", value = 0))
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
                                                               uiOutput("Mean_Widg_Activity_Engagement")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Pain_Willingness")
                                                        )
                                                      ),
                                                      
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Chelune et al. (1993)' || input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)' || input.RelChangeMethod == 'Speer (1992)'",
                                                                       
                                                                       h4(tags$strong("Enter a mean value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                uiOutput("Retest_M_Widg")
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Activity_Engagement", "Activity Engagement", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                uiOutput("Retest_M_PW_Widg")
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
                                                               uiOutput("Sd_Widg_Activity_Engagement")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Pain_Willingness")
                                                        )
                                                      ),
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)'",
                                                                       
                                                                       h4(tags$strong("Enter a standard deviation value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                uiOutput("Retest_Sd_Widg")
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Activity_Engagement", "Activity Engagement", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                uiOutput("Retest_Sd_PW_Widg")
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
                                                               uiOutput("Cutoff_Widg_Activity_Engagement_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Pain_Willingness_1") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Second cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Activity_Engagement_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Pain_Willingness_2") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Third cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Activity_Engagement_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Pain_Willingness_3") 
                                                        )
                                                        
                                                      )
                                                      , hr()
                                                      
                                             ),
                                             
                                             
                                             h4(tags$strong("Psychometric Properties of the CPAQ Relevant to Assessing Reliable & Clinically Significant Change")),
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
                                              downloadButton("report", div(tags$strong("Step 3."), "Generate Patient Report"), class = "reportbutton",  width = '270px'),
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
                                                       h4(tags$strong("Step 1.")), h4("Create a New Service Analytics Dataset or Add to an Existing One"),
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
    
    input$Q_Version
    
    updateSelectInput(session, "Version", selected = input$Q_Version)
    
  })
  
  
  observe({
    
    input$Version
    
    updateSelectInput(session, "Q_Version", selected = input$Version)
    
  })
  
  
  
  
  
  #Create Questionnaires 
  
  output$CPAQ_Scale<- renderUI({
    if(input$Version == "CPAQ-20") {
    tagList(
      fluidRow(
        column(width = 1, textInput("Item_1", label = NULL)),
        column(width = 11, h4("1. I am getting on with the business of living no matter what my level of pain is."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_2", label = NULL)),
        column(width = 11, h4("2. My life is going well, even though I have chronic pain."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_3", label = NULL)),
        column(width = 11, h4("3. It’s OK to experience pain."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_4", label = NULL)),
        column(width = 11, h4("4. I would gladly sacrifice important things in my life to control this pain better."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_5", label = NULL)),
        column(width = 11, h4("5. It’s not necessary for me to control my pain in order to handle my life well."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_6", label = NULL)),
        column(width = 11, h4("6. Although things have changed, I am living a normal life despite my chronic pain."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_7", label = NULL)),
        column(width = 11, h4("7. I need to concentrate on getting ride of my pain."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_8", label = NULL)),
        column(width = 11, h4("8. There are many activities I do when I feel pain."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_9", label = NULL)),
        column(width = 11, h4("9. I lead a full life even though I have chronic pain."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_10", label = NULL)),
        column(width = 11, h4("10. Controlling my pain is less important than any other goals in my life."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_11", label = NULL)),
        column(width = 11, h4("11. My thoughts and feelings about pain must change before I can take important steps in my life."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_12", label = NULL)),
        column(width = 11, h4("12. Despite the pain, I am now sticking to a certain course in my life."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_13", label = NULL)),
        column(width = 11, h4("13. Keeping my pain level under control takes first priority whenever I’m doing something."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_14", label = NULL)),
        column(width = 11, h4("14. Before I can make any serious plans, I have to get some control over my pain."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_15", label = NULL)),
        column(width = 11, h4("15. When my pain increases, I can still take care of my responsibilities."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_16", label = NULL)),
        column(width = 11, h4("16. I will have better control over my life if I can control my negative thoughts about pain."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_17", label = NULL)),
        column(width = 11, h4("17. I avoid putting myself in situations where my pain might increase."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_18", label = NULL)),
        column(width = 11, h4("18. My worries and fears about what pain will do to me are true."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_19", label = NULL)),
        column(width = 11, h4("19. It’s a great relief to realize that I don’t have to change my pain to get on with life."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_20", label = NULL)),
        column(width = 11, h4("20. I have to struggle to do things when I have pain."))
      ), 
      fluidRow(
        column(width = 12, h5("Scale Source: McCraken, L. M., Vowles, K. E. & Eccleston, C. (2004). Acceptance of chronic pain: component analysis and a revised assessment method. Pain, 107, 159-166."))
      )
    )
    } else if(input$Version == "CPAQ-8") {
      
    tagList(
      fluidRow(
        column(width = 1, textInput("Item_1_Short", label = NULL)),
        column(width = 11, h4("1. I am getting on with the business of living no matter what my level of pain is."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_6_Short", label = NULL)),
        column(width = 11, h4("2. Although things have changed, I am living a normal life despite my chronic pain."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_9_Short", label = NULL)),
        column(width = 11, h4("3. I lead a full life even though I have chronic pain."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_15_Short", label = NULL)),
        column(width = 11, h4("4. When my pain increases, I can still take care of my responsibilities."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_13_Short", label = NULL)),
        column(width = 11, h4("5. Keeping my pain level under control takes first priority whenever I’m doing something."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_14_Short", label = NULL)),
        column(width = 11, h4("6. Before I can make any serious plans, I have to get some control over my pain."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_17_Short", label = NULL)),
        column(width = 11, h4("7. I avoid putting myself in situations where my pain might increase."))
      ),
      fluidRow(
        column(width = 1, textInput("Item_18_Short", label = NULL)),
        column(width = 11, h4("8. My worries and fears about what pain will do to me are true."))
      ), 
      fluidRow(
        column(width = 12, h5("Scale Source: McCraken, L. M., Vowles, K. E. & Eccleston, C. (2004). Acceptance of chronic pain: component analysis and a revised assessment method. Pain, 107, 159-166."))
      )
      
    )

  } 
       
  })
  
  
  
  #Questionnaire responses
  
  observe({
    if(input$Version == "CPAQ-20") {
      
    Q_Scores <- paste(input$Item_1, input$Item_2, input$Item_3, input$Item_4, input$Item_5, input$Item_6, input$Item_7, 
                      input$Item_8, input$Item_9, input$Item_10, input$Item_11, input$Item_12, input$Item_13, input$Item_14,
                      input$Item_15, input$Item_16, input$Item_17, input$Item_18, input$Item_19, input$Item_20,  sep = ",")
    } else if(input$Version == "CPAQ-8") {
      
      Q_Scores<- paste(input$Item_1_Short, input$Item_6_Short, input$Item_9_Short, input$Item_15_Short, input$Item_13_Short, 
                       input$Item_14_Short, input$Item_17_Short, input$Item_18_Short, sep = ",")
      
    }
    
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
    if(input$Version == "CPAQ-8") {
      #data field lab
      CPAQ_Lab<<- "CPAQ-8 Total Scale"
      AE_Lab<<- "Activity Engagement"
      PW_Lab<<- "Pain Willingness"
      #Graph info
      Graph_Up_Lim<<- 58
      Y_Vals<<- c(0,8,16,24,32,40,48)
      Y_Vals_Labs<<- c("0 Min","8","16","24","32","40","48 Max")
      Graph_Title<<- "Total CPAQ-8 Score"
      #Graph info AE
      Graph_Up_Lim_AE<<- 34
      Y_Vals_AE<<- c(0,4,8,12,16,20,24)
      Y_Vals_Labs_AE<<- c("0 Min","4","8","12","16","20","24 Max")
      #Graph info PW
      Graph_Up_Lim_PW<<- 34
      Y_Vals_PW<<- c(0,4,8,12,16,20,24)
      Y_Vals_Labs_PW<<- c("0 Min","4","8","12","16","20","24 Max")
      if(input$Pop_8 == "Chronic Pain") {
        Mean_Val<<- 19.26
        Sd_Val<<- 8.43
        Source_Mean<<- "Baranoff, Hanrahan, Kapur & Connor (2012)"
        Source_Sd<<- "Baranoff, Hanrahan, Kapur & Connor (2012)"
        Cut_Val_1<<- 7.4 
        Cut_Val_2<<- 19.3
        Cut_Val_3<<- 33.1
        Cut_Lab_1<<- "Low (Rovner et al., 2015)"
        Cut_Lab_2<<- "Medium (Rovner et al., 2015)"
        Cut_Lab_3<<- "High (Rovner et al., 2015)"
        Source_Cutoff_1<<- "Rovner, Vowles, Gerdle & Gillanders (2015)" 
        Source_Cutoff_2<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Source_Cutoff_3<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Mean_Val_Activity_Engagement<<- 11.34
        Sd_Val_Activity_Engagement<<- 5.31
        Source_Mean_Activity_Engagement<<- "Baranoff, Hanrahan, Kapur & Connor (2012)"
        Source_Sd_Activity_Engagement<<- "Baranoff, Hanrahan, Kapur & Connor (2012)"
        Cut_Val_Activity_Engagement_1<<- 4.2
        Cut_Val_Activity_Engagement_2<<- 8.2
        Cut_Val_Activity_Engagement_3<<- 16.6
        Cut_Lab_1_Activity_Engagement<<- "Low (Rovner et al., 2015)"
        Cut_Lab_2_Activity_Engagement<<- "Medium (Rovner et al., 2015)"
        Cut_Lab_3_Activity_Engagement<<- "High (Rovner et al., 2015)"
        Source_Cutoff_Activity_Engagement_1<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Source_Cutoff_Activity_Engagement_2<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Source_Cutoff_Activity_Engagement_3<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Mean_Val_Pain_Willingness<<- 5.9
        Sd_Val_Pain_Willingness<<- 5
        Source_Mean_Pain_Willingness<<- "Baranoff, Hanrahan, Kapur & Connor (2012)"
        Source_Sd_Pain_Willingness<<- "Baranoff, Hanrahan, Kapur & Connor (2012)"
        Cut_Val_Pain_Willingness_1<<- 3.2
        Cut_Val_Pain_Willingness_2<<- 11
        Cut_Val_Pain_Willingness_3<<- 16.5
        Cut_Lab_1_Pain_Willingness<<- "Low (Rovner et al., 2015)"
        Cut_Lab_2_Pain_Willingness<<- "Medium (Rovner et al., 2015)"
        Cut_Lab_3_Pain_Willingness<<- "High (Rovner et al., 2015)"
        Source_Cutoff_Pain_Willingness_1<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Source_Cutoff_Pain_Willingness_2<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Source_Cutoff_Pain_Willingness_3<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
      }
    } else if(input$Version == "CPAQ-20") {  
      #data field lab
      CPAQ_Lab<<- "CPAQ-20 Total Scale"
      AE_Lab<<- "Activity Engagement"
      PW_Lab<<- "Pain Willingness"
      #Graph info
      Graph_Up_Lim<<- 140
      Y_Vals<<- c(0,20,40,60,80,100,120)
      Y_Vals_Labs<<- c("0 Min","20","40","60","80","100","120 Max")
      Graph_Title<<- "Total CPAQ-20 Score"
      #Graph info AE
      Graph_Up_Lim_AE<<- 64 
      Y_Vals_AE<<- c(0,9,18,27,36,45,54)
      Y_Vals_Labs_AE<<-c("0 Min","9","18","27","36","45","54 Max") 
      #Graph info PW
      Graph_Up_Lim_PW<<- 76
      Y_Vals_PW<<- c(0,11,22,33,44,55,66)
      Y_Vals_Labs_PW<<- c("0 Min","11","22","33","44","55","66 Max")
      if(input$Pop_20 == "Chronic Pain") {
        Mean_Val<<- 48.48
        Sd_Val<<- 16.83
        Source_Mean<<- "Baranoff, Hanrahan, Kapur & Connor (2012)"
        Source_Sd<<- "Baranoff, Hanrahan, Kapur & Connor (2012)"
        Cut_Val_1<<- 23.6
        Cut_Val_2<<- 47.6
        Cut_Val_3<<- 74.9
        Cut_Lab_1<<- "Low (Rovner et al., 2015)"
        Cut_Lab_2<<- "Medium (Rovner et al., 2015)"
        Cut_Lab_3<<- "High (Rovner et al., 2015)"
        Source_Cutoff_1<<- "Rovner, Vowles, Gerdle & Gillanders (2015)" 
        Source_Cutoff_2<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Source_Cutoff_3<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Mean_Val_Activity_Engagement<<- 30.12
        Sd_Val_Activity_Engagement<<- 11.91
        Source_Mean_Activity_Engagement<<- "Baranoff, Hanrahan, Kapur & Connor (2012)"
        Source_Sd_Activity_Engagement<<- "Baranoff, Hanrahan, Kapur & Connor (2012)"
        Cut_Val_Activity_Engagement_1<<- 12.4
        Cut_Val_Activity_Engagement_2<<- 23.3
        Cut_Val_Activity_Engagement_3<<- 41.9
        Cut_Lab_1_Activity_Engagement<<- "Low (Rovner et al., 2015)"
        Cut_Lab_2_Activity_Engagement<<- "Medium (Rovner et al., 2015)"
        Cut_Lab_3_Activity_Engagement<<- "High (Rovner et al., 2015)"
        Source_Cutoff_Activity_Engagement_1<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Source_Cutoff_Activity_Engagement_2<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Source_Cutoff_Activity_Engagement_3<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Mean_Val_Pain_Willingness<<- 18.73
        Sd_Val_Pain_Willingness<<- 9.08
        Source_Mean_Pain_Willingness<<- "Baranoff, Hanrahan, Kapur & Connor (2012)"
        Source_Sd_Pain_Willingness<<- "Baranoff, Hanrahan, Kapur & Connor (2012)"
        Cut_Val_Pain_Willingness_1<<- 10.9
        Cut_Val_Pain_Willingness_2<<- 24.2
        Cut_Val_Pain_Willingness_3<<- 33
        Cut_Lab_1_Pain_Willingness<<- "Low (Rovner et al., 2015)"
        Cut_Lab_2_Pain_Willingness<<- "Medium (Rovner et al., 2015)"
        Cut_Lab_3_Pain_Willingness<<- "High (Rovner et al., 2015)"
        Source_Cutoff_Pain_Willingness_1<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Source_Cutoff_Pain_Willingness_2<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
        Source_Cutoff_Pain_Willingness_3<<- "Rovner, Vowles, Gerdle & Gillanders (2015)"
      } 
    }
  })
  

  output$Manual_CI_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      selectInput("Select_CI", label = paste0(CPAQ_Lab),
                  choices = list("No" = 1, "Yes" = 2),
                  selected = 1)
    )
  })
  outputOptions(output, "Manual_CI_Widg", suspendWhenHidden = FALSE)
  
  output$Mean_Widg<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean", paste0(CPAQ_Lab), Mean_Val),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Activity_Engagement<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Activity_Engagement", "Activity Engagement", Mean_Val_Activity_Engagement),
      h6(paste("Reference:", Source_Mean_Activity_Engagement))
    )
  })
  outputOptions(output, "Mean_Widg_Activity_Engagement", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Pain_Willingness<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Pain_Willingness", paste0(PW_Lab), Mean_Val_Pain_Willingness),
      h6(paste("Reference:", Source_Mean_Pain_Willingness))
    )
  })
  outputOptions(output, "Mean_Widg_Pain_Willingness", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd",  paste0(CPAQ_Lab), Sd_Val),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Activity_Engagement<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Activity_Engagement", "Activity Engagement", Sd_Val_Activity_Engagement),
      h6(paste("Reference:", Source_Sd_Activity_Engagement))
    )
  })
  outputOptions(output, "Sd_Widg_Activity_Engagement", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Pain_Willingness<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Pain_Willingness", paste0(PW_Lab), Sd_Val_Pain_Willingness),
      h6(paste("Reference:", Source_Sd_Pain_Willingness))
    )
  })
  outputOptions(output, "Sd_Widg_Pain_Willingness", suspendWhenHidden = FALSE)
  
  
  output$Retest_M_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Retest_Mean", paste0(CPAQ_Lab), value = 0)
    )
  })
  outputOptions(output, "Retest_M_Widg", suspendWhenHidden = FALSE)
  
  
  output$Retest_Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Retest_Sd", paste0(CPAQ_Lab), value = 0)
    )
  })
  outputOptions(output, "Retest_Sd_Widg", suspendWhenHidden = FALSE)
  
  
  output$Retest_M_PW_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Retest_Mean", paste0(PW_Lab), value = 0)
    )
  })
  
  
  output$Retest_Sd_PW_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Retest_Sd", paste0(PW_Lab), value = 0)
    )
  })
  outputOptions(output, "Retest_Sd_PW_Widg", suspendWhenHidden = FALSE)
  
  
  output$Retest_Widg<- renderUI({ 
    CI_Vals_Reac()
    tagList(
      fluidRow(
        column(width = 2,
               numericInput("Reliability", paste0(CPAQ_Lab), value = .81),
               h6("Reference: Fish, Hogan, Morrison, Stewart & McGuire (2013)")
        ),
        column(width = 2,
               numericInput("Reliability_Activity_Engagement", "Activity Engagement", value = .86),
               h6("Reference: Fish, Hogan, Morrison, Stewart & McGuire (2013)")
        ),
        column(width = 2,
               numericInput("Reliability_Pain_Willingness", paste0(PW_Lab), value = .68),
               h6("Reference: Fish, Hogan, Morrison, Stewart & McGuire (2013)")
        )
      )
    )
  })
  outputOptions(output, "Retest_Widg", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_1",  paste0(CPAQ_Lab), as.numeric(Cut_Val_1)),
      textInput("Cutoff_Text_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source_Cutoff_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Activity_Engagement_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Activity_Engagement_1", "Activity Engagement", as.numeric(Cut_Val_Activity_Engagement_1)),
      textInput("Cutoff_Text_Activity_Engagement_1", "Cut-Off Score Name", Cut_Lab_1_Activity_Engagement),
      h6(paste("Reference:", Source_Cutoff_Activity_Engagement_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Activity_Engagement_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Pain_Willingness_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Pain_Willingness_1", "Pain Willingness", as.numeric(Cut_Val_Pain_Willingness_1)),
      textInput("Cutoff_Text_Pain_Willingness_1", "Cut-Off Score Name", Cut_Lab_1_Pain_Willingness),
      h6(paste("Reference:", Source_Cutoff_Pain_Willingness_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Pain_Willingness_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_2",  paste0(CPAQ_Lab), as.numeric(Cut_Val_2)),
      textInput("Cutoff_Text_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source_Cutoff_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Activity_Engagement_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Activity_Engagement_2", "Activity Engagement", as.numeric(Cut_Val_Activity_Engagement_2)),
      textInput("Cutoff_Text_Activity_Engagement_2", "Cut-Off Score Name", Cut_Lab_2_Activity_Engagement),
      h6(paste("Reference:", Source_Cutoff_Activity_Engagement_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Activity_Engagement_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Pain_Willingness_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Pain_Willingness_2", "Pain Willingness", as.numeric(Cut_Val_Pain_Willingness_2)),
      textInput("Cutoff_Text_Pain_Willingness_2", "Cut-Off Score Name", Cut_Lab_2_Pain_Willingness),
      h6(paste("Reference:", Source_Cutoff_Pain_Willingness_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Pain_Willingness_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_3", paste0(CPAQ_Lab), as.numeric(Cut_Val_3)),
      textInput("Cutoff_Text_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source_Cutoff_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Activity_Engagement_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Activity_Engagement_3", "Activity Engagement", as.numeric(Cut_Val_Activity_Engagement_3)),
      textInput("Cutoff_Text_Activity_Engagement_3", "Cut-Off Score Name", Cut_Lab_3_Activity_Engagement),
      h6(paste("Reference:", Source_Cutoff_Activity_Engagement_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Activity_Engagement_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Pain_Willingness_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Pain_Willingness_3", "Pain Willingness", as.numeric(Cut_Val_Pain_Willingness_3)),
      textInput("Cutoff_Text_Pain_Willingness_3", "Cut-Off Score Name", Cut_Lab_3_Pain_Willingness),
      h6(paste("Reference:", Source_Cutoff_Pain_Willingness_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Pain_Willingness_3", suspendWhenHidden = FALSE)
  
  
  #Create a reactive expression defining entered values  
  
  Entered_Scores_Reac<- reactive({
    
    TP<- input$Timepoint
    
    PN<<- input$PatientName
    CN<<- input$ClinicianName
    
    if(input$Version == "CPAQ-8") {
      Pop<- input$Pop_8
    } else if(input$Version == "CPAQ-20") {
      Pop<- input$Pop_20
    }
    
    RelChangeMethod<- input$RelChangeMethod
    
    CI_Vals_Reac()
    
    M<- input$Pop_Mean
    SD<- input$Pop_Sd
    M_Activity_Engagement<- input$Pop_Mean_Activity_Engagement
    SD_Activity_Engagement<-input$Pop_Sd_Activity_Engagement
    M_Pain_Willingness<- input$Pop_Mean_Pain_Willingness
    SD_Pain_Willingness<- input$Pop_Sd_Pain_Willingness
    
    
    M_Retest<- input$Retest_Mean
    SD_Retest<- input$Retest_Sd
    M_Retest_Activity_Engagement<- input$Retest_Mean_Activity_Engagement
    SD_Retest_Activity_Engagement<- input$Retest_Sd_Activity_Engagement
    M_Retest_Pain_Willingness<- input$Retest_Mean_Pain_Willingness
    SD_Retest_Pain_Willingness<- input$Retest_Sd_Pain_Willingness
    
    
    SampleN<- input$SampleN
    
    Rel<- input$Reliability
    Rel_Activity_Engagement<- input$Reliability_Activity_Engagement
    Rel_Pain_Willingness<- input$Reliability_Pain_Willingness
    
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
      SE_Activity_Engagement<-SD_Activity_Engagement * sqrt(1 - Rel_Activity_Engagement^2)
      SE_Pain_Willingness<-SD_Pain_Willingness * sqrt(1 - Rel_Pain_Willingness^2)
      SE<- round(SE, digits = 2)
      SE_Activity_Engagement<- round(SE_Activity_Engagement, digits = 2)
      SE_Pain_Willingness<- round(SE_Pain_Willingness, digits = 2)
    } else if (input$RelChangeMethod == "Chelune et al. (1993)" | input$RelChangeMethod == "Jacobson & Truax (1991)" | input$RelChangeMethod == "Speer (1992)") {
      SE<- sqrt((2*(SD^2))*(1-Rel))
      SE_Activity_Engagement<- sqrt((2*(SD_Activity_Engagement^2))*(1-Rel_Activity_Engagement))
      SE_Pain_Willingness<- sqrt((2*(SD_Pain_Willingness^2))*(1-Rel_Pain_Willingness))
      SE<- round(SE, digits = 2)
      SE_Activity_Engagement<- round(SE_Activity_Engagement, digits = 2)
      SE_Pain_Willingness<- round(SE_Pain_Willingness, digits = 2)
    } else if (input$RelChangeMethod == "Maassen et al. (2006)") {
      SE<- sqrt((SD^2 + SD_Retest^2)*(1-Rel))
      SE_Activity_Engagement<- sqrt((SD_Activity_Engagement^2 + SD_Retest_Activity_Engagement^2)*(1-Rel_Activity_Engagement))
      SE_Pain_Willingness<- sqrt((SD_Pain_Willingness^2 + SD_Retest_Pain_Willingness^2)*(1-Rel_Pain_Willingness))
      SE<- round(SE, digits = 2)
      SE_Activity_Engagement<- round(SE_Activity_Engagement, digits = 2)
      SE_Pain_Willingness<- round(SE_Pain_Willingness, digits = 2)
    } else if (input$RelChangeMethod == "McSweeny et al. (1993)") {
      SE<- SD_Retest*sqrt(1 - Rel^2)
      SE_Activity_Engagement<- SD_Retest_Activity_Engagement*sqrt(1 - Rel_Activity_Engagement^2)
      SE_Pain_Willingness<- SD_Retest_Pain_Willingness*sqrt(1 - Rel_Pain_Willingness^2)
      SE<- round(SE, digits = 2)
      SE_Activity_Engagement<- round(SE_Activity_Engagement, digits = 2)
      SE_Pain_Willingness<- round(SE_Pain_Willingness, digits = 2)
    }
    
    
    #Caclculate McSweeny standard error to be employed in deriving the Crawford & Howell standard error if selected
    
    McSweeny_SE<- SD_Retest*sqrt(1 - Rel^2)
    McSweeny_SE_Activity_Engagement<- SD_Retest_Activity_Engagement*sqrt(1 - Rel_Activity_Engagement^2)
    McSweeny_SE_Pain_Willingness<- SD_Retest_Pain_Willingness*sqrt(1 - Rel_Pain_Willingness^2)
    
    #Generate object storing names of cut-off scores
    
    Cutoff_Name_1<- input$Cutoff_Text_1
    Cutoff_Name_2<- input$Cutoff_Text_2
    Cutoff_Name_3<- input$Cutoff_Text_3
    Cutoff_Name_Activity_Engagement_1<- input$Cutoff_Text_Activity_Engagement_1
    Cutoff_Name_Activity_Engagement_2<- input$Cutoff_Text_Activity_Engagement_2
    Cutoff_Name_Activity_Engagement_3<- input$Cutoff_Text_Activity_Engagement_3
    Cutoff_Name_Pain_Willingness_1<- input$Cutoff_Text_Pain_Willingness_1
    Cutoff_Name_Pain_Willingness_2<- input$Cutoff_Text_Pain_Willingness_2
    Cutoff_Name_Pain_Willingness_3<- input$Cutoff_Text_Pain_Willingness_3
    Cutoff_Names<<- data.frame(Cutoff_Name_1,Cutoff_Name_2,Cutoff_Name_3,Cutoff_Name_Activity_Engagement_1,Cutoff_Name_Activity_Engagement_2,Cutoff_Name_Activity_Engagement_3,
                               Cutoff_Name_Pain_Willingness_1, Cutoff_Name_Pain_Willingness_2, Cutoff_Name_Pain_Willingness_3)
    
    #Define the data & calculate PS, SE, confidence intervals and cut-off scores
    
    if(input$Timepoint == "1") {
      Date<- input$Date_1
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      if(input$Version == "CPAQ-20") {
        Recode<- car::recode(Score_1a[c(4,7,11,13,14,16,17,18,20)],'0=6;1=5;2=4;3=3;4=2;5=1;6=0')
        Score_1a[c(4,7,11,13,14,16,17,18,20)]<- Recode
        Item_Df<<- data.frame(Item = 1:length(Score_1a), Score = Score_1a)
        Score<- sum(Score_1a, na.rm = TRUE)
        Score_Activity_Engagement<- sum(Score_1a[c(1,2,3,5,6,8,9,10,12,15,19)], na.rm = TRUE)
        Score_Pain_Willingness<- sum(Score_1a[c(4,7,11,13,14,16,17,18,20)], na.rm = TRUE)
      } 
      if(input$Version == "CPAQ-8") {
        Recode<- car::recode(Score_1a[c(5,6,7,8)],'0=6;1=5;2=4;3=3;4=2;5=1;6=0')
        Score_1a[c(5,6,7,8)]<- Recode
        Item_Df<<- data.frame(Item = 1:length(Score_1a), Score = Score_1a)
        Score<- sum(Score_1a, na.rm = TRUE)
        Score_Activity_Engagement<- sum(Score_1a[c(1,2,3,4)], na.rm = TRUE)
        Score_Pain_Willingness<- sum(Score_1a[c(5,6,7,8)], na.rm = TRUE)
      }
      Change<- 0
      Change_Activity_Engagement<- 0
      Change_Pain_Willingness<- 0
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS<- (Rel * Score) + (M * (1 - Rel))
        PTS_Activity_Engagement<- (Rel_Activity_Engagement * Score_Activity_Engagement) + (M_Activity_Engagement * (1 - Rel_Activity_Engagement))
        PTS_Pain_Willingness<- (Rel_Pain_Willingness * Score_Pain_Willingness) + (M_Pain_Willingness * (1 - Rel_Pain_Willingness))
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS<- Score + (M_Retest - M)  
        PTS_Activity_Engagement<- Score_Activity_Engagement + (M_Retest_Activity_Engagement - M_Activity_Engagement)  
        PTS_Pain_Willingness<- Score_Pain_Willingness + (M_Retest_Pain_Willingness - M_Pain_Willingness)  
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Activity_Engagement<- Score_Activity_Engagement
        PTS_Pain_Willingness<- Score_Pain_Willingness
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS<- (B_Adj * Score) + A_Adj
        B_Slope_Activity_Engagement<- Rel_Activity_Engagement * (SD_Retest_Activity_Engagement/SD_Activity_Engagement)
        A_Constant_Activity_Engagement<- M_Retest_Activity_Engagement - (B_Slope_Activity_Engagement * M_Activity_Engagement)
        B_Adj_Activity_Engagement<- SD_Retest_Activity_Engagement/SD_Activity_Engagement
        A_Adj_Activity_Engagement<- M_Retest_Activity_Engagement - (B_Adj_Activity_Engagement * M_Activity_Engagement)
        PTS_Activity_Engagement<- (B_Adj_Activity_Engagement * Score_Activity_Engagement) + A_Adj_Activity_Engagement
        B_Slope_Pain_Willingness<- Rel_Pain_Willingness * (SD_Retest_Pain_Willingness/SD_Pain_Willingness)
        A_Constant_Pain_Willingness<- M_Retest_Pain_Willingness - (B_Slope_Pain_Willingness * M_Pain_Willingness)
        B_Adj_Pain_Willingness<- SD_Retest_Pain_Willingness/SD_Pain_Willingness
        A_Adj_Pain_Willingness<- M_Retest_Pain_Willingness - (B_Adj_Pain_Willingness * M_Pain_Willingness)
        PTS_Pain_Willingness<- (B_Adj_Pain_Willingness * Score_Pain_Willingness) + A_Adj_Pain_Willingness
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS<- B_Slope * Score
        B_Slope_Activity_Engagement<- Rel_Activity_Engagement * (SD_Retest_Activity_Engagement/SD_Activity_Engagement)
        PTS_Activity_Engagement<- B_Slope_Activity_Engagement * Score_Activity_Engagement
        B_Slope_Pain_Willingness<- Rel_Pain_Willingness * (SD_Retest_Pain_Willingness/SD_Pain_Willingness)
        PTS_Pain_Willingness<- B_Slope_Pain_Willingness * Score_Pain_Willingness
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS<- Score + (M_Retest - M)
        PTS_Activity_Engagement<- Score_Activity_Engagement + (M_Retest_Activity_Engagement - M_Activity_Engagement)
        PTS<- Pain_Willingness<- Score_Pain_Willingness + (M_Retest_Pain_Willingness - M_Pain_Willingness)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Activity_Engagement<- round(PTS_Activity_Engagement, digits = 2)
      PTS_Pain_Willingness<- round(PTS_Pain_Willingness, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score - M)^2/(SD^2*(SampleN-1))))
        SE_Activity_Engagement<- McSweeny_SE_Activity_Engagement*sqrt(1 + (1/SampleN) + ((Score_Activity_Engagement - M_Activity_Engagement)^2/(SD_Activity_Engagement^2*(SampleN-1))))
        SE_Pain_Willingness<- McSweeny_SE_Pain_Willingness*sqrt(1 + (1/SampleN) + ((Score_Pain_Willingness - M_Pain_Willingness)^2/(SD_Pain_Willingness^2*(SampleN-1))))
        SE<- round(SE, digits = 2)
        SE_Activity_Engagement<- round(SE_Activity_Engagement, digits = 2)
        SE_Pain_Willingness<- round(SE_Pain_Willingness, digits = 2)
        CI<- (Conf*SE)
        CI<- round(CI, digits = 2)
        CI_Activity_Engagement<- (Conf*SE_Activity_Engagement)
        CI_Activity_Engagement<- round(CI_Activity_Engagement, digits = 2)
        CI_Pain_Willingness<- (Conf*SE_Pain_Willingness)
        CI_Pain_Willingness<- round(CI_Pain_Willingness, digits = 2)
      }
      CI<- (Conf*SE)
      CI<- round(CI, digits = 2)
      CI_Activity_Engagement<- (Conf*SE_Activity_Engagement)
      CI_Activity_Engagement<- round(CI_Activity_Engagement, digits = 2)
      CI_Pain_Willingness<- (Conf*SE_Pain_Willingness)
      CI_Pain_Willingness<- round(CI_Pain_Willingness, digits = 2)
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Activity_Engagement<- PTS_Activity_Engagement + CI_Activity_Engagement
      CI_Upper_Lim_Activity_Engagement<- round(CI_Upper_Lim_Activity_Engagement, digits = 2)
      CI_Lower_Lim_Activity_Engagement<-PTS_Activity_Engagement - CI_Activity_Engagement
      CI_Lower_Lim_Activity_Engagement<- round(CI_Lower_Lim_Activity_Engagement, digits = 2)
      CI_Upper_Lim_Pain_Willingness<- PTS_Pain_Willingness + CI_Pain_Willingness
      CI_Upper_Lim_Pain_Willingness<- round(CI_Upper_Lim_Pain_Willingness, digits = 2)
      CI_Lower_Lim_Pain_Willingness<-PTS_Pain_Willingness - CI_Pain_Willingness
      CI_Lower_Lim_Pain_Willingness<- round(CI_Lower_Lim_Pain_Willingness, digits = 2)
      if(input$Select_CI == "2") {
        CI<- input$Man_CI
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Activity_Engagement == "2") {
        CI_Activity_Engagement<- input$Man_CI_Activity_Engagement
        CI_Activity_Engagement<- round(CI_Activity_Engagement, digits = 2)
        CI_Upper_Lim_Activity_Engagement<- Score_Activity_Engagement + CI_Activity_Engagement
        CI_Upper_Lim_Activity_Engagement<- round(CI_Upper_Lim_Activity_Engagement, digits = 2)
        CI_Lower_Lim_Activity_Engagement<- Score_Activity_Engagement - CI_Activity_Engagement
        CI_Lower_Lim_Activity_Engagement<- round(CI_Lower_Lim_Activity_Engagement, digits = 2)
      }
      if(input$Select_CI_Pain_Willingness == "2") {
        CI_Pain_Willingness<- input$Man_CI_Pain_Willingness
        CI_Pain_Willingness<- round(CI_Pain_Willingness, digits = 2)
        CI_Upper_Lim_Pain_Willingness<- Score_Pain_Willingness + CI_Pain_Willingness
        CI_Upper_Lim_Pain_Willingness<- round(CI_Upper_Lim_Pain_Willingness, digits = 2)
        CI_Lower_Lim_Pain_Willingness<- Score_Pain_Willingness - CI_Pain_Willingness
        CI_Lower_Lim_Pain_Willingness<- round(CI_Lower_Lim_Pain_Willingness, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Activity_Engagement_1<- round(input$Cutoff_Activity_Engagement_1, digits = 2)
      Cutoff_Score_Activity_Engagement_2<- round(input$Cutoff_Activity_Engagement_2, digits = 2)
      Cutoff_Score_Activity_Engagement_3<- round(input$Cutoff_Activity_Engagement_3, digits = 2)
      Cutoff_Score_Pain_Willingness_1<- round(input$Cutoff_Pain_Willingness_1, digits = 2)
      Cutoff_Score_Pain_Willingness_2<- round(input$Cutoff_Pain_Willingness_2, digits = 2)
      Cutoff_Score_Pain_Willingness_3<- round(input$Cutoff_Pain_Willingness_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Activity_Engagement,Change_Activity_Engagement,PTS_Activity_Engagement, SE_Activity_Engagement, CI_Upper_Lim_Activity_Engagement, CI_Lower_Lim_Activity_Engagement, Cutoff_Score_Activity_Engagement_1,Cutoff_Score_Activity_Engagement_2,Cutoff_Score_Activity_Engagement_3,
                                      Score_Pain_Willingness,Change_Pain_Willingness, PTS_Pain_Willingness, SE_Pain_Willingness, CI_Upper_Lim_Pain_Willingness, CI_Lower_Lim_Pain_Willingness, Cutoff_Score_Pain_Willingness_1,Cutoff_Score_Pain_Willingness_2,Cutoff_Score_Pain_Willingness_3)
    } else if(input$Timepoint == "2") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date<- c(Date_1, Date_2)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<- as.numeric(unlist(strsplit(input$Text_1,",")))
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      if(input$Version == "CPAQ-20") {
        Recode_1<- car::recode(Score_1a[c(4,7,11,13,14,16,17,18,20)],'0=6;1=5;2=4;3=3;4=2;5=1;6=0')
        Score_1a[c(4,7,11,13,14,16,17,18,20)]<- Recode_1
        Score_1<- sum(Score_1a, na.rm = TRUE)
        Score_Activity_Engagement_1<- sum(Score_1a[c(1,2,3,5,6,8,9,10,12,15,19)], na.rm = TRUE)
        Score_Pain_Willingness_1<- sum(Score_1a[c(4,7,11,13,14,16,17,18,20)], na.rm = TRUE)
        Recode_2<- car::recode(Score_2a[c(4,7,11,13,14,16,17,18,20)],'0=6;1=5;2=4;3=3;4=2;5=1;6=0')
        Score_2a[c(4,7,11,13,14,16,17,18,20)]<- Recode_2
        Item_Df<<- data.frame(Item = 1:length(Score_2a), Score = Score_2a)
        Score_2<- sum(Score_2a, na.rm = TRUE)
        Score_Activity_Engagement_2<- sum(Score_2a[c(1,2,3,5,6,8,9,10,12,15,19)], na.rm = TRUE)
        Score_Pain_Willingness_2<- sum(Score_2a[c(4,7,11,13,14,16,17,18,20)], na.rm = TRUE)
      } 
      if(input$Version == "CPAQ-8") {
        Recode<- car::recode(Score_1a[c(5,6,7,8)],'0=6; 1=5; 2=4; 3=3; 4=2; 5=1;6=0')
        Score_1a[c(5,6,7,8)]<- Recode
        Score_1<- sum(Score_1a, na.rm = TRUE)
        Score_Activity_Engagement_1<- sum(Score_1a[c(1,2,3,4)], na.rm = TRUE)
        Score_Pain_Willingness_1<- sum(Score_1a[c(5,6,7,8)], na.rm = TRUE)
        Recode_2<- car::recode(Score_2a[c(5,6,7,8)],'0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0')
        Score_2a[c(5,6,7,8)]<- Recode_2
        Item_Df<<- data.frame(Item = 1:length(Score_2a), Score = Score_2a)
        Score_2<- sum(Score_2a, na.rm = TRUE)
        Score_Activity_Engagement_2<- sum(Score_2a[c(1,2,3,5,6,8,9,10,12,15,19)], na.rm = TRUE)
        Score_Pain_Willingness_2<- sum(Score_2a[c(4,7,11,13,14,16,17,18,20)], na.rm = TRUE)
      }
      Score<- c(Score_1, Score_2)
      Score_Activity_Engagement<- c(Score_Activity_Engagement_1,Score_Activity_Engagement_2)
      Score_Pain_Willingness<- c(Score_Pain_Willingness_1,Score_Pain_Willingness_2)
      Change<- c(0, (Score_2 - Score_1))
      Change_Activity_Engagement<- c(0, (Score_Activity_Engagement_2 - Score_Activity_Engagement_1))
      Change_Pain_Willingness<- c(0, (Score_Pain_Willingness_2 - Score_Pain_Willingness_1))
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2)
        PTS_Activity_Engagement_1<- (Rel_Activity_Engagement * Score_Activity_Engagement_1) + (M_Activity_Engagement * (1 - Rel_Activity_Engagement))
        PTS_Activity_Engagement_2<- (Rel_Activity_Engagement * Score_Activity_Engagement_2) + (M_Activity_Engagement * (1 - Rel_Activity_Engagement))
        PTS_Activity_Engagement<-c(PTS_Activity_Engagement_1, PTS_Activity_Engagement_2)
        PTS_Pain_Willingness_1<- (Rel_Pain_Willingness * Score_Pain_Willingness_1) + (M_Pain_Willingness * (1 - Rel_Pain_Willingness))
        PTS_Pain_Willingness_2<- (Rel_Pain_Willingness * Score_Pain_Willingness_2) + (M_Pain_Willingness * (1 - Rel_Pain_Willingness))
        PTS_Pain_Willingness<-c(PTS_Pain_Willingness_1,PTS_Pain_Willingness_2)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2)
        PTS_Activity_Engagement_1<- Score_Activity_Engagement_1 + (M_Retest_Activity_Engagement - M_Activity_Engagement)  
        PTS_Activity_Engagement_2<- Score_Activity_Engagement_2 + (M_Retest_Activity_Engagement - M_Activity_Engagement)
        PTS_Activity_Engagement<- c(PTS_Activity_Engagement_1, PTS_Activity_Engagement_2)
        PTS_Pain_Willingness_1<- Score_Pain_Willingness_1 + (M_Retest_Pain_Willingness - M_Pain_Willingness)  
        PTS_Pain_Willingness_2<- Score_Pain_Willingness_2 + (M_Retest_Pain_Willingness - M_Pain_Willingness)
        PTS_Pain_Willingness<- c(PTS_Pain_Willingness_1, PTS_Pain_Willingness_2)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS<- c(PTS_1,PTS_2)
        PTS_Activity_Engagement_1<- Score_Activity_Engagement_1
        PTS_Activity_Engagement_2<- Score_Activity_Engagement_2
        PTS_Activity_Engagement<- c(PTS_Activity_Engagement_1, PTS_Activity_Engagement_2)
        PTS_Pain_Willingness_1<- Score_Pain_Willingness_1
        PTS_Pain_Willingness_2<- Score_Pain_Willingness_2
        PTS_Pain_Willingness<- c(PTS_Pain_Willingness_1, PTS_Pain_Willingness_2)
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Activity_Engagement<- Rel_Activity_Engagement * (SD_Retest_Activity_Engagement/SD_Activity_Engagement)
        A_Constant_Activity_Engagement<- M_Retest_Activity_Engagement - (B_Slope_Activity_Engagement * M_Activity_Engagement)
        B_Adj_Activity_Engagement<- SD_Retest_Activity_Engagement/SD_Activity_Engagement
        A_Adj_Activity_Engagement<- M_Retest_Activity_Engagement - (B_Adj_Activity_Engagement * M_Activity_Engagement)
        PTS_Activity_Engagement_1<- (B_Adj_Activity_Engagement * Score_Activity_Engagement_1) + A_Adj_Activity_Engagement
        PTS_Activity_Engagement_2<- (B_Adj_Activity_Engagement * Score_Activity_Engagement_2) + A_Adj_Activity_Engagement
        PTS_Activity_Engagement<- c(PTS_Activity_Engagement_1,PTS_Activity_Engagement_2)
        B_Slope_Pain_Willingness<- Rel_Pain_Willingness * (SD_Retest_Pain_Willingness/SD_Pain_Willingness)
        A_Constant_Pain_Willingness<- M_Retest_Pain_Willingness - (B_Slope_Pain_Willingness * M_Pain_Willingness)
        B_Adj_Pain_Willingness<- SD_Retest_Pain_Willingness/SD_Pain_Willingness
        A_Adj_Pain_Willingness<- M_Retest_Pain_Willingness - (B_Adj_Pain_Willingness * M_Pain_Willingness)
        PTS_Pain_Willingness_1<- (B_Adj_Pain_Willingness * Score_Pain_Willingness_1) + A_Adj_Pain_Willingness
        PTS_Pain_Willingness_2<- (B_Adj_Pain_Willingness * Score_Pain_Willingness_2) + A_Adj_Pain_Willingness
        PTS_Pain_Willingness<- c(PTS_Pain_Willingness_1,PTS_Pain_Willingness_2)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Activity_Engagement<- Rel_Activity_Engagement * (SD_Retest_Activity_Engagement/SD_Activity_Engagement)
        PTS_Activity_Engagement_1<- B_Slope_Activity_Engagement * Score_Activity_Engagement_1
        PTS_Activity_Engagement_2<- B_Slope_Activity_Engagement * Score_Activity_Engagement_2
        PTS_Activity_Engagement<- c(PTS_Activity_Engagement_1, PTS_Activity_Engagement_2)
        B_Slope_Pain_Willingness<- Rel_Pain_Willingness * (SD_Retest_Pain_Willingness/SD_Pain_Willingness)
        PTS_Pain_Willingness_1<- B_Slope_Pain_Willingness * Score_Pain_Willingness_1
        PTS_Pain_Willingness_2<- B_Slope_Pain_Willingness * Score_Pain_Willingness_2
        PTS_Pain_Willingness<- c(PTS_Pain_Willingness_1, PTS_Pain_Willingness_2)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2)
        PTS_Activity_Engagement_1<- Score_Activity_Engagement_1 + (M_Retest_Activity_Engagement - M_Activity_Engagement)
        PTS_Activity_Engagement_2<- Score_Activity_Engagement_2 + (M_Retest_Activity_Engagement - M_Activity_Engagement)
        PTS_Activity_Engagement<- c(PTS_Activity_Engagement_1, PTS_Activity_Engagement_2)
        PTS_Pain_Willingness_1<- Score_Pain_Willingness_1 + (M_Retest_Pain_Willingness - M_Pain_Willingness)
        PTS_Pain_Willingness_2<- Score_Pain_Willingness_2 + (M_Retest_Pain_Willingness - M_Pain_Willingness)
        PTS_Pain_Willingness<- c(PTS_Pain_Willingness_1, PTS_Pain_Willingness_2)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Activity_Engagement<- round(PTS_Activity_Engagement, digits = 2)
      PTS_Pain_Willingness<- round(PTS_Pain_Willingness, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2)
        SE_Activity_Engagement_1<- McSweeny_SE_Activity_Engagement*sqrt(1 + (1/SampleN) + ((Score_Activity_Engagement_1 - M_Activity_Engagement)^2/(SD_Activity_Engagement^2*(SampleN-1))))
        SE_Activity_Engagement_2<- McSweeny_SE_Activity_Engagement*sqrt(1 + (1/SampleN) + ((Score_Activity_Engagement_2 - M_Activity_Engagement)^2/(SD_Activity_Engagement^2*(SampleN-1))))
        SE_Activity_Engagement<- c(SE_Activity_Engagement_1, SE_Activity_Engagement_2)
        SE_Pain_Willingness_1<- McSweeny_SE_Pain_Willingness*sqrt(1 + (1/SampleN) + ((Score_Pain_Willingness_1 - M_Pain_Willingness)^2/(SD_Pain_Willingness^2*(SampleN-1))))
        SE_Pain_Willingness_2<- McSweeny_SE_Pain_Willingness*sqrt(1 + (1/SampleN) + ((Score_Pain_Willingness_2 - M_Pain_Willingness)^2/(SD_Pain_Willingness^2*(SampleN-1))))
        SE_Pain_Willingness<-c(SE_Pain_Willingness_1, SE_Pain_Willingness_2)
        SE<- round(SE, digits = 2)
        SE_Activity_Engagement<- round(SE_Activity_Engagement, digits = 2)
        SE_Pain_Willingness<- round(SE_Pain_Willingness, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2))
        CI<- round(CI, digits = 2)
        CI_Activity_Engagement<- c((Conf*SE_Activity_Engagement_1), (Conf*SE_Activity_Engagement_2))
        CI_Activity_Engagement<- round(CI_Activity_Engagement, digits = 2)
        CI_Pain_Willingness<- c((Conf*SE_Pain_Willingness_1), (Conf*SE_Pain_Willingness_2))
        CI_Pain_Willingness<- round(CI_Pain_Willingness, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Activity_Engagement<- c((Conf*SE_Activity_Engagement), (Conf*SE_Activity_Engagement))
        CI_Activity_Engagement<- round(CI_Activity_Engagement, digits = 2)
        CI_Pain_Willingness<- c((Conf*SE_Pain_Willingness), (Conf*SE_Pain_Willingness))
        CI_Pain_Willingness<- round(CI_Pain_Willingness, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Activity_Engagement<- PTS_Activity_Engagement + CI_Activity_Engagement
      CI_Upper_Lim_Activity_Engagement<- round(CI_Upper_Lim_Activity_Engagement, digits = 2)
      CI_Lower_Lim_Activity_Engagement<-PTS_Activity_Engagement - CI_Activity_Engagement
      CI_Lower_Lim_Activity_Engagement<- round(CI_Lower_Lim_Activity_Engagement, digits = 2)
      CI_Upper_Lim_Pain_Willingness<- PTS_Pain_Willingness + CI_Pain_Willingness
      CI_Upper_Lim_Pain_Willingness<- round(CI_Upper_Lim_Pain_Willingness, digits = 2)
      CI_Lower_Lim_Pain_Willingness<-PTS_Pain_Willingness - CI_Pain_Willingness
      CI_Lower_Lim_Pain_Willingness<- round(CI_Lower_Lim_Pain_Willingness, digits = 2)
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Activity_Engagement == "2") {
        CI_Activity_Engagement<- input$Man_CI_Activity_Engagement
        CI_Activity_Engagement<- c(CI_Activity_Engagement, CI_Activity_Engagement)
        CI_Activity_Engagement<- round(CI_Activity_Engagement, digits = 2)
        CI_Upper_Lim_Activity_Engagement<- Score_Activity_Engagement + CI_Activity_Engagement
        CI_Upper_Lim_Activity_Engagement<- round(CI_Upper_Lim_Activity_Engagement, digits = 2)
        CI_Lower_Lim_Activity_Engagement<- Score_Activity_Engagement - CI_Activity_Engagement
        CI_Lower_Lim_Activity_Engagement<- round(CI_Lower_Lim_Activity_Engagement, digits = 2)
      }
      if(input$Select_CI_Pain_Willingness == "2") {
        CI_Pain_Willingness<- input$Man_CI_Pain_Willingness
        CI_Pain_Willingness<- c(CI_Pain_Willingness, CI_Pain_Willingness)
        CI_Pain_Willingness<- round(CI_Pain_Willingness, digits = 2)
        CI_Upper_Lim_Pain_Willingness<- Score_Pain_Willingness + CI_Pain_Willingness
        CI_Upper_Lim_Pain_Willingness<- round(CI_Upper_Lim_Pain_Willingness, digits = 2)
        CI_Lower_Lim_Pain_Willingness<- Score_Pain_Willingness - CI_Pain_Willingness
        CI_Lower_Lim_Pain_Willingness<- round(CI_Lower_Lim_Pain_Willingness, digits = 2)
      } 
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_1<- rep(Cutoff_Score_1, 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_2<- rep(Cutoff_Score_2, 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_3<- rep(Cutoff_Score_3, 2)
      Cutoff_Score_Activity_Engagement_1<- round(input$Cutoff_Activity_Engagement_1, digits = 2)
      Cutoff_Score_Activity_Engagement_1<- rep(Cutoff_Score_Activity_Engagement_1, 2)
      Cutoff_Score_Activity_Engagement_2<- round(input$Cutoff_Activity_Engagement_2, digits = 2)
      Cutoff_Score_Activity_Engagement_2<- rep(Cutoff_Score_Activity_Engagement_2, 2)
      Cutoff_Score_Activity_Engagement_3<- round(input$Cutoff_Activity_Engagement_3, digits = 2)
      Cutoff_Score_Activity_Engagement_3<- rep(Cutoff_Score_Activity_Engagement_3, 2)
      Cutoff_Score_Pain_Willingness_1<- round(input$Cutoff_Pain_Willingness_1, digits = 2)
      Cutoff_Score_Pain_Willingness_1<- rep(Cutoff_Score_Pain_Willingness_1, 2)
      Cutoff_Score_Pain_Willingness_2<- round(input$Cutoff_Pain_Willingness_2, digits = 2)
      Cutoff_Score_Pain_Willingness_2<- rep(Cutoff_Score_Pain_Willingness_2, 2)
      Cutoff_Score_Pain_Willingness_3<- round(input$Cutoff_Pain_Willingness_3, digits = 2)
      Cutoff_Score_Pain_Willingness_3<- rep(Cutoff_Score_Pain_Willingness_3, 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Activity_Engagement,Change_Activity_Engagement,PTS_Activity_Engagement, SE_Activity_Engagement, CI_Upper_Lim_Activity_Engagement, CI_Lower_Lim_Activity_Engagement, Cutoff_Score_Activity_Engagement_1,Cutoff_Score_Activity_Engagement_2,Cutoff_Score_Activity_Engagement_3,
                                      Score_Pain_Willingness,Change_Pain_Willingness, PTS_Pain_Willingness, SE_Pain_Willingness, CI_Upper_Lim_Pain_Willingness, CI_Lower_Lim_Pain_Willingness, Cutoff_Score_Pain_Willingness_1,Cutoff_Score_Pain_Willingness_2,Cutoff_Score_Pain_Willingness_3)
    } else if(input$Timepoint == "3") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date_3<- input$Date_3
      Date<- c(Date_1, Date_2, Date_3)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<- as.numeric(unlist(strsplit(input$Text_1,",")))
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Score_3a<<- as.numeric(unlist(strsplit(input$Text_3,",")))
      if(input$Version == "CPAQ-20") {
        Recode_1<- car::recode(Score_1a[c(4,7,11,13,14,16,17,18,20)],'0=6;1=5;2=4;3=3;4=2;5=1;6=0')
        Score_1a[c(4,7,11,13,14,16,17,18,20)]<- Recode_1
        Score_1<- sum(Score_1a, na.rm = TRUE)
        Score_Activity_Engagement_1<- sum(Score_1a[c(1,2,3,5,6,8,9,10,12,15,19)], na.rm = TRUE)
        Score_Pain_Willingness_1<- sum(Score_1a[c(4,7,11,13,14,16,17,18,20)], na.rm = TRUE)
        Recode_2<- car::recode(Score_2a[c(4,7,11,13,14,16,17,18,20)],'0=6;1=5;2=4;3=3;4=2;5=1;6=0')
        Score_2a[c(4,7,11,13,14,16,17,18,20)]<- Recode_2
        Score_2<- sum(Score_2a, na.rm = TRUE)
        Score_Activity_Engagement_2<- sum(Score_2a[c(1,2,3,5,6,8,9,10,12,15,19)], na.rm = TRUE)
        Score_Pain_Willingness_2<- sum(Score_2a[c(4,7,11,13,14,16,17,18,20)], na.rm = TRUE)
        Recode_3<- car::recode(Score_3a[c(4,7,11,13,14,16,17,18,20)],'0=6;1=5;2=4;3=3;4=2;5=1;6=0')
        Score_3a[c(4,7,11,13,14,16,17,18,20)]<- Recode_3
        Item_Df<<- data.frame(Item = 1:length(Score_3a), Score = Score_3a)
        Score_3<- sum(Score_3a, na.rm = TRUE)
        Score_Activity_Engagement_3<- sum(Score_3a[c(1,2,3,5,6,8,9,10,12,15,19)], na.rm = TRUE)
        Score_Pain_Willingness_3<- sum(Score_3a[c(4,7,11,13,14,16,17,18,20)], na.rm = TRUE)
      } 
      if(input$Version == "CPAQ-8") {
        Recode<- car::recode(Score_1a[c(5,6,7,8)],'0=6;1=5;2=4;3=3;4=2;5=1;6=0')
        Score_1a[c(5,6,7,8)]<- Recode
        Score_1<- sum(Score_1a, na.rm = TRUE)
        Score_Activity_Engagement_1<- sum(Score_1a[c(1,2,3,4)], na.rm = TRUE)
        Score_Pain_Willingness_1<- sum(Score_1a[c(5,6,7,8)], na.rm = TRUE)
        Recode_2<- car::recode(Score_2a[c(5,6,7,8)],'0=6; 1=5; 2=4; 3=3; 4=2; 5=1; 6=0')
        Score_2a[c(5,6,7,8)]<- Recode_2
        Score_2<- sum(Score_2a, na.rm = TRUE)
        Score_Activity_Engagement_2<- sum(Score_2a[c(1,2,3,5,6,8,9,10,12,15,19)], na.rm = TRUE)
        Score_Pain_Willingness_2<- sum(Score_2a[c(4,7,11,13,14,16,17,18,20)], na.rm = TRUE)
        Recode_3<- car::recode(Score_3a[c(5,6,7,8)],'0=6;1=5;2=4;3=3;4=2;5=1;6=0')
        Score_3a[c(5,6,7,8)]<- Recode_3
        Item_Df<<- data.frame(Item = 1:length(Score_3a), Score = Score_3a)
        Score_3<- sum(Score_3a, na.rm = TRUE)
        Score_Activity_Engagement_3<- sum(Score_3a[c(1,2,3,5,6,8,9,10,12,15,19)], na.rm = TRUE)
        Score_Pain_Willingness_3<- sum(Score_3a[c(4,7,11,13,14,16,17,18,20)], na.rm = TRUE)
      }
      Score<- c(Score_1, Score_2, Score_3)
      Score<- round(Score, digits = 2)
      Score_Activity_Engagement<- c(Score_Activity_Engagement_1,Score_Activity_Engagement_2, Score_Activity_Engagement_3)
      Score_Activity_Engagement<- round(Score_Activity_Engagement, digits = 2)
      Score_Pain_Willingness<- c(Score_Pain_Willingness_1,Score_Pain_Willingness_2, Score_Pain_Willingness_3)
      Score_Pain_Willingness<- round(Score_Pain_Willingness, digits = 2)
      Change<- c(0, (Score_2 - Score_1), (Score_3 - Score_2))
      Change<- round(Change, digits = 2)
      Change_Activity_Engagement<- c(0, Score_Activity_Engagement_2 - Score_Activity_Engagement_1, Score_Activity_Engagement_3 - Score_Activity_Engagement_2)
      Change_Activity_Engagement<- round(Change_Activity_Engagement, digits = 2)
      Change_Pain_Willingness<- c(0, Score_Pain_Willingness_2 - Score_Pain_Willingness_1, Score_Pain_Willingness_3 - Score_Pain_Willingness_2)
      Change_Pain_Willingness<- round(Change_Pain_Willingness, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS_3<- (Rel * Score_3) + (M * (1 - Rel))
        PTS<<- c(PTS_1, PTS_2, PTS_3)
        PTS_Activity_Engagement_1<- (Rel_Activity_Engagement * Score_Activity_Engagement_1) + (M_Activity_Engagement * (1 - Rel_Activity_Engagement))
        PTS_Activity_Engagement_2<- (Rel_Activity_Engagement * Score_Activity_Engagement_2) + (M_Activity_Engagement * (1 - Rel_Activity_Engagement))
        PTS_Activity_Engagement_3<- (Rel_Activity_Engagement * Score_Activity_Engagement_3) + (M_Activity_Engagement * (1 - Rel_Activity_Engagement))
        PTS_Activity_Engagement<<- c(PTS_Activity_Engagement_1, PTS_Activity_Engagement_2, PTS_Activity_Engagement_3)
        PTS_Pain_Willingness_1<- (Rel_Pain_Willingness * Score_Pain_Willingness_1) + (M_Pain_Willingness * (1 - Rel_Pain_Willingness))
        PTS_Pain_Willingness_2<- (Rel_Pain_Willingness * Score_Pain_Willingness_2) + (M_Pain_Willingness * (1 - Rel_Pain_Willingness))
        PTS_Pain_Willingness_3<- (Rel_Pain_Willingness * Score_Pain_Willingness_3) + (M_Pain_Willingness * (1 - Rel_Pain_Willingness))
        PTS_Pain_Willingness<- c(PTS_Pain_Willingness_1,PTS_Pain_Willingness_2, PTS_Pain_Willingness_3)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Activity_Engagement_1<- Score_Activity_Engagement_1 + (M_Retest_Activity_Engagement - M_Activity_Engagement)  
        PTS_Activity_Engagement_2<- Score_Activity_Engagement_2 + (M_Retest_Activity_Engagement - M_Activity_Engagement)
        PTS_Activity_Engagement_3<- Score_Activity_Engagement_3 + (M_Retest_Activity_Engagement - M_Activity_Engagement)
        PTS_Activity_Engagement<- c(PTS_Activity_Engagement_1, PTS_Activity_Engagement_2, PTS_Activity_Engagement_3)
        PTS_Pain_Willingness_1<- Score_Pain_Willingness_1 + (M_Retest_Pain_Willingness - M_Pain_Willingness)  
        PTS_Pain_Willingness_2<- Score_Pain_Willingness_2 + (M_Retest_Pain_Willingness - M_Pain_Willingness)
        PTS_Pain_Willingness_3<- Score_Pain_Willingness_3 + (M_Retest_Pain_Willingness - M_Pain_Willingness)
        PTS_Pain_Willingness<- c(PTS_Pain_Willingness_1, PTS_Pain_Willingness_2, PTS_Pain_Willingness_3)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS_3<- Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Activity_Engagement_1<- Score_Activity_Engagement_1
        PTS_Activity_Engagement_2<- Score_Activity_Engagement_2
        PTS_Activity_Engagement_3<- Score_Activity_Engagement_3
        PTS_Activity_Engagement<- c(PTS_Activity_Engagement_1, PTS_Activity_Engagement_2, PTS_Activity_Engagement_3)
        PTS_Pain_Willingness_1<- Score_Pain_Willingness_1
        PTS_Pain_Willingness_2<- Score_Pain_Willingness_2
        PTS_Pain_Willingness_3<- Score_Pain_Willingness_3
        PTS_Pain_Willingness<- c(PTS_Pain_Willingness_1, PTS_Pain_Willingness_2, PTS_Pain_Willingness_3)
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS_3<- (B_Adj * Score_3) + A_Adj
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Activity_Engagement<- Rel_Activity_Engagement * (SD_Retest_Activity_Engagement/SD_Activity_Engagement)
        A_Constant_Activity_Engagement<- M_Retest_Activity_Engagement - (B_Slope_Activity_Engagement * M_Activity_Engagement)
        B_Adj_Activity_Engagement<- SD_Retest_Activity_Engagement/SD_Activity_Engagement
        A_Adj_Activity_Engagement<- M_Retest_Activity_Engagement - (B_Adj_Activity_Engagement * M_Activity_Engagement)
        PTS_Activity_Engagement_1<- (B_Adj_Activity_Engagement * Score_Activity_Engagement_1) + A_Adj_Activity_Engagement
        PTS_Activity_Engagement_2<- (B_Adj_Activity_Engagement * Score_Activity_Engagement_2) + A_Adj_Activity_Engagement
        PTS_Activity_Engagement_3<- (B_Adj_Activity_Engagement * Score_Activity_Engagement_3) + A_Adj_Activity_Engagement
        PTS_Activity_Engagement<- c(PTS_Activity_Engagement_1, PTS_Activity_Engagement_2, PTS_Activity_Engagement_3)
        B_Slope_Pain_Willingness<- Rel_Pain_Willingness * (SD_Retest_Pain_Willingness/SD_Pain_Willingness)
        A_Constant_Pain_Willingness<- M_Retest_Pain_Willingness - (B_Slope_Pain_Willingness * M_Pain_Willingness)
        B_Adj_Pain_Willingness<- SD_Retest_Pain_Willingness/SD_Pain_Willingness
        A_Adj_Pain_Willingness<- M_Retest_Pain_Willingness - (B_Adj_Pain_Willingness * M_Pain_Willingness)
        PTS_Pain_Willingness_1<- (B_Adj_Pain_Willingness * Score_Pain_Willingness_1) + A_Adj_Pain_Willingness
        PTS_Pain_Willingness_2<- (B_Adj_Pain_Willingness * Score_Pain_Willingness_2) + A_Adj_Pain_Willingness
        PTS_Pain_Willingness_3<- (B_Adj_Pain_Willingness * Score_Pain_Willingness_3) + A_Adj_Pain_Willingness
        PTS_Pain_Willingness<- c(PTS_Pain_Willingness_1, PTS_Pain_Willingness_2, PTS_Pain_Willingness_3)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope * Score_1
        PTS_2<- B_Slope * Score_2
        PTS_3<- B_Slope * Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Activity_Engagement<- Rel_Activity_Engagement * (SD_Retest_Activity_Engagement/SD_Activity_Engagement)
        PTS_Activity_Engagement_1<- B_Slope_Activity_Engagement * Score_Activity_Engagement_1
        PTS_Activity_Engagement_2<- B_Slope_Activity_Engagement * Score_Activity_Engagement_2
        PTS_Activity_Engagement_3<- B_Slope_Activity_Engagement * Score_Activity_Engagement_3
        PTS_Activity_Engagement<- c(PTS_Activity_Engagement_1, PTS_Activity_Engagement_2, PTS_Activity_Engagement_3)
        B_Slope_Pain_Willingness<- Rel_Pain_Willingness * (SD_Retest_Pain_Willingness/SD_Pain_Willingness)
        PTS_Pain_Willingness_1<- B_Slope_Pain_Willingness * Score_Pain_Willingness_1
        PTS_Pain_Willingness_2<- B_Slope_Pain_Willingness * Score_Pain_Willingness_2
        PTS_Pain_Willingness_3<- B_Slope_Pain_Willingness * Score_Pain_Willingness_3
        PTS_Pain_Willingness<- c(PTS_Pain_Willingness_1, PTS_Pain_Willingness_2, PTS_Pain_Willingness_3)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Activity_Engagement_1<- Score_Activity_Engagement_1 + (M_Retest_Activity_Engagement - M_Activity_Engagement)
        PTS_Activity_Engagement_2<- Score_Activity_Engagement_2 + (M_Retest_Activity_Engagement - M_Activity_Engagement)
        PTS_Activity_Engagement_3<- Score_Activity_Engagement_3 + (M_Retest_Activity_Engagement - M_Activity_Engagement)
        PTS_Activity_Engagement<- c(PTS_Activity_Engagement_1, PTS_Activity_Engagement_2, PTS_Activity_Engagement_3)
        PTS_Pain_Willingness_1<- Score_Pain_Willingness_1 + (M_Retest_Pain_Willingness - M_Pain_Willingness)
        PTS_Pain_Willingness_2<- Score_Pain_Willingness_2 + (M_Retest_Pain_Willingness - M_Pain_Willingness)
        PTS_Pain_Willingness_3<- Score_Pain_Willingness_3 + (M_Retest_Pain_Willingness - M_Pain_Willingness)
        PTS_Pain_Willingness<- c(PTS_Pain_Willingness_1, PTS_Pain_Willingness_2, PTS_Pain_Willingness_3)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Activity_Engagement<- round(PTS_Activity_Engagement, digits = 2)
      PTS_Pain_Willingness<- round(PTS_Pain_Willingness, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE3<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_3 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2, SE3)
        SE_Activity_Engagement_1<- McSweeny_SE_Activity_Engagement*sqrt(1 + (1/SampleN) + ((Score_Activity_Engagement_1 - M_Activity_Engagement)^2/(SD_Activity_Engagement^2*(SampleN-1))))
        SE_Activity_Engagement_2<- McSweeny_SE_Activity_Engagement*sqrt(1 + (1/SampleN) + ((Score_Activity_Engagement_2 - M_Activity_Engagement)^2/(SD_Activity_Engagement^2*(SampleN-1))))
        SE_Activity_Engagement_3<- McSweeny_SE_Activity_Engagement*sqrt(1 + (1/SampleN) + ((Score_Activity_Engagement_3 - M_Activity_Engagement)^2/(SD_Activity_Engagement^2*(SampleN-1))))
        SE_Activity_Engagement<- c(SE_Activity_Engagement_1, SE_Activity_Engagement_2, SE_Activity_Engagement_3)
        SE_Pain_Willingness_1<- McSweeny_SE_Pain_Willingness*sqrt(1 + (1/SampleN) + ((Score_Pain_Willingness_1 - M_Pain_Willingness)^2/(SD_Pain_Willingness^2*(SampleN-1))))
        SE_Pain_Willingness_2<- McSweeny_SE_Pain_Willingness*sqrt(1 + (1/SampleN) + ((Score_Pain_Willingness_2 - M_Pain_Willingness)^2/(SD_Pain_Willingness^2*(SampleN-1))))
        SE_Pain_Willingness_3<- McSweeny_SE_Pain_Willingness*sqrt(1 + (1/SampleN) + ((Score_Pain_Willingness_3 - M_Pain_Willingness)^2/(SD_Pain_Willingness^2*(SampleN-1))))
        SE_Pain_Willingness<- c(SE_Pain_Willingness_1, SE_Pain_Willingness_2, SE_Pain_Willingness_3)
        SE<- round(SE, digits = 2)
        SE_Activity_Engagement<- round(SE_Activity_Engagement, digits = 2)
        SE_Pain_Willingness<- round(SE_Pain_Willingness, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2), (Conf*SE3))
        CI<- round(CI, digits = 2)
        CI_Activity_Engagement<- c((Conf*SE_Activity_Engagement_1), (Conf*SE_Activity_Engagement_2), (Conf*SE_Activity_Engagement_3))
        CI_Activity_Engagement<- round(CI_Activity_Engagement, digits = 2)
        CI_Pain_Willingness<- c((Conf*SE_Pain_Willingness_1), (Conf*SE_Pain_Willingness_2), (Conf*SE_Pain_Willingness_3))
        CI_Pain_Willingness<- round(CI_Pain_Willingness, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Activity_Engagement<- c((Conf*SE_Activity_Engagement), (Conf*SE_Activity_Engagement), (Conf*SE_Activity_Engagement))
        CI_Activity_Engagement<- round(CI_Activity_Engagement, digits = 2)
        CI_Pain_Willingness<- c((Conf*SE_Pain_Willingness), (Conf*SE_Pain_Willingness), (Conf*SE_Pain_Willingness))
        CI_Pain_Willingness<- round(CI_Pain_Willingness, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Activity_Engagement<- PTS_Activity_Engagement + CI_Activity_Engagement
      CI_Upper_Lim_Activity_Engagement<- round(CI_Upper_Lim_Activity_Engagement, digits = 2)
      CI_Lower_Lim_Activity_Engagement<-PTS_Activity_Engagement - CI_Activity_Engagement
      CI_Lower_Lim_Activity_Engagement<- round(CI_Lower_Lim_Activity_Engagement, digits = 2)
      CI_Upper_Lim_Pain_Willingness<- PTS_Pain_Willingness + CI_Pain_Willingness
      CI_Upper_Lim_Pain_Willingness<- round(CI_Upper_Lim_Pain_Willingness, digits = 2)
      CI_Lower_Lim_Pain_Willingness<-PTS_Pain_Willingness - CI_Pain_Willingness
      CI_Lower_Lim_Pain_Willingness<- round(CI_Lower_Lim_Pain_Willingness, digits = 2)
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Activity_Engagement == "2") {
        CI_Activity_Engagement<- input$Man_CI_Activity_Engagement
        CI_Activity_Engagement<- c(CI_Activity_Engagement, CI_Activity_Engagement, CI_Activity_Engagement)
        CI_Activity_Engagement<- round(CI_Activity_Engagement, digits = 2)
        CI_Upper_Lim_Activity_Engagement<- Score_Activity_Engagement + CI_Activity_Engagement
        CI_Upper_Lim_Activity_Engagement<- round(CI_Upper_Lim_Activity_Engagement, digits = 2)
        CI_Lower_Lim_Activity_Engagement<- Score_Activity_Engagement - CI_Activity_Engagement
        CI_Lower_Lim_Activity_Engagement<- round(CI_Lower_Lim_Activity_Engagement, digits = 2)
      }
      if(input$Select_CI_Pain_Willingness == "2") {
        CI_Pain_Willingness<- input$Man_CI_Pain_Willingness
        CI_Pain_Willingness<- c(CI_Pain_Willingness, CI_Pain_Willingness, CI_Pain_Willingness)
        CI_Pain_Willingness<- round(CI_Pain_Willingness, digits = 2)
        CI_Upper_Lim_Pain_Willingness<- Score_Pain_Willingness + CI_Pain_Willingness
        CI_Upper_Lim_Pain_Willingness<- round(CI_Upper_Lim_Pain_Willingness, digits = 2)
        CI_Lower_Lim_Pain_Willingness<- Score_Pain_Willingness - CI_Pain_Willingness
        CI_Lower_Lim_Pain_Willingness<- round(CI_Lower_Lim_Pain_Willingness, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_1<- rep(Cutoff_Score_1, 3)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_2<- rep(Cutoff_Score_2, 3)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_3<- rep(Cutoff_Score_3, 3)
      Cutoff_Score_Activity_Engagement_1<- round(input$Cutoff_Activity_Engagement_1, digits = 2)
      Cutoff_Score_Activity_Engagement_1<- rep(Cutoff_Score_Activity_Engagement_1, 3)
      Cutoff_Score_Activity_Engagement_2<- round(input$Cutoff_Activity_Engagement_2, digits = 2)
      Cutoff_Score_Activity_Engagement_2<- rep(Cutoff_Score_Activity_Engagement_2, 3)
      Cutoff_Score_Activity_Engagement_3<- round(input$Cutoff_Activity_Engagement_3, digits = 2)
      Cutoff_Score_Activity_Engagement_3<- rep(Cutoff_Score_Activity_Engagement_3, 3)
      Cutoff_Score_Pain_Willingness_1<- round(input$Cutoff_Pain_Willingness_1, digits = 2)
      Cutoff_Score_Pain_Willingness_1<- rep(Cutoff_Score_Pain_Willingness_1, 3)
      Cutoff_Score_Pain_Willingness_2<- round(input$Cutoff_Pain_Willingness_2, digits = 2)
      Cutoff_Score_Pain_Willingness_2<- rep(Cutoff_Score_Pain_Willingness_2, 3)
      Cutoff_Score_Pain_Willingness_3<- round(input$Cutoff_Pain_Willingness_3, digits = 2)
      Cutoff_Score_Pain_Willingness_3<- rep(Cutoff_Score_Pain_Willingness_3, 3)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Activity_Engagement,Change_Activity_Engagement,PTS_Activity_Engagement, SE_Activity_Engagement, CI_Upper_Lim_Activity_Engagement, CI_Lower_Lim_Activity_Engagement, Cutoff_Score_Activity_Engagement_1,Cutoff_Score_Activity_Engagement_2,Cutoff_Score_Activity_Engagement_3,
                                      Score_Pain_Willingness,Change_Pain_Willingness, PTS_Pain_Willingness, SE_Pain_Willingness, CI_Upper_Lim_Pain_Willingness, CI_Lower_Lim_Pain_Willingness, Cutoff_Score_Pain_Willingness_1,Cutoff_Score_Pain_Willingness_2,Cutoff_Score_Pain_Willingness_3)
    }
    
    
    #Create dataframes to be displayed as tables in pdf report (statistical values used in reliable change analyses)
    
    if(input$RelChangeMethod == 'Nunnally & Bernstein (1994)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Activity_Engagement<<- data.frame(Pop,  M_Activity_Engagement, SD_Activity_Engagement, RelChangeMethod, Rel_Activity_Engagement, ConfInt)
      names(Stats_Table_Activity_Engagement)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Pain_Willingness<<- data.frame(Pop,  M_Pain_Willingness, SD_Pain_Willingness, RelChangeMethod, Rel_Pain_Willingness, ConfInt)
      names(Stats_Table_Pain_Willingness)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Chelune et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Activity_Engagement<<- data.frame(Pop,  M_Activity_Engagement, M_Retest_Activity_Engagement, SD_Activity_Engagement, RelChangeMethod, Rel_Activity_Engagement, ConfInt)
      names(Stats_Table_Activity_Engagement)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Pain_Willingness<<- data.frame(Pop,  M_Pain_Willingness, M_Retest_Pain_Willingness, SD_Pain_Willingness, RelChangeMethod, Rel_Pain_Willingness, ConfInt)
      names(Stats_Table_Pain_Willingness)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Maassen et al. (2006)' | input$RelChangeMethod == 'McSweeny et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Activity_Engagement<<- data.frame(Pop,  M_Activity_Engagement, M_Retest_Activity_Engagement, SD_Activity_Engagement, SD_Retest_Activity_Engagement, RelChangeMethod, Rel_Activity_Engagement, ConfInt)
      names(Stats_Table_Activity_Engagement)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Pain_Willingness<<- data.frame(Pop,  M_Pain_Willingness, M_Retest_Pain_Willingness, SD_Pain_Willingness, SD_Retest_Pain_Willingness, RelChangeMethod, Rel_Pain_Willingness, ConfInt)
      names(Stats_Table_Pain_Willingness)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Crawford & Howell (1998)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, SampleN, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Activity_Engagement<<- data.frame(Pop,  M_Activity_Engagement, M_Retest_Activity_Engagement, SD_Activity_Engagement, SD_Retest_Activity_Engagement, RelChangeMethod, Rel_Activity_Engagement, SampleN,ConfInt)
      names(Stats_Table_Activity_Engagement)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Pain_Willingness<<- data.frame(Pop,  M_Pain_Willingness, M_Retest_Pain_Willingness, SD_Pain_Willingness, SD_Retest_Pain_Willingness, RelChangeMethod, Rel_Pain_Willingness, SampleN, ConfInt)
      names(Stats_Table_Pain_Willingness)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
    } else if (input$RelChangeMethod == 'Jacobson & Truax (1991)' | input$RelChangeMethod == 'Speer (1992)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Activity_Engagement<<- data.frame(Pop,  SD_Activity_Engagement, RelChangeMethod, Rel_Activity_Engagement, ConfInt)
      names(Stats_Table_Activity_Engagement)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Pain_Willingness<<- data.frame(Pop,  SD_Pain_Willingness, RelChangeMethod, Rel_Pain_Willingness, ConfInt)
      names(Stats_Table_Pain_Willingness)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
    }
    
    #If custom confidence intervals options are selected, make sure that no values appear for PS and SE in pdf tables
    #Change the look of tables in report if custom confidence intervals options are selected
    
    if (input$Select_CI == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS = NA, SE = NA)
      Stats_Table_Fullscale<<- Stats_Table_Fullscale %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI[1])
    }
    if (input$Select_CI_Activity_Engagement == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Activity_Engagement = NA, SE_Activity_Engagement = NA)
      Stats_Table_Activity_Engagement<<- Stats_Table_Activity_Engagement %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                        "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Activity_Engagement[1])
    }
    if (input$Select_CI_Pain_Willingness == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Pain_Willingness = NA, SE_Pain_Willingness = NA)
      Stats_Table_Pain_Willingness<<- Stats_Table_Pain_Willingness %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                                  "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Pain_Willingness[1])
    }
    
    #Create a dataframe to be used in the analytics spreadsheet.
    
    #Calculate variables for significant improvement
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$CI_Upper_Lim[1]) {
      CPAQ.Fullscale.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$CI_Upper_Lim[1]) {
      CPAQ.Fullscale.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$CI_Lower_Lim[1]) {
      CPAQ.Fullscale.Sig.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$CI_Lower_Lim[1]) {
      CPAQ.Fullscale.Sig.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Activity_Engagement[length(Entered_Scores_Df$Score_Activity_Engagement)] > Entered_Scores_Df$CI_Upper_Lim_Activity_Engagement[1]) {
      CPAQ.Activity.Engagement.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Activity_Engagement[length(Entered_Scores_Df$Score_Activity_Engagement)] <= Entered_Scores_Df$CI_Upper_Lim_Activity_Engagement[1]) {
      CPAQ.Activity.Engagement.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Activity_Engagement[length(Entered_Scores_Df$Score_Activity_Engagement)] < Entered_Scores_Df$CI_Lower_Lim_Activity_Engagement[1]) {
      CPAQ.Activity.Engagement.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Activity_Engagement[length(Entered_Scores_Df$Score_Activity_Engagement)] >= Entered_Scores_Df$CI_Lower_Lim_Activity_Engagement[1]) {
      CPAQ.Activity.Engagement.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Pain_Willingness[length(Entered_Scores_Df$Score_Pain_Willingness)] > Entered_Scores_Df$CI_Upper_Lim_Pain_Willingness[1]) {
      CPAQ.Pain.Willingness.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Pain_Willingness[length(Entered_Scores_Df$Score_Pain_Willingness)] <= Entered_Scores_Df$CI_Upper_Lim_Pain_Willingness[1]) {
      CPAQ.Pain.Willingness.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Pain_Willingness[length(Entered_Scores_Df$Score_Pain_Willingness)] < Entered_Scores_Df$CI_Lower_Lim_Pain_Willingness[1]) {
      CPAQ.Pain.Willingness.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Pain_Willingness[length(Entered_Scores_Df$Score_Pain_Willingness)] >= Entered_Scores_Df$CI_Lower_Lim_Pain_Willingness[1]) {
      CPAQ.Pain.Willingness.Sig.Deterioration<- "No"
    }
    
    #Calculate variables for improvement
    
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$Score[1]) {
      CPAQ.Fullscale.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$Score[1]) {
      CPAQ.Fullscale.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$Score[1]) {
      CPAQ.Fullscale.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$Score[1]) {
      CPAQ.Fullscale.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Activity_Engagement[length(Entered_Scores_Df$Score_Activity_Engagement)] > Entered_Scores_Df$Score_Activity_Engagement[1]) {
      CPAQ.Activity.Engagement.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Activity_Engagement[length(Entered_Scores_Df$Score_Activity_Engagement)] <= Entered_Scores_Df$Score_Activity_Engagement[1]) {
      CPAQ.Activity.Engagement.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Activity_Engagement[length(Entered_Scores_Df$Score_Activity_Engagement)] < Entered_Scores_Df$Score_Activity_Engagement[1]) {
      CPAQ.Activity.Engagement.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Activity_Engagement[length(Entered_Scores_Df$Score_Activity_Engagement)] >= Entered_Scores_Df$Score_Activity_Engagement[1]) {
      CPAQ.Activity.Engagement.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Pain_Willingness[length(Entered_Scores_Df$Score_Pain_Willingness)] > Entered_Scores_Df$Score_Pain_Willingness[1]) {
      CPAQ.Pain.Willingness.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Pain_Willingness[length(Entered_Scores_Df$Score_Pain_Willingness)] <= Entered_Scores_Df$Score_Pain_Willingness[1]) {
      CPAQ.Pain.Willingness.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Pain_Willingness[length(Entered_Scores_Df$Score_Pain_Willingness)] < Entered_Scores_Df$Score_Pain_Willingness[1]) {
      CPAQ.Pain.Willingness.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Pain_Willingness[length(Entered_Scores_Df$Score_Pain_Willingness)] >= Entered_Scores_Df$Score_Pain_Willingness[1]) {
      CPAQ.Pain.Willingness.Deterioration<- "No"
    }
    
    
    CPAQ.Fullscale.Change<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] - Entered_Scores_Df$Score[1]
    CPAQ.Activity.Engagement.Change<- Entered_Scores_Df$Score_Activity_Engagement[length(Entered_Scores_Df$Score_Activity_Engagement)] - Entered_Scores_Df$Score_Activity_Engagement[1]
    CPAQ.Pain.Willingness.Change<- Entered_Scores_Df$Score_Pain_Willingness[length(Entered_Scores_Df$Score_Pain_Willingness)] - Entered_Scores_Df$Score_Pain_Willingness[1]
    CPAQ.Fullscale.Comparisons<- length(Entered_Scores_Df$Change) - 1
    CPAQ.Activity.Engagement.Comparisons<- length(Entered_Scores_Df$Change_Activity_Engagement) - 1
    CPAQ.Pain.Willingness.Comparisons<- length(Entered_Scores_Df$Change_Pain_Willingness) - 1
    CPAQ.Fullscale.First.Date<- Entered_Scores_Df$Date[1]
    CPAQ.Activity.Engagement.First.Date<- Entered_Scores_Df$Date[1]
    CPAQ.Pain.Willingness.First.Date<- Entered_Scores_Df$Date[1]
    CPAQ.Fullscale.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    CPAQ.Activity.Engagement.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    CPAQ.Pain.Willingness.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    CPAQ.Fullscale.First.Score<- Entered_Scores_Df$Score[1]
    CPAQ.Activity.Engagement.First.Score<- Entered_Scores_Df$Score_Activity_Engagement[1]
    CPAQ.Pain.Willingness.First.Score<- Entered_Scores_Df$Score_Pain_Willingness[1]
    CPAQ.Fullscale.Last.Score<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)]
    CPAQ.Activity.Engagement.Last.Score<- Entered_Scores_Df$Score_Activity_Engagement[length(Entered_Scores_Df$Score_Activity_Engagement)]
    CPAQ.Pain.Willingness.Last.Score<- Entered_Scores_Df$Score_Pain_Willingness[length(Entered_Scores_Df$Score_Pain_Willingness)]
    
    
    Analytics_Df<<- data.frame(CPAQ.Fullscale.First.Date, CPAQ.Fullscale.First.Score, CPAQ.Fullscale.Comparisons, CPAQ.Fullscale.Change, CPAQ.Fullscale.Last.Date, CPAQ.Fullscale.Last.Score, CPAQ.Fullscale.Improvement,CPAQ.Fullscale.Sig.Improvement, CPAQ.Fullscale.Deterioration, CPAQ.Fullscale.Sig.Deterioration,
                               CPAQ.Activity.Engagement.First.Date, CPAQ.Activity.Engagement.First.Score, CPAQ.Activity.Engagement.Comparisons, CPAQ.Activity.Engagement.Change, CPAQ.Activity.Engagement.Last.Date, CPAQ.Activity.Engagement.Last.Score, CPAQ.Activity.Engagement.Improvement, CPAQ.Activity.Engagement.Sig.Improvement, CPAQ.Activity.Engagement.Deterioration, CPAQ.Activity.Engagement.Sig.Deterioration,
                               CPAQ.Pain.Willingness.First.Date, CPAQ.Pain.Willingness.First.Score, CPAQ.Pain.Willingness.Comparisons, CPAQ.Pain.Willingness.Change, CPAQ.Pain.Willingness.Last.Date, CPAQ.Pain.Willingness.Last.Score, CPAQ.Pain.Willingness.Improvement, CPAQ.Pain.Willingness.Sig.Improvement, CPAQ.Pain.Willingness.Deterioration, CPAQ.Pain.Willingness.Sig.Deterioration)
    
    
    
  })
  
  
  
  #Create an expression to activate the entered data & create notifications to indicate inaccurate data entry
  
  Entered_Scores_Notifications<- observeEvent(input$Action_Submit_Data, {
    
    Entered_Scores_Reac()
    
    if(input$Version == "CPAQ-20") {
    
    if(length(Score_1a) < 20) {
      showNotification("The CPAQ is a 20-item scale. You have entered less than 20 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(length(Score_1a) > 20) {
      showNotification("The CPAQ is a 20-item scale. You have entered more than 20 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(input$Timepoint != "1") {
      if(length(Score_2a) < 20) {
        showNotification("The CPAQ is a 20-item scale. You have entered less than 20 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_2a) > 20) {
        showNotification("The CPAQ is a 20-item scale. You have entered more than 20 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
    if(input$Timepoint == "3") {
      if(length(Score_3a) < 20) {
        showNotification("The CPAQ is a 20-item scale. You have entered less than 20 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_3a) > 20) {
        showNotification("The CPAQ is a 20-item scale. You have entered more than 20 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
    } else if(input$Version == "CPAQ-8") {
      if(length(Score_1a) < 8) {
        showNotification("The CPAQ is an 8-item scale. You have entered less than 8 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
      
      if(length(Score_1a) > 8) {
        showNotification("The CPAQ is an 8-item scale. You have entered more than 8 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
      
      if(input$Timepoint != "1") {
        if(length(Score_2a) < 8) {
          showNotification("The CPAQ is an 8-item scale. You have entered less than 8 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
        } else if(length(Score_2a) > 8) {
          showNotification("The CPAQ is an 8-item scale. You have entered more than 8 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
        }
      }
      
      if(input$Timepoint == "3") {
        if(length(Score_3a) < 8) {
          showNotification("The CPAQ is an 8-item scale. You have entered less than 8 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
        } else if(length(Score_3a) > 8) {
          showNotification("The CPAQ is an 8-item scale. You have entered more than 8 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
        }
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
    
    Gap_Activity_Engagement<- Entered_Scores_Df[1,11] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),11]
    Entered_Scores_Df[1,12]<- Gap_Activity_Engagement
    
    Gap_Pain_Willingness<- Entered_Scores_Df[1,20] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),20]
    Entered_Scores_Df[1,21]<- Gap_Pain_Willingness
    
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
    
    filename = paste0(" CPAQ Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
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
        Stats_Table_Activity_Engagement = Stats_Table_Activity_Engagement,
        Stats_Table_Pain_Willingness = Stats_Table_Pain_Willingness,
        Cutoff_Names = Cutoff_Names,
        Item_Df = Item_Df,
        Graph_Up_Lim = Graph_Up_Lim,
        Y_Vals = Y_Vals,
        Y_Vals_Labs = Y_Vals_Labs,
        Graph_Title =  Graph_Title,
        Graph_Up_Lim_AE = Graph_Up_Lim_AE,
        Y_Vals_AE = Y_Vals_AE,
        Y_Vals_Labs_AE = Y_Vals_Labs_AE,
        Graph_Up_Lim_PW = Graph_Up_Lim_PW,
        Y_Vals_PW = Y_Vals_PW,
        Y_Vals_Labs_PW = Y_Vals_Labs_PW
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
      paste(paste0(" CPAQ Data ", format(Sys.time(), '%d/%m/%y')), input$Output_Filetype1, sep = ".")
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













