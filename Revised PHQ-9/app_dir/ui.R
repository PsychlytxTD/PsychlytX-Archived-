
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

ui<- sidebar <- dashboardSidebar(
  ## DC: module
  sidebarMenu(
    br(),
    menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "PHQ9"),
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
  dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| Patient Health Questionnaire 9-Item Scale (PHQ-9)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 820),
  sidebar,
  dashboardBody(
    
    tags$head(            #Link to the css style sheet
      tags$link(rel = "stylesheet", type = "text/css", href = "Styling.css")
    ),
    tabItems(
      tabItem(tabName = "About", br(), br(), br(), br(),br(), br(),br(),br(), br(),
              column(12, offset = 4, h1(tags$a(href = "http://psychlytx.com.au", "Visit PsychlytX here.",  style = "color: #827717;")))
      ),
      
      tabItem(tabName = "References",
              
              h3(tags$strong("References")), br(),
              "Gilbody, S., Littlewood, E., Hewitt, C., Brierley, G., Tharmanathan, P., Araya, R., . . . REEACT Team. (2015). Computerised cognitive behaviour therapy (cCBT) as treatment for depression in primary care (REEACT trial): Large scale pragmatic randomised controlled trial. BMJ (Clinical Research Ed.), 351, h5627. doi:10.1136/bmj.h5627", br(), br(),
              "Kocalevent, R., Hinz, A., & Brähler, E. (2013). Standardization of the depression screener patient health questionnaire (PHQ-9) in the general population. General Hospital Psychiatry, 35(5), 551-555.", br(), br(),
              "Kroenke, K., Spitzer, R. L., & Williams, J. B. (2001). The phq‐9. Journal of General Internal Medicine, 16(9), 606-613.", br(), br(),
              "Kroenke, K., Wu, J., Yu, Z., Bair, M. J., Kean, J., Stump, T., & Monahan, P. O. (2016). Patient health questionnaire anxiety and depression scale: Initial validation in three clinical trials. Psychosomatic Medicine, 78(6), 716-727. doi:10.1097/PSY.0000000000000322", br(), br(),
              "Mitchell, A. J., Yadegarfar, M., Gill, J., & Stubbs, B. (2016). Case finding and screening clinical utility of the patient health questionnaire (PHQ-9 and PHQ-2) for depression in primary care: A diagnostic meta-analysis of 40 studies. British Journal of Psychiatry Open, 2(2), 127-138.", br(), br(),
              "Moriarty, A. S., Gilbody, S., McMillan, D., & Manea, L. (2015). Screening and case finding for major depressive disorder using the patient health questionnaire (PHQ-9): A meta-analysis. General Hospital Psychiatry, 37(6), 567-576.", br(), br(),
              "Pinto‐Meza, A., Serrano‐Blanco, A., Peñarrubia, M. T., Blanco, E., & Haro, J. M. (2005). Assessing depression in primary care with the PHQ‐9: Can it be carried out over the telephone? Journal of General Internal Medicine, 20(8), 738-742.", br(), br(),
              "Stafford, L., Berk, M., & Jackson, H. J. (2007). Validity of the hospital anxiety and depression scale and patient health questionnaire-9 to screen for depression in patients with coronary artery disease. General Hospital Psychiatry, 29(5), 417-424.", br(), br(),
              "Turner, A., Hambridge, J., White, J., Carter, G., Clover, K., Nelson, L., & Hackett, M. (2012). Depression screening in stroke: A comparison of alternative measures with the structured diagnostic interview for the diagnostic and statistical manual of mental disorders, fourth edition (major depressive episode) as criterion standard. Stroke, 43(4), 1000-1005. doi:10.1161/STROKEAHA.111.643296", br(), br(),
              "Van Steenbergen-Weijenburg, K. M., de Vroege, L., Ploeger, R. R., Brals, J. W., Vloedbeld, M. G., Veneman, T. F., . . . van der Feltz-Cornelis, Christina M. (2010). Validation of the PHQ-9 as a screening instrument for depression in diabetes patients in specialized outpatient clinics. BMC Health Services Research, 10(1), 235."
              
              
      ),
      
      
      
      tabItem(tabName = "PHQ9",
              fluidRow(
                tabBox(
                  id = "Box",
                  width = 12,
                  tabPanel("Scale",
                           wellPanel(style = "background-color: #ffffff; color: black",
                                     fluidRow(
                                       column(width = 12, offset = 5, h3(tags$strong("PHQ-9")))
                                     ),
                                     hr(),
                                     fluidRow(
                                       column(width = 6, h4(tags$strong("Over the past 2 weeks, how often have you been bothered by any of the following problems?"))),
                                       column(width = 1, offset = 1, h4(tags$strong("Not at all"))),
                                       column(width = 1, h4(tags$strong("Several days"))),
                                       column(width = 1, h4(tags$strong("More than half the days"))),
                                       column(width = 1, h4(tags$strong("Nearly every day")))
                                     ),
                                     
                                     fluidRow(
                                       column(width = 7, h4("1. Little interest or pleasure in doing things")),
                                       column(width = 5, radioButtons("Item_1", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, , selected = character(0)))
                                     ),
                                     fluidRow(
                                       column(width = 7, h4("2. Feeling down, depressed, or hopeless")),
                                       column(width = 5, radioButtons("Item_2", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                     ),
                                     fluidRow(
                                       column(width = 7, h4("3. Trouble falling or staying asleep, or sleeping too much")),
                                       column(width = 5, radioButtons("Item_3", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                     ),
                                     fluidRow(
                                       column(width = 7, h4("4. Feeling tired or having little energy")),
                                       column(width = 5, radioButtons("Item_4", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                     ),
                                     fluidRow(
                                       column(width = 7, h4("5. Poor appetite or overeating")),
                                       column(width = 5, radioButtons("Item_5", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                     ),
                                     fluidRow(
                                       column(width = 7, h4("6. Feeling bad about yourself — or that you are a failure or have let yourself or your family down")),
                                       column(width = 5, radioButtons("Item_6", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                     ),
                                     fluidRow(
                                       column(width = 7, h4("7. Trouble concentrating on things, such as reading the newspaper or watching television")),
                                       column(width = 5, radioButtons("Item_7", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                     ),
                                     fluidRow(
                                       column(width = 7, h4("8. Moving or speaking so slowly that other people could have noticed? Or the opposite — being so fidgety or restless that you have been moving around a lot more than usual")),
                                       column(width = 5, radioButtons("Item_8", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                     ),
                                     fluidRow(
                                       column(width = 7, h4("9. Thoughts that you would be better off dead or of hurting yourself in some way")),
                                       column(width = 5, radioButtons("Item_9", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                     ),
                                     hr(),
                                     fluidRow(
                                       column(width = 4, dateInput("Q_Date", "Date",  format = "dd/mm/yyyy")),
                                       column(width = 4, textInput("Q_Name", "Name")),
                                       column(width = 4, textInput("Q_Clin_Name", "Clinician's Name"))
                                     ),
                                     fluidRow(
                                       column(width = 12, h5("Scale Source: Kroenke K., Spitzer R.L., & Williams J.B. (2001). The PHQ-9: validity of a brief depression severity measure. J. of Gen Intern Med, 16, 606-613"))
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
                                                    selectInput("Pop", "", choices = c("General Population 14-24 Years", "General Population 25-34 Years", "General Population 35-44 Years", "General Population 45-54 Years", "General Population 65-74 Years", "General Population Greater 75 years +",
                                                                                       "Primary Care", "Depression", "Coronary Heart Disease", "Diabetes", "Chronic Musculoskeletal Pain", "Cancer", "Stroke"))
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
                                                             selectInput("Select_CI", label = "PHQ-9 total scale",
                                                                         choices = list("No" = 1, "Yes" = 2),
                                                                         selected = 1),
                                                             conditionalPanel(condition = "input.Select_CI == '2'",
                                                                              numericInput("Man_CI", "Specify the width of the confidence interval", value = 0))
                                                      ))),
                                           
                                           
                                           
                                           tabPanel("Mean", width = 12,
                                                    h4(tags$strong("Enter a mean value")),
                                                    fluidRow(
                                                      column(width = 6,
                                                             uiOutput("Mean_Widg")
                                                             
                                                             
                                                      )),
                                                    
                                                    conditionalPanel(condition = "input.RelChangeMethod == 'Chelune et al. (1993)' || input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)' || input.RelChangeMethod == 'Speer (1992)'",
                                                                     
                                                                     h4(tags$strong("Enter a mean value for the retest timepoint")),
                                                                     fluidRow(
                                                                       column(width = 4,
                                                                              numericInput("Retest_Mean_1", "PHQ-9 total scale", value = 0)
                                                                              
                                                                       )))),
                                           
                                           tabPanel("Sd", width = 12,
                                                    h4(tags$strong("Enter a standard deviation value")),
                                                    fluidRow(
                                                      column(width = 6,
                                                             uiOutput("Sd_Widg")
                                                      )),
                                                    
                                                    conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)'",
                                                                     
                                                                     h4(tags$strong("Enter a standard deviation value for the retest timepoint")),
                                                                     fluidRow(
                                                                       column(width = 4,
                                                                              numericInput("Retest_Sd_1", "PHQ-9 total scale", value = 0)
                                                                       )))),
                                           
                                           tabPanel("Test-Retest Reliability", width = 12,
                                                    h4(tags$strong("Enter a test-retest reliability value")),
                                                    fluidRow(
                                                      column(width = 6,
                                                             numericInput("Reliability", "PHQ-9 total scale", value = .96),
                                                             h5("Reference: Lowe, Unützer, Callahan, Perkins & Kroenke (2004)")
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
                                                      column(width = 6,
                                                             uiOutput("Cutoff_Widg_1")
                                                      )),
                                                    
                                                    hr(),
                                                    h4(tags$strong("Second cut-off score")),
                                                    fluidRow(
                                                      column(width = 6,
                                                             uiOutput("Cutoff_Widg_2")
                                                             
                                                      )),
                                                    
                                                    hr(),
                                                    h4(tags$strong("Third cut-off score")),
                                                    fluidRow(
                                                      column(width = 6,
                                                             uiOutput("Cutoff_Widg_3")
                                                             
                                                             
                                                             
                                                      )), hr()
                                                    
                                           ),
                                           
                                           
                                           h4(tags$strong("Psychometric Properties of the PHQ-9 Relevant to Assessing Reliable & Clinically Significant Change")),
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