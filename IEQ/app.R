
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
      menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "IEQ"),
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
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| Injustice Experience Questionnaire (IEQ)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 680),
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
                
                "Ferrari, R. (2015). A prospective study of perceived injustice in whiplash victims and its relationship to recovery. Clinical Rheumatology, 34(5), 975-979.", br(), br(), 
                "Kennedy, L., & Dunstan, D. A. (2014). Confirmatory factor analysis of the injustice experience questionnaire in an australian compensable population. Journal of Occupational Rehabilitation, 24(3), 385-392.", br(), br(),  
                "Margiotta, F., Hannigan, A., Imran, A., & Harmon, D. C. (2017). Pain, perceived injustice, and pain catastrophizing in chronic pain patients in ireland. Pain Practice, 17(5), 663-668.", br(), br(),   
                "Rodero, B., Luciano, J. V., Montero-Marin, J., Casanueva, B., Palacin, J. C., Gili, M., . . . Garcia-Campayo, J. (2012). Perceived injustice in fibromyalgia: Psychometric characteristics of the injustice experience questionnaire and relationship with pain catastrophising and pain acceptance. Journal of Psychosomatic Research, 73(2), 86-91. doi:10.1016/j.jpsychores.2012.05.011.", br(), br(),  
                "Scott, W., Milioto, M., Trost, Z., & Sullivan, M. J. (2016). The relationship between perceived injustice and the working alliance: A cross-sectional study of patients with persistent pain attending multidisciplinary rehabilitation. Disability and Rehabilitation, 38(24), 2365-2373.", br(), br(),   
                "Scott, W., Trost, Z., Bernier, E., & Sullivan, M. J. (2013). Anger differentially mediates the relationship between perceived injustice and chronic pain outcomes. Pain®, 154(9), 1691-1698.", br(), br(),   
                "Sullivan, M. J. (2008). User Manual for the Injustice Experience Questionnaire IEQ.", br(), br(),   
                "Sullivan, M. J., Adams, H., Horan, S., Maher, D., Boland, D., & Gross, R. (2008). The role of perceived injustice in the experience of chronic pain and disability: Scale development and validation. Journal of Occupational Rehabilitation, 18(3), 249-261.", br(), br(),   
                "Sullivan, M. J., Yakobov, E., Scott, W., & Tait, R. (2014). Perceived injustice and adverse recovery outcomes. Psychological Injury and Law, 7(4), 325-334.", br(), br(),   
                "Sullivan, M. J., Scott, W., & Trost, Z. (2012). Perceived injustice: A risk factor for problematic pain outcomes. The Clinical Journal of Pain, 28(6), 484-488. doi:10.1097/AJP.0b013e3182527d13.", br(), br(),  
                "Trost, Z., Agtarap, S., Scott, W., Driver, S., Guck, A., Roden-Foreman, K., . . . Warren, A. M. (2015). Perceived injustice after traumatic injury: Associations with pain, psychological distress, and quality of life outcomes 12 months after injury. Rehabilitation Psychology, 60(3), 213.", br(), br()  
                
        ),
        
        
        
        tabItem(tabName = "IEQ",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel("Scale",
                             wellPanel(style = "background-color: #ffffff; color: black",
                                       fluidRow(
                                         column(width = 12, offset = 10, h1(tags$strong(tags$em("IEQ"))))
                                       ),
                                      fluidRow(
                                        column(width = 12,
                                          h4("When injuries happen, they can have profound effects on our lives. This scale was designed to assess how your injury 
                                             has affected your life."),
                                          br(),
                                          h4("Listed below are twelve statements describing different thoughts and feelings that you may experience when you think about 
                                             your injury. Using the following scale, please indicate how frequently you experience these thoughts and feelings when you 
                                             think about your injury.")
                                        )
                                      ),
                                      
                                      fluidRow(
                                        column(width = 2, 
                                               div(h3(tags$strong("0"), "-", "never"))
                                               ),
                                        column(width = 2,
                                               div(h3(tags$strong("1"), "-", "rarely"))
                                               ),
                                        column(width = 3,
                                              div(h3(tags$strong("2"), "-", "sometimes"))
                                              ),
                                        column(width = 2,
                                              div(h3(tags$strong("3"), "-", "often"))
                                              ),
                                        column(width = 3,
                                              div(h3(tags$strong("4"), "-", "all the time"))
                                              )
                             
                                      ),
                                       hr(),
                                       
                                       fluidRow(
                                         column(width = 1, textInput("Item_1", label = NULL)),
                                         column(width = 11, h4("1. Most people don't understand how severe my condition is"))
                                       ),
                                       fluidRow(
                                         column(width = 1, textInput("Item_2", label = NULL)),
                                         column(width = 11, h4("2. My life will never be the same."))
                                       ),
                                       fluidRow(
                                         column(width = 1, textInput("Item_3", label = NULL)),
                                         column(width = 11, h4("3. I am suffering because of someone else’s negligence."))
                                       ),
                                       fluidRow(
                                         column(width = 1, textInput("Item_4", label = NULL)),
                                         column(width = 11, h4("4. No one should have to live this way."))
                                       ),
                                       fluidRow(
                                         column(width = 1, textInput("Item_5", label = NULL)),
                                         column(width = 11, h4("5. I just want to have my life back."))
                                       ),
                                       fluidRow(
                                         column(width = 1, textInput("Item_6", label = NULL)),
                                         column(width = 11, h4("6. I feel that this has affected me in a permanent way."))
                                       ),
                                       fluidRow(
                                         column(width = 1, textInput("Item_7", label = NULL)),
                                         column(width = 11, h4("7. It all seems so unfair."))
                                       ),
                                       fluidRow(
                                         column(width = 1, textInput("Item_8", label = NULL)),
                                         column(width = 11, h4("8. I worry that my condition is not being taken seriously."))
                                       ),
                                       fluidRow(
                                         column(width = 1, textInput("Item_9", label = NULL)),
                                         column(width = 11, h4("9. Nothing will ever make up for all that I have gone through."))
                                       ),
                                       fluidRow(
                                         column(width = 1, textInput("Item_10", label = NULL)),
                                         column(width = 11, h4("10. I feel as if I have been robbed of something very precious."))
                                       ),
                                       fluidRow(
                                         column(width = 1, textInput("Item_11", label = NULL)),
                                         column(width = 11, h4("11. I am troubled by fears that I may never achieve my dreams."))
                                       ),
                                       fluidRow(
                                         column(width = 1, textInput("Item_12", label = NULL)),
                                         column(width = 11, h4("12. I can't believe this has happened to me."))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, dateInput("Q_Date", "Date",  format = "dd/mm/yyyy")),
                                         column(width = 4, textInput("Q_Name", "Name")),
                                         column(width = 4, textInput("Q_Clin_Name", "Clinician's Name"))
                                       ), 
                                      fluidRow(
                                        column(width = 12, h5("Scale Source: Sullivan, M. J., Adams, H., Horan, S., Maher, D., Boland, D., & Gross, R. (2008). The role of perceived injustice in the experience of chronic pain and disability: Scale development and validation. Journal of Occupational Rehabilitation, 18(3), 249-261."))
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
                                                      selectInput("Pop", "", choices = c("Musculoskeletal Condition (Workplace or Motor Vehical Accident)"))
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
                                                               selectInput("Select_CI", label = "IEQ total scale",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI == '2'",
                                                                                numericInput("Man_CI", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Severity_Irreparability", label = "Severity/Irreparability",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Severity_Irreparability == '2'",
                                                                                numericInput("Man_CI_Severity_Irreparability", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Blame_Unfairness", label = "Blame/Unfairness",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Blame_Unfairness == '2'",
                                                                                numericInput("Man_CI_Blame_Unfairness", "Specify the width of the confidence interval", value = 0))
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
                                                               uiOutput("Mean_Widg_Severity_Irreparability")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Blame_Unfairness")
                                                        )
                                                      ),
                                                      
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Chelune et al. (1993)' || input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)' || input.RelChangeMethod == 'Speer (1992)'",
                                                                       
                                                                       h4(tags$strong("Enter a mean value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean", "IEQ total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Severity_Irreparability", "Severity/Irreparability", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Blame_Unfairness", "Blame/Unfairness", value = 0)
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
                                                               uiOutput("Sd_Widg_Severity_Irreparability")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Blame_Unfairness")
                                                        )
                                                      ),
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)'",
                                                                       
                                                                       h4(tags$strong("Enter a standard deviation value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd", "IEQ total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Severity_Irreparability", "Severity/Irreparability", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Blame_Unfairness", "Blame/Unfairness", value = 0)
                                                                         )
                                                                       )
                                                                       
                                                      )
                                             ),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      h4(tags$strong("Enter test-retest reliability values")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability", "IEQ total scale", value = .90),
                                                               h6("Reference: Sullivan (2008)")
                                                               
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Severity_Irreparability", "*Severity/Irreparability", value = .92),
                                                               h6("Reference: Kennedy & Dunstan (2014)"),
                                                               h6("*Value is Cronbach's Alpha, not test-retest reliability.")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Blame_Unfairness", "*Blame/Unfairness", value = .92),
                                                               h6("Reference: Kennedy & Dunstan (2014)"),
                                                               h6("*Value is Cronbach's Alpha, not test-retest reliability.")
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
                                                      checkboxInput("Dev_Cutoffs", "Use severity ratings & percentiles developed by scale author", value = TRUE, width = '100%'),
                                                      hr(),
                                                      h4(tags$strong("First cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Severity_Irreparability_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Blame_Unfairness_1") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Second cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Severity_Irreparability_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Blame_Unfairness_2") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Third cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Severity_Irreparability_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Blame_Unfairness_3") 
                                                        )
                                                        
                                                      )
                                                      , 
                                                      
                                                      hr(),
                                                      h4(tags$strong("Fourth cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_4") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Severity_Irreparability_4") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Blame_Unfairness_4") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Fifth cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_5") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Severity_Irreparability_5") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Blame_Unfairness_5") 
                                                        )
                                                        
                                                      )
                                             ),
                                             
                                             
                                             h4(tags$strong("Psychometric Properties of the IEQ Relevant to Assessing Reliable & Clinically Significant Change")),
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
                      input$Item_8, input$Item_9, input$Item_10, input$Item_11, input$Item_12, sep = ",")
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
                    , pageLength = 100, dom = "t", scrollX = TRUE, fixedColumns = list(leftColumns = 3), autoWidth = TRUE, columnDefs = list(list(width = '180px', targets = c(1,9,10)), list(width= '140px', targets = c(0,2))))) %>% formatStyle('Authors', color = '#ffffff', backgroundColor = '#d35400')
    
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
    
    if(input$Pop == "Musculoskeletal Condition (Workplace or Motor Vehical Accident)") {
      Mean_Val<<- 19.6
      Sd_Val<<- 12.6
      Source_Mean<<- "Sullivan (2008)"
      Source_Sd<<- "Sullivan (2008)"
        if(input$Dev_Cutoffs == TRUE) {
        Cut_Val_1<<- 8
        Cut_Val_2<<- 14
        Cut_Val_3<<- 23
        Cut_Val_4<<- 30
        Cut_Val_5<<- 34
        Cut_Lab_1<<- "Low (25th Percentile)"
        Cut_Lab_2<<- "Average (40th Percentile)"
        Cut_Lab_3<<- "Moderate (60th Percentile)"
        Cut_Lab_4<<- "High & Clinically Relevant (75th Percentile)"
        Cut_Lab_5<<- "Very High (86th Percentile)"
        Source_Cutoff<<- "Sullivan (2008)"
        } else {
            Cut_Val_1<<- Mean_Val
            Cut_Val_2<<- Mean_Val + (0.5*Sd_Val)
            Cut_Val_3<<- Mean_Val + Sd_Val
            Cut_Val_4<<- Mean_Val + (1.5*Sd_Val)
            Cut_Val_5<<- Mean_Val + (2*Sd_Val)
            Cut_Lab_1<<- "Mean"
            Cut_Lab_2<<- "Mean + 0.5 Sd"
            Cut_Lab_3<<- "Mean + 1 Sd"
            Cut_Lab_4<<- "Mean + 1.5 Sd"
            Cut_Lab_5<<- "Mean + 2 Sd"
            Source_Cutoff<<- "Sullivan, Adams, Horan, Maher, Boland & Gross (2008)"
        }
        Mean_Val_Severity_Irreparability<<- 11.3
        Sd_Val_Severity_Irreparability<<- 6.4
        Source_Mean_Severity_Irreparability<<- "Sullivan (2008)"
        Source_Sd_Severity_Irreparability<<- "Sullivan (2008)"
        if(input$Dev_Cutoffs == TRUE) {
        Cut_Val_Severity_Irreparability_1<<- 5
        Cut_Val_Severity_Irreparability_2<<- 8
        Cut_Val_Severity_Irreparability_3<<- 13
        Cut_Val_Severity_Irreparability_4<<- 16
        Cut_Val_Severity_Irreparability_5<<- 18
        Cut_Lab_1_Severity_Irreparability<<- "Low (25th Percentile)"
        Cut_Lab_2_Severity_Irreparability<<- "Average (40th Percentile)"
        Cut_Lab_3_Severity_Irreparability<<- "Moderate (60th Percentile)"
        Cut_Lab_4_Severity_Irreparability<<- "High & Clinically Relevant (75th Percentile)"
        Cut_Lab_5_Severity_Irreparability<<- "Very High (86th Percentile)"
        Source_Cutoff_Severity_Irreparability<<- "Sullivan (2008)"
        } else {
          Cut_Val_Severity_Irreparability_1<<- Mean_Val_Severity_Irreparability 
          Cut_Val_Severity_Irreparability_2<<- Mean_Val_Severity_Irreparability + (0.5*Sd_Val_Severity_Irreparability)
          Cut_Val_Severity_Irreparability_3<<- Mean_Val_Severity_Irreparability + Sd_Val_Severity_Irreparability
          Cut_Val_Severity_Irreparability_4<<- Mean_Val_Severity_Irreparability + (1.5*Sd_Val_Severity_Irreparability)
          Cut_Val_Severity_Irreparability_5<<- Mean_Val_Severity_Irreparability + (2*Sd_Val_Severity_Irreparability)
          Cut_Lab_1_Severity_Irreparability<<- "Mean"
          Cut_Lab_2_Severity_Irreparability<<- "Mean + 0.5 Sd"
          Cut_Lab_3_Severity_Irreparability<<- "Mean + 1 Sd"
          Cut_Lab_4_Severity_Irreparability<<- "Mean + 1.5 Sd"
          Cut_Lab_5_Severity_Irreparability<<- "Mean + 2 Sd"
          Source_Cutoff_Severity_Irreparability<<- "Sullivan, Adams, Horan, Maher, Boland & Gross (2008)"
        }
        Mean_Val_Blame_Unfairness<<- 8.2
        Sd_Val_Blame_Unfairness<<-7
        Source_Mean_Blame_Unfairness<<- "Sullivan (2008)"
        Source_Sd_Blame_Unfairness<<- "Sullivan (2008)"
        if(input$Dev_Cutoffs == TRUE) {
        Cut_Val_Blame_Unfairness_1<<- 2
        Cut_Val_Blame_Unfairness_2<<- 4
        Cut_Val_Blame_Unfairness_3<<- 9
        Cut_Val_Blame_Unfairness_4<<- 14
        Cut_Val_Blame_Unfairness_5<<- 17
        Cut_Lab_1_Blame_Unfairness<<- "Low (25th Percentile)"
        Cut_Lab_2_Blame_Unfairness<<- "Average (40th Percentile)"
        Cut_Lab_3_Blame_Unfairness<<- "Moderate (60th Percentile)"
        Cut_Lab_4_Blame_Unfairness<<- "High & Clinically Relevant (75th Percentile)"
        Cut_Lab_5_Blame_Unfairness<<- "Very High (86th Percentile)"
        Source_Cutoff_Blame_Unfairness<<- "Sullivan (2008)"
        } else {
          Cut_Val_Blame_Unfairness_1<<- Mean_Val_Blame_Unfairness
          Cut_Val_Blame_Unfairness_2<<- Mean_Val_Blame_Unfairness + (0.5*Sd_Val_Blame_Unfairness)
          Cut_Val_Blame_Unfairness_3<<- Mean_Val_Blame_Unfairness + Sd_Val_Blame_Unfairness
          Cut_Val_Blame_Unfairness_4<<- Mean_Val_Blame_Unfairness + (1.5*Sd_Val_Blame_Unfairness)
          Cut_Val_Blame_Unfairness_5<<- Mean_Val_Blame_Unfairness + (2*Sd_Val_Blame_Unfairness)
          Cut_Lab_1_Blame_Unfairness<<- "Mean"
          Cut_Lab_2_Blame_Unfairness<<- "Mean + 0.5 Sd"
          Cut_Lab_3_Blame_Unfairness<<- "Mean + 1 Sd"
          Cut_Lab_4_Blame_Unfairness<<- "Mean + 1.5 Sd"
          Cut_Lab_5_Blame_Unfairness<<- "Mean + 2 Sd"
          Source_Cutoff_Blame_Unfairness<<- "Sullivan, Adams, Horan, Maher, Boland & Gross (2008)"
        }
      
    }     
    
  })
  
  
  output$Mean_Widg<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean", "IEQ total scale", Mean_Val),
      h6(paste("Reference:", Source_Mean))
          )
  })
  outputOptions(output, "Mean_Widg", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Severity_Irreparability<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Severity_Irreparability", "Severity/Irreparability", Mean_Val_Severity_Irreparability),
      h6(paste("Reference:", Source_Mean_Severity_Irreparability))
          )
  })
  outputOptions(output, "Mean_Widg_Severity_Irreparability", suspendWhenHidden = FALSE)
  

  output$Mean_Widg_Blame_Unfairness<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Blame_Unfairness", "Blame/Unfairness", Mean_Val_Blame_Unfairness),
      h6(paste("Reference:", Source_Mean_Blame_Unfairness))
          )
  })
  outputOptions(output, "Mean_Widg_Blame_Unfairness", suspendWhenHidden = FALSE)
  
  
  
  
  output$Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd", "IEQ total scale", Sd_Val),
      h6(paste("Reference:", Source_Sd))
           )
  })
  outputOptions(output, "Sd_Widg", suspendWhenHidden = FALSE)
  

  output$Sd_Widg_Severity_Irreparability<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Severity_Irreparability", "Severity/Irreparability", Sd_Val_Severity_Irreparability),
      h6(paste("Reference:", Source_Sd_Severity_Irreparability))
          )
  })
  outputOptions(output, "Sd_Widg_Severity_Irreparability", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Blame_Unfairness<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Blame_Unfairness", "Blame/Unfairness", Sd_Val_Blame_Unfairness),
      h6(paste("Reference:", Source_Sd_Blame_Unfairness))
          )
  })
  outputOptions(output, "Sd_Widg_Blame_Unfairness", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_1", "IEQ total scale", as.numeric(Cut_Val_1)),
      textInput("Cutoff_Text_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source_Cutoff))
          )
  })
  outputOptions(output, "Cutoff_Widg_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Severity_Irreparability_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Severity_Irreparability_1", "Severity/Irreparability", as.numeric(Cut_Val_Severity_Irreparability_1)),
      textInput("Cutoff_Text_Severity_Irreparability_1", "Cut-Off Score Name", Cut_Lab_1_Severity_Irreparability),
      h6(paste("Reference:", Source_Cutoff_Severity_Irreparability))
          )
  })
  outputOptions(output, "Cutoff_Widg_Severity_Irreparability_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Blame_Unfairness_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Blame_Unfairness_1", "Blame/Unfairness", as.numeric(Cut_Val_Blame_Unfairness_1)),
      textInput("Cutoff_Text_Blame_Unfairness_1", "Cut-Off Score Name", Cut_Lab_1_Blame_Unfairness),
      h6(paste("Reference:", Source_Cutoff_Blame_Unfairness))
           )
  })
  outputOptions(output, "Cutoff_Widg_Blame_Unfairness_1", suspendWhenHidden = FALSE)
  
  
  
  output$Cutoff_Widg_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_2", "IEQ total scale", as.numeric(Cut_Val_2)),
      textInput("Cutoff_Text_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source_Cutoff))
          )
  })
  outputOptions(output, "Cutoff_Widg_2", suspendWhenHidden = FALSE)
  

  output$Cutoff_Widg_Severity_Irreparability_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Severity_Irreparability_2", "Severity/Irreparability", as.numeric(Cut_Val_Severity_Irreparability_2)),
      textInput("Cutoff_Text_Severity_Irreparability_2", "Cut-Off Score Name", Cut_Lab_2_Severity_Irreparability),
      h6(paste("Reference:", Source_Cutoff_Severity_Irreparability))
           )
  })
  outputOptions(output, "Cutoff_Widg_Severity_Irreparability_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Blame_Unfairness_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Blame_Unfairness_2", "Blame/Unfairness", as.numeric(Cut_Val_Blame_Unfairness_2)),
      textInput("Cutoff_Text_Blame_Unfairness_2", "Cut-Off Score Name", Cut_Lab_2_Blame_Unfairness),
      h6(paste("Reference:", Source_Cutoff_Blame_Unfairness))
    )
  })
  outputOptions(output, "Cutoff_Widg_Blame_Unfairness_2", suspendWhenHidden = FALSE)
  
  

  output$Cutoff_Widg_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_3", "IEQ total scale", as.numeric(Cut_Val_3)),
      textInput("Cutoff_Text_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source_Cutoff))
           )
          
  })
  outputOptions(output, "Cutoff_Widg_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Severity_Irreparability_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Severity_Irreparability_3", "Severity/Irreparability", as.numeric(Cut_Val_Severity_Irreparability_3)),
      textInput("Cutoff_Text_Severity_Irreparability_3", "Cut-Off Score Name", Cut_Lab_3_Severity_Irreparability),
      h6(paste("Reference:", Source_Cutoff_Severity_Irreparability))
          )
  })
  outputOptions(output, "Cutoff_Widg_Severity_Irreparability_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Blame_Unfairness_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Blame_Unfairness_3", "Blame/Unfairness", as.numeric(Cut_Val_Blame_Unfairness_3)),
      textInput("Cutoff_Text_Blame_Unfairness_3", "Cut-Off Score Name", Cut_Lab_3_Blame_Unfairness),
      h6(paste("Reference:", Source_Cutoff_Blame_Unfairness))
           )
  })
  outputOptions(output, "Cutoff_Widg_Blame_Unfairness_3", suspendWhenHidden = FALSE)
  

  
  
  output$Cutoff_Widg_4<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_4", "IEQ total scale", as.numeric(Cut_Val_4)),
      textInput("Cutoff_Text_4", "Cut-Off Score Name", Cut_Lab_4),
      h6(paste("Reference:", Source_Cutoff))
    )
  })
  outputOptions(output, "Cutoff_Widg_4", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Severity_Irreparability_4<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Severity_Irreparability_4", "Severity/Irreparability", as.numeric(Cut_Val_Severity_Irreparability_4)),
      textInput("Cutoff_Text_Severity_Irreparability_4", "Cut-Off Score Name", Cut_Lab_4_Severity_Irreparability),
      h6(paste("Reference:", Source_Cutoff_Severity_Irreparability))
    )
  })
  outputOptions(output, "Cutoff_Widg_Severity_Irreparability_4", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Blame_Unfairness_4<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Blame_Unfairness_4", "Blame/Unfairness", as.numeric(Cut_Val_Blame_Unfairness_4)),
      textInput("Cutoff_Text_Blame_Unfairness_4", "Cut-Off Score Name", Cut_Lab_4_Blame_Unfairness),
      h6(paste("Reference:", Source_Cutoff_Blame_Unfairness))
    )
  })
  outputOptions(output, "Cutoff_Widg_Blame_Unfairness_4", suspendWhenHidden = FALSE)
  
  
  
  output$Cutoff_Widg_5<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_5", "IEQ total scale", as.numeric(Cut_Val_5)),
      textInput("Cutoff_Text_5", "Cut-Off Score Name", Cut_Lab_5),
      h6(paste("Reference:", Source_Cutoff))
    )
  })
  outputOptions(output, "Cutoff_Widg_5", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Severity_Irreparability_5<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Severity_Irreparability_5", "Severity/Irreparability", as.numeric(Cut_Val_Severity_Irreparability_5)),
      textInput("Cutoff_Text_Severity_Irreparability_5", "Cut-Off Score Name", Cut_Lab_5_Severity_Irreparability),
      h6(paste("Reference:", Source_Cutoff_Severity_Irreparability))
    )
  })
  outputOptions(output, "Cutoff_Widg_Severity_Irreparability_5", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Blame_Unfairness_5<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Blame_Unfairness_5", "Blame/Unfairness", as.numeric(Cut_Val_Blame_Unfairness_5)),
      textInput("Cutoff_Text_Blame_Unfairness_5", "Cut-Off Score Name", Cut_Lab_5_Blame_Unfairness),
      h6(paste("Reference:", Source_Cutoff_Blame_Unfairness))
    )
  })
  outputOptions(output, "Cutoff_Widg_Blame_Unfairness_5", suspendWhenHidden = FALSE)
  
  

  
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
    M_Severity_Irreparability<- input$Pop_Mean_Severity_Irreparability
    SD_Severity_Irreparability<-input$Pop_Sd_Severity_Irreparability
    M_Blame_Unfairness<- input$Pop_Mean_Blame_Unfairness
    SD_Blame_Unfairness<- input$Pop_Sd_Blame_Unfairness
   
    
    M_Retest<- input$Retest_Mean
    SD_Retest<- input$Retest_Sd
    M_Retest_Severity_Irreparability<- input$Retest_Mean_Severity_Irreparability
    SD_Retest_Severity_Irreparability<- input$Retest_Sd_Severity_Irreparability
    M_Retest_Blame_Unfairness<- input$Retest_Mean_Blame_Unfairness
    SD_Retest_Blame_Unfairness<- input$Retest_Sd_Blame_Unfairness
   
   
    
    SampleN<- input$SampleN
    
    Rel<- input$Reliability
    Rel_Severity_Irreparability<- input$Reliability_Severity_Irreparability
    Rel_Blame_Unfairness<- input$Reliability_Blame_Unfairness
   
    
    
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
      SE_Severity_Irreparability<-SD_Severity_Irreparability * sqrt(1 - Rel_Severity_Irreparability^2)
      SE_Blame_Unfairness<-SD_Blame_Unfairness * sqrt(1 - Rel_Blame_Unfairness^2)
      SE<- round(SE, digits = 2)
      SE_Severity_Irreparability<- round(SE_Severity_Irreparability, digits = 2)
      SE_Blame_Unfairness<- round(SE_Blame_Unfairness, digits = 2)
    } else if (input$RelChangeMethod == "Chelune et al. (1993)" | input$RelChangeMethod == "Jacobson & Truax (1991)" | input$RelChangeMethod == "Speer (1992)") {
      SE<- sqrt((2*(SD^2))*(1-Rel))
      SE_Severity_Irreparability<- sqrt((2*(SD_Severity_Irreparability^2))*(1-Rel_Severity_Irreparability))
      SE_Blame_Unfairness<- sqrt((2*(SD_Blame_Unfairness^2))*(1-Rel_Blame_Unfairness))
      SE<- round(SE, digits = 2)
      SE_Severity_Irreparability<- round(SE_Severity_Irreparability, digits = 2)
      SE_Blame_Unfairness<- round(SE_Blame_Unfairness, digits = 2)
    } else if (input$RelChangeMethod == "Maassen et al. (2006)") {
      SE<- sqrt((SD^2 + SD_Retest^2)*(1-Rel))
      SE_Severity_Irreparability<- sqrt((SD_Severity_Irreparability^2 + SD_Retest_Severity_Irreparability^2)*(1-Rel_Severity_Irreparability))
      SE_Blame_Unfairness<- sqrt((SD_Blame_Unfairness^2 + SD_Retest_Blame_Unfairness^2)*(1-Rel_Blame_Unfairness))
      SE<- round(SE, digits = 2)
      SE_Severity_Irreparability<- round(SE_Severity_Irreparability, digits = 2)
      SE_Blame_Unfairness<- round(SE_Blame_Unfairness, digits = 2)
    } else if (input$RelChangeMethod == "McSweeny et al. (1993)") {
      SE<- SD_Retest*sqrt(1 - Rel^2)
      SE_Severity_Irreparability<- SD_Retest_Severity_Irreparability*sqrt(1 - Rel_Severity_Irreparability^2)
      SE_Blame_Unfairness<- SD_Retest_Blame_Unfairness*sqrt(1 - Rel_Blame_Unfairness^2)
      SE<- round(SE, digits = 2)
      SE_Severity_Irreparability<- round(SE_Severity_Irreparability, digits = 2)
      SE_Blame_Unfairness<- round(SE_Blame_Unfairness, digits = 2)
    }
    
    
    #Caclculate McSweeny standard error to be employed in deriving the Crawford & Howell standard error if selected
    
    McSweeny_SE<- SD_Retest*sqrt(1 - Rel^2)
    McSweeny_SE_Severity_Irreparability<- SD_Retest_Severity_Irreparability*sqrt(1 - Rel_Severity_Irreparability^2)
    McSweeny_SE_Blame_Unfairness<- SD_Retest_Blame_Unfairness*sqrt(1 - Rel_Blame_Unfairness^2)
   
    
    #Generate object storing names of cut-off scores
    
    Cutoff_Name_1<- input$Cutoff_Text_1
    Cutoff_Name_2<- input$Cutoff_Text_2
    Cutoff_Name_3<- input$Cutoff_Text_3
    Cutoff_Name_4<- input$Cutoff_Text_4
    Cutoff_Name_5<- input$Cutoff_Text_5
    Cutoff_Name_Severity_Irreparability_1<- input$Cutoff_Text_Severity_Irreparability_1
    Cutoff_Name_Severity_Irreparability_2<- input$Cutoff_Text_Severity_Irreparability_2
    Cutoff_Name_Severity_Irreparability_3<- input$Cutoff_Text_Severity_Irreparability_3
    Cutoff_Name_Severity_Irreparability_4<- input$Cutoff_Text_Severity_Irreparability_4
    Cutoff_Name_Severity_Irreparability_5<- input$Cutoff_Text_Severity_Irreparability_5
    Cutoff_Name_Blame_Unfairness_1<- input$Cutoff_Text_Blame_Unfairness_1
    Cutoff_Name_Blame_Unfairness_2<- input$Cutoff_Text_Blame_Unfairness_2
    Cutoff_Name_Blame_Unfairness_3<- input$Cutoff_Text_Blame_Unfairness_3
    Cutoff_Name_Blame_Unfairness_4<- input$Cutoff_Text_Blame_Unfairness_4
    Cutoff_Name_Blame_Unfairness_5<- input$Cutoff_Text_Blame_Unfairness_5
  
    
    Cutoff_Names<<- data.frame(Cutoff_Name_1,Cutoff_Name_2,Cutoff_Name_3,Cutoff_Name_4,Cutoff_Name_5,
                               Cutoff_Name_Severity_Irreparability_1,Cutoff_Name_Severity_Irreparability_2,Cutoff_Name_Severity_Irreparability_3, Cutoff_Name_Severity_Irreparability_4,Cutoff_Name_Severity_Irreparability_5,
                               Cutoff_Name_Blame_Unfairness_1, Cutoff_Name_Blame_Unfairness_2, Cutoff_Name_Blame_Unfairness_3,Cutoff_Name_Blame_Unfairness_4,Cutoff_Name_Blame_Unfairness_5
                               )
                               
    
    #Define the data & calculate PS, SE, confidence intervals and cut-off scores
    
    if(input$Timepoint == "1") {
      Date<- input$Date_1
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_1a), Score = Score_1a)
      Score<- sum(Score_1a, na.rm = TRUE)
      Score<- round(Score, digits = 2)
      Score_Severity_Irreparability<- sum(Score_1a[c(1,2,4,5,6,8)], na.rm = TRUE)
      Score_Blame_Unfairness<- sum(Score_1a[c(3,7,9,10,11,12)], na.rm = TRUE)
      Change<- 0
      Change<- round(Change, digits = 2)
      Change_Severity_Irreparability<- 0
      Change_Blame_Unfairness<- 0
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS<- (Rel * Score) + (M * (1 - Rel))
        PTS_Severity_Irreparability<- (Rel_Severity_Irreparability * Score_Severity_Irreparability) + (M_Severity_Irreparability * (1 - Rel_Severity_Irreparability))
        PTS_Blame_Unfairness<- (Rel_Blame_Unfairness * Score_Blame_Unfairness) + (M_Blame_Unfairness * (1 - Rel_Blame_Unfairness))
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS<- Score + (M_Retest - M)  
        PTS_Severity_Irreparability<- Score_Severity_Irreparability + (M_Retest_Severity_Irreparability - M_Severity_Irreparability)  
        PTS_Blame_Unfairness<- Score_Blame_Unfairness + (M_Retest_Blame_Unfairness - M_Blame_Unfairness)  
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Severity_Irreparability<- Score_Severity_Irreparability
        PTS_Blame_Unfairness<- Score_Blame_Unfairness
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS<- (B_Adj * Score) + A_Adj
        B_Slope_Severity_Irreparability<- Rel_Severity_Irreparability * (SD_Retest_Severity_Irreparability/SD_Severity_Irreparability)
        A_Constant_Severity_Irreparability<- M_Retest_Severity_Irreparability - (B_Slope_Severity_Irreparability * M_Severity_Irreparability)
        B_Adj_Severity_Irreparability<- SD_Retest_Severity_Irreparability/SD_Severity_Irreparability
        A_Adj_Severity_Irreparability<- M_Retest_Severity_Irreparability - (B_Adj_Severity_Irreparability * M_Severity_Irreparability)
        PTS_Severity_Irreparability<- (B_Adj_Severity_Irreparability * Score_Severity_Irreparability) + A_Adj_Severity_Irreparability
        B_Slope_Blame_Unfairness<- Rel_Blame_Unfairness * (SD_Retest_Blame_Unfairness/SD_Blame_Unfairness)
        A_Constant_Blame_Unfairness<- M_Retest_Blame_Unfairness - (B_Slope_Blame_Unfairness * M_Blame_Unfairness)
        B_Adj_Blame_Unfairness<- SD_Retest_Blame_Unfairness/SD_Blame_Unfairness
        A_Adj_Blame_Unfairness<- M_Retest_Blame_Unfairness - (B_Adj_Blame_Unfairness * M_Blame_Unfairness)
        PTS_Blame_Unfairness<- (B_Adj_Blame_Unfairness * Score_Blame_Unfairness) + A_Adj_Blame_Unfairness
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS<- B_Slope * Score
        B_Slope_Severity_Irreparability<- Rel_Severity_Irreparability * (SD_Retest_Severity_Irreparability/SD_Severity_Irreparability)
        PTS_Severity_Irreparability<- B_Slope_Severity_Irreparability * Score_Severity_Irreparability
        B_Slope_Blame_Unfairness<- Rel_Blame_Unfairness * (SD_Retest_Blame_Unfairness/SD_Blame_Unfairness)
        PTS_Blame_Unfairness<- B_Slope_Blame_Unfairness * Score_Blame_Unfairness
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS<- Score + (M_Retest - M)
        PTS_Severity_Irreparability<- Score_Severity_Irreparability + (M_Retest_Severity_Irreparability - M_Severity_Irreparability)
        PTS<- Blame/Unfairness<- Score_Blame_Unfairness + (M_Retest_Blame_Unfairness - M_Blame_Unfairness)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Severity_Irreparability<- round(PTS_Severity_Irreparability, digits = 2)
      PTS_Blame_Unfairness<- round(PTS_Blame_Unfairness, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score - M)^2/(SD^2*(SampleN-1))))
        SE_Severity_Irreparability<- McSweeny_SE_Severity_Irreparability*sqrt(1 + (1/SampleN) + ((Score_Severity_Irreparability - M_Severity_Irreparability)^2/(SD_Severity_Irreparability^2*(SampleN-1))))
        SE_Blame_Unfairness<- McSweeny_SE_Blame_Unfairness*sqrt(1 + (1/SampleN) + ((Score_Blame_Unfairness - M_Blame_Unfairness)^2/(SD_Blame_Unfairness^2*(SampleN-1))))
        SE<- round(SE, digits = 2)
        SE_Severity_Irreparability<- round(SE_Severity_Irreparability, digits = 2)
        SE_Blame_Unfairness<- round(SE_Blame_Unfairness, digits = 2)
        CI<- (Conf*SE)
        CI<- round(CI, digits = 2)
        CI_Severity_Irreparability<- (Conf*SE_Severity_Irreparability)
        CI_Severity_Irreparability<- round(CI_Severity_Irreparability, digits = 2)
        CI_Blame_Unfairness<- (Conf*SE_Blame_Unfairness)
        CI_Blame_Unfairness<- round(CI_Blame_Unfairness, digits = 2)
      }
      CI<- (Conf*SE)
      CI<- round(CI, digits = 2)
      CI_Severity_Irreparability<- (Conf*SE_Severity_Irreparability)
      CI_Severity_Irreparability<- round(CI_Severity_Irreparability, digits = 2)
      CI_Blame_Unfairness<- (Conf*SE_Blame_Unfairness)
      CI_Blame_Unfairness<- round(CI_Blame_Unfairness, digits = 2)
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Severity_Irreparability<- PTS_Severity_Irreparability + CI_Severity_Irreparability
      CI_Upper_Lim_Severity_Irreparability<- round(CI_Upper_Lim_Severity_Irreparability, digits = 2)
      CI_Lower_Lim_Severity_Irreparability<-PTS_Severity_Irreparability - CI_Severity_Irreparability
      CI_Lower_Lim_Severity_Irreparability<- round(CI_Lower_Lim_Severity_Irreparability, digits = 2)
      CI_Upper_Lim_Blame_Unfairness<- PTS_Blame_Unfairness + CI_Blame_Unfairness
      CI_Upper_Lim_Blame_Unfairness<- round(CI_Upper_Lim_Blame_Unfairness, digits = 2)
      CI_Lower_Lim_Blame_Unfairness<-PTS_Blame_Unfairness - CI_Blame_Unfairness
      CI_Lower_Lim_Blame_Unfairness<- round(CI_Lower_Lim_Blame_Unfairness, digits = 2)
      if(input$Select_CI == "2") {
        CI<- input$Man_CI
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Severity_Irreparability == "2") {
        CI_Severity_Irreparability<- input$Man_CI_Severity_Irreparability
        CI_Severity_Irreparability<- round(CI_Severity_Irreparability, digits = 2)
        CI_Upper_Lim_Severity_Irreparability<- Score_Severity_Irreparability + CI_Severity_Irreparability
        CI_Upper_Lim_Severity_Irreparability<- round(CI_Upper_Lim_Severity_Irreparability, digits = 2)
        CI_Lower_Lim_Severity_Irreparability<- Score_Severity_Irreparability - CI_Severity_Irreparability
        CI_Lower_Lim_Severity_Irreparability<- round(CI_Lower_Lim_Severity_Irreparability, digits = 2)
      }
      if(input$Select_CI_Blame_Unfairness == "2") {
        CI_Blame_Unfairness<- input$Man_CI_Blame_Unfairness
        CI_Blame_Unfairness<- round(CI_Blame_Unfairness, digits = 2)
        CI_Upper_Lim_Blame_Unfairness<- Score_Blame_Unfairness + CI_Blame_Unfairness
        CI_Upper_Lim_Blame_Unfairness<- round(CI_Upper_Lim_Blame_Unfairness, digits = 2)
        CI_Lower_Lim_Blame_Unfairness<- Score_Blame_Unfairness - CI_Blame_Unfairness
        CI_Lower_Lim_Blame_Unfairness<- round(CI_Lower_Lim_Blame_Unfairness, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_4<- round(input$Cutoff_4, digits = 2)
      Cutoff_Score_5<- round(input$Cutoff_5, digits = 2)
      Cutoff_Score_Severity_Irreparability_1<- round(input$Cutoff_Severity_Irreparability_1, digits = 2)
      Cutoff_Score_Severity_Irreparability_2<- round(input$Cutoff_Severity_Irreparability_2, digits = 2)
      Cutoff_Score_Severity_Irreparability_3<- round(input$Cutoff_Severity_Irreparability_3, digits = 2)
      Cutoff_Score_Severity_Irreparability_4<- round(input$Cutoff_Severity_Irreparability_4, digits = 2)
      Cutoff_Score_Severity_Irreparability_5<- round(input$Cutoff_Severity_Irreparability_5, digits = 2)
      Cutoff_Score_Blame_Unfairness_1<- round(input$Cutoff_Blame_Unfairness_1, digits = 2)
      Cutoff_Score_Blame_Unfairness_2<- round(input$Cutoff_Blame_Unfairness_2, digits = 2)
      Cutoff_Score_Blame_Unfairness_3<- round(input$Cutoff_Blame_Unfairness_3, digits = 2)
      Cutoff_Score_Blame_Unfairness_4<- round(input$Cutoff_Blame_Unfairness_4, digits = 2)
      Cutoff_Score_Blame_Unfairness_5<- round(input$Cutoff_Blame_Unfairness_5, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Cutoff_Score_4,Cutoff_Score_5,
                                      Score_Severity_Irreparability,Change_Severity_Irreparability,PTS_Severity_Irreparability, SE_Severity_Irreparability, CI_Upper_Lim_Severity_Irreparability, CI_Lower_Lim_Severity_Irreparability, Cutoff_Score_Severity_Irreparability_1,Cutoff_Score_Severity_Irreparability_2,Cutoff_Score_Severity_Irreparability_3,Cutoff_Score_Severity_Irreparability_4,Cutoff_Score_Severity_Irreparability_5,
                                      Score_Blame_Unfairness,Change_Blame_Unfairness, PTS_Blame_Unfairness, SE_Blame_Unfairness, CI_Upper_Lim_Blame_Unfairness, CI_Lower_Lim_Blame_Unfairness, Cutoff_Score_Blame_Unfairness_1,Cutoff_Score_Blame_Unfairness_2,Cutoff_Score_Blame_Unfairness_3,Cutoff_Score_Blame_Unfairness_4,Cutoff_Score_Blame_Unfairness_5
                                     )
      
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
      Score_Severity_Irreparability_1<- sum(Score_1a[c(1,2,4,5,6,8)], na.rm = TRUE)
      Score_Severity_Irreparability_2<- sum(Score_2a[c(1,2,4,5,6,8)], na.rm = TRUE)
      Score_Severity_Irreparability<- c(Score_Severity_Irreparability_1,Score_Severity_Irreparability_2)
      Score_Severity_Irreparability<- round(Score_Severity_Irreparability, digits = 2)
      Score_Blame_Unfairness_1<- sum(Score_1a[c(3,7,9,10,11,12)], na.rm = TRUE)
      Score_Blame_Unfairness_2<- sum(Score_2a[c(3,7,9,10,11,12)], na.rm = TRUE)
      Score_Blame_Unfairness<- c(Score_Blame_Unfairness_1,Score_Blame_Unfairness_2)
      Score_Blame_Unfairness<- round(Score_Blame_Unfairness, digits = 2)
      Change<- c(0, (Score_2 - Score_1))
      Change<- round(Change, digits = 2)
      Change_Severity_Irreparability<- c(0, (Score_Severity_Irreparability_2 - Score_Severity_Irreparability_1))
      Change_Severity_Irreparability<- round(Change_Severity_Irreparability, digits = 2)
      Change_Blame_Unfairness<- c(0, (Score_Blame_Unfairness_2 - Score_Blame_Unfairness_1))
      Change_Blame_Unfairness<- round(Change_Blame_Unfairness, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2)
        PTS_Severity_Irreparability_1<- (Rel_Severity_Irreparability * Score_Severity_Irreparability_1) + (M_Severity_Irreparability * (1 - Rel_Severity_Irreparability))
        PTS_Severity_Irreparability_2<- (Rel_Severity_Irreparability * Score_Severity_Irreparability_2) + (M_Severity_Irreparability * (1 - Rel_Severity_Irreparability))
        PTS_Severity_Irreparability<-c(PTS_Severity_Irreparability_1, PTS_Severity_Irreparability_2)
        PTS_Blame_Unfairness_1<- (Rel_Blame_Unfairness * Score_Blame_Unfairness_1) + (M_Blame_Unfairness * (1 - Rel_Blame_Unfairness))
        PTS_Blame_Unfairness_2<- (Rel_Blame_Unfairness * Score_Blame_Unfairness_2) + (M_Blame_Unfairness * (1 - Rel_Blame_Unfairness))
        PTS_Blame_Unfairness<-c(PTS_Blame_Unfairness_1,PTS_Blame_Unfairness_2)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2)
        PTS_Severity_Irreparability_1<- Score_Severity_Irreparability_1 + (M_Retest_Severity_Irreparability - M_Severity_Irreparability)  
        PTS_Severity_Irreparability_2<- Score_Severity_Irreparability_2 + (M_Retest_Severity_Irreparability - M_Severity_Irreparability)
        PTS_Severity_Irreparability<- c(PTS_Severity_Irreparability_1, PTS_Severity_Irreparability_2)
        PTS_Blame_Unfairness_1<- Score_Blame_Unfairness_1 + (M_Retest_Blame_Unfairness - M_Blame_Unfairness)  
        PTS_Blame_Unfairness_2<- Score_Blame_Unfairness_2 + (M_Retest_Blame_Unfairness - M_Blame_Unfairness)
        PTS_Blame_Unfairness<- c(PTS_Blame_Unfairness_1, PTS_Blame_Unfairness_2)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS<- c(PTS_1,PTS_2)
        PTS_Severity_Irreparability_1<- Score_Severity_Irreparability_1
        PTS_Severity_Irreparability_2<- Score_Severity_Irreparability_2
        PTS_Severity_Irreparability<- c(PTS_Severity_Irreparability_1, PTS_Severity_Irreparability_2)
        PTS_Blame_Unfairness_1<- Score_Blame_Unfairness_1
        PTS_Blame_Unfairness_2<- Score_Blame_Unfairness_2
        PTS_Blame_Unfairness<- c(PTS_Blame_Unfairness_1, PTS_Blame_Unfairness_2)
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Severity_Irreparability<- Rel_Severity_Irreparability * (SD_Retest_Severity_Irreparability/SD_Severity_Irreparability)
        A_Constant_Severity_Irreparability<- M_Retest_Severity_Irreparability - (B_Slope_Severity_Irreparability * M_Severity_Irreparability)
        B_Adj_Severity_Irreparability<- SD_Retest_Severity_Irreparability/SD_Severity_Irreparability
        A_Adj_Severity_Irreparability<- M_Retest_Severity_Irreparability - (B_Adj_Severity_Irreparability * M_Severity_Irreparability)
        PTS_Severity_Irreparability_1<- (B_Adj_Severity_Irreparability * Score_Severity_Irreparability_1) + A_Adj_Severity_Irreparability
        PTS_Severity_Irreparability_2<- (B_Adj_Severity_Irreparability * Score_Severity_Irreparability_2) + A_Adj_Severity_Irreparability
        PTS_Severity_Irreparability<- c(PTS_Severity_Irreparability_1,PTS_Severity_Irreparability_2)
        B_Slope_Blame_Unfairness<- Rel_Blame_Unfairness * (SD_Retest_Blame_Unfairness/SD_Blame_Unfairness)
        A_Constant_Blame_Unfairness<- M_Retest_Blame_Unfairness - (B_Slope_Blame_Unfairness * M_Blame_Unfairness)
        B_Adj_Blame_Unfairness<- SD_Retest_Blame_Unfairness/SD_Blame_Unfairness
        A_Adj_Blame_Unfairness<- M_Retest_Blame_Unfairness - (B_Adj_Blame_Unfairness * M_Blame_Unfairness)
        PTS_Blame_Unfairness_1<- (B_Adj_Blame_Unfairness * Score_Blame_Unfairness_1) + A_Adj_Blame_Unfairness
        PTS_Blame_Unfairness_2<- (B_Adj_Blame_Unfairness * Score_Blame_Unfairness_2) + A_Adj_Blame_Unfairness
        PTS_Blame_Unfairness<- c(PTS_Blame_Unfairness_1,PTS_Blame_Unfairness_2)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Severity_Irreparability<- Rel_Severity_Irreparability * (SD_Retest_Severity_Irreparability/SD_Severity_Irreparability)
        PTS_Severity_Irreparability_1<- B_Slope_Severity_Irreparability * Score_Severity_Irreparability_1
        PTS_Severity_Irreparability_2<- B_Slope_Severity_Irreparability * Score_Severity_Irreparability_2
        PTS_Severity_Irreparability<- c(PTS_Severity_Irreparability_1, PTS_Severity_Irreparability_2)
        B_Slope_Blame_Unfairness<- Rel_Blame_Unfairness * (SD_Retest_Blame_Unfairness/SD_Blame_Unfairness)
        PTS_Blame_Unfairness_1<- B_Slope_Blame_Unfairness * Score_Blame_Unfairness_1
        PTS_Blame_Unfairness_2<- B_Slope_Blame_Unfairness * Score_Blame_Unfairness_2
        PTS_Blame_Unfairness<- c(PTS_Blame_Unfairness_1, PTS_Blame_Unfairness_2)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2)
        PTS_Severity_Irreparability_1<- Score_Severity_Irreparability_1 + (M_Retest_Severity_Irreparability - M_Severity_Irreparability)
        PTS_Severity_Irreparability_2<- Score_Severity_Irreparability_2 + (M_Retest_Severity_Irreparability - M_Severity_Irreparability)
        PTS_Severity_Irreparability<- c(PTS_Severity_Irreparability_1, PTS_Severity_Irreparability_2)
        PTS_Blame_Unfairness_1<- Score_Blame_Unfairness_1 + (M_Retest_Blame_Unfairness - M_Blame_Unfairness)
        PTS_Blame_Unfairness_2<- Score_Blame_Unfairness_2 + (M_Retest_Blame_Unfairness - M_Blame_Unfairness)
        PTS_Blame_Unfairness<- c(PTS_Blame_Unfairness_1, PTS_Blame_Unfairness_2)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Severity_Irreparability<- round(PTS_Severity_Irreparability, digits = 2)
      PTS_Blame_Unfairness<- round(PTS_Blame_Unfairness, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2)
        SE_Severity_Irreparability_1<- McSweeny_SE_Severity_Irreparability*sqrt(1 + (1/SampleN) + ((Score_Severity_Irreparability_1 - M_Severity_Irreparability)^2/(SD_Severity_Irreparability^2*(SampleN-1))))
        SE_Severity_Irreparability_2<- McSweeny_SE_Severity_Irreparability*sqrt(1 + (1/SampleN) + ((Score_Severity_Irreparability_2 - M_Severity_Irreparability)^2/(SD_Severity_Irreparability^2*(SampleN-1))))
        SE_Severity_Irreparability<- c(SE_Severity_Irreparability_1, SE_Severity_Irreparability_2)
        SE_Blame_Unfairness_1<- McSweeny_SE_Blame_Unfairness*sqrt(1 + (1/SampleN) + ((Score_Blame_Unfairness_1 - M_Blame_Unfairness)^2/(SD_Blame_Unfairness^2*(SampleN-1))))
        SE_Blame_Unfairness_2<- McSweeny_SE_Blame_Unfairness*sqrt(1 + (1/SampleN) + ((Score_Blame_Unfairness_2 - M_Blame_Unfairness)^2/(SD_Blame_Unfairness^2*(SampleN-1))))
        SE_Blame_Unfairness<-c(SE_Blame_Unfairness_1, SE_Blame_Unfairness_2)
        SE<- round(SE, digits = 2)
        SE_Severity_Irreparability<- round(SE_Severity_Irreparability, digits = 2)
        SE_Blame_Unfairness<- round(SE_Blame_Unfairness, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2))
        CI<- round(CI, digits = 2)
        CI_Severity_Irreparability<- c((Conf*SE_Severity_Irreparability_1), (Conf*SE_Severity_Irreparability_2))
        CI_Severity_Irreparability<- round(CI_Severity_Irreparability, digits = 2)
        CI_Blame_Unfairness<- c((Conf*SE_Blame_Unfairness_1), (Conf*SE_Blame_Unfairness_2))
        CI_Blame_Unfairness<- round(CI_Blame_Unfairness, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Severity_Irreparability<- c((Conf*SE_Severity_Irreparability), (Conf*SE_Severity_Irreparability))
        CI_Severity_Irreparability<- round(CI_Severity_Irreparability, digits = 2)
        CI_Blame_Unfairness<- c((Conf*SE_Blame_Unfairness), (Conf*SE_Blame_Unfairness))
        CI_Blame_Unfairness<- round(CI_Blame_Unfairness, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Severity_Irreparability<- PTS_Severity_Irreparability + CI_Severity_Irreparability
      CI_Upper_Lim_Severity_Irreparability<- round(CI_Upper_Lim_Severity_Irreparability, digits = 2)
      CI_Lower_Lim_Severity_Irreparability<-PTS_Severity_Irreparability - CI_Severity_Irreparability
      CI_Lower_Lim_Severity_Irreparability<- round(CI_Lower_Lim_Severity_Irreparability, digits = 2)
      CI_Upper_Lim_Blame_Unfairness<- PTS_Blame_Unfairness + CI_Blame_Unfairness
      CI_Upper_Lim_Blame_Unfairness<- round(CI_Upper_Lim_Blame_Unfairness, digits = 2)
      CI_Lower_Lim_Blame_Unfairness<-PTS_Blame_Unfairness - CI_Blame_Unfairness
      CI_Lower_Lim_Blame_Unfairness<- round(CI_Lower_Lim_Blame_Unfairness, digits = 2)
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Severity_Irreparability == "2") {
        CI_Severity_Irreparability<- input$Man_CI_Severity_Irreparability
        CI_Severity_Irreparability<- c(CI_Severity_Irreparability, CI_Severity_Irreparability)
        CI_Severity_Irreparability<- round(CI_Severity_Irreparability, digits = 2)
        CI_Upper_Lim_Severity_Irreparability<- Score_Severity_Irreparability + CI_Severity_Irreparability
        CI_Upper_Lim_Severity_Irreparability<- round(CI_Upper_Lim_Severity_Irreparability, digits = 2)
        CI_Lower_Lim_Severity_Irreparability<- Score_Severity_Irreparability - CI_Severity_Irreparability
        CI_Lower_Lim_Severity_Irreparability<- round(CI_Lower_Lim_Severity_Irreparability, digits = 2)
      }
      if(input$Select_CI_Blame_Unfairness == "2") {
        CI_Blame_Unfairness<- input$Man_CI_Blame_Unfairness
        CI_Blame_Unfairness<- c(CI_Blame_Unfairness, CI_Blame_Unfairness)
        CI_Blame_Unfairness<- round(CI_Blame_Unfairness, digits = 2)
        CI_Upper_Lim_Blame_Unfairness<- Score_Blame_Unfairness + CI_Blame_Unfairness
        CI_Upper_Lim_Blame_Unfairness<- round(CI_Upper_Lim_Blame_Unfairness, digits = 2)
        CI_Lower_Lim_Blame_Unfairness<- Score_Blame_Unfairness - CI_Blame_Unfairness
        CI_Lower_Lim_Blame_Unfairness<- round(CI_Lower_Lim_Blame_Unfairness, digits = 2)
      } 
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_1<- rep(Cutoff_Score_1, 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_2<- rep(Cutoff_Score_2, 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_3<- rep(Cutoff_Score_3, 2)
      Cutoff_Score_4<- round(input$Cutoff_4, digits = 2)
      Cutoff_Score_4<- rep(Cutoff_Score_4, 2)
      Cutoff_Score_5<- round(input$Cutoff_5, digits = 2)
      Cutoff_Score_5<- rep(Cutoff_Score_5, 2)
      Cutoff_Score_Severity_Irreparability_1<- round(input$Cutoff_Severity_Irreparability_1, digits = 2)
      Cutoff_Score_Severity_Irreparability_1<- rep(Cutoff_Score_Severity_Irreparability_1, 2)
      Cutoff_Score_Severity_Irreparability_2<- round(input$Cutoff_Severity_Irreparability_2, digits = 2)
      Cutoff_Score_Severity_Irreparability_2<- rep(Cutoff_Score_Severity_Irreparability_2, 2)
      Cutoff_Score_Severity_Irreparability_3<- round(input$Cutoff_Severity_Irreparability_3, digits = 2)
      Cutoff_Score_Severity_Irreparability_3<- rep(Cutoff_Score_Severity_Irreparability_3, 2)
      Cutoff_Score_Severity_Irreparability_4<- round(input$Cutoff_Severity_Irreparability_4, digits = 2)
      Cutoff_Score_Severity_Irreparability_4<- rep(Cutoff_Score_Severity_Irreparability_4, 2)
      Cutoff_Score_Severity_Irreparability_5<- round(input$Cutoff_Severity_Irreparability_5, digits = 2)
      Cutoff_Score_Severity_Irreparability_5<- rep(Cutoff_Score_Severity_Irreparability_5, 2)
      Cutoff_Score_Blame_Unfairness_1<- round(input$Cutoff_Blame_Unfairness_1, digits = 2)
      Cutoff_Score_Blame_Unfairness_1<- rep(Cutoff_Score_Blame_Unfairness_1, 2)
      Cutoff_Score_Blame_Unfairness_2<- round(input$Cutoff_Blame_Unfairness_2, digits = 2)
      Cutoff_Score_Blame_Unfairness_2<- rep(Cutoff_Score_Blame_Unfairness_2, 2)
      Cutoff_Score_Blame_Unfairness_3<- round(input$Cutoff_Blame_Unfairness_3, digits = 2)
      Cutoff_Score_Blame_Unfairness_3<- rep(Cutoff_Score_Blame_Unfairness_3, 2)
      Cutoff_Score_Blame_Unfairness_4<- round(input$Cutoff_Blame_Unfairness_4, digits = 2)
      Cutoff_Score_Blame_Unfairness_4<- rep(Cutoff_Score_Blame_Unfairness_4, 2)
      Cutoff_Score_Blame_Unfairness_5<- round(input$Cutoff_Blame_Unfairness_5, digits = 2)
      Cutoff_Score_Blame_Unfairness_5<- rep(Cutoff_Score_Blame_Unfairness_5, 2)
  
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Cutoff_Score_4,Cutoff_Score_5,
                                      Score_Severity_Irreparability,Change_Severity_Irreparability,PTS_Severity_Irreparability, SE_Severity_Irreparability, CI_Upper_Lim_Severity_Irreparability, CI_Lower_Lim_Severity_Irreparability, Cutoff_Score_Severity_Irreparability_1,Cutoff_Score_Severity_Irreparability_2,Cutoff_Score_Severity_Irreparability_3,Cutoff_Score_Severity_Irreparability_4,Cutoff_Score_Severity_Irreparability_5,
                                      Score_Blame_Unfairness,Change_Blame_Unfairness, PTS_Blame_Unfairness, SE_Blame_Unfairness, CI_Upper_Lim_Blame_Unfairness, CI_Lower_Lim_Blame_Unfairness, Cutoff_Score_Blame_Unfairness_1,Cutoff_Score_Blame_Unfairness_2,Cutoff_Score_Blame_Unfairness_3,Cutoff_Score_Blame_Unfairness_4,Cutoff_Score_Blame_Unfairness_5
                                      )
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
      Score_Severity_Irreparability_1<- sum(Score_1a[c(1,2,4,5,6,8)], na.rm = TRUE)
      Score_Severity_Irreparability_2<- sum(Score_2a[c(1,2,4,5,6,8)], na.rm = TRUE)
      Score_Severity_Irreparability_3<- sum(Score_3a[c(1,2,4,5,6,8)], na.rm = TRUE)
      Score_Severity_Irreparability<- c(Score_Severity_Irreparability_1,Score_Severity_Irreparability_2,Score_Severity_Irreparability_3)
      Score_Severity_Irreparability<- round(Score_Severity_Irreparability, digits = 2)
      Score_Blame_Unfairness_1<- sum(Score_1a[c(3,7,9,10,11,12)], na.rm = TRUE)
      Score_Blame_Unfairness_2<- sum(Score_2a[c(3,7,9,10,11,12)], na.rm = TRUE)
      Score_Blame_Unfairness_3<- sum(Score_3a[c(3,7,9,10,11,12)], na.rm = TRUE)
      Score_Blame_Unfairness<- c(Score_Blame_Unfairness_1,Score_Blame_Unfairness_2, Score_Blame_Unfairness_3)
      Score_Blame_Unfairness<- round(Score_Blame_Unfairness, digits = 2)
      Change<- c(0, (Score_2 - Score_1), (Score_3 - Score_2))
      Change<- round(Change, digits = 2)
      Change_Severity_Irreparability<- c(0, Score_Severity_Irreparability_2 - Score_Severity_Irreparability_1, Score_Severity_Irreparability_3 - Score_Severity_Irreparability_2)
      Change_Severity_Irreparability<- round(Change_Severity_Irreparability, digits = 2)
      Change_Blame_Unfairness<- c(0, Score_Blame_Unfairness_2 - Score_Blame_Unfairness_1, Score_Blame_Unfairness_3 - Score_Blame_Unfairness_2)
      Change_Blame_Unfairness<- round(Change_Blame_Unfairness, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS_3<- (Rel * Score_3) + (M * (1 - Rel))
        PTS<<- c(PTS_1, PTS_2, PTS_3)
        PTS_Severity_Irreparability_1<- (Rel_Severity_Irreparability * Score_Severity_Irreparability_1) + (M_Severity_Irreparability * (1 - Rel_Severity_Irreparability))
        PTS_Severity_Irreparability_2<- (Rel_Severity_Irreparability * Score_Severity_Irreparability_2) + (M_Severity_Irreparability * (1 - Rel_Severity_Irreparability))
        PTS_Severity_Irreparability_3<- (Rel_Severity_Irreparability * Score_Severity_Irreparability_3) + (M_Severity_Irreparability * (1 - Rel_Severity_Irreparability))
        PTS_Severity_Irreparability<<- c(PTS_Severity_Irreparability_1, PTS_Severity_Irreparability_2, PTS_Severity_Irreparability_3)
        PTS_Blame_Unfairness_1<- (Rel_Blame_Unfairness * Score_Blame_Unfairness_1) + (M_Blame_Unfairness * (1 - Rel_Blame_Unfairness))
        PTS_Blame_Unfairness_2<- (Rel_Blame_Unfairness * Score_Blame_Unfairness_2) + (M_Blame_Unfairness * (1 - Rel_Blame_Unfairness))
        PTS_Blame_Unfairness_3<- (Rel_Blame_Unfairness * Score_Blame_Unfairness_3) + (M_Blame_Unfairness * (1 - Rel_Blame_Unfairness))
        PTS_Blame_Unfairness<- c(PTS_Blame_Unfairness_1,PTS_Blame_Unfairness_2, PTS_Blame_Unfairness_3)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Severity_Irreparability_1<- Score_Severity_Irreparability_1 + (M_Retest_Severity_Irreparability - M_Severity_Irreparability)  
        PTS_Severity_Irreparability_2<- Score_Severity_Irreparability_2 + (M_Retest_Severity_Irreparability - M_Severity_Irreparability)
        PTS_Severity_Irreparability_3<- Score_Severity_Irreparability_3 + (M_Retest_Severity_Irreparability - M_Severity_Irreparability)
        PTS_Severity_Irreparability<- c(PTS_Severity_Irreparability_1, PTS_Severity_Irreparability_2, PTS_Severity_Irreparability_3)
        PTS_Blame_Unfairness_1<- Score_Blame_Unfairness_1 + (M_Retest_Blame_Unfairness - M_Blame_Unfairness)  
        PTS_Blame_Unfairness_2<- Score_Blame_Unfairness_2 + (M_Retest_Blame_Unfairness - M_Blame_Unfairness)
        PTS_Blame_Unfairness_3<- Score_Blame_Unfairness_3 + (M_Retest_Blame_Unfairness - M_Blame_Unfairness)
        PTS_Blame_Unfairness<- c(PTS_Blame_Unfairness_1, PTS_Blame_Unfairness_2, PTS_Blame_Unfairness_3)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS_3<- Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Severity_Irreparability_1<- Score_Severity_Irreparability_1
        PTS_Severity_Irreparability_2<- Score_Severity_Irreparability_2
        PTS_Severity_Irreparability_3<- Score_Severity_Irreparability_3
        PTS_Severity_Irreparability<- c(PTS_Severity_Irreparability_1, PTS_Severity_Irreparability_2, PTS_Severity_Irreparability_3)
        PTS_Blame_Unfairness_1<- Score_Blame_Unfairness_1
        PTS_Blame_Unfairness_2<- Score_Blame_Unfairness_2
        PTS_Blame_Unfairness_3<- Score_Blame_Unfairness_3
        PTS_Blame_Unfairness<- c(PTS_Blame_Unfairness_1, PTS_Blame_Unfairness_2, PTS_Blame_Unfairness_3)
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS_3<- (B_Adj * Score_3) + A_Adj
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Severity_Irreparability<- Rel_Severity_Irreparability * (SD_Retest_Severity_Irreparability/SD_Severity_Irreparability)
        A_Constant_Severity_Irreparability<- M_Retest_Severity_Irreparability - (B_Slope_Severity_Irreparability * M_Severity_Irreparability)
        B_Adj_Severity_Irreparability<- SD_Retest_Severity_Irreparability/SD_Severity_Irreparability
        A_Adj_Severity_Irreparability<- M_Retest_Severity_Irreparability - (B_Adj_Severity_Irreparability * M_Severity_Irreparability)
        PTS_Severity_Irreparability_1<- (B_Adj_Severity_Irreparability * Score_Severity_Irreparability_1) + A_Adj_Severity_Irreparability
        PTS_Severity_Irreparability_2<- (B_Adj_Severity_Irreparability * Score_Severity_Irreparability_2) + A_Adj_Severity_Irreparability
        PTS_Severity_Irreparability_3<- (B_Adj_Severity_Irreparability * Score_Severity_Irreparability_3) + A_Adj_Severity_Irreparability
        PTS_Severity_Irreparability<- c(PTS_Severity_Irreparability_1, PTS_Severity_Irreparability_2, PTS_Severity_Irreparability_3)
        B_Slope_Blame_Unfairness<- Rel_Blame_Unfairness * (SD_Retest_Blame_Unfairness/SD_Blame_Unfairness)
        A_Constant_Blame_Unfairness<- M_Retest_Blame_Unfairness - (B_Slope_Blame_Unfairness * M_Blame_Unfairness)
        B_Adj_Blame_Unfairness<- SD_Retest_Blame_Unfairness/SD_Blame_Unfairness
        A_Adj_Blame_Unfairness<- M_Retest_Blame_Unfairness - (B_Adj_Blame_Unfairness * M_Blame_Unfairness)
        PTS_Blame_Unfairness_1<- (B_Adj_Blame_Unfairness * Score_Blame_Unfairness_1) + A_Adj_Blame_Unfairness
        PTS_Blame_Unfairness_2<- (B_Adj_Blame_Unfairness * Score_Blame_Unfairness_2) + A_Adj_Blame_Unfairness
        PTS_Blame_Unfairness_3<- (B_Adj_Blame_Unfairness * Score_Blame_Unfairness_3) + A_Adj_Blame_Unfairness
        PTS_Blame_Unfairness<- c(PTS_Blame_Unfairness_1, PTS_Blame_Unfairness_2, PTS_Blame_Unfairness_3)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope * Score_1
        PTS_2<- B_Slope * Score_2
        PTS_3<- B_Slope * Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Severity_Irreparability<- Rel_Severity_Irreparability * (SD_Retest_Severity_Irreparability/SD_Severity_Irreparability)
        PTS_Severity_Irreparability_1<- B_Slope_Severity_Irreparability * Score_Severity_Irreparability_1
        PTS_Severity_Irreparability_2<- B_Slope_Severity_Irreparability * Score_Severity_Irreparability_2
        PTS_Severity_Irreparability_3<- B_Slope_Severity_Irreparability * Score_Severity_Irreparability_3
        PTS_Severity_Irreparability<- c(PTS_Severity_Irreparability_1, PTS_Severity_Irreparability_2, PTS_Severity_Irreparability_3)
        B_Slope_Blame_Unfairness<- Rel_Blame_Unfairness * (SD_Retest_Blame_Unfairness/SD_Blame_Unfairness)
        PTS_Blame_Unfairness_1<- B_Slope_Blame_Unfairness * Score_Blame_Unfairness_1
        PTS_Blame_Unfairness_2<- B_Slope_Blame_Unfairness * Score_Blame_Unfairness_2
        PTS_Blame_Unfairness_3<- B_Slope_Blame_Unfairness * Score_Blame_Unfairness_3
        PTS_Blame_Unfairness<- c(PTS_Blame_Unfairness_1, PTS_Blame_Unfairness_2, PTS_Blame_Unfairness_3)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Severity_Irreparability_1<- Score_Severity_Irreparability_1 + (M_Retest_Severity_Irreparability - M_Severity_Irreparability)
        PTS_Severity_Irreparability_2<- Score_Severity_Irreparability_2 + (M_Retest_Severity_Irreparability - M_Severity_Irreparability)
        PTS_Severity_Irreparability_3<- Score_Severity_Irreparability_3 + (M_Retest_Severity_Irreparability - M_Severity_Irreparability)
        PTS_Severity_Irreparability<- c(PTS_Severity_Irreparability_1, PTS_Severity_Irreparability_2, PTS_Severity_Irreparability_3)
        PTS_Blame_Unfairness_1<- Score_Blame_Unfairness_1 + (M_Retest_Blame_Unfairness - M_Blame_Unfairness)
        PTS_Blame_Unfairness_2<- Score_Blame_Unfairness_2 + (M_Retest_Blame_Unfairness - M_Blame_Unfairness)
        PTS_Blame_Unfairness_3<- Score_Blame_Unfairness_3 + (M_Retest_Blame_Unfairness - M_Blame_Unfairness)
        PTS_Blame_Unfairness<- c(PTS_Blame_Unfairness_1, PTS_Blame_Unfairness_2, PTS_Blame_Unfairness_3)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Severity_Irreparability<- round(PTS_Severity_Irreparability, digits = 2)
      PTS_Blame_Unfairness<- round(PTS_Blame_Unfairness, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE3<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_3 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2, SE3)
        SE_Severity_Irreparability_1<- McSweeny_SE_Severity_Irreparability*sqrt(1 + (1/SampleN) + ((Score_Severity_Irreparability_1 - M_Severity_Irreparability)^2/(SD_Severity_Irreparability^2*(SampleN-1))))
        SE_Severity_Irreparability_2<- McSweeny_SE_Severity_Irreparability*sqrt(1 + (1/SampleN) + ((Score_Severity_Irreparability_2 - M_Severity_Irreparability)^2/(SD_Severity_Irreparability^2*(SampleN-1))))
        SE_Severity_Irreparability_3<- McSweeny_SE_Severity_Irreparability*sqrt(1 + (1/SampleN) + ((Score_Severity_Irreparability_3 - M_Severity_Irreparability)^2/(SD_Severity_Irreparability^2*(SampleN-1))))
        SE_Severity_Irreparability<- c(SE_Severity_Irreparability_1, SE_Severity_Irreparability_2, SE_Severity_Irreparability_3)
        SE_Blame_Unfairness_1<- McSweeny_SE_Blame_Unfairness*sqrt(1 + (1/SampleN) + ((Score_Blame_Unfairness_1 - M_Blame_Unfairness)^2/(SD_Blame_Unfairness^2*(SampleN-1))))
        SE_Blame_Unfairness_2<- McSweeny_SE_Blame_Unfairness*sqrt(1 + (1/SampleN) + ((Score_Blame_Unfairness_2 - M_Blame_Unfairness)^2/(SD_Blame_Unfairness^2*(SampleN-1))))
        SE_Blame_Unfairness_3<- McSweeny_SE_Blame_Unfairness*sqrt(1 + (1/SampleN) + ((Score_Blame_Unfairness_3 - M_Blame_Unfairness)^2/(SD_Blame_Unfairness^2*(SampleN-1))))
        SE_Blame_Unfairness<- c(SE_Blame_Unfairness_1, SE_Blame_Unfairness_2, SE_Blame_Unfairness_3)
        SE<- round(SE, digits = 2)
        SE_Severity_Irreparability<- round(SE_Severity_Irreparability, digits = 2)
        SE_Blame_Unfairness<- round(SE_Blame_Unfairness, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2), (Conf*SE3))
        CI<- round(CI, digits = 2)
        CI_Severity_Irreparability<- c((Conf*SE_Severity_Irreparability_1), (Conf*SE_Severity_Irreparability_2), (Conf*SE_Severity_Irreparability_3))
        CI_Severity_Irreparability<- round(CI_Severity_Irreparability, digits = 2)
        CI_Blame_Unfairness<- c((Conf*SE_Blame_Unfairness_1), (Conf*SE_Blame_Unfairness_2), (Conf*SE_Blame_Unfairness_3))
        CI_Blame_Unfairness<- round(CI_Blame_Unfairness, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Severity_Irreparability<- c((Conf*SE_Severity_Irreparability), (Conf*SE_Severity_Irreparability), (Conf*SE_Severity_Irreparability))
        CI_Severity_Irreparability<- round(CI_Severity_Irreparability, digits = 2)
        CI_Blame_Unfairness<- c((Conf*SE_Blame_Unfairness), (Conf*SE_Blame_Unfairness), (Conf*SE_Blame_Unfairness))
        CI_Blame_Unfairness<- round(CI_Blame_Unfairness, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Severity_Irreparability<- PTS_Severity_Irreparability + CI_Severity_Irreparability
      CI_Upper_Lim_Severity_Irreparability<- round(CI_Upper_Lim_Severity_Irreparability, digits = 2)
      CI_Lower_Lim_Severity_Irreparability<-PTS_Severity_Irreparability - CI_Severity_Irreparability
      CI_Lower_Lim_Severity_Irreparability<- round(CI_Lower_Lim_Severity_Irreparability, digits = 2)
      CI_Upper_Lim_Blame_Unfairness<- PTS_Blame_Unfairness + CI_Blame_Unfairness
      CI_Upper_Lim_Blame_Unfairness<- round(CI_Upper_Lim_Blame_Unfairness, digits = 2)
      CI_Lower_Lim_Blame_Unfairness<-PTS_Blame_Unfairness - CI_Blame_Unfairness
      CI_Lower_Lim_Blame_Unfairness<- round(CI_Lower_Lim_Blame_Unfairness, digits = 2)
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Severity_Irreparability == "2") {
        CI_Severity_Irreparability<- input$Man_CI_Severity_Irreparability
        CI_Severity_Irreparability<- c(CI_Severity_Irreparability, CI_Severity_Irreparability, CI_Severity_Irreparability)
        CI_Severity_Irreparability<- round(CI_Severity_Irreparability, digits = 2)
        CI_Upper_Lim_Severity_Irreparability<- Score_Severity_Irreparability + CI_Severity_Irreparability
        CI_Upper_Lim_Severity_Irreparability<- round(CI_Upper_Lim_Severity_Irreparability, digits = 2)
        CI_Lower_Lim_Severity_Irreparability<- Score_Severity_Irreparability - CI_Severity_Irreparability
        CI_Lower_Lim_Severity_Irreparability<- round(CI_Lower_Lim_Severity_Irreparability, digits = 2)
      }
      if(input$Select_CI_Blame_Unfairness == "2") {
        CI_Blame_Unfairness<- input$Man_CI_Blame_Unfairness
        CI_Blame_Unfairness<- c(CI_Blame_Unfairness, CI_Blame_Unfairness, CI_Blame_Unfairness)
        CI_Blame_Unfairness<- round(CI_Blame_Unfairness, digits = 2)
        CI_Upper_Lim_Blame_Unfairness<- Score_Blame_Unfairness + CI_Blame_Unfairness
        CI_Upper_Lim_Blame_Unfairness<- round(CI_Upper_Lim_Blame_Unfairness, digits = 2)
        CI_Lower_Lim_Blame_Unfairness<- Score_Blame_Unfairness - CI_Blame_Unfairness
        CI_Lower_Lim_Blame_Unfairness<- round(CI_Lower_Lim_Blame_Unfairness, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_1<- rep(Cutoff_Score_1, 3)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_2<- rep(Cutoff_Score_2, 3)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_3<- rep(Cutoff_Score_3, 3)
      Cutoff_Score_4<- round(input$Cutoff_4, digits = 2)
      Cutoff_Score_4<- rep(Cutoff_Score_4, 3)
      Cutoff_Score_5<- round(input$Cutoff_5, digits = 2)
      Cutoff_Score_5<- rep(Cutoff_Score_5, 3)
      Cutoff_Score_Severity_Irreparability_1<- round(input$Cutoff_Severity_Irreparability_1, digits = 2)
      Cutoff_Score_Severity_Irreparability_1<- rep(Cutoff_Score_Severity_Irreparability_1, 3)
      Cutoff_Score_Severity_Irreparability_2<- round(input$Cutoff_Severity_Irreparability_2, digits = 2)
      Cutoff_Score_Severity_Irreparability_2<- rep(Cutoff_Score_Severity_Irreparability_2, 3)
      Cutoff_Score_Severity_Irreparability_3<- round(input$Cutoff_Severity_Irreparability_3, digits = 2)
      Cutoff_Score_Severity_Irreparability_3<- rep(Cutoff_Score_Severity_Irreparability_3, 3)
      Cutoff_Score_Severity_Irreparability_4<- round(input$Cutoff_Severity_Irreparability_4, digits = 2)
      Cutoff_Score_Severity_Irreparability_4<- rep(Cutoff_Score_Severity_Irreparability_4, 3)
      Cutoff_Score_Severity_Irreparability_5<- round(input$Cutoff_Severity_Irreparability_5, digits = 2)
      Cutoff_Score_Severity_Irreparability_5<- rep(Cutoff_Score_Severity_Irreparability_5, 3)
      Cutoff_Score_Blame_Unfairness_1<- round(input$Cutoff_Blame_Unfairness_1, digits = 2)
      Cutoff_Score_Blame_Unfairness_1<- rep(Cutoff_Score_Blame_Unfairness_1, 3)
      Cutoff_Score_Blame_Unfairness_2<- round(input$Cutoff_Blame_Unfairness_2, digits = 2)
      Cutoff_Score_Blame_Unfairness_2<- rep(Cutoff_Score_Blame_Unfairness_2, 3)
      Cutoff_Score_Blame_Unfairness_3<- round(input$Cutoff_Blame_Unfairness_3, digits = 2)
      Cutoff_Score_Blame_Unfairness_3<- rep(Cutoff_Score_Blame_Unfairness_3, 3)
      Cutoff_Score_Blame_Unfairness_4<- round(input$Cutoff_Blame_Unfairness_4, digits = 2)
      Cutoff_Score_Blame_Unfairness_4<- rep(Cutoff_Score_Blame_Unfairness_4, 3)
      Cutoff_Score_Blame_Unfairness_5<- round(input$Cutoff_Blame_Unfairness_5, digits = 2)
      Cutoff_Score_Blame_Unfairness_5<- rep(Cutoff_Score_Blame_Unfairness_5, 3)

      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Cutoff_Score_4,Cutoff_Score_5,
                                      Score_Severity_Irreparability,Change_Severity_Irreparability,PTS_Severity_Irreparability, SE_Severity_Irreparability, CI_Upper_Lim_Severity_Irreparability, CI_Lower_Lim_Severity_Irreparability, Cutoff_Score_Severity_Irreparability_1,Cutoff_Score_Severity_Irreparability_2,Cutoff_Score_Severity_Irreparability_3,Cutoff_Score_Severity_Irreparability_4,Cutoff_Score_Severity_Irreparability_5,
                                      Score_Blame_Unfairness,Change_Blame_Unfairness, PTS_Blame_Unfairness, SE_Blame_Unfairness, CI_Upper_Lim_Blame_Unfairness, CI_Lower_Lim_Blame_Unfairness, Cutoff_Score_Blame_Unfairness_1,Cutoff_Score_Blame_Unfairness_2,Cutoff_Score_Blame_Unfairness_3,Cutoff_Score_Blame_Unfairness_4,Cutoff_Score_Blame_Unfairness_5
                                      )
                                      
    }
    
  
    #Create dataframes to be displayed as tables in pdf report (statistical values used in reliable change analyses)
    
    if(input$RelChangeMethod == 'Nunnally & Bernstein (1994)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Severity_Irreparability<<- data.frame(Pop,  M_Severity_Irreparability, SD_Severity_Irreparability, RelChangeMethod, Rel_Severity_Irreparability, ConfInt)
      names(Stats_Table_Severity_Irreparability)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Blame_Unfairness<<- data.frame(Pop,  M_Blame_Unfairness, SD_Blame_Unfairness, RelChangeMethod, Rel_Blame_Unfairness, ConfInt)
      names(Stats_Table_Blame_Unfairness)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Chelune et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Severity_Irreparability<<- data.frame(Pop,  M_Severity_Irreparability, M_Retest_Severity_Irreparability, SD_Severity_Irreparability, RelChangeMethod, Rel_Severity_Irreparability, ConfInt)
      names(Stats_Table_Severity_Irreparability)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Blame_Unfairness<<- data.frame(Pop,  M_Blame_Unfairness, M_Retest_Blame_Unfairness, SD_Blame_Unfairness, RelChangeMethod, Rel_Blame_Unfairness, ConfInt)
      names(Stats_Table_Blame_Unfairness)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Maassen et al. (2006)' | input$RelChangeMethod == 'McSweeny et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Severity_Irreparability<<- data.frame(Pop,  M_Severity_Irreparability, M_Retest_Severity_Irreparability, SD_Severity_Irreparability, SD_Retest_Severity_Irreparability, RelChangeMethod, Rel_Severity_Irreparability, ConfInt)
      names(Stats_Table_Severity_Irreparability)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Blame_Unfairness<<- data.frame(Pop,  M_Blame_Unfairness, M_Retest_Blame_Unfairness, SD_Blame_Unfairness, SD_Retest_Blame_Unfairness, RelChangeMethod, Rel_Blame_Unfairness, ConfInt)
      names(Stats_Table_Blame_Unfairness)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Crawford & Howell (1998)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, SampleN, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Severity_Irreparability<<- data.frame(Pop,  M_Severity_Irreparability, M_Retest_Severity_Irreparability, SD_Severity_Irreparability, SD_Retest_Severity_Irreparability, RelChangeMethod, Rel_Severity_Irreparability, SampleN,ConfInt)
      names(Stats_Table_Severity_Irreparability)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Blame_Unfairness<<- data.frame(Pop,  M_Blame_Unfairness, M_Retest_Blame_Unfairness, SD_Blame_Unfairness, SD_Retest_Blame_Unfairness, RelChangeMethod, Rel_Blame_Unfairness, SampleN, ConfInt)
      names(Stats_Table_Blame_Unfairness)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
    } else if (input$RelChangeMethod == 'Jacobson & Truax (1991)' | input$RelChangeMethod == 'Speer (1992)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Severity_Irreparability<<- data.frame(Pop,  SD_Severity_Irreparability, RelChangeMethod, Rel_Severity_Irreparability, ConfInt)
      names(Stats_Table_Severity_Irreparability)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Blame_Unfairness<<- data.frame(Pop,  SD_Blame_Unfairness, RelChangeMethod, Rel_Blame_Unfairness, ConfInt)
      names(Stats_Table_Blame_Unfairness)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
    }
    
    #If custom confidence intervals options are selected, make sure that no values appear for PS and SE in pdf tables
    #Change the look of tables in report if custom confidence intervals options are selected
    
    if (input$Select_CI == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS = NA, SE = NA)
      Stats_Table_Fullscale<<- Stats_Table_Fullscale %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI[1])
    }
    if (input$Select_CI_Severity_Irreparability == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Severity_Irreparability = NA, SE_Severity_Irreparability = NA)
      Stats_Table_Severity_Irreparability<<- Stats_Table_Severity_Irreparability %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                                    "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Severity_Irreparability[1])
    }
    if (input$Select_CI_Blame_Unfairness == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Blame_Unfairness = NA, SE_Blame_Unfairness = NA)
      Stats_Table_Blame_Unfairness<<- Stats_Table_Blame_Unfairness %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                              "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Blame_Unfairness[1])
    }
    
    
    #Create a dataframe to be used in the analytics spreadsheet.
    
    #Calculate variables for significant improvement
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$CI_Lower_Lim[1]) {
     IEQ.Fullscale.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$CI_Lower_Lim[1]) {
     IEQ.Fullscale.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$CI_Upper_Lim[1]) {
     IEQ.Fullscale.Sig.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$CI_Upper_Lim[1]) {
     IEQ.Fullscale.Sig.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Severity_Irreparability[length(Entered_Scores_Df$Score_Severity_Irreparability)] < Entered_Scores_Df$CI_Lower_Lim_Severity_Irreparability[1]) {
     IEQ.Severity.Irreparability.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Severity_Irreparability[length(Entered_Scores_Df$Score_Severity_Irreparability)] >= Entered_Scores_Df$CI_Lower_Lim_Severity_Irreparability[1]) {
     IEQ.Severity.Irreparability.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Severity_Irreparability[length(Entered_Scores_Df$Score_Severity_Irreparability)] > Entered_Scores_Df$CI_Upper_Lim_Severity_Irreparability[1]) {
     IEQ.Severity.Irreparability.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Severity_Irreparability[length(Entered_Scores_Df$Score_Severity_Irreparability)] <= Entered_Scores_Df$CI_Upper_Lim_Severity_Irreparability[1]) {
     IEQ.Severity.Irreparability.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Blame_Unfairness[length(Entered_Scores_Df$Score_Blame_Unfairness)] < Entered_Scores_Df$CI_Lower_Lim_Blame_Unfairness[1]) {
     IEQ.Blame.Unfairness.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Blame_Unfairness[length(Entered_Scores_Df$Score_Blame_Unfairness)] >= Entered_Scores_Df$CI_Lower_Lim_Blame_Unfairness[1]) {
     IEQ.Blame.Unfairness.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Blame_Unfairness[length(Entered_Scores_Df$Score_Blame_Unfairness)] > Entered_Scores_Df$CI_Upper_Lim_Blame_Unfairness[1]) {
     IEQ.Blame.Unfairness.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Blame_Unfairness[length(Entered_Scores_Df$Score_Blame_Unfairness)] <= Entered_Scores_Df$CI_Upper_Lim_Blame_Unfairness[1]) {
     IEQ.Blame.Unfairness.Sig.Deterioration<- "No"
    }
    
    
    
    #Calculate variables for improvement
    
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$Score[1]) {
     IEQ.Fullscale.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$Score[1]) {
     IEQ.Fullscale.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$Score[1]) {
     IEQ.Fullscale.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$Score[1]) {
     IEQ.Fullscale.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Severity_Irreparability[length(Entered_Scores_Df$Score_Severity_Irreparability)] < Entered_Scores_Df$Score_Severity_Irreparability[1]) {
     IEQ.Severity.Irreparability.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Severity_Irreparability[length(Entered_Scores_Df$Score_Severity_Irreparability)] >= Entered_Scores_Df$Score_Severity_Irreparability[1]) {
     IEQ.Severity.Irreparability.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Severity_Irreparability[length(Entered_Scores_Df$Score_Severity_Irreparability)] > Entered_Scores_Df$Score_Severity_Irreparability[1]) {
     IEQ.Severity.Irreparability.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Severity_Irreparability[length(Entered_Scores_Df$Score_Severity_Irreparability)] <= Entered_Scores_Df$Score_Severity_Irreparability[1]) {
     IEQ.Severity.Irreparability.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Blame_Unfairness[length(Entered_Scores_Df$Score_Blame_Unfairness)] < Entered_Scores_Df$Score_Blame_Unfairness[1]) {
     IEQ.Blame.Unfairness.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Blame_Unfairness[length(Entered_Scores_Df$Score_Blame_Unfairness)] >= Entered_Scores_Df$Score_Blame_Unfairness[1]) {
     IEQ.Blame.Unfairness.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Blame_Unfairness[length(Entered_Scores_Df$Score_Blame_Unfairness)] > Entered_Scores_Df$Score_Blame_Unfairness[1]) {
     IEQ.Blame.Unfairness.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Blame_Unfairness[length(Entered_Scores_Df$Score_Blame_Unfairness)] <= Entered_Scores_Df$Score_Blame_Unfairness[1]) {
     IEQ.Blame.Unfairness.Deterioration<- "No"
    }
    
   
   IEQ.Fullscale.Change<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] - Entered_Scores_Df$Score[1]
   IEQ.Severity.Irreparability.Change<- Entered_Scores_Df$Score_Severity_Irreparability[length(Entered_Scores_Df$Score_Severity_Irreparability)] - Entered_Scores_Df$Score_Severity_Irreparability[1]
   IEQ.Blame.Unfairness.Change<- Entered_Scores_Df$Score_Blame_Unfairness[length(Entered_Scores_Df$Score_Blame_Unfairness)] - Entered_Scores_Df$Score_Blame_Unfairness[1]
   IEQ.Fullscale.Comparisons<- length(Entered_Scores_Df$Change) - 1
   IEQ.Severity.Irreparability.Comparisons<- length(Entered_Scores_Df$Change_Severity_Irreparability) - 1
   IEQ.Blame.Unfairness.Comparisons<- length(Entered_Scores_Df$Change_Blame_Unfairness) - 1
   IEQ.Fullscale.First.Date<- Entered_Scores_Df$Date[1]
   IEQ.Severity.Irreparability.First.Date<- Entered_Scores_Df$Date[1]
   IEQ.Blame.Unfairness.First.Date<- Entered_Scores_Df$Date[1]
   IEQ.Fullscale.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
   IEQ.Severity.Irreparability.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
   IEQ.Blame.Unfairness.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
   IEQ.Fullscale.First.Score<- Entered_Scores_Df$Score[1]
   IEQ.Severity.Irreparability.First.Score<- Entered_Scores_Df$Score_Severity_Irreparability[1]
   IEQ.Blame.Unfairness.First.Score<- Entered_Scores_Df$Score_Blame_Unfairness[1]
   IEQ.Fullscale.Last.Score<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)]
   IEQ.Severity.Irreparability.Last.Score<- Entered_Scores_Df$Score_Severity_Irreparability[length(Entered_Scores_Df$Score_Severity_Irreparability)]
   IEQ.Blame.Unfairness.Last.Score<- Entered_Scores_Df$Score_Blame_Unfairness[length(Entered_Scores_Df$Score_Blame_Unfairness)]

    Analytics_Df<<- data.frame(IEQ.Fullscale.First.Date,IEQ.Fullscale.First.Score,IEQ.Fullscale.Comparisons,IEQ.Fullscale.Change,IEQ.Fullscale.Last.Date,IEQ.Fullscale.Last.Score,IEQ.Fullscale.Improvement,IEQ.Fullscale.Sig.Improvement,IEQ.Fullscale.Deterioration,IEQ.Fullscale.Sig.Deterioration,
                              IEQ.Severity.Irreparability.First.Date,IEQ.Severity.Irreparability.First.Score,IEQ.Severity.Irreparability.Comparisons,IEQ.Severity.Irreparability.Change,IEQ.Severity.Irreparability.Last.Date,IEQ.Severity.Irreparability.Last.Score,IEQ.Severity.Irreparability.Improvement,IEQ.Severity.Irreparability.Sig.Improvement,IEQ.Severity.Irreparability.Deterioration,IEQ.Severity.Irreparability.Sig.Deterioration,
                              IEQ.Blame.Unfairness.First.Date,IEQ.Blame.Unfairness.First.Score,IEQ.Blame.Unfairness.Comparisons,IEQ.Blame.Unfairness.Change,IEQ.Blame.Unfairness.Last.Date,IEQ.Blame.Unfairness.Last.Score,IEQ.Blame.Unfairness.Improvement,IEQ.Blame.Unfairness.Sig.Improvement,IEQ.Blame.Unfairness.Deterioration,IEQ.Blame.Unfairness.Sig.Deterioration
                             )
    
    
    
  })
  
  
  
  #Create an expression to activate the entered data & create notifications to indicate inaccurate data entry
  
  Entered_Scores_Notifications<- observeEvent(input$Action_Submit_Data, {
    
    Entered_Scores_Reac()
    
    if(length(Score_1a) < 12) {
      showNotification("The IEQ is a 12-item scale. You have entered less than 12 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(length(Score_1a) > 12) {
      showNotification("The IEQ is a 12-item scale. You have entered more than 12 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(input$Timepoint != "1") {
      if(length(Score_2a) < 12) {
        showNotification("The IEQ is a 12-item scale. You have entered less than 12 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_2a) > 12) {
        showNotification("The IEQ is a 12-item scale. You have entered more than 12 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
    if(input$Timepoint == "3") {
      if(length(Score_3a) < 12) {
        showNotification("The IEQ is a 12-item scale. You have entered less than 12 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_3a) > 12) {
        showNotification("The IEQ is a 12-item scale. You have entered more than 12 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
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
    
    Gap_Severity_Irreparability<- Entered_Scores_Df[1,13] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),13]
    Entered_Scores_Df[1,14]<- Gap_Severity_Irreparability
    
    Gap_Blame_Unfairness<- Entered_Scores_Df[1,24] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),24]
    Entered_Scores_Df[1,25]<- Gap_Blame_Unfairness

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
    
    filename = paste0(" IEQ Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
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
        Stats_Table_Severity_Irreparability = Stats_Table_Severity_Irreparability,
        Stats_Table_Blame_Unfairness = Stats_Table_Blame_Unfairness,
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
      paste(paste0(" IEQ Data ", format(Sys.time(), '%d/%m/%y')), input$Output_Filetype1, sep = ".")
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













