
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
      menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "DASS-21"),
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
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| Depression Anxiety Stress Scales - 21 (DASS-21)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 800),
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
                
                "Asghari, A., Saed, F., & Dibajnia, P. (2008). Psychometric properties of the depression anxiety stress scales-21 (DASS-21) in a non-clinical iranian sample. Int J Psychol, 2(2), 82-102.", br(), br(), 
                "Crawford, J. R., Garthwaite, P. H., Lawrie, C. J., Henry, J. D., MacDonald, M. A., Sutherland, J., & Sinha, P. (2009). A convenient method of obtaining percentile norms and accompanying interval estimates for self‐report mood scales (DASS, DASS‐21, HADS, PANAS, and sAD). British Journal of Clinical Psychology, 48(2), 163-180.", br(), br(),  
                "Crawford, J. R., & Henry, J. D. (2003). The depression anxiety stress scales (DASS): Normative data and latent structure in a large non‐clinical sample. British Journal of Clinical Psychology, 42(2), 111-131.", br(), br(),  
                "Crawford, J., Cayley, C., Lovibond, P. F., Wilson, P. H., & Hartley, C. (2011). Percentile norms and accompanying interval estimates from an australian general adult population sample for self‐report mood scales (BAI, BDI, CRSD, CES‐D, DASS, DASS‐21, STAI‐X, STAI‐Y, SRDS, and SRAS). Australian Psychologist, 46(1), 3-14.", br(), br(),  
                "Henry, J. D., & Crawford, J. R. (2005). The short‐form version of the depression anxiety stress scales (DASS‐21): Construct validity and normative data in a large non‐clinical sample. British Journal of Clinical Psychology, 44(2), 227-239.", br(), br(),  
                "Norton, P. J. (2007). Depression anxiety and stress scales (DASS-21): Psychometric analysis across four racial groups. Anxiety, Stress, and Coping, 20(3), 253-265.", br(), br(),  
                "Osman, A., Wong, J. L., Bagge, C. L., Freedenthal, S., Gutierrez, P. M., & Lozano, G. (2012). The depression anxiety stress Scales—21 (DASS‐21): Further examination of dimensions, scale reliability, and correlates. Journal of Clinical Psychology, 68(12), 1322-1338.", br(), br(),  
                "Page, A. C., Hooke, G. R., & Morrison, D. L. (2007). Psychometric properties of the depression anxiety stress scales (DASS) in depressed clinical samples. British Journal of Clinical Psychology, 46(3), 283-297.", br(), br(),
                "Ronk, F. R., Korman, J. R., Hooke, G. R., & Page, A. C. (2013). Assessing clinical significance of treatment outcomes using the DASS-21. Psychological Assessment, Journal of Clinical Psychology, 25(4), 1103.", br(), br(), 
                "Sinclair, S. J., Siefert, C. J., Slavin-Mulford, J. M., Stein, M. B., Renna, M., & Blais, M. A. (2012). Psychometric evaluation and normative data for the depression, anxiety, and stress scales-21 (DASS-21) in a nonclinical sample of US adults. Evaluation & the Health Professions, 35(3), 259-279.", br(), br(),  
                "Szabó, M. (2010). The short version of the depression anxiety stress scales (DASS-21): Factor structure in a young adolescent sample. Journal of Adolescence, 33(1), 1-8." 
                
               
        ),
        
        
        
        tabItem(tabName = "DASS-21",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel("Scale",
                             wellPanel(style = "background-color: #ffffff; color: black",
                                       fluidRow(
                                         column(width = 12, h3(tags$strong("DASS-21")))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 12,
                                                div(h4("Please read each statement and circle a number 0, 1, 2 or 3 which indicates how much the statement applied to you", tags$strong("over the past week."), "There are no right or wrong answers. Do not spend too much time on any statement.")), br(),
                                                
                                                  tags$ol(
                                                    
                                                    tags$li(h4("The rating scale is as follows:")),
                                                    tags$li(h4("Did not apply to me at all - NEVER")), 
                                                    tags$li(h4("Applied to me to some degree, or some of the time - SOMETIMES")),
                                                    tags$li(h4("Applied to me to a considerable degree, or a good part of time - OFTEN")),
                                                    tags$li(h4("Applied to me very much, or most of the time - ALMOST ALWAYS")) 
                                                  )
                                                )
                                                ),

                                       fluidRow(
                                         column(width = 7, h4(tags$strong(""))),
                                         column(width = 1, h4(tags$strong("N"))),
                                         column(width = 1, h4(tags$strong("S"))),
                                         column(width = 1, h4(tags$strong("O"))),
                                         column(width = 1, h4(tags$strong("AA")))
                                       ),
                                       
                                       
                                       fluidRow(
                                         column(width = 7, h4("1. I found it hard to wind down")),
                                         column(width = 5, radioButtons("Item_1", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("2. I was aware of dryness of my mouth")),
                                         column(width = 5, radioButtons("Item_2", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("3. I couldn’t seem to experience any positive feeling at all")),
                                         column(width = 5, radioButtons("Item_3", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("4. I experienced breathing difficulty (eg, excessively rapid breathing, breathlessness in the absence of physical exertion)")),
                                         column(width = 5, radioButtons("Item_4", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("5. I found it difficult to work up the initiative to do things")),
                                         column(width = 5, radioButtons("Item_5", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("6. I tended to over-react to situations")),
                                         column(width = 5, radioButtons("Item_6", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("7. I experienced trembling (eg, in the hands)")),
                                         column(width = 5, radioButtons("Item_7", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("8. I felt that I was using a lot of nervous energy")),
                                         column(width = 5, radioButtons("Item_8", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("9. I was worried about situations in which I might panic and make a fool of myself")),
                                         column(width = 5, radioButtons("Item_9", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("10. I felt that I had nothing to look forward to")),
                                         column(width = 5, radioButtons("Item_10", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("11. I found myself getting agitated")),
                                         column(width = 5, radioButtons("Item_11", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("12. I found it difficult to relax")),
                                         column(width = 5, radioButtons("Item_12", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("13. I felt down-hearted and blue")),
                                         column(width = 5, radioButtons("Item_13", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("14. I was intolerant of anything that kept me from getting on with what I was doing")),
                                         column(width = 5, radioButtons("Item_14", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("15. I felt I was close to panic")),
                                         column(width = 5, radioButtons("Item_15", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("16. I was unable to become enthusiastic about anything")),
                                         column(width = 5, radioButtons("Item_16", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("17. I felt I wasn’t worth much as a person")),
                                         column(width = 5, radioButtons("Item_17", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("18. I felt that I was rather touchy")),
                                         column(width = 5, radioButtons("Item_18", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("19. I was aware of the action of my heart in the absence of physical exertion (eg, sense of heart rate increase, heart missing a beat)")),
                                         column(width = 5, radioButtons("Item_19", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("20. I felt scared without any good reason")),
                                         column(width = 5, radioButtons("Item_20", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       fluidRow(
                                         column(width = 7, h4("21. I felt that life was meaningless")),
                                         column(width = 5, radioButtons("Item_21", label = NULL, choices = c("0", "1", "2", "3"), inline = TRUE, selected = character(0)))
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(width = 4, dateInput("Q_Date", "Date",  format = "dd/mm/yyyy")),
                                         column(width = 4, textInput("Q_Name", "Name")),
                                         column(width = 4, textInput("Q_Clin_Name", "Clinician's Name"))
                                       ),
                                       fluidRow(
                                         column(width = 12, h5("Scale Source: Lovibond, S. H., & Lovibond, P. F. (1995). Manual for the depression anxiety stress scales. Sydney: Psychology Foundation Monograph."))
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
                                                      selectInput("Pop", "", choices = c("General Population"))
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
                                                               selectInput("Select_CI", label = "DASS-21 total scale",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI == '2'",
                                                                                numericInput("Man_CI", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Depression", label = "Depression",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Depression == '2'",
                                                                                numericInput("Man_CI_Depression", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Anxiety", label = "Anxiety",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Anxiety == '2'",
                                                                                numericInput("Man_CI_Anxiety", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Stress", label = "Stress",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Stress == '2'",
                                                                                numericInput("Man_CI_Stress", "Specify the width of the confidence interval", value = 0))
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
                                                               uiOutput("Mean_Widg_Depression")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Anxiety")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Stress")
                                                        )
                                                      ),
                                                      
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Chelune et al. (1993)' || input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)' || input.RelChangeMethod == 'Speer (1992)'",
                                                                       
                                                                       h4(tags$strong("Enter a mean value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean", "DASS-21 total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Depression", "Depression", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Anxiety", "Anxiety", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Stress", "Stress", value = 0)
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
                                                               uiOutput("Sd_Widg_Depression")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Anxiety")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Stress")
                                                        )
                                                      ),
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)'",
                                                                       
                                                                       h4(tags$strong("Enter a standard deviation value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd", "DASS-21 total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Depression", "Depression", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Anxiety", "Anxiety", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Stress", "Stress", value = 0)
                                                                         )
                                                                       )
                                                                       
                                                      )
                                             ),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      h4(tags$strong("Enter test-retest reliability values")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability", "*DASS-21 total scale", value = .94),
                                                               h6("Reference: Crawford, Garthwaite, Lawrie, Henry, MacDonald, Sutherland & Sinha (2009)"),
                                                               h6("*Value is coefficient alpha not test-retest reliability.")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Depression", "Depression", value = .77),
                                                               h6("Reference: Asghari, Saed & Dibajnia (2008)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Anxiety", "Anxiety", value = .89),
                                                               h6("Reference: Asghari, Saed & Dibajnia (2008)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Stress", "Stress", value = .85),
                                                               h6("Reference: Asghari, Saed & Dibajnia (2008)")
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
                                                      radioButtons("Dev_Cutoffs", "", choices = c("Use severity ratings recommended by the scale developers" = "1", "Use percentiles for the general population (Henry & Crawford, 2005)" = "2"), width = '100%'),
                                                      hr(),
                                                      h4(tags$strong("First cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Depression_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Anxiety_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Stress_1") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Second cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Depression_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Anxiety_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Stress_2") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Third cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Depression_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Anxiety_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Stress_3") 
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
                                                               uiOutput("Cutoff_Widg_Depression_4") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Anxiety_4") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Stress_4") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Fifth cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_5") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Depression_5") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Anxiety_5") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Stress_5") 
                                                        )
                                                        
                                                      )
                                             ),
                                             
                                             
                                             h4(tags$strong("Psychometric Properties of the DASS-21 Relevant to Assessing Reliable & Clinically Significant Change")),
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
  
  #Questionnaire responses
  
  observe({
    
    Q_Scores <- paste(input$Item_1, input$Item_2, input$Item_3, input$Item_4, input$Item_5, input$Item_6, input$Item_7, 
                      input$Item_8, input$Item_9, input$Item_10, input$Item_11, input$Item_12, input$Item_13, input$Item_14, input$Item_15, input$Item_16, 
                      input$Item_17, input$Item_18, input$Item_19, input$Item_20, input$Item_21, sep = ",")
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
      Mean_Val<<- 10.59
      Sd_Val<<-10.61
      Source_Mean<<- "Crawford, Garthwaite, Lawrie, Henry, MacDonald, Sutherland & Sinha (2009)"
      Source_Sd<<- "Crawford, Garthwaite, Lawrie, Henry, MacDonald, Sutherland & Sinha (2009)"
        Cut_Val_1<<- 7
        Cut_Val_2<<- 10
        Cut_Val_3<<- 13
        Cut_Val_4<<- 18
        Cut_Val_5<<- 28
        Cut_Lab_1<<- "51st Percentile (Henry & Crawford, 2005)"
        Cut_Lab_2<<- "65th Percentile (Henry & Crawford, 2005)"
        Cut_Lab_3<<- "76th Pecentile (Henry & Crawford, 2005)"
        Cut_Lab_4<<- "87th Percentile (Henry & Crawford, 2005)"
        Cut_Lab_5<<- "95th Pecentile (Henry & Crawford, 2005)"
        Source_Cutoff<<- "Henry & Crawford (2005)"
        Mean_Val_Depression<<- 3.18
        Sd_Val_Depression<<- 4.16
        Source_Mean_Depression<<- "Crawford, Garthwaite, Lawrie, Henry, MacDonald, Sutherland & Sinha (2009)"
        Source_Sd_Depression<<- "Crawford, Garthwaite, Lawrie, Henry, MacDonald, Sutherland & Sinha (2009)"
        if(input$Dev_Cutoffs == "2") {
        Cut_Val_Depression_1<<- 2
        Cut_Val_Depression_2<<- 4
        Cut_Val_Depression_3<<- 6
        Cut_Val_Depression_4<<- 8
        Cut_Val_Depression_5<<- 11
        Cut_Lab_1_Depression<<- "58th Percentile (Henry & Crawford, 2005)"
        Cut_Lab_2_Depression<<- "75th Percentile (Henry & Crawford, 2005)"
        Cut_Lab_3_Depression<<- "85th Percentile (Henry & Crawford, 2005)"
        Cut_Lab_4_Depression<<- "91st Percentile (Henry & Crawford, 2005)"
        Cut_Lab_5_Depression<<- "95th Percentile (Henry & Crawford, 2005)"
        Source_Cutoff_Depression<<- "Henry & Crawford (2005)"
        } else {
          Cut_Val_Depression_1<<- 0
          Cut_Val_Depression_2<<- 5
          Cut_Val_Depression_3<<- 7
          Cut_Val_Depression_4<<- 11
          Cut_Val_Depression_5<<- 14
          Cut_Lab_1_Depression<<- "Normal"
          Cut_Lab_2_Depression<<- "Mild"
          Cut_Lab_3_Depression<<- "Moderate"
          Cut_Lab_4_Depression<<- "Severe"
          Cut_Lab_5_Depression<<- "Extremely Severe"
          Source_Cutoff_Depression<<- "Lovibond & Lovibond (1995)"
        }
        Mean_Val_Anxiety<<- 2.25
        Sd_Val_Anxiety<<-3.34
        Source_Mean_Anxiety<<- "Crawford, Garthwaite, Lawrie, Henry, MacDonald, Sutherland & Sinha (2009)"
        Source_Sd_Anxiety<<- "Crawford, Garthwaite, Lawrie, Henry, MacDonald, Sutherland & Sinha (2009)"
        if(input$Dev_Cutoffs == "2") {
        Cut_Val_Anxiety_1<<-1
        Cut_Val_Anxiety_2<<- 2
        Cut_Val_Anxiety_3<<- 3
        Cut_Val_Anxiety_5<<- 5
        Cut_Val_Anxiety_8<<- 8
        Cut_Lab_1_Anxiety<<- "54th Percentile (Henry & Crawford, 2005)"
        Cut_Lab_2_Anxiety<<- "69th Percentile (Henry & Crawford, 2005)"
        Cut_Lab_3_Anxiety<<- "79th Percentile (Henry & Crawford, 2005)"
        Cut_Lab_4_Anxiety<<- "89th Percentile (Henry & Crawford, 2005)"
        Cut_Lab_5_Anxiety<<- "95th Percentile (Henry & Crawford, 2005)"
        Source_Cutoff_Anxiety<<- "Henry & Crawford (2005)"
        } else {
          Cut_Val_Anxiety_1<<- 0
          Cut_Val_Anxiety_2<<- 4
          Cut_Val_Anxiety_3<<- 6
          Cut_Val_Anxiety_4<<- 8
          Cut_Val_Anxiety_5<<- 10
          Cut_Lab_1_Anxiety<<- "Normal"
          Cut_Lab_2_Anxiety<<- "Mild"
          Cut_Lab_3_Anxiety<<- "Moderate"
          Cut_Lab_4_Anxiety<<- "Severe"
          Cut_Lab_5_Anxiety<<- "Extremely Severe"
          Source_Cutoff_Anxiety<<- "Lovibond & Lovibond (2005)"
        }
        Mean_Val_Stress<<- 5.16
        Sd_Val_Stress<<-4.44
        Source_Mean_Stress<<- "Crawford, Garthwaite, Lawrie, Henry, MacDonald, Sutherland & Sinha (2009)"
        Source_Sd_Stress<<- "Crawford, Garthwaite, Lawrie, Henry, MacDonald, Sutherland & Sinha (2009)"
        if(input$Dev_Cutoffs == "2") {
        Cut_Val_Stress_1<<-  4
        Cut_Val_Stress_2<<- 5
        Cut_Val_Stress_3<<- 7
        Cut_Val_Stress_4<<- 9
        Cut_Val_Stress_5<<- 13
        Cut_Lab_1_Stress<<- "51st Percentile (Henry & Crawford, 2005)"
        Cut_Lab_2_Stress<<- "60th Percentile (Henry & Crawford, 2005)"
        Cut_Lab_3_Stress<<- "77th Percentile (Henry & Crawford, 2005)"
        Cut_Lab_4_Stress<<- "86th Percentile (Henry & Crawford, 2005)"
        Cut_Lab_5_Stress<<- "95th Percentile (Henry & Crawford, 2005)"
        Source_Cutoff_Stress<<- "Henry & Crawford (2005)"
        } else {
          Cut_Val_Stress_1<<- 0 
          Cut_Val_Stress_2<<- 8
          Cut_Val_Stress_3<<- 10
          Cut_Val_Stress_4<<- 13
          Cut_Val_Stress_5<<- 17
          Cut_Lab_1_Stress<<- "Normal"
          Cut_Lab_2_Stress<<- "Mild"
          Cut_Lab_3_Stress<<- "Moderate"
          Cut_Lab_4_Stress<<- "Severe"
          Cut_Lab_5_Stress<<- "Extremely Severe"
          Source_Cutoff_Stress<<- "Lovibond & Lovibond (2005)"
        }
    } 
    
  })
  
  
  output$Mean_Widg<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean", "DASS-21 total scale", Mean_Val),
      h6(paste("Reference:", Source_Mean))
          )
  })
  outputOptions(output, "Mean_Widg", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Depression<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Depression", "Depression", Mean_Val_Depression),
      h6(paste("Reference:", Source_Mean_Depression))
          )
  })
  outputOptions(output, "Mean_Widg_Depression", suspendWhenHidden = FALSE)
  

  output$Mean_Widg_Anxiety<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Anxiety", "Anxiety", Mean_Val_Anxiety),
      h6(paste("Reference:", Source_Mean_Anxiety))
          )
  })
  outputOptions(output, "Mean_Widg_Anxiety", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Stress<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Stress", "Stress", Mean_Val_Stress),
      h6(paste("Reference:", Source_Mean_Stress))
          )
  })
  outputOptions(output, "Mean_Widg_Stress", suspendWhenHidden = FALSE) 
  
  
  
  
  output$Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd", "DASS-21 total scale", Sd_Val),
      h6(paste("Reference:", Source_Sd))
           )
  })
  outputOptions(output, "Sd_Widg", suspendWhenHidden = FALSE)
  

  output$Sd_Widg_Depression<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Depression", "Depression", Sd_Val_Depression),
      h6(paste("Reference:", Source_Sd_Depression))
          )
  })
  outputOptions(output, "Sd_Widg_Depression", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Anxiety<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Anxiety", "Anxiety", Sd_Val_Anxiety),
      h6(paste("Reference:", Source_Sd_Anxiety))
          )
  })
  outputOptions(output, "Sd_Widg_Anxiety", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Stress<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Stress", "Stress", Sd_Val_Stress),
      h6(paste("Reference:", Source_Sd_Stress))
          )
  })
  outputOptions(output, "Sd_Widg_Stress", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_1", "DASS-21 total scale", as.numeric(Cut_Val_1)),
      textInput("Cutoff_Text_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source_Cutoff))
          )
  })
  outputOptions(output, "Cutoff_Widg_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Depression_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Depression_1", "Depression", as.numeric(Cut_Val_Depression_1)),
      textInput("Cutoff_Text_Depression_1", "Cut-Off Score Name", Cut_Lab_1_Depression),
      h6(paste("Reference:", Source_Cutoff_Depression))
          )
  })
  outputOptions(output, "Cutoff_Widg_Depression_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Anxiety_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Anxiety_1", "Anxiety", as.numeric(Cut_Val_Anxiety_1)),
      textInput("Cutoff_Text_Anxiety_1", "Cut-Off Score Name", Cut_Lab_1_Anxiety),
      h6(paste("Reference:", Source_Cutoff_Anxiety))
           )
  })
  outputOptions(output, "Cutoff_Widg_Anxiety_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Stress_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Stress_1", "Stress", as.numeric(Cut_Val_Stress_1)),
      textInput("Cutoff_Text_Stress_1", "Cut-Off Score Name", Cut_Lab_1_Stress),
      h6(paste("Reference:", Source_Cutoff_Stress))
          )
  })
  outputOptions(output, "Cutoff_Widg_Stress_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_2", "DASS-21 total scale", as.numeric(Cut_Val_2)),
      textInput("Cutoff_Text_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source_Cutoff))
          )
  })
  outputOptions(output, "Cutoff_Widg_2", suspendWhenHidden = FALSE)
  

  output$Cutoff_Widg_Depression_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Depression_2", "Depression", as.numeric(Cut_Val_Depression_2)),
      textInput("Cutoff_Text_Depression_2", "Cut-Off Score Name", Cut_Lab_2_Depression),
      h6(paste("Reference:", Source_Cutoff_Depression))
           )
  })
  outputOptions(output, "Cutoff_Widg_Depression_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Anxiety_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Anxiety_2", "Anxiety", as.numeric(Cut_Val_Anxiety_2)),
      textInput("Cutoff_Text_Anxiety_2", "Cut-Off Score Name", Cut_Lab_2_Anxiety),
      h6(paste("Reference:", Source_Cutoff_Anxiety))
    )
  })
  outputOptions(output, "Cutoff_Widg_Anxiety_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Stress_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Stress_2", "Stress", as.numeric(Cut_Val_Stress_2)),
      textInput("Cutoff_Text_Stress_2", "Cut-Off Score Name", Cut_Lab_2_Stress),
      h6(paste("Reference:", Source_Cutoff_Stress))
          )
  })
  outputOptions(output, "Cutoff_Widg_Stress_2", suspendWhenHidden = FALSE)
  

  output$Cutoff_Widg_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_3", "DASS-21 total scale", as.numeric(Cut_Val_3)),
      textInput("Cutoff_Text_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source_Cutoff))
           )
          
  })
  outputOptions(output, "Cutoff_Widg_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Depression_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Depression_3", "Depression", as.numeric(Cut_Val_Depression_3)),
      textInput("Cutoff_Text_Depression_3", "Cut-Off Score Name", Cut_Lab_3_Depression),
      h6(paste("Reference:", Source_Cutoff_Depression))
          )
  })
  outputOptions(output, "Cutoff_Widg_Depression_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Anxiety_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Anxiety_3", "Anxiety", as.numeric(Cut_Val_Anxiety_3)),
      textInput("Cutoff_Text_Anxiety_3", "Cut-Off Score Name", Cut_Lab_3_Anxiety),
      h6(paste("Reference:", Source_Cutoff_Anxiety))
           )
  })
  outputOptions(output, "Cutoff_Widg_Anxiety_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Stress_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Stress_3", "Stress", as.numeric(Cut_Val_Stress_3)),
      textInput("Cutoff_Text_Stress_3", "Cut-Off Score Name", Cut_Lab_3_Stress),
      h6(paste("Reference:", Source_Cutoff_Stress))
          )
  })
  outputOptions(output, "Cutoff_Widg_Stress_3", suspendWhenHidden = FALSE)
  
  
  
  output$Cutoff_Widg_4<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_4", "DASS-21 total scale", as.numeric(Cut_Val_4)),
      textInput("Cutoff_Text_4", "Cut-Off Score Name", Cut_Lab_4),
      h6(paste("Reference:", Source_Cutoff))
    )
  })
  outputOptions(output, "Cutoff_Widg_4", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Depression_4<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Depression_4", "Depression", as.numeric(Cut_Val_Depression_4)),
      textInput("Cutoff_Text_Depression_4", "Cut-Off Score Name", Cut_Lab_4_Depression),
      h6(paste("Reference:", Source_Cutoff_Depression))
    )
  })
  outputOptions(output, "Cutoff_Widg_Depression_4", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Anxiety_4<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Anxiety_4", "Anxiety", as.numeric(Cut_Val_Anxiety_4)),
      textInput("Cutoff_Text_Anxiety_4", "Cut-Off Score Name", Cut_Lab_4_Anxiety),
      h6(paste("Reference:", Source_Cutoff_Anxiety))
    )
  })
  outputOptions(output, "Cutoff_Widg_Anxiety_4", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Stress_4<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Stress_4", "Stress", as.numeric(Cut_Val_Stress_4)),
      textInput("Cutoff_Text_Stress_4", "Cut-Off Score Name", Cut_Lab_4_Stress),
      h6(paste("Reference:", Source_Cutoff_Stress))
    )
  })
  outputOptions(output, "Cutoff_Widg_Stress_4", suspendWhenHidden = FALSE)
  
  
  
  output$Cutoff_Widg_5<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_5", "DASS-21 total scale", as.numeric(Cut_Val_5)),
      textInput("Cutoff_Text_5", "Cut-Off Score Name", Cut_Lab_5),
      h6(paste("Reference:", Source_Cutoff))
    )
  })
  outputOptions(output, "Cutoff_Widg_5", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Depression_5<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Depression_5", "Depression", as.numeric(Cut_Val_Depression_5)),
      textInput("Cutoff_Text_Depression_5", "Cut-Off Score Name", Cut_Lab_5_Depression),
      h6(paste("Reference:", Source_Cutoff_Depression))
    )
  })
  outputOptions(output, "Cutoff_Widg_Depression_5", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Anxiety_5<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Anxiety_5", "Anxiety", as.numeric(Cut_Val_Anxiety_5)),
      textInput("Cutoff_Text_Anxiety_5", "Cut-Off Score Name", Cut_Lab_5_Anxiety),
      h6(paste("Reference:", Source_Cutoff_Anxiety))
    )
  })
  outputOptions(output, "Cutoff_Widg_Anxiety_5", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Stress_5<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Stress_5", "Stress", as.numeric(Cut_Val_Stress_5)),
      textInput("Cutoff_Text_Stress_5", "Cut-Off Score Name", Cut_Lab_5_Stress),
      h6(paste("Reference:", Source_Cutoff_Stress))
    )
  })
  outputOptions(output, "Cutoff_Widg_Stress_5", suspendWhenHidden = FALSE)
  
  
  
  
  
  
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
    M_Depression<- input$Pop_Mean_Depression
    SD_Depression<-input$Pop_Sd_Depression
    M_Anxiety<- input$Pop_Mean_Anxiety
    SD_Anxiety<- input$Pop_Sd_Anxiety
    M_Stress<- input$Pop_Mean_Stress
    SD_Stress<- input$Pop_Sd_Stress
    
    M_Retest<- input$Retest_Mean
    SD_Retest<- input$Retest_Sd
    M_Retest_Depression<- input$Retest_Mean_Depression
    SD_Retest_Depression<- input$Retest_Sd_Depression
    M_Retest_Anxiety<- input$Retest_Mean_Anxiety
    SD_Retest_Anxiety<- input$Retest_Sd_Anxiety
    M_Retest_Stress<- input$Retest_Mean_Stress
    SD_Retest_Stress<- input$Retest_Sd_Stress
   
    
    SampleN<- input$SampleN
    
    Rel<- input$Reliability
    Rel_Depression<- input$Reliability_Depression
    Rel_Anxiety<- input$Reliability_Anxiety
    Rel_Stress<- input$Reliability_Stress
    
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
      SE_Depression<-SD_Depression * sqrt(1 - Rel_Depression^2)
      SE_Anxiety<-SD_Anxiety * sqrt(1 - Rel_Anxiety^2)
      SE_Stress<-SD_Stress * sqrt(1 - Rel_Stress^2)
      
      SE<- round(SE, digits = 2)
      SE_Depression<- round(SE_Depression, digits = 2)
      SE_Anxiety<- round(SE_Anxiety, digits = 2)
      SE_Stress<- round(SE_Stress, digits = 2)
     
    } else if (input$RelChangeMethod == "Chelune et al. (1993)" | input$RelChangeMethod == "Jacobson & Truax (1991)" | input$RelChangeMethod == "Speer (1992)") {
      SE<- sqrt((2*(SD^2))*(1-Rel))
      SE_Depression<- sqrt((2*(SD_Depression^2))*(1-Rel_Depression))
      SE_Anxiety<- sqrt((2*(SD_Anxiety^2))*(1-Rel_Anxiety))
      SE_Stress<- sqrt((2*(SD_Stress^2))*(1-Rel_Stress))
     
      SE<- round(SE, digits = 2)
      SE_Depression<- round(SE_Depression, digits = 2)
      SE_Anxiety<- round(SE_Anxiety, digits = 2)
      SE_Stress<- round(SE_Stress, digits = 2)
    
    } else if (input$RelChangeMethod == "Maassen et al. (2006)") {
      SE<- sqrt((SD^2 + SD_Retest^2)*(1-Rel))
      SE_Depression<- sqrt((SD_Depression^2 + SD_Retest_Depression^2)*(1-Rel_Depression))
      SE_Anxiety<- sqrt((SD_Anxiety^2 + SD_Retest_Anxiety^2)*(1-Rel_Anxiety))
      SE_Stress<- sqrt((SD_Stress^2 + SD_Retest_Stress^2)*(1-Rel_Stress))
      
      SE<- round(SE, digits = 2)
      SE_Depression<- round(SE_Depression, digits = 2)
      SE_Anxiety<- round(SE_Anxiety, digits = 2)
      SE_Stress<- round(SE_Stress, digits = 2)
      
    } else if (input$RelChangeMethod == "McSweeny et al. (1993)") {
      SE<- SD_Retest*sqrt(1 - Rel^2)
      SE_Depression<- SD_Retest_Depression*sqrt(1 - Rel_Depression^2)
      SE_Anxiety<- SD_Retest_Anxiety*sqrt(1 - Rel_Anxiety^2)
      SE_Stress<- SD_Retest_Stress*sqrt(1 - Rel_Stress^2)
     
      SE<- round(SE, digits = 2)
      SE_Depression<- round(SE_Depression, digits = 2)
      SE_Anxiety<- round(SE_Anxiety, digits = 2)
      SE_Stress<- round(SE_Stress, digits = 2)
     
    }
    
    
    #Caclculate McSweeny standard error to be employed in deriving the Crawford & Howell standard error if selected
    
    McSweeny_SE<- SD_Retest*sqrt(1 - Rel^2)
    McSweeny_SE_Depression<- SD_Retest_Depression*sqrt(1 - Rel_Depression^2)
    McSweeny_SE_Anxiety<- SD_Retest_Anxiety*sqrt(1 - Rel_Anxiety^2)
    McSweeny_SE_Stress<- SD_Retest_Stress*sqrt(1 - Rel_Stress^2)
   
    
    #Generate object storing names of cut-off scores
    
    Cutoff_Name_1<- input$Cutoff_Text_1
    Cutoff_Name_2<- input$Cutoff_Text_2
    Cutoff_Name_3<- input$Cutoff_Text_3
    Cutoff_Name_4<- input$Cutoff_Text_4
    Cutoff_Name_5<- input$Cutoff_Text_5
    Cutoff_Name_Depression_1<- input$Cutoff_Text_Depression_1
    Cutoff_Name_Depression_2<- input$Cutoff_Text_Depression_2
    Cutoff_Name_Depression_3<- input$Cutoff_Text_Depression_3
    Cutoff_Name_Depression_4<- input$Cutoff_Text_Depression_4
    Cutoff_Name_Depression_5<- input$Cutoff_Text_Depression_5
    Cutoff_Name_Anxiety_1<- input$Cutoff_Text_Anxiety_1
    Cutoff_Name_Anxiety_2<- input$Cutoff_Text_Anxiety_2
    Cutoff_Name_Anxiety_3<- input$Cutoff_Text_Anxiety_3
    Cutoff_Name_Anxiety_4<- input$Cutoff_Text_Anxiety_4
    Cutoff_Name_Anxiety_5<- input$Cutoff_Text_Anxiety_5
    Cutoff_Name_Stress_1<- input$Cutoff_Text_Stress_1
    Cutoff_Name_Stress_2<- input$Cutoff_Text_Stress_2
    Cutoff_Name_Stress_3<- input$Cutoff_Text_Stress_3
    Cutoff_Name_Stress_4<- input$Cutoff_Text_Stress_4
    Cutoff_Name_Stress_5<- input$Cutoff_Text_Stress_5
    
    Cutoff_Names<<- data.frame(Cutoff_Name_1,Cutoff_Name_2,Cutoff_Name_3,Cutoff_Name_4,Cutoff_Name_5,
                               Cutoff_Name_Depression_1,Cutoff_Name_Depression_2,Cutoff_Name_Depression_3, Cutoff_Name_Depression_4,Cutoff_Name_Depression_5,
                               Cutoff_Name_Anxiety_1, Cutoff_Name_Anxiety_2, Cutoff_Name_Anxiety_3,Cutoff_Name_Anxiety_4,Cutoff_Name_Anxiety_5, 
                               Cutoff_Name_Stress_1, Cutoff_Name_Stress_2, Cutoff_Name_Stress_3,Cutoff_Name_Stress_4,Cutoff_Name_Stress_5)
    
    #Define the data & calculate PS, SE, confidence intervals and cut-off scores
    
    if(input$Timepoint == "1") {
      Date<- input$Date_1
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_1a), Score = Score_1a)
      Score<- sum(Score_1a, na.rm = TRUE)
      Score<- round(Score, digits = 2)
      Score_Depression<- sum(Score_1a[c(3,5,10,13,16,17,21)], na.rm = TRUE)
      Score_Anxiety<- sum(Score_1a[c(2,4,7,9,15,19,20)], na.rm = TRUE)
      Score_Stress<- sum(Score_1a[c(1,6,8,11,12,14,18)], na.rm = TRUE)
      
      Change<- 0
      Change<- round(Change, digits = 2)
      Change_Depression<- 0
      Change_Anxiety<- 0
      Change_Stress<- 0
    
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS<- (Rel * Score) + (M * (1 - Rel))
        PTS_Depression<- (Rel_Depression * Score_Depression) + (M_Depression * (1 - Rel_Depression))
        PTS_Anxiety<- (Rel_Anxiety * Score_Anxiety) + (M_Anxiety * (1 - Rel_Anxiety))
        PTS_Stress<- (Rel_Stress * Score_Stress) + (M_Stress * (1 - Rel_Stress))
       
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS<- Score + (M_Retest - M)  
        PTS_Depression<- Score_Depression + (M_Retest_Depression - M_Depression)  
        PTS_Anxiety<- Score_Anxiety + (M_Retest_Anxiety - M_Anxiety)  
        PTS_Stress<- Score_Stress + (M_Retest_Stress - M_Stress) 
        
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Depression<- Score_Depression
        PTS_Anxiety<- Score_Anxiety
        PTS_Stress<- Score_Stress
       
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS<- (B_Adj * Score) + A_Adj
        B_Slope_Depression<- Rel_Depression * (SD_Retest_Depression/SD_Depression)
        A_Constant_Depression<- M_Retest_Depression - (B_Slope_Depression * M_Depression)
        B_Adj_Depression<- SD_Retest_Depression/SD_Depression
        A_Adj_Depression<- M_Retest_Depression - (B_Adj_Depression * M_Depression)
        PTS_Depression<- (B_Adj_Depression * Score_Depression) + A_Adj_Depression
        B_Slope_Anxiety<- Rel_Anxiety * (SD_Retest_Anxiety/SD_Anxiety)
        A_Constant_Anxiety<- M_Retest_Anxiety - (B_Slope_Anxiety * M_Anxiety)
        B_Adj_Anxiety<- SD_Retest_Anxiety/SD_Anxiety
        A_Adj_Anxiety<- M_Retest_Anxiety - (B_Adj_Anxiety * M_Anxiety)
        PTS_Anxiety<- (B_Adj_Anxiety * Score_Anxiety) + A_Adj_Anxiety
        B_Slope_Stress<- Rel_Stress * (SD_Retest_Stress/SD_Stress)
        A_Constant_Stress<- M_Retest_Stress - (B_Slope_Stress * M_Stress)
        B_Adj_Stress<- SD_Retest_Stress/SD_Stress
        A_Adj_Stress<- M_Retest_Stress - (B_Adj_Stress * M_Stress)
        PTS_Stress<- (B_Adj_Stress * Score_Stress) + A_Adj_Stress
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS<- B_Slope * Score
        B_Slope_Depression<- Rel_Depression * (SD_Retest_Depression/SD_Depression)
        PTS_Depression<- B_Slope_Depression * Score_Depression
        B_Slope_Anxiety<- Rel_Anxiety * (SD_Retest_Anxiety/SD_Anxiety)
        PTS_Anxiety<- B_Slope_Anxiety * Score_Anxiety
        B_Slope_Stress<- Rel_Stress * (SD_Retest_Stress/SD_Stress)
        PTS_Stress<- B_Slope_Stress * Score_Stress
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS<- Score + (M_Retest - M)
        PTS_Depression<- Score_Depression + (M_Retest_Depression - M_Depression)
        PTS<- Anxiety<- Score_Anxiety + (M_Retest_Anxiety - M_Anxiety)
        PTS_Stress<- Score_Stress + (M_Retest_Stress - M_Stress)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Depression<- round(PTS_Depression, digits = 2)
      PTS_Anxiety<- round(PTS_Anxiety, digits = 2)
      PTS_Stress<- round(PTS_Stress, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score - M)^2/(SD^2*(SampleN-1))))
        SE_Depression<- McSweeny_SE_Depression*sqrt(1 + (1/SampleN) + ((Score_Depression - M_Depression)^2/(SD_Depression^2*(SampleN-1))))
        SE_Anxiety<- McSweeny_SE_Anxiety*sqrt(1 + (1/SampleN) + ((Score_Anxiety - M_Anxiety)^2/(SD_Anxiety^2*(SampleN-1))))
        SE_Stress<- McSweeny_SE_Stress*sqrt(1 + (1/SampleN) + ((Score_Stress - M_Stress)^2/(SD_Stress^2*(SampleN-1))))
        
        SE<- round(SE, digits = 2)
        SE_Depression<- round(SE_Depression, digits = 2)
        SE_Anxiety<- round(SE_Anxiety, digits = 2)
        SE_Stress<- round(SE_Stress, digits = 2)
    
        CI<- (Conf*SE)
        CI<- round(CI, digits = 2)
        CI_Depression<- (Conf*SE_Depression)
        CI_Depression<- round(CI_Depression, digits = 2)
        CI_Anxiety<- (Conf*SE_Anxiety)
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Stress<- (Conf*SE_Stress)
        CI_Stress<- round(CI_Stress, digits = 2)
       
      }
      CI<- (Conf*SE)
      CI<- round(CI, digits = 2)
      CI_Depression<- (Conf*SE_Depression)
      CI_Depression<- round(CI_Depression, digits = 2)
      CI_Anxiety<- (Conf*SE_Anxiety)
      CI_Anxiety<- round(CI_Anxiety, digits = 2)
      CI_Stress<- (Conf*SE_Stress)
      CI_Stress<- round(CI_Stress, digits = 2)
     
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Depression<- PTS_Depression + CI_Depression
      CI_Upper_Lim_Depression<- round(CI_Upper_Lim_Depression, digits = 2)
      CI_Lower_Lim_Depression<-PTS_Depression - CI_Depression
      CI_Lower_Lim_Depression<- round(CI_Lower_Lim_Depression, digits = 2)
      CI_Upper_Lim_Anxiety<- PTS_Anxiety + CI_Anxiety
      CI_Upper_Lim_Anxiety<- round(CI_Upper_Lim_Anxiety, digits = 2)
      CI_Lower_Lim_Anxiety<-PTS_Anxiety - CI_Anxiety
      CI_Lower_Lim_Anxiety<- round(CI_Lower_Lim_Anxiety, digits = 2)
      CI_Upper_Lim_Stress<- PTS_Stress + CI_Stress
      CI_Upper_Lim_Stress<- round(CI_Upper_Lim_Stress, digits = 2)
      CI_Lower_Lim_Stress<-PTS_Stress - CI_Stress
      CI_Lower_Lim_Stress<- round(CI_Lower_Lim_Stress, digits = 2)
     
      if(input$Select_CI == "2") {
        CI<- input$Man_CI
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Depression == "2") {
        CI_Depression<- input$Man_CI_Depression
        CI_Depression<- round(CI_Depression, digits = 2)
        CI_Upper_Lim_Depression<- Score_Depression + CI_Depression
        CI_Upper_Lim_Depression<- round(CI_Upper_Lim_Depression, digits = 2)
        CI_Lower_Lim_Depression<- Score_Depression - CI_Depression
        CI_Lower_Lim_Depression<- round(CI_Lower_Lim_Depression, digits = 2)
      }
      if(input$Select_CI_Anxiety == "2") {
        CI_Anxiety<- input$Man_CI_Anxiety
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Upper_Lim_Anxiety<- Score_Anxiety + CI_Anxiety
        CI_Upper_Lim_Anxiety<- round(CI_Upper_Lim_Anxiety, digits = 2)
        CI_Lower_Lim_Anxiety<- Score_Anxiety - CI_Anxiety
        CI_Lower_Lim_Anxiety<- round(CI_Lower_Lim_Anxiety, digits = 2)
      }
      if(input$Select_CI_Stress == "2") {
        CI_Stress<- input$Man_CI_Stress
        CI_Stress<- round(CI_Stress, digits = 2)
        CI_Upper_Lim_Stress<- Score_Stress + CI_Stress
        CI_Upper_Lim_Stress<- round(CI_Upper_Lim_Stress, digits = 2)
        CI_Lower_Lim_Stress<- Score_Stress - CI_Stress
        CI_Lower_Lim_Stress<- round(CI_Lower_Lim_Stress, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_4<- round(input$Cutoff_4, digits = 2)
      Cutoff_Score_5<- round(input$Cutoff_5, digits = 2)
      Cutoff_Score_Depression_1<- round(input$Cutoff_Depression_1, digits = 2)
      Cutoff_Score_Depression_2<- round(input$Cutoff_Depression_2, digits = 2)
      Cutoff_Score_Depression_3<- round(input$Cutoff_Depression_3, digits = 2)
      Cutoff_Score_Depression_4<- round(input$Cutoff_Depression_4, digits = 2)
      Cutoff_Score_Depression_5<- round(input$Cutoff_Depression_5, digits = 2)
      Cutoff_Score_Anxiety_1<- round(input$Cutoff_Anxiety_1, digits = 2)
      Cutoff_Score_Anxiety_2<- round(input$Cutoff_Anxiety_2, digits = 2)
      Cutoff_Score_Anxiety_3<- round(input$Cutoff_Anxiety_3, digits = 2)
      Cutoff_Score_Anxiety_4<- round(input$Cutoff_Anxiety_4, digits = 2)
      Cutoff_Score_Anxiety_5<- round(input$Cutoff_Anxiety_5, digits = 2)
      Cutoff_Score_Stress_1<- round(input$Cutoff_Stress_1, digits = 2)
      Cutoff_Score_Stress_2<- round(input$Cutoff_Stress_2, digits = 2)
      Cutoff_Score_Stress_3<- round(input$Cutoff_Stress_3, digits = 2)
      Cutoff_Score_Stress_4<- round(input$Cutoff_Stress_4, digits = 2)
      Cutoff_Score_Stress_5<- round(input$Cutoff_Stress_5, digits = 2)
     
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Cutoff_Score_4,Cutoff_Score_5,
                                      Score_Depression,Change_Depression,PTS_Depression, SE_Depression, CI_Upper_Lim_Depression, CI_Lower_Lim_Depression, Cutoff_Score_Depression_1,Cutoff_Score_Depression_2,Cutoff_Score_Depression_3,Cutoff_Score_Depression_4,Cutoff_Score_Depression_5,
                                      Score_Anxiety,Change_Anxiety, PTS_Anxiety, SE_Anxiety, CI_Upper_Lim_Anxiety, CI_Lower_Lim_Anxiety, Cutoff_Score_Anxiety_1,Cutoff_Score_Anxiety_2,Cutoff_Score_Anxiety_3,Cutoff_Score_Anxiety_4,Cutoff_Score_Anxiety_5, 
                                      Score_Stress,Change_Stress,PTS_Stress, SE_Stress, CI_Upper_Lim_Stress, CI_Lower_Lim_Stress, Cutoff_Score_Stress_1,Cutoff_Score_Stress_2,Cutoff_Score_Stress_3,Cutoff_Score_Stress_4,Cutoff_Score_Stress_5)
      
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
      Score_Depression_1<- sum(Score_1a[c(3,5,10,13,16,17,21)], na.rm = TRUE)
      Score_Depression_2<- sum(Score_2a[c(3,5,10,13,16,17,21)], na.rm = TRUE)
      Score_Depression<- c(Score_Depression_1,Score_Depression_2)
      Score_Depression<- round(Score_Depression, digits = 2)
      Score_Anxiety_1<- sum(Score_1a[c(2,4,7,9,15,19,20)], na.rm = TRUE)
      Score_Anxiety_2<- sum(Score_2a[c(2,4,7,9,15,19,20)], na.rm = TRUE)
      Score_Anxiety<- c(Score_Anxiety_1,Score_Anxiety_2)
      Score_Anxiety<- round(Score_Anxiety, digits = 2)
      Score_Stress_1<- sum(Score_1a[c(1,6,8,11,12,14,18)], na.rm = TRUE)
      Score_Stress_2<- sum(Score_2a[c(1,6,8,11,12,14,18)], na.rm = TRUE)
      Score_Stress<- c(Score_Stress_1,Score_Stress_2)
      Score_Stress<- round(Score_Stress, digits = 2)
      Change<- c(0, (Score_2 - Score_1))
      Change<- round(Change, digits = 2)
      Change_Depression<- c(0, (Score_Depression_2 - Score_Depression_1))
      Change_Depression<- round(Change_Depression, digits = 2)
      Change_Anxiety<- c(0, (Score_Anxiety_2 - Score_Anxiety_1))
      Change_Anxiety<- round(Change_Anxiety, digits = 2)
      Change_Stress<- c(0, (Score_Stress_2 - Score_Stress_1))
      Change_Stress<- round(Change_Stress, digits = 2)
  
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2)
        PTS_Depression_1<- (Rel_Depression * Score_Depression_1) + (M_Depression * (1 - Rel_Depression))
        PTS_Depression_2<- (Rel_Depression * Score_Depression_2) + (M_Depression * (1 - Rel_Depression))
        PTS_Depression<-c(PTS_Depression_1, PTS_Depression_2)
        PTS_Anxiety_1<- (Rel_Anxiety * Score_Anxiety_1) + (M_Anxiety * (1 - Rel_Anxiety))
        PTS_Anxiety_2<- (Rel_Anxiety * Score_Anxiety_2) + (M_Anxiety * (1 - Rel_Anxiety))
        PTS_Anxiety<-c(PTS_Anxiety_1,PTS_Anxiety_2)
        PTS_Stress_1<- (Rel_Stress * Score_Stress_1) + (M_Stress * (1 - Rel_Stress))
        PTS_Stress_2<- (Rel_Stress * Score_Stress_2) + (M_Stress * (1 - Rel_Stress))
        PTS_Stress<-c(PTS_Stress_1, PTS_Stress_2)
       
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2)
        PTS_Depression_1<- Score_Depression_1 + (M_Retest_Depression - M_Depression)  
        PTS_Depression_2<- Score_Depression_2 + (M_Retest_Depression - M_Depression)
        PTS_Depression<- c(PTS_Depression_1, PTS_Depression_2)
        PTS_Anxiety_1<- Score_Anxiety_1 + (M_Retest_Anxiety - M_Anxiety)  
        PTS_Anxiety_2<- Score_Anxiety_2 + (M_Retest_Anxiety - M_Anxiety)
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2)
        PTS_Stress_1<- Score_Stress_1 + (M_Retest_Stress - M_Stress)  
        PTS_Stress_2<- Score_Stress_2 + (M_Retest_Stress - M_Stress)
        PTS_Stress<- c(PTS_Stress_1, PTS_Stress_2)
       
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS<- c(PTS_1,PTS_2)
        PTS_Depression_1<- Score_Depression_1
        PTS_Depression_2<- Score_Depression_2
        PTS_Depression<- c(PTS_Depression_1, PTS_Depression_2)
        PTS_Anxiety_1<- Score_Anxiety_1
        PTS_Anxiety_2<- Score_Anxiety_2
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2)
        PTS_Stress_1<- Score_Stress_1
        PTS_Stress_2<- Score_Stress_2
        PTS_Stress<- c(PTS_Stress_1, PTS_Stress_2)
       
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Depression<- Rel_Depression * (SD_Retest_Depression/SD_Depression)
        A_Constant_Depression<- M_Retest_Depression - (B_Slope_Depression * M_Depression)
        B_Adj_Depression<- SD_Retest_Depression/SD_Depression
        A_Adj_Depression<- M_Retest_Depression - (B_Adj_Depression * M_Depression)
        PTS_Depression_1<- (B_Adj_Depression * Score_Depression_1) + A_Adj_Depression
        PTS_Depression_2<- (B_Adj_Depression * Score_Depression_2) + A_Adj_Depression
        PTS_Depression<- c(PTS_Depression_1,PTS_Depression_2)
        B_Slope_Anxiety<- Rel_Anxiety * (SD_Retest_Anxiety/SD_Anxiety)
        A_Constant_Anxiety<- M_Retest_Anxiety - (B_Slope_Anxiety * M_Anxiety)
        B_Adj_Anxiety<- SD_Retest_Anxiety/SD_Anxiety
        A_Adj_Anxiety<- M_Retest_Anxiety - (B_Adj_Anxiety * M_Anxiety)
        PTS_Anxiety_1<- (B_Adj_Anxiety * Score_Anxiety_1) + A_Adj_Anxiety
        PTS_Anxiety_2<- (B_Adj_Anxiety * Score_Anxiety_2) + A_Adj_Anxiety
        PTS_Anxiety<- c(PTS_Anxiety_1,PTS_Anxiety_2)
        B_Slope_Stress<- Rel_Stress * (SD_Retest_Stress/SD_Stress)
        A_Constant_Stress<- M_Retest_Stress - (B_Slope_Stress * M_Stress)
        B_Adj_Stress<- SD_Retest_Stress/SD_Stress
        A_Adj_Stress<- M_Retest_Stress - (B_Adj_Stress * M_Stress)
        PTS_Stress_1<- (B_Adj_Stress * Score_Stress_1) + A_Adj_Stress
        PTS_Stress_2<- (B_Adj_Stress * Score_Stress_2) + A_Adj_Stress
        PTS_Stress<- c(PTS_Stress_1,PTS_Stress_2)
        
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Depression<- Rel_Depression * (SD_Retest_Depression/SD_Depression)
        PTS_Depression_1<- B_Slope_Depression * Score_Depression_1
        PTS_Depression_2<- B_Slope_Depression * Score_Depression_2
        PTS_Depression<- c(PTS_Depression_1, PTS_Depression_2)
        B_Slope_Anxiety<- Rel_Anxiety * (SD_Retest_Anxiety/SD_Anxiety)
        PTS_Anxiety_1<- B_Slope_Anxiety * Score_Anxiety_1
        PTS_Anxiety_2<- B_Slope_Anxiety * Score_Anxiety_2
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2)
        B_Slope_Stress<- Rel_Stress * (SD_Retest_Stress/SD_Stress)
        PTS_Stress_1<- B_Slope_Stress * Score_Stress_1
        PTS_Stress_2<- B_Slope_Stress * Score_Stress_2
        PTS_Stress<- c(PTS_Stress_1, PTS_Stress_2)
       
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2)
        PTS_Depression_1<- Score_Depression_1 + (M_Retest_Depression - M_Depression)
        PTS_Depression_2<- Score_Depression_2 + (M_Retest_Depression - M_Depression)
        PTS_Depression<- c(PTS_Depression_1, PTS_Depression_2)
        PTS_Anxiety_1<- Score_Anxiety_1 + (M_Retest_Anxiety - M_Anxiety)
        PTS_Anxiety_2<- Score_Anxiety_2 + (M_Retest_Anxiety - M_Anxiety)
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2)
        PTS_Stress_1<- Score_Stress_1 + (M_Retest_Stress - M_Stress)
        PTS_Stress_2<- Score_Stress_2 + (M_Retest_Stress - M_Stress)
        PTS_Stress<- c(PTS_Stress_1, PTS_Stress_2)
        
      }
      PTS<- round(PTS, digits = 2)
      PTS_Depression<- round(PTS_Depression, digits = 2)
      PTS_Anxiety<- round(PTS_Anxiety, digits = 2)
      PTS_Stress<- round(PTS_Stress, digits = 2)
     
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2)
        SE_Depression_1<- McSweeny_SE_Depression*sqrt(1 + (1/SampleN) + ((Score_Depression_1 - M_Depression)^2/(SD_Depression^2*(SampleN-1))))
        SE_Depression_2<- McSweeny_SE_Depression*sqrt(1 + (1/SampleN) + ((Score_Depression_2 - M_Depression)^2/(SD_Depression^2*(SampleN-1))))
        SE_Depression<- c(SE_Depression_1, SE_Depression_2)
        SE_Anxiety_1<- McSweeny_SE_Anxiety*sqrt(1 + (1/SampleN) + ((Score_Anxiety_1 - M_Anxiety)^2/(SD_Anxiety^2*(SampleN-1))))
        SE_Anxiety_2<- McSweeny_SE_Anxiety*sqrt(1 + (1/SampleN) + ((Score_Anxiety_2 - M_Anxiety)^2/(SD_Anxiety^2*(SampleN-1))))
        SE_Anxiety<-c(SE_Anxiety_1, SE_Anxiety_2)
        SE_Stress_1<- McSweeny_SE_Stress*sqrt(1 + (1/SampleN) + ((Score_Stress_1 - M_Stress)^2/(SD_Stress^2*(SampleN-1))))
        SE_Stress_2<- McSweeny_SE_Stress*sqrt(1 + (1/SampleN) + ((Score_Stress_2 - M_Stress)^2/(SD_Stress^2*(SampleN-1))))
        SE_Stress<- c(SE_Stress_1, SE_Stress_2)
       
        SE<- round(SE, digits = 2)
        SE_Depression<- round(SE_Depression, digits = 2)
        SE_Anxiety<- round(SE_Anxiety, digits = 2)
        SE_Stress<- round(SE_Stress, digits = 2)
       
        CI<- c((Conf*SE1), (Conf*SE2))
        CI<- round(CI, digits = 2)
        CI_Depression<- c((Conf*SE_Depression_1), (Conf*SE_Depression_2))
        CI_Depression<- round(CI_Depression, digits = 2)
        CI_Anxiety<- c((Conf*SE_Anxiety_1), (Conf*SE_Anxiety_2))
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Stress<- c((Conf*SE_Stress_1), (Conf*SE_Stress_2))
        CI_Stress<- round(CI_Stress, digits = 2)
       
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Depression<- c((Conf*SE_Depression), (Conf*SE_Depression))
        CI_Depression<- round(CI_Depression, digits = 2)
        CI_Anxiety<- c((Conf*SE_Anxiety), (Conf*SE_Anxiety))
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Stress<- c((Conf*SE_Stress), (Conf*SE_Stress))
        CI_Stress<- round(CI_Stress, digits = 2)
       
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Depression<- PTS_Depression + CI_Depression
      CI_Upper_Lim_Depression<- round(CI_Upper_Lim_Depression, digits = 2)
      CI_Lower_Lim_Depression<-PTS_Depression - CI_Depression
      CI_Lower_Lim_Depression<- round(CI_Lower_Lim_Depression, digits = 2)
      CI_Upper_Lim_Anxiety<- PTS_Anxiety + CI_Anxiety
      CI_Upper_Lim_Anxiety<- round(CI_Upper_Lim_Anxiety, digits = 2)
      CI_Lower_Lim_Anxiety<-PTS_Anxiety - CI_Anxiety
      CI_Lower_Lim_Anxiety<- round(CI_Lower_Lim_Anxiety, digits = 2)
      CI_Upper_Lim_Stress<- PTS_Stress + CI_Stress
      CI_Upper_Lim_Stress<- round(CI_Upper_Lim_Stress, digits = 2)
      CI_Lower_Lim_Stress<-PTS_Stress - CI_Stress
      CI_Lower_Lim_Stress<- round(CI_Lower_Lim_Stress, digits = 2)
     
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Depression == "2") {
        CI_Depression<- input$Man_CI_Depression
        CI_Depression<- c(CI_Depression, CI_Depression)
        CI_Depression<- round(CI_Depression, digits = 2)
        CI_Upper_Lim_Depression<- Score_Depression + CI_Depression
        CI_Upper_Lim_Depression<- round(CI_Upper_Lim_Depression, digits = 2)
        CI_Lower_Lim_Depression<- Score_Depression - CI_Depression
        CI_Lower_Lim_Depression<- round(CI_Lower_Lim_Depression, digits = 2)
      }
      if(input$Select_CI_Anxiety == "2") {
        CI_Anxiety<- input$Man_CI_Anxiety
        CI_Anxiety<- c(CI_Anxiety, CI_Anxiety)
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Upper_Lim_Anxiety<- Score_Anxiety + CI_Anxiety
        CI_Upper_Lim_Anxiety<- round(CI_Upper_Lim_Anxiety, digits = 2)
        CI_Lower_Lim_Anxiety<- Score_Anxiety - CI_Anxiety
        CI_Lower_Lim_Anxiety<- round(CI_Lower_Lim_Anxiety, digits = 2)
      } 
      if(input$Select_CI_Stress == "2") {
        CI_Stress<- input$Man_CI_Stress
        CI_Stress<- c(CI_Stress, CI_Stress)
        CI_Stress<- round(CI_Stress, digits = 2)
        CI_Upper_Lim_Stress<- Score_Stress + CI_Stress
        CI_Upper_Lim_Stress<- round(CI_Upper_Lim_Stress, digits = 2)
        CI_Lower_Lim_Stress<- Score_Stress - CI_Stress
        CI_Lower_Lim_Stress<- round(CI_Lower_Lim_Stress, digits = 2)
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
      Cutoff_Score_Depression_1<- round(input$Cutoff_Depression_1, digits = 2)
      Cutoff_Score_Depression_1<- rep(Cutoff_Score_Depression_1, 2)
      Cutoff_Score_Depression_2<- round(input$Cutoff_Depression_2, digits = 2)
      Cutoff_Score_Depression_2<- rep(Cutoff_Score_Depression_2, 2)
      Cutoff_Score_Depression_3<- round(input$Cutoff_Depression_3, digits = 2)
      Cutoff_Score_Depression_3<- rep(Cutoff_Score_Depression_3, 2)
      Cutoff_Score_Depression_4<- round(input$Cutoff_Depression_4, digits = 2)
      Cutoff_Score_Depression_4<- rep(Cutoff_Score_Depression_4, 2)
      Cutoff_Score_Depression_5<- round(input$Cutoff_Depression_5, digits = 2)
      Cutoff_Score_Depression_5<- rep(Cutoff_Score_Depression_5, 2)
      Cutoff_Score_Anxiety_1<- round(input$Cutoff_Anxiety_1, digits = 2)
      Cutoff_Score_Anxiety_1<- rep(Cutoff_Score_Anxiety_1, 2)
      Cutoff_Score_Anxiety_2<- round(input$Cutoff_Anxiety_2, digits = 2)
      Cutoff_Score_Anxiety_2<- rep(Cutoff_Score_Anxiety_2, 2)
      Cutoff_Score_Anxiety_3<- round(input$Cutoff_Anxiety_3, digits = 2)
      Cutoff_Score_Anxiety_3<- rep(Cutoff_Score_Anxiety_3, 2)
      Cutoff_Score_Anxiety_4<- round(input$Cutoff_Anxiety_4, digits = 2)
      Cutoff_Score_Anxiety_4<- rep(Cutoff_Score_Anxiety_4, 2)
      Cutoff_Score_Anxiety_5<- round(input$Cutoff_Anxiety_5, digits = 2)
      Cutoff_Score_Anxiety_5<- rep(Cutoff_Score_Anxiety_5, 2)
      Cutoff_Score_Stress_1<- round(input$Cutoff_Stress_1, digits = 2)
      Cutoff_Score_Stress_1<- rep(Cutoff_Score_Stress_1, 2)
      Cutoff_Score_Stress_2<- round(input$Cutoff_Stress_2, digits = 2)
      Cutoff_Score_Stress_2<- rep(Cutoff_Score_Stress_2, 2)
      Cutoff_Score_Stress_3<- round(input$Cutoff_Stress_3, digits = 2)
      Cutoff_Score_Stress_3<- rep(Cutoff_Score_Stress_3, 2)
      Cutoff_Score_Stress_4<- round(input$Cutoff_Stress_4, digits = 2)
      Cutoff_Score_Stress_4<- rep(Cutoff_Score_Stress_4, 2)
      Cutoff_Score_Stress_5<- round(input$Cutoff_Stress_5, digits = 2)
      Cutoff_Score_Stress_5<- rep(Cutoff_Score_Stress_5, 2)
      
      
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Cutoff_Score_4,Cutoff_Score_5,
                                      Score_Depression,Change_Depression,PTS_Depression, SE_Depression, CI_Upper_Lim_Depression, CI_Lower_Lim_Depression, Cutoff_Score_Depression_1,Cutoff_Score_Depression_2,Cutoff_Score_Depression_3,Cutoff_Score_Depression_4,Cutoff_Score_Depression_5,
                                      Score_Anxiety,Change_Anxiety, PTS_Anxiety, SE_Anxiety, CI_Upper_Lim_Anxiety, CI_Lower_Lim_Anxiety, Cutoff_Score_Anxiety_1,Cutoff_Score_Anxiety_2,Cutoff_Score_Anxiety_3,Cutoff_Score_Anxiety_4,Cutoff_Score_Anxiety_5, 
                                      Score_Stress,Change_Stress,PTS_Stress, SE_Stress, CI_Upper_Lim_Stress, CI_Lower_Lim_Stress, Cutoff_Score_Stress_1,Cutoff_Score_Stress_2,Cutoff_Score_Stress_3,Cutoff_Score_Stress_4,Cutoff_Score_Stress_5)
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
      Score_Depression_1<- sum(Score_1a[c(3,5,10,13,16,17,21)], na.rm = TRUE)
      Score_Depression_2<- sum(Score_2a[c(3,5,10,13,16,17,21)], na.rm = TRUE)
      Score_Depression_3<- sum(Score_3a[c(3,5,10,13,16,17,21)], na.rm = TRUE)
      Score_Depression<- c(Score_Depression_1,Score_Depression_2,Score_Depression_3)
      Score_Depression<- round(Score_Depression, digits = 2)
      Score_Anxiety_1<- sum(Score_1a[c(2,4,7,9,15,19,20)], na.rm = TRUE)
      Score_Anxiety_2<- sum(Score_2a[c(2,4,7,9,15,19,20)], na.rm = TRUE)
      Score_Anxiety_3<- sum(Score_3a[c(2,4,7,9,15,19,20)], na.rm = TRUE)
      Score_Anxiety<- c(Score_Anxiety_1,Score_Anxiety_2, Score_Anxiety_3)
      Score_Anxiety<- round(Score_Anxiety, digits = 2)
      Score_Stress_1<- sum(Score_1a[c(1,6,8,11,12,14,18)], na.rm = TRUE)
      Score_Stress_2<- sum(Score_2a[c(1,6,8,11,12,14,18)], na.rm = TRUE)
      Score_Stress_3<- sum(Score_3a[c(1,6,8,11,12,14,18)], na.rm = TRUE)
      Score_Stress<- c(Score_Stress_1,Score_Stress_2,Score_Stress_3)
      Score_Stress<- round(Score_Stress, digits = 2)
      
      Change<- c(0, (Score_2 - Score_1), (Score_3 - Score_2))
      Change<- round(Change, digits = 2)
      Change_Depression<- c(0, Score_Depression_2 - Score_Depression_1, Score_Depression_3 - Score_Depression_2)
      Change_Depression<- round(Change_Depression, digits = 2)
      Change_Anxiety<- c(0, Score_Anxiety_2 - Score_Anxiety_1, Score_Anxiety_3 - Score_Anxiety_2)
      Change_Anxiety<- round(Change_Anxiety, digits = 2)
      Change_Stress<- c(0, Score_Stress_2 - Score_Stress_1, Score_Stress_3 - Score_Stress_2)
      Change_Stress<- round(Change_Stress, digits = 2)
      
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS_3<- (Rel * Score_3) + (M * (1 - Rel))
        PTS<<- c(PTS_1, PTS_2, PTS_3)
        PTS_Depression_1<- (Rel_Depression * Score_Depression_1) + (M_Depression * (1 - Rel_Depression))
        PTS_Depression_2<- (Rel_Depression * Score_Depression_2) + (M_Depression * (1 - Rel_Depression))
        PTS_Depression_3<- (Rel_Depression * Score_Depression_3) + (M_Depression * (1 - Rel_Depression))
        PTS_Depression<<- c(PTS_Depression_1, PTS_Depression_2, PTS_Depression_3)
        PTS_Anxiety_1<- (Rel_Anxiety * Score_Anxiety_1) + (M_Anxiety * (1 - Rel_Anxiety))
        PTS_Anxiety_2<- (Rel_Anxiety * Score_Anxiety_2) + (M_Anxiety * (1 - Rel_Anxiety))
        PTS_Anxiety_3<- (Rel_Anxiety * Score_Anxiety_3) + (M_Anxiety * (1 - Rel_Anxiety))
        PTS_Anxiety<- c(PTS_Anxiety_1,PTS_Anxiety_2, PTS_Anxiety_3)
        PTS_Stress_1<- (Rel_Stress * Score_Stress_1) + (M_Stress * (1 - Rel_Stress))
        PTS_Stress_2<- (Rel_Stress * Score_Stress_2) + (M_Stress * (1 - Rel_Stress))
        PTS_Stress_3<- (Rel_Stress * Score_Stress_3) + (M_Stress * (1 - Rel_Stress))
        PTS_Stress<<- c(PTS_Stress_1, PTS_Stress_2, PTS_Stress_3)
        
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Depression_1<- Score_Depression_1 + (M_Retest_Depression - M_Depression)  
        PTS_Depression_2<- Score_Depression_2 + (M_Retest_Depression - M_Depression)
        PTS_Depression_3<- Score_Depression_3 + (M_Retest_Depression - M_Depression)
        PTS_Depression<- c(PTS_Depression_1, PTS_Depression_2, PTS_Depression_3)
        PTS_Anxiety_1<- Score_Anxiety_1 + (M_Retest_Anxiety - M_Anxiety)  
        PTS_Anxiety_2<- Score_Anxiety_2 + (M_Retest_Anxiety - M_Anxiety)
        PTS_Anxiety_3<- Score_Anxiety_3 + (M_Retest_Anxiety - M_Anxiety)
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2, PTS_Anxiety_3)
        PTS_Stress_1<- Score_Stress_1 + (M_Retest_Stress - M_Stress)  
        PTS_Stress_2<- Score_Stress_2 + (M_Retest_Stress - M_Stress)
        PTS_Stress_3<- Score_Stress_3 + (M_Retest_Stress - M_Stress)
        PTS_Stress<- c(PTS_Stress_1, PTS_Stress_2, PTS_Stress_3)
       
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS_1<- Score_1
        PTS_2<- Score_2
        PTS_3<- Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Depression_1<- Score_Depression_1
        PTS_Depression_2<- Score_Depression_2
        PTS_Depression_3<- Score_Depression_3
        PTS_Depression<- c(PTS_Depression_1, PTS_Depression_2, PTS_Depression_3)
        PTS_Anxiety_1<- Score_Anxiety_1
        PTS_Anxiety_2<- Score_Anxiety_2
        PTS_Anxiety_3<- Score_Anxiety_3
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2, PTS_Anxiety_3)
        PTS_Stress_1<- Score_Stress_1
        PTS_Stress_2<- Score_Stress_2
        PTS_Stress_3<- Score_Stress_3
        PTS_Stress<- c(PTS_Stress_1, PTS_Stress_2, PTS_Stress_3)
       
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS_3<- (B_Adj * Score_3) + A_Adj
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Depression<- Rel_Depression * (SD_Retest_Depression/SD_Depression)
        A_Constant_Depression<- M_Retest_Depression - (B_Slope_Depression * M_Depression)
        B_Adj_Depression<- SD_Retest_Depression/SD_Depression
        A_Adj_Depression<- M_Retest_Depression - (B_Adj_Depression * M_Depression)
        PTS_Depression_1<- (B_Adj_Depression * Score_Depression_1) + A_Adj_Depression
        PTS_Depression_2<- (B_Adj_Depression * Score_Depression_2) + A_Adj_Depression
        PTS_Depression_3<- (B_Adj_Depression * Score_Depression_3) + A_Adj_Depression
        PTS_Depression<- c(PTS_Depression_1, PTS_Depression_2, PTS_Depression_3)
        B_Slope_Anxiety<- Rel_Anxiety * (SD_Retest_Anxiety/SD_Anxiety)
        A_Constant_Anxiety<- M_Retest_Anxiety - (B_Slope_Anxiety * M_Anxiety)
        B_Adj_Anxiety<- SD_Retest_Anxiety/SD_Anxiety
        A_Adj_Anxiety<- M_Retest_Anxiety - (B_Adj_Anxiety * M_Anxiety)
        PTS_Anxiety_1<- (B_Adj_Anxiety * Score_Anxiety_1) + A_Adj_Anxiety
        PTS_Anxiety_2<- (B_Adj_Anxiety * Score_Anxiety_2) + A_Adj_Anxiety
        PTS_Anxiety_3<- (B_Adj_Anxiety * Score_Anxiety_3) + A_Adj_Anxiety
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2, PTS_Anxiety_3)
        B_Slope_Stress<- Rel_Stress * (SD_Retest_Stress/SD_Stress)
        A_Constant_Stress<- M_Retest_Stress - (B_Slope_Stress * M_Stress)
        B_Adj_Stress<- SD_Retest_Stress/SD_Stress
        A_Adj_Stress<- M_Retest_Stress - (B_Adj_Stress * M_Stress)
        PTS_Stress_1<- (B_Adj_Stress * Score_Stress_1) + A_Adj_Stress
        PTS_Stress_2<- (B_Adj_Stress * Score_Stress_2) + A_Adj_Stress
        PTS_Stress_3<- (B_Adj_Stress * Score_Stress_3) + A_Adj_Stress
        PTS_Stress<- c(PTS_Stress_1, PTS_Stress_2, PTS_Stress_3)
       
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope * Score_1
        PTS_2<- B_Slope * Score_2
        PTS_3<- B_Slope * Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Depression<- Rel_Depression * (SD_Retest_Depression/SD_Depression)
        PTS_Depression_1<- B_Slope_Depression * Score_Depression_1
        PTS_Depression_2<- B_Slope_Depression * Score_Depression_2
        PTS_Depression_3<- B_Slope_Depression * Score_Depression_3
        PTS_Depression<- c(PTS_Depression_1, PTS_Depression_2, PTS_Depression_3)
        B_Slope_Anxiety<- Rel_Anxiety * (SD_Retest_Anxiety/SD_Anxiety)
        PTS_Anxiety_1<- B_Slope_Anxiety * Score_Anxiety_1
        PTS_Anxiety_2<- B_Slope_Anxiety * Score_Anxiety_2
        PTS_Anxiety_3<- B_Slope_Anxiety * Score_Anxiety_3
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2, PTS_Anxiety_3)
        B_Slope_Stress<- Rel_Stress * (SD_Retest_Stress/SD_Stress)
        PTS_Stress_1<- B_Slope_Stress * Score_Stress_1
        PTS_Stress_2<- B_Slope_Stress * Score_Stress_2
        PTS_Stress_3<- B_Slope_Stress * Score_Stress_3
        PTS_Stress<- c(PTS_Stress_1, PTS_Stress_2, PTS_Stress_3)
        
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Depression_1<- Score_Depression_1 + (M_Retest_Depression - M_Depression)
        PTS_Depression_2<- Score_Depression_2 + (M_Retest_Depression - M_Depression)
        PTS_Depression_3<- Score_Depression_3 + (M_Retest_Depression - M_Depression)
        PTS_Depression<- c(PTS_Depression_1, PTS_Depression_2, PTS_Depression_3)
        PTS_Anxiety_1<- Score_Anxiety_1 + (M_Retest_Anxiety - M_Anxiety)
        PTS_Anxiety_2<- Score_Anxiety_2 + (M_Retest_Anxiety - M_Anxiety)
        PTS_Anxiety_3<- Score_Anxiety_3 + (M_Retest_Anxiety - M_Anxiety)
        PTS_Anxiety<- c(PTS_Anxiety_1, PTS_Anxiety_2, PTS_Anxiety_3)
        PTS_Stress_1<- Score_Stress_1 + (M_Retest_Stress - M_Stress)
        PTS_Stress_2<- Score_Stress_2 + (M_Retest_Stress - M_Stress)
        PTS_Stress_3<- Score_Stress_3 + (M_Retest_Stress - M_Stress)
        PTS_Stress<- c(PTS_Stress_1, PTS_Stress_2, PTS_Stress_3)
       
      }
      PTS<- round(PTS, digits = 2)
      PTS_Depression<- round(PTS_Depression, digits = 2)
      PTS_Anxiety<- round(PTS_Anxiety, digits = 2)
      PTS_Stress<- round(PTS_Stress, digits = 2)
     
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE3<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_3 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2, SE3)
        SE_Depression_1<- McSweeny_SE_Depression*sqrt(1 + (1/SampleN) + ((Score_Depression_1 - M_Depression)^2/(SD_Depression^2*(SampleN-1))))
        SE_Depression_2<- McSweeny_SE_Depression*sqrt(1 + (1/SampleN) + ((Score_Depression_2 - M_Depression)^2/(SD_Depression^2*(SampleN-1))))
        SE_Depression_3<- McSweeny_SE_Depression*sqrt(1 + (1/SampleN) + ((Score_Depression_3 - M_Depression)^2/(SD_Depression^2*(SampleN-1))))
        SE_Depression<- c(SE_Depression_1, SE_Depression_2, SE_Depression_3)
        SE_Anxiety_1<- McSweeny_SE_Anxiety*sqrt(1 + (1/SampleN) + ((Score_Anxiety_1 - M_Anxiety)^2/(SD_Anxiety^2*(SampleN-1))))
        SE_Anxiety_2<- McSweeny_SE_Anxiety*sqrt(1 + (1/SampleN) + ((Score_Anxiety_2 - M_Anxiety)^2/(SD_Anxiety^2*(SampleN-1))))
        SE_Anxiety_3<- McSweeny_SE_Anxiety*sqrt(1 + (1/SampleN) + ((Score_Anxiety_3 - M_Anxiety)^2/(SD_Anxiety^2*(SampleN-1))))
        SE_Anxiety<- c(SE_Anxiety_1, SE_Anxiety_2, SE_Anxiety_3)
        SE_Stress_1<- McSweeny_SE_Stress*sqrt(1 + (1/SampleN) + ((Score_Stress_1 - M_Stress)^2/(SD_Stress^2*(SampleN-1))))
        SE_Stress_2<- McSweeny_SE_Stress*sqrt(1 + (1/SampleN) + ((Score_Stress_2 - M_Stress)^2/(SD_Stress^2*(SampleN-1))))
        SE_Stress_3<- McSweeny_SE_Stress*sqrt(1 + (1/SampleN) + ((Score_Stress_3 - M_Stress)^2/(SD_Stress^2*(SampleN-1))))
        SE_Stress<- c(SE_Stress_1, SE_Stress_2, SE_Stress_3)
       
        SE<- round(SE, digits = 2)
        SE_Depression<- round(SE_Depression, digits = 2)
        SE_Anxiety<- round(SE_Anxiety, digits = 2)
        SE_Stress<- round(SE_Stress, digits = 2)
       
        CI<- c((Conf*SE1), (Conf*SE2), (Conf*SE3))
        CI<- round(CI, digits = 2)
        CI_Depression<- c((Conf*SE_Depression_1), (Conf*SE_Depression_2), (Conf*SE_Depression_3))
        CI_Depression<- round(CI_Depression, digits = 2)
        CI_Anxiety<- c((Conf*SE_Anxiety_1), (Conf*SE_Anxiety_2), (Conf*SE_Anxiety_3))
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Stress<- c((Conf*SE_Stress_1), (Conf*SE_Stress_2), (Conf*SE_Stress_3))
        CI_Stress<- round(CI_Stress, digits = 2)
       
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Depression<- c((Conf*SE_Depression), (Conf*SE_Depression), (Conf*SE_Depression))
        CI_Depression<- round(CI_Depression, digits = 2)
        CI_Anxiety<- c((Conf*SE_Anxiety), (Conf*SE_Anxiety), (Conf*SE_Anxiety))
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Stress<- c((Conf*SE_Stress), (Conf*SE_Stress), (Conf*SE_Stress))
        CI_Stress<- round(CI_Stress, digits = 2)
       
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Depression<- PTS_Depression + CI_Depression
      CI_Upper_Lim_Depression<- round(CI_Upper_Lim_Depression, digits = 2)
      CI_Lower_Lim_Depression<-PTS_Depression - CI_Depression
      CI_Lower_Lim_Depression<- round(CI_Lower_Lim_Depression, digits = 2)
      CI_Upper_Lim_Anxiety<- PTS_Anxiety + CI_Anxiety
      CI_Upper_Lim_Anxiety<- round(CI_Upper_Lim_Anxiety, digits = 2)
      CI_Lower_Lim_Anxiety<-PTS_Anxiety - CI_Anxiety
      CI_Lower_Lim_Anxiety<- round(CI_Lower_Lim_Anxiety, digits = 2)
      CI_Upper_Lim_Stress<- PTS_Stress + CI_Stress
      CI_Upper_Lim_Stress<- round(CI_Upper_Lim_Stress, digits = 2)
      CI_Lower_Lim_Stress<-PTS_Stress - CI_Stress
      CI_Lower_Lim_Stress<- round(CI_Lower_Lim_Stress, digits = 2)
     
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Depression == "2") {
        CI_Depression<- input$Man_CI_Depression
        CI_Depression<- c(CI_Depression, CI_Depression, CI_Depression)
        CI_Depression<- round(CI_Depression, digits = 2)
        CI_Upper_Lim_Depression<- Score_Depression + CI_Depression
        CI_Upper_Lim_Depression<- round(CI_Upper_Lim_Depression, digits = 2)
        CI_Lower_Lim_Depression<- Score_Depression - CI_Depression
        CI_Lower_Lim_Depression<- round(CI_Lower_Lim_Depression, digits = 2)
      }
      if(input$Select_CI_Anxiety == "2") {
        CI_Anxiety<- input$Man_CI_Anxiety
        CI_Anxiety<- c(CI_Anxiety, CI_Anxiety, CI_Anxiety)
        CI_Anxiety<- round(CI_Anxiety, digits = 2)
        CI_Upper_Lim_Anxiety<- Score_Anxiety + CI_Anxiety
        CI_Upper_Lim_Anxiety<- round(CI_Upper_Lim_Anxiety, digits = 2)
        CI_Lower_Lim_Anxiety<- Score_Anxiety - CI_Anxiety
        CI_Lower_Lim_Anxiety<- round(CI_Lower_Lim_Anxiety, digits = 2)
      }
      if(input$Select_CI_Stress == "2") {
        CI_Stress<- input$Man_CI_Stress
        CI_Stress<- c(CI_Stress, CI_Stress, CI_Stress)
        CI_Stress<- round(CI_Stress, digits = 2)
        CI_Upper_Lim_Stress<- Score_Stress + CI_Stress
        CI_Upper_Lim_Stress<- round(CI_Upper_Lim_Stress, digits = 2)
        CI_Lower_Lim_Stress<- Score_Stress - CI_Stress
        CI_Lower_Lim_Stress<- round(CI_Lower_Lim_Stress, digits = 2)
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
      Cutoff_Score_Depression_1<- round(input$Cutoff_Depression_1, digits = 2)
      Cutoff_Score_Depression_1<- rep(Cutoff_Score_Depression_1, 3)
      Cutoff_Score_Depression_2<- round(input$Cutoff_Depression_2, digits = 2)
      Cutoff_Score_Depression_2<- rep(Cutoff_Score_Depression_2, 3)
      Cutoff_Score_Depression_3<- round(input$Cutoff_Depression_3, digits = 2)
      Cutoff_Score_Depression_3<- rep(Cutoff_Score_Depression_3, 3)
      Cutoff_Score_Depression_4<- round(input$Cutoff_Depression_4, digits = 2)
      Cutoff_Score_Depression_4<- rep(Cutoff_Score_Depression_4, 3)
      Cutoff_Score_Depression_5<- round(input$Cutoff_Depression_5, digits = 2)
      Cutoff_Score_Depression_5<- rep(Cutoff_Score_Depression_5, 3)
      Cutoff_Score_Anxiety_1<- round(input$Cutoff_Anxiety_1, digits = 2)
      Cutoff_Score_Anxiety_1<- rep(Cutoff_Score_Anxiety_1, 3)
      Cutoff_Score_Anxiety_2<- round(input$Cutoff_Anxiety_2, digits = 2)
      Cutoff_Score_Anxiety_2<- rep(Cutoff_Score_Anxiety_2, 3)
      Cutoff_Score_Anxiety_3<- round(input$Cutoff_Anxiety_3, digits = 2)
      Cutoff_Score_Anxiety_3<- rep(Cutoff_Score_Anxiety_3, 3)
      Cutoff_Score_Anxiety_4<- round(input$Cutoff_Anxiety_4, digits = 2)
      Cutoff_Score_Anxiety_4<- rep(Cutoff_Score_Anxiety_4, 3)
      Cutoff_Score_Anxiety_5<- round(input$Cutoff_Anxiety_5, digits = 2)
      Cutoff_Score_Anxiety_5<- rep(Cutoff_Score_Anxiety_5, 3)
      Cutoff_Score_Stress_1<- round(input$Cutoff_Stress_1, digits = 2)
      Cutoff_Score_Stress_1<- rep(Cutoff_Score_Stress_1, 3)
      Cutoff_Score_Stress_2<- round(input$Cutoff_Stress_2, digits = 2)
      Cutoff_Score_Stress_2<- rep(Cutoff_Score_Stress_2, 3)
      Cutoff_Score_Stress_3<- round(input$Cutoff_Stress_3, digits = 2)
      Cutoff_Score_Stress_3<- rep(Cutoff_Score_Stress_3, 3)
      Cutoff_Score_Stress_4<- round(input$Cutoff_Stress_4, digits = 2)
      Cutoff_Score_Stress_4<- rep(Cutoff_Score_Stress_4, 3)
      Cutoff_Score_Stress_5<- round(input$Cutoff_Stress_5, digits = 2)
      Cutoff_Score_Stress_5<- rep(Cutoff_Score_Stress_5, 3)
      
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Cutoff_Score_4,Cutoff_Score_5,
                                      Score_Depression,Change_Depression,PTS_Depression, SE_Depression, CI_Upper_Lim_Depression, CI_Lower_Lim_Depression, Cutoff_Score_Depression_1,Cutoff_Score_Depression_2,Cutoff_Score_Depression_3,Cutoff_Score_Depression_4,Cutoff_Score_Depression_5,
                                      Score_Anxiety,Change_Anxiety, PTS_Anxiety, SE_Anxiety, CI_Upper_Lim_Anxiety, CI_Lower_Lim_Anxiety, Cutoff_Score_Anxiety_1,Cutoff_Score_Anxiety_2,Cutoff_Score_Anxiety_3,Cutoff_Score_Anxiety_4,Cutoff_Score_Anxiety_5, 
                                      Score_Stress,Change_Stress,PTS_Stress, SE_Stress, CI_Upper_Lim_Stress, CI_Lower_Lim_Stress, Cutoff_Score_Stress_1,Cutoff_Score_Stress_2,Cutoff_Score_Stress_3,Cutoff_Score_Stress_4,Cutoff_Score_Stress_5)
    }
    
  
    #Create dataframes to be displayed as tables in pdf report (statistical values used in reliable change analyses)
    
    if(input$RelChangeMethod == 'Nunnally & Bernstein (1994)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Depression<<- data.frame(Pop,  M_Depression, SD_Depression, RelChangeMethod, Rel_Depression, ConfInt)
      names(Stats_Table_Depression)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Anxiety<<- data.frame(Pop,  M_Anxiety, SD_Anxiety, RelChangeMethod, Rel_Anxiety, ConfInt)
      names(Stats_Table_Anxiety)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Stress<<- data.frame(Pop,  M_Stress, SD_Stress, RelChangeMethod, Rel_Stress, ConfInt)
      names(Stats_Table_Stress)<<- c("Reference Population",  "M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      
      } else if (input$RelChangeMethod == 'Chelune et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Depression<<- data.frame(Pop,  M_Depression, M_Retest_Depression, SD_Depression, RelChangeMethod, Rel_Depression, ConfInt)
      names(Stats_Table_Depression)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Anxiety<<- data.frame(Pop,  M_Anxiety, M_Retest_Anxiety, SD_Anxiety, RelChangeMethod, Rel_Anxiety, ConfInt)
      names(Stats_Table_Anxiety)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Stress<<- data.frame(Pop,  M_Stress, M_Retest_Stress, SD_Stress, RelChangeMethod, Rel_Stress, ConfInt)
      names(Stats_Table_Stress)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
     
    } else if (input$RelChangeMethod == 'Maassen et al. (2006)' | input$RelChangeMethod == 'McSweeny et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Depression<<- data.frame(Pop,  M_Depression, M_Retest_Depression, SD_Depression, SD_Retest_Depression, RelChangeMethod, Rel_Depression, ConfInt)
      names(Stats_Table_Depression)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Anxiety<<- data.frame(Pop,  M_Anxiety, M_Retest_Anxiety, SD_Anxiety, SD_Retest_Anxiety, RelChangeMethod, Rel_Anxiety, ConfInt)
      names(Stats_Table_Anxiety)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Stress<<- data.frame(Pop,  M_Stress, M_Retest_Stress, SD_Stress, SD_Retest_Stress, RelChangeMethod, Rel_Stress, ConfInt)
      names(Stats_Table_Stress)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
     
    } else if (input$RelChangeMethod == 'Crawford & Howell (1998)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, SampleN, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Depression<<- data.frame(Pop,  M_Depression, M_Retest_Depression, SD_Depression, SD_Retest_Depression, RelChangeMethod, Rel_Depression, SampleN,ConfInt)
      names(Stats_Table_Depression)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Anxiety<<- data.frame(Pop,  M_Anxiety, M_Retest_Anxiety, SD_Anxiety, SD_Retest_Anxiety, RelChangeMethod, Rel_Anxiety, SampleN, ConfInt)
      names(Stats_Table_Anxiety)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Stress<<- data.frame(Pop,  M_Stress, M_Retest_Stress, SD_Stress, SD_Retest_Stress, RelChangeMethod, Rel_Stress, SampleN,ConfInt)
      names(Stats_Table_Stress)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
     
    } else if (input$RelChangeMethod == 'Jacobson & Truax (1991)' | input$RelChangeMethod == 'Speer (1992)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Depression<<- data.frame(Pop,  SD_Depression, RelChangeMethod, Rel_Depression, ConfInt)
      names(Stats_Table_Depression)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Anxiety<<- data.frame(Pop,  SD_Anxiety, RelChangeMethod, Rel_Anxiety, ConfInt)
      names(Stats_Table_Anxiety)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Stress<<- data.frame(Pop,  SD_Stress, RelChangeMethod, Rel_Stress, ConfInt)
      names(Stats_Table_Stress)<<- c("Reference Population",  "Sd", "Reliable Change Method", "Reliability", "Confidence")
      
    }
    
    #If custom confidence intervals options are selected, make sure that no values appear for PS and SE in pdf tables
    #Change the look of tables in report if custom confidence intervals options are selected
    
    if (input$Select_CI == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS = NA, SE = NA)
      Stats_Table_Fullscale<<- Stats_Table_Fullscale %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI[1])
    }
    if (input$Select_CI_Depression == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Depression = NA, SE_Depression = NA)
      Stats_Table_Depression<<- Stats_Table_Depression %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Depression[1])
    }
    if (input$Select_CI_Anxiety == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Anxiety = NA, SE_Anxiety = NA)
      Stats_Table_Anxiety<<- Stats_Table_Anxiety %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                      "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Anxiety[1])
    }
    if (input$Select_CI_Stress == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Stress = NA, SE_Stress = NA)
      Stats_Table_Stress<<- Stats_Table_Stress %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                      "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Stress[1])
    }
    
    
    #Create a dataframe to be used in the analytics spreadsheet.
    
    #Calculate variables for significant improvement
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$CI_Lower_Lim[1]) {
      DASS21.Fullscale.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$CI_Lower_Lim[1]) {
      DASS21.Fullscale.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$CI_Upper_Lim[1]) {
      DASS21.Fullscale.Sig.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$CI_Upper_Lim[1]) {
      DASS21.Fullscale.Sig.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Depression[length(Entered_Scores_Df$Score_Depression)] < Entered_Scores_Df$CI_Lower_Lim_Depression[1]) {
      DASS21.Depression.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Depression[length(Entered_Scores_Df$Score_Depression)] >= Entered_Scores_Df$CI_Lower_Lim_Depression[1]) {
      DASS21.Depression.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Depression[length(Entered_Scores_Df$Score_Depression)] > Entered_Scores_Df$CI_Upper_Lim_Depression[1]) {
      DASS21.Depression.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Depression[length(Entered_Scores_Df$Score_Depression)] <= Entered_Scores_Df$CI_Upper_Lim_Depression[1]) {
      DASS21.Depression.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] < Entered_Scores_Df$CI_Lower_Lim_Anxiety[1]) {
      DASS21.Anxiety.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] >= Entered_Scores_Df$CI_Lower_Lim_Anxiety[1]) {
      DASS21.Anxiety.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] > Entered_Scores_Df$CI_Upper_Lim_Anxiety[1]) {
      DASS21.Anxiety.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] <= Entered_Scores_Df$CI_Upper_Lim_Anxiety[1]) {
      DASS21.Anxiety.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Stress[length(Entered_Scores_Df$Score_Stress)] < Entered_Scores_Df$CI_Lower_Lim_Stress[1]) {
      DASS21.Stress.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Stress[length(Entered_Scores_Df$Score_Stress)] >= Entered_Scores_Df$CI_Lower_Lim_Stress[1]) {
      DASS21.Stress.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Stress[length(Entered_Scores_Df$Score_Stress)] > Entered_Scores_Df$CI_Upper_Lim_Stress[1]) {
      DASS21.Stress.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Stress[length(Entered_Scores_Df$Score_Stress)] <= Entered_Scores_Df$CI_Upper_Lim_Stress[1]) {
      DASS21.Stress.Sig.Deterioration<- "No"
    }
    
   
    
    
    #Calculate variables for improvement
    
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$Score[1]) {
      DASS21.Fullscale.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$Score[1]) {
      DASS21.Fullscale.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$Score[1]) {
      DASS21.Fullscale.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$Score[1]) {
      DASS21.Fullscale.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Depression[length(Entered_Scores_Df$Score_Depression)] < Entered_Scores_Df$Score_Depression[1]) {
      DASS21.Depression.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Depression[length(Entered_Scores_Df$Score_Depression)] >= Entered_Scores_Df$Score_Depression[1]) {
      DASS21.Depression.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Depression[length(Entered_Scores_Df$Score_Depression)] > Entered_Scores_Df$Score_Depression[1]) {
      DASS21.Depression.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Depression[length(Entered_Scores_Df$Score_Depression)] <= Entered_Scores_Df$Score_Depression[1]) {
      DASS21.Depression.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] < Entered_Scores_Df$Score_Anxiety[1]) {
      DASS21.Anxiety.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] >= Entered_Scores_Df$Score_Anxiety[1]) {
      DASS21.Anxiety.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] > Entered_Scores_Df$Score_Anxiety[1]) {
      DASS21.Anxiety.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] <= Entered_Scores_Df$Score_Anxiety[1]) {
      DASS21.Anxiety.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Stress[length(Entered_Scores_Df$Score_Stress)] < Entered_Scores_Df$Score_Stress[1]) {
      DASS21.Stress.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Stress[length(Entered_Scores_Df$Score_Stress)] >= Entered_Scores_Df$Score_Stress[1]) {
      DASS21.Stress.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Stress[length(Entered_Scores_Df$Score_Stress)] > Entered_Scores_Df$Score_Stress[1]) {
      DASS21.Stress.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Stress[length(Entered_Scores_Df$Score_Stress)] <= Entered_Scores_Df$Score_Stress[1]) {
      DASS21.Stress.Deterioration<- "No"
    }
    
    
    DASS21.Fullscale.Change<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] - Entered_Scores_Df$Score[1]
    DASS21.Depression.Change<- Entered_Scores_Df$Score_Depression[length(Entered_Scores_Df$Score_Depression)] - Entered_Scores_Df$Score_Depression[1]
    DASS21.Anxiety.Change<- Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)] - Entered_Scores_Df$Score_Anxiety[1]
    DASS21.Stress.Change<- Entered_Scores_Df$Score_Stress[length(Entered_Scores_Df$Score_Stress)] - Entered_Scores_Df$Score_Stress[1]
  
    DASS21.Fullscale.Comparisons<- length(Entered_Scores_Df$Change) - 1
    DASS21.Depression.Comparisons<- length(Entered_Scores_Df$Change_Depression) - 1
    DASS21.Anxiety.Comparisons<- length(Entered_Scores_Df$Change_Anxiety) - 1
    DASS21.Stress.Comparisons<- length(Entered_Scores_Df$Change_Stress) - 1
   
    DASS21.Fullscale.First.Date<- Entered_Scores_Df$Date[1]
    DASS21.Depression.First.Date<- Entered_Scores_Df$Date[1]
    DASS21.Anxiety.First.Date<- Entered_Scores_Df$Date[1]
    DASS21.Stress.First.Date<- Entered_Scores_Df$Date[1]
   
    DASS21.Fullscale.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    DASS21.Depression.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    DASS21.Anxiety.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    DASS21.Stress.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    
    DASS21.Fullscale.First.Score<- Entered_Scores_Df$Score[1]
    DASS21.Depression.First.Score<- Entered_Scores_Df$Score_Depression[1]
    DASS21.Anxiety.First.Score<- Entered_Scores_Df$Score_Anxiety[1]
    DASS21.Stress.First.Score<- Entered_Scores_Df$Score_Stress[1]
  
    DASS21.Fullscale.Last.Score<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)]
    DASS21.Depression.Last.Score<- Entered_Scores_Df$Score_Depression[length(Entered_Scores_Df$Score_Depression)]
    DASS21.Anxiety.Last.Score<- Entered_Scores_Df$Score_Anxiety[length(Entered_Scores_Df$Score_Anxiety)]
    DASS21.Stress.Last.Score<- Entered_Scores_Df$Score_Stress[length(Entered_Scores_Df$Score_Stress)]
   
    
    
    Analytics_Df<<- data.frame(DASS21.Fullscale.First.Date, DASS21.Fullscale.First.Score, DASS21.Fullscale.Comparisons, DASS21.Fullscale.Change, DASS21.Fullscale.Last.Date, DASS21.Fullscale.Last.Score, DASS21.Fullscale.Improvement,DASS21.Fullscale.Sig.Improvement, DASS21.Fullscale.Deterioration, DASS21.Fullscale.Sig.Deterioration,
                               DASS21.Depression.First.Date, DASS21.Depression.First.Score, DASS21.Depression.Comparisons, DASS21.Depression.Change, DASS21.Depression.Last.Date, DASS21.Depression.Last.Score, DASS21.Depression.Improvement, DASS21.Depression.Sig.Improvement, DASS21.Depression.Deterioration, DASS21.Depression.Sig.Deterioration,
                               DASS21.Anxiety.First.Date, DASS21.Anxiety.First.Score, DASS21.Anxiety.Comparisons, DASS21.Anxiety.Change, DASS21.Anxiety.Last.Date, DASS21.Anxiety.Last.Score, DASS21.Anxiety.Improvement, DASS21.Anxiety.Sig.Improvement, DASS21.Anxiety.Deterioration, DASS21.Anxiety.Sig.Deterioration, 
                               DASS21.Stress.First.Date, DASS21.Stress.First.Score, DASS21.Stress.Comparisons, DASS21.Stress.Change, DASS21.Stress.Last.Date, DASS21.Stress.Last.Score, DASS21.Stress.Improvement, DASS21.Stress.Sig.Improvement, DASS21.Stress.Deterioration, DASS21.Stress.Sig.Deterioration)
    
    
    
  })
  
  
  
  #Create an expression to activate the entered data & create notifications to indicate inaccurate data entry
  
  Entered_Scores_Notifications<- observeEvent(input$Action_Submit_Data, {
    
    Entered_Scores_Reac()
    
    if(length(Score_1a) < 21) {
      showNotification("The DASS-21 is a 21-item scale. You have entered less than 21 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(length(Score_1a) > 21) {
      showNotification("The DASS-21 is a 21-item scale. You have entered more than 21 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(input$Timepoint != "1") {
      if(length(Score_2a) < 21) {
        showNotification("The DASS-21 is a 21-item scale. You have entered less than 21 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_2a) > 21) {
        showNotification("The DASS-21 is a 21-item scale. You have entered more than 21 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
    if(input$Timepoint == "3") {
      if(length(Score_3a) < 21) {
        showNotification("The DASS-21 is a 21-item scale. You have entered less than 21 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_3a) > 21) {
        showNotification("The DASS-21 is a 21-item scale. You have entered more than 21 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
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
    
    Gap_Depression<- Entered_Scores_Df[1,13] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),13]
    Entered_Scores_Df[1,14]<- Gap_Depression
    
    Gap_Anxiety<- Entered_Scores_Df[1,24] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),24]
    Entered_Scores_Df[1,25]<- Gap_Anxiety
    
    Gap_Stress<- Entered_Scores_Df[1,35] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),35]
    Entered_Scores_Df[1,36]<- Gap_Stress
    
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
    
    filename = paste0(" DASS-21 Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
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
        Entered_Scores_Df = Entered_Scores_Df,
        Tab_Reference = Tab_Reference,
        Stats_Table_Fullscale = Stats_Table_Fullscale,
        Tab_Reference = Tab_Reference,
        Stats_Table_Depression = Stats_Table_Depression,
        Stats_Table_Anxiety = Stats_Table_Anxiety,
        Stats_Table_Stress = Stats_Table_Stress,
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
      paste(paste0(" DASS-21 Data ", format(Sys.time(), '%d/%m/%y')), input$Output_Filetype1, sep = ".")
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













