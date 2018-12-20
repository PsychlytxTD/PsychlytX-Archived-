
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
      menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "AQOL8D"),
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
      br(),
      tags$footer(tags$a(href = "http://psychlytx.com.au", target = "_blank", HTML("<br><center>"), "PsychlytX", tags$sup(icon("registered")), br(),
                         "© Timothy Deitz 2018"))
    )
  )
  
  dashboardPage(
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| Assessment of Quality of Life - 8 Dimensions (AQoL-8D)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 850),
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
                
                "Maxwell, A., Ozmen, M., Iezzi, A., & Richardson, J. (2016). Norms for the AQoL-6D and AQoL-8D multi attribute utility instruments. Research Paper, 94.", br(), br(),
                "Richardson J., Khan. M., Iezzi, A., Sinha, K., Mihalopoulos, C., Herrman, C., Hawthorne, H & Schweitzer, I. (2009). The AQoL-8D (PsyQoL) MAU Instrument: Overview September 2009. Research Paper, 39.", br(), br(),
                "Richardson, J., & Iezzi, A. (2011). Psychometric Validity and the AQoL-8D Multi Attribute Utility Instrument. Research Paper, 71."

        ),
        
        
        
        tabItem(tabName = "AQOL8D",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel("Scale",
                             h2(tags$strong("AQoL-8D")),
                             hr(),
                             h4(tags$strong(tags$em("Tick the box that best describes your situation as it has been over the past week ")), style = "color:red"),
                             fluidRow(
                               column(width = 12,
                                 h4(tags$strong("Q1 How much energy do you have to do the things you want to do?")),
                                 radioButtons("Item_1", h4(tags$strong("I am")), choices = c("always full of energy" = "1", "usually full of energy" = "2",
                                                                            "occasionally full of energy" = "3", "usually tired and lacking energy" = "4",
                                                                            "always tired and lacking energy" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q2 How often do you feel socially excluded or left out?")),
                                      radioButtons("Item_2", "", choices = c("never" = "1", "rarely" = "2","sometimes" = "3", "often" = "4","always" = "5"), 
                                                   width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q3 How easy or difficult is it for you to get around by yourself outside your place of residence
                                                     (eg. to go shopping, visiting)?")),
                                      radioButtons("Item_3", "", choices = c("getting around is enjoyable and easy" = "1", "I have no difficulty getting
                                                                             around my place of residence" = "2","a little difficulty" = "3", 
                                                                             "moderate difficulty" = "4","a lot of difficulty" = "5",
                                                                             "I cannot get around unless somebody is there to help me" = "6"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q4 Does your health affect your role in your community (e.g. residential, sporting,
                                                     church or cultural activities)?")),
                                      radioButtons("Item_4", "", choices = c("my role in the community is unaffected by my health" = "1", "there are some parts of my 
                                                                             community role I cannot carry out" = "2", "there are many parts of my community role 
                                                                             I cannot carry out" = "3", 
                                                                             "I cannot carry out any part of my community role" = "4"), width = '100%', selected = character(0))
                                      )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q5 How often do you feel sad?")),
                                      radioButtons("Item_5", "", choices = c("never" = "1", "rarely" = "2",
                                                                             "some of the time" = "3", "usually" = "4",
                                                                             "nearly all the time" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q6 How often do you experience serious pain?")),
                                      radioButtons("Item_6", h4(tags$strong("I experience it")), choices = c("very rarely" = "1", "less than once a week" = "2",
                                                                             "once or twice a week" = "3", "three to four times a week" = "4",
                                                                             "most of the time" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q7 How much confidence to you have in yourself?")),
                                      radioButtons("Item_7", "", choices = c("complete confidence" = "1", "a lot" = "2",
                                                    "a moderate amount" = "3", "a little" = "4", "none at all" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q8 Do you normally feel calm and tranquil or agitated?")),
                                      radioButtons("Item_8", h4(tags$strong("I am")), choices = c("always calm and tranquil" = "1",
                                                      "usually calm and tranquil" = "2", "sometimes calm and tranquil,
                                                      sometimes agitated" = "3", "usually agitated" = "4", "always agitated" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q9 Does your health affect your relationship with your family?")),
                                      radioButtons("Item_9", "", choices = c("my role in the family is unaffected
                                                   by my health" = "1", "there are some parts of my family role I
                                                   cannot carry out" = "2", "there are many parts of my family role I cannot
                                                   carry out" = "3", "I cannot carry out any part of my family role" = "4"), width = '100%', selected = character(0))
                                      )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q10 How satisfying are your close relationships (family and friends)?")),
                                      radioButtons("Item_10", "", choices = c("very satisfying" = "1", "satisfying" = "2",
                                                                             "neither satisfying nor dissatisfying" = "3", 
                                                                             "dissatisfying" = "4",
                                                                             "unpleasant" = "5",
                                                                             "very unpleasant" = "6"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q11 How well do you communicate with others (talking, signing, texting,
                                                     being understood by others and understanding them)?")),
                                      radioButtons("Item_11", "", choices = c("I have no trouble being understood" = "1",
                                                          "I have some difficulty being understood by people who do not know me" = "2",
                                                           "I am understood only by people who know me" = "3", 
                                                          "I cannot adequately communicate with others" = "4"), width = '100%', selected = character(0))
                                      )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q12 How often do you have trouble sleeping?")),
                                      radioButtons("Item_12", "", choices = c("never" = "1",
                                                                              "almost never" = "2",
                                                                              "sometimes" = "3", 
                                                                              "often" = "4",
                                                                              "all the time" = "5"), width = '100%', selected = character(0))
                                      )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q13 How often do you feel worthless?")),
                                      radioButtons("Item_13", "", choices = c("never" = "1",
                                                                              "almost never" = "2",
                                                                              "sometimes" = "3", 
                                                                              "usually" = "4",
                                                                              "always" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q14 How often do you feel angry?")),
                                      radioButtons("Item_14", "", choices = c("never" = "1",
                                                                              "almost never" = "2",
                                                                              "sometimes" = "3", 
                                                                              "often" = "4",
                                                                              "all the time" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q15 How easy or difficult is it for you to move around 
                                                     (using any aids or equipment you need eg a wheelchair,
                                                     frame or stick)?")),
                                      radioButtons("Item_15", "", choices = c("I am very mobile" = "1",
                                                                              "I have no difficulty with mobility" = "2",
                                                                              "I have some difficulty with mobility
                                                                              (for example, going uphill)" = "3", 
                                                                              "I have difficulty with mobility. I can go short distances only" = "4",
                                                                              "I have a lot of difficulty with mobility. I need someone to help me" = "5",
                                                                              "I am bedridden" = "6"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q16 Do you ever feel like hurting yourself?")),
                                      radioButtons("Item_16", "", choices = c("never" = "1",
                                                                              "rarely" = "2",
                                                                              "sometimes" = "3", 
                                                                              "often" = "4",
                                                                              "all the time" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q17 How enthusiastic do you feel?")),
                                      radioButtons("Item_17", "", choices = c("extremely" = "1",
                                                                              "very" = "2",
                                                                              "somewhat" = "3", 
                                                                              "not much" = "4",
                                                                              "not at all" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q18 How often did you feel worried in the last seven days?")),
                                      radioButtons("Item_18", "", choices = c("never" = "1",
                                                                              "occasionally" = "2",
                                                                              "sometimes" = "3", 
                                                                              "often" = "4",
                                                                              "all the time" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q19 How difficult is it for you to wash, toilet, dress yourself,
                                                     eat or care for your appearance?")),
                                      radioButtons("Item_19", "", 
                                                   choices = c("these things are very easy for me to do" = "1",
                                                              "I have no real difficulty in doing these things" = "2",
                                                               "I find some of these things difficult, but I manage to do
                                                              them on my own" = "3", 
                                                               "many of these things are difficult, and I need help to do them" = "4",
                                                                "I cannot do these things by myself at all" = "5"), width = '100%', selected = character(0))
                                      )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q20 How often do you feel happy?")),
                                      radioButtons("Item_20", "", choices = c("all the time" = "1",
                                                                              "mostly" = "2",
                                                                              "sometimes" = "3", 
                                                                              "almost never" = "4",
                                                                              "never" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q21 How much do you feel you can cope with life's problems?")),
                                      radioButtons("Item_21", "", choices = c("completely" = "1",
                                                                              "mostly" = "2",
                                                                              "partly" = "3", 
                                                                              "very little" = "4",
                                                                              "not at all" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q22 How much pain or discomfort do you experience?")),
                                      radioButtons("Item_22", "", choices = c("none at all" = "1",
                                                                              "mostly" = "2",
                                                                              "I have moderate pain" = "3", 
                                                                              "I suffer from severe pain" = "4",
                                                                              "I suffer unbearable pain" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q23 How much do you enjoy your close relationships (family and friends)?")),
                                      radioButtons("Item_23", "", choices = c("immensely" = "1",
                                                                              "a lot" = "2",
                                                                              "a little" = "3", 
                                                                              "not much" = "4",
                                                                              "I hate it" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q24 How often does pain interfere with your usual activities?")),
                                      radioButtons("Item_24", "", choices = c("never" = "1",
                                                                              "rarely" = "2",
                                                                              "sometimes" = "3", 
                                                                              "often" = "4",
                                                                              "always" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q25 How often do you feel pleasure?")),
                                      radioButtons("Item_25", "", choices = c("always" = "1",
                                                                              "usually" = "2",
                                                                              "sometimes" = "3", 
                                                                              "almost never" = "4",
                                                                              "never" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q26 How much of a burden do you feel you are to other people?")),
                                      radioButtons("Item_26", "", choices = c("not at all" = "1",
                                                                              "a little" = "2",
                                                                              "a moderate amount" = "3", 
                                                                              "a lot" = "4",
                                                                              "totally" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q27 How content are you with your life?")),
                                      radioButtons("Item_27", "", choices = c("extremely" = "1",
                                                                              "mainly" = "2",
                                                                              "moderately" = "3", 
                                                                              "slightly" = "4",
                                                                              "not at all" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q28 How well can you see (using your glasses or contact lenses if they are needed)?")),
                                      radioButtons("Item_28", "", choices = c("I have excellent sight" = "1",
                                                                              "I see normaly" = "2",
                                                                              "I have some difficulty seeing things sharply (e.g. small print, objects in 
                                                                              the distance, or watching television)" = "3", 
                                                                              "I only see general shapes" = "4",
                                                                              "I am completely blind" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q29 How often do you feel in control of your life?")),
                                      radioButtons("Item_29", "", choices = c("always" = "1",
                                                                              "mostly" = "2",
                                                                              "sometimes" = "3", 
                                                                              "only occasionally" = "4",
                                                                              "never" = "5"), width = '100%', selected = character(0))
                             )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q30 How much help do you need with jobs around your place of residence
                                                     (e.g. preparing food, cleaning, gardening)?")),
                                      radioButtons("Item_30", "", choices = c("I can do all these tasks very easily without help" = "1",
                                                                              "I can do these tasks relatively easily without help" = "2",
                                                                              "I can do these tasks only very slowly without help" = "3", 
                                                                              "I cannot do most of these tasks unless I have help" = "4",
                                                                              "I can do none of these tasks by myself" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q31 How often do you feel socially isolated?")),
                                      radioButtons("Item_31", "", choices = c("never" = "1",
                                                                              "rarely" = "2",
                                                                              "sometimes" = "3", 
                                                                              "often" = "4",
                                                                              "always" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q32 How well can you hear (using your hearing aid if needed)?")),
                                      radioButtons("Item_32", "", choices = c("I have excellent hearing" = "1",
                                                                              "I hear normally" = "2",
                                                                              "I have some difficulty hearing or I do
                                                                              not hear clearly (e.g. when there is 
                                                                              background noise" = "3", 
                                                                              "I have difficulty hearing things clearly.
                                                                              Often I do not understand what is said. I
                                                                              usually do not take part in conversations
                                                                              because I cannot hear what is said" = "4",
                                                                              "I hear very little" = "5",
                                                                              "I am completely deaf" = "6"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q33 How often do you feel depressed?")),
                                      radioButtons("Item_33", "", choices = c("never" = "1",
                                                                              "almost never" = "2",
                                                                              "sometimes" = "3", 
                                                                              "often" = "4",
                                                                              "very often" = "5",
                                                                              "all the time" = "6"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q34 How happy are you with your close and intimate relationships?")),
                                      radioButtons("Item_34", "", choices = c("very happy" = "1",
                                                                              "generally happy" = "2",
                                                                              "neither happy nor unhappy" = "3", 
                                                                              "generally unhappy" = "4",
                                                                              "very unhappy" = "5"), width = '100%', selected = character(0))
                               )),
                             fluidRow(
                               column(width = 12,
                                      h4(tags$strong("Q35 How often did you feel despair in the last seven days?")),
                                      radioButtons("Item_35", "", choices = c("never" = "1",
                                                                              "occasionally" = "2",
                                                                              "sometimes" = "3", 
                                                                              "often" = "4",
                                                                              "all the time" = "5"), width = '100%', selected = character(0))
                               )),

                                      hr(),
                                      fluidRow(
                                        column(width = 4, dateInput("Q_Date", "Date",  format = "dd/mm/yyyy")),
                                        column(width = 4, textInput("Q_Name", "Name")),
                                        column(width = 4, textInput("Q_Clin_Name", "Clinician's Name"))
                                      ),
                             fluidRow(
                               column(width = 12, h5("Scale Source: Richardson & Iezzi (2011)"))
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
                                                      selectInput("Pop", "", choices = c("Females: General Population (16-24 yrs)", "Males: General Population (16-24 yrs)",
                                                                                         "Females: General Population (25-34 yrs)", "Males: General Population (25-34 yrs)",
                                                                                         "Females: General Population (35-44 yrs)", "Males: General Population (25-44 yrs)",
                                                                                         "Females: General Population (45-54 yrs)", "Males: General Population (45-54 yrs)",
                                                                                         "Females: General Population (55-64 yrs)", "Males: General Population (66-64 yrs)",
                                                                                         "Females: General Population (65-74 yrs)", "Males: General Population (65-74 yrs)")
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
                                                        column(width = 3,
                                                               selectInput("Select_CI", label = "AQOL-8D total scale", 
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI == '2'",
                                                                                numericInput("Man_CI", "Specify the width of the confidence interval", value = 0))
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 3,
                                                               selectInput("Select_CI_Super_Mental", label = "Mental Super Dimension", 
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Super_Mental == '2'",
                                                                                numericInput("Man_CI_Super_Mental", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 3,
                                                               selectInput("Select_CI_Super_Physical", label = "Physical Super Dimension",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Super_Physical == '2'",
                                                                                numericInput("Man_CI_Super_Physical", "Specify the width of the confidence interval", value = 0))
                                                        )
                                                        
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               selectInput("Select_CI_Independent_Living", label = "Independent Living",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Independent_Living == '2'",
                                                                                numericInput("Man_CI_Independent_Living", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Pain", label = "Pain",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Pain == '2'",
                                                                                numericInput("Man_CI_Pain", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Senses", label = "Senses",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Senses == '2'",
                                                                                numericInput("Man_CI_Senses", "Specify the width of the confidence interval", value = 0))
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               selectInput("Select_CI_Relationships", label = "Relationships",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Relationships == '2'",
                                                                                numericInput("Man_CI_Relationships", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Mental_Health", label = "Mental Health",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Mental_Health == '2'",
                                                                                numericInput("Man_CI_Mental_Health", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Coping", label = "Coping",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Coping == '2'",
                                                                                numericInput("Man_CI_Coping", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Self_Worth", label = "Self Worth",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Self_Worth == '2'",
                                                                                numericInput("Man_CI_Self_Worth", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Happiness", label = "Happiness",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Happiness == '2'",
                                                                                numericInput("Man_CI_Happiness", "Specify the width of the confidence interval", value = 0))
                                                        )

                                                      )
                                                      
                                                     
                                             ),
                                             
                                             tabPanel("Mean", width = 12,
                                                      h4(tags$strong("Enter a mean value")),
                                                      fluidRow(
                                                        column(width = 3,
                                                               uiOutput("Mean_Widg")
                                                        ) 
                                                      ),
                                                      fluidRow(
                                                        column(width = 3, 
                                                               uiOutput("Mean_Widg_Super_Mental")
                                                        ),
                                                        column(width = 3, 
                                                               uiOutput("Mean_Widg_Super_Physical")
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Independent_Living")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Pain")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Senses")
                                                        )
                                                        
                                                      ),
                                                      
                                                      fluidRow(
                                                        
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Relationships")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Mental_Health")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Coping")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Self_Worth")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Happiness")
                                                        )
                                                      ),
                                                      
                                                  

                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Chelune et al. (1993)' || input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)' || input.RelChangeMethod == 'Speer (1992)'",
                                                                       
                                                                       h4(tags$strong("Enter a mean value for the retest timepoint")),
                                                                       
                                                                       fluidRow(
                                                                         column(width = 3,
                                                                                numericInput("Retest_Mean", "AQOL-8D total scale", value = 0)
                                                                         )
                                                                       ),
                                                                       
                                                                       fluidRow(
                                                                         column(width = 3,
                                                                                numericInput("Retest_Mean_Super_Mental", "Mental Super Dimension", value = 0)
                                                                         ),
                                                                         column(width = 3,
                                                                                numericInput("Retest_Mean_Super_Physical", "Physical Super Dimension", value = 0)
                                                                         )
                                                                       ),
                                                                       
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Independent_Living", "Independent Living", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Pain", "Pain", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Senses", "Senses", value = 0)
                                                                         )
                                                                       ),
                                                                       
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Relationships", "Relationships", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Mental_Health", "Mental Health", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Coping", "Coping", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Self_Worth", "Self Worth", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Happiness", "Happiness", value = 0)
                                                                         )
                                                                       )
       
                                                      )
                                                      
                                                      
                                             ),
                                             
                                             tabPanel("Sd", width = 12,
                                                      h4(tags$strong("Enter a standard deviation value")),
                                                      fluidRow(
                                                        column(width = 3,
                                                               uiOutput("Sd_Widg")
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 3,
                                                               uiOutput("Sd_Widg_Super_Mental")
                                                        ),
                                                        column(width = 3,
                                                               uiOutput("Sd_Widg_Super_Physical")
                                                        )
                                                        
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Independent_Living")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Pain")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Senses")
                                                        )
                                                        
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Relationships")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Mental_Health")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Coping")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Self_Worth")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Happiness")
                                                        )
                                                      ),

                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)'",
                                                                       
                                                                       h4(tags$strong("Enter a standard deviation value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 3,
                                                                                numericInput("Retest_Sd", "AQOL-8D total scale", value = 0)
                                                                         ) 
                                                                       ),

                                                                       fluidRow(
                                                                         column(width = 3,
                                                                                numericInput("Retest_Sd_Super_Mental", "Mental Super Dimension", value = 0)
                                                                         ),
                                                                         column(width = 3,
                                                                                numericInput("Retest_Sd_Super_Physical", "Physical Super Dimension", value = 0)
                                                                         )
                                                                       ),
                                                                       
                                                                       
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Independent_Living", "Independent Living", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Pain", "Pain", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Senses", "Senses", value = 0)
                                                                         )
                                                                       ),
                                                                       
                                                                       
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Relationships", "Relationships", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Mental_Health", "Mental Health", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Coping", "Coping", value = 0)
                                                                        ),
                                                                        column(width = 2,
                                                                               numericInput("Retest_Sd_Self_Worth", "Self Worth", value = 0)
                                                                        ),
                                                                        column(width = 2,
                                                                               numericInput("Retest_Sd_Happiness", "Happiness", value = 0)
                                                                        )
                                                                       )
     
                                                      )
                                             ),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      h4(tags$strong("Enter test-retest reliability values")), 
                                                      fluidRow(
                                                        column(width = 3,
                                                               numericInput("Reliability", "AQOL8D total scale", value = .907),
                                                               h6("Reference: Richardson & Iezzi (2011)")
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 3,
                                                               numericInput("Reliability_Super_Mental", "Mental Super Dimension", value = .902),
                                                               h6("Reference: Richardson & Iezzi (2011)")
                                                        ),
                                                        column(width = 3,
                                                               numericInput("Reliability_Super_Physical", "Physical Super Dimension", value = .842),
                                                               h6("Reference: Richardson & Iezzi (2011)")
                                                        )
                                                        
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability_Independent_Living", "Independent Living", value = .861),
                                                               h6("Reference: Richardson & Iezzi (2011)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Pain", "Pain", value = .851),
                                                               h6("Reference: Richardson & Iezzi (2011)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Senses", "Senses", value = .644),
                                                               h6("Reference: Richardson & Iezzi (2011)")
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability_Relationships", "Relationships", value = .783),
                                                               h6("Reference: Richardson & Iezzi (2011)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Mental_Health", "Mental Health", value = .870),
                                                               h6("Reference: Richardson & Iezzi (2011)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Coping", "Coping", value = .816),
                                                               h6("Reference: Richardson & Iezzi (2011)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Self_Worth", "Self Worth", value = .863),
                                                               h6("Reference: Richardson & Iezzi (2011)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Happiness", "Happiness", value = .858),
                                                               h6("Reference: Richardson & Iezzi (2011)")
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
                                                        column(width = 3,
                                                               uiOutput("Cutoff_Widg_1") 
                                                        ) 
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 3,
                                                               uiOutput("Cutoff_Widg_Super_Mental_1") 
                                                        ),
                                                        column(width = 3,
                                                               uiOutput("Cutoff_Widg_Super_Physical_1") 
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Independent_Living_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Pain_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Senses_1") 
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Relationships_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Mental_Health_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Coping_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Self_Worth_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Happiness_1") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Second cut-off score")),
                                                      fluidRow(
                                                        column(width = 3,
                                                               uiOutput("Cutoff_Widg_2") 
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 3,
                                                               uiOutput("Cutoff_Widg_Super_Mental_2") 
                                                        ),
                                                        column(width = 3,
                                                               uiOutput("Cutoff_Widg_Super_Physical_2") 
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Independent_Living_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Pain_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Senses_2") 
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Relationships_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Mental_Health_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Coping_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Self_Worth_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Happiness_2") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      
                                                      hr(),
                                                      h4(tags$strong("Third cut-off score")),
                                                      fluidRow(
                                                        column(width = 3,
                                                               uiOutput("Cutoff_Widg_3") 
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 3,
                                                               uiOutput("Cutoff_Widg_Super_Mental_3") 
                                                        ),
                                                        column(width = 3,
                                                               uiOutput("Cutoff_Widg_Super_Physical_3") 
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Independent_Living_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Pain_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Senses_3") 
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Relationships_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Mental_Health_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Coping_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Self_Worth_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Happiness_3") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr()
                                                      
                                             ),
                                             
                                             
                                             h4(tags$strong("Psychometric Properties of the AQOL8D Relevant to Assessing Reliable & Clinically Significant Change")),
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
                      input$Item_17, input$Item_18, input$Item_19, input$Item_20, 
                      input$Item_21, input$Item_22, input$Item_23, input$Item_24, input$Item_25, input$Item_26, input$Item_27, 
                      input$Item_28, input$Item_29, input$Item_30, input$Item_31, input$Item_32, input$Item_33, input$Item_34, input$Item_35, sep = ",")
    
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
                    , pageLength = 16, dom = "t", scrollX = TRUE, fixedColumns = list(leftColumns = 3), autoWidth = TRUE, columnDefs = list(list(width = '180px', targets = c(1,9,10)), list(width= '140px', targets = c(0,2))))) %>% formatStyle('Authors', color = '#ffffff', backgroundColor = '#d35400')
    
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
    
    if(input$Pop == "Females: General Population (16-24 yrs)") {
      Mean_Val<<- 80.97
      Sd_Val<<- 11.46
      Mean_Val_Independent_Living<<-95.04
      Sd_Val_Independent_Living<<- 7.49
      Mean_Val_Relationships<<- 83.66
      Sd_Val_Relationships<<- 12.11
      Mean_Val_Mental_Health<<- 74.1
      Sd_Val_Mental_Health<<- 15.4
      Mean_Val_Coping<<- 73.43
      Sd_Val_Coping<<-17.69
      Mean_Val_Pain<<- 90.62
      Sd_Val_Pain<<-12.79
      Mean_Val_Senses<<- 91.59
      Sd_Val_Senses<<- 9.4
      Mean_Val_Self_Worth<<- 72.46
      Sd_Val_Self_Worth<<- 19.77
      Mean_Val_Happiness<<- 72.12
      Sd_Val_Happiness<<- 17.46
      Mean_Val_Super_Mental<<- 76.09
      Sd_Val_Super_Mental<<- 14.34
      Mean_Val_Super_Physical<<- 92.87
      Sd_Val_Super_Physical<<- 7.11
    } else if (input$Pop == "Males: General Population (16-24 yrs)") {
        Mean_Val<<- 86.89
        Sd_Val<<- 7.04
        Mean_Val_Independent_Living<<- 95.83 
        Sd_Val_Independent_Living<<- 5.49
        Mean_Val_Relationships<<- 88
        Sd_Val_Relationships<<- 7.62
        Mean_Val_Mental_Health<<- 82.7
        Sd_Val_Mental_Health<<- 9.1
        Mean_Val_Coping<<- 84.13
        Sd_Val_Coping<<-10.12
        Mean_Val_Pain<<- 92.38
        Sd_Val_Pain<<-8.49
        Mean_Val_Senses<<- 93.42
        Sd_Val_Senses<<- 5.46
        Mean_Val_Self_Worth<<- 83.83
        Sd_Val_Self_Worth<<- 11.1
        Mean_Val_Happiness<<- 79.23
        Sd_Val_Happiness<<- 11.42
        Mean_Val_Super_Mental<<- 83.88
        Sd_Val_Super_Mental<<- 8.47
        Mean_Val_Super_Physical<<- 94.22
        Sd_Val_Super_Physical<<- 4.98
    } else if (input$Pop == "Females: General Population (25-34 yrs)") {
        Mean_Val<<- 80.7
        Sd_Val<<- 12.82
        Mean_Val_Independent_Living<<-94.91
        Sd_Val_Independent_Living<<- 9.99
        Mean_Val_Relationships<<- 84.19
        Sd_Val_Relationships<<- 13.55
        Mean_Val_Mental_Health<<- 73.64
        Sd_Val_Mental_Health<<- 15.71
        Mean_Val_Coping<<-71.77 
        Sd_Val_Coping<<-18.96
        Mean_Val_Pain<<- 89.07
        Sd_Val_Pain<<-17.18
        Mean_Val_Senses<<- 91.46
        Sd_Val_Senses<<- 9.32
        Mean_Val_Self_Worth<<- 74.99
        Sd_Val_Self_Worth<<- 20.36
        Mean_Val_Happiness<<- 70.37
        Sd_Val_Happiness<<- 18.27
        Mean_Val_Super_Mental<<- 75.9
        Sd_Val_Super_Mental<<- 15.25
        Mean_Val_Super_Physical<<- 92.39
        Sd_Val_Super_Physical<<- 9.56
    } else if (input$Pop == "Males: General Population (25-34 yrs)") {
        Mean_Val<<- 82.99
        Sd_Val<<-10.83
        Mean_Val_Independent_Living<<- 94.34
        Sd_Val_Independent_Living<<- 8.68
        Mean_Val_Relationships<<- 84.64
        Sd_Val_Relationships<<- 12.41
        Mean_Val_Mental_Health<<- 77.91
        Sd_Val_Mental_Health<<- 13.26
        Mean_Val_Coping<<- 77.81
        Sd_Val_Coping<<- 15.49
        Mean_Val_Pain<<- 89.71
        Sd_Val_Pain<<-13.52
        Mean_Val_Senses<<- 91.89
        Sd_Val_Senses<<- 9.32
        Mean_Val_Self_Worth<<- 79.5
        Sd_Val_Self_Worth<<- 16.7
        Mean_Val_Happiness<<- 72.97
        Sd_Val_Happiness<<- 16.25
        Mean_Val_Super_Mental<<- 79.12
        Sd_Val_Super_Mental<<- 12.88
        Mean_Val_Super_Physical<<- 92.43
        Sd_Val_Super_Physical<<- 8.16
    } else if (input$Pop == "Females: General Population (35-44 yrs)") {
      Mean_Val<<-79.42
      Sd_Val<<- 12.77
        Mean_Val_Independent_Living<<-93.02
        Sd_Val_Independent_Living<<- 11.43
        Mean_Val_Relationships<<- 82.15
        Sd_Val_Relationships<<- 14.46
        Mean_Val_Mental_Health<<- 72.57
        Sd_Val_Mental_Health<<- 15.5
        Mean_Val_Coping<<- 71.12
        Sd_Val_Coping<<- 17.69
        Mean_Val_Pain<<- 85.07
        Sd_Val_Pain<<-19.94
        Mean_Val_Senses<<- 89.96
        Sd_Val_Senses<<- 9.43
        Mean_Val_Self_Worth<<- 76.65
        Sd_Val_Self_Worth<<- 18.76
        Mean_Val_Happiness<<- 69.84
        Sd_Val_Happiness<<- 18.41
        Mean_Val_Super_Mental<<- 75.04
        Sd_Val_Super_Mental<<- 14.96
        Mean_Val_Super_Physical<<- 90.1
        Sd_Val_Super_Physical<<- 10.24
    } else if (input$Pop == "Males: General Population (35-44 yrs)") {
      Mean_Val<<- 80.16
        Sd_Val<<- 11.05
        Mean_Val_Independent_Living<<- 93.11
        Sd_Val_Independent_Living<<-10.61
        Mean_Val_Relationships<<- 82.2
        Sd_Val_Relationships<<- 12.87
        Mean_Val_Mental_Health<<- 75.41
        Sd_Val_Mental_Health<<- 13.22
        Mean_Val_Coping<<- 72.71
        Sd_Val_Coping<<- 15.27
        Mean_Val_Pain<<- 86.82
        Sd_Val_Pain<<- 16.1
        Mean_Val_Senses<<- 90.06
        Sd_Val_Senses<<- 9.61
        Mean_Val_Self_Worth<<- 77.87
        Sd_Val_Self_Worth<<- 16.61
        Mean_Val_Happiness<<- 67.05
        Sd_Val_Happiness<<- 15.96
        Mean_Val_Super_Mental<<- 75.88
        Sd_Val_Super_Mental<<- 12.83
        Mean_Val_Super_Physical<<- 90.61
        Sd_Val_Super_Physical<<- 9.75
    } else if (input$Pop == "Females: General Population (45-54 yrs)") {
      Mean_Val<<- 78.04
        Sd_Val<<- 13.78
        Mean_Val_Independent_Living<<-92.10
        Sd_Val_Independent_Living<<-13.52
        Mean_Val_Relationships<<- 81.85
        Sd_Val_Relationships<<- 15.22
        Mean_Val_Mental_Health<<- 71.81
        Sd_Val_Mental_Health<<- 16.91
        Mean_Val_Coping<<- 70.02
        Sd_Val_Coping<<- 19.44
        Mean_Val_Pain<<- 82.97
        Sd_Val_Pain<<- 22.41
        Mean_Val_Senses<<- 85.12
        Sd_Val_Senses<<- 10.41
        Mean_Val_Self_Worth<<- 75.78
        Sd_Val_Self_Worth<<- 20.74
        Mean_Val_Happiness<<- 67.51
        Sd_Val_Happiness<<- 18.36
        Mean_Val_Super_Mental<<- 74.09
        Sd_Val_Super_Mental<<- 15.87
        Mean_Val_Super_Physical<<- 87.66
        Sd_Val_Super_Physical<<- 12.25
    } else if (input$Pop == "Males: General Population (45-54 yrs)") {
      Mean_Val<<- 79.7
        Sd_Val<<- 14.09
        Mean_Val_Independent_Living<<- 91.38
        Sd_Val_Independent_Living<<- 13.97
        Mean_Val_Relationships<<- 81.68
        Sd_Val_Relationships<<- 16.78
        Mean_Val_Mental_Health<<- 76.35
        Sd_Val_Mental_Health<<- 16.36
        Mean_Val_Coping<<- 72.64
        Sd_Val_Coping<<-17.6
        Mean_Val_Pain<<- 82.49
        Sd_Val_Pain<<- 21.13
        Mean_Val_Senses<<- 84.66
        Sd_Val_Senses<<- 11.31
        Mean_Val_Self_Worth<<- 80.43
        Sd_Val_Self_Worth<<- 18.42
        Mean_Val_Happiness<<- 69.13
        Sd_Val_Happiness<<- 19.24
        Mean_Val_Super_Mental<<- 76.68
        Sd_Val_Super_Mental<<- 15.9
        Mean_Val_Super_Physical<<- 87.08
        Sd_Val_Super_Physical<<-  12.56
    } else if (input$Pop == "Females: General Population (55-64 yrs)") {
      Mean_Val<<- 79.46
        Sd_Val<<- 13.44
        Mean_Val_Independent_Living<<-89.43
        Sd_Val_Independent_Living<<-16.61
        Mean_Val_Relationships<<-83.47
        Sd_Val_Relationships<<- 15.33
        Mean_Val_Mental_Health<<- 75.14
        Sd_Val_Mental_Health<<- 15.13
        Mean_Val_Coping<<- 73.64
        Sd_Val_Coping<<- 17.85
        Mean_Val_Pain<<- 79.71
        Sd_Val_Pain<<- 24.49
        Mean_Val_Senses<<- 84.96
        Sd_Val_Senses<<- 9.84
        Mean_Val_Self_Worth<<- 79.52
        Sd_Val_Self_Worth<<- 18.46
        Mean_Val_Happiness<<- 70.08
        Sd_Val_Happiness<<- 18.69
        Mean_Val_Super_Mental<<- 76.93
        Sd_Val_Super_Mental<<- 14.81
        Mean_Val_Super_Physical<<- 85.64
        Sd_Val_Super_Physical<<- 13.81
    } else if (input$Pop == "Males: General Population (55-64 yrs)") {
      Mean_Val<<- 80.93
        Sd_Val<<- 14.31
        Mean_Val_Independent_Living<<-89.86
        Sd_Val_Independent_Living<<-17.05
        Mean_Val_Relationships<<- 83.59
        Sd_Val_Relationships<<- 16.85
        Mean_Val_Mental_Health<<- 77.99
        Sd_Val_Mental_Health<<- 16.58
        Mean_Val_Coping<<- 75.85
        Sd_Val_Coping<<- 19.45
        Mean_Val_Pain<<- 82.15
        Sd_Val_Pain<<- 25.44
        Mean_Val_Senses<<- 83.89
        Sd_Val_Senses<<- 11.95
        Mean_Val_Self_Worth<<- 83.66
        Sd_Val_Self_Worth<<- 18.67
        Mean_Val_Happiness<<- 71.08
        Sd_Val_Happiness<<- 21.09
        Mean_Val_Super_Mental<<- 78.82
        Sd_Val_Super_Mental<<- 16
        Mean_Val_Super_Physical<<- 86.08
        Sd_Val_Super_Physical<<- 14.45
    } else if (input$Pop == "Females: General Population (65-74 yrs)") {
      Mean_Val<<- 81.12
        Sd_Val<<- 13.99
        Mean_Val_Independent_Living<<-87.68
        Sd_Val_Independent_Living<<-20.16
        Mean_Val_Relationships<<- 84.96
        Sd_Val_Relationships<<- 15.86
        Mean_Val_Mental_Health<<- 78.35
        Sd_Val_Mental_Health<<- 16.05
        Mean_Val_Coping<<- 76.3
        Sd_Val_Coping<<-18.19
        Mean_Val_Pain<<- 76.34
        Sd_Val_Pain<<-27.47
        Mean_Val_Senses<<- 84.2
        Sd_Val_Senses<<- 11.23
        Mean_Val_Self_Worth<<- 84.99
        Sd_Val_Self_Worth<<- 17.98
        Mean_Val_Happiness<<- 74.18
        Sd_Val_Happiness<<- 17.62
        Mean_Val_Super_Mental<<- 80.02
        Sd_Val_Super_Mental<<- 14.91
        Mean_Val_Super_Physical<<- 83.81
        Sd_Val_Super_Physical<<-  15.92
    } else if (input$Pop == "Males: General Population (65-74 yrs)") {
      Mean_Val<<- 82.92
        Sd_Val<<- 14.3
        Mean_Val_Independent_Living<<-88.62
        Sd_Val_Independent_Living<<- 19.10
        Mean_Val_Relationships<<- 86.73
        Sd_Val_Relationships<<- 15.59
        Mean_Val_Mental_Health<<- 80.7
        Sd_Val_Mental_Health<<- 16.35
        Mean_Val_Coping<<- 78.11
        Sd_Val_Coping<<- 19.5
        Mean_Val_Pain<<- 79.93
        Sd_Val_Pain<<- 26.38
        Mean_Val_Senses<<- 82.9
        Sd_Val_Senses<<- 13.67
        Mean_Val_Self_Worth<<- 87.4
        Sd_Val_Self_Worth<<- 19.4
        Mean_Val_Happiness<<- 76.8
        Sd_Val_Happiness<<- 18.66
        Mean_Val_Super_Mental<<- 82.2
        Sd_Val_Super_Mental<<- 15.36
        Mean_Val_Super_Physical<<- 84.69
        Sd_Val_Super_Physical<<- 15.66
    }  
      
      Source<<- "Maxwell, Ozmen, Iezzi & Richardson (2016)"
      Cut_Lab_1<<- "Mean - 2 Sd"
      Cut_Lab_2<<- "Mean - 1 Sd"
      Cut_Lab_3<<- "Mean"
      
      
      Cut_Val_1<<- Mean_Val - (2*Sd_Val)
      Cut_Val_2<<- Mean_Val - Sd_Val
      Cut_Val_3<<- Mean_Val
      

      Cut_Val_Independent_Living_1<<- Mean_Val_Independent_Living - (2*Sd_Val_Independent_Living)
      Cut_Val_Independent_Living_2<<- Mean_Val_Independent_Living - Sd_Val_Independent_Living
      Cut_Val_Independent_Living_3<<- Mean_Val_Independent_Living 

      
      Cut_Val_Relationships_1<<-Mean_Val_Relationships - (2*Sd_Val_Relationships)
      Cut_Val_Relationships_2<<- Mean_Val_Relationships - Sd_Val_Relationships
      Cut_Val_Relationships_3<<- Mean_Val_Relationships 

    
      Cut_Val_Mental_Health_1<<- Mean_Val_Mental_Health - (2*Sd_Val_Mental_Health)
      Cut_Val_Mental_Health_2<<- Mean_Val_Mental_Health - Sd_Val_Mental_Health
      Cut_Val_Mental_Health_3<<- Mean_Val_Mental_Health

     
      Cut_Val_Coping_1<<- Mean_Val_Coping - (2*Sd_Val_Coping)
      Cut_Val_Coping_2<<- Mean_Val_Coping - Sd_Val_Coping
      Cut_Val_Coping_3<<- Mean_Val_Coping
 
      
      Cut_Val_Pain_1<<- Mean_Val_Pain - (2*Sd_Val_Pain)
      Cut_Val_Pain_2<<- Mean_Val_Pain - Sd_Val_Pain
      Cut_Val_Pain_3<<- Mean_Val_Pain 

   
      Cut_Val_Senses_1<<- Mean_Val_Senses - (2*Sd_Val_Senses)
      Cut_Val_Senses_2<<- Mean_Val_Senses - Sd_Val_Senses
      Cut_Val_Senses_3<<- Mean_Val_Senses 

     
      Cut_Val_Self_Worth_1<<- Mean_Val_Self_Worth - (2*Sd_Val_Self_Worth) 
      Cut_Val_Self_Worth_2<<- Mean_Val_Self_Worth - Sd_Val_Self_Worth
      Cut_Val_Self_Worth_3<<- Mean_Val_Self_Worth 

      
      Cut_Val_Happiness_1<<- Mean_Val_Happiness - (2*Sd_Val_Happiness)
      Cut_Val_Happiness_2<<- Mean_Val_Happiness - Sd_Val_Happiness
      Cut_Val_Happiness_3<<- Mean_Val_Happiness

      
      Cut_Val_Super_Mental_1<<- Mean_Val_Super_Mental - (2*Sd_Val_Super_Mental)
      Cut_Val_Super_Mental_2<<- Mean_Val_Super_Mental - Sd_Val_Super_Mental
      Cut_Val_Super_Mental_3<<- Mean_Val_Super_Mental 

     
      Cut_Val_Super_Physical_1<<- Mean_Val_Super_Physical - (2*Sd_Val_Super_Physical)
      Cut_Val_Super_Physical_2<<- Mean_Val_Super_Physical - Sd_Val_Super_Physical
      Cut_Val_Super_Physical_3<<- Mean_Val_Super_Physical
     
    
  })
  
  
  output$Mean_Widg<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean", "AQOL-8D total scale", Mean_Val),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Mean_Widg", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Independent_Living<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Independent_Living", "Independent Living", Mean_Val_Independent_Living),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Mean_Widg_Independent_Living", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Relationships<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Relationships", "Relationships", Mean_Val_Relationships),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Mean_Widg_Relationships", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Mental_Health<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Mental_Health", "Mental Health", Mean_Val_Mental_Health),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Mean_Widg_Mental_Health", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Coping<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Coping", "Coping", Mean_Val_Coping),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Mean_Widg_Coping", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Pain<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Pain", "Pain", Mean_Val_Pain),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Mean_Widg_Pain", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Senses<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Senses", "Senses", Mean_Val_Senses),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Mean_Widg_Senses", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Self_Worth<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Self_Worth", "Self Worth", Mean_Val_Self_Worth),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Mean_Widg_Self_Worth", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Happiness<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Happiness", "Happiness", Mean_Val_Happiness),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Mean_Widg_Happiness", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Super_Mental<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Super_Mental", "Mental Super Dimension", Mean_Val_Super_Mental),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Mean_Widg_Super_Mental", suspendWhenHidden = FALSE) 
  
  output$Mean_Widg_Super_Physical<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Super_Physical", "Physical Super Dimension", Mean_Val_Super_Physical),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Mean_Widg_Super_Physical", suspendWhenHidden = FALSE) 
  

  output$Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd", "AQOL-8D total scale", Sd_Val),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Sd_Widg", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Independent_Living<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Independent_Living", "Independent Living", Sd_Val_Independent_Living),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Sd_Widg_Independent_Living", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Relationships<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Relationships", "Relationships", Sd_Val_Relationships),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Sd_Widg_Relationships", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Mental_Health<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Mental_Health", "Mental Health", Sd_Val_Mental_Health),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Sd_Widg_Mental_Health", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Coping<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Coping", "Coping", Sd_Val_Coping),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Sd_Widg_Coping", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Pain<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Pain", "Pain", Sd_Val_Pain),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Sd_Widg_Pain", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Senses<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Senses", "Senses", Sd_Val_Senses),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Sd_Widg_Senses", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Self_Worth<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Self_Worth", "Self Worth", Sd_Val_Self_Worth),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Sd_Widg_Self_Worth", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Happiness<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Happiness", "Happiness", Sd_Val_Happiness),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Sd_Widg_Happiness", suspendWhenHidden = FALSE)
  
  output$Sd_Widg_Super_Mental<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Super_Mental", "Mental Super Dimension", Sd_Val_Super_Mental),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Sd_Widg_Super_Mental", suspendWhenHidden = FALSE)
  
  output$Sd_Widg_Super_Physical<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Super_Physical", "Physical Super Dimension", Sd_Val_Super_Physical),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Sd_Widg_Super_Physical", suspendWhenHidden = FALSE)
  
  
  
  output$Cutoff_Widg_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_1", "AQOL-8D total scale", as.numeric(Cut_Val_1)),
      textInput("Cutoff_Text_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Independent_Living_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Independent_Living_1", "Independent Living", as.numeric(Cut_Val_Independent_Living_1)),
      textInput("Cutoff_Text_Independent_Living_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Independent_Living_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Relationships_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Relationships_1", "Relationships", as.numeric(Cut_Val_Relationships_1)),
      textInput("Cutoff_Text_Relationships_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Relationships_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Mental_Health_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Mental_Health_1", "Mental Health", as.numeric(Cut_Val_Mental_Health_1)),
      textInput("Cutoff_Text_Mental_Health_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Mental_Health_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Coping_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Coping_1", "Coping", as.numeric(Cut_Val_Coping_1)),
      textInput("Cutoff_Text_Coping_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Coping_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Pain_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Pain_1", "Pain", as.numeric(Cut_Val_Pain_1)),
      textInput("Cutoff_Text_Pain_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Pain_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Senses_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Senses_1", "Senses", as.numeric(Cut_Val_Senses_1)),
      textInput("Cutoff_Text_Senses_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Senses_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Self_Worth_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Self_Worth_1", "Self Worth", as.numeric(Cut_Val_Self_Worth_1)),
      textInput("Cutoff_Text_Self_Worth_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Self_Worth_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Happiness_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Happiness_1", "Happiness", as.numeric(Cut_Val_Happiness_1)),
      textInput("Cutoff_Text_Happiness_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Happiness_1", suspendWhenHidden = FALSE)
  
  output$Cutoff_Widg_Super_Mental_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Super_Mental_1", "Mental Super Dimension", as.numeric(Cut_Val_Super_Mental_1)),
      textInput("Cutoff_Text_Super_Mental_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Super_Mental_1", suspendWhenHidden = FALSE)
  
  output$Cutoff_Widg_Super_Physical_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Super_Physical_1", "Physical Super Dimension", as.numeric(Cut_Val_Super_Physical_1)),
      textInput("Cutoff_Text_Super_Physical_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Super_Physical_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_2", "AQOL-8D total scale", as.numeric(Cut_Val_2)),
      textInput("Cutoff_Text_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Independent_Living_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Independent_Living_2", "Independent Living", as.numeric(Cut_Val_Independent_Living_2)),
      textInput("Cutoff_Text_Independent_Living_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Independent_Living_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Relationships_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Relationships_2", "Relationships", as.numeric(Cut_Val_Relationships_2)),
      textInput("Cutoff_Text_Relationships_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Relationships_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Mental_Health_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Mental_Health_2", "Mental Health", as.numeric(Cut_Val_Mental_Health_2)),
      textInput("Cutoff_Text_Mental_Health_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Mental_Health_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Coping_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Coping_2", "Coping", as.numeric(Cut_Val_Coping_2)),
      textInput("Cutoff_Text_Coping_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Coping_2", suspendWhenHidden = FALSE)
  

  output$Cutoff_Widg_Pain_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Pain_2", "Pain", as.numeric(Cut_Val_Pain_2)),
      textInput("Cutoff_Text_Pain_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Pain_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Senses_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Senses_2", "Senses", as.numeric(Cut_Val_Senses_2)),
      textInput("Cutoff_Text_Senses_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Senses_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Self_Worth_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Self_Worth_2", "Self Worth", as.numeric(Cut_Val_Self_Worth_2)),
      textInput("Cutoff_Text_Self_Worth_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Self_Worth_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Happiness_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Happiness_2", "Happiness", as.numeric(Cut_Val_Happiness_2)),
      textInput("Cutoff_Text_Happiness_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Happiness_2", suspendWhenHidden = FALSE)
  
  output$Cutoff_Widg_Super_Mental_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Super_Mental_2", "Mental Super Dimension", as.numeric(Cut_Val_Super_Mental_2)),
      textInput("Cutoff_Text_Super_Mental_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Super_Mental_2", suspendWhenHidden = FALSE)
  
  output$Cutoff_Widg_Super_Physical_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Super_Physical_2", "Physical Super Dimension", as.numeric(Cut_Val_Super_Physical_2)),
      textInput("Cutoff_Text_Super_Physical_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Super_Physical_2", suspendWhenHidden = FALSE)
  

  output$Cutoff_Widg_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_3", "AQOL-8D total scale", as.numeric(Cut_Val_3)),
      textInput("Cutoff_Text_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Independent_Living_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Independent_Living_3", "Independent Living", as.numeric(Cut_Val_Independent_Living_3)),
      textInput("Cutoff_Text_Independent_Living_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Independent_Living_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Relationships_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Relationships_3", "Relationships", as.numeric(Cut_Val_Relationships_3)),
      textInput("Cutoff_Text_Relationships_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Relationships_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Mental_Health_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Mental_Health_3", "Mental Health", as.numeric(Cut_Val_Mental_Health_3)),
      textInput("Cutoff_Text_Mental_Health_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Mental_Health_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Coping_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Coping_3", "Coping", as.numeric(Cut_Val_Coping_3)),
      textInput("Cutoff_Text_Coping_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Coping_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Pain_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Pain_3", "Pain", as.numeric(Cut_Val_Pain_3)),
      textInput("Cutoff_Text_Pain_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Pain_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Senses_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Senses_3", "Senses", as.numeric(Cut_Val_Senses_3)),
      textInput("Cutoff_Text_Senses_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Senses_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Self_Worth_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Self_Worth_3", "Self Worth", as.numeric(Cut_Val_Self_Worth_3)),
      textInput("Cutoff_Text_Self_Worth_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Self_Worth_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Happiness_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Happiness_3", "Happiness", as.numeric(Cut_Val_Happiness_3)),
      textInput("Cutoff_Text_Happiness_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Happiness_3", suspendWhenHidden = FALSE)
  
  output$Cutoff_Widg_Super_Mental_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Super_Mental_3", "Mental Super Dimension", as.numeric(Cut_Val_Super_Mental_3)),
      textInput("Cutoff_Text_Super_Mental_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Super_Mental_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Super_Physical_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Super_Physical_3", "Physical Super Dimension", as.numeric(Cut_Val_Super_Physical_3)),
      textInput("Cutoff_Text_Super_Physical_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source))
    )
  })
  outputOptions(output, "Cutoff_Widg_Super_Physical_3", suspendWhenHidden = FALSE)
  
  
  
  
  #Create a reactive expression defining entered values  
  
  Entered_Scores_Reac<- reactive({
    
    TP<- input$Timepoint
    
    PN<<- input$PatientName
    CN<<- input$ClinicianName
    
    Pop<- input$Pop
    
    RelChangeMethod<- input$RelChangeMethod
    
    Tab_Reference<<- Source
    
    CI_Vals_Reac()
    
    M<- input$Pop_Mean
    SD<- input$Pop_Sd
    M_Independent_Living<- input$Pop_Mean_Independent_Living
    SD_Independent_Living<-input$Pop_Sd_Independent_Living
    M_Relationships<- input$Pop_Mean_Relationships
    SD_Relationships<- input$Pop_Sd_Relationships
    M_Mental_Health<- input$Pop_Mean_Mental_Health
    SD_Mental_Health<- input$Pop_Sd_Mental_Health
    M_Coping<- input$Pop_Mean_Coping
    SD_Coping<- input$Pop_Sd_Coping
    M_Pain<- input$Pop_Mean_Pain
    SD_Pain<-input$Pop_Sd_Pain
    M_Senses<- input$Pop_Mean_Senses
    SD_Senses<- input$Pop_Sd_Senses
    M_Self_Worth<- input$Pop_Mean_Self_Worth
    SD_Self_Worth<- input$Pop_Sd_Self_Worth
    M_Happiness<- input$Pop_Mean_Happiness
    SD_Happiness<- input$Pop_Sd_Happiness
    M_Super_Mental<- input$Pop_Mean_Super_Mental
    SD_Super_Mental<- input$Pop_Sd_Super_Mental
    M_Super_Physical<- input$Pop_Mean_Super_Physical
    SD_Super_Physical<- input$Pop_Sd_Super_Physical
    
    
    M_Retest<- input$Retest_Mean
    SD_Retest<- input$Retest_Sd
    M_Retest_Independent_Living<- input$Retest_Mean_Independent_Living
    SD_Retest_Independent_Living<- input$Retest_Sd_Independent_Living
    M_Retest_Relationships<- input$Retest_Mean_Relationships
    SD_Retest_Relationships<- input$Retest_Sd_Relationships
    M_Retest_Mental_Health<- input$Retest_Mean_Mental_Health
    SD_Retest_Mental_Health<- input$Retest_Sd_Mental_Health
    M_Retest_Coping<- input$Retest_Mean_Coping
    SD_Retest_Coping<- input$Retest_Sd_Coping
    M_Retest_Pain<- input$Retest_Mean_Pain
    SD_Retest_Pain<- input$Retest_Sd_Pain
    M_Retest_Senses<- input$Retest_Mean_Senses
    SD_Retest_Senses<- input$Retest_Sd_Senses
    M_Retest_Self_Worth<- input$Retest_Mean_Self_Worth
    SD_Retest_Self_Worth<- input$Retest_Sd_Self_Worth
    M_Retest_Happiness<- input$Retest_Mean_Happiness
    SD_Retest_Happiness<- input$Retest_Sd_Happiness
    M_Retest_Super_Mental<- input$Retest_Mean_Super_Mental
    SD_Retest_Super_Mental<- input$Retest_Sd_Super_Mental
    M_Retest_Super_Physical<- input$Retest_Mean_Super_Physical
    SD_Retest_Super_Physical<- input$Retest_Sd_Super_Physical
    
    SampleN<- input$SampleN
    
    Rel<- input$Reliability
    Rel_Independent_Living<- input$Reliability_Independent_Living
    Rel_Relationships<- input$Reliability_Relationships
    Rel_Mental_Health<- input$Reliability_Mental_Health
    Rel_Coping<- input$Reliability_Coping
    Rel_Pain<- input$Reliability_Pain
    Rel_Senses<- input$Reliability_Senses
    Rel_Self_Worth<- input$Reliability_Self_Worth
    Rel_Happiness<- input$Reliability_Happiness
    Rel_Super_Mental<- input$Reliability_Super_Mental
    Rel_Super_Physical<- input$Reliability_Super_Physical
    
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
      SE_Independent_Living<-SD_Independent_Living * sqrt(1 - Rel_Independent_Living^2)
      SE_Relationships<-SD_Relationships * sqrt(1 - Rel_Relationships^2)
      SE_Mental_Health<-SD_Mental_Health * sqrt(1 - Rel_Mental_Health^2)
      SE_Coping<-SD_Coping * sqrt(1 - Rel_Coping^2)
      SE<- round(SE, digits = 2)
      SE_Independent_Living<- round(SE_Independent_Living, digits = 2)
      SE_Relationships<- round(SE_Relationships, digits = 2)
      SE_Mental_Health<- round(SE_Mental_Health, digits = 2)
      SE_Coping<- round(SE_Coping, digits = 2)
      SE_Pain<-SD_Pain * sqrt(1 - Rel_Pain^2)
      SE_Senses<-SD_Senses * sqrt(1 - Rel_Senses^2)
      SE_Self_Worth<-SD_Self_Worth * sqrt(1 - Rel_Self_Worth^2)
      SE_Happiness<-SD_Happiness * sqrt(1 - Rel_Happiness^2)
      SE_Super_Mental<- SD_Super_Mental * sqrt(1 - Rel_Super_Mental^2)
      SE_Super_Physical<- SD_Super_Physical * sqrt(1 - Rel_Super_Physical^2)
      SE_Pain<- round(SE_Pain, digits = 2)
      SE_Senses<- round(SE_Senses, digits = 2)
      SE_Self_Worth<- round(SE_Self_Worth, digits = 2)
      SE_Happiness<- round(SE_Happiness, digits = 2)
      SE_Super_Mental<- round(SE_Super_Mental, digits = 2)
      SE_Super_Physical<- round(SE_Super_Physical, digits = 2)
    } else if (input$RelChangeMethod == "Chelune et al. (1993)" | input$RelChangeMethod == "Jacobson & Truax (1991)" | input$RelChangeMethod == "Speer (1992)") {
      SE<- sqrt((2*(SD^2))*(1-Rel))
      SE_Independent_Living<- sqrt((2*(SD_Independent_Living^2))*(1-Rel_Independent_Living))
      SE_Relationships<- sqrt((2*(SD_Relationships^2))*(1-Rel_Relationships))
      SE_Mental_Health<- sqrt((2*(SD_Mental_Health^2))*(1-Rel_Mental_Health))
      SE_Coping<- sqrt((2*(SD_Coping^2))*(1-Rel_Coping))
      SE_Pain<- sqrt((2*(SD_Pain^2))*(1-Rel_Pain))
      SE_Senses<- sqrt((2*(SD_Senses^2))*(1-Rel_Senses))
      SE_Self_Worth<- sqrt((2*(SD_Self_Worth^2))*(1-Rel_Self_Worth))
      SE_Happiness<- sqrt((2*(SD_Happiness^2))*(1-Rel_Happiness))
      SE_Super_Mental<- sqrt((2*(SD_Super_Mental^2))*(1-Rel_Super_Mental))
      SE_Super_Physical<- sqrt((2*(SD_Super_Physical^2))*(1-Rel_Super_Physical))
      SE<- round(SE, digits = 2)
      SE_Independent_Living<- round(SE_Independent_Living, digits = 2)
      SE_Relationships<- round(SE_Relationships, digits = 2)
      SE_Mental_Health<- round(SE_Mental_Health, digits = 2)
      SE_Coping<- round(SE_Coping, digits = 2)
      SE_Pain<- round(SE_Pain, digits = 2)
      SE_Senses<- round(SE_Senses, digits = 2)
      SE_Self_Worth<- round(SE_Self_Worth, digits = 2)
      SE_Happiness<- round(SE_Happiness, digits = 2)
      SE_Super_Mental<- round(SE_Super_Mental, digits = 2)
      SE_Super_Physical<- round(SE_Super_Physical, digits = 2)
    } else if (input$RelChangeMethod == "Maassen et al. (2006)") {
      SE<- sqrt((SD^2 + SD_Retest^2)*(1-Rel))
      SE_Independent_Living<- sqrt((SD_Independent_Living^2 + SD_Retest_Independent_Living^2)*(1-Rel_Independent_Living))
      SE_Relationships<- sqrt((SD_Relationships^2 + SD_Retest_Relationships^2)*(1-Rel_Relationships))
      SE_Mental_Health<- sqrt((SD_Mental_Health^2 + SD_Retest_Mental_Health^2)*(1-Rel_Mental_Health))
      SE_Coping<- sqrt((SD_Coping^2 + SD_Retest_Coping^2)*(1-Rel_Coping))
      SE<- round(SE, digits = 2)
      SE_Independent_Living<- round(SE_Independent_Living, digits = 2)
      SE_Relationships<- round(SE_Relationships, digits = 2)
      SE_Mental_Health<- round(SE_Mental_Health, digits = 2)
      SE_Coping<- round(SE_Coping, digits = 2)
      SE_Pain<- sqrt((SD_Pain^2 + SD_Retest_Pain^2)*(1-Rel_Pain))
      SE_Senses<- sqrt((SD_Senses^2 + SD_Retest_Senses^2)*(1-Rel_Senses))
      SE_Self_Worth<- sqrt((SD_Self_Worth^2 + SD_Retest_Self_Worth^2)*(1-Rel_Self_Worth))
      SE_Happiness<- sqrt((SD_Happiness^2 + SD_Retest_Happiness^2)*(1-Rel_Happiness))
      SE_Super_Mental<- sqrt((SD_Super_Mental^2 + SD_Retest_Super_Mental^2)*(1-Rel_Super_Mental))
      SE_Super_Physical<- sqrt((SD_Super_Physical^2 + SD_Retest_Super_Physical^2)*(1-Rel_Super_Physical))
      SE<- round(SE, digits = 2)
      SE_Pain<- round(SE_Pain, digits = 2)
      SE_Senses<- round(SE_Senses, digits = 2)
      SE_Self_Worth<- round(SE_Self_Worth, digits = 2)
      SE_Happiness<- round(SE_Happiness, digits = 2)
      SE_Super_Mental<- round(SE_Super_Mental, digits = 2)
      SE_Super_Physical<- round(SE_Super_Physical, digits = 2)
    } else if (input$RelChangeMethod == "McSweeny et al. (1993)") {
      SE<- SD_Retest*sqrt(1 - Rel^2)
      SE_Independent_Living<- SD_Retest_Independent_Living*sqrt(1 - Rel_Independent_Living^2)
      SE_Relationships<- SD_Retest_Relationships*sqrt(1 - Rel_Relationships^2)
      SE_Mental_Health<- SD_Retest_Mental_Health*sqrt(1 - Rel_Mental_Health^2)
      SE_Coping<- SD_Retest_Coping*sqrt(1 - Rel_Coping^2)
      SE<- round(SE, digits = 2)
      SE_Independent_Living<- round(SE_Independent_Living, digits = 2)
      SE_Relationships<- round(SE_Relationships, digits = 2)
      SE_Mental_Health<- round(SE_Mental_Health, digits = 2)
      SE_Coping<- round(SE_Coping, digits = 2)
      SE_Pain<- SD_Retest_Pain*sqrt(1 - Rel_Pain^2)
      SE_Senses<- SD_Retest_Senses*sqrt(1 - Rel_Senses^2)
      SE_Self_Worth<- SD_Retest_Self_Worth*sqrt(1 - Rel_Self_Worth^2)
      SE_Happiness<- SD_Retest_Happiness*sqrt(1 - Rel_Happiness^2)
      SE_Super_Mental<- SD_Retest_Super_Mental*sqrt(1 - Rel_Super_Mental^2)
      SE_Super_Physical<- SD_Retest_Super_Physical*sqrt(1 - Rel_Super_Physical^2)
      SE<- round(SE, digits = 2)
      SE_Pain<- round(SE_Pain, digits = 2)
      SE_Senses<- round(SE_Senses, digits = 2)
      SE_Self_Worth<- round(SE_Self_Worth, digits = 2)
      SE_Happiness<- round(SE_Happiness, digits = 2)
      SE_Super_Mental<- round(SE_Super_Mental, digits = 2)
      SE_Super_Physical<- round(SE_Super_Physical, digits = 2)
    }
    
    #Caclculate McSweeny standard error to be employed in deriving the Crawford & Howell standard error if selected
    
    McSweeny_SE<- SD_Retest*sqrt(1 - Rel^2)
    McSweeny_SE_Independent_Living<- SD_Retest_Independent_Living*sqrt(1 - Rel_Independent_Living^2)
    McSweeny_SE_Relationships<- SD_Retest_Relationships*sqrt(1 - Rel_Relationships^2)
    McSweeny_SE_Mental_Health<- SD_Retest_Mental_Health*sqrt(1 - Rel_Mental_Health^2)
    McSweeny_SE_Coping<- SD_Retest_Coping*sqrt(1 - Rel_Coping^2)
    McSweeny_SE_Pain<- SD_Retest_Pain*sqrt(1 - Rel_Pain^2)
    McSweeny_SE_Senses<- SD_Retest_Senses*sqrt(1 - Rel_Senses^2)
    McSweeny_SE_Self_Worth<- SD_Retest_Self_Worth*sqrt(1 - Rel_Self_Worth^2)
    McSweeny_SE_Happiness<- SD_Retest_Happiness*sqrt(1 - Rel_Happiness^2)
    McSweeny_SE_Super_Mental<- SD_Retest_Super_Mental*sqrt(1 - Rel_Super_Mental^2)
    McSweeny_SE_Super_Physical<- SD_Retest_Super_Physical*sqrt(1 - Rel_Super_Physical^2)
    
    #Generate object storing names of cut-off scores
    
    Cutoff_Name_1<- input$Cutoff_Text_1
    Cutoff_Name_2<- input$Cutoff_Text_2
    Cutoff_Name_3<- input$Cutoff_Text_3
    Cutoff_Name_Independent_Living_1<- input$Cutoff_Text_Independent_Living_1
    Cutoff_Name_Independent_Living_2<- input$Cutoff_Text_Independent_Living_2
    Cutoff_Name_Independent_Living_3<- input$Cutoff_Text_Independent_Living_3
    Cutoff_Name_Relationships_1<- input$Cutoff_Text_Relationships_1
    Cutoff_Name_Relationships_2<- input$Cutoff_Text_Relationships_2
    Cutoff_Name_Relationships_3<- input$Cutoff_Text_Relationships_3
    Cutoff_Name_Mental_Health_1<- input$Cutoff_Text_Mental_Health_1
    Cutoff_Name_Mental_Health_2<- input$Cutoff_Text_Mental_Health_2
    Cutoff_Name_Mental_Health_3<- input$Cutoff_Text_Mental_Health_3
    Cutoff_Name_Coping_1<- input$Cutoff_Text_Coping_1
    Cutoff_Name_Coping_2<- input$Cutoff_Text_Coping_2
    Cutoff_Name_Coping_3<- input$Cutoff_Text_Coping_3
    Cutoff_Name_Pain_1<- input$Cutoff_Text_Pain_1
    Cutoff_Name_Pain_2<- input$Cutoff_Text_Pain_2
    Cutoff_Name_Pain_3<- input$Cutoff_Text_Pain_3
    Cutoff_Name_Senses_1<- input$Cutoff_Text_Senses_1
    Cutoff_Name_Senses_2<- input$Cutoff_Text_Senses_2
    Cutoff_Name_Senses_3<- input$Cutoff_Text_Senses_3
    Cutoff_Name_Self_Worth_1<- input$Cutoff_Text_Self_Worth_1
    Cutoff_Name_Self_Worth_2<- input$Cutoff_Text_Self_Worth_2
    Cutoff_Name_Self_Worth_3<- input$Cutoff_Text_Self_Worth_3
    Cutoff_Name_Happiness_1<- input$Cutoff_Text_Happiness_1
    Cutoff_Name_Happiness_2<- input$Cutoff_Text_Happiness_2
    Cutoff_Name_Happiness_3<- input$Cutoff_Text_Happiness_3
    Cutoff_Name_Super_Mental_1<- input$Cutoff_Text_Super_Mental_1
    Cutoff_Name_Super_Mental_2<- input$Cutoff_Text_Super_Mental_2
    Cutoff_Name_Super_Mental_3<- input$Cutoff_Text_Super_Mental_3
    Cutoff_Name_Super_Physical_1<- input$Cutoff_Text_Super_Physical_1
    Cutoff_Name_Super_Physical_2<- input$Cutoff_Text_Super_Physical_2
    Cutoff_Name_Super_Physical_3<- input$Cutoff_Text_Super_Physical_3
    
    Cutoff_Names<<- data.frame(Cutoff_Name_1,Cutoff_Name_2,Cutoff_Name_3,Cutoff_Name_Independent_Living_1,Cutoff_Name_Independent_Living_2,Cutoff_Name_Independent_Living_3,
                               Cutoff_Name_Relationships_1, Cutoff_Name_Relationships_2, Cutoff_Name_Relationships_3, Cutoff_Name_Mental_Health_1,
                               Cutoff_Name_Mental_Health_2, Cutoff_Name_Mental_Health_3, Cutoff_Name_Coping_1, Cutoff_Name_Coping_2, Cutoff_Name_Coping_3,
                               Cutoff_Name_Pain_1,Cutoff_Name_Pain_2,Cutoff_Name_Pain_3,
                               Cutoff_Name_Senses_1, Cutoff_Name_Senses_2, Cutoff_Name_Senses_3, Cutoff_Name_Self_Worth_1,
                               Cutoff_Name_Self_Worth_2, Cutoff_Name_Self_Worth_3, Cutoff_Name_Happiness_1, Cutoff_Name_Happiness_2, Cutoff_Name_Happiness_3, 
                               Cutoff_Name_Super_Mental_1, Cutoff_Name_Super_Mental_2, Cutoff_Name_Super_Mental_3 ,
                               Cutoff_Name_Super_Physical_1, Cutoff_Name_Super_Physical_2, Cutoff_Name_Super_Physical_3 
                               )
    
    #Define the data & calculate PS, SE, confidence intervals and cut-off scores
    
    if(input$Timepoint == "1") {
      Date<- input$Date_1
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_1a), Score = Score_1a)
      Score<- sum(Score_1a, na.rm = TRUE)
      Score<- (1-(Score - 35)/(176-35))*100
      Score<- round(Score, digits = 2)
      Score_Independent_Living<- sum(Score_1a[c(30,3,15,9)], na.rm = TRUE)
      Score_Independent_Living<- (1-(Score_Independent_Living- 4)/(22-4))*100
      Score_Independent_Living<- round(Score_Independent_Living, digits = 2)
      Score_Relationships<- sum(Score_1a[c(23,10,31,2,34,9,4)], na.rm = TRUE)
      Score_Relationships<- (1-(Score_Relationships - 7)/(34-7))*100
      Score_Relationships<- round(Score_Relationships, digits = 2)
      Score_Mental_Health<- sum(Score_1a[c(33,12,14,16,35,18,5,8)], na.rm = TRUE)
      Score_Mental_Health<- (1-(Score_Mental_Health - 8)/(41-8))*100
      Score_Mental_Health<- round(Score_Mental_Health, digits = 2)
      Score_Coping<- sum(Score_1a[c(1,29,21)], na.rm = TRUE)
      Score_Coping<- (1-(Score_Coping - 3)/(15-3))*100
      Score_Coping<- round(Score_Coping, digits = 2)
      Score_Pain<- sum(Score_1a[c(6,22,24)], na.rm = TRUE)
      Score_Pain<- (1-(Score_Pain - 3)/(13-3))*100
      Score_Pain<- round(Score_Pain, digits = 2)
      Score_Senses<- sum(Score_1a[c(28,32,11)], na.rm = TRUE)
      Score_Senses<- (1-(Score_Senses - 3)/(16-3))*100
      Score_Senses<- round(Score_Senses, digits = 2)
      Score_Self_Worth<- sum(Score_1a[c(26,13,7)], na.rm = TRUE)
      Score_Self_Worth<- (1-(Score_Self_Worth - 3)/(15-3))*100
      Score_Self_Worth<- round(Score_Self_Worth, digits = 2)
      Score_Happiness<- sum(Score_1a[c(27,17,20,25)], na.rm = TRUE)
      Score_Happiness<- (1-(Score_Happiness - 4)/(20-4))*100
      Score_Happiness<- round(Score_Happiness, digits = 2)
      Score_Super_Mental<- sum(Score_1a[c(33,12,14,16,35,18,5,8,27,17,20,25,1,29,21,23,10,31,2,34,9,4,26,13,7)], na.rm = TRUE)
      Score_Super_Mental<- (1-(Score_Super_Mental - 25)/(125-25))*100
      Score_Super_Mental<- round(Score_Super_Mental, digits = 2)
      Score_Super_Physical<- sum(Score_1a[c(30,3,15,9,6,22,24,28,32,11)], na.rm = TRUE) 
      Score_Super_Physical<- (1-(Score_Super_Physical - 10)/(51-10))*100
      Score_Super_Physical<- round(Score_Super_Physical, digits = 2)
      Change<- 0
      Change_Independent_Living<- 0
      Change_Relationships<- 0
      Change_Mental_Health<- 0
      Change_Coping<- 0
      Change_Pain<- 0
      Change_Senses<- 0
      Change_Self_Worth<- 0
      Change_Happiness<- 0
      Change_Super_Mental<- 0
      Change_Super_Physical<- 0
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS<- (Rel * Score) + (M * (1 - Rel))
        PTS_Independent_Living<- (Rel_Independent_Living * Score_Independent_Living) + (M_Independent_Living * (1 - Rel_Independent_Living))
        PTS_Relationships<- (Rel_Relationships * Score_Relationships) + (M_Relationships * (1 - Rel_Relationships))
        PTS_Mental_Health<- (Rel_Mental_Health * Score_Mental_Health) + (M_Mental_Health * (1 - Rel_Mental_Health))
        PTS_Coping<- (Rel_Coping * Score_Coping) + (M_Coping * (1 - Rel_Coping))
        PTS_Pain<- (Rel_Pain * Score_Pain) + (M_Pain * (1 - Rel_Pain))
        PTS_Senses<- (Rel_Senses * Score_Senses) + (M_Senses * (1 - Rel_Senses))
        PTS_Self_Worth<- (Rel_Self_Worth * Score_Self_Worth) + (M_Self_Worth * (1 - Rel_Self_Worth))
        PTS_Happiness<- (Rel_Happiness * Score_Happiness) + (M_Happiness * (1 - Rel_Happiness))
        PTS_Super_Mental<- (Rel_Super_Mental * Score_Super_Mental) + (M_Super_Mental * (1 - Rel_Super_Mental))
        PTS_Super_Physical<- (Rel_Super_Physical * Score_Super_Physical) + (M_Super_Physical * (1 - Rel_Super_Physical))
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS<- Score + (M_Retest - M)  
        PTS_Independent_Living<- Score_Independent_Living + (M_Retest_Independent_Living - M_Independent_Living)  
        PTS_Relationships<- Score_Relationships + (M_Retest_Relationships - M_Relationships)  
        PTS_Mental_Health<- Score_Mental_Health + (M_Retest_Mental_Health - M_Mental_Health) 
        PTS_Coping<- Score_Coping + (M_Retest_Coping - M_Coping)
        PTS_Pain<- Score_Pain + (M_Retest_Pain - M_Pain)  
        PTS_Senses<- Score_Senses + (M_Retest_Senses - M_Senses)  
        PTS_Self_Worth<- Score_Self_Worth + (M_Retest_Self_Worth - M_Self_Worth) 
        PTS_Happiness<- Score_Happiness + (M_Retest_Happiness - M_Happiness) 
        PTS_Super_Mental<- Score_Super_Mental + (M_Retest_Super_Mental - M_Super_Mental) 
        PTS_Super_Physical<- Score_Super_Physical + (M_Retest_Super_Physical - M_Super_Physical) 
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Independent_Living<- Score_Independent_Living
        PTS_Relationships<- Score_Relationships
        PTS_Mental_Health<- Score_Mental_Health
        PTS_Coping<- Score_Coping
        PTS_Pain<- Score_Pain
        PTS_Senses<- Score_Senses
        PTS_Self_Worth<- Score_Self_Worth
        PTS_Happiness<- Score_Happiness
        PTS_Super_Mental<- Score_Super_Mental
        PTS_Super_Physical<- Score_Super_Physical
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS<- (B_Adj * Score) + A_Adj
        B_Slope_Independent_Living<- Rel_Independent_Living * (SD_Retest_Independent_Living/SD_Independent_Living)
        A_Constant_Independent_Living<- M_Retest_Independent_Living - (B_Slope_Independent_Living * M_Independent_Living)
        B_Adj_Independent_Living<- SD_Retest_Independent_Living/SD_Independent_Living
        A_Adj_Independent_Living<- M_Retest_Independent_Living - (B_Adj_Independent_Living * M_Independent_Living)
        PTS_Independent_Living<- (B_Adj_Independent_Living * Score_Independent_Living) + A_Adj_Independent_Living
        B_Slope_Relationships<- Rel_Relationships * (SD_Retest_Relationships/SD_Relationships)
        A_Constant_Relationships<- M_Retest_Relationships - (B_Slope_Relationships * M_Relationships)
        B_Adj_Relationships<- SD_Retest_Relationships/SD_Relationships
        A_Adj_Relationships<- M_Retest_Relationships - (B_Adj_Relationships * M_Relationships)
        PTS_Relationships<- (B_Adj_Relationships * Score_Relationships) + A_Adj_Relationships
        B_Slope_Mental_Health<- Rel_Mental_Health * (SD_Retest_Mental_Health/SD_Mental_Health)
        A_Constant_Mental_Health<- M_Retest_Mental_Health - (B_Slope_Mental_Health * M_Mental_Health)
        B_Adj_Mental_Health<- SD_Retest_Mental_Health/SD_Mental_Health
        A_Adj_Mental_Health<- M_Retest_Mental_Health - (B_Adj_Mental_Health * M_Mental_Health)
        PTS_Mental_Health<- (B_Adj_Mental_Health * Score_Mental_Health) + A_Adj_Mental_Health
        B_Slope_Coping<- Rel_Coping * (SD_Retest_Coping/SD_Coping)
        A_Constant_Coping<- M_Retest_Coping - (B_Slope_Coping * M_Coping)
        B_Adj_Coping<- SD_Retest_Coping/SD_Coping
        A_Adj_Coping<- M_Retest_Coping - (B_Adj_Coping * M_Coping)
        PTS_Coping<- (B_Adj_Coping * Score_Coping) + A_Adj_Coping
        B_Slope_Pain<- Rel_Pain * (SD_Retest_Pain/SD_Pain)
        A_Constant_Pain<- M_Retest_Pain - (B_Slope_Pain * M_Pain)
        B_Adj_Pain<- SD_Retest_Pain/SD_Pain
        A_Adj_Pain<- M_Retest_Pain - (B_Adj_Pain * M_Pain)
        PTS_Pain<- (B_Adj_Pain * Score_Pain) + A_Adj_Pain
        B_Slope_Senses<- Rel_Senses * (SD_Retest_Senses/SD_Senses)
        A_Constant_Senses<- M_Retest_Senses - (B_Slope_Senses * M_Senses)
        B_Adj_Senses<- SD_Retest_Senses/SD_Senses
        A_Adj_Senses<- M_Retest_Senses - (B_Adj_Senses * M_Senses)
        PTS_Senses<- (B_Adj_Senses * Score_Senses) + A_Adj_Senses
        B_Slope_Self_Worth<- Rel_Self_Worth * (SD_Retest_Self_Worth/SD_Self_Worth)
        A_Constant_Self_Worth<- M_Retest_Self_Worth - (B_Slope_Self_Worth * M_Self_Worth)
        B_Adj_Self_Worth<- SD_Retest_Self_Worth/SD_Self_Worth
        A_Adj_Self_Worth<- M_Retest_Self_Worth - (B_Adj_Self_Worth * M_Self_Worth)
        PTS_Self_Worth<- (B_Adj_Self_Worth * Score_Self_Worth) + A_Adj_Self_Worth
        B_Slope_Happiness<- Rel_Happiness * (SD_Retest_Happiness/SD_Happiness)
        A_Constant_Happiness<- M_Retest_Happiness - (B_Slope_Happiness * M_Happiness)
        B_Adj_Happiness<- SD_Retest_Happiness/SD_Happiness
        A_Adj_Happiness<- M_Retest_Happiness - (B_Adj_Happiness * M_Happiness)
        PTS_Happiness<- (B_Adj_Happiness * Score_Happiness) + A_Adj_Happiness
        B_Slope_Super_Mental<- Rel_Super_Mental * (SD_Retest_Super_Mental/SD_Super_Mental)
        A_Constant_Super_Mental<- M_Retest_Super_Mental - (B_Slope_Super_Mental * M_Super_Mental)
        B_Adj_Super_Mental<- SD_Retest_Super_Mental/SD_Super_Mental
        A_Adj_Super_Mental<- M_Retest_Super_Mental - (B_Adj_Super_Mental * M_Super_Mental)
        PTS_Super_Mental<- (B_Adj_Super_Mental * Score_Super_Mental) + A_Adj_Super_Mental
        B_Slope_Super_Physical<- Rel_Super_Physical * (SD_Retest_Super_Physical/SD_Super_Physical)
        A_Constant_Super_Physical<- M_Retest_Super_Physical - (B_Slope_Super_Physical * M_Super_Physical)
        B_Adj_Super_Physical<- SD_Retest_Super_Physical/SD_Super_Physical
        A_Adj_Super_Physical<- M_Retest_Super_Physical - (B_Adj_Super_Physical * M_Super_Physical)
        PTS_Super_Physical<- (B_Adj_Super_Physical * Score_Super_Physical) + A_Adj_Super_Physical
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS<- B_Slope * Score
        B_Slope_Independent_Living<- Rel_Independent_Living * (SD_Retest_Independent_Living/SD_Independent_Living)
        PTS_Independent_Living<- B_Slope_Independent_Living * Score_Independent_Living
        B_Slope_Relationships<- Rel_Relationships * (SD_Retest_Relationships/SD_Relationships)
        PTS_Relationships<- B_Slope_Relationships * Score_Relationships
        B_Slope_Mental_Health<- Rel_Mental_Health * (SD_Retest_Mental_Health/SD_Mental_Health)
        PTS_Mental_Health<- B_Slope_Mental_Health * Score_Mental_Health
        B_Slope_Coping<- Rel_Coping * (SD_Retest_Coping/SD_Coping)
        PTS_Coping<- B_Slope_Coping * Score_Coping
        B_Slope_Pain<- Rel_Pain * (SD_Retest_Pain/SD_Pain)
        PTS_Pain<- B_Slope_Pain * Score_Pain
        B_Slope_Senses<- Rel_Senses * (SD_Retest_Senses/SD_Senses)
        PTS_Senses<- B_Slope_Senses * Score_Senses
        B_Slope_Self_Worth<- Rel_Self_Worth * (SD_Retest_Self_Worth/SD_Self_Worth)
        PTS_Self_Worth<- B_Slope_Self_Worth * Score_Self_Worth
        B_Slope_Happiness<- Rel_Happiness * (SD_Retest_Happiness/SD_Happiness)
        PTS_Happiness<- B_Slope_Happiness * Score_Happiness 
        B_Slope_Super_Mental<- Rel_Super_Mental * (SD_Retest_Super_Mental/SD_Super_Mental)
        PTS_Super_Mental<- B_Slope_Super_Mental * Score_Super_Mental
        B_Slope_Super_Physical<- Rel_Super_Physical * (SD_Retest_Super_Physical/SD_Super_Physical)
        PTS_Super_Physical<- B_Slope_Super_Physical * Score_Super_Physical
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS<- Score + (M_Retest - M)
        PTS_Independent_Living<- Score_Independent_Living + (M_Retest_Independent_Living - M_Independent_Living)
        PTS_Relationships<- Score_Relationships + (M_Retest_Relationships - M_Relationships)
        PTS_Mental_Health<- Score_Mental_Health + (M_Retest_Mental_Health - M_Mental_Health)
        PTS_Coping<- Score_Coping + (M_Retest_Coping - M_Coping)
        PTS_Pain<- Score_Pain + (M_Retest_Pain - M_Pain)
        PTS_Senses<- Score_Senses + (M_Retest_Senses - M_Senses)
        PTS_Self_Worth<- Score_Self_Worth + (M_Retest_Self_Worth - M_Self_Worth)
        PTS_Happiness<- Score_Happiness + (M_Retest_Happiness - M_Happiness)
        PTS_Super_Mental<- Score_Super_Mental + (M_Retest_Super_Mental - M_Super_Mental)
        PTS_Super_Physical<- Score_Super_Physical + (M_Retest_Super_Physical - M_Super_Physical)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Independent_Living<- round(PTS_Independent_Living, digits = 2)
      PTS_Relationships<- round(PTS_Relationships, digits = 2)
      PTS_Mental_Health<- round(PTS_Mental_Health, digits = 2)
      PTS_Coping<- round(PTS_Coping, digits = 2)
      PTS_Pain<- round(PTS_Pain, digits = 2)
      PTS_Senses<- round(PTS_Senses, digits = 2)
      PTS_Self_Worth<- round(PTS_Self_Worth, digits = 2)
      PTS_Happiness<- round(PTS_Happiness, digits = 2)
      PTS_Super_Mental<- round(PTS_Super_Mental, digits = 2)
      PTS_Super_Physical<- round(PTS_Super_Physical, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score - M)^2/(SD^2*(SampleN-1))))
        SE_Independent_Living<- McSweeny_SE_Independent_Living*sqrt(1 + (1/SampleN) + ((Score_Independent_Living - M_Independent_Living)^2/(SD_Independent_Living^2*(SampleN-1))))
        SE_Relationships<- McSweeny_SE_Relationships*sqrt(1 + (1/SampleN) + ((Score_Relationships - M_Relationships)^2/(SD_Relationships^2*(SampleN-1))))
        SE_Mental_Health<- McSweeny_SE_Mental_Health*sqrt(1 + (1/SampleN) + ((Score_Mental_Health - M_Mental_Health)^2/(SD_Mental_Health^2*(SampleN-1))))
        SE_Coping<- McSweeny_SE_Coping*sqrt(1 + (1/SampleN) + ((Score_Coping - M_Coping)^2/(SD_Coping^2*(SampleN-1))))
        SE_Pain<- McSweeny_SE_Pain*sqrt(1 + (1/SampleN) + ((Score_Pain - M_Pain)^2/(SD_Pain^2*(SampleN-1))))
        SE_Senses<- McSweeny_SE_Senses*sqrt(1 + (1/SampleN) + ((Score_Senses - M_Senses)^2/(SD_Senses^2*(SampleN-1))))
        SE_Self_Worth<- McSweeny_SE_Self_Worth*sqrt(1 + (1/SampleN) + ((Score_Self_Worth - M_Self_Worth)^2/(SD_Self_Worth^2*(SampleN-1))))
        SE_Happiness<- McSweeny_SE_Happiness*sqrt(1 + (1/SampleN) + ((Score_Happiness - M_Happiness)^2/(SD_Happiness^2*(SampleN-1))))
        SE_Super_Mental<- McSweeny_SE_Super_Mental*sqrt(1 + (1/SampleN) + ((Score_Super_Mental - M_Super_Mental)^2/(SD_Super_Mental^2*(SampleN-1))))
        SE_Super_Physical<- McSweeny_SE_Super_Physical*sqrt(1 + (1/SampleN) + ((Score_Super_Physical - M_Super_Physical)^2/(SD_Super_Physical^2*(SampleN-1))))
        SE<- round(SE, digits = 2)
        SE_Independent_Living<- round(SE_Independent_Living, digits = 2)
        SE_Relationships<- round(SE_Relationships, digits = 2)
        SE_Mental_Health<- round(SE_Mental_Health, digits = 2)
        SE_Coping<- round(SE_Coping, digits = 2)
        SE_Pain<- round(SE_Pain, digits = 2)
        SE_Senses<- round(SE_Senses, digits = 2)
        SE_Self_Worth<- round(SE_Self_Worth, digits = 2)
        SE_Happiness<- round(SE_Happiness, digits = 2)
        SE_Super_Mental<- round(SE_Super_Mental, digits = 2)
        SE_Super_Physical<- round(SE_Super_Physical, digits = 2)
        CI<- (Conf*SE)
        CI<- round(CI, digits = 2)
        CI_Independent_Living<- (Conf*SE_Independent_Living)
        CI_Independent_Living<- round(CI_Independent_Living, digits = 2)
        CI_Relationships<- (Conf*SE_Relationships)
        CI_Relationships<- round(CI_Relationships, digits = 2)
        CI_Mental_Health<- (Conf*SE_Mental_Health)
        CI_Mental_Health<- round(CI_Mental_Health, digits = 2)
        CI_Coping<- (Conf*SE_Coping)
        CI_Coping<- round(CI_Coping, digits = 2)
        CI_Pain<- (Conf*SE_Pain)
        CI_Pain<- round(CI_Pain, digits = 2)
        CI_Senses<- (Conf*SE_Senses)
        CI_Senses<- round(CI_Senses, digits = 2)
        CI_Self_Worth<- (Conf*SE_Self_Worth)
        CI_Self_Worth<- round(CI_Self_Worth, digits = 2)
        CI_Happiness<- (Conf*SE_Happiness)
        CI_Happiness<- round(CI_Happiness, digits = 2)
        CI_Super_Mental<- (Conf*SE_Super_Mental)
        CI_Super_Mental<- round(CI_Super_Mental, digits = 2)
        CI_Super_Physical<- (Conf*SE_Super_Physical)
        CI_Super_Physical<- round(CI_Super_Physical, digits = 2)
      }
      CI<- (Conf*SE)
      CI<- round(CI, digits = 2)
      CI<- round(CI, digits = 2)
      CI_Independent_Living<- (Conf*SE_Independent_Living)
      CI_Independent_Living<- round(CI_Independent_Living, digits = 2)
      CI_Relationships<- (Conf*SE_Relationships)
      CI_Relationships<- round(CI_Relationships, digits = 2)
      CI_Mental_Health<- (Conf*SE_Mental_Health)
      CI_Mental_Health<- round(CI_Mental_Health, digits = 2)
      CI_Coping<- (Conf*SE_Coping)
      CI_Coping<- round(CI_Coping, digits = 2)
      CI_Pain<- (Conf*SE_Pain)
      CI_Pain<- round(CI_Pain, digits = 2)
      CI_Senses<- (Conf*SE_Senses)
      CI_Senses<- round(CI_Senses, digits = 2)
      CI_Self_Worth<- (Conf*SE_Self_Worth)
      CI_Self_Worth<- round(CI_Self_Worth, digits = 2)
      CI_Happiness<- (Conf*SE_Happiness)
      CI_Happiness<- round(CI_Happiness, digits = 2)
      CI_Super_Mental<- (Conf*SE_Super_Mental)
      CI_Super_Mental<- round(CI_Super_Mental, digits = 2)
      CI_Super_Physical<- (Conf*SE_Super_Physical)
      CI_Super_Physical<- round(CI_Super_Physical, digits = 2)
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Independent_Living<- PTS_Independent_Living + CI_Independent_Living
      CI_Upper_Lim_Independent_Living<- round(CI_Upper_Lim_Independent_Living, digits = 2)
      CI_Lower_Lim_Independent_Living<-PTS_Independent_Living - CI_Independent_Living
      CI_Lower_Lim_Independent_Living<- round(CI_Lower_Lim_Independent_Living, digits = 2)
      CI_Upper_Lim_Relationships<- PTS_Relationships + CI_Relationships
      CI_Upper_Lim_Relationships<- round(CI_Upper_Lim_Relationships, digits = 2)
      CI_Lower_Lim_Relationships<-PTS_Relationships - CI_Relationships
      CI_Lower_Lim_Relationships<- round(CI_Lower_Lim_Relationships, digits = 2)
      CI_Upper_Lim_Mental_Health<- PTS_Mental_Health + CI_Mental_Health
      CI_Upper_Lim_Mental_Health<- round(CI_Upper_Lim_Mental_Health, digits = 2)
      CI_Lower_Lim_Mental_Health<-PTS_Mental_Health - CI_Mental_Health
      CI_Lower_Lim_Mental_Health<- round(CI_Lower_Lim_Mental_Health, digits = 2)
      CI_Upper_Lim_Coping<- PTS_Coping + CI_Coping
      CI_Upper_Lim_Coping<- round(CI_Upper_Lim_Coping, digits = 2)
      CI_Lower_Lim_Coping<-PTS_Coping - CI_Coping
      CI_Lower_Lim_Coping<- round(CI_Lower_Lim_Coping, digits = 2)
      CI_Upper_Lim_Pain<- PTS_Pain + CI_Pain
      CI_Upper_Lim_Pain<- round(CI_Upper_Lim_Pain, digits = 2)
      CI_Lower_Lim_Pain<-PTS_Pain - CI_Pain
      CI_Lower_Lim_Pain<- round(CI_Lower_Lim_Pain, digits = 2)
      CI_Upper_Lim_Senses<- PTS_Senses + CI_Senses
      CI_Upper_Lim_Senses<- round(CI_Upper_Lim_Senses, digits = 2)
      CI_Lower_Lim_Senses<- PTS_Senses - CI_Senses
      CI_Lower_Lim_Senses<- round(CI_Lower_Lim_Senses, digits = 2)
      CI_Upper_Lim_Self_Worth<- PTS_Self_Worth + CI_Self_Worth
      CI_Upper_Lim_Self_Worth<- round(CI_Upper_Lim_Self_Worth, digits = 2)
      CI_Lower_Lim_Self_Worth<-PTS_Self_Worth - CI_Self_Worth
      CI_Lower_Lim_Self_Worth<- round(CI_Lower_Lim_Self_Worth, digits = 2)
      CI_Upper_Lim_Happiness<- PTS_Happiness + CI_Happiness
      CI_Upper_Lim_Happiness<- round(CI_Upper_Lim_Happiness, digits = 2)
      CI_Lower_Lim_Happiness<-PTS_Happiness - CI_Happiness
      CI_Lower_Lim_Happiness<- round(CI_Lower_Lim_Happiness, digits = 2)
      CI_Upper_Lim_Super_Mental<- PTS_Super_Mental + CI_Super_Mental
      CI_Upper_Lim_Super_Mental<- round(CI_Upper_Lim_Super_Mental, digits = 2)
      CI_Lower_Lim_Super_Mental<-PTS_Super_Mental - CI_Super_Mental
      CI_Lower_Lim_Super_Mental<- round(CI_Lower_Lim_Super_Mental, digits = 2)
      CI_Upper_Lim_Super_Physical<- PTS_Super_Physical + CI_Super_Physical
      CI_Upper_Lim_Super_Physical<- round(CI_Upper_Lim_Super_Physical, digits = 2)
      CI_Lower_Lim_Super_Physical<-PTS_Super_Physical - CI_Super_Physical
      CI_Lower_Lim_Super_Physical<- round(CI_Lower_Lim_Super_Physical, digits = 2)
      
      if(input$Select_CI == "2") {
        CI<- input$Man_CI
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Independent_Living == "2") {
        CI_Independent_Living<- input$Man_CI_Independent_Living
        CI_Independent_Living<- round(CI_Independent_Living, digits = 2)
        CI_Upper_Lim_Independent_Living<- Score_Independent_Living + CI_Independent_Living
        CI_Upper_Lim_Independent_Living<- round(CI_Upper_Lim_Independent_Living, digits = 2)
        CI_Lower_Lim_Independent_Living<- Score_Independent_Living - CI_Independent_Living
        CI_Lower_Lim_Independent_Living<- round(CI_Lower_Lim_Independent_Living, digits = 2)
      }
      if(input$Select_CI_Relationships == "2") {
        CI_Relationships<- input$Man_CI_Relationships
        CI_Relationships<- round(CI_Relationships, digits = 2)
        CI_Upper_Lim_Relationships<- Score_Relationships + CI_Relationships
        CI_Upper_Lim_Relationships<- round(CI_Upper_Lim_Relationships, digits = 2)
        CI_Lower_Lim_Relationships<- Score_Relationships - CI_Relationships
        CI_Lower_Lim_Relationships<- round(CI_Lower_Lim_Relationships, digits = 2)
      }
      if(input$Select_CI_Mental_Health == "2") {
        CI_Mental_Health<- input$Man_CI_Mental_Health
        CI_Mental_Health<- round(CI_Mental_Health, digits = 2)
        CI_Upper_Lim_Mental_Health<- Score_Mental_Health + CI_Mental_Health
        CI_Upper_Lim_Mental_Health<- round(CI_Upper_Lim_Mental_Health, digits = 2)
        CI_Lower_Lim_Mental_Health<- Score_Mental_Health - CI_Mental_Health
        CI_Lower_Lim_Mental_Health<- round(CI_Lower_Lim_Mental_Health, digits = 2)
      }
      if(input$Select_CI_Coping == "2") {
        CI_Coping<- input$Man_CI_Coping
        CI_Coping<- round(CI_Coping, digits = 2)
        CI_Upper_Lim_Coping<- Score_Coping + CI_Coping
        CI_Upper_Lim_Coping<- round(CI_Upper_Lim_Coping, digits = 2)
        CI_Lower_Lim_Coping<- Score_Coping - CI_Coping
        CI_Lower_Lim_Coping<- round(CI_Lower_Lim_Coping, digits = 2)
      }
      if(input$Select_CI_Pain == "2") {
        CI_Pain<- input$Man_CI_Pain
        CI_Pain<- round(CI_Pain, digits = 2)
        CI_Upper_Lim_Pain<- Score_Pain + CI_Pain
        CI_Upper_Lim_Pain<- round(CI_Upper_Lim_Pain, digits = 2)
        CI_Lower_Lim_Pain<- Score_Pain - CI_Pain
        CI_Lower_Lim_Pain<- round(CI_Lower_Lim_Pain, digits = 2)
      }
      if(input$Select_CI_Senses == "2") {
        CI_Senses<- input$Man_CI_Senses
        CI_Senses<- round(CI_Senses, digits = 2)
        CI_Upper_Lim_Senses<- Score_Senses + CI_Senses
        CI_Upper_Lim_Senses<- round(CI_Upper_Lim_Senses, digits = 2)
        CI_Lower_Lim_Senses<- Score_Senses - CI_Senses
        CI_Lower_Lim_Senses<- round(CI_Lower_Lim_Senses, digits = 2)
      }
      if(input$Select_CI_Self_Worth == "2") {
        CI_Self_Worth<- input$Man_CI_Self_Worth
        CI_Self_Worth<- round(CI_Self_Worth, digits = 2)
        CI_Upper_Lim_Self_Worth<- Score_Self_Worth + CI_Self_Worth
        CI_Upper_Lim_Self_Worth<- round(CI_Upper_Lim_Self_Worth, digits = 2)
        CI_Lower_Lim_Self_Worth<- Score_Self_Worth - CI_Self_Worth
        CI_Lower_Lim_Self_Worth<- round(CI_Lower_Lim_Self_Worth, digits = 2)
      }
      if(input$Select_CI_Happiness == "2") {
        CI_Happiness<- input$Man_CI_Happiness
        CI_Happiness<- round(CI_Happiness, digits = 2)
        CI_Upper_Lim_Happiness<- Score_Happiness + CI_Happiness
        CI_Upper_Lim_Happiness<- round(CI_Upper_Lim_Happiness, digits = 2)
        CI_Lower_Lim_Happiness<- Score_Happiness - CI_Happiness
        CI_Lower_Lim_Happiness<- round(CI_Lower_Lim_Happiness, digits = 2)
      }
      if(input$Select_CI_Super_Mental == "2") {
        CI_Super_Mental<- input$Man_CI_Super_Mental
        CI_Super_Mental<- round(CI_Super_Mental, digits = 2)
        CI_Upper_Lim_Super_Mental<- Score_Super_Mental + CI_Super_Mental
        CI_Upper_Lim_Super_Mental<- round(CI_Upper_Lim_Super_Mental, digits = 2)
        CI_Lower_Lim_Super_Mental<- Score_Super_Mental - CI_Super_Mental
        CI_Lower_Lim_Super_Mental<- round(CI_Lower_Lim_Super_Mental, digits = 2)
      }
      if(input$Select_CI_Super_Physical == "2") {
        CI_Super_Physical<- input$Man_CI_Super_Physical
        CI_Super_Physical<- round(CI_Super_Physical, digits = 2)
        CI_Upper_Lim_Super_Physical<- Score_Super_Physical + CI_Super_Physical
        CI_Upper_Lim_Super_Physical<- round(CI_Upper_Lim_Super_Physical, digits = 2)
        CI_Lower_Lim_Super_Physical<- Score_Super_Physical - CI_Super_Physical
        CI_Lower_Lim_Super_Physical<- round(CI_Lower_Lim_Super_Physical, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Independent_Living_1<- round(input$Cutoff_Independent_Living_1, digits = 2)
      Cutoff_Score_Independent_Living_2<- round(input$Cutoff_Independent_Living_2, digits = 2)
      Cutoff_Score_Independent_Living_3<- round(input$Cutoff_Independent_Living_3, digits = 2)
      Cutoff_Score_Relationships_1<- round(input$Cutoff_Relationships_1, digits = 2)
      Cutoff_Score_Relationships_2<- round(input$Cutoff_Relationships_2, digits = 2)
      Cutoff_Score_Relationships_3<- round(input$Cutoff_Relationships_3, digits = 2)
      Cutoff_Score_Mental_Health_1<- round(input$Cutoff_Mental_Health_1, digits = 2)
      Cutoff_Score_Mental_Health_2<- round(input$Cutoff_Mental_Health_2, digits = 2)
      Cutoff_Score_Mental_Health_3<- round(input$Cutoff_Mental_Health_3, digits = 2)
      Cutoff_Score_Coping_1<- round(input$Cutoff_Coping_1, digits = 2)
      Cutoff_Score_Coping_2<- round(input$Cutoff_Coping_2, digits = 2)
      Cutoff_Score_Coping_3<- round(input$Cutoff_Coping_3, digits = 2)
      Cutoff_Score_Pain_1<- round(input$Cutoff_Pain_1, digits = 2)
      Cutoff_Score_Pain_2<- round(input$Cutoff_Pain_2, digits = 2)
      Cutoff_Score_Pain_3<- round(input$Cutoff_Pain_3, digits = 2)
      Cutoff_Score_Senses_1<- round(input$Cutoff_Senses_1, digits = 2)
      Cutoff_Score_Senses_2<- round(input$Cutoff_Senses_2, digits = 2)
      Cutoff_Score_Senses_3<- round(input$Cutoff_Senses_3, digits = 2)
      Cutoff_Score_Self_Worth_1<- round(input$Cutoff_Self_Worth_1, digits = 2)
      Cutoff_Score_Self_Worth_2<- round(input$Cutoff_Self_Worth_2, digits = 2)
      Cutoff_Score_Self_Worth_3<- round(input$Cutoff_Self_Worth_3, digits = 2)
      Cutoff_Score_Happiness_1<- round(input$Cutoff_Happiness_1, digits = 2)
      Cutoff_Score_Happiness_2<- round(input$Cutoff_Happiness_2, digits = 2)
      Cutoff_Score_Happiness_3<- round(input$Cutoff_Happiness_3, digits = 2)
      Cutoff_Score_Super_Mental_1<- round(input$Cutoff_Super_Mental_1, digits = 2)
      Cutoff_Score_Super_Mental_2<- round(input$Cutoff_Super_Mental_2, digits = 2)
      Cutoff_Score_Super_Mental_3<- round(input$Cutoff_Super_Mental_3, digits = 2)
      Cutoff_Score_Super_Physical_1<- round(input$Cutoff_Super_Physical_1, digits = 2)
      Cutoff_Score_Super_Physical_2<- round(input$Cutoff_Super_Physical_2, digits = 2)
      Cutoff_Score_Super_Physical_3<- round(input$Cutoff_Super_Physical_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Independent_Living,Change_Independent_Living,PTS_Independent_Living, SE_Independent_Living, CI_Upper_Lim_Independent_Living, CI_Lower_Lim_Independent_Living, Cutoff_Score_Independent_Living_1,Cutoff_Score_Independent_Living_2,Cutoff_Score_Independent_Living_3,
                                      Score_Relationships,Change_Relationships, PTS_Relationships, SE_Relationships, CI_Upper_Lim_Relationships, CI_Lower_Lim_Relationships, Cutoff_Score_Relationships_1,Cutoff_Score_Relationships_2,Cutoff_Score_Relationships_3, 
                                      Score_Mental_Health,Change_Mental_Health,PTS_Mental_Health, SE_Mental_Health, CI_Upper_Lim_Mental_Health, CI_Lower_Lim_Mental_Health, Cutoff_Score_Mental_Health_1,Cutoff_Score_Mental_Health_2,Cutoff_Score_Mental_Health_3, 
                                      Score_Coping,Change_Coping,PTS_Coping, SE_Coping, CI_Upper_Lim_Coping, CI_Lower_Lim_Coping, Cutoff_Score_Coping_1,Cutoff_Score_Coping_2,Cutoff_Score_Coping_3, 
                                      Score_Pain,Change_Pain,PTS_Pain, SE_Pain, CI_Upper_Lim_Pain, CI_Lower_Lim_Pain, Cutoff_Score_Pain_1,Cutoff_Score_Pain_2,Cutoff_Score_Pain_3,
                                      Score_Senses,Change_Senses, PTS_Senses, SE_Senses, CI_Upper_Lim_Senses, CI_Lower_Lim_Senses, Cutoff_Score_Senses_1,Cutoff_Score_Senses_2,Cutoff_Score_Senses_3, 
                                      Score_Self_Worth,Change_Self_Worth,PTS_Self_Worth, SE_Self_Worth, CI_Upper_Lim_Self_Worth, CI_Lower_Lim_Self_Worth, Cutoff_Score_Self_Worth_1,Cutoff_Score_Self_Worth_2,Cutoff_Score_Self_Worth_3, 
                                      Score_Happiness,Change_Happiness,PTS_Happiness, SE_Happiness, CI_Upper_Lim_Happiness, CI_Lower_Lim_Happiness, Cutoff_Score_Happiness_1,Cutoff_Score_Happiness_2,Cutoff_Score_Happiness_3, 
                                      Score_Super_Mental,Change_Super_Mental,PTS_Super_Mental, SE_Super_Mental, CI_Upper_Lim_Super_Mental, CI_Lower_Lim_Super_Mental, Cutoff_Score_Super_Mental_1,Cutoff_Score_Super_Mental_2,Cutoff_Score_Super_Mental_3,
                                      Score_Super_Physical,Change_Super_Physical,PTS_Super_Physical, SE_Super_Physical, CI_Upper_Lim_Super_Physical, CI_Lower_Lim_Super_Physical, Cutoff_Score_Super_Physical_1,Cutoff_Score_Super_Physical_2,Cutoff_Score_Super_Physical_3)
    } else if(input$Timepoint == "2") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date<- c(Date_1, Date_2)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Score_1<- sum(Score_1a, na.rm = TRUE)
      Score_1<- (1-(Score_1 - 35)/(176-35))*100
      Score_1<- round(Score_1, digits = 2)
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_2a), Score = Score_2a)
      Score_2<- sum(Score_2a, na.rm = TRUE)
      Score_2<- (1-(Score_2 - 35)/(176-35))*100
      Score_2<- round(Score_2, digits = 2)
      Score<- c(Score_1, Score_2)
      Score_Independent_Living_1<- sum(Score_1a[c(30,3,15,9)], na.rm = TRUE)
      Score_Independent_Living_1<- (1-(Score_Independent_Living_1- 4)/(22-4))*100
      Score_Independent_Living_1<- round(Score_Independent_Living_1, digits = 2)
      Score_Independent_Living_2<- sum(Score_2a[c(30,3,15,9)], na.rm = TRUE)
      Score_Independent_Living_2<- (1-(Score_Independent_Living_2- 4)/(22-4))*100
      Score_Independent_Living_2<- round(Score_Independent_Living_2, digits = 2)
      Score_Independent_Living<- c(Score_Independent_Living_1, Score_Independent_Living_2)
      Score_Relationships_1<- sum(Score_1a[c(23,10,31,2,34,9,4)], na.rm = TRUE)
      Score_Relationships_1<- (1-(Score_Relationships_1 - 7)/(34-7))*100
      Score_Relationships_1<- round(Score_Relationships_1, digits = 2)
      Score_Relationships_2<- sum(Score_2a[c(23,10,31,2,34,9,4)], na.rm = TRUE)
      Score_Relationships_2<- (1-(Score_Relationships_2 - 7)/(34-7))*100
      Score_Relationships_2<- round(Score_Relationships_2, digits = 2)
      Score_Relationships<- c(Score_Relationships_1,Score_Relationships_2)
      Score_Mental_Health_1<- sum(Score_1a[c(33,12,14,16,35,18,5,8)], na.rm = TRUE)
      Score_Mental_Health_1<- (1-(Score_Mental_Health_1 - 8)/(41-8))*100
      Score_Mental_Health_1<- round(Score_Mental_Health_1, digits = 2)
      Score_Mental_Health_2<- sum(Score_2a[c(33,12,14,16,35,18,5,8)], na.rm = TRUE)
      Score_Mental_Health_2<- (1-(Score_Mental_Health_2 - 8)/(41-8))*100
      Score_Mental_Health_2<- round(Score_Mental_Health_2, digits = 2)
      Score_Mental_Health<- c(Score_Mental_Health_1, Score_Mental_Health_2)
      Score_Coping_1<- sum(Score_1a[c(1,29,21)], na.rm = TRUE)
      Score_Coping_1<- (1-(Score_Coping_1 - 3)/(15-3))*100
      Score_Coping_1<- round(Score_Coping_1, digits = 2)
      Score_Coping_2<- sum(Score_2a[c(1,29,21)], na.rm = TRUE)
      Score_Coping_2<- (1-(Score_Coping_2 - 3)/(15-3))*100
      Score_Coping_2<- round(Score_Coping_2, digits = 2)
      Score_Coping<- c(Score_Coping_1, Score_Coping_2)
      Score_Pain_1<- sum(Score_1a[c(6,22,24)], na.rm = TRUE)
      Score_Pain_1<- (1-(Score_Pain_1 - 3)/(13-3))*100
      Score_Pain_1<- round(Score_Pain_1, digits = 2)
      Score_Pain_2<- sum(Score_2a[c(6,22,24)], na.rm = TRUE)
      Score_Pain_2<- (1-(Score_Pain_2 - 3)/(13-3))*100
      Score_Pain_2<- round(Score_Pain_2, digits = 2)
      Score_Pain<- c(Score_Pain_1, Score_Pain_2)
      Score_Senses_1<- sum(Score_1a[c(28,32,11)], na.rm = TRUE)
      Score_Senses_1<- (1-(Score_Senses_1 - 3)/(16-3))*100
      Score_Senses_1<- round(Score_Senses_1, digits = 2)
      Score_Senses_2<- sum(Score_2a[c(28,32,11)], na.rm = TRUE)
      Score_Senses_2<- (1-(Score_Senses_2 - 3)/(16-3))*100
      Score_Senses_2<- round(Score_Senses_2, digits = 2)
      Score_Senses<- c(Score_Senses_1, Score_Senses_2)
      Score_Self_Worth_1<- sum(Score_1a[c(26,13,7)], na.rm = TRUE)
      Score_Self_Worth_1<- (1-(Score_Self_Worth_1 - 3)/(15-3))*100
      Score_Self_Worth_1<- round(Score_Self_Worth_1, digits = 2)
      Score_Self_Worth_2<- sum(Score_2a[c(26,13,7)], na.rm = TRUE)
      Score_Self_Worth_2<- (1-(Score_Self_Worth_2 - 3)/(15-3))*100
      Score_Self_Worth_2<- round(Score_Self_Worth_2, digits = 2)
      Score_Self_Worth<- c(Score_Self_Worth_1, Score_Self_Worth_2)
      Score_Happiness_1<- sum(Score_1a[c(27,17,20,25)], na.rm = TRUE)
      Score_Happiness_1<- (1-(Score_Happiness_1 - 4)/(20-4))*100
      Score_Happiness_1<- round(Score_Happiness_1, digits = 2)
      Score_Happiness_2<- sum(Score_2a[c(27,17,20,25)], na.rm = TRUE)
      Score_Happiness_2<- (1-(Score_Happiness_2 - 4)/(20-4))*100
      Score_Happiness_2<- round(Score_Happiness_2, digits = 2)
      Score_Happiness<- c(Score_Happiness_1, Score_Happiness_2)
      Score_Super_Mental_1<- sum(Score_1a[c(33,12,14,16,35,18,5,8,27,17,20,25,1,29,21,23,10,31,2,34,9,4,26,13,7)], na.rm = TRUE)
      Score_Super_Mental_1<- (1-(Score_Super_Mental_1 - 25)/(125-25))*100
      Score_Super_Mental_1<- round(Score_Super_Mental_1, digits = 2)
      Score_Super_Mental_2<- sum(Score_2a[c(33,12,14,16,35,18,5,8,27,17,20,25,1,29,21,23,10,31,2,34,9,4,26,13,7)], na.rm = TRUE) 
      Score_Super_Mental_2<- (1-(Score_Super_Mental_2 - 25)/(125-25))*100
      Score_Super_Mental_2<- round(Score_Super_Mental_2, digits = 2)
      Score_Super_Mental<- c(Score_Super_Mental_1, Score_Super_Mental_2)
      Score_Super_Physical_1<- sum(Score_1a[c(30,3,15,9,6,22,24,28,32,11)], na.rm = TRUE) 
      Score_Super_Physical_1<- (1-(Score_Super_Physical_1 - 10)/(51-10))*100
      Score_Super_Physical_1<- round(Score_Super_Physical_1, digits = 2)
      Score_Super_Physical_2<- sum(Score_2a[c(30,3,15,9,6,22,24,28,32,11)], na.rm = TRUE) 
      Score_Super_Physical_2<- (1-(Score_Super_Physical_2 - 10)/(51-10))*100
      Score_Super_Physical_2<- round(Score_Super_Physical_2, digits = 2)
      Score_Super_Physical<- c(Score_Super_Physical_1, Score_Super_Physical_2)
      Change<- c(0, (Score_2 - Score_1))
      Change<- round(Change, digits = 2)
      Change_Independent_Living<- c(0, (Score_Independent_Living_2 - Score_Independent_Living_1))
      Change_Independent_Living<- round(Change_Independent_Living, digits = 2)
      Change_Relationships<- c(0, (Score_Relationships_2 - Score_Relationships_1))
      Change_Relationships<- round(Change_Relationships, digits = 2)
      Change_Mental_Health<- c(0, (Score_Mental_Health_2 - Score_Mental_Health_1))
      Change_Mental_Health<- round(Change_Mental_Health, digits = 2)
      Change_Coping<- c(0, (Score_Coping_2 - Score_Coping_1))
      Change_Coping<- round(Change_Coping, digits = 2)
      Change_Pain<- c(0, (Score_Pain_2 - Score_Pain_1))
      Change_Pain<- round(Change_Pain, digits = 2)
      Change_Senses<- c(0, (Score_Senses_2 - Score_Senses_1))
      Change_Senses<- round(Change_Senses, digits = 2)
      Change_Self_Worth<- c(0, (Score_Self_Worth_2 - Score_Self_Worth_1))
      Change_Self_Worth<- round(Change_Self_Worth, digits = 2)
      Change_Happiness<- c(0, (Score_Happiness_2 - Score_Happiness_1))
      Change_Happiness<- round(Change_Happiness, digits = 2)
      Change_Super_Mental<- c(0, (Score_Super_Mental_2 - Score_Super_Mental_1))
      Change_Super_Mental<- round(Change_Super_Mental, digits = 2)
      Change_Super_Physical<- c(0, (Score_Super_Physical_2 - Score_Super_Physical_1))
      Change_Super_Physical<- round(Change_Super_Physical, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2)
        PTS_Independent_Living_1<- (Rel_Independent_Living * Score_Independent_Living_1) + (M_Independent_Living * (1 - Rel_Independent_Living))
        PTS_Independent_Living_2<- (Rel_Independent_Living * Score_Independent_Living_2) + (M_Independent_Living * (1 - Rel_Independent_Living))
        PTS_Independent_Living<- c(PTS_Independent_Living_1, PTS_Independent_Living_2)
        PTS_Relationships_1<- (Rel_Relationships * Score_Relationships_1) + (M_Relationships * (1 - Rel_Relationships))
        PTS_Relationships_2<- (Rel_Relationships * Score_Relationships_2) + (M_Relationships * (1 - Rel_Relationships))
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2)
        PTS_Mental_Health_1<- (Rel_Mental_Health * Score_Mental_Health_1) + (M_Mental_Health * (1 - Rel_Mental_Health))
        PTS_Mental_Health_2<- (Rel_Mental_Health * Score_Mental_Health_2) + (M_Mental_Health * (1 - Rel_Mental_Health))
        PTS_Mental_Health<- c(PTS_Mental_Health_1, PTS_Mental_Health_2)
        PTS_Coping_1<- (Rel_Coping * Score_Coping_1) + (M_Coping * (1 - Rel_Coping))
        PTS_Coping_2<- (Rel_Coping * Score_Coping_2) + (M_Coping * (1 - Rel_Coping))
        PTS_Coping<- c(PTS_Coping_1, PTS_Coping_2)
        PTS_Pain_1<- (Rel_Pain * Score_Pain_1) + (M_Pain * (1 - Rel_Pain))
        PTS_Pain_2<- (Rel_Pain * Score_Pain_2) + (M_Pain * (1 - Rel_Pain))
        PTS_Pain<- c(PTS_Pain_1, PTS_Pain_2)
        PTS_Senses_1<- (Rel_Senses * Score_Senses_1) + (M_Senses * (1 - Rel_Senses))
        PTS_Senses_2<- (Rel_Senses * Score_Senses_2) + (M_Senses * (1 - Rel_Senses))
        PTS_Senses<- c(PTS_Senses_1, PTS_Senses_2)
        PTS_Self_Worth_1<- (Rel_Self_Worth * Score_Self_Worth_1) + (M_Self_Worth * (1 - Rel_Self_Worth))
        PTS_Self_Worth_2<- (Rel_Self_Worth * Score_Self_Worth_2) + (M_Self_Worth * (1 - Rel_Self_Worth))
        PTS_Self_Worth<- c(PTS_Self_Worth_1, PTS_Self_Worth_2)
        PTS_Happiness_1<- (Rel_Happiness * Score_Happiness_1) + (M_Happiness * (1 - Rel_Happiness))
        PTS_Happiness_2<- (Rel_Happiness * Score_Happiness_2) + (M_Happiness * (1 - Rel_Happiness))
        PTS_Happiness<- c(PTS_Happiness_1, PTS_Happiness_2)
        PTS_Super_Mental_1<- (Rel_Super_Mental * Score_Super_Mental_1) + (M_Super_Mental * (1 - Rel_Super_Mental))
        PTS_Super_Mental_2<- (Rel_Super_Mental * Score_Super_Mental_2) + (M_Super_Mental * (1 - Rel_Super_Mental))
        PTS_Super_Mental<- c(PTS_Super_Mental_1, PTS_Super_Mental_2)
        PTS_Super_Physical_1<- (Rel_Super_Physical * Score_Super_Physical_1) + (M_Super_Physical * (1 - Rel_Super_Physical))
        PTS_Super_Physical_2<- (Rel_Super_Physical * Score_Super_Physical_2) + (M_Super_Physical * (1 - Rel_Super_Physical))
        PTS_Super_Physical<- c(PTS_Super_Physical_1, PTS_Super_Physical_2)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2)
        PTS_Independent_Living_1<- Score_Independent_Living_1 + (M_Retest_Independent_Living - M_Independent_Living)  
        PTS_Independent_Living_2<- Score_Independent_Living_2 + (M_Retest_Independent_Living - M_Independent_Living) 
        PTS_Independent_Living<- c(PTS_Independent_Living_1, PTS_Independent_Living_2)
        PTS_Relationships_1<- Score_Relationships_1 + (M_Retest_Relationships - M_Relationships)  
        PTS_Relationships_2<- Score_Relationships_2 + (M_Retest_Relationships - M_Relationships) 
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2)
        PTS_Mental_Health_1<- Score_Mental_Health_1 + (M_Retest_Mental_Health - M_Mental_Health)  
        PTS_Mental_Health_2<- Score_Mental_Health_2 + (M_Retest_Mental_Health - M_Mental_Health) 
        PTS_Mental_Health<- c(PTS_Mental_Health_1, PTS_Mental_Health_2)
        PTS_Coping_1<- Score_Coping_1 + (M_Retest_Coping - M_Coping)  
        PTS_Coping_2<- Score_Coping_2 + (M_Retest_Coping - M_Coping) 
        PTS_Coping<- c(PTS_Coping_1, PTS_Coping_2)
        PTS_Pain_1<- Score_Pain_1 + (M_Retest_Pain - M_Pain)  
        PTS_Pain_2<- Score_Pain_2 + (M_Retest_Pain - M_Pain) 
        PTS_Pain<- c(PTS_Pain_1, PTS_Pain_2)
        PTS_Senses_1<- Score_Senses_1 + (M_Retest_Senses - M_Senses)  
        PTS_Senses_2<- Score_Senses_2 + (M_Retest_Senses - M_Senses) 
        PTS_Senses<- c(PTS_Senses_1, PTS_Senses_2)
        PTS_Self_Worth_1<- Score_Self_Worth_1 + (M_Retest_Self_Worth - M_Self_Worth)  
        PTS_Self_Worth_2<- Score_Self_Worth_2 + (M_Retest_Self_Worth - M_Self_Worth) 
        PTS_Self_Worth<- c(PTS_Self_Worth_1, PTS_Self_Worth_2)
        PTS_Happiness_1<- Score_Happiness_1 + (M_Retest_Happiness - M_Happiness)  
        PTS_Happiness_2<- Score_Happiness_2 + (M_Retest_Happiness - M_Happiness) 
        PTS_Happiness<- c(PTS_Happiness_1, PTS_Happiness_2)
        PTS_Super_Mental_1<- Score_Super_Mental_1 + (M_Retest_Super_Mental - M_Super_Mental)  
        PTS_Super_Mental_2<- Score_Super_Mental_2 + (M_Retest_Super_Mental - M_Super_Mental) 
        PTS_Super_Mental<- c(PTS_Super_Mental_1, PTS_Super_Mental_2)
        PTS_Super_Physical_1<- Score_Super_Physical_1 + (M_Retest_Super_Physical - M_Super_Physical)  
        PTS_Super_Physical_2<- Score_Super_Physical_2 + (M_Retest_Super_Physical - M_Super_Physical) 
        PTS_Super_Physical<- c(PTS_Super_Physical_1, PTS_Super_Physical_2)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Independent_Living<- Score_Independent_Living
        PTS_Relationships<- Score_Relationships
        PTS_Mental_Health<- Score_Mental_Health
        PTS_Coping<- Score_Coping
        PTS_Pain<- Score_Pain
        PTS_Senses<- Score_Senses
        PTS_Self_Worth<- Score_Self_Worth
        PTS_Happiness<- Score_Happiness
        PTS_Super_Mental<- Score_Super_Mental
        PTS_Super_Physical<- Score_Super_Physical
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Independent_Living<- Rel_Independent_Living * (SD_Retest_Independent_Living/SD_Independent_Living)
        A_Constant_Independent_Living<- M_Retest_Independent_Living - (B_Slope_Independent_Living * M_Independent_Living)
        B_Adj_Independent_Living<- SD_Retest_Independent_Living/SD_Independent_Living
        A_Adj_Independent_Living<- M_Retest_Independent_Living - (B_Adj_Independent_Living * M_Independent_Living)
        PTS_Independent_Living_1<- (B_Adj_Independent_Living * Score_Independent_Living_1) + A_Adj_Independent_Living
        PTS_Independent_Living_2<- (B_Adj_Independent_Living * Score_Independent_Living_2) + A_Adj_Independent_Living
        PTS_Independent_Living<- c(PTS_Independent_Living_1,PTS_Independent_Living_2)
        B_Slope_Relationships<- Rel_Relationships * (SD_Retest_Relationships/SD_Relationships)
        A_Constant_Relationships<- M_Retest_Relationships - (B_Slope_Relationships * M_Relationships)
        B_Adj_Relationships<- SD_Retest_Relationships/SD_Relationships
        A_Adj_Relationships<- M_Retest_Relationships - (B_Adj_Relationships * M_Relationships)
        PTS_Relationships_1<- (B_Adj_Relationships * Score_Relationships_1) + A_Adj_Relationships
        PTS_Relationships_2<- (B_Adj_Relationships * Score_Relationships_2) + A_Adj_Relationships
        PTS_Relationships<- c(PTS_Relationships_1,PTS_Relationships_2)
        B_Slope_Mental_Health<- Rel_Mental_Health * (SD_Retest_Mental_Health/SD_Mental_Health)
        A_Constant_Mental_Health<- M_Retest_Mental_Health - (B_Slope_Mental_Health * M_Mental_Health)
        B_Adj_Mental_Health<- SD_Retest_Mental_Health/SD_Mental_Health
        A_Adj_Mental_Health<- M_Retest_Mental_Health - (B_Adj_Mental_Health * M_Mental_Health)
        PTS_Mental_Health_1<- (B_Adj_Mental_Health * Score_Mental_Health_1) + A_Adj_Mental_Health
        PTS_Mental_Health_2<- (B_Adj_Mental_Health * Score_Mental_Health_2) + A_Adj_Mental_Health
        PTS_Mental_Health<- c(PTS_Mental_Health_1,PTS_Mental_Health_2)
        B_Slope_Coping<- Rel_Coping * (SD_Retest_Coping/SD_Coping)
        A_Constant_Coping<- M_Retest_Coping - (B_Slope_Coping * M_Coping)
        B_Adj_Coping<- SD_Retest_Coping/SD_Coping
        A_Adj_Coping<- M_Retest_Coping - (B_Adj_Coping * M_Coping)
        PTS_Coping_1<- (B_Adj_Coping * Score_Coping_1) + A_Adj_Coping
        PTS_Coping_2<- (B_Adj_Coping * Score_Coping_2) + A_Adj_Coping
        PTS_Coping<- c(PTS_Coping_1,PTS_Coping_2)
        B_Slope_Pain<- Rel_Pain * (SD_Retest_Pain/SD_Pain)
        A_Constant_Pain<- M_Retest_Pain - (B_Slope_Pain * M_Pain)
        B_Adj_Pain<- SD_Retest_Pain/SD_Pain
        A_Adj_Pain<- M_Retest_Pain - (B_Adj_Pain * M_Pain)
        PTS_Pain_1<- (B_Adj_Pain * Score_Pain_1) + A_Adj_Pain
        PTS_Pain_2<- (B_Adj_Pain * Score_Pain_2) + A_Adj_Pain
        PTS_Pain<- c(PTS_Pain_1,PTS_Pain_2)
        B_Slope_Senses<- Rel_Senses * (SD_Retest_Senses/SD_Senses)
        A_Constant_Senses<- M_Retest_Senses - (B_Slope_Senses * M_Senses)
        B_Adj_Senses<- SD_Retest_Senses/SD_Senses
        A_Adj_Senses<- M_Retest_Senses - (B_Adj_Senses * M_Senses)
        PTS_Senses_1<- (B_Adj_Senses * Score_Senses_1) + A_Adj_Senses
        PTS_Senses_2<- (B_Adj_Senses * Score_Senses_2) + A_Adj_Senses
        PTS_Senses<- c(PTS_Senses_1, PTS_Senses_2)
        B_Slope_Self_Worth<- Rel_Self_Worth * (SD_Retest_Self_Worth/SD_Self_Worth)
        A_Constant_Self_Worth<- M_Retest_Self_Worth - (B_Slope_Self_Worth * M_Self_Worth)
        B_Adj_Self_Worth<- SD_Retest_Self_Worth/SD_Self_Worth
        A_Adj_Self_Worth<- M_Retest_Self_Worth - (B_Adj_Self_Worth * M_Self_Worth)
        PTS_Self_Worth_1<- (B_Adj_Self_Worth * Score_Self_Worth_1) + A_Adj_Self_Worth
        PTS_Self_Worth_2<- (B_Adj_Self_Worth * Score_Self_Worth_2) + A_Adj_Self_Worth
        PTS_Self_Worth<- c(PTS_Self_Worth_1,PTS_Self_Worth_2)
        B_Slope_Happiness<- Rel_Happiness * (SD_Retest_Happiness/SD_Happiness)
        A_Constant_Happiness<- M_Retest_Happiness - (B_Slope_Happiness * M_Happiness)
        B_Adj_Happiness<- SD_Retest_Happiness/SD_Happiness
        A_Adj_Happiness<- M_Retest_Happiness - (B_Adj_Happiness * M_Happiness)
        PTS_Happiness_1<- (B_Adj_Happiness * Score_Happiness_1) + A_Adj_Happiness
        PTS_Happiness_2<- (B_Adj_Happiness * Score_Happiness_2) + A_Adj_Happiness
        PTS_Happiness<- c(PTS_Happiness_1,PTS_Happiness_2)
        B_Slope_Super_Mental<- Rel_Super_Mental * (SD_Retest_Super_Mental/SD_Super_Mental)
        A_Constant_Super_Mental<- M_Retest_Super_Mental - (B_Slope_Super_Mental * M_Super_Mental)
        B_Adj_Super_Mental<- SD_Retest_Super_Mental/SD_Super_Mental
        A_Adj_Super_Mental<- M_Retest_Super_Mental - (B_Adj_Super_Mental * M_Super_Mental)
        PTS_Super_Mental_1<- (B_Adj_Super_Mental * Score_Super_Mental_1) + A_Adj_Super_Mental
        PTS_Super_Mental_2<- (B_Adj_Super_Mental * Score_Super_Mental_2) + A_Adj_Super_Mental
        PTS_Super_Mental<- c(PTS_Super_Mental_1,PTS_Super_Mental_2)
        B_Slope_Super_Physical<- Rel_Super_Physical * (SD_Retest_Super_Physical/SD_Super_Physical)
        A_Constant_Super_Physical<- M_Retest_Super_Physical - (B_Slope_Super_Physical * M_Super_Physical)
        B_Adj_Super_Physical<- SD_Retest_Super_Physical/SD_Super_Physical
        A_Adj_Super_Physical<- M_Retest_Super_Physical - (B_Adj_Super_Physical * M_Super_Physical)
        PTS_Super_Physical_1<- (B_Adj_Super_Physical * Score_Super_Physical_1) + A_Adj_Super_Physical
        PTS_Super_Physical_2<- (B_Adj_Super_Physical * Score_Super_Physical_2) + A_Adj_Super_Physical
        PTS_Super_Physical<- c(PTS_Super_Physical_1,PTS_Super_Physical_2)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Independent_Living<- Rel_Independent_Living * (SD_Retest_Independent_Living/SD_Independent_Living)
        PTS_Independent_Living_1<- B_Slope_Independent_Living * Score_Independent_Living_1
        PTS_Independent_Living_2<- B_Slope_Independent_Living * Score_Independent_Living_2
        PTS_Independent_Living<- c(PTS_Independent_Living_1, PTS_Independent_Living_2)
        B_Slope_Relationships<- Rel_Relationships * (SD_Retest_Relationships/SD_Relationships)
        PTS_Relationships_1<- B_Slope_Relationships * Score_Relationships_1
        PTS_Relationships_2<- B_Slope_Relationships * Score_Relationships_2
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2)
        B_Slope_Mental_Health<- Rel_Mental_Health * (SD_Retest_Mental_Health/SD_Mental_Health)
        PTS_Mental_Health_1<- B_Slope_Mental_Health * Score_Mental_Health_1
        PTS_Mental_Health_2<- B_Slope_Mental_Health * Score_Mental_Health_2
        PTS_Mental_Health<- c(PTS_Mental_Health_1, PTS_Mental_Health_2)
        B_Slope_Coping<- Rel_Coping * (SD_Retest_Coping/SD_Coping)
        PTS_Coping_1<- B_Slope_Coping * Score_Coping_1
        PTS_Coping_2<- B_Slope_Coping * Score_Coping_2
        PTS_Coping<- c(PTS_Coping_1, PTS_Coping_2)
        B_Slope_Pain<- Rel_Pain * (SD_Retest_Pain/SD_Pain)
        PTS_Pain_1<- B_Slope_Pain * Score_Pain_1
        PTS_Pain_2<- B_Slope_Pain * Score_Pain_2
        PTS_Pain<- c(PTS_Pain_1, PTS_Pain_2)
        B_Slope_Senses<- Rel_Senses * (SD_Retest_Senses/SD_Senses)
        PTS_Senses_1<- B_Slope_Senses * Score_Senses_1
        PTS_Senses_2<- B_Slope_Senses * Score_Senses_2
        PTS_Senses<- c(PTS_Senses_1, PTS_Senses_2)
        B_Slope_Self_Worth<- Rel_Self_Worth * (SD_Retest_Self_Worth/SD_Self_Worth)
        PTS_Self_Worth_1<- B_Slope_Self_Worth * Score_Self_Worth_1
        PTS_Self_Worth_2<- B_Slope_Self_Worth * Score_Self_Worth_2
        PTS_Self_Worth<- c(PTS_Self_Worth_1, PTS_Self_Worth_2) 
        B_Slope_Happiness<- Rel_Happiness * (SD_Retest_Happiness/SD_Happiness)
        PTS_Happiness_1<- B_Slope_Happiness * Score_Happiness_1
        PTS_Happiness_2<- B_Slope_Happiness * Score_Happiness_2
        PTS_Happiness<- c(PTS_Happiness_1, PTS_Happiness_2)
        B_Slope_Super_Mental<- Rel_Super_Mental * (SD_Retest_Super_Mental/SD_Super_Mental)
        PTS_Super_Mental_1<- B_Slope_Super_Mental * Score_Super_Mental_1
        PTS_Super_Mental_2<- B_Slope_Super_Mental * Score_Super_Mental_2
        PTS_Super_Mental<- c(PTS_Super_Mental_1, PTS_Super_Mental_2)
        B_Slope_Super_Physical<- Rel_Super_Physical * (SD_Retest_Super_Physical/SD_Super_Physical)
        PTS_Super_Physical_1<- B_Slope_Super_Physical * Score_Super_Physical_1
        PTS_Super_Physical_2<- B_Slope_Super_Physical * Score_Super_Physical_2
        PTS_Super_Physical<- c(PTS_Super_Physical_1, PTS_Super_Physical_2)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2)
        PTS_Independent_Living_1<- Score_Independent_Living_1 + (M_Retest_Independent_Living - M_Independent_Living)
        PTS_Independent_Living_2<- Score_Independent_Living_2 + (M_Retest_Independent_Living - M_Independent_Living)
        PTS_Independent_Living<- c(PTS_Independent_Living_1, PTS_Independent_Living_2)
        PTS_Relationships_1<- Score_Relationships_1 + (M_Retest_Relationships - M_Relationships)
        PTS_Relationships_2<- Score_Relationships_2 + (M_Retest_Relationships - M_Relationships)
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2)
        PTS_Mental_Health_1<- Score_Mental_Health_1 + (M_Retest_Mental_Health - M_Mental_Health)
        PTS_Mental_Health_2<- Score_Mental_Health_2 + (M_Retest_Mental_Health - M_Mental_Health)
        PTS_Mental_Health<- c(PTS_Mental_Health_1, PTS_Mental_Health_2)
        PTS_Coping_1<- Score_Coping_1 + (M_Retest_Coping - M_Coping)
        PTS_Coping_2<- Score_Coping_2 + (M_Retest_Coping - M_Coping)
        PTS_Coping<- c(PTS_Coping_1, PTS_Coping_2)
        PTS_Pain_1<- Score_Pain_1 + (M_Retest_Pain - M_Pain)
        PTS_Pain_2<- Score_Pain_2 + (M_Retest_Pain - M_Pain)
        PTS_Pain<- c(PTS_Pain_1, PTS_Pain_2)
        PTS_Senses_1<- Score_Senses_1 + (M_Retest_Senses - M_Senses)
        PTS_Senses_2<- Score_Senses_2 + (M_Retest_Senses - M_Senses)
        PTS_Senses<- c(PTS_Senses_1, PTS_Senses_2)
        PTS_Self_Worth_1<- Score_Self_Worth_1 + (M_Retest_Self_Worth - M_Self_Worth)
        PTS_Self_Worth_2<- Score_Self_Worth_2 + (M_Retest_Self_Worth - M_Self_Worth)
        PTS_Self_Worth<- c(PTS_Self_Worth_1, PTS_Self_Worth_2)
        PTS_Happiness_1<- Score_Happiness_1 + (M_Retest_Happiness - M_Happiness)
        PTS_Happiness_2<- Score_Happiness_2 + (M_Retest_Happiness - M_Happiness)
        PTS_Happiness<- c(PTS_Happiness_1, PTS_Happiness_2)
        PTS_Super_Mental_1<- Score_Super_Mental_1 + (M_Retest_Super_Mental - M_Super_Mental)
        PTS_Super_Mental_2<- Score_Super_Mental_2 + (M_Retest_Super_Mental - M_Super_Mental)
        PTS_Super_Mental<- c(PTS_Super_Mental_1, PTS_Super_Mental_2)
        PTS_Super_Physical_1<- Score_Super_Physical_1 + (M_Retest_Super_Physical - M_Super_Physical)
        PTS_Super_Physical_2<- Score_Super_Physical_2 + (M_Retest_Super_Physical - M_Super_Physical)
        PTS_Super_Physical<- c(PTS_Super_Physical_1, PTS_Super_Physical_2)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Independent_Living<- round(PTS_Independent_Living, digits = 2)
      PTS_Relationships<- round(PTS_Relationships, digits = 2)
      PTS_Mental_Health<- round(PTS_Mental_Health, digits = 2)
      PTS_Coping<- round(PTS_Coping, digits = 2)
      PTS_Pain<- round(PTS_Pain, digits = 2)
      PTS_Senses<- round(PTS_Senses, digits = 2)
      PTS_Self_Worth<- round(PTS_Self_Worth, digits = 2)
      PTS_Happiness<- round(PTS_Happiness, digits = 2)
      PTS_Super_Mental<- round(PTS_Super_Mental, digits = 2)
      PTS_Super_Physical<- round(PTS_Super_Physical, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2)
        SE_Independent_Living_1<- McSweeny_SE_Independent_Living*sqrt(1 + (1/SampleN) + ((Score_Independent_Living_1 - M_Independent_Living)^2/(SD_Independent_Living^2*(SampleN-1))))
        SE_Independent_Living_2<- McSweeny_SE_Independent_Living*sqrt(1 + (1/SampleN) + ((Score_Independent_Living_2 - M_Independent_Living)^2/(SD_Independent_Living^2*(SampleN-1))))
        SE_Independent_Living<-c(SE_Independent_Living_1, SE_Independent_Living_2)
        SE_Relationships_1<- McSweeny_SE_Relationships*sqrt(1 + (1/SampleN) + ((Score_Relationships_1 - M_Relationships)^2/(SD_Relationships^2*(SampleN-1))))
        SE_Relationships_2<- McSweeny_SE_Relationships*sqrt(1 + (1/SampleN) + ((Score_Relationships_2 - M_Relationships)^2/(SD_Relationships^2*(SampleN-1))))
        SE_Relationships<-c(SE_Relationships_1, SE_Relationships_2)
        SE_Mental_Health_1<- McSweeny_SE_Mental_Health*sqrt(1 + (1/SampleN) + ((Score_Mental_Health_1 - M_Mental_Health)^2/(SD_Mental_Health^2*(SampleN-1))))
        SE_Mental_Health_2<- McSweeny_SE_Mental_Health*sqrt(1 + (1/SampleN) + ((Score_Mental_Health_2 - M_Mental_Health)^2/(SD_Mental_Health^2*(SampleN-1))))
        SE_Mental_Health<-c(SE_Mental_Health_1, SE_Mental_Health_2)
        SE_Coping_1<- McSweeny_SE_Coping*sqrt(1 + (1/SampleN) + ((Score_Coping_1 - M_Coping)^2/(SD_Coping^2*(SampleN-1))))
        SE_Coping_2<- McSweeny_SE_Coping*sqrt(1 + (1/SampleN) + ((Score_Coping_2 - M_Coping)^2/(SD_Coping^2*(SampleN-1))))
        SE_Coping<-c(SE_Coping_1, SE_Coping_2)
        SE_Pain_1<- McSweeny_SE_Pain*sqrt(1 + (1/SampleN) + ((Score_Pain_1 - M_Pain)^2/(SD_Pain^2*(SampleN-1))))
        SE_Pain_2<- McSweeny_SE_Pain*sqrt(1 + (1/SampleN) + ((Score_Pain_2 - M_Pain)^2/(SD_Pain^2*(SampleN-1))))
        SE_Pain<-c(SE_Pain_1, SE_Pain_2)
        SE_Senses_1<- McSweeny_SE_Senses*sqrt(1 + (1/SampleN) + ((Score_Senses_1 - M_Senses)^2/(SD_Senses^2*(SampleN-1))))
        SE_Senses_2<- McSweeny_SE_Senses*sqrt(1 + (1/SampleN) + ((Score_Senses_2 - M_Senses)^2/(SD_Senses^2*(SampleN-1))))
        SE_Senses<-c(SE_Senses_1, SE_Senses_2)
        SE_Self_Worth_1<- McSweeny_SE_Self_Worth*sqrt(1 + (1/SampleN) + ((Score_Self_Worth_1 - M_Self_Worth)^2/(SD_Self_Worth^2*(SampleN-1))))
        SE_Self_Worth_2<- McSweeny_SE_Self_Worth*sqrt(1 + (1/SampleN) + ((Score_Self_Worth_2 - M_Self_Worth)^2/(SD_Self_Worth^2*(SampleN-1))))
        SE_Self_Worth<-c(SE_Self_Worth_1, SE_Self_Worth_2)
        SE_Happiness_1<- McSweeny_SE_Happiness*sqrt(1 + (1/SampleN) + ((Score_Happiness_1 - M_Happiness)^2/(SD_Happiness^2*(SampleN-1))))
        SE_Happiness_2<- McSweeny_SE_Happiness*sqrt(1 + (1/SampleN) + ((Score_Happiness_2 - M_Happiness)^2/(SD_Happiness^2*(SampleN-1))))
        SE_Happiness<-c(SE_Happiness_1, SE_Happiness_2)
        SE_Super_Mental_1<- McSweeny_SE_Super_Mental*sqrt(1 + (1/SampleN) + ((Score_Super_Mental_1 - M_Super_Mental)^2/(SD_Super_Mental^2*(SampleN-1))))
        SE_Super_Mental_2<- McSweeny_SE_Super_Mental*sqrt(1 + (1/SampleN) + ((Score_Super_Mental_2 - M_Super_Mental)^2/(SD_Super_Mental^2*(SampleN-1))))
        SE_Super_Mental<-c(SE_Super_Mental_1, SE_Super_Mental_2)
        SE_Super_Physical_1<- McSweeny_SE_Super_Physical*sqrt(1 + (1/SampleN) + ((Score_Super_Physical_1 - M_Super_Physical)^2/(SD_Super_Physical^2*(SampleN-1))))
        SE_Super_Physical_2<- McSweeny_SE_Super_Physical*sqrt(1 + (1/SampleN) + ((Score_Super_Physical_2 - M_Super_Physical)^2/(SD_Super_Physical^2*(SampleN-1))))
        SE_Super_Physical<-c(SE_Super_Physical_1, SE_Super_Physical_2)
        SE<- round(SE, digits = 2)
        SE_Independent_Living<- round(SE_Independent_Living, digits = 2)
        SE_Relationships<- round(SE_Relationships, digits = 2)
        SE_Mental_Health<- round(SE_Mental_Health, digits = 2)
        SE_Coping<- round(SE_Coping, digits = 2)
        SE_Pain<- round(SE_Pain, digits = 2)
        SE_Senses<- round(SE_Senses, digits = 2)
        SE_Self_Worth<- round(SE_Self_Worth, digits = 2)
        SE_Happiness<- round(SE_Happiness, digits = 2)
        SE_Super_Mental<- round(SE_Super_Mental, digits = 2)
        SE_Super_Physical<- round(SE_Super_Physical, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2))
        CI<- round(CI, digits = 2)
        CI_Independent_Living<- c((Conf*SE_Independent_Living_1), (Conf*SE_Independent_Living_2))
        CI_Independent_Living<- round(CI_Independent_Living, digits = 2)
        CI_Relationships<- c((Conf*SE_Relationships_1), (Conf*SE_Relationships_2))
        CI_Relationships<- round(CI_Relationships, digits = 2)
        CI_Mental_Health<- c((Conf*SE_Mental_Health_1), (Conf*SE_Mental_Health_2))
        CI_Mental_Health<- round(CI_Mental_Health, digits = 2)
        CI_Coping<- c((Conf*SE_Coping_1), (Conf*SE_Coping_2))
        CI_Coping<- round(CI_Coping, digits = 2)
        CI_Pain<- c((Conf*SE_Pain_1), (Conf*SE_Pain_2))
        CI_Pain<- round(CI_Pain, digits = 2)
        CI_Senses<- c((Conf*SE_Senses_1), (Conf*SE_Senses_2))
        CI_Senses<- round(CI_Senses, digits = 2)
        CI_Self_Worth<- c((Conf*SE_Self_Worth_1), (Conf*SE_Self_Worth_2))
        CI_Self_Worth<- round(CI_Self_Worth, digits = 2)
        CI_Happiness<- c((Conf*SE_Happiness_1), (Conf*SE_Happiness_2))
        CI_Happiness<- round(CI_Happiness, digits = 2)
        CI_Super_Mental<- c((Conf*SE_Super_Mental_1), (Conf*SE_Super_Mental_2))
        CI_Super_Mental<- round(CI_Super_Mental, digits = 2)
        CI_Super_Physical<- c((Conf*SE_Super_Physical_1), (Conf*SE_Super_Physical_2))
        CI_Super_Physical<- round(CI_Super_Physical, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
      CI<- c((Conf*SE), (Conf*SE))
      CI<- round(CI, digits = 2)
      CI_Independent_Living<- c((Conf*SE_Independent_Living), (Conf*SE_Independent_Living))
      CI_Independent_Living<- round(CI_Independent_Living, digits = 2)
      CI_Relationships<- c((Conf*SE_Relationships), (Conf*SE_Relationships))
      CI_Relationships<- round(CI_Relationships, digits = 2)
      CI_Mental_Health<- c((Conf*SE_Mental_Health), (Conf*SE_Mental_Health))
      CI_Mental_Health<- round(CI_Mental_Health, digits = 2)
      CI_Coping<- c((Conf*SE_Coping), (Conf*SE_Coping))
      CI_Coping<- round(CI_Coping, digits = 2)
      CI_Pain<- c((Conf*SE_Pain), (Conf*SE_Pain))
      CI_Pain<- round(CI_Pain, digits = 2)
      CI_Senses<- c((Conf*SE_Senses), (Conf*SE_Senses))
      CI_Senses<- round(CI_Senses, digits = 2)
      CI_Self_Worth<- c((Conf*SE_Self_Worth), (Conf*SE_Self_Worth))
      CI_Self_Worth<- round(CI_Self_Worth, digits = 2)
      CI_Happiness<- c((Conf*SE_Happiness), (Conf*SE_Happiness))
      CI_Happiness<- round(CI_Happiness, digits = 2)
      CI_Super_Mental<- c((Conf*SE_Super_Mental), (Conf*SE_Super_Mental))
      CI_Super_Mental<- round(CI_Super_Mental, digits = 2)
      CI_Super_Physical<- c((Conf*SE_Super_Physical), (Conf*SE_Super_Physical))
      CI_Super_Physical<- round(CI_Super_Physical, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Independent_Living<- PTS_Independent_Living + CI_Independent_Living
      CI_Upper_Lim_Independent_Living<- round(CI_Upper_Lim_Independent_Living, digits = 2)
      CI_Lower_Lim_Independent_Living<-PTS_Independent_Living - CI_Independent_Living
      CI_Lower_Lim_Independent_Living<- round(CI_Lower_Lim_Independent_Living, digits = 2)
      CI_Upper_Lim_Relationships<- PTS_Relationships + CI_Relationships
      CI_Upper_Lim_Relationships<- round(CI_Upper_Lim_Relationships, digits = 2)
      CI_Lower_Lim_Relationships<-PTS_Relationships - CI_Relationships
      CI_Lower_Lim_Relationships<- round(CI_Lower_Lim_Relationships, digits = 2)
      CI_Upper_Lim_Mental_Health<- PTS_Mental_Health + CI_Mental_Health
      CI_Upper_Lim_Mental_Health<- round(CI_Upper_Lim_Mental_Health, digits = 2)
      CI_Lower_Lim_Mental_Health<-PTS_Mental_Health - CI_Mental_Health
      CI_Lower_Lim_Mental_Health<- round(CI_Lower_Lim_Mental_Health, digits = 2)
      CI_Upper_Lim_Coping<- PTS_Coping + CI_Coping
      CI_Upper_Lim_Coping<- round(CI_Upper_Lim_Coping, digits = 2)
      CI_Lower_Lim_Coping<-PTS_Coping - CI_Coping
      CI_Lower_Lim_Coping<- round(CI_Lower_Lim_Coping, digits = 2)
      CI_Upper_Lim_Pain<- PTS_Pain + CI_Pain
      CI_Upper_Lim_Pain<- round(CI_Upper_Lim_Pain, digits = 2)
      CI_Lower_Lim_Pain<-PTS_Pain - CI_Pain
      CI_Lower_Lim_Pain<- round(CI_Lower_Lim_Pain, digits = 2)
      CI_Upper_Lim_Senses<- PTS_Senses + CI_Senses
      CI_Upper_Lim_Senses<- round(CI_Upper_Lim_Senses, digits = 2)
      CI_Lower_Lim_Senses<- PTS_Senses - CI_Senses
      CI_Lower_Lim_Senses<- round(CI_Lower_Lim_Senses, digits = 2)
      CI_Upper_Lim_Self_Worth<- PTS_Self_Worth + CI_Self_Worth
      CI_Upper_Lim_Self_Worth<- round(CI_Upper_Lim_Self_Worth, digits = 2)
      CI_Lower_Lim_Self_Worth<-PTS_Self_Worth - CI_Self_Worth
      CI_Lower_Lim_Self_Worth<- round(CI_Lower_Lim_Self_Worth, digits = 2)
      CI_Upper_Lim_Happiness<- PTS_Happiness + CI_Happiness
      CI_Upper_Lim_Happiness<- round(CI_Upper_Lim_Happiness, digits = 2)
      CI_Lower_Lim_Happiness<-PTS_Happiness - CI_Happiness
      CI_Lower_Lim_Happiness<- round(CI_Lower_Lim_Happiness, digits = 2)
      CI_Upper_Lim_Super_Mental<- PTS_Super_Mental + CI_Super_Mental
      CI_Upper_Lim_Super_Mental<- round(CI_Upper_Lim_Super_Mental, digits = 2)
      CI_Lower_Lim_Super_Mental<-PTS_Super_Mental - CI_Super_Mental
      CI_Lower_Lim_Super_Mental<- round(CI_Lower_Lim_Super_Mental, digits = 2)
      CI_Upper_Lim_Super_Physical<- PTS_Super_Physical + CI_Super_Physical
      CI_Upper_Lim_Super_Physical<- round(CI_Upper_Lim_Super_Physical, digits = 2)
      CI_Lower_Lim_Super_Physical<-PTS_Super_Physical - CI_Super_Physical
      CI_Lower_Lim_Super_Physical<- round(CI_Lower_Lim_Super_Physical, digits = 2)
      
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Independent_Living == "2") {
        CI_Independent_Living<- input$Man_CI_Independent_Living
        CI_Independent_Living<- c(CI_Independent_Living, CI_Independent_Living)
        CI_Independent_Living<- round(CI_Independent_Living, digits = 2)
        CI_Upper_Lim_Independent_Living<- Score_Independent_Living + CI_Independent_Living
        CI_Upper_Lim_Independent_Living<- round(CI_Upper_Lim_Independent_Living, digits = 2)
        CI_Lower_Lim_Independent_Living<- Score_Independent_Living - CI_Independent_Living
        CI_Lower_Lim_Independent_Living<- round(CI_Lower_Lim_Independent_Living, digits = 2)
      }
      if(input$Select_CI_Relationships == "2") {
        CI_Relationships<- input$Man_CI_Relationships
        CI_Relationships<- c(CI_Relationships, CI_Relationships)
        CI_Relationships<- round(CI_Relationships, digits = 2)
        CI_Upper_Lim_Relationships<- Score_Relationships + CI_Relationships
        CI_Upper_Lim_Relationships<- round(CI_Upper_Lim_Relationships, digits = 2)
        CI_Lower_Lim_Relationships<- Score_Relationships - CI_Relationships
        CI_Lower_Lim_Relationships<- round(CI_Lower_Lim_Relationships, digits = 2)
      }
      if(input$Select_CI_Mental_Health == "2") {
        CI_Mental_Health<- input$Man_CI_Mental_Health
        CI_Mental_Health<- c(CI_Mental_Health, CI_Mental_Health)
        CI_Mental_Health<- round(CI_Mental_Health, digits = 2)
        CI_Upper_Lim_Mental_Health<- Score_Mental_Health + CI_Mental_Health
        CI_Upper_Lim_Mental_Health<- round(CI_Upper_Lim_Mental_Health, digits = 2)
        CI_Lower_Lim_Mental_Health<- Score_Mental_Health - CI_Mental_Health
        CI_Lower_Lim_Mental_Health<- round(CI_Lower_Lim_Mental_Health, digits = 2)
      }
      if(input$Select_CI_Coping == "2") {
        CI_Coping<- input$Man_CI_Coping
        CI_Coping<- c(CI_Coping, CI_Coping)
        CI_Coping<- round(CI_Coping, digits = 2)
        CI_Upper_Lim_Coping<- Score_Coping + CI_Coping
        CI_Upper_Lim_Coping<- round(CI_Upper_Lim_Coping, digits = 2)
        CI_Lower_Lim_Coping<- Score_Coping - CI_Coping
        CI_Lower_Lim_Coping<- round(CI_Lower_Lim_Coping, digits = 2)
      }
      if(input$Select_CI_Pain == "2") {
        CI_Pain<- input$Man_CI_Pain
        CI_Pain<- c(CI_Pain,  CI_Pain)
        CI_Pain<- round(CI_Pain, digits = 2)
        CI_Upper_Lim_Pain<- Score_Pain + CI_Pain
        CI_Upper_Lim_Pain<- round(CI_Upper_Lim_Pain, digits = 2)
        CI_Lower_Lim_Pain<- Score_Pain - CI_Pain
        CI_Lower_Lim_Pain<- round(CI_Lower_Lim_Pain, digits = 2)
      }
      if(input$Select_CI_Senses == "2") {
        CI_Senses<- input$Man_CI_Senses
        CI_Senses<- c(CI_Senses, CI_Senses)
        CI_Senses<- round(CI_Senses, digits = 2)
        CI_Upper_Lim_Senses<- Score_Senses + CI_Senses
        CI_Upper_Lim_Senses<- round(CI_Upper_Lim_Senses, digits = 2)
        CI_Lower_Lim_Senses<- Score_Senses - CI_Senses
        CI_Lower_Lim_Senses<- round(CI_Lower_Lim_Senses, digits = 2)
      }
      if(input$Select_CI_Self_Worth == "2") {
        CI_Self_Worth<- input$Man_CI_Self_Worth
        CI_Self_Worth<- c(CI_Self_Worth, CI_Self_Worth)
        CI_Self_Worth<- round(CI_Self_Worth, digits = 2)
        CI_Upper_Lim_Self_Worth<- Score_Self_Worth + CI_Self_Worth
        CI_Upper_Lim_Self_Worth<- round(CI_Upper_Lim_Self_Worth, digits = 2)
        CI_Lower_Lim_Self_Worth<- Score_Self_Worth - CI_Self_Worth
        CI_Lower_Lim_Self_Worth<- round(CI_Lower_Lim_Self_Worth, digits = 2)
      }
      if(input$Select_CI_Happiness == "2") {
        CI_Happiness<- input$Man_CI_Happiness
        CI_Happiness<- round(CI_Happiness, digits = 2)
        CI_Happiness<- c(CI_Happiness,  CI_Happiness)
        CI_Upper_Lim_Happiness<- Score_Happiness + CI_Happiness
        CI_Upper_Lim_Happiness<- round(CI_Upper_Lim_Happiness, digits = 2)
        CI_Lower_Lim_Happiness<- Score_Happiness - CI_Happiness
        CI_Lower_Lim_Happiness<- round(CI_Lower_Lim_Happiness, digits = 2)
      }
      if(input$Select_CI_Super_Mental == "2") {
        CI_Super_Mental<- input$Man_CI_Super_Mental
        CI_Super_Mental<- c(CI_Super_Mental, CI_Super_Mental)
        CI_Super_Mental<- round(CI_Super_Mental, digits = 2)
        CI_Upper_Lim_Super_Mental<- Score_Super_Mental + CI_Super_Mental
        CI_Upper_Lim_Super_Mental<- round(CI_Upper_Lim_Super_Mental, digits = 2)
        CI_Lower_Lim_Super_Mental<- Score_Super_Mental - CI_Super_Mental
        CI_Lower_Lim_Super_Mental<- round(CI_Lower_Lim_Super_Mental, digits = 2)
      }
      if(input$Select_CI_Super_Physical == "2") {
        CI_Super_Physical<- input$Man_CI_Super_Physical
        CI_Super_Physical<- c(CI_Super_Physical, CI_Super_Physical)
        CI_Super_Physical<- round(CI_Super_Physical, digits = 2)
        CI_Upper_Lim_Super_Physical<- Score_Super_Physical + CI_Super_Physical
        CI_Upper_Lim_Super_Physical<- round(CI_Upper_Lim_Super_Physical, digits = 2)
        CI_Lower_Lim_Super_Physical<- Score_Super_Physical - CI_Super_Physical
        CI_Lower_Lim_Super_Physical<- round(CI_Lower_Lim_Super_Physical, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Independent_Living_1<- round(input$Cutoff_Independent_Living_1, digits = 2)
      Cutoff_Score_Independent_Living_2<- round(input$Cutoff_Independent_Living_2, digits = 2)
      Cutoff_Score_Independent_Living_3<- round(input$Cutoff_Independent_Living_3, digits = 2)
      Cutoff_Score_Relationships_1<- round(input$Cutoff_Relationships_1, digits = 2)
      Cutoff_Score_Relationships_2<- round(input$Cutoff_Relationships_2, digits = 2)
      Cutoff_Score_Relationships_3<- round(input$Cutoff_Relationships_3, digits = 2)
      Cutoff_Score_Mental_Health_1<- round(input$Cutoff_Mental_Health_1, digits = 2)
      Cutoff_Score_Mental_Health_2<- round(input$Cutoff_Mental_Health_2, digits = 2)
      Cutoff_Score_Mental_Health_3<- round(input$Cutoff_Mental_Health_3, digits = 2)
      Cutoff_Score_Coping_1<- round(input$Cutoff_Coping_1, digits = 2)
      Cutoff_Score_Coping_2<- round(input$Cutoff_Coping_2, digits = 2)
      Cutoff_Score_Coping_3<- round(input$Cutoff_Coping_3, digits = 2)
      Cutoff_Score_Pain_1<- round(input$Cutoff_Pain_1, digits = 2)
      Cutoff_Score_Pain_2<- round(input$Cutoff_Pain_2, digits = 2)
      Cutoff_Score_Pain_3<- round(input$Cutoff_Pain_3, digits = 2)
      Cutoff_Score_Senses_1<- round(input$Cutoff_Senses_1, digits = 2)
      Cutoff_Score_Senses_2<- round(input$Cutoff_Senses_2, digits = 2)
      Cutoff_Score_Senses_3<- round(input$Cutoff_Senses_3, digits = 2)
      Cutoff_Score_Self_Worth_1<- round(input$Cutoff_Self_Worth_1, digits = 2)
      Cutoff_Score_Self_Worth_2<- round(input$Cutoff_Self_Worth_2, digits = 2)
      Cutoff_Score_Self_Worth_3<- round(input$Cutoff_Self_Worth_3, digits = 2)
      Cutoff_Score_Happiness_1<- round(input$Cutoff_Happiness_1, digits = 2)
      Cutoff_Score_Happiness_2<- round(input$Cutoff_Happiness_2, digits = 2)
      Cutoff_Score_Happiness_3<- round(input$Cutoff_Happiness_3, digits = 2)
      Cutoff_Score_Super_Mental_1<- round(input$Cutoff_Super_Mental_1, digits = 2)
      Cutoff_Score_Super_Mental_2<- round(input$Cutoff_Super_Mental_2, digits = 2)
      Cutoff_Score_Super_Mental_3<- round(input$Cutoff_Super_Mental_3, digits = 2)
      Cutoff_Score_Super_Physical_1<- round(input$Cutoff_Super_Physical_1, digits = 2)
      Cutoff_Score_Super_Physical_2<- round(input$Cutoff_Super_Physical_2, digits = 2)
      Cutoff_Score_Super_Physical_3<- round(input$Cutoff_Super_Physical_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Independent_Living,Change_Independent_Living,PTS_Independent_Living, SE_Independent_Living, CI_Upper_Lim_Independent_Living, CI_Lower_Lim_Independent_Living, Cutoff_Score_Independent_Living_1,Cutoff_Score_Independent_Living_2,Cutoff_Score_Independent_Living_3,
                                      Score_Relationships,Change_Relationships, PTS_Relationships, SE_Relationships, CI_Upper_Lim_Relationships, CI_Lower_Lim_Relationships, Cutoff_Score_Relationships_1,Cutoff_Score_Relationships_2,Cutoff_Score_Relationships_3, 
                                      Score_Mental_Health,Change_Mental_Health,PTS_Mental_Health, SE_Mental_Health, CI_Upper_Lim_Mental_Health, CI_Lower_Lim_Mental_Health, Cutoff_Score_Mental_Health_1,Cutoff_Score_Mental_Health_2,Cutoff_Score_Mental_Health_3, 
                                      Score_Coping,Change_Coping,PTS_Coping, SE_Coping, CI_Upper_Lim_Coping, CI_Lower_Lim_Coping, Cutoff_Score_Coping_1,Cutoff_Score_Coping_2,Cutoff_Score_Coping_3, 
                                      Score_Pain,Change_Pain,PTS_Pain, SE_Pain, CI_Upper_Lim_Pain, CI_Lower_Lim_Pain, Cutoff_Score_Pain_1,Cutoff_Score_Pain_2,Cutoff_Score_Pain_3,
                                      Score_Senses,Change_Senses, PTS_Senses, SE_Senses, CI_Upper_Lim_Senses, CI_Lower_Lim_Senses, Cutoff_Score_Senses_1,Cutoff_Score_Senses_2,Cutoff_Score_Senses_3, 
                                      Score_Self_Worth,Change_Self_Worth,PTS_Self_Worth, SE_Self_Worth, CI_Upper_Lim_Self_Worth, CI_Lower_Lim_Self_Worth, Cutoff_Score_Self_Worth_1,Cutoff_Score_Self_Worth_2,Cutoff_Score_Self_Worth_3, 
                                      Score_Happiness,Change_Happiness,PTS_Happiness, SE_Happiness, CI_Upper_Lim_Happiness, CI_Lower_Lim_Happiness, Cutoff_Score_Happiness_1,Cutoff_Score_Happiness_2,Cutoff_Score_Happiness_3, 
                                      Score_Super_Mental,Change_Super_Mental,PTS_Super_Mental, SE_Super_Mental, CI_Upper_Lim_Super_Mental, CI_Lower_Lim_Super_Mental, Cutoff_Score_Super_Mental_1,Cutoff_Score_Super_Mental_2,Cutoff_Score_Super_Mental_3,
                                      Score_Super_Physical,Change_Super_Physical,PTS_Super_Physical, SE_Super_Physical, CI_Upper_Lim_Super_Physical, CI_Lower_Lim_Super_Physical, Cutoff_Score_Super_Physical_1,Cutoff_Score_Super_Physical_2,Cutoff_Score_Super_Physical_3)
    } else if(input$Timepoint == "3") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date_3<- input$Date_3
      Date<- c(Date_1, Date_2, Date_3)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Score_1<- sum(Score_1a, na.rm = TRUE)
      Score_1<- (1-(Score_1 - 35)/(176-35))*100
      Score_1<- round(Score_1, digits = 2)
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Score_2<- sum(Score_2a, na.rm = TRUE)
      Score_2<- (1-(Score_2 - 35)/(176-35))*100
      Score_2<- round(Score_2, digits = 2)
      Score_3a<<- as.numeric(unlist(strsplit(input$Text_3,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_3a), Score = Score_3a)
      Score_3<- sum(Score_3a, na.rm = TRUE)
      Score_3<- (1-(Score_3 - 35)/(176-35))*100
      Score_3<- round(Score_3, digits = 2)
      Score<- c(Score_1, Score_2, Score_3)
      Score_Independent_Living_1<- sum(Score_1a[c(30,3,15,9)], na.rm = TRUE)
      Score_Independent_Living_1<- (1-(Score_Independent_Living_1- 4)/(22-4))*100
      Score_Independent_Living_1<- round(Score_Independent_Living_1, digits = 2)
      Score_Independent_Living_2<- sum(Score_2a[c(30,3,15,9)], na.rm = TRUE)
      Score_Independent_Living_2<- (1-(Score_Independent_Living_2- 4)/(22-4))*100
      Score_Independent_Living_2<- round(Score_Independent_Living_2, digits = 2)
      Score_Independent_Living_3<- sum(Score_3a[c(30,3,15,9)], na.rm = TRUE)
      Score_Independent_Living_3<- (1-(Score_Independent_Living_3- 4)/(22-4))*100
      Score_Independent_Living_3<- round(Score_Independent_Living_3, digits = 2)
      Score_Independent_Living<- c(Score_Independent_Living_1, Score_Independent_Living_2, Score_Independent_Living_3)
      Score_Relationships_1<- sum(Score_1a[c(23,10,31,2,34,9,4)], na.rm = TRUE)
      Score_Relationships_1<- (1-(Score_Relationships_1 - 7)/(34-7))*100
      Score_Relationships_1<- round(Score_Relationships_1, digits = 2)
      Score_Relationships_2<- sum(Score_2a[c(23,10,31,2,34,9,4)], na.rm = TRUE)
      Score_Relationships_2<- (1-(Score_Relationships_2 - 7)/(34-7))*100
      Score_Relationships_2<- round(Score_Relationships_2, digits = 2)
      Score_Relationships_3<- sum(Score_3a[c(23,10,31,2,34,9,4)], na.rm = TRUE)
      Score_Relationships_3<- (1-(Score_Relationships_3 - 7)/(34-7))*100
      Score_Relationships_3<- round(Score_Relationships_3, digits = 2)
      Score_Relationships<- c(Score_Relationships_1,Score_Relationships_2, Score_Relationships_3)
      Score_Mental_Health_1<- sum(Score_1a[c(33,12,14,16,35,18,5,8)], na.rm = TRUE)
      Score_Mental_Health_1<- (1-(Score_Mental_Health_1 - 8)/(41-8))*100
      Score_Mental_Health_1<- round(Score_Mental_Health_1, digits = 2)
      Score_Mental_Health_2<- sum(Score_2a[c(33,12,14,16,35,18,5,8)], na.rm = TRUE)
      Score_Mental_Health_2<- (1-(Score_Mental_Health_2 - 8)/(41-8))*100
      Score_Mental_Health_2<- round(Score_Mental_Health_2, digits = 2)
      Score_Mental_Health_3<- sum(Score_3a[c(33,12,14,16,35,18,5,8)], na.rm = TRUE)
      Score_Mental_Health_3<- (1-(Score_Mental_Health_3 - 8)/(41-8))*100
      Score_Mental_Health_3<- round(Score_Mental_Health_3, digits = 2)
      Score_Mental_Health<- c(Score_Mental_Health_1, Score_Mental_Health_2, Score_Mental_Health_3)
      Score_Coping_1<- sum(Score_1a[c(1,29,21)], na.rm = TRUE)
      Score_Coping_1<- (1-(Score_Coping_1 - 3)/(15-3))*100
      Score_Coping_1<- round(Score_Coping_1, digits = 2)
      Score_Coping_2<- sum(Score_2a[c(1,29,21)], na.rm = TRUE)
      Score_Coping_2<- (1-(Score_Coping_2 - 3)/(15-3))*100
      Score_Coping_2<- round(Score_Coping_2, digits = 2)
      Score_Coping_3<- sum(Score_3a[c(1,29,21)], na.rm = TRUE)
      Score_Coping_3<- (1-(Score_Coping_3 - 3)/(15-3))*100
      Score_Coping_3<- round(Score_Coping_3, digits = 2)
      Score_Coping<- c(Score_Coping_1, Score_Coping_2, Score_Coping_3)
      Score_Pain_1<- sum(Score_1a[c(6,22,24)], na.rm = TRUE)
      Score_Pain_1<- (1-(Score_Pain_1 - 3)/(13-3))*100
      Score_Pain_1<- round(Score_Pain_1, digits = 2)
      Score_Pain_2<- sum(Score_2a[c(6,22,24)], na.rm = TRUE)
      Score_Pain_2<- (1-(Score_Pain_2 - 3)/(13-3))*100
      Score_Pain_2<- round(Score_Pain_2, digits = 2)
      Score_Pain_3<- sum(Score_3a[c(6,22,24)], na.rm = TRUE)
      Score_Pain_3<- (1-(Score_Pain_3 - 3)/(13-3))*100
      Score_Pain_3<- round(Score_Pain_3, digits = 2)
      Score_Pain<- c(Score_Pain_1, Score_Pain_2, Score_Pain_3)
      Score_Senses_1<- sum(Score_1a[c(28,32,11)], na.rm = TRUE)
      Score_Senses_1<- (1-(Score_Senses_1 - 3)/(16-3))*100
      Score_Senses_1<- round(Score_Senses_1, digits = 2)
      Score_Senses_2<- sum(Score_2a[c(28,32,11)], na.rm = TRUE)
      Score_Senses_2<- (1-(Score_Senses_2 - 3)/(16-3))*100
      Score_Senses_2<- round(Score_Senses_2, digits = 2)
      Score_Senses_3<- sum(Score_3a[c(28,32,11)], na.rm = TRUE)
      Score_Senses_3<- (1-(Score_Senses_3 - 3)/(16-3))*100
      Score_Senses_3<- round(Score_Senses_3, digits = 2)
      Score_Senses<- c(Score_Senses_1, Score_Senses_2, Score_Senses_3)
      Score_Self_Worth_1<- sum(Score_1a[c(26,13,7)], na.rm = TRUE)
      Score_Self_Worth_1<- (1-(Score_Self_Worth_1 - 3)/(15-3))*100
      Score_Self_Worth_1<- round(Score_Self_Worth_1, digits = 2)
      Score_Self_Worth_2<- sum(Score_2a[c(26,13,7)], na.rm = TRUE)
      Score_Self_Worth_2<- (1-(Score_Self_Worth_2 - 3)/(15-3))*100
      Score_Self_Worth_2<- round(Score_Self_Worth_2, digits = 2)
      Score_Self_Worth_3<- sum(Score_3a[c(26,13,7)], na.rm = TRUE)
      Score_Self_Worth_3<- (1-(Score_Self_Worth_3 - 3)/(15-3))*100
      Score_Self_Worth_3<- round(Score_Self_Worth_3, digits = 2)
      Score_Self_Worth<- c(Score_Self_Worth_1, Score_Self_Worth_2, Score_Self_Worth_3)
      Score_Happiness_1<- sum(Score_1a[c(27,17,20,25)], na.rm = TRUE)
      Score_Happiness_1<- (1-(Score_Happiness_1 - 4)/(20-4))*100
      Score_Happiness_1<- round(Score_Happiness_1, digits = 2)
      Score_Happiness_2<- sum(Score_2a[c(27,17,20,25)], na.rm = TRUE)
      Score_Happiness_2<- (1-(Score_Happiness_2 - 4)/(20-4))*100
      Score_Happiness_2<- round(Score_Happiness_2, digits = 2)
      Score_Happiness_3<- sum(Score_3a[c(27,17,20,25)], na.rm = TRUE)
      Score_Happiness_3<- (1-(Score_Happiness_3 - 4)/(20-4))*100
      Score_Happiness_3<- round(Score_Happiness_3, digits = 2)
      Score_Happiness<- c(Score_Happiness_1, Score_Happiness_2, Score_Happiness_3)
      Score_Super_Mental_1<- sum(Score_1a[c(33,12,14,16,35,18,5,8,27,17,20,25,1,29,21,23,10,31,2,34,9,4,26,13,7)], na.rm = TRUE)
      Score_Super_Mental_1<- (1-(Score_Super_Mental_1 - 25)/(125-25))*100
      Score_Super_Mental_1<- round(Score_Super_Mental_1, digits = 2)
      Score_Super_Mental_2<- sum(Score_2a[c(33,12,14,16,35,18,5,8,27,17,20,25,1,29,21,23,10,31,2,34,9,4,26,13,7)], na.rm = TRUE) 
      Score_Super_Mental_2<- (1-(Score_Super_Mental_2 - 25)/(125-25))*100
      Score_Super_Mental_2<- round(Score_Super_Mental_2, digits = 2)
      Score_Super_Mental_3<- sum(Score_3a[c(33,12,14,16,35,18,5,8,27,17,20,25,1,29,21,23,10,31,2,34,9,4,26,13,7)], na.rm = TRUE) 
      Score_Super_Mental_3<- (1-(Score_Super_Mental_3 - 25)/(125-25))*100
      Score_Super_Mental_3<- round(Score_Super_Mental_3, digits = 2)
      Score_Super_Mental<- c(Score_Super_Mental_1, Score_Super_Mental_2, Score_Super_Mental_3)
      Score_Super_Physical_1<- sum(Score_1a[c(30,3,15,9,6,22,24,28,32,11)], na.rm = TRUE) 
      Score_Super_Physical_1<- (1-(Score_Super_Physical_1 - 10)/(51-10))*100
      Score_Super_Physical_1<- round(Score_Super_Physical_1, digits = 2)
      Score_Super_Physical_2<- sum(Score_2a[c(30,3,15,9,6,22,24,28,32,11)], na.rm = TRUE) 
      Score_Super_Physical_2<- (1-(Score_Super_Physical_2 - 10)/(51-10))*100
      Score_Super_Physical_2<- round(Score_Super_Physical_2, digits = 2)
      Score_Super_Physical_3<- sum(Score_3a[c(30,3,15,9,6,22,24,28,32,11)], na.rm = TRUE) 
      Score_Super_Physical_3<- (1-(Score_Super_Physical_3 - 10)/(51-10))*100
      Score_Super_Physical_3<- round(Score_Super_Physical_3, digits = 2)
      Score_Super_Physical<- c(Score_Super_Physical_1, Score_Super_Physical_2, Score_Super_Physical_3)
      Change<- c(0, (Score_2 - Score_1), (Score_3 - Score_2))
      Change<- round(Change, digits = 2)
      Change_Independent_Living<- c(0, (Score_Independent_Living_2 - Score_Independent_Living_1), (Score_Independent_Living_3  - Score_Independent_Living_2))
      Change_Independent_Living<- round(Change_Independent_Living, digits = 2)
      Change_Relationships<- c(0, (Score_Relationships_2 - Score_Relationships_1), (Score_Relationships_3 - Score_Relationships_2))
      Change_Relationships<- round(Change_Relationships, digits = 2)
      Change_Mental_Health<- c(0, (Score_Mental_Health_2 - Score_Mental_Health_1),  (Score_Mental_Health_3 - Score_Mental_Health_2))
      Change_Mental_Health<- round(Change_Mental_Health, digits = 2)
      Change_Coping<- c(0, (Score_Coping_2 - Score_Coping_1), (Score_Coping_3 - Score_Coping_2))
      Change_Coping<- round(Change_Coping, digits = 2)
      Change_Pain<- c(0, (Score_Pain_2 - Score_Pain_1), (Score_Pain_3 - Score_Pain_2))
      Change_Pain<- round(Change_Pain, digits = 2)
      Change_Senses<- c(0, (Score_Senses_2 - Score_Senses_1), (Score_Senses_3 - Score_Senses_2))
      Change_Senses<- round(Change_Senses, digits = 2)
      Change_Self_Worth<- c(0, (Score_Self_Worth_2 - Score_Self_Worth_1), (Score_Self_Worth_3 - Score_Self_Worth_2))
      Change_Self_Worth<- round(Change_Self_Worth, digits = 2)
      Change_Happiness<- c(0, (Score_Happiness_2 - Score_Happiness_1), (Score_Happiness_3 - Score_Happiness_2))
      Change_Happiness<- round(Change_Happiness, digits = 2)
      Change_Super_Mental<- c(0, (Score_Super_Mental_2 - Score_Super_Mental_1),  (Score_Super_Mental_3 - Score_Super_Mental_2))
      Change_Super_Mental<- round(Change_Super_Mental, digits = 2)
      Change_Super_Physical<- c(0, (Score_Super_Physical_2 - Score_Super_Physical_1),  (Score_Super_Physical_3 - Score_Super_Physical_2))
      Change_Super_Physical<- round(Change_Super_Physical, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS_3<- (Rel * Score_3) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Independent_Living_1<- (Rel_Independent_Living * Score_Independent_Living_1) + (M_Independent_Living * (1 - Rel_Independent_Living))
        PTS_Independent_Living_2<- (Rel_Independent_Living * Score_Independent_Living_2) + (M_Independent_Living * (1 - Rel_Independent_Living))
        PTS_Independent_Living_3<- (Rel_Independent_Living * Score_Independent_Living_3) + (M_Independent_Living * (1 - Rel_Independent_Living))
        PTS_Independent_Living<- c(PTS_Independent_Living_1, PTS_Independent_Living_2, PTS_Independent_Living_3)
        PTS_Relationships_1<- (Rel_Relationships * Score_Relationships_1) + (M_Relationships * (1 - Rel_Relationships))
        PTS_Relationships_2<- (Rel_Relationships * Score_Relationships_2) + (M_Relationships * (1 - Rel_Relationships))
        PTS_Relationships_3<- (Rel_Relationships * Score_Relationships_3) + (M_Relationships * (1 - Rel_Relationships))
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2, PTS_Relationships_3)
        PTS_Mental_Health_1<- (Rel_Mental_Health * Score_Mental_Health_1) + (M_Mental_Health * (1 - Rel_Mental_Health))
        PTS_Mental_Health_2<- (Rel_Mental_Health * Score_Mental_Health_2) + (M_Mental_Health * (1 - Rel_Mental_Health))
        PTS_Mental_Health_3<- (Rel_Mental_Health * Score_Mental_Health_3) + (M_Mental_Health * (1 - Rel_Mental_Health))
        PTS_Mental_Health<- c(PTS_Mental_Health_1, PTS_Mental_Health_2, PTS_Mental_Health_3)
        PTS_Coping_1<- (Rel_Coping * Score_Coping_1) + (M_Coping * (1 - Rel_Coping))
        PTS_Coping_2<- (Rel_Coping * Score_Coping_2) + (M_Coping * (1 - Rel_Coping))
        PTS_Coping_3<- (Rel_Coping * Score_Coping_3) + (M_Coping * (1 - Rel_Coping))
        PTS_Coping<- c(PTS_Coping_1, PTS_Coping_2,  PTS_Coping_3)
        PTS_Pain_1<- (Rel_Pain * Score_Pain_1) + (M_Pain * (1 - Rel_Pain))
        PTS_Pain_2<- (Rel_Pain * Score_Pain_2) + (M_Pain * (1 - Rel_Pain))
        PTS_Pain_3<- (Rel_Pain * Score_Pain_3) + (M_Pain * (1 - Rel_Pain))
        PTS_Pain<- c(PTS_Pain_1, PTS_Pain_2, PTS_Pain_3)
        PTS_Senses_1<- (Rel_Senses * Score_Senses_1) + (M_Senses * (1 - Rel_Senses))
        PTS_Senses_2<- (Rel_Senses * Score_Senses_2) + (M_Senses * (1 - Rel_Senses))
        PTS_Senses_3<- (Rel_Senses * Score_Senses_3) + (M_Senses * (1 - Rel_Senses))
        PTS_Senses<- c(PTS_Senses_1, PTS_Senses_2, PTS_Senses_3)
        PTS_Self_Worth_1<- (Rel_Self_Worth * Score_Self_Worth_1) + (M_Self_Worth * (1 - Rel_Self_Worth))
        PTS_Self_Worth_2<- (Rel_Self_Worth * Score_Self_Worth_2) + (M_Self_Worth * (1 - Rel_Self_Worth))
        PTS_Self_Worth_3<- (Rel_Self_Worth * Score_Self_Worth_3) + (M_Self_Worth * (1 - Rel_Self_Worth))
        PTS_Self_Worth<- c(PTS_Self_Worth_1, PTS_Self_Worth_2, PTS_Self_Worth_3)
        PTS_Happiness_1<- (Rel_Happiness * Score_Happiness_1) + (M_Happiness * (1 - Rel_Happiness))
        PTS_Happiness_2<- (Rel_Happiness * Score_Happiness_2) + (M_Happiness * (1 - Rel_Happiness))
        PTS_Happiness_3<- (Rel_Happiness * Score_Happiness_3) + (M_Happiness * (1 - Rel_Happiness))
        PTS_Happiness<- c(PTS_Happiness_1, PTS_Happiness_2, PTS_Happiness_3)
        PTS_Super_Mental_1<- (Rel_Super_Mental * Score_Super_Mental_1) + (M_Super_Mental * (1 - Rel_Super_Mental))
        PTS_Super_Mental_2<- (Rel_Super_Mental * Score_Super_Mental_2) + (M_Super_Mental * (1 - Rel_Super_Mental))
        PTS_Super_Mental_3<- (Rel_Super_Mental * Score_Super_Mental_3) + (M_Super_Mental * (1 - Rel_Super_Mental))
        PTS_Super_Mental<- c(PTS_Super_Mental_1, PTS_Super_Mental_2, PTS_Super_Mental_3)
        PTS_Super_Physical_1<- (Rel_Super_Physical * Score_Super_Physical_1) + (M_Super_Physical * (1 - Rel_Super_Physical))
        PTS_Super_Physical_2<- (Rel_Super_Physical * Score_Super_Physical_2) + (M_Super_Physical * (1 - Rel_Super_Physical))
        PTS_Super_Physical_3<- (Rel_Super_Physical * Score_Super_Physical_3) + (M_Super_Physical * (1 - Rel_Super_Physical))
        PTS_Super_Physical<- c(PTS_Super_Physical_1, PTS_Super_Physical_2, PTS_Super_Physical_3)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Independent_Living_1<- Score_Independent_Living_1 + (M_Retest_Independent_Living - M_Independent_Living)  
        PTS_Independent_Living_2<- Score_Independent_Living_2 + (M_Retest_Independent_Living - M_Independent_Living) 
        PTS_Independent_Living_3<- Score_Independent_Living_3 + (M_Retest_Independent_Living - M_Independent_Living) 
        PTS_Independent_Living<- c(PTS_Independent_Living_1, PTS_Independent_Living_2, PTS_Independent_Living_3)
        PTS_Relationships_1<- Score_Relationships_1 + (M_Retest_Relationships - M_Relationships)  
        PTS_Relationships_2<- Score_Relationships_2 + (M_Retest_Relationships - M_Relationships) 
        PTS_Relationships_3<- Score_Relationships_3 + (M_Retest_Relationships - M_Relationships) 
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2, PTS_Relationships_3)
        PTS_Mental_Health_1<- Score_Mental_Health_1 + (M_Retest_Mental_Health - M_Mental_Health)  
        PTS_Mental_Health_2<- Score_Mental_Health_2 + (M_Retest_Mental_Health - M_Mental_Health) 
        PTS_Mental_Health_3<- Score_Mental_Health_3 + (M_Retest_Mental_Health - M_Mental_Health) 
        PTS_Mental_Health<- c(PTS_Mental_Health_1, PTS_Mental_Health_2, PTS_Mental_Health_3)
        PTS_Coping_1<- Score_Coping_1 + (M_Retest_Coping - M_Coping)  
        PTS_Coping_2<- Score_Coping_2 + (M_Retest_Coping - M_Coping) 
        PTS_Coping_3<- Score_Coping_3 + (M_Retest_Coping - M_Coping) 
        PTS_Coping<- c(PTS_Coping_1, PTS_Coping_2, PTS_Coping_3)
        PTS_Pain_1<- Score_Pain_1 + (M_Retest_Pain - M_Pain)  
        PTS_Pain_2<- Score_Pain_2 + (M_Retest_Pain - M_Pain) 
        PTS_Pain_3<- Score_Pain_3 + (M_Retest_Pain - M_Pain) 
        PTS_Pain<- c(PTS_Pain_1, PTS_Pain_2, PTS_Pain_3)
        PTS_Senses_1<- Score_Senses_1 + (M_Retest_Senses - M_Senses)  
        PTS_Senses_2<- Score_Senses_2 + (M_Retest_Senses - M_Senses) 
        PTS_Senses_3<- Score_Senses_3 + (M_Retest_Senses - M_Senses) 
        PTS_Senses<- c(PTS_Senses_1, PTS_Senses_2, PTS_Senses_3)
        PTS_Self_Worth_1<- Score_Self_Worth_1 + (M_Retest_Self_Worth - M_Self_Worth)  
        PTS_Self_Worth_2<- Score_Self_Worth_2 + (M_Retest_Self_Worth - M_Self_Worth) 
        PTS_Self_Worth_3<- Score_Self_Worth_3 + (M_Retest_Self_Worth - M_Self_Worth) 
        PTS_Self_Worth<- c(PTS_Self_Worth_1, PTS_Self_Worth_2, PTS_Self_Worth_3)
        PTS_Happiness_1<- Score_Happiness_1 + (M_Retest_Happiness - M_Happiness)  
        PTS_Happiness_2<- Score_Happiness_2 + (M_Retest_Happiness - M_Happiness) 
        PTS_Happiness_3<- Score_Happiness_3 + (M_Retest_Happiness - M_Happiness) 
        PTS_Happiness<- c(PTS_Happiness_1, PTS_Happiness_2, PTS_Happiness_3)
        PTS_Super_Mental_1<- Score_Super_Mental_1 + (M_Retest_Super_Mental - M_Super_Mental)  
        PTS_Super_Mental_2<- Score_Super_Mental_2 + (M_Retest_Super_Mental - M_Super_Mental) 
        PTS_Super_Mental_3<- Score_Super_Mental_3 + (M_Retest_Super_Mental - M_Super_Mental) 
        PTS_Super_Mental<- c(PTS_Super_Mental_1, PTS_Super_Mental_2, PTS_Super_Mental_3)
        PTS_Super_Physical_1<- Score_Super_Physical_1 + (M_Retest_Super_Physical - M_Super_Physical)  
        PTS_Super_Physical_2<- Score_Super_Physical_2 + (M_Retest_Super_Physical - M_Super_Physical) 
        PTS_Super_Physical_3<- Score_Super_Physical_3 + (M_Retest_Super_Physical - M_Super_Physical) 
        PTS_Super_Physical<- c(PTS_Super_Physical_1, PTS_Super_Physical_2, PTS_Super_Physical_3)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Independent_Living<- Score_Independent_Living
        PTS_Relationships<- Score_Relationships
        PTS_Mental_Health<- Score_Mental_Health
        PTS_Coping<- Score_Coping
        PTS_Pain<- Score_Pain
        PTS_Senses<- Score_Senses
        PTS_Self_Worth<- Score_Self_Worth
        PTS_Happiness<- Score_Happiness
        PTS_Super_Mental<- Score_Super_Mental
        PTS_Super_Physical<- Score_Super_Physical
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS_3<- (B_Adj * Score_3) + A_Adj
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Independent_Living<- Rel_Independent_Living * (SD_Retest_Independent_Living/SD_Independent_Living)
        A_Constant_Independent_Living<- M_Retest_Independent_Living - (B_Slope_Independent_Living * M_Independent_Living)
        B_Adj_Independent_Living<- SD_Retest_Independent_Living/SD_Independent_Living
        A_Adj_Independent_Living<- M_Retest_Independent_Living - (B_Adj_Independent_Living * M_Independent_Living)
        PTS_Independent_Living_1<- (B_Adj_Independent_Living * Score_Independent_Living_1) + A_Adj_Independent_Living
        PTS_Independent_Living_2<- (B_Adj_Independent_Living * Score_Independent_Living_2) + A_Adj_Independent_Living
        PTS_Independent_Living_3<- (B_Adj_Independent_Living * Score_Independent_Living_3) + A_Adj_Independent_Living
        PTS_Independent_Living<- c(PTS_Independent_Living_1,PTS_Independent_Living_2, PTS_Independent_Living_3)
        B_Slope_Relationships<- Rel_Relationships * (SD_Retest_Relationships/SD_Relationships)
        A_Constant_Relationships<- M_Retest_Relationships - (B_Slope_Relationships * M_Relationships)
        B_Adj_Relationships<- SD_Retest_Relationships/SD_Relationships
        A_Adj_Relationships<- M_Retest_Relationships - (B_Adj_Relationships * M_Relationships)
        PTS_Relationships_1<- (B_Adj_Relationships * Score_Relationships_1) + A_Adj_Relationships
        PTS_Relationships_2<- (B_Adj_Relationships * Score_Relationships_2) + A_Adj_Relationships
        PTS_Relationships_3<- (B_Adj_Relationships * Score_Relationships_3) + A_Adj_Relationships
        PTS_Relationships<- c(PTS_Relationships_1,PTS_Relationships_2, PTS_Relationships_3)
        B_Slope_Mental_Health<- Rel_Mental_Health * (SD_Retest_Mental_Health/SD_Mental_Health)
        A_Constant_Mental_Health<- M_Retest_Mental_Health - (B_Slope_Mental_Health * M_Mental_Health)
        B_Adj_Mental_Health<- SD_Retest_Mental_Health/SD_Mental_Health
        A_Adj_Mental_Health<- M_Retest_Mental_Health - (B_Adj_Mental_Health * M_Mental_Health)
        PTS_Mental_Health_1<- (B_Adj_Mental_Health * Score_Mental_Health_1) + A_Adj_Mental_Health
        PTS_Mental_Health_2<- (B_Adj_Mental_Health * Score_Mental_Health_2) + A_Adj_Mental_Health
        PTS_Mental_Health_3<- (B_Adj_Mental_Health * Score_Mental_Health_3) + A_Adj_Mental_Health
        PTS_Mental_Health<- c(PTS_Mental_Health_1,PTS_Mental_Health_2, PTS_Mental_Health_3)
        B_Slope_Coping<- Rel_Coping * (SD_Retest_Coping/SD_Coping)
        A_Constant_Coping<- M_Retest_Coping - (B_Slope_Coping * M_Coping)
        B_Adj_Coping<- SD_Retest_Coping/SD_Coping
        A_Adj_Coping<- M_Retest_Coping - (B_Adj_Coping * M_Coping)
        PTS_Coping_1<- (B_Adj_Coping * Score_Coping_1) + A_Adj_Coping
        PTS_Coping_2<- (B_Adj_Coping * Score_Coping_2) + A_Adj_Coping
        PTS_Coping_3<- (B_Adj_Coping * Score_Coping_3) + A_Adj_Coping
        PTS_Coping<- c(PTS_Coping_1,PTS_Coping_2, PTS_Coping_3)
        B_Slope_Pain<- Rel_Pain * (SD_Retest_Pain/SD_Pain)
        A_Constant_Pain<- M_Retest_Pain - (B_Slope_Pain * M_Pain)
        B_Adj_Pain<- SD_Retest_Pain/SD_Pain
        A_Adj_Pain<- M_Retest_Pain - (B_Adj_Pain * M_Pain)
        PTS_Pain_1<- (B_Adj_Pain * Score_Pain_1) + A_Adj_Pain
        PTS_Pain_2<- (B_Adj_Pain * Score_Pain_2) + A_Adj_Pain
        PTS_Pain_3<- (B_Adj_Pain * Score_Pain_3) + A_Adj_Pain
        PTS_Pain<- c(PTS_Pain_1,PTS_Pain_2, PTS_Pain_3)
        B_Slope_Senses<- Rel_Senses * (SD_Retest_Senses/SD_Senses)
        A_Constant_Senses<- M_Retest_Senses - (B_Slope_Senses * M_Senses)
        B_Adj_Senses<- SD_Retest_Senses/SD_Senses
        A_Adj_Senses<- M_Retest_Senses - (B_Adj_Senses * M_Senses)
        PTS_Senses_1<- (B_Adj_Senses * Score_Senses_1) + A_Adj_Senses
        PTS_Senses_2<- (B_Adj_Senses * Score_Senses_2) + A_Adj_Senses
        PTS_Senses_3<- (B_Adj_Senses * Score_Senses_3) + A_Adj_Senses
        PTS_Senses<- c(PTS_Senses_1,PTS_Senses_2, PTS_Senses_3)
        B_Slope_Self_Worth<- Rel_Self_Worth * (SD_Retest_Self_Worth/SD_Self_Worth)
        A_Constant_Self_Worth<- M_Retest_Self_Worth - (B_Slope_Self_Worth * M_Self_Worth)
        B_Adj_Self_Worth<- SD_Retest_Self_Worth/SD_Self_Worth
        A_Adj_Self_Worth<- M_Retest_Self_Worth - (B_Adj_Self_Worth * M_Self_Worth)
        PTS_Self_Worth_1<- (B_Adj_Self_Worth * Score_Self_Worth_1) + A_Adj_Self_Worth
        PTS_Self_Worth_2<- (B_Adj_Self_Worth * Score_Self_Worth_2) + A_Adj_Self_Worth
        PTS_Self_Worth_3<- (B_Adj_Self_Worth * Score_Self_Worth_3) + A_Adj_Self_Worth
        PTS_Self_Worth<- c(PTS_Self_Worth_1,PTS_Self_Worth_2, PTS_Self_Worth_3)
        B_Slope_Happiness<- Rel_Happiness * (SD_Retest_Happiness/SD_Happiness)
        A_Constant_Happiness<- M_Retest_Happiness - (B_Slope_Happiness * M_Happiness)
        B_Adj_Happiness<- SD_Retest_Happiness/SD_Happiness
        A_Adj_Happiness<- M_Retest_Happiness - (B_Adj_Happiness * M_Happiness)
        PTS_Happiness_1<- (B_Adj_Happiness * Score_Happiness_1) + A_Adj_Happiness
        PTS_Happiness_2<- (B_Adj_Happiness * Score_Happiness_2) + A_Adj_Happiness
        PTS_Happiness_3<- (B_Adj_Happiness * Score_Happiness_3) + A_Adj_Happiness
        PTS_Happiness<- c(PTS_Happiness_1,PTS_Happiness_2, PTS_Happiness_3)
        B_Slope_Super_Mental<- Rel_Super_Mental * (SD_Retest_Super_Mental/SD_Super_Mental)
        A_Constant_Super_Mental<- M_Retest_Super_Mental - (B_Slope_Super_Mental * M_Super_Mental)
        B_Adj_Super_Mental<- SD_Retest_Super_Mental/SD_Super_Mental
        A_Adj_Super_Mental<- M_Retest_Super_Mental - (B_Adj_Super_Mental * M_Super_Mental)
        PTS_Super_Mental_1<- (B_Adj_Super_Mental * Score_Super_Mental_1) + A_Adj_Super_Mental
        PTS_Super_Mental_2<- (B_Adj_Super_Mental * Score_Super_Mental_2) + A_Adj_Super_Mental
        PTS_Super_Mental_3<- (B_Adj_Super_Mental * Score_Super_Mental_3) + A_Adj_Super_Mental
        PTS_Super_Mental<- c(PTS_Super_Mental_1,PTS_Super_Mental_2, PTS_Super_Mental_3)
        B_Slope_Super_Physical<- Rel_Super_Physical * (SD_Retest_Super_Physical/SD_Super_Physical)
        A_Constant_Super_Physical<- M_Retest_Super_Physical - (B_Slope_Super_Physical * M_Super_Physical)
        B_Adj_Super_Physical<- SD_Retest_Super_Physical/SD_Super_Physical
        A_Adj_Super_Physical<- M_Retest_Super_Physical - (B_Adj_Super_Physical * M_Super_Physical)
        PTS_Super_Physical_1<- (B_Adj_Super_Physical * Score_Super_Physical_1) + A_Adj_Super_Physical
        PTS_Super_Physical_2<- (B_Adj_Super_Physical * Score_Super_Physical_2) + A_Adj_Super_Physical
        PTS_Super_Physical_3<- (B_Adj_Super_Physical * Score_Super_Physical_3) + A_Adj_Super_Physical
        PTS_Super_Physical<- c(PTS_Super_Physical_1,PTS_Super_Physical_2, PTS_Super_Physical_3)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS_3<- B_Slope*Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Independent_Living<- Rel_Independent_Living * (SD_Retest_Independent_Living/SD_Independent_Living)
        PTS_Independent_Living_1<- B_Slope_Independent_Living * Score_Independent_Living_1
        PTS_Independent_Living_2<- B_Slope_Independent_Living * Score_Independent_Living_2
        PTS_Independent_Living_3<- B_Slope_Independent_Living * Score_Independent_Living_3
        PTS_Independent_Living<- c(PTS_Independent_Living_1, PTS_Independent_Living_2, PTS_Independent_Living_3)
        B_Slope_Relationships<- Rel_Relationships * (SD_Retest_Relationships/SD_Relationships)
        PTS_Relationships_1<- B_Slope_Relationships * Score_Relationships_1
        PTS_Relationships_2<- B_Slope_Relationships * Score_Relationships_2
        PTS_Relationships_3<- B_Slope_Relationships * Score_Relationships_3
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2, PTS_Relationships_3)
        B_Slope_Mental_Health<- Rel_Mental_Health * (SD_Retest_Mental_Health/SD_Mental_Health)
        PTS_Mental_Health_1<- B_Slope_Mental_Health * Score_Mental_Health_1
        PTS_Mental_Health_2<- B_Slope_Mental_Health * Score_Mental_Health_2
        PTS_Mental_Health_3<- B_Slope_Mental_Health * Score_Mental_Health_3
        PTS_Mental_Health<- c(PTS_Mental_Health_1, PTS_Mental_Health_2, PTS_Mental_Health_3)
        B_Slope_Coping<- Rel_Coping * (SD_Retest_Coping/SD_Coping)
        PTS_Coping_1<- B_Slope_Coping * Score_Coping_1
        PTS_Coping_2<- B_Slope_Coping * Score_Coping_2
        PTS_Coping_3<- B_Slope_Coping * Score_Coping_3
        PTS_Coping<- c(PTS_Coping_1, PTS_Coping_2, PTS_Coping_3)
        B_Slope_Pain<- Rel_Pain * (SD_Retest_Pain/SD_Pain)
        PTS_Pain_1<- B_Slope_Pain * Score_Pain_1
        PTS_Pain_2<- B_Slope_Pain * Score_Pain_2
        PTS_Pain_3<- B_Slope_Pain * Score_Pain_3
        PTS_Pain<- c(PTS_Pain_1, PTS_Pain_2, PTS_Pain_3)
        B_Slope_Senses<- Rel_Senses * (SD_Retest_Senses/SD_Senses)
        PTS_Senses_1<- B_Slope_Senses * Score_Senses_1
        PTS_Senses_2<- B_Slope_Senses * Score_Senses_2
        PTS_Senses_3<- B_Slope_Senses * Score_Senses_3
        PTS_Senses<- c(PTS_Senses_1, PTS_Senses_2, PTS_Senses_3)
        B_Slope_Self_Worth<- Rel_Self_Worth * (SD_Retest_Self_Worth/SD_Self_Worth)
        PTS_Self_Worth_1<- B_Slope_Self_Worth * Score_Self_Worth_1
        PTS_Self_Worth_2<- B_Slope_Self_Worth * Score_Self_Worth_2
        PTS_Self_Worth_3<- B_Slope_Self_Worth * Score_Self_Worth_3
        PTS_Self_Worth<- c(PTS_Self_Worth_1, PTS_Self_Worth_2, PTS_Self_Worth_3) 
        B_Slope_Happiness<- Rel_Happiness * (SD_Retest_Happiness/SD_Happiness)
        PTS_Happiness_1<- B_Slope_Happiness * Score_Happiness_1
        PTS_Happiness_2<- B_Slope_Happiness * Score_Happiness_2
        PTS_Happiness_3<- B_Slope_Happiness * Score_Happiness_3
        PTS_Happiness<- c(PTS_Happiness_1, PTS_Happiness_2, PTS_Happiness_3)
        B_Slope_Super_Mental<- Rel_Super_Mental * (SD_Retest_Super_Mental/SD_Super_Mental)
        PTS_Super_Mental_1<- B_Slope_Super_Mental * Score_Super_Mental_1
        PTS_Super_Mental_2<- B_Slope_Super_Mental * Score_Super_Mental_2
        PTS_Super_Mental_3<- B_Slope_Super_Mental * Score_Super_Mental_3
        PTS_Super_Mental<- c(PTS_Super_Mental_1, PTS_Super_Mental_2, PTS_Super_Mental_3)
        B_Slope_Super_Physical<- Rel_Super_Physical * (SD_Retest_Super_Physical/SD_Super_Physical)
        PTS_Super_Physical_1<- B_Slope_Super_Physical * Score_Super_Physical_1
        PTS_Super_Physical_2<- B_Slope_Super_Physical * Score_Super_Physical_2
        PTS_Super_Physical_3<- B_Slope_Super_Physical * Score_Super_Physical_3
        PTS_Super_Physical<- c(PTS_Super_Physical_1, PTS_Super_Physical_2, PTS_Super_Physical_3)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Independent_Living_1<- Score_Independent_Living_1 + (M_Retest_Independent_Living - M_Independent_Living)
        PTS_Independent_Living_2<- Score_Independent_Living_2 + (M_Retest_Independent_Living - M_Independent_Living)
        PTS_Independent_Living_3<- Score_Independent_Living_3 + (M_Retest_Independent_Living - M_Independent_Living)
        PTS_Independent_Living<- c(PTS_Independent_Living_1, PTS_Independent_Living_2, PTS_Independent_Living_3)
        PTS_Relationships_1<- Score_Relationships_1 + (M_Retest_Relationships - M_Relationships)
        PTS_Relationships_2<- Score_Relationships_2 + (M_Retest_Relationships - M_Relationships)
        PTS_Relationships_3<- Score_Relationships_3 + (M_Retest_Relationships - M_Relationships)
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2, PTS_Relationships_3)
        PTS_Mental_Health_1<- Score_Mental_Health_1 + (M_Retest_Mental_Health - M_Mental_Health)
        PTS_Mental_Health_2<- Score_Mental_Health_2 + (M_Retest_Mental_Health - M_Mental_Health)
        PTS_Mental_Health_3<- Score_Mental_Health_3 + (M_Retest_Mental_Health - M_Mental_Health)
        PTS_Mental_Health<- c(PTS_Mental_Health_1, PTS_Mental_Health_2, PTS_Mental_Health_3)
        PTS_Coping_1<- Score_Coping_1 + (M_Retest_Coping - M_Coping)
        PTS_Coping_2<- Score_Coping_2 + (M_Retest_Coping - M_Coping)
        PTS_Coping_3<- Score_Coping_3 + (M_Retest_Coping - M_Coping)
        PTS_Coping<- c(PTS_Coping_1, PTS_Coping_2, PTS_Coping_3)
        PTS_Pain_1<- Score_Pain_1 + (M_Retest_Pain - M_Pain)
        PTS_Pain_2<- Score_Pain_2 + (M_Retest_Pain - M_Pain)
        PTS_Pain_3<- Score_Pain_3 + (M_Retest_Pain - M_Pain)
        PTS_Pain<- c(PTS_Pain_1, PTS_Pain_2, PTS_Pain_3)
        PTS_Senses_1<- Score_Senses_1 + (M_Retest_Senses - M_Senses)
        PTS_Senses_2<- Score_Senses_2 + (M_Retest_Senses - M_Senses)
        PTS_Senses_3<- Score_Senses_3 + (M_Retest_Senses - M_Senses)
        PTS_Senses<- c(PTS_Senses_1, PTS_Senses_2, PTS_Senses_3)
        PTS_Self_Worth_1<- Score_Self_Worth_1 + (M_Retest_Self_Worth - M_Self_Worth)
        PTS_Self_Worth_2<- Score_Self_Worth_2 + (M_Retest_Self_Worth - M_Self_Worth)
        PTS_Self_Worth_3<- Score_Self_Worth_3 + (M_Retest_Self_Worth - M_Self_Worth)
        PTS_Self_Worth<- c(PTS_Self_Worth_1, PTS_Self_Worth_2, PTS_Self_Worth_3)
        PTS_Happiness_1<- Score_Happiness_1 + (M_Retest_Happiness - M_Happiness)
        PTS_Happiness_2<- Score_Happiness_2 + (M_Retest_Happiness - M_Happiness)
        PTS_Happiness_3<- Score_Happiness_3 + (M_Retest_Happiness - M_Happiness)
        PTS_Happiness<- c(PTS_Happiness_1, PTS_Happiness_2, PTS_Happiness_3)
        PTS_Super_Mental_1<- Score_Super_Mental_1 + (M_Retest_Super_Mental - M_Super_Mental)
        PTS_Super_Mental_2<- Score_Super_Mental_2 + (M_Retest_Super_Mental - M_Super_Mental)
        PTS_Super_Mental_3<- Score_Super_Mental_3 + (M_Retest_Super_Mental - M_Super_Mental)
        PTS_Super_Mental<- c(PTS_Super_Mental_1, PTS_Super_Mental_2, PTS_Super_Mental_3)
        PTS_Super_Physical_1<- Score_Super_Physical_1 + (M_Retest_Super_Physical - M_Super_Physical)
        PTS_Super_Physical_2<- Score_Super_Physical_2 + (M_Retest_Super_Physical - M_Super_Physical)
        PTS_Super_Physical_3<- Score_Super_Physical_3 + (M_Retest_Super_Physical - M_Super_Physical)
        PTS_Super_Physical<- c(PTS_Super_Physical_1, PTS_Super_Physical_2, PTS_Super_Physical_3)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Independent_Living<- round(PTS_Independent_Living, digits = 2)
      PTS_Relationships<- round(PTS_Relationships, digits = 2)
      PTS_Mental_Health<- round(PTS_Mental_Health, digits = 2)
      PTS_Coping<- round(PTS_Coping, digits = 2)
      PTS_Pain<- round(PTS_Pain, digits = 2)
      PTS_Senses<- round(PTS_Senses, digits = 2)
      PTS_Self_Worth<- round(PTS_Self_Worth, digits = 2)
      PTS_Happiness<- round(PTS_Happiness, digits = 2)
      PTS_Super_Mental<- round(PTS_Super_Mental, digits = 2)
      PTS_Super_Physical<- round(PTS_Super_Physical, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE3<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_3 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2, SE3)
        SE_Independent_Living_1<- McSweeny_SE_Independent_Living*sqrt(1 + (1/SampleN) + ((Score_Independent_Living_1 - M_Independent_Living)^2/(SD_Independent_Living^2*(SampleN-1))))
        SE_Independent_Living_2<- McSweeny_SE_Independent_Living*sqrt(1 + (1/SampleN) + ((Score_Independent_Living_2 - M_Independent_Living)^2/(SD_Independent_Living^2*(SampleN-1))))
        SE_Independent_Living_3<- McSweeny_SE_Independent_Living*sqrt(1 + (1/SampleN) + ((Score_Independent_Living_3 - M_Independent_Living)^2/(SD_Independent_Living^2*(SampleN-1))))
        SE_Independent_Living<-c(SE_Independent_Living_1, SE_Independent_Living_2, SE_Independent_Living_3)
        SE_Relationships_1<- McSweeny_SE_Relationships*sqrt(1 + (1/SampleN) + ((Score_Relationships_1 - M_Relationships)^2/(SD_Relationships^2*(SampleN-1))))
        SE_Relationships_2<- McSweeny_SE_Relationships*sqrt(1 + (1/SampleN) + ((Score_Relationships_2 - M_Relationships)^2/(SD_Relationships^2*(SampleN-1))))
        SE_Relationships_3<- McSweeny_SE_Relationships*sqrt(1 + (1/SampleN) + ((Score_Relationships_3 - M_Relationships)^2/(SD_Relationships^2*(SampleN-1))))
        SE_Relationships<-c(SE_Relationships_1, SE_Relationships_2, SE_Relationships_3)
        SE_Mental_Health_1<- McSweeny_SE_Mental_Health*sqrt(1 + (1/SampleN) + ((Score_Mental_Health_1 - M_Mental_Health)^2/(SD_Mental_Health^2*(SampleN-1))))
        SE_Mental_Health_2<- McSweeny_SE_Mental_Health*sqrt(1 + (1/SampleN) + ((Score_Mental_Health_2 - M_Mental_Health)^2/(SD_Mental_Health^2*(SampleN-1))))
        SE_Mental_Health_3<- McSweeny_SE_Mental_Health*sqrt(1 + (1/SampleN) + ((Score_Mental_Health_3 - M_Mental_Health)^2/(SD_Mental_Health^2*(SampleN-1))))
        SE_Mental_Health<-c(SE_Mental_Health_1, SE_Mental_Health_2, SE_Mental_Health_3)
        SE_Coping_1<- McSweeny_SE_Coping*sqrt(1 + (1/SampleN) + ((Score_Coping_1 - M_Coping)^2/(SD_Coping^2*(SampleN-1))))
        SE_Coping_2<- McSweeny_SE_Coping*sqrt(1 + (1/SampleN) + ((Score_Coping_2 - M_Coping)^2/(SD_Coping^2*(SampleN-1))))
        SE_Coping_3<- McSweeny_SE_Coping*sqrt(1 + (1/SampleN) + ((Score_Coping_3 - M_Coping)^2/(SD_Coping^2*(SampleN-1))))
        SE_Coping<-c(SE_Coping_1, SE_Coping_2, SE_Coping_3)
        SE_Pain_1<- McSweeny_SE_Pain*sqrt(1 + (1/SampleN) + ((Score_Pain_1 - M_Pain)^2/(SD_Pain^2*(SampleN-1))))
        SE_Pain_2<- McSweeny_SE_Pain*sqrt(1 + (1/SampleN) + ((Score_Pain_2 - M_Pain)^2/(SD_Pain^2*(SampleN-1))))
        SE_Pain_3<- McSweeny_SE_Pain*sqrt(1 + (1/SampleN) + ((Score_Pain_3 - M_Pain)^2/(SD_Pain^2*(SampleN-1))))
        SE_Pain<-c(SE_Pain_1, SE_Pain_2, SE_Pain_3)
        SE_Senses_1<- McSweeny_SE_Senses*sqrt(1 + (1/SampleN) + ((Score_Senses_1 - M_Senses)^2/(SD_Senses^2*(SampleN-1))))
        SE_Senses_2<- McSweeny_SE_Senses*sqrt(1 + (1/SampleN) + ((Score_Senses_2 - M_Senses)^2/(SD_Senses^2*(SampleN-1))))
        SE_Senses_3<- McSweeny_SE_Senses*sqrt(1 + (1/SampleN) + ((Score_Senses_3 - M_Senses)^2/(SD_Senses^2*(SampleN-1))))
        SE_Senses<-c(SE_Senses_1, SE_Senses_2, SE_Senses_3)
        SE_Self_Worth_1<- McSweeny_SE_Self_Worth*sqrt(1 + (1/SampleN) + ((Score_Self_Worth_1 - M_Self_Worth)^2/(SD_Self_Worth^2*(SampleN-1))))
        SE_Self_Worth_2<- McSweeny_SE_Self_Worth*sqrt(1 + (1/SampleN) + ((Score_Self_Worth_2 - M_Self_Worth)^2/(SD_Self_Worth^2*(SampleN-1))))
        SE_Self_Worth_3<- McSweeny_SE_Self_Worth*sqrt(1 + (1/SampleN) + ((Score_Self_Worth_3 - M_Self_Worth)^2/(SD_Self_Worth^2*(SampleN-1))))
        SE_Self_Worth<-c(SE_Self_Worth_1, SE_Self_Worth_2, SE_Self_Worth_3)
        SE_Happiness_1<- McSweeny_SE_Happiness*sqrt(1 + (1/SampleN) + ((Score_Happiness_1 - M_Happiness)^2/(SD_Happiness^2*(SampleN-1))))
        SE_Happiness_2<- McSweeny_SE_Happiness*sqrt(1 + (1/SampleN) + ((Score_Happiness_2 - M_Happiness)^2/(SD_Happiness^2*(SampleN-1))))
        SE_Happiness_3<- McSweeny_SE_Happiness*sqrt(1 + (1/SampleN) + ((Score_Happiness_3 - M_Happiness)^2/(SD_Happiness^2*(SampleN-1))))
        SE_Happiness<-c(SE_Happiness_1, SE_Happiness_2, SE_Happiness_3)
        SE_Super_Mental_1<- McSweeny_SE_Super_Mental*sqrt(1 + (1/SampleN) + ((Score_Super_Mental_1 - M_Super_Mental)^2/(SD_Super_Mental^2*(SampleN-1))))
        SE_Super_Mental_2<- McSweeny_SE_Super_Mental*sqrt(1 + (1/SampleN) + ((Score_Super_Mental_2 - M_Super_Mental)^2/(SD_Super_Mental^2*(SampleN-1))))
        SE_Super_Mental_3<- McSweeny_SE_Super_Mental*sqrt(1 + (1/SampleN) + ((Score_Super_Mental_3 - M_Super_Mental)^2/(SD_Super_Mental^2*(SampleN-1))))
        SE_Super_Physical_1<- McSweeny_SE_Super_Physical*sqrt(1 + (1/SampleN) + ((Score_Super_Physical_1 - M_Super_Physical)^2/(SD_Super_Physical^2*(SampleN-1))))
        SE_Super_Physical_2<- McSweeny_SE_Super_Physical*sqrt(1 + (1/SampleN) + ((Score_Super_Physical_2 - M_Super_Physical)^2/(SD_Super_Physical^2*(SampleN-1))))
        SE_Super_Physical_3<- McSweeny_SE_Super_Physical*sqrt(1 + (1/SampleN) + ((Score_Super_Physical_3 - M_Super_Physical)^2/(SD_Super_Physical^2*(SampleN-1))))
        SE_Super_Physical<-c(SE_Super_Physical_1, SE_Super_Physical_2, SE_Super_Physical_3)
        SE<- round(SE, digits = 2)
        SE_Independent_Living<- round(SE_Independent_Living, digits = 2)
        SE_Relationships<- round(SE_Relationships, digits = 2)
        SE_Mental_Health<- round(SE_Mental_Health, digits = 2)
        SE_Coping<- round(SE_Coping, digits = 2)
        SE_Pain<- round(SE_Pain, digits = 2)
        SE_Senses<- round(SE_Senses, digits = 2)
        SE_Self_Worth<- round(SE_Self_Worth, digits = 2)
        SE_Happiness<- round(SE_Happiness, digits = 2)
        SE_Super_Mental<- round(SE_Super_Mental, digits = 2)
        SE_Super_Physical<- round(SE_Super_Physical, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2), (Conf*SE3))
        CI<- round(CI, digits = 2)
        CI_Independent_Living<- c((Conf*SE_Independent_Living_1), (Conf*SE_Independent_Living_2), (Conf*SE_Independent_Living_3))
        CI_Independent_Living<- round(CI_Independent_Living, digits = 2)
        CI_Relationships<- c((Conf*SE_Relationships_1), (Conf*SE_Relationships_2), (Conf*SE_Relationships_3))
        CI_Relationships<- round(CI_Relationships, digits = 2)
        CI_Mental_Health<- c((Conf*SE_Mental_Health_1), (Conf*SE_Mental_Health_2), (Conf*SE_Mental_Health_3))
        CI_Mental_Health<- round(CI_Mental_Health, digits = 2)
        CI_Coping<- c((Conf*SE_Coping_1), (Conf*SE_Coping_2), (Conf*SE_Coping_3))
        CI_Coping<- round(CI_Coping, digits = 2)
        CI_Pain<- c((Conf*SE_Pain_1), (Conf*SE_Pain_2), (Conf*SE_Pain_3))
        CI_Pain<- round(CI_Pain, digits = 2)
        CI_Senses<- c((Conf*SE_Senses_1), (Conf*SE_Senses_2), (Conf*SE_Senses_3))
        CI_Senses<- round(CI_Senses, digits = 2)
        CI_Self_Worth<- c((Conf*SE_Self_Worth_1), (Conf*SE_Self_Worth_2), (Conf*SE_Self_Worth_3))
        CI_Self_Worth<- round(CI_Self_Worth, digits = 2)
        CI_Happiness<- c((Conf*SE_Happiness_1), (Conf*SE_Happiness_2), (Conf*SE_Happiness_3))
        CI_Happiness<- round(CI_Happiness, digits = 2)
        CI_Super_Mental<- c((Conf*SE_Super_Mental_1), (Conf*SE_Super_Mental_2), (Conf*SE_Super_Mental_3))
        CI_Super_Mental<- round(CI_Super_Mental, digits = 2)
        CI_Super_Physical<- c((Conf*SE_Super_Physical_1), (Conf*SE_Super_Physical_2), (Conf*SE_Super_Physical_3))
        CI_Super_Physical<- round(CI_Super_Physical, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Independent_Living<- c((Conf*SE_Independent_Living), (Conf*SE_Independent_Living), (Conf*SE_Independent_Living))
        CI_Independent_Living<- round(CI_Independent_Living, digits = 2)
        CI_Relationships<- c((Conf*SE_Relationships), (Conf*SE_Relationships), (Conf*SE_Relationships))
        CI_Relationships<- round(CI_Relationships, digits = 2)
        CI_Mental_Health<- c((Conf*SE_Mental_Health), (Conf*SE_Mental_Health), (Conf*SE_Mental_Health))
        CI_Mental_Health<- round(CI_Mental_Health, digits = 2)
        CI_Coping<- c((Conf*SE_Coping), (Conf*SE_Coping), (Conf*SE_Coping))
        CI_Coping<- round(CI_Coping, digits = 2)
        CI_Pain<- c((Conf*SE_Pain), (Conf*SE_Pain), (Conf*SE_Pain))
        CI_Pain<- round(CI_Pain, digits = 2)
        CI_Senses<- c((Conf*SE_Senses), (Conf*SE_Senses), (Conf*SE_Senses))
        CI_Senses<- round(CI_Senses, digits = 2)
        CI_Self_Worth<- c((Conf*SE_Self_Worth), (Conf*SE_Self_Worth), (Conf*SE_Self_Worth))
        CI_Self_Worth<- round(CI_Self_Worth, digits = 2)
        CI_Happiness<- c((Conf*SE_Happiness), (Conf*SE_Happiness))
        CI_Happiness<- round(CI_Happiness, digits = 2)
        CI_Super_Mental<- c((Conf*SE_Super_Mental), (Conf*SE_Super_Mental), (Conf*SE_Super_Mental))
        CI_Super_Mental<- round(CI_Super_Mental, digits = 2)
        CI_Super_Physical<- c((Conf*SE_Super_Physical), (Conf*SE_Super_Physical), (Conf*SE_Super_Physical))
        CI_Super_Physical<- round(CI_Super_Physical, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Independent_Living<- PTS_Independent_Living + CI_Independent_Living
      CI_Upper_Lim_Independent_Living<- round(CI_Upper_Lim_Independent_Living, digits = 2)
      CI_Lower_Lim_Independent_Living<-PTS_Independent_Living - CI_Independent_Living
      CI_Lower_Lim_Independent_Living<- round(CI_Lower_Lim_Independent_Living, digits = 2)
      CI_Upper_Lim_Relationships<- PTS_Relationships + CI_Relationships
      CI_Upper_Lim_Relationships<- round(CI_Upper_Lim_Relationships, digits = 2)
      CI_Lower_Lim_Relationships<-PTS_Relationships - CI_Relationships
      CI_Lower_Lim_Relationships<- round(CI_Lower_Lim_Relationships, digits = 2)
      CI_Upper_Lim_Mental_Health<- PTS_Mental_Health + CI_Mental_Health
      CI_Upper_Lim_Mental_Health<- round(CI_Upper_Lim_Mental_Health, digits = 2)
      CI_Lower_Lim_Mental_Health<-PTS_Mental_Health - CI_Mental_Health
      CI_Lower_Lim_Mental_Health<- round(CI_Lower_Lim_Mental_Health, digits = 2)
      CI_Upper_Lim_Coping<- PTS_Coping + CI_Coping
      CI_Upper_Lim_Coping<- round(CI_Upper_Lim_Coping, digits = 2)
      CI_Lower_Lim_Coping<-PTS_Coping - CI_Coping
      CI_Lower_Lim_Coping<- round(CI_Lower_Lim_Coping, digits = 2)
      CI_Upper_Lim_Pain<- PTS_Pain + CI_Pain
      CI_Upper_Lim_Pain<- round(CI_Upper_Lim_Pain, digits = 2)
      CI_Lower_Lim_Pain<-PTS_Pain - CI_Pain
      CI_Lower_Lim_Pain<- round(CI_Lower_Lim_Pain, digits = 2)
      CI_Upper_Lim_Senses<- PTS_Senses + CI_Senses
      CI_Upper_Lim_Senses<- round(CI_Upper_Lim_Senses, digits = 2)
      CI_Lower_Lim_Senses<-PTS_Senses - CI_Senses
      CI_Lower_Lim_Senses<- round(CI_Lower_Lim_Senses, digits = 2)
      CI_Upper_Lim_Self_Worth<- PTS_Self_Worth + CI_Self_Worth
      CI_Upper_Lim_Self_Worth<- round(CI_Upper_Lim_Self_Worth, digits = 2)
      CI_Lower_Lim_Self_Worth<-PTS_Self_Worth - CI_Self_Worth
      CI_Lower_Lim_Self_Worth<- round(CI_Lower_Lim_Self_Worth, digits = 2)
      CI_Upper_Lim_Happiness<- PTS_Happiness + CI_Happiness
      CI_Upper_Lim_Happiness<- round(CI_Upper_Lim_Happiness, digits = 2)
      CI_Lower_Lim_Happiness<-PTS_Happiness - CI_Happiness
      CI_Lower_Lim_Happiness<- round(CI_Lower_Lim_Happiness, digits = 2)
      CI_Upper_Lim_Super_Mental<- PTS_Super_Mental + CI_Super_Mental
      CI_Upper_Lim_Super_Mental<- round(CI_Upper_Lim_Super_Mental, digits = 2)
      CI_Lower_Lim_Super_Mental<-PTS_Super_Mental - CI_Super_Mental
      CI_Lower_Lim_Super_Mental<- round(CI_Lower_Lim_Super_Mental, digits = 2)
      CI_Upper_Lim_Super_Physical<- PTS_Super_Physical + CI_Super_Physical
      CI_Upper_Lim_Super_Physical<- round(CI_Upper_Lim_Super_Physical, digits = 2)
      CI_Lower_Lim_Super_Physical<-PTS_Super_Physical - CI_Super_Physical
      CI_Lower_Lim_Super_Physical<- round(CI_Lower_Lim_Super_Physical, digits = 2)
      
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Independent_Living == "2") {
        CI_Independent_Living<- input$Man_CI_Independent_Living
        CI_Independent_Living<- c(CI_Independent_Living, CI_Independent_Living, CI_Independent_Living)
        CI_Independent_Living<- round(CI_Independent_Living, digits = 2)
        CI_Upper_Lim_Independent_Living<- Score_Independent_Living + CI_Independent_Living
        CI_Upper_Lim_Independent_Living<- round(CI_Upper_Lim_Independent_Living, digits = 2)
        CI_Lower_Lim_Independent_Living<- Score_Independent_Living - CI_Independent_Living
        CI_Lower_Lim_Independent_Living<- round(CI_Lower_Lim_Independent_Living, digits = 2)
      }
      if(input$Select_CI_Relationships == "2") {
        CI_Relationships<- input$Man_CI_Relationships
        CI_Relationships<- c(CI_Relationships, CI_Relationships, CI_Relationships)
        CI_Relationships<- round(CI_Relationships, digits = 2)
        CI_Upper_Lim_Relationships<- Score_Relationships + CI_Relationships
        CI_Upper_Lim_Relationships<- round(CI_Upper_Lim_Relationships, digits = 2)
        CI_Lower_Lim_Relationships<- Score_Relationships - CI_Relationships
        CI_Lower_Lim_Relationships<- round(CI_Lower_Lim_Relationships, digits = 2)
      }
      if(input$Select_CI_Mental_Health == "2") {
        CI_Mental_Health<- input$Man_CI_Mental_Health
        CI_Mental_Health<- c(CI_Mental_Health, CI_Mental_Health, CI_Mental_Health)
        CI_Mental_Health<- round(CI_Mental_Health, digits = 2)
        CI_Upper_Lim_Mental_Health<- Score_Mental_Health + CI_Mental_Health
        CI_Upper_Lim_Mental_Health<- round(CI_Upper_Lim_Mental_Health, digits = 2)
        CI_Lower_Lim_Mental_Health<- Score_Mental_Health - CI_Mental_Health
        CI_Lower_Lim_Mental_Health<- round(CI_Lower_Lim_Mental_Health, digits = 2)
      }
      if(input$Select_CI_Coping == "2") {
        CI_Coping<- input$Man_CI_Coping
        CI_Coping<- c(CI_Coping, CI_Coping, CI_Coping)
        CI_Coping<- round(CI_Coping, digits = 2)
        CI_Upper_Lim_Coping<- Score_Coping + CI_Coping
        CI_Upper_Lim_Coping<- round(CI_Upper_Lim_Coping, digits = 2)
        CI_Lower_Lim_Coping<- Score_Coping - CI_Coping
        CI_Lower_Lim_Coping<- round(CI_Lower_Lim_Coping, digits = 2)
      }
      if(input$Select_CI_Pain == "2") {
        CI_Pain<- input$Man_CI_Pain
        CI_Pain<- c(CI_Pain,  CI_Pain, CI_Pain)
        CI_Pain<- round(CI_Pain, digits = 2)
        CI_Upper_Lim_Pain<- Score_Pain + CI_Pain
        CI_Upper_Lim_Pain<- round(CI_Upper_Lim_Pain, digits = 2)
        CI_Lower_Lim_Pain<- Score_Pain - CI_Pain
        CI_Lower_Lim_Pain<- round(CI_Lower_Lim_Pain, digits = 2)
      }
      if(input$Select_CI_Senses == "2") {
        CI_Senses<- input$Man_CI_Senses
        CI_Senses<- c(CI_Senses, CI_Senses, CI_Senses)
        CI_Senses<- round(CI_Senses, digits = 2)
        CI_Upper_Lim_Senses<- Score_Senses + CI_Senses
        CI_Upper_Lim_Senses<- round(CI_Upper_Lim_Senses, digits = 2)
        CI_Lower_Lim_Senses<- Score_Senses - CI_Senses
        CI_Lower_Lim_Senses<- round(CI_Lower_Lim_Senses, digits = 2)
      }
      if(input$Select_CI_Self_Worth == "2") {
        CI_Self_Worth<- input$Man_CI_Self_Worth
        CI_Self_Worth<- c(CI_Self_Worth, CI_Self_Worth, CI_Self_Worth)
        CI_Self_Worth<- round(CI_Self_Worth, digits = 2)
        CI_Upper_Lim_Self_Worth<- Score_Self_Worth + CI_Self_Worth
        CI_Upper_Lim_Self_Worth<- round(CI_Upper_Lim_Self_Worth, digits = 2)
        CI_Lower_Lim_Self_Worth<- Score_Self_Worth - CI_Self_Worth
        CI_Lower_Lim_Self_Worth<- round(CI_Lower_Lim_Self_Worth, digits = 2)
      }
      if(input$Select_CI_Happiness == "2") {
        CI_Happiness<- input$Man_CI_Happiness
        CI_Happiness<- round(CI_Happiness, digits = 2)
        CI_Happiness<- c(CI_Happiness,  CI_Happiness, CI_Happiness)
        CI_Upper_Lim_Happiness<- Score_Happiness + CI_Happiness
        CI_Upper_Lim_Happiness<- round(CI_Upper_Lim_Happiness, digits = 2)
        CI_Lower_Lim_Happiness<- Score_Happiness - CI_Happiness
        CI_Lower_Lim_Happiness<- round(CI_Lower_Lim_Happiness, digits = 2)
      }
      if(input$Select_CI_Super_Mental == "2") {
        CI_Super_Mental<- input$Man_CI_Super_Mental
        CI_Super_Mental<- c(CI_Super_Mental, CI_Super_Mental, CI_Super_Mental)
        CI_Super_Mental<- round(CI_Super_Mental, digits = 2)
        CI_Upper_Lim_Super_Mental<- Score_Super_Mental + CI_Super_Mental
        CI_Upper_Lim_Super_Mental<- round(CI_Upper_Lim_Super_Mental, digits = 2)
        CI_Lower_Lim_Super_Mental<- Score_Super_Mental - CI_Super_Mental
        CI_Lower_Lim_Super_Mental<- round(CI_Lower_Lim_Super_Mental, digits = 2)
      }
      if(input$Select_CI_Super_Physical == "2") {
        CI_Super_Physical<- input$Man_CI_Super_Physical
        CI_Super_Physical<- c(CI_Super_Physical, CI_Super_Physical, CI_Super_Physical)
        CI_Super_Physical<- round(CI_Super_Physical, digits = 2)
        CI_Upper_Lim_Super_Physical<- Score_Super_Physical + CI_Super_Physical
        CI_Upper_Lim_Super_Physical<- round(CI_Upper_Lim_Super_Physical, digits = 2)
        CI_Lower_Lim_Super_Physical<- Score_Super_Physical - CI_Super_Physical
        CI_Lower_Lim_Super_Physical<- round(CI_Lower_Lim_Super_Physical, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Independent_Living_1<- round(input$Cutoff_Independent_Living_1, digits = 2)
      Cutoff_Score_Independent_Living_2<- round(input$Cutoff_Independent_Living_2, digits = 2)
      Cutoff_Score_Independent_Living_3<- round(input$Cutoff_Independent_Living_3, digits = 2)
      Cutoff_Score_Relationships_1<- round(input$Cutoff_Relationships_1, digits = 2)
      Cutoff_Score_Relationships_2<- round(input$Cutoff_Relationships_2, digits = 2)
      Cutoff_Score_Relationships_3<- round(input$Cutoff_Relationships_3, digits = 2)
      Cutoff_Score_Mental_Health_1<- round(input$Cutoff_Mental_Health_1, digits = 2)
      Cutoff_Score_Mental_Health_2<- round(input$Cutoff_Mental_Health_2, digits = 2)
      Cutoff_Score_Mental_Health_3<- round(input$Cutoff_Mental_Health_3, digits = 2)
      Cutoff_Score_Coping_1<- round(input$Cutoff_Coping_1, digits = 2)
      Cutoff_Score_Coping_2<- round(input$Cutoff_Coping_2, digits = 2)
      Cutoff_Score_Coping_3<- round(input$Cutoff_Coping_3, digits = 2)
      Cutoff_Score_Pain_1<- round(input$Cutoff_Pain_1, digits = 2)
      Cutoff_Score_Pain_2<- round(input$Cutoff_Pain_2, digits = 2)
      Cutoff_Score_Pain_3<- round(input$Cutoff_Pain_3, digits = 2)
      Cutoff_Score_Senses_1<- round(input$Cutoff_Senses_1, digits = 2)
      Cutoff_Score_Senses_2<- round(input$Cutoff_Senses_2, digits = 2)
      Cutoff_Score_Senses_3<- round(input$Cutoff_Senses_3, digits = 2)
      Cutoff_Score_Self_Worth_1<- round(input$Cutoff_Self_Worth_1, digits = 2)
      Cutoff_Score_Self_Worth_2<- round(input$Cutoff_Self_Worth_2, digits = 2)
      Cutoff_Score_Self_Worth_3<- round(input$Cutoff_Self_Worth_3, digits = 2)
      Cutoff_Score_Happiness_1<- round(input$Cutoff_Happiness_1, digits = 2)
      Cutoff_Score_Happiness_2<- round(input$Cutoff_Happiness_2, digits = 2)
      Cutoff_Score_Happiness_3<- round(input$Cutoff_Happiness_3, digits = 2)
      Cutoff_Score_Super_Mental_1<- round(input$Cutoff_Super_Mental_1, digits = 2)
      Cutoff_Score_Super_Mental_2<- round(input$Cutoff_Super_Mental_2, digits = 2)
      Cutoff_Score_Super_Mental_3<- round(input$Cutoff_Super_Mental_3, digits = 2)
      Cutoff_Score_Super_Physical_1<- round(input$Cutoff_Super_Physical_1, digits = 2)
      Cutoff_Score_Super_Physical_2<- round(input$Cutoff_Super_Physical_2, digits = 2)
      Cutoff_Score_Super_Physical_3<- round(input$Cutoff_Super_Physical_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Independent_Living,Change_Independent_Living,PTS_Independent_Living, SE_Independent_Living, CI_Upper_Lim_Independent_Living, CI_Lower_Lim_Independent_Living, Cutoff_Score_Independent_Living_1,Cutoff_Score_Independent_Living_2,Cutoff_Score_Independent_Living_3,
                                      Score_Relationships,Change_Relationships, PTS_Relationships, SE_Relationships, CI_Upper_Lim_Relationships, CI_Lower_Lim_Relationships, Cutoff_Score_Relationships_1,Cutoff_Score_Relationships_2,Cutoff_Score_Relationships_3, 
                                      Score_Mental_Health,Change_Mental_Health,PTS_Mental_Health, SE_Mental_Health, CI_Upper_Lim_Mental_Health, CI_Lower_Lim_Mental_Health, Cutoff_Score_Mental_Health_1,Cutoff_Score_Mental_Health_2,Cutoff_Score_Mental_Health_3, 
                                      Score_Coping,Change_Coping,PTS_Coping, SE_Coping, CI_Upper_Lim_Coping, CI_Lower_Lim_Coping, Cutoff_Score_Coping_1,Cutoff_Score_Coping_2,Cutoff_Score_Coping_3, 
                                      Score_Pain,Change_Pain,PTS_Pain, SE_Pain, CI_Upper_Lim_Pain, CI_Lower_Lim_Pain, Cutoff_Score_Pain_1,Cutoff_Score_Pain_2,Cutoff_Score_Pain_3,
                                      Score_Senses,Change_Senses, PTS_Senses, SE_Senses, CI_Upper_Lim_Senses, CI_Lower_Lim_Senses, Cutoff_Score_Senses_1,Cutoff_Score_Senses_2,Cutoff_Score_Senses_3, 
                                      Score_Self_Worth,Change_Self_Worth,PTS_Self_Worth, SE_Self_Worth, CI_Upper_Lim_Self_Worth, CI_Lower_Lim_Self_Worth, Cutoff_Score_Self_Worth_1,Cutoff_Score_Self_Worth_2,Cutoff_Score_Self_Worth_3, 
                                      Score_Happiness,Change_Happiness,PTS_Happiness, SE_Happiness, CI_Upper_Lim_Happiness, CI_Lower_Lim_Happiness, Cutoff_Score_Happiness_1,Cutoff_Score_Happiness_2,Cutoff_Score_Happiness_3, 
                                      Score_Super_Mental,Change_Super_Mental,PTS_Super_Mental, SE_Super_Mental, CI_Upper_Lim_Super_Mental, CI_Lower_Lim_Super_Mental, Cutoff_Score_Super_Mental_1,Cutoff_Score_Super_Mental_2,Cutoff_Score_Super_Mental_3,
                                      Score_Super_Physical,Change_Super_Physical,PTS_Super_Physical, SE_Super_Physical, CI_Upper_Lim_Super_Physical, CI_Lower_Lim_Super_Physical, Cutoff_Score_Super_Physical_1,Cutoff_Score_Super_Physical_2,Cutoff_Score_Super_Physical_3)
    }
    
    
    #Create dataframes to be displayed as tables in pdf report (statistical values used in reliable change analyses)
    
    if(input$RelChangeMethod == 'Nunnally & Bernstein (1994)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, SD, Rel, RelChangeMethod, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Independent_Living<<- data.frame(Pop,  M_Independent_Living, SD_Independent_Living, Rel_Independent_Living, RelChangeMethod, ConfInt)
      names(Stats_Table_Independent_Living)<<- c("Reference Population",  "M", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Relationships<<- data.frame(Pop,  M_Relationships, SD_Relationships, Rel_Relationships, RelChangeMethod, ConfInt)
      names(Stats_Table_Relationships)<<- c("Reference Population",  "M", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Mental_Health<<- data.frame(Pop,  M_Mental_Health, SD_Mental_Health, Rel_Mental_Health, RelChangeMethod, ConfInt)
      names(Stats_Table_Mental_Health)<<- c("Reference Population",  "M", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Coping<<- data.frame(Pop,  M_Coping, SD_Coping, Rel_Coping, RelChangeMethod, ConfInt)
      names(Stats_Table_Coping)<<- c("Reference Population",  "M", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Pain<<- data.frame(Pop,  M_Pain, SD_Pain, Rel_Pain, RelChangeMethod, ConfInt)
      names(Stats_Table_Pain)<<- c("Reference Population",  "M", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Senses<<- data.frame(Pop,  M_Senses, SD_Senses, Rel_Senses, RelChangeMethod, ConfInt)
      names(Stats_Table_Senses)<<- c("Reference Population",  "M", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Self_Worth<<- data.frame(Pop,  M_Self_Worth, SD_Self_Worth, Rel_Self_Worth, RelChangeMethod, ConfInt)
      names(Stats_Table_Self_Worth)<<- c("Reference Population",  "M", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Happiness<<- data.frame(Pop,  M_Happiness, SD_Happiness, Rel_Happiness, RelChangeMethod, ConfInt)
      names(Stats_Table_Happiness)<<- c("Reference Population",  "M", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Super_Mental<<- data.frame(Pop,  M_Super_Mental, SD_Super_Mental, Rel_Super_Mental, RelChangeMethod, ConfInt)
      names(Stats_Table_Super_Mental)<<- c("Reference Population",  "M", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Super_Physical<<- data.frame(Pop,  M_Super_Physical, SD_Super_Physical, Rel_Super_Physical, RelChangeMethod, ConfInt)
      names(Stats_Table_Super_Physical)<<- c("Reference Population",  "M", "Sd", "Reliability", "Reliable Change Method", "Confidence")
    } else if (input$RelChangeMethod == 'Chelune et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, Rel, RelChangeMethod, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Independent_Living<<- data.frame(Pop,  M_Independent_Living, M_Retest_Independent_Living, SD_Independent_Living, Rel_Independent_Living, RelChangeMethod, ConfInt)
      names(Stats_Table_Independent_Living)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Relationships<<- data.frame(Pop,  M_Relationships, M_Retest_Relationships, SD_Relationships, Rel_Relationships, RelChangeMethod, ConfInt)
      names(Stats_Table_Relationships)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Mental_Health<<- data.frame(Pop,  M_Mental_Health, M_Retest_Mental_Health, SD_Mental_Health, Rel_Mental_Health, RelChangeMethod, ConfInt)
      names(Stats_Table_Mental_Health)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Coping<<- data.frame(Pop,  M_Coping, M_Retest_Coping, SD_Coping, Rel_Coping, RelChangeMethod, ConfInt)
      names(Stats_Table_Coping)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      
      Stats_Table_Pain<<- data.frame(Pop,  M_Pain, M_Retest_Pain, SD_Pain, Rel_Pain, RelChangeMethod, ConfInt)
      names(Stats_Table_Pain)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Senses<<- data.frame(Pop,  M_Senses, M_Retest_Senses, SD_Senses, Rel_Senses, RelChangeMethod, ConfInt)
      names(Stats_Table_Senses)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Self_Worth<<- data.frame(Pop,  M_Self_Worth, M_Retest_Self_Worth, SD_Self_Worth, Rel_Self_Worth, RelChangeMethod, ConfInt)
      names(Stats_Table_Self_Worth)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Happiness<<- data.frame(Pop,  M_Happiness, M_Retest_Happiness, SD_Happiness, Rel_Happiness, RelChangeMethod, ConfInt)
      names(Stats_Table_Happiness)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Super_Mental<<- data.frame(Pop,  M_Super_Mental, M_Retest_Super_Mental, SD_Super_Mental, Rel_Super_Mental, RelChangeMethod, ConfInt)
      names(Stats_Table_Super_Mental)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Super_Physical<<- data.frame(Pop,  M_Super_Physical, M_Retest_Super_Physical, SD_Super_Physical, Rel_Super_Physical, RelChangeMethod, ConfInt)
      names(Stats_Table_Super_Physical)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Reliability", "Reliable Change Method", "Confidence")
    } else if (input$RelChangeMethod == 'Maassen et al. (2006)' | input$RelChangeMethod == 'McSweeny et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, Rel, RelChangeMethod, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Independent_Living<<- data.frame(Pop,  M_Independent_Living, M_Retest_Independent_Living, SD_Independent_Living, SD_Retest_Independent_Living, Rel_Independent_Living, RelChangeMethod, ConfInt)
      names(Stats_Table_Independent_Living)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Relationships<<- data.frame(Pop,  M_Relationships, M_Retest_Relationships, SD_Relationships, SD_Retest_Relationships, Rel_Relationships, RelChangeMethod, ConfInt)
      names(Stats_Table_Relationships)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Mental_Health<<- data.frame(Pop,  M_Mental_Health, M_Retest_Mental_Health, SD_Mental_Health, SD_Retest_Mental_Health, Rel_Mental_Health, RelChangeMethod, ConfInt)
      names(Stats_Table_Mental_Health)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Coping<<- data.frame(Pop,  M_Coping, M_Retest_Coping, SD_Coping, SD_Retest_Coping, Rel_Coping, RelChangeMethod, ConfInt)
      names(Stats_Table_Coping)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Pain<<- data.frame(Pop,  M_Pain, M_Retest_Pain, SD_Pain, SD_Retest_Pain, Rel_Pain, RelChangeMethod, ConfInt)
      names(Stats_Table_Pain)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Senses<<- data.frame(Pop,  M_Senses, M_Retest_Senses, SD_Senses, SD_Retest_Senses, Rel_Senses, RelChangeMethod, ConfInt)
      names(Stats_Table_Senses)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Self_Worth<<- data.frame(Pop,  M_Self_Worth, M_Retest_Self_Worth, SD_Self_Worth, SD_Retest_Self_Worth, Rel_Self_Worth, RelChangeMethod, ConfInt)
      names(Stats_Table_Self_Worth)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Happiness<<- data.frame(Pop,  M_Happiness, M_Retest_Happiness, SD_Happiness, SD_Retest_Happiness, Rel_Happiness, RelChangeMethod, ConfInt)
      names(Stats_Table_Happiness)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Super_Mental<<- data.frame(Pop,  M_Super_Mental, M_Retest_Super_Mental, SD_Super_Mental, SD_Retest_Super_Mental, Rel_Super_Mental, RelChangeMethod, ConfInt)
      names(Stats_Table_Super_Mental)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Super_Physical<<- data.frame(Pop,  M_Super_Physical, M_Retest_Super_Physical, SD_Super_Physical, SD_Retest_Super_Physical, Rel_Super_Physical, RelChangeMethod, ConfInt)
      names(Stats_Table_Super_Physical)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "Reliable Change Method", "Confidence")
      
    } else if (input$RelChangeMethod == 'Crawford & Howell (1998)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, Rel, SampleN, RelChangeMethod, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "N", "Reliable Change Method", "Confidence")
      Stats_Table_Independent_Living<<- data.frame(Pop,  M_Independent_Living, M_Retest_Independent_Living, SD_Independent_Living, SD_Retest_Independent_Living, Rel_Independent_Living, SampleN,RelChangeMethod, ConfInt)
      names(Stats_Table_Independent_Living)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "N", "Reliable Change Method", "Confidence")
      Stats_Table_Relationships<<- data.frame(Pop,  M_Relationships, M_Retest_Relationships, SD_Relationships, SD_Retest_Relationships, Rel_Relationships, SampleN, RelChangeMethod, ConfInt)
      names(Stats_Table_Relationships)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "N", "Reliable Change Method", "Confidence")
      Stats_Table_Mental_Health<<- data.frame(Pop,  M_Mental_Health, M_Retest_Mental_Health, SD_Mental_Health, SD_Retest_Mental_Health, Rel_Mental_Health, SampleN,RelChangeMethod, ConfInt)
      names(Stats_Table_Mental_Health)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "N", "Reliable Change Method", "Confidence")
      Stats_Table_Coping<<- data.frame(Pop,  M_Coping, M_Retest_Coping, SD_Coping, SD_Retest_Coping, Rel_Coping, SampleN,RelChangeMethod, ConfInt)
      names(Stats_Table_Coping)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "N", "Reliable Change Method", "Confidence")
      Stats_Table_Pain<<- data.frame(Pop,  M_Pain, M_Retest_Pain, SD_Pain, SD_Retest_Pain, Rel_Pain, SampleN,RelChangeMethod, ConfInt)
      names(Stats_Table_Pain)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "N", "Reliable Change Method", "Confidence")
      Stats_Table_Senses<<- data.frame(Pop,  M_Senses, M_Retest_Senses, SD_Senses, SD_Retest_Senses, Rel_Senses, SampleN, RelChangeMethod, ConfInt)
      names(Stats_Table_Senses)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "N", "Reliable Change Method", "Confidence")
      Stats_Table_Self_Worth<<- data.frame(Pop,  M_Self_Worth, M_Retest_Self_Worth, SD_Self_Worth, SD_Retest_Self_Worth, Rel_Self_Worth, SampleN,RelChangeMethod, ConfInt)
      names(Stats_Table_Self_Worth)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "N", "Reliable Change Method", "Confidence")
      Stats_Table_Happiness<<- data.frame(Pop,  M_Happiness, M_Retest_Happiness, SD_Happiness, SD_Retest_Happiness, Rel_Happiness, SampleN,RelChangeMethod, ConfInt)
      names(Stats_Table_Happiness)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "N", "Reliable Change Method", "Confidence")
      Stats_Table_Super_Mental<<- data.frame(Pop,  M_Super_Mental, M_Retest_Super_Mental, SD_Super_Mental, SD_Retest_Super_Mental, Rel_Super_Mental, SampleN,RelChangeMethod, ConfInt)
      names(Stats_Table_Super_Mental)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "N", "Reliable Change Method", "Confidence")
      Stats_Table_Super_Physical<<- data.frame(Pop,  M_Super_Physical, M_Retest_Super_Physical, SD_Super_Physical, SD_Retest_Super_Physical, Rel_Super_Physical, SampleN,RelChangeMethod, ConfInt)
      names(Stats_Table_Super_Physical)<<- c("Reference Population",  "M", "M (Retest)", "Sd", "Sd (Retest)", "Reliability", "N", "Reliable Change Method", "Confidence")
    } else if (input$RelChangeMethod == 'Jacobson & Truax (1991)' | input$RelChangeMethod == 'Speer (1992)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  SD, Rel, RelChangeMethod, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population",  "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Independent_Living<<- data.frame(Pop,  SD_Independent_Living, Rel_Independent_Living, RelChangeMethod, ConfInt)
      names(Stats_Table_Independent_Living)<<- c("Reference Population",  "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Relationships<<- data.frame(Pop,  SD_Relationships, Rel_Relationships, RelChangeMethod, ConfInt)
      names(Stats_Table_Relationships)<<- c("Reference Population",  "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Mental_Health<<- data.frame(Pop,  SD_Mental_Health, Rel_Mental_Health, RelChangeMethod, ConfInt)
      names(Stats_Table_Mental_Health)<<- c("Reference Population",  "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Coping<<- data.frame(Pop,  SD_Coping, Rel_Coping, RelChangeMethod, ConfInt)
      names(Stats_Table_Coping)<<- c("Reference Population",  "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Pain<<- data.frame(Pop,  SD_Pain, Rel_Pain, RelChangeMethod, ConfInt)
      names(Stats_Table_Pain)<<- c("Reference Population",  "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Senses<<- data.frame(Pop,  SD_Senses, Rel_Senses, RelChangeMethod, ConfInt)
      names(Stats_Table_Senses)<<- c("Reference Population",  "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Self_Worth<<- data.frame(Pop,  SD_Self_Worth, Rel_Self_Worth, RelChangeMethod, ConfInt)
      names(Stats_Table_Self_Worth)<<- c("Reference Population",  "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Happiness<<- data.frame(Pop,  SD_Happiness, Rel_Happiness, RelChangeMethod, ConfInt)
      names(Stats_Table_Happiness)<<- c("Reference Population",  "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Super_Mental<<- data.frame(Pop,  SD_Super_Mental, Rel_Super_Mental, RelChangeMethod, ConfInt)
      names(Stats_Table_Super_Mental)<<- c("Reference Population",  "Sd", "Reliability", "Reliable Change Method", "Confidence")
      Stats_Table_Super_Physical<<- data.frame(Pop,  SD_Super_Physical, Rel_Super_Physical, RelChangeMethod, ConfInt)
      names(Stats_Table_Super_Physical)<<- c("Reference Population",  "Sd", "Reliability", "Reliable Change Method", "Confidence")
      
      
    }
    
    #If custom confidence intervals options are selected, make sure that no values appear for PS and SE in pdf tables
    #Change the look of tables in report if custom confidence intervals options are selected
    
    if (input$Select_CI == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS = NA, SE = NA)
      Stats_Table_Fullscale<<- Stats_Table_Fullscale %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI[1])
    }
    if (input$Select_CI_Independent_Living == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Independent_Living = NA, SE_Independent_Living = NA)
      Stats_Table_Independent_Living<<- Stats_Table_Independent_Living %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Independent_Living[1])
    }
    if (input$Select_CI_Relationships == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Relationships = NA, SE_Relationships = NA)
      Stats_Table_Relationships<<- Stats_Table_Relationships %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                      "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Relationships[1])
    }
    if (input$Select_CI_Mental_Health == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Mental_Health = NA, SE_Mental_Health = NA)
      Stats_Table_Mental_Health<<- Stats_Table_Mental_Health %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                      "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Mental_Health[1])
    }
    if (input$Select_CI_Coping == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Coping = NA, SE_Coping = NA)
      Stats_Table_Coping<<- Stats_Table_Coping %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Coping[1])
    }
    if (input$Select_CI_Pain == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Pain = NA, SE_Pain = NA)
      Stats_Table_Pain<<- Stats_Table_Pain %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                    "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Pain[1])
    }
    if (input$Select_CI_Senses == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Senses = NA, SE_Senses = NA)
      Stats_Table_Senses<<- Stats_Table_Senses %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                                        "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Senses[1])
    }
    if (input$Select_CI_Self_Worth == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Self_Worth = NA, SE_Self_Worth = NA)
      Stats_Table_Self_Worth<<- Stats_Table_Self_Worth %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                    "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Self_Worth[1])
    }
    if (input$Select_CI_Happiness == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Happiness = NA, SE_Happiness = NA)
      Stats_Table_Happiness<<- Stats_Table_Happiness %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                        "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Happiness[1])
    }
    if (input$Select_CI_Super_Mental == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Super_Mental = NA, SE_Super_Mental = NA)
      Stats_Table_Super_Mental<<- Stats_Table_Super_Mental %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Super_Mental[1])
    }
    if (input$Select_CI_Super_Physical == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Super_Physical = NA, SE_Super_Physical = NA)
      Stats_Table_Super_Physical<<- Stats_Table_Super_Physical %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                      "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Super_Physical[1])
    }
    
    
    
    #Create a dataframe to be used in the analytics spreadsheet.
    
    #Calculate variables for significant improvement
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$CI_Upper_Lim[1]) {
      AQOL8D.Fullscale.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$CI_Upper_Lim[1]) {
      AQOL8D.Fullscale.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$CI_Lower_Lim[1]) {
      AQOL8D.Fullscale.Sig.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$CI_Lower_Lim[1]) {
      AQOL8D.Fullscale.Sig.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Independent_Living[length(Entered_Scores_Df$Score_Independent_Living)] > Entered_Scores_Df$CI_Upper_Lim_Independent_Living[1]) {
      AQOL8D.Independent.Living.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Independent_Living[length(Entered_Scores_Df$Score_Independent_Living)] <= Entered_Scores_Df$CI_Upper_Lim_Independent_Living[1]) {
      AQOL8D.Independent.Living.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Independent_Living[length(Entered_Scores_Df$Score_Independent_Living)] < Entered_Scores_Df$CI_Lower_Lim_Independent_Living[1]) {
      AQOL8D.Independent.Living.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Independent_Living[length(Entered_Scores_Df$Score_Independent_Living)] >= Entered_Scores_Df$CI_Lower_Lim_Independent_Living[1]) {
      AQOL8D.Independent.Living.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] > Entered_Scores_Df$CI_Upper_Lim_Relationships[1]) {
      AQOL8D.Relationships.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] <= Entered_Scores_Df$CI_Upper_Lim_Relationships[1]) {
      AQOL8D.Relationships.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] < Entered_Scores_Df$CI_Lower_Lim_Relationships[1]) {
      AQOL8D.Relationships.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] >= Entered_Scores_Df$CI_Lower_Lim_Relationships[1]) {
      AQOL8D.Relationships.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Mental_Health[length(Entered_Scores_Df$Score_Mental_Health)] > Entered_Scores_Df$CI_Upper_Lim_Mental_Health[1]) {
      AQOL8D.Mental.Health.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Mental_Health[length(Entered_Scores_Df$Score_Mental_Health)] <= Entered_Scores_Df$CI_Upper_Lim_Mental_Health[1]) {
      AQOL8D.Mental.Health.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Mental_Health[length(Entered_Scores_Df$Score_Mental_Health)] < Entered_Scores_Df$CI_Lower_Lim_Mental_Health[1]) {
      AQOL8D.Mental.Health.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Mental_Health[length(Entered_Scores_Df$Score_Mental_Health)] >= Entered_Scores_Df$CI_Lower_Lim_Mental_Health[1]) {
      AQOL8D.Mental.Health.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Coping[length(Entered_Scores_Df$Score_Coping)] > Entered_Scores_Df$CI_Upper_Lim_Coping[1]) {
      AQOL8D.Coping.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Coping[length(Entered_Scores_Df$Score_Coping)] <= Entered_Scores_Df$CI_Upper_Lim_Coping[1]) {
      AQOL8D.Coping.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Coping[length(Entered_Scores_Df$Score_Coping)] < Entered_Scores_Df$CI_Lower_Lim_Coping[1]) {
      AQOL8D.Coping.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Coping[length(Entered_Scores_Df$Score_Coping)] >= Entered_Scores_Df$CI_Lower_Lim_Coping[1]) {
      AQOL8D.Coping.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Pain[length(Entered_Scores_Df$Score_Pain)] > Entered_Scores_Df$CI_Upper_Lim_Pain[1]) {
      AQOL8D.Pain.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Pain[length(Entered_Scores_Df$Score_Pain)] <= Entered_Scores_Df$CI_Upper_Lim_Pain[1]) {
      AQOL8D.Pain.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Pain[length(Entered_Scores_Df$Score_Pain)] < Entered_Scores_Df$CI_Lower_Lim_Pain[1]) {
      AQOL8D.Pain.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Pain[length(Entered_Scores_Df$Score_Pain)] >= Entered_Scores_Df$CI_Lower_Lim_Pain[1]) {
      AQOL8D.Pain.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Senses[length(Entered_Scores_Df$Score_Senses)] > Entered_Scores_Df$CI_Upper_Lim_Senses[1]) {
      AQOL8D.Senses.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Senses[length(Entered_Scores_Df$Score_Senses)] <= Entered_Scores_Df$CI_Upper_Lim_Senses[1]) {
      AQOL8D.Senses.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Senses[length(Entered_Scores_Df$Score_Senses)] < Entered_Scores_Df$CI_Lower_Lim_Senses[1]) {
      AQOL8D.Senses.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Senses[length(Entered_Scores_Df$Score_Senses)] >= Entered_Scores_Df$CI_Lower_Lim_Senses[1]) {
      AQOL8D.Senses.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Self_Worth[length(Entered_Scores_Df$Score_Self_Worth)] > Entered_Scores_Df$CI_Upper_Lim_Self_Worth[1]) {
      AQOL8D.Self.Worth.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Self_Worth[length(Entered_Scores_Df$Score_Self_Worth)] <= Entered_Scores_Df$CI_Upper_Lim_Self_Worth[1]) {
      AQOL8D.Self.Worth.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Self_Worth[length(Entered_Scores_Df$Score_Self_Worth)] < Entered_Scores_Df$CI_Lower_Lim_Self_Worth[1]) {
      AQOL8D.Self.Worth.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Self_Worth[length(Entered_Scores_Df$Score_Self_Worth)] >= Entered_Scores_Df$CI_Lower_Lim_Self_Worth[1]) {
      AQOL8D.Self.Worth.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Happiness[length(Entered_Scores_Df$Score_Happiness)] > Entered_Scores_Df$CI_Upper_Lim_Happiness[1]) {
      AQOL8D.Happiness.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Happiness[length(Entered_Scores_Df$Score_Happiness)] <= Entered_Scores_Df$CI_Upper_Lim_Happiness[1]) {
      AQOL8D.Happiness.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Happiness[length(Entered_Scores_Df$Score_Happiness)] < Entered_Scores_Df$CI_Lower_Lim_Happiness[1]) {
      AQOL8D.Happiness.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Happiness[length(Entered_Scores_Df$Score_Happiness)] >= Entered_Scores_Df$CI_Lower_Lim_Happiness[1]) {
      AQOL8D.Happiness.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Super_Mental[length(Entered_Scores_Df$Score_Super_Mental)] > Entered_Scores_Df$CI_Upper_Lim_Super_Mental[1]) {
      AQOL8D.Super.Mental.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Super_Mental[length(Entered_Scores_Df$Score_Super_Mental)] <= Entered_Scores_Df$CI_Upper_Lim_Super_Mental[1]) {
      AQOL8D.Super.Mental.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Super_Mental[length(Entered_Scores_Df$Score_Super_Mental)] < Entered_Scores_Df$CI_Lower_Lim_Super_Mental[1]) {
      AQOL8D.Super.Mental.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Super_Mental[length(Entered_Scores_Df$Score_Super_Mental)] >= Entered_Scores_Df$CI_Lower_Lim_Super_Mental[1]) {
      AQOL8D.Super.Mental.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Super_Physical[length(Entered_Scores_Df$Score_Super_Physical)] > Entered_Scores_Df$CI_Upper_Lim_Super_Physical[1]) {
      AQOL8D.Super.Physical.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Super_Physical[length(Entered_Scores_Df$Score_Super_Physical)] <= Entered_Scores_Df$CI_Upper_Lim_Super_Physical[1]) {
      AQOL8D.Super.Physical.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Super_Physical[length(Entered_Scores_Df$Score_Super_Physical)] < Entered_Scores_Df$CI_Lower_Lim_Super_Physical[1]) {
      AQOL8D.Super.Physical.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Super_Physical[length(Entered_Scores_Df$Score_Super_Physical)] >= Entered_Scores_Df$CI_Lower_Lim_Super_Physical[1]) {
      AQOL8D.Super.Physical.Sig.Deterioration<- "No"
    }
    
    #Calculate variables for improvement
    
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$Score[1]) {
      AQOL8D.Fullscale.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$Score[1]) {
      AQOL8D.Fullscale.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$Score[1]) {
      AQOL8D.Fullscale.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$Score[1]) {
      AQOL8D.Fullscale.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Independent_Living[length(Entered_Scores_Df$Score_Independent_Living)] > Entered_Scores_Df$Score_Independent_Living[1]) {
      AQOL8D.Independent.Living.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Independent_Living[length(Entered_Scores_Df$Score_Independent_Living)] <= Entered_Scores_Df$Score_Independent_Living[1]) {
      AQOL8D.Independent.Living.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Independent_Living[length(Entered_Scores_Df$Score_Independent_Living)] < Entered_Scores_Df$Score_Independent_Living[1]) {
      AQOL8D.Independent.Living.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Independent_Living[length(Entered_Scores_Df$Score_Independent_Living)] >= Entered_Scores_Df$Score_Independent_Living[1]) {
      AQOL8D.Independent.Living.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] > Entered_Scores_Df$Score_Relationships[1]) {
      AQOL8D.Relationships.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] <= Entered_Scores_Df$Score_Relationships[1]) {
      AQOL8D.Relationships.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] < Entered_Scores_Df$Score_Relationships[1]) {
      AQOL8D.Relationships.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] >= Entered_Scores_Df$Score_Relationships[1]) {
      AQOL8D.Relationships.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Mental_Health[length(Entered_Scores_Df$Score_Mental_Health)] > Entered_Scores_Df$Score_Mental_Health[1]) {
      AQOL8D.Mental.Health.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Mental_Health[length(Entered_Scores_Df$Score_Mental_Health)] <= Entered_Scores_Df$Score_Mental_Health[1]) {
      AQOL8D.Mental.Health.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Mental_Health[length(Entered_Scores_Df$Score_Mental_Health)] < Entered_Scores_Df$Score_Mental_Health[1]) {
      AQOL8D.Mental.Health.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Mental_Health[length(Entered_Scores_Df$Score_Mental_Health)] >= Entered_Scores_Df$Score_Mental_Health[1]) {
      AQOL8D.Mental.Health.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Coping[length(Entered_Scores_Df$Score_Coping)] > Entered_Scores_Df$Score_Coping[1]) {
      AQOL8D.Coping.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Coping[length(Entered_Scores_Df$Score_Coping)] <= Entered_Scores_Df$Score_Coping[1]) {
      AQOL8D.Coping.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Coping[length(Entered_Scores_Df$Score_Coping)] < Entered_Scores_Df$Score_Coping[1]) {
      AQOL8D.Coping.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Coping[length(Entered_Scores_Df$Score_Coping)] >= Entered_Scores_Df$Score_Coping[1]) {
      AQOL8D.Coping.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Pain[length(Entered_Scores_Df$Score_Pain)] > Entered_Scores_Df$Score_Pain[1]) {
      AQOL8D.Pain.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Pain[length(Entered_Scores_Df$Score_Pain)] <= Entered_Scores_Df$Score_Pain[1]) {
      AQOL8D.Pain.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Pain[length(Entered_Scores_Df$Score_Pain)] < Entered_Scores_Df$Score_Pain[1]) {
      AQOL8D.Pain.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Pain[length(Entered_Scores_Df$Score_Pain)] >= Entered_Scores_Df$Score_Pain[1]) {
      AQOL8D.Pain.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Senses[length(Entered_Scores_Df$Score_Senses)] > Entered_Scores_Df$Score_Senses[1]) {
      AQOL8D.Senses.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Senses[length(Entered_Scores_Df$Score_Senses)] <= Entered_Scores_Df$Score_Senses[1]) {
      AQOL8D.Senses.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Senses[length(Entered_Scores_Df$Score_Senses)] < Entered_Scores_Df$Score_Senses[1]) {
      AQOL8D.Senses.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Senses[length(Entered_Scores_Df$Score_Senses)] >= Entered_Scores_Df$Score_Senses[1]) {
      AQOL8D.Senses.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Self_Worth[length(Entered_Scores_Df$Score_Self_Worth)] > Entered_Scores_Df$Score_Self_Worth[1]) {
      AQOL8D.Self.Worth.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Self_Worth[length(Entered_Scores_Df$Score_Self_Worth)] <= Entered_Scores_Df$Score_Self_Worth[1]) {
      AQOL8D.Self.Worth.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Self_Worth[length(Entered_Scores_Df$Score_Self_Worth)] < Entered_Scores_Df$Score_Self_Worth[1]) {
      AQOL8D.Self.Worth.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Self_Worth[length(Entered_Scores_Df$Score_Self_Worth)] >= Entered_Scores_Df$Score_Self_Worth[1]) {
      AQOL8D.Self.Worth.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Happiness[length(Entered_Scores_Df$Score_Happiness)] > Entered_Scores_Df$Score_Happiness[1]) {
      AQOL8D.Happiness.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Happiness[length(Entered_Scores_Df$Score_Happiness)] <= Entered_Scores_Df$Score_Happiness[1]) {
      AQOL8D.Happiness.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Happiness[length(Entered_Scores_Df$Score_Happiness)] < Entered_Scores_Df$Score_Happiness[1]) {
      AQOL8D.Happiness.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Happiness[length(Entered_Scores_Df$Score_Happiness)] >= Entered_Scores_Df$Score_Happiness[1]) {
      AQOL8D.Happiness.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Super_Mental[length(Entered_Scores_Df$Score_Super_Mental)] > Entered_Scores_Df$Score_Super_Mental[1]) {
      AQOL8D.Super.Mental.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Super_Mental[length(Entered_Scores_Df$Score_Super_Mental)] <= Entered_Scores_Df$Score_Super_Mental[1]) {
      AQOL8D.Super.Mental.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Super_Mental[length(Entered_Scores_Df$Score_Super_Mental)] < Entered_Scores_Df$Score_Super_Mental[1]) {
      AQOL8D.Super.Mental.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Super_Mental[length(Entered_Scores_Df$Score_Super_Mental)] >= Entered_Scores_Df$Score_Super_Mental[1]) {
      AQOL8D.Super.Mental.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Super_Physical[length(Entered_Scores_Df$Score_Super_Physical)] > Entered_Scores_Df$Score_Super_Physical[1]) {
      AQOL8D.Super.Physical.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Super_Physical[length(Entered_Scores_Df$Score_Super_Physical)] <= Entered_Scores_Df$Score_Super_Physical[1]) {
      AQOL8D.Super.Physical.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Super_Physical[length(Entered_Scores_Df$Score_Super_Physical)] < Entered_Scores_Df$Score_Super_Physical[1]) {
      AQOL8D.Super.Physical.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Super_Physical[length(Entered_Scores_Df$Score_Super_Physical)] >= Entered_Scores_Df$Score_Super_Physical[1]) {
      AQOL8D.Super.Physical.Deterioration<- "No"
    }
    
    AQOL8D.Fullscale.Change<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] - Entered_Scores_Df$Score[1]
    AQOL8D.Independent.Living.Change<- Entered_Scores_Df$Score_Independent_Living[length(Entered_Scores_Df$Score_Independent_Living)] - Entered_Scores_Df$Score_Independent_Living[1]
    AQOL8D.Relationships.Change<- Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] - Entered_Scores_Df$Score_Relationships[1]
    AQOL8D.Mental.Health.Change<- Entered_Scores_Df$Score_Mental_Health[length(Entered_Scores_Df$Score_Mental_Health)] - Entered_Scores_Df$Score_Mental_Health[1]
    AQOL8D.Coping.Change<- Entered_Scores_Df$Score_Coping[length(Entered_Scores_Df$Score_Coping)] - Entered_Scores_Df$Score_Coping[1]
    AQOL8D.Fullscale.Comparisons<- length(Entered_Scores_Df$Change) - 1
    AQOL8D.Independent.Living.Comparisons<- length(Entered_Scores_Df$Change_Independent_Living) - 1
    AQOL8D.Relationships.Comparisons<- length(Entered_Scores_Df$Change_Relationships) - 1
    AQOL8D.Mental.Health.Comparisons<- length(Entered_Scores_Df$Change_Mental_Health) - 1
    AQOL8D.Coping.Comparisons<- length(Entered_Scores_Df$Change_Coping) - 1
    AQOL8D.Fullscale.First.Date<- Entered_Scores_Df$Date[1]
    AQOL8D.Independent.Living.First.Date<- Entered_Scores_Df$Date[1]
    AQOL8D.Relationships.First.Date<- Entered_Scores_Df$Date[1]
    AQOL8D.Mental.Health.First.Date<- Entered_Scores_Df$Date[1]
    AQOL8D.Coping.First.Date<- Entered_Scores_Df$Date[1]
    AQOL8D.Fullscale.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    AQOL8D.Independent.Living.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    AQOL8D.Relationships.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    AQOL8D.Mental.Health.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    AQOL8D.Coping.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    AQOL8D.Fullscale.First.Score<- Entered_Scores_Df$Score[1]
    AQOL8D.Independent.Living.First.Score<- Entered_Scores_Df$Score_Independent_Living[1]
    AQOL8D.Relationships.First.Score<- Entered_Scores_Df$Score_Relationships[1]
    AQOL8D.Mental.Health.First.Score<- Entered_Scores_Df$Score_Mental_Health[1]
    AQOL8D.Coping.First.Score<- Entered_Scores_Df$Score_Coping[1]
    AQOL8D.Fullscale.Last.Score<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)]
    AQOL8D.Independent.Living.Last.Score<- Entered_Scores_Df$Score_Independent_Living[length(Entered_Scores_Df$Score_Independent_Living)]
    AQOL8D.Relationships.Last.Score<- Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)]
    AQOL8D.Mental.Health.Last.Score<- Entered_Scores_Df$Score_Mental_Health[length(Entered_Scores_Df$Score_Mental_Health)]
    AQOL8D.Coping.Last.Score<- Entered_Scores_Df$Score_Coping[length(Entered_Scores_Df$Score_Coping)]
    
    
    AQOL8D.Pain.Change<- Entered_Scores_Df$Score_Pain[length(Entered_Scores_Df$Score_Pain)] - Entered_Scores_Df$Score_Pain[1]
    AQOL8D.Senses.Change<- Entered_Scores_Df$Score_Senses[length(Entered_Scores_Df$Score_Senses)] - Entered_Scores_Df$Score_Senses[1]
    AQOL8D.Self.Worth.Change<- Entered_Scores_Df$Score_Self_Worth[length(Entered_Scores_Df$Score_Self_Worth)] - Entered_Scores_Df$Score_Self_Worth[1]
    AQOL8D.Happiness.Change<- Entered_Scores_Df$Score_Happiness[length(Entered_Scores_Df$Score_Happiness)] - Entered_Scores_Df$Score_Happiness[1]
    AQOL8D.Super.Mental.Change<- Entered_Scores_Df$Score_Super_Mental[length(Entered_Scores_Df$Score_Super_Mental)] - Entered_Scores_Df$Score_Super_Mental[1]
    AQOL8D.Super.Physical.Change<- Entered_Scores_Df$Score_Super_Physical[length(Entered_Scores_Df$Score_Super_Physical)] - Entered_Scores_Df$Score_Super_Physical[1]
    AQOL8D.Pain.Comparisons<- length(Entered_Scores_Df$Change_Pain) - 1
    AQOL8D.Senses.Comparisons<- length(Entered_Scores_Df$Change_Senses) - 1
    AQOL8D.Self.Worth.Comparisons<- length(Entered_Scores_Df$Change_Self_Worth) - 1
    AQOL8D.Happiness.Comparisons<- length(Entered_Scores_Df$Change_Happiness) - 1
    AQOL8D.Super.Mental.Comparisons<- length(Entered_Scores_Df$Change_Super_Mental) - 1
    AQOL8D.Super.Physical.Comparisons<- length(Entered_Scores_Df$Change_Super_Physical) - 1
    AQOL8D.Pain.First.Date<- Entered_Scores_Df$Date[1]
    AQOL8D.Senses.First.Date<- Entered_Scores_Df$Date[1]
    AQOL8D.Self.Worth.First.Date<- Entered_Scores_Df$Date[1]
    AQOL8D.Happiness.First.Date<- Entered_Scores_Df$Date[1]
    AQOL8D.Super.Mental.First.Date<- Entered_Scores_Df$Date[1]
    AQOL8D.Super.Physical.First.Date<- Entered_Scores_Df$Date[1]
    AQOL8D.Pain.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    AQOL8D.Senses.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    AQOL8D.Self.Worth.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    AQOL8D.Happiness.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    AQOL8D.Super.Mental.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    AQOL8D.Super.Physical.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    AQOL8D.Pain.First.Score<- Entered_Scores_Df$Score_Pain[1]
    AQOL8D.Senses.First.Score<- Entered_Scores_Df$Score_Senses[1]
    AQOL8D.Self.Worth.First.Score<- Entered_Scores_Df$Score_Self_Worth[1]
    AQOL8D.Happiness.First.Score<- Entered_Scores_Df$Score_Happiness[1]
    AQOL8D.Super.Mental.First.Score<- Entered_Scores_Df$Score_Super_Mental[1]
    AQOL8D.Super.Physical.First.Score<- Entered_Scores_Df$Score_Super_Physical[1]
    AQOL8D.Pain.Last.Score<- Entered_Scores_Df$Score_Pain[length(Entered_Scores_Df$Score_Pain)]
    AQOL8D.Senses.Last.Score<- Entered_Scores_Df$Score_Senses[length(Entered_Scores_Df$Score_Senses)]
    AQOL8D.Self.Worth.Last.Score<- Entered_Scores_Df$Score_Self_Worth[length(Entered_Scores_Df$Score_Self_Worth)]
    AQOL8D.Happiness.Last.Score<- Entered_Scores_Df$Score_Happiness[length(Entered_Scores_Df$Score_Happiness)]
    AQOL8D.Super.Mental.Last.Score<- Entered_Scores_Df$Score_Super_Mental[length(Entered_Scores_Df$Score_Super_Mental)]
    AQOL8D.Super.Physical.Last.Score<- Entered_Scores_Df$Score_Super_Physical[length(Entered_Scores_Df$Score_Super_Physical)]
    
    
    
    
    
    
    Analytics_Df<<- data.frame(AQOL8D.Fullscale.First.Date, AQOL8D.Fullscale.First.Score, AQOL8D.Fullscale.Comparisons, AQOL8D.Fullscale.Change, AQOL8D.Fullscale.Last.Date, AQOL8D.Fullscale.Last.Score, AQOL8D.Fullscale.Improvement,AQOL8D.Fullscale.Sig.Improvement, AQOL8D.Fullscale.Deterioration, AQOL8D.Fullscale.Sig.Deterioration,
                               AQOL8D.Independent.Living.First.Date, AQOL8D.Independent.Living.First.Score, AQOL8D.Independent.Living.Comparisons, AQOL8D.Independent.Living.Change, AQOL8D.Independent.Living.Last.Date, AQOL8D.Independent.Living.Last.Score, AQOL8D.Independent.Living.Improvement, AQOL8D.Independent.Living.Sig.Improvement, AQOL8D.Independent.Living.Deterioration, AQOL8D.Independent.Living.Sig.Deterioration,
                               AQOL8D.Relationships.First.Date, AQOL8D.Relationships.First.Score, AQOL8D.Relationships.Comparisons, AQOL8D.Relationships.Change, AQOL8D.Relationships.Last.Date, AQOL8D.Relationships.Last.Score, AQOL8D.Relationships.Improvement, AQOL8D.Relationships.Sig.Improvement, AQOL8D.Relationships.Deterioration, AQOL8D.Relationships.Sig.Deterioration, 
                               AQOL8D.Mental.Health.First.Date, AQOL8D.Mental.Health.First.Score, AQOL8D.Mental.Health.Comparisons, AQOL8D.Mental.Health.Change, AQOL8D.Mental.Health.Last.Date, AQOL8D.Mental.Health.Last.Score, AQOL8D.Mental.Health.Improvement, AQOL8D.Mental.Health.Sig.Improvement, AQOL8D.Mental.Health.Deterioration, AQOL8D.Mental.Health.Sig.Deterioration, 
                               AQOL8D.Coping.First.Date, AQOL8D.Coping.First.Score, AQOL8D.Coping.Comparisons, AQOL8D.Coping.Change, AQOL8D.Coping.Last.Date, AQOL8D.Coping.Last.Score, AQOL8D.Coping.Improvement, AQOL8D.Coping.Sig.Improvement, AQOL8D.Coping.Deterioration, AQOL8D.Coping.Sig.Deterioration,
                               AQOL8D.Pain.First.Date, AQOL8D.Pain.First.Score, AQOL8D.Pain.Comparisons, AQOL8D.Pain.Change, AQOL8D.Pain.Last.Date, AQOL8D.Pain.Last.Score, AQOL8D.Pain.Improvement,AQOL8D.Pain.Sig.Improvement, AQOL8D.Pain.Deterioration, AQOL8D.Pain.Sig.Deterioration,
                               AQOL8D.Senses.First.Date, AQOL8D.Senses.First.Score, AQOL8D.Senses.Comparisons, AQOL8D.Senses.Change, AQOL8D.Senses.Last.Date, AQOL8D.Senses.Last.Score, AQOL8D.Senses.Improvement, AQOL8D.Senses.Sig.Improvement, AQOL8D.Senses.Deterioration, AQOL8D.Senses.Sig.Deterioration,
                               AQOL8D.Self.Worth.First.Date, AQOL8D.Self.Worth.First.Score, AQOL8D.Self.Worth.Comparisons, AQOL8D.Self.Worth.Change, AQOL8D.Self.Worth.Last.Date, AQOL8D.Self.Worth.Last.Score, AQOL8D.Self.Worth.Improvement, AQOL8D.Self.Worth.Sig.Improvement, AQOL8D.Self.Worth.Deterioration, AQOL8D.Self.Worth.Sig.Deterioration, 
                               AQOL8D.Happiness.First.Date, AQOL8D.Happiness.First.Score, AQOL8D.Happiness.Comparisons, AQOL8D.Happiness.Change, AQOL8D.Happiness.Last.Date, AQOL8D.Happiness.Last.Score, AQOL8D.Happiness.Improvement, AQOL8D.Happiness.Sig.Improvement, AQOL8D.Happiness.Deterioration, AQOL8D.Happiness.Sig.Deterioration, 
                               AQOL8D.Super.Mental.First.Date, AQOL8D.Super.Mental.First.Score, AQOL8D.Super.Mental.Comparisons, AQOL8D.Super.Mental.Change, AQOL8D.Super.Mental.Last.Date, AQOL8D.Super.Mental.Last.Score, AQOL8D.Super.Mental.Improvement, AQOL8D.Super.Mental.Sig.Improvement, AQOL8D.Super.Mental.Deterioration, AQOL8D.Super.Mental.Sig.Deterioration,
                               AQOL8D.Super.Physical.First.Date, AQOL8D.Super.Physical.First.Score, AQOL8D.Super.Physical.Comparisons, AQOL8D.Super.Physical.Change, AQOL8D.Super.Physical.Last.Date, AQOL8D.Super.Physical.Last.Score, AQOL8D.Super.Physical.Improvement, AQOL8D.Super.Physical.Sig.Improvement, AQOL8D.Super.Physical.Deterioration, AQOL8D.Super.Physical.Sig.Deterioration)
    
    
    
  })
  
  
  
  #Create an expression to activate the entered data & create notifications to indicate inaccurate data entry
  
  Entered_Scores_Notifications<- observeEvent(input$Action_Submit_Data, {
    
    Entered_Scores_Reac()
    
    if(length(Score_1a) < 35) {
      showNotification("The AQOL8D is a 35-item scale. You have entered less than 35 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(length(Score_1a) > 35) {
      showNotification("The AQOL8D is a 35-item scale. You have entered more than 35 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(input$Timepoint != "1") {
      if(length(Score_2a) < 35) {
        showNotification("The AQOL8D is a 35-item scale. You have entered less than 35 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_2a) > 35) {
        showNotification("The AQOL8D is a 35-item scale. You have entered more than 35 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
    if(input$Timepoint == "3") {
      if(length(Score_3a) < 35) {
        showNotification("The AQOL8D is a 35-item scale. You have entered less than 35 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_3a) > 35) {
        showNotification("The AQOL8D is a 35-item scale. You have entered more than 35 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
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
    
    Gap_Independent_Living<- Entered_Scores_Df[1,11] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),11]
    Entered_Scores_Df[1,12]<- Gap_Independent_Living
    
    Gap_Relationships<- Entered_Scores_Df[1,20] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),20]
    Entered_Scores_Df[1,21]<- Gap_Relationships
    
    Gap_Mental_Health<- Entered_Scores_Df[1,29] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),29]
    Entered_Scores_Df[1,30]<- Gap_Mental_Health
    
    Gap_Coping<- Entered_Scores_Df[1,38] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),38]
    Entered_Scores_Df[1,39]<- Gap_Coping
    
    Gap_Pain<- Entered_Scores_Df[1,47] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),47]
    Entered_Scores_Df[1,48]<- Gap_Pain
    
    Gap_Senses<- Entered_Scores_Df[1,56] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),56]
    Entered_Scores_Df[1,57]<- Gap_Senses
    
    Gap_Self_Worth<- Entered_Scores_Df[1,65] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),65]
    Entered_Scores_Df[1,66]<- Gap_Self_Worth
    
    Gap_Happiness<- Entered_Scores_Df[1,74] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),74]
    Entered_Scores_Df[1,75]<- Gap_Happiness
    
    Gap_Super_Mental<- Entered_Scores_Df[1,83] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),83]
    Entered_Scores_Df[1,84]<- Gap_Super_Mental
    
    Gap_Super_Physical<- Entered_Scores_Df[1,92] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),83]
    Entered_Scores_Df[1,93]<- Gap_Super_Physical

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
  
  
  #Create pdf download functionality
  
  
  output$report <- downloadHandler(
    
    filename = paste0(" AQOL-8D Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
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
        Stats_Table_Fullscale = Stats_Table_Fullscale,
        Stats_Table_Independent_Living = Stats_Table_Independent_Living,
        Stats_Table_Relationships = Stats_Table_Relationships,
        Stats_Table_Mental_Health = Stats_Table_Mental_Health,
        Stats_Table_Coping = Stats_Table_Coping,
        Stats_Table_Pain = Stats_Table_Pain,
        Stats_Table_Senses = Stats_Table_Senses,
        Stats_Table_Self_Worth = Stats_Table_Self_Worth,
        Stats_Table_Happiness = Stats_Table_Happiness,
        Stats_Table_Super_Mental = Stats_Table_Super_Mental,
        Stats_Table_Super_Physical = Stats_Table_Super_Physical,
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
      paste(paste0(" AQOL-8D Data ", format(Sys.time(), '%d/%m/%y')), input$Output_Filetype1, sep = ".")
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













