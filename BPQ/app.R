
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
      menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "BPQ"),
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
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| Borderline Personality Questionnaire (BPQ)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 750),
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
                "Chanen, Jovev, Yuen & Rawlings (2008). Screeing for Borderline Personality Disorder in Outpatient Youth. Journal of Personality Disorders, 22(4), 353-364.", br(), br(),
                "Poreh, Rawlings & Claridge (2006). The BPQ: A scale for the assessment of borderline personality based on DSM-IV criteria. Journal of Personality Disorders, 20(3), 247-260."
        ),
        
        
        
        tabItem(tabName = "BPQ",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel("Scale",
                             wellPanel(style = "background-color: #ededed; color: black",
                                       fluidRow(
                                         column(width = 12, offset = 5, h3(tags$strong("BPQ")))
                                       ),
                                       hr(),
                                      fluidRow(style = "background-color: #ededed",
                                       column(width = 12, h4("Instructions: Please put a circle around the response that you feel best DESCRIBES YOUR USUAL SELF 
                                           (for the past two years or longer) in relation to each statement. Circle T if you think the statement is true. Circle F if you think 
                                           the statement is false. There are no right or wrong answers and there are no trick questions. Please respond as honestly as you can,
                                           but don't ponder too long over each item."))
                                             ),
                                       hr(),
                                       fluidRow(
                                         column(width = 10, h4("1. I often do things without thinking them through.")),
                                         column(width = 2, radioButtons("Item_1", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                       ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("2.	I often become depressed or anxious 'out of the blue'.")),
                                        column(width = 2, radioButtons("Item_2", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("3. People often leave me.")),
                                        column(width = 2, radioButtons("Item_3", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("4. I am rarely disappointed by my friends")),
                                        column(width = 2, radioButtons("Item_4", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("5.	I feel inferior to other people.")),
                                        column(width = 2, radioButtons("Item_5", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("6.	I have threatened to hurt myself in the past.")),
                                               column(width = 2, radioButtons("Item_6", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("7. I do not believe that I have the skills to do anything with my life.")),
                                        column(width = 2, radioButtons("Item_7", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("8. I rarely get angry at other people.")),
                                        column(width = 2, radioButtons("Item_8", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("9. Sometimes I feel like I am not real.")),
                                        column(width = 2, radioButtons("Item_9", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("10. I will not have sex with someone unless I have known them for quite some time.")),
                                               column(width = 2, radioButtons("Item_10", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("11.	I sometimes feel anxious or irritable and become sad a few hours later.")),
                                        column(width = 2, radioButtons("Item_11", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("12.	When people close to me die or leave me, I feel abandoned.")),
                                        column(width = 2, radioButtons("Item_12", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("13. I often exaggerate the potential of friendships only to find out later that they will not work out. ")),
                                        column(width = 2, radioButtons("Item_13", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("14. If I were more like other people I would feel better about myself.")),
                                               column(width = 2, radioButtons("Item_14", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("15. I have deliberately tried to hurt myself without trying to kill myself.")),
                                        column(width = 2, radioButtons("Item_15", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("16.	In general, my life is pretty boring.")),
                                        column(width = 2, radioButtons("Item_16", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("17.	I frequently get into physical fights.")),
                                        column(width = 2, radioButtons("Item_17", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("18. People are sometimes out to get me.")),
                                               column(width = 2, radioButtons("Item_18", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("19.	My friends have told me that my mood changes very quickly.")),
                                        column(width = 2, radioButtons("Item_19", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("20.	I am afraid to spend time alone.")),
                                        column(width = 2, radioButtons("Item_20", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("21.	People who seem trustworthy often disappoint me.")),
                                        column(width = 2, radioButtons("Item_21", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("22. I have made a suicide attempt in the past.")),
                                               column(width = 2, radioButtons("Item_22", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("23.	I often feel like I have nothing to offer others.")),
                                        column(width = 2, radioButtons("Item_23", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("24.	I have trouble controlling my temper.")),
                                        column(width = 2, radioButtons("Item_24", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("25.	I can read other people's minds.")),
                                        column(width = 2, radioButtons("Item_25", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("26.	I have tried 'hard' street drugs (e.g. cocaine, heroin).")),
                                               column(width = 2, radioButtons("Item_26", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("27.	My mood frequently alternates throughout the day between happiness, anger, anxiety and depression.")),
                                        column(width = 2, radioButtons("Item_27", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("28.	When my friends leave, I am confident I will see them again.")),
                                        column(width = 2, radioButtons("Item_28", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("29.	My friends often disappoint me.")),
                                        column(width = 2, radioButtons("Item_29", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("30.	I have cut myself on purpose.")),
                                               column(width = 2, radioButtons("Item_30", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("31.	I often feel lonely and deserted.")),
                                        column(width = 2, radioButtons("Item_31", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("32.	I have no difficulty controlling my temper.")),
                                        column(width = 2, radioButtons("Item_32", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("33.	I sometimes see or hear things that others cannot see or hear")),
                                        column(width = 2, radioButtons("Item_33", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("34. It is not unusual for me to have sex on the first date.")),
                                               column(width = 2, radioButtons("Item_34", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("35.	I sometimes feel very sad but this feeling can change quickly.")),
                                        column(width = 2, radioButtons("Item_35", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("36.	People often let me down.")),
                                        column(width = 2, radioButtons("Item_36", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("37.	I wish I could be more like some of my friends.")),
                                        column(width = 2, radioButtons("Item_37", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("38. I used to try to hurt myself to get attention.")),
                                               column(width = 2, radioButtons("Item_38", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("39.	I am often different with different people in different situations so that sometimes I am not sure who I am.")),
                                        column(width = 2, radioButtons("Item_39", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("40.	I easily become irritated by others.")),
                                        column(width = 2, radioButtons("Item_40", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("41.	Sometimes I can actually hear what other people are thinking.")),
                                        column(width = 2, radioButtons("Item_41", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("42. I get high on drugs whenever I feel like it.")),
                                               column(width = 2, radioButtons("Item_42", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("43.	I rarely feel sad or anxious.")),
                                        column(width = 2, radioButtons("Item_43", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("44.	No one loves me.")),
                                        column(width = 2, radioButtons("Item_44", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("45.	When I trust people, they rarely disappoint me.")),
                                        column(width = 2, radioButtons("Item_45", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("46. I feel that people would not like me if they really knew me well.")),
                                               column(width = 2, radioButtons("Item_46", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("47.	I get angry easily.")),
                                        column(width = 2, radioButtons("Item_47", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("48.	It is impossible to read others' minds.")),
                                        column(width = 2, radioButtons("Item_48", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("49.	I sometimes feel very happy but this feeling can change quickly.")),
                                        column(width = 2, radioButtons("Item_49", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("50.	I find it difficult to depend on others because they will not be there when I need them.")),
                                               column(width = 2, radioButtons("Item_50", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("51.	The relationships with people I care about have lots of ups and downs.")),
                                        column(width = 2, radioButtons("Item_51", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("52.	I feel comfortable acting like myself.")),
                                        column(width = 2, radioButtons("Item_52", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("53.	I have never made an attempt to hurt myself.")),
                                        column(width = 2, radioButtons("Item_53", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("54. I rarely feel lonely.")),
                                               column(width = 2, radioButtons("Item_54", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("55.	I often find that the littlest things make me angry.")),
                                        column(width = 2, radioButtons("Item_55", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("56.	Sometimes I can't tell between what is real and what I have imagined.")),
                                        column(width = 2, radioButtons("Item_56", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("57.	When I drink, I drink too much.")),
                                        column(width = 2, radioButtons("Item_57", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("58. I consider myself to be a moody person.")),
                                               column(width = 2, radioButtons("Item_58", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("59.	I have difficulty developing close relationships because people often abandon me.")),
                                        column(width = 2, radioButtons("Item_59", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("60.	My friends are always there when I need them.")),
                                        column(width = 2, radioButtons("Item_60", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("61.	I wish I were someone else.")),
                                        column(width = 2, radioButtons("Item_61", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("62. I feel like my life is not interesting.")),
                                               column(width = 2, radioButtons("Item_62", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("63.	When I am angry, I sometimes hit objects and break them.")),
                                        column(width = 2, radioButtons("Item_63", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("64.	I often receive speeding tickets.")),
                                        column(width = 2, radioButtons("Item_64", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("65.	I often feel like I am on an emotional 'roller coaster'.")),
                                        column(width = 2, radioButtons("Item_65", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("66. I feel like my family has deserted me.")),
                                               column(width = 2, radioButtons("Item_66", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("67.	I am very comfortable with who I am.")),
                                        column(width = 2, radioButtons("Item_67", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("68.	I often do things impulsively.")),
                                        column(width = 2, radioButtons("Item_68", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("69.	My life is without purpose.")),
                                        column(width = 2, radioButtons("Item_69", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("70. I am not sure what I want to do in the future.")),
                                               column(width = 2, radioButtons("Item_70", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("71.	At times I eat so much that I am in pain or have to force myself to throw up.")),
                                        column(width = 2, radioButtons("Item_71", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("72.	People tell me that I am a moody person.")),
                                        column(width = 2, radioButtons("Item_72", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("73.	The people I love often leave me.")),
                                        column(width = 2, radioButtons("Item_73", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("74. In social situations, I often feel that others will see through me and realise that I don’t have much to offer.")),
                                               column(width = 2, radioButtons("Item_74", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("75.	I have been in the hospital for trying to harm myself.")),
                                        column(width = 2, radioButtons("Item_75", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("76.	I often feel empty inside.")),
                                        column(width = 2, radioButtons("Item_76", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("77.	Others often make me angry.")),
                                        column(width = 2, radioButtons("Item_77", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                               column(width = 10, h4("78. I often become frantic when I think that someone I care about will leave me.")),
                                               column(width = 2, radioButtons("Item_78", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(
                                        column(width = 10, h4("79.	I am confused about my long-term goals.")),
                                        column(width = 2, radioButtons("Item_79", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      fluidRow(style = "background-color: #ffffff",
                                        column(width = 10, h4("80.	Others say I'm quick tempered.")),
                                        column(width = 2, radioButtons("Item_80", label = NULL, choices = c("T" = "1", "F" = "0"), inline = TRUE, selected = character(0)))
                                      ),
                                      hr(),
                                      fluidRow(
                                        column(width = 4, dateInput("Q_Date", "Date",  format = "dd/mm/yyyy")),
                                        column(width = 4, textInput("Q_Name", "Name")),
                                        column(width = 4, textInput("Q_Clin_Name", "Clinician's Name"))
                                      ),
                                      fluidRow(
                                        column(width = 12, h5("Scale Source: Poreh, Claridge, Rawlings & Shelton (2006)"))
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
                                                      selectInput("Pop", "", choices = c("University Student"))
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
                                                               selectInput("Select_CI", label = "BPQ total scale",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI == '2'",
                                                                                numericInput("Man_CI", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Impulsivity", label = "Impulsivity",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Impulsivity == '2'",
                                                                                numericInput("Man_CI_Impulsivity", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Affective_Instability", label = "Affective Instability",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Affective_Instability == '2'",
                                                                                numericInput("Man_CI_Affective_Instability", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Abandonment", label = "Abandonment",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Abandonment == '2'",
                                                                                numericInput("Man_CI_Abandonment", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Relationships", label = "Relationships",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Relationships == '2'",
                                                                                numericInput("Man_CI_Relationships", "Specify the width of the confidence interval", value = 0))
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               selectInput("Select_CI_Self_Image", label = "Self Image",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Self_Image == '2'",
                                                                                numericInput("Man_CI_Self_Image", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Suicide_Mutilation", label = "Suicide/Mutilation",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Suicide_Mutilation == '2'",
                                                                                numericInput("Man_CI_Suicide_Mutilation", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Emptiness", label = "Emptiness",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Emptiness == '2'",
                                                                                numericInput("Man_CI_Emptiness", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        column(width = 2,
                                                               selectInput("Select_CI_Anger", label = "Intense Anger",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Anger == '2'",
                                                                                numericInput("Man_CI_Anger", "Specify the width of the confidence interval", value = 0))
                                                        ),
                                                        
                                                        column(width = 2,
                                                               selectInput("Select_CI_Quasi_Psychotic", label = "Quasi-Psychotic",
                                                                           choices = list("No" = 1, "Yes" = 2),
                                                                           selected = 1),
                                                               conditionalPanel(condition = "input.Select_CI_Quasi_Psychotic == '2'",
                                                                                numericInput("Man_CI_Quasi_Psychotic", "Specify the width of the confidence interval", value = 0))
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
                                                               uiOutput("Mean_Widg_Impulsivity")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Affective_Instability")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Abandonment")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Relationships")
                                                        )
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Self_Image")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Suicide_Mutilation")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Emptiness")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Anger")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Mean_Widg_Quasi_Psychotic")
                                                        )
                                                      ),
                                                      
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Chelune et al. (1993)' || input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)' || input.RelChangeMethod == 'Speer (1992)'",
                                                                       
                                                                       h4(tags$strong("Enter a mean value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean", "BPQ total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Impulsivity", "Impulsivity", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Affective_Instability", "Affective Instability", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Abandonment", "Abandonment", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Relationships", "Relationships", value = 0)
                                                                         )
                                                                       ),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Self_Image", "Self Image total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Suicide_Mutilation", "Suicide/Mutilation", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Emptiness", "Emptiness", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Anger", "Intense Anger", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Mean_Quasi_Psychotic", "Quasi-Psychotic", value = 0)
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
                                                               uiOutput("Sd_Widg_Impulsivity")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Affective_Instability")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Abandonment")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Relationships")
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Self_Image")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Suicide_Mutilation")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Emptiness")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Anger")
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Sd_Widg_Quasi_Psychotic")
                                                        )
                                                      ),
                                                      conditionalPanel(condition = "input.RelChangeMethod == 'Crawford & Howell (1998)'|| input.RelChangeMethod == 'Maassen et al. (2006)' || input.RelChangeMethod == 'McSweeny et al. (1993)'",
                                                                       
                                                                       h4(tags$strong("Enter a standard deviation value for the retest timepoint")),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd", "BPQ total scale", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Impulsivity", "Impulsivity", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Affective_Instability", "Cognition Mood", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Abandonment", "Abandonment", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Relationships", "Relationships", value = 0)
                                                                         )
                                                                       ),
                                                                       fluidRow(
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Self_Image", "Self Image", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Suicide_Mutilation", "Suicide/Mutilation", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Emptiness", "Emptiness", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Anger", "Anger", value = 0)
                                                                         ),
                                                                         column(width = 2,
                                                                                numericInput("Retest_Sd_Quasi_Psychotic", "Quasi-Psychotic", value = 0)
                                                                         )
                                                                       )
                                                                       
                                                      )
                                             ),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      h4(tags$strong("Enter test-retest reliability values")), 
                                                      h6("* No test-retest reliability data was available for subscales. Reported subscale reliability values represent internal consistency (Kuder Richardson Formula 20; KR-20)."),
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability", "BPQ total scale", value = .92),
                                                               h6("Chanen, Jovev, Yuen & Rawlings (2008)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Impulsivity", "Impulsivity", value = .64),
                                                               h6("Poreh, Rawlings & Claridge (2006)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Affective_Instability", "Affective Instability", value = .89),
                                                               h6("Poreh, Rawlings & Claridge (2006)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Abandonment", "Abandonment", value = .67),
                                                               h6("Poreh, Rawlings & Claridge (2006)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Relationships", "Relationships", value = .85),
                                                               h6("Poreh, Rawlings & Claridge (2006)")
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               numericInput("Reliability_Self_Image", "Self Image", value = .79),
                                                               h6("Poreh, Rawlings & Claridge (2006)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Suicide_Mutilation", "Suicide/Mutilation", value = .77),
                                                               h6("Poreh, Rawlings & Claridge (2006)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Emptiness", "Emptiness", value = .81),
                                                               h6("Poreh, Rawlings & Claridge (2006)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Anger", "Intense Anger", value = .84),
                                                               h6("Poreh, Rawlings & Claridge (2006)")
                                                        ),
                                                        column(width = 2,
                                                               numericInput("Reliability_Quasi_Psychotic", "Quasi-Psychotic", value = .51),
                                                               h6("Poreh, Rawlings & Claridge (2006)")
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
                                                               uiOutput("Cutoff_Widg_Impulsivity_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Affective_Instability_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Abandonment_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Relationships_1") 
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Self_Image_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Suicide_Mutilation_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Emptiness_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Anger_1") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Quasi_Psychotic_1") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Second cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Impulsivity_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Affective_Instability_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Abandonment_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Relationships_2") 
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Self_Image_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Suicide_Mutilation_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Emptiness_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Anger_2") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Quasi_Psychotic_2") 
                                                        )
                                                        
                                                      ),
                                                      
                                                      hr(),
                                                      h4(tags$strong("Third cut-off score")),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Impulsivity_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Affective_Instability_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Abandonment_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Relationships_3") 
                                                        )
                                                        
                                                      ),
                                                      fluidRow(
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Self_Image_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Suicide_Mutilation_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Emptiness_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Anger_3") 
                                                        ),
                                                        column(width = 2,
                                                               uiOutput("Cutoff_Widg_Quasi_Psychotic_3") 
                                                        )
                                                        
                                                      )
                                                      , hr()
                                                      
                                             ),
                                             
                                             
                                             h4(tags$strong("Psychometric Properties of the BPQ Relevant to Assessing Reliable & Clinically Significant Change")),
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
                      input$Item_21, input$Item_22, input$Item_23, input$Item_24, input$Item_25, input$Item_26, input$Item_27, 
                      input$Item_28, input$Item_29, input$Item_30, input$Item_31, input$Item_32, input$Item_33, input$Item_34, input$Item_35, input$Item_36, 
                      input$Item_37, input$Item_38, input$Item_39, input$Item_40, 
                      input$Item_41, input$Item_42, input$Item_43, input$Item_44, input$Item_45, input$Item_46, input$Item_47, 
                      input$Item_48, input$Item_49, input$Item_50, input$Item_51, input$Item_52, input$Item_53, input$Item_54, input$Item_55, input$Item_56, 
                      input$Item_57, input$Item_58, input$Item_59, input$Item_60, 
                      input$Item_61, input$Item_62, input$Item_63, input$Item_64, input$Item_65, input$Item_66, input$Item_67, 
                      input$Item_68, input$Item_69, input$Item_70, input$Item_71, input$Item_72, input$Item_73, input$Item_74, input$Item_75, input$Item_76, 
                      input$Item_77, input$Item_78, input$Item_79, input$Item_80, sep = ",")
    
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
    
    if(input$Pop == "University Student") {
      Mean_Val<<-21.23
      Sd_Val<<- 13.91
      Source_Mean<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Sd<<- "Poreh, Rawlings & Claridge (2006)"
      Cut_Val_1<<- Mean_Val
      Cut_Val_2<<- Mean_Val + (2*Sd_Val)
      Cut_Val_3<<- 57
      Cut_Lab_1<<- "Mean"
      Cut_Lab_2<<- "Mean + 2 SD"
      Cut_Lab_3<<- "BPD Cutoff (Chanen et al., 2008)"
      Source_Cutoff_1<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_2<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_3<<- "Chanen, Jovev, Yuen & Rawlings (2008)"
      Mean_Val_Impulsivity<<-1.82
      Sd_Val_Impulsivity<<-1.72
      Cut_Val_Impulsivity_1<<- Mean_Val_Impulsivity 
      Cut_Val_Impulsivity_2<<- Mean_Val_Impulsivity + Sd_Val_Impulsivity
      Cut_Val_Impulsivity_3<<- Mean_Val_Impulsivity + (2*Sd_Val_Impulsivity)
      Cut_Lab_Impulsivity_1<<- "Mean"
      Cut_Lab_Impulsivity_2<<- "Mean + 1 SD"
      Cut_Lab_Impulsivity_3<<- "Mean + 2 SD"
      Source_Cutoff_Impulsivity_1<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Impulsivity_2<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Impulsivity_3<<- "Poreh, Rawlings & Claridge (2006)"
      Mean_Val_Affective_Instability<<- 4.39
      Sd_Val_Affective_Instability<<- 3.43
      Cut_Val_Affective_Instability_1<<- Mean_Val_Affective_Instability 
      Cut_Val_Affective_Instability_2<<- Mean_Val_Affective_Instability + Sd_Val_Affective_Instability
      Cut_Val_Affective_Instability_3<<- Mean_Val_Affective_Instability + (2*Sd_Val_Affective_Instability)
      Cut_Lab_Affective_Instability_1<<- "Mean"
      Cut_Lab_Affective_Instability_2<<- "Mean + 1 SD"
      Cut_Lab_Affective_Instability_3<<- "Mean + 2 SD"
      Source_Cutoff_Affective_Instability_1<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Affective_Instability_2<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Affective_Instability_3<<- "Poreh, Rawlings & Claridge (2006)"
      Mean_Val_Abandonment<<- 1.66
      Sd_Val_Abandonment<<- 1.73
      Cut_Val_Abandonment_1<<- Mean_Val_Abandonment 
      Cut_Val_Abandonment_2<<- Mean_Val_Abandonment + Sd_Val_Abandonment
      Cut_Val_Abandonment_3<<- Mean_Val_Abandonment + (2*Sd_Val_Abandonment)
      Cut_Lab_Abandonment_1<<- "Mean"
      Cut_Lab_Abandonment_2<<- "Mean + 1 SD"
      Cut_Lab_Abandonment_3<<- "Mean + 2 SD"
      Source_Cutoff_Abandonment_1<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Abandonment_2<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Abandonment_3<<- "Poreh, Rawlings & Claridge (2006)"
      Mean_Val_Relationships<<- 2.52
      Sd_Val_Relationships<<- 2.52
      Cut_Val_Relationships_1<<- Mean_Val_Relationships 
      Cut_Val_Relationships_2<<- Mean_Val_Relationships + Sd_Val_Relationships
      Cut_Val_Relationships_3<<- Mean_Val_Relationships + (2*Sd_Val_Relationships)
      Cut_Lab_Relationships_1<<- "Mean"
      Cut_Lab_Relationships_2<<- "Mean + 1 SD"
      Cut_Lab_Relationships_3<<- "Mean + 2 SD"
      Source_Cutoff_Relationships_1<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Relationships_2<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Relationships_3<<- "Poreh, Rawlings & Claridge (2006)"
      Mean_Val_Self_Image<<- 2.69
      Sd_Val_Self_Image<<- 2.42
      Source_Mean_Self_Image<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Sd_Self_Image<<- "Poreh, Rawlings & Claridge (2006)"
      Cut_Val_Self_Image_1<<- Mean_Val_Self_Image
      Cut_Val_Self_Image_2<<- Mean_Val_Self_Image + Sd_Val_Self_Image
      Cut_Val_Self_Image_3<<- Mean_Val_Self_Image + (2*Sd_Val_Self_Image)
      Cut_Lab_Self_Image_1<<- "Mean"
      Cut_Lab_Self_Image_2<<- "Mean + 1 SD"
      Cut_Lab_Self_Image_3<<- "Mean + 2 SD"
      Source_Cutoff_Self_Image_1<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Self_Image_2<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Self_Image_3<<- "Poreh, Rawlings & Claridge (2006)"
      Mean_Val_Suicide_Mutilation<<- .86
      Sd_Val_Suicide_Mutilation<<- 1.45
      Cut_Val_Suicide_Mutilation_1<<- Mean_Val_Suicide_Mutilation 
      Cut_Val_Suicide_Mutilation_2<<- Mean_Val_Suicide_Mutilation + Sd_Val_Suicide_Mutilation
      Cut_Val_Suicide_Mutilation_3<<- Mean_Val_Suicide_Mutilation + (2*Sd_Val_Suicide_Mutilation)
      Cut_Lab_Suicide_Mutilation_1<<- "Mean"
      Cut_Lab_Suicide_Mutilation_2<<- "Mean + 1 SD"
      Cut_Lab_Suicide_Mutilation_3<<- "Mean + 2 SD"
      Source_Cutoff_Suicide_Mutilation_1<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Suicide_Mutilation_2<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Suicide_Mutilation_3<<- "Poreh, Rawlings & Claridge (2006)"
      Mean_Val_Emptiness<<- 2.77
      Sd_Val_Emptiness<<- 2.57
      Cut_Val_Emptiness_1<<- Mean_Val_Emptiness 
      Cut_Val_Emptiness_2<<- Mean_Val_Emptiness + Sd_Val_Emptiness
      Cut_Val_Emptiness_3<<- Mean_Val_Emptiness + (2*Sd_Val_Emptiness)
      Cut_Lab_Emptiness_1<<- "Mean"
      Cut_Lab_Emptiness_2<<- "Mean + 1 SD"
      Cut_Lab_Emptiness_3<<- "Mean + 2 SD"
      Source_Cutoff_Emptiness_1<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Emptiness_2<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Emptiness_3<<- "Poreh, Rawlings & Claridge (2006)"
      Mean_Val_Anger<<- 2.79
      Sd_Val_Anger<<- 2.78
      Cut_Val_Anger_1<<- Mean_Val_Anger 
      Cut_Val_Anger_2<<- Mean_Val_Anger + Sd_Val_Anger
      Cut_Val_Anger_3<<- Mean_Val_Anger + (2*Sd_Val_Anger)
      Cut_Lab_Anger_1<<- "Mean"
      Cut_Lab_Anger_2<<- "Mean + 1 SD"
      Cut_Lab_Anger_3<<- "Mean + 2 SD"
      Source_Cutoff_Anger_1<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Anger_2<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Anger_3<<- "Poreh, Rawlings & Claridge (2006)"
      Mean_Val_Quasi_Psychotic<<- 1.73
      Sd_Val_Quasi_Psychotic<<- 1.39
      Cut_Val_Quasi_Psychotic_1<<- Mean_Val_Quasi_Psychotic 
      Cut_Val_Quasi_Psychotic_2<<- Mean_Val_Quasi_Psychotic + Sd_Val_Quasi_Psychotic
      Cut_Val_Quasi_Psychotic_3<<- Mean_Val_Quasi_Psychotic + (2*Sd_Val_Quasi_Psychotic)
      Cut_Lab_Quasi_Psychotic_1<<- "Mean"
      Cut_Lab_Quasi_Psychotic_2<<- "Mean + 1 SD"
      Cut_Lab_Quasi_Psychotic_3<<- "Mean + 2 SD"
      Source_Cutoff_Quasi_Psychotic_1<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Quasi_Psychotic_2<<- "Poreh, Rawlings & Claridge (2006)"
      Source_Cutoff_Quasi_Psychotic_3<<- "Poreh, Rawlings & Claridge (2006)"
      
    } 
    
  })
  
  
  output$Mean_Widg<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean", "BPQ total scale", Mean_Val),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Impulsivity<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Impulsivity", "Impulsivity", Mean_Val_Impulsivity),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Impulsivity", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Affective_Instability<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Affective_Instability", "Affective Instability", Mean_Val_Affective_Instability),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Affective_Instability", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Abandonment<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Abandonment", "Abandonment", Mean_Val_Abandonment),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Abandonment", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Relationships<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Relationships", "Relationships", Mean_Val_Relationships),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Relationships", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Self_Image<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Self_Image", "Self Image", Mean_Val_Self_Image),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Self_Image", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Suicide_Mutilation<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Suicide_Mutilation", "Suicide/Mutilation", Mean_Val_Suicide_Mutilation),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Suicide_Mutilation", suspendWhenHidden = FALSE)
  
  
  output$Mean_Widg_Emptiness<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Emptiness", "Emptiness", Mean_Val_Emptiness),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Emptiness", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Anger<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Anger", "Intense Anger", Mean_Val_Anger),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Anger", suspendWhenHidden = FALSE) 
  
  
  output$Mean_Widg_Quasi_Psychotic<- renderUI({
    
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Mean_Quasi_Psychotic", "Quasi-Psychotic", Mean_Val_Quasi_Psychotic),
      h6(paste("Reference:", Source_Mean))
    )
  })
  outputOptions(output, "Mean_Widg_Quasi_Psychotic", suspendWhenHidden = FALSE) 
  
  

  output$Sd_Widg<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd", "BPQ total scale", Sd_Val),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Impulsivity<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Impulsivity", "Impulsivity", Sd_Val_Impulsivity),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Impulsivity", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Affective_Instability<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Affective_Instability", "Affective Instability", Sd_Val_Affective_Instability),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Affective_Instability", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Abandonment<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Abandonment", "Abandonment", Sd_Val_Abandonment),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Abandonment", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Relationships<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Relationships", "Relationships", Sd_Val_Relationships),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Relationships", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Self_Image<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Self_Image", "Self Image", Sd_Val_Self_Image),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Self_Image", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Suicide_Mutilation<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Suicide_Mutilation", "Suicide/Mutilation", Sd_Val_Suicide_Mutilation),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Suicide_Mutilation", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Emptiness<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Emptiness", "Emptiness", Sd_Val_Emptiness),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Emptiness", suspendWhenHidden = FALSE)
  
  
  output$Sd_Widg_Anger<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Anger", "Intense Anger", Sd_Val_Anger),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Anger", suspendWhenHidden = FALSE)
  
  output$Sd_Widg_Quasi_Psychotic<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Pop_Sd_Quasi_Psychotic", "Quasi-Psychotic", Sd_Val_Quasi_Psychotic),
      h6(paste("Reference:", Source_Sd))
    )
  })
  outputOptions(output, "Sd_Widg_Quasi_Psychotic", suspendWhenHidden = FALSE)
  
  
  
  output$Cutoff_Widg_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_1", "BPQ total scale", as.numeric(Cut_Val_1)),
      textInput("Cutoff_Text_1", "Cut-Off Score Name", Cut_Lab_1),
      h6(paste("Reference:", Source_Cutoff_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Impulsivity_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Impulsivity_1", "Impulsivity", as.numeric(Cut_Val_Impulsivity_1)),
      textInput("Cutoff_Text_Impulsivity_1", "Cut-Off Score Name", Cut_Lab_Impulsivity_1),
      h6(paste("Reference:", Source_Cutoff_Impulsivity_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Impulsivity_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Affective_Instability_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Affective_Instability_1", "Affective Instability", as.numeric(Cut_Val_Affective_Instability_1)),
      textInput("Cutoff_Text_Affective_Instability_1", "Cut-Off Score Name", Cut_Lab_Affective_Instability_1),
      h6(paste("Reference:", Source_Cutoff_Affective_Instability_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Affective_Instability_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Abandonment_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Abandonment_1", "Abandonment", as.numeric(Cut_Val_Abandonment_1)),
      textInput("Cutoff_Text_Abandonment_1", "Cut-Off Score Name", Cut_Lab_Abandonment_1),
      h6(paste("Reference:", Source_Cutoff_Abandonment_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Abandonment_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Relationships_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Relationships_1", "Relationships", as.numeric(Cut_Val_Relationships_1)),
      textInput("Cutoff_Text_Relationships_1", "Cut-Off Score Name", Cut_Lab_Relationships_1),
      h6(paste("Reference:", Source_Cutoff_Relationships_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Relationships_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Self_Image_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Self_Image_1", "Self Image", as.numeric(Cut_Val_Self_Image_1)),
      textInput("Cutoff_Text_Self_Image_1", "Cut-Off Score Name", Cut_Lab_Self_Image_1),
      h6(paste("Reference:", Source_Cutoff_Self_Image_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Self_Image_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Suicide_Mutilation_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Suicide_Mutilation_1", "Suicide/Mutilation", as.numeric(Cut_Val_Suicide_Mutilation_1)),
      textInput("Cutoff_Text_Suicide_Mutilation_1", "Cut-Off Score Name", Cut_Lab_Suicide_Mutilation_1),
      h6(paste("Reference:", Source_Cutoff_Suicide_Mutilation_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Suicide_Mutilation_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Emptiness_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Emptiness_1", "Emptiness", as.numeric(Cut_Val_Emptiness_1)),
      textInput("Cutoff_Text_Emptiness_1", "Cut-Off Score Name", Cut_Lab_Emptiness_1),
      h6(paste("Reference:", Source_Cutoff_Emptiness_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Emptiness_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Anger_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Anger_1", "Intense Anger", as.numeric(Cut_Val_Anger_1)),
      textInput("Cutoff_Text_Anger_1", "Cut-Off Score Name", Cut_Lab_Anger_1),
      h6(paste("Reference:", Source_Cutoff_Anger_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Anger_1", suspendWhenHidden = FALSE)
  
  output$Cutoff_Widg_Quasi_Psychotic_1<- renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Quasi_Psychotic_1", "Quasi-Psychotic ", as.numeric(Cut_Val_Quasi_Psychotic_1)),
      textInput("Cutoff_Text_Quasi_Psychotic_1", "Cut-Off Score Name", Cut_Lab_Quasi_Psychotic_1),
      h6(paste("Reference:", Source_Cutoff_Quasi_Psychotic_1))
    )
  })
  outputOptions(output, "Cutoff_Widg_Quasi_Psychotic_1", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_2", "BPQ total scale", as.numeric(Cut_Val_2)),
      textInput("Cutoff_Text_2", "Cut-Off Score Name", Cut_Lab_2),
      h6(paste("Reference:", Source_Cutoff_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Impulsivity_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Impulsivity_2", "Impulsivity", as.numeric(Cut_Val_Impulsivity_2)),
      textInput("Cutoff_Text_Impulsivity_2", "Cut-Off Score Name", Cut_Lab_Impulsivity_2),
      h6(paste("Reference:", Source_Cutoff_Impulsivity_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Impulsivity_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Affective_Instability_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Affective_Instability_2", "Affective Instability", as.numeric(Cut_Val_Affective_Instability_2)),
      textInput("Cutoff_Text_Affective_Instability_2", "Cut-Off Score Name", Cut_Lab_Affective_Instability_2),
      h6(paste("Reference:", Source_Cutoff_Affective_Instability_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Affective_Instability_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Abandonment_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Abandonment_2", "Abandonment", as.numeric(Cut_Val_Abandonment_2)),
      textInput("Cutoff_Text_Abandonment_2", "Cut-Off Score Name", Cut_Lab_Abandonment_2),
      h6(paste("Reference:", Source_Cutoff_Abandonment_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Abandonment_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Relationships_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Relationships_2", "Relationships", as.numeric(Cut_Val_Relationships_2)),
      textInput("Cutoff_Text_Relationships_2", "Cut-Off Score Name", Cut_Lab_Relationships_2),
      h6(paste("Reference:", Source_Cutoff_Relationships_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Relationships_2", suspendWhenHidden = FALSE)
  

  output$Cutoff_Widg_Self_Image_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Self_Image_2", "Self Image", as.numeric(Cut_Val_Self_Image_2)),
      textInput("Cutoff_Text_Self_Image_2", "Cut-Off Score Name", Cut_Lab_Self_Image_2),
      h6(paste("Reference:", Source_Cutoff_Self_Image_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Self_Image_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Suicide_Mutilation_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Suicide_Mutilation_2", "Suicide/Mutilation", as.numeric(Cut_Val_Suicide_Mutilation_2)),
      textInput("Cutoff_Text_Suicide_Mutilation_2", "Cut-Off Score Name", Cut_Lab_Suicide_Mutilation_2),
      h6(paste("Reference:", Source_Cutoff_Suicide_Mutilation_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Suicide_Mutilation_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Emptiness_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Emptiness_2", "Emptiness", as.numeric(Cut_Val_Emptiness_2)),
      textInput("Cutoff_Text_Emptiness_2", "Cut-Off Score Name", Cut_Lab_Emptiness_2),
      h6(paste("Reference:", Source_Cutoff_Emptiness_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Emptiness_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Anger_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Anger_2", "Intense Anger", as.numeric(Cut_Val_Anger_2)),
      textInput("Cutoff_Text_Anger_2", "Cut-Off Score Name", Cut_Lab_Anger_2),
      h6(paste("Reference:", Source_Cutoff_Anger_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Anger_2", suspendWhenHidden = FALSE)
  
  output$Cutoff_Widg_Quasi_Psychotic_2<-  renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Quasi_Psychotic_2", "Quasi-Psychotic ", as.numeric(Cut_Val_Quasi_Psychotic_2)),
      textInput("Cutoff_Text_Quasi_Psychotic_2", "Cut-Off Score Name", Cut_Lab_Quasi_Psychotic_2),
      h6(paste("Reference:", Source_Cutoff_Quasi_Psychotic_2))
    )
  })
  outputOptions(output, "Cutoff_Widg_Quasi_Psychotic_2", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_3", "BPQ total scale", as.numeric(Cut_Val_3)),
      textInput("Cutoff_Text_3", "Cut-Off Score Name", Cut_Lab_3),
      h6(paste("Reference:", Source_Cutoff_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Impulsivity_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Impulsivity_3", "Impulsivity", as.numeric(Cut_Val_Impulsivity_3)),
      textInput("Cutoff_Text_Impulsivity_3", "Cut-Off Score Name", Cut_Lab_Impulsivity_3),
      h6(paste("Reference:", Source_Cutoff_Impulsivity_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Impulsivity_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Affective_Instability_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Affective_Instability_3", "Affective Instability", as.numeric(Cut_Val_Affective_Instability_3)),
      textInput("Cutoff_Text_Affective_Instability_3", "Cut-Off Score Name", Cut_Lab_Affective_Instability_3),
      h6(paste("Reference:", Source_Cutoff_Affective_Instability_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Affective_Instability_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Abandonment_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Abandonment_3", "Abandonment", as.numeric(Cut_Val_Abandonment_3)),
      textInput("Cutoff_Text_Abandonment_3", "Cut-Off Score Name", Cut_Lab_Abandonment_3),
      h6(paste("Reference:", Source_Cutoff_Abandonment_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Abandonment_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Relationships_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Relationships_3", "Relationships", as.numeric(Cut_Val_Relationships_3)),
      textInput("Cutoff_Text_Relationships_3", "Cut-Off Score Name", Cut_Lab_Relationships_3),
      h6(paste("Reference:", Source_Cutoff_Relationships_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Relationships_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Self_Image_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Self_Image_3", "Self Image", as.numeric(Cut_Val_Self_Image_3)),
      textInput("Cutoff_Text_Self_Image_3", "Cut-Off Score Name", Cut_Lab_Self_Image_3),
      h6(paste("Reference:", Source_Cutoff_Self_Image_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Self_Image_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Suicide_Mutilation_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Suicide_Mutilation_3", "Suicide/Mutilation", as.numeric(Cut_Val_Suicide_Mutilation_3)),
      textInput("Cutoff_Text_Suicide_Mutilation_3", "Cut-Off Score Name", Cut_Lab_Suicide_Mutilation_3),
      h6(paste("Reference:", Source_Cutoff_Suicide_Mutilation_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Suicide_Mutilation_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Emptiness_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Emptiness_3", "Emptiness", as.numeric(Cut_Val_Emptiness_3)),
      textInput("Cutoff_Text_Emptiness_3", "Cut-Off Score Name", Cut_Lab_Emptiness_3),
      h6(paste("Reference:", Source_Cutoff_Emptiness_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Emptiness_3", suspendWhenHidden = FALSE)
  
  
  output$Cutoff_Widg_Anger_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Anger_3", "Intense Anger", as.numeric(Cut_Val_Anger_3)),
      textInput("Cutoff_Text_Anger_3", "Cut-Off Score Name", Cut_Lab_Anger_3),
      h6(paste("Reference:", Source_Cutoff_Anger_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Anger_3", suspendWhenHidden = FALSE)
  
  output$Cutoff_Widg_Quasi_Psychotic_3<-renderUI({
    CI_Vals_Reac()
    tagList(
      numericInput("Cutoff_Quasi_Psychotic_3", "Quasi-Psychotic ", as.numeric(Cut_Val_Quasi_Psychotic_3)),
      textInput("Cutoff_Text_Quasi_Psychotic_3", "Cut-Off Score Name", Cut_Lab_Quasi_Psychotic_3),
      h6(paste("Reference:", Source_Cutoff_Quasi_Psychotic_3))
    )
  })
  outputOptions(output, "Cutoff_Widg_Quasi_Psychotic_3", suspendWhenHidden = FALSE)
  
  

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
    M_Impulsivity<- input$Pop_Mean_Impulsivity
    SD_Impulsivity<-input$Pop_Sd_Impulsivity
    M_Affective_Instability<- input$Pop_Mean_Affective_Instability
    SD_Affective_Instability<- input$Pop_Sd_Affective_Instability
    M_Abandonment<- input$Pop_Mean_Abandonment
    SD_Abandonment<- input$Pop_Sd_Abandonment
    M_Relationships<- input$Pop_Mean_Relationships
    SD_Relationships<- input$Pop_Sd_Relationships
    M_Self_Image<- input$Pop_Mean_Self_Image
    SD_Self_Image<-input$Pop_Sd_Self_Image
    M_Suicide_Mutilation<- input$Pop_Mean_Suicide_Mutilation
    SD_Suicide_Mutilation<- input$Pop_Sd_Suicide_Mutilation
    M_Emptiness<- input$Pop_Mean_Emptiness
    SD_Emptiness<- input$Pop_Sd_Emptiness
    M_Anger<- input$Pop_Mean_Anger
    SD_Anger<- input$Pop_Sd_Anger
    M_Quasi_Psychotic<- input$Pop_Mean_Quasi_Psychotic
    SD_Quasi_Psychotic<- input$Pop_Sd_Quasi_Psychotic
    
    
    M_Retest<- input$Retest_Mean
    SD_Retest<- input$Retest_Sd
    M_Retest_Impulsivity<- input$Retest_Mean_Impulsivity
    SD_Retest_Impulsivity<- input$Retest_Sd_Impulsivity
    M_Retest_Affective_Instability<- input$Retest_Mean_Affective_Instability
    SD_Retest_Affective_Instability<- input$Retest_Sd_Affective_Instability
    M_Retest_Abandonment<- input$Retest_Mean_Abandonment
    SD_Retest_Abandonment<- input$Retest_Sd_Abandonment
    M_Retest_Relationships<- input$Retest_Mean_Relationships
    SD_Retest_Relationships<- input$Retest_Sd_Relationships
    M_Retest_Self_Image<- input$Retest_Mean_Self_Image
    SD_Retest_Self_Image<- input$Retest_Sd_Self_Image
    M_Retest_Suicide_Mutilation<- input$Retest_Mean_Suicide_Mutilation
    SD_Retest_Suicide_Mutilation<- input$Retest_Sd_Suicide_Mutilation
    M_Retest_Emptiness<- input$Retest_Mean_Emptiness
    SD_Retest_Emptiness<- input$Retest_Sd_Emptiness
    M_Retest_Anger<- input$Retest_Mean_Anger
    SD_Retest_Anger<- input$Retest_Sd_Anger
    M_Retest_Quasi_Psychotic<- input$Retest_Mean_Quasi_Psychotic
    SD_Retest_Quasi_Psychotic<- input$Retest_Sd_Quasi_Psychotic
    
    
    SampleN<- input$SampleN
    
    Rel<- input$Reliability
    Rel_Impulsivity<- input$Reliability_Impulsivity
    Rel_Affective_Instability<- input$Reliability_Affective_Instability
    Rel_Abandonment<- input$Reliability_Abandonment
    Rel_Relationships<- input$Reliability_Relationships
    Rel_Self_Image<- input$Reliability_Self_Image
    Rel_Suicide_Mutilation<- input$Reliability_Suicide_Mutilation
    Rel_Emptiness<- input$Reliability_Emptiness
    Rel_Anger<- input$Reliability_Anger
    Rel_Quasi_Psychotic<- input$Reliability_Quasi_Psychotic
    
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
      SE_Impulsivity<-SD_Impulsivity * sqrt(1 - Rel_Impulsivity^2)
      SE_Affective_Instability<-SD_Affective_Instability * sqrt(1 - Rel_Affective_Instability^2)
      SE_Abandonment<-SD_Abandonment * sqrt(1 - Rel_Abandonment^2)
      SE_Relationships<-SD_Relationships * sqrt(1 - Rel_Relationships^2)
      SE<- round(SE, digits = 2)
      SE_Impulsivity<- round(SE_Impulsivity, digits = 2)
      SE_Affective_Instability<- round(SE_Affective_Instability, digits = 2)
      SE_Abandonment<- round(SE_Abandonment, digits = 2)
      SE_Relationships<- round(SE_Relationships, digits = 2)
      SE_Self_Image<-SD_Self_Image * sqrt(1 - Rel_Self_Image^2)
      SE_Suicide_Mutilation<-SD_Suicide_Mutilation * sqrt(1 - Rel_Suicide_Mutilation^2)
      SE_Emptiness<-SD_Emptiness * sqrt(1 - Rel_Emptiness^2)
      SE_Anger<-SD_Anger * sqrt(1 - Rel_Anger^2)
      SE_Quasi_Psychotic<- SD_Quasi_Psychotic * sqrt(1 - Rel_Quasi_Psychotic^2)
      SE_Self_Image<- round(SE_Self_Image, digits = 2)
      SE_Suicide_Mutilation<- round(SE_Suicide_Mutilation, digits = 2)
      SE_Emptiness<- round(SE_Emptiness, digits = 2)
      SE_Anger<- round(SE_Anger, digits = 2)
      SE_Quasi_Psychotic<- round(SE_Quasi_Psychotic, digits = 2)
    } else if (input$RelChangeMethod == "Chelune et al. (1993)" | input$RelChangeMethod == "Jacobson & Truax (1991)" | input$RelChangeMethod == "Speer (1992)") {
      SE<- sqrt((2*(SD^2))*(1-Rel))
      SE_Impulsivity<- sqrt((2*(SD_Impulsivity^2))*(1-Rel_Impulsivity))
      SE_Affective_Instability<- sqrt((2*(SD_Affective_Instability^2))*(1-Rel_Affective_Instability))
      SE_Abandonment<- sqrt((2*(SD_Abandonment^2))*(1-Rel_Abandonment))
      SE_Relationships<- sqrt((2*(SD_Relationships^2))*(1-Rel_Relationships))
      SE_Self_Image<- sqrt((2*(SD_Self_Image^2))*(1-Rel_Self_Image))
      SE_Suicide_Mutilation<- sqrt((2*(SD_Suicide_Mutilation^2))*(1-Rel_Suicide_Mutilation))
      SE_Emptiness<- sqrt((2*(SD_Emptiness^2))*(1-Rel_Emptiness))
      SE_Anger<- sqrt((2*(SD_Anger^2))*(1-Rel_Anger))
      SE_Quasi_Psychotic<- sqrt((2*(SD_Quasi_Psychotic^2))*(1-Rel_Quasi_Psychotic))
      SE<- round(SE, digits = 2)
      SE_Impulsivity<- round(SE_Impulsivity, digits = 2)
      SE_Affective_Instability<- round(SE_Affective_Instability, digits = 2)
      SE_Abandonment<- round(SE_Abandonment, digits = 2)
      SE_Relationships<- round(SE_Relationships, digits = 2)
      SE_Self_Image<- round(SE_Self_Image, digits = 2)
      SE_Suicide_Mutilation<- round(SE_Suicide_Mutilation, digits = 2)
      SE_Emptiness<- round(SE_Emptiness, digits = 2)
      SE_Anger<- round(SE_Anger, digits = 2)
      SE_Quasi_Psychotic<- round(SE_Quasi_Psychotic, digits = 2)
    } else if (input$RelChangeMethod == "Maassen et al. (2006)") {
      SE<- sqrt((SD^2 + SD_Retest^2)*(1-Rel))
      SE_Impulsivity<- sqrt((SD_Impulsivity^2 + SD_Retest_Impulsivity^2)*(1-Rel_Impulsivity))
      SE_Affective_Instability<- sqrt((SD_Affective_Instability^2 + SD_Retest_Affective_Instability^2)*(1-Rel_Affective_Instability))
      SE_Abandonment<- sqrt((SD_Abandonment^2 + SD_Retest_Abandonment^2)*(1-Rel_Abandonment))
      SE_Relationships<- sqrt((SD_Relationships^2 + SD_Retest_Relationships^2)*(1-Rel_Relationships))
      SE<- round(SE, digits = 2)
      SE_Impulsivity<- round(SE_Impulsivity, digits = 2)
      SE_Affective_Instability<- round(SE_Affective_Instability, digits = 2)
      SE_Abandonment<- round(SE_Abandonment, digits = 2)
      SE_Relationships<- round(SE_Relationships, digits = 2)
      SE_Self_Image<- sqrt((SD_Self_Image^2 + SD_Retest_Self_Image^2)*(1-Rel_Self_Image))
      SE_Suicide_Mutilation<- sqrt((SD_Suicide_Mutilation^2 + SD_Retest_Suicide_Mutilation^2)*(1-Rel_Suicide_Mutilation))
      SE_Emptiness<- sqrt((SD_Emptiness^2 + SD_Retest_Emptiness^2)*(1-Rel_Emptiness))
      SE_Anger<- sqrt((SD_Anger^2 + SD_Retest_Anger^2)*(1-Rel_Anger))
      SE_Quasi_Psychotic<- sqrt((SD_Quasi_Psychotic^2 + SD_Retest_Quasi_Psychotic^2)*(1-Rel_Quasi_Psychotic))
      SE<- round(SE, digits = 2)
      SE_Self_Image<- round(SE_Self_Image, digits = 2)
      SE_Suicide_Mutilation<- round(SE_Suicide_Mutilation, digits = 2)
      SE_Emptiness<- round(SE_Emptiness, digits = 2)
      SE_Anger<- round(SE_Anger, digits = 2)
      SE_Quasi_Psychotic<- round(SE_Quasi_Psychotic, digits = 2)
    } else if (input$RelChangeMethod == "McSweeny et al. (1993)") {
      SE<- SD_Retest*sqrt(1 - Rel^2)
      SE_Impulsivity<- SD_Retest_Impulsivity*sqrt(1 - Rel_Impulsivity^2)
      SE_Affective_Instability<- SD_Retest_Affective_Instability*sqrt(1 - Rel_Affective_Instability^2)
      SE_Abandonment<- SD_Retest_Abandonment*sqrt(1 - Rel_Abandonment^2)
      SE_Relationships<- SD_Retest_Relationships*sqrt(1 - Rel_Relationships^2)
      SE<- round(SE, digits = 2)
      SE_Impulsivity<- round(SE_Impulsivity, digits = 2)
      SE_Affective_Instability<- round(SE_Affective_Instability, digits = 2)
      SE_Abandonment<- round(SE_Abandonment, digits = 2)
      SE_Relationships<- round(SE_Relationships, digits = 2)
      SE_Self_Image<- SD_Retest_Self_Image*sqrt(1 - Rel_Self_Image^2)
      SE_Suicide_Mutilation<- SD_Retest_Suicide_Mutilation*sqrt(1 - Rel_Suicide_Mutilation^2)
      SE_Emptiness<- SD_Retest_Emptiness*sqrt(1 - Rel_Emptiness^2)
      SE_Anger<- SD_Retest_Anger*sqrt(1 - Rel_Anger^2)
      SE_Quasi_Psychotic<- SD_Retest_Quasi_Psychotic*sqrt(1 - Rel_Quasi_Psychotic^2)
      SE<- round(SE, digits = 2)
      SE_Self_Image<- round(SE_Self_Image, digits = 2)
      SE_Suicide_Mutilation<- round(SE_Suicide_Mutilation, digits = 2)
      SE_Emptiness<- round(SE_Emptiness, digits = 2)
      SE_Anger<- round(SE_Anger, digits = 2)
      SE_Quasi_Psychotic<- round(SE_Quasi_Psychotic, digits = 2)
    }
    
    #Caclculate McSweeny standard error to be employed in deriving the Crawford & Howell standard error if selected
    
    McSweeny_SE<- SD_Retest*sqrt(1 - Rel^2)
    McSweeny_SE_Impulsivity<- SD_Retest_Impulsivity*sqrt(1 - Rel_Impulsivity^2)
    McSweeny_SE_Affective_Instability<- SD_Retest_Affective_Instability*sqrt(1 - Rel_Affective_Instability^2)
    McSweeny_SE_Abandonment<- SD_Retest_Abandonment*sqrt(1 - Rel_Abandonment^2)
    McSweeny_SE_Relationships<- SD_Retest_Relationships*sqrt(1 - Rel_Relationships^2)
    McSweeny_SE_Self_Image<- SD_Retest_Self_Image*sqrt(1 - Rel_Self_Image^2)
    McSweeny_SE_Suicide_Mutilation<- SD_Retest_Suicide_Mutilation*sqrt(1 - Rel_Suicide_Mutilation^2)
    McSweeny_SE_Emptiness<- SD_Retest_Emptiness*sqrt(1 - Rel_Emptiness^2)
    McSweeny_SE_Anger<- SD_Retest_Anger*sqrt(1 - Rel_Anger^2)
    McSweeny_SE_Quasi_Psychotic<- SD_Retest_Quasi_Psychotic*sqrt(1 - Rel_Quasi_Psychotic^2)
    
    #Generate object storing names of cut-off scores
    
    Cutoff_Name_1<- input$Cutoff_Text_1
    Cutoff_Name_2<- input$Cutoff_Text_2
    Cutoff_Name_3<- input$Cutoff_Text_3
    Cutoff_Name_Impulsivity_1<- input$Cutoff_Text_Impulsivity_1
    Cutoff_Name_Impulsivity_2<- input$Cutoff_Text_Impulsivity_2
    Cutoff_Name_Impulsivity_3<- input$Cutoff_Text_Impulsivity_3
    Cutoff_Name_Affective_Instability_1<- input$Cutoff_Text_Affective_Instability_1
    Cutoff_Name_Affective_Instability_2<- input$Cutoff_Text_Affective_Instability_2
    Cutoff_Name_Affective_Instability_3<- input$Cutoff_Text_Affective_Instability_3
    Cutoff_Name_Abandonment_1<- input$Cutoff_Text_Abandonment_1
    Cutoff_Name_Abandonment_2<- input$Cutoff_Text_Abandonment_2
    Cutoff_Name_Abandonment_3<- input$Cutoff_Text_Abandonment_3
    Cutoff_Name_Relationships_1<- input$Cutoff_Text_Relationships_1
    Cutoff_Name_Relationships_2<- input$Cutoff_Text_Relationships_2
    Cutoff_Name_Relationships_3<- input$Cutoff_Text_Relationships_3
    Cutoff_Name_Self_Image_1<- input$Cutoff_Text_Self_Image_1
    Cutoff_Name_Self_Image_2<- input$Cutoff_Text_Self_Image_2
    Cutoff_Name_Self_Image_3<- input$Cutoff_Text_Self_Image_3
    Cutoff_Name_Suicide_Mutilation_1<- input$Cutoff_Text_Suicide_Mutilation_1
    Cutoff_Name_Suicide_Mutilation_2<- input$Cutoff_Text_Suicide_Mutilation_2
    Cutoff_Name_Suicide_Mutilation_3<- input$Cutoff_Text_Suicide_Mutilation_3
    Cutoff_Name_Emptiness_1<- input$Cutoff_Text_Emptiness_1
    Cutoff_Name_Emptiness_2<- input$Cutoff_Text_Emptiness_2
    Cutoff_Name_Emptiness_3<- input$Cutoff_Text_Emptiness_3
    Cutoff_Name_Anger_1<- input$Cutoff_Text_Anger_1
    Cutoff_Name_Anger_2<- input$Cutoff_Text_Anger_2
    Cutoff_Name_Anger_3<- input$Cutoff_Text_Anger_3
    Cutoff_Name_Quasi_Psychotic_1<- input$Cutoff_Text_Quasi_Psychotic_1
    Cutoff_Name_Quasi_Psychotic_2<- input$Cutoff_Text_Quasi_Psychotic_2
    Cutoff_Name_Quasi_Psychotic_3<- input$Cutoff_Text_Quasi_Psychotic_3
    
    
    Cutoff_Names<<- data.frame(Cutoff_Name_1,Cutoff_Name_2,Cutoff_Name_3,Cutoff_Name_Impulsivity_1,Cutoff_Name_Impulsivity_2,Cutoff_Name_Impulsivity_3,
                               Cutoff_Name_Affective_Instability_1, Cutoff_Name_Affective_Instability_2, Cutoff_Name_Affective_Instability_3, Cutoff_Name_Abandonment_1,
                               Cutoff_Name_Abandonment_2, Cutoff_Name_Abandonment_3, Cutoff_Name_Relationships_1, Cutoff_Name_Relationships_2, Cutoff_Name_Relationships_3,
                               Cutoff_Name_Self_Image_1,Cutoff_Name_Self_Image_2,Cutoff_Name_Self_Image_3,
                               Cutoff_Name_Suicide_Mutilation_1, Cutoff_Name_Suicide_Mutilation_2, Cutoff_Name_Suicide_Mutilation_3, Cutoff_Name_Emptiness_1,
                               Cutoff_Name_Emptiness_2, Cutoff_Name_Emptiness_3, Cutoff_Name_Anger_1, Cutoff_Name_Anger_2, Cutoff_Name_Anger_3, 
                               Cutoff_Name_Quasi_Psychotic_1, Cutoff_Name_Quasi_Psychotic_2, Cutoff_Name_Quasi_Psychotic_3 
                               )
    
    #Define the data & calculate PS, SE, confidence intervals and cut-off scores
    
    if(input$Timepoint == "1") {
      Date<- input$Date_1
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_1a), Score = Score_1a)
      Recode<- car::recode(Score_1a[c(10,43,28,4,45,60,52,67,53,54,8,32,48)],'0=1; 1=0')
      Score_1a[c(10,43,28,4,45,60,52,67,53,54,8,32,48)]<- Recode
      Score<- sum(Score_1a, na.rm = TRUE)
      Score<- round(Score, digits = 2)
      Score_Impulsivity<- sum(Score_1a[c(1,10,26,34,42,57,64,68,71)], na.rm = TRUE)
      Score_Affective_Instability<- sum(Score_1a[c(2, 11, 19, 27, 35, 43, 49, 58, 65, 72)], na.rm = TRUE)
      Score_Abandonment<- sum(Score_1a[c(3, 12, 20, 28, 44, 50, 59, 66, 73, 78)], na.rm = TRUE)
      Score_Relationships<- sum(Score_1a[c(4, 13, 21, 29, 36, 45, 51, 60)], na.rm = TRUE)
      Score_Self_Image<- sum(Score_1a[c(5, 14, 37, 46, 52, 61, 67, 70, 74)], na.rm = TRUE)
      Score_Suicide_Mutilation<- sum(Score_1a[c(6, 15, 22, 30, 38, 53, 75)], na.rm = TRUE)
      Score_Emptiness<- sum(Score_1a[c(7, 16, 23, 31, 39, 54, 62, 69, 76, 79)], na.rm = TRUE)
      Score_Anger<- sum(Score_1a[c(8, 17, 24, 32, 40, 47, 55, 63, 77, 80)], na.rm = TRUE)
      Score_Quasi_Psychotic<- sum(Score_1a[c(9, 18, 25, 33, 41, 48, 56)], na.rm = TRUE) 
      Change<- 0
      Change_Impulsivity<- 0
      Change_Affective_Instability<- 0
      Change_Abandonment<- 0
      Change_Relationships<- 0
      Change_Self_Image<- 0
      Change_Suicide_Mutilation<- 0
      Change_Emptiness<- 0
      Change_Anger<- 0
      Change_Quasi_Psychotic<- 0
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS<- (Rel * Score) + (M * (1 - Rel))
        PTS_Impulsivity<- (Rel_Impulsivity * Score_Impulsivity) + (M_Impulsivity * (1 - Rel_Impulsivity))
        PTS_Affective_Instability<- (Rel_Affective_Instability * Score_Affective_Instability) + (M_Affective_Instability * (1 - Rel_Affective_Instability))
        PTS_Abandonment<- (Rel_Abandonment * Score_Abandonment) + (M_Abandonment * (1 - Rel_Abandonment))
        PTS_Relationships<- (Rel_Relationships * Score_Relationships) + (M_Relationships * (1 - Rel_Relationships))
        PTS_Self_Image<- (Rel_Self_Image * Score_Self_Image) + (M_Self_Image * (1 - Rel_Self_Image))
        PTS_Suicide_Mutilation<- (Rel_Suicide_Mutilation * Score_Suicide_Mutilation) + (M_Suicide_Mutilation * (1 - Rel_Suicide_Mutilation))
        PTS_Emptiness<- (Rel_Emptiness * Score_Emptiness) + (M_Emptiness * (1 - Rel_Emptiness))
        PTS_Anger<- (Rel_Anger * Score_Anger) + (M_Anger * (1 - Rel_Anger))
        PTS_Quasi_Psychotic<- (Rel_Quasi_Psychotic * Score_Quasi_Psychotic) + (M_Quasi_Psychotic * (1 - Rel_Quasi_Psychotic))
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS<- Score + (M_Retest - M)  
        PTS_Impulsivity<- Score_Impulsivity + (M_Retest_Impulsivity - M_Impulsivity)  
        PTS_Affective_Instability<- Score_Affective_Instability + (M_Retest_Affective_Instability - M_Affective_Instability)  
        PTS_Abandonment<- Score_Abandonment + (M_Retest_Abandonment - M_Abandonment) 
        PTS_Relationships<- Score_Relationships + (M_Retest_Relationships - M_Relationships)
        PTS_Self_Image<- Score_Self_Image + (M_Retest_Self_Image - M_Self_Image)  
        PTS_Suicide_Mutilation<- Score_Suicide_Mutilation + (M_Retest_Suicide_Mutilation - M_Suicide_Mutilation)  
        PTS_Emptiness<- Score_Emptiness + (M_Retest_Emptiness - M_Emptiness) 
        PTS_Anger<- Score_Anger + (M_Retest_Anger - M_Anger) 
        PTS_Quasi_Psychotic<- Score_Quasi_Psychotic + (M_Retest_Quasi_Psychotic - M_Quasi_Psychotic) 
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Impulsivity<- Score_Impulsivity
        PTS_Affective_Instability<- Score_Affective_Instability
        PTS_Abandonment<- Score_Abandonment
        PTS_Relationships<- Score_Relationships
        PTS_Self_Image<- Score_Self_Image
        PTS_Suicide_Mutilation<- Score_Suicide_Mutilation
        PTS_Emptiness<- Score_Emptiness
        PTS_Anger<- Score_Anger
        PTS_Quasi_Psychotic<- Score_Quasi_Psychotic
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS<- (B_Adj * Score) + A_Adj
        B_Slope_Impulsivity<- Rel_Impulsivity * (SD_Retest_Impulsivity/SD_Impulsivity)
        A_Constant_Impulsivity<- M_Retest_Impulsivity - (B_Slope_Impulsivity * M_Impulsivity)
        B_Adj_Impulsivity<- SD_Retest_Impulsivity/SD_Impulsivity
        A_Adj_Impulsivity<- M_Retest_Impulsivity - (B_Adj_Impulsivity * M_Impulsivity)
        PTS_Impulsivity<- (B_Adj_Impulsivity * Score_Impulsivity) + A_Adj_Impulsivity
        B_Slope_Affective_Instability<- Rel_Affective_Instability * (SD_Retest_Affective_Instability/SD_Affective_Instability)
        A_Constant_Affective_Instability<- M_Retest_Affective_Instability - (B_Slope_Affective_Instability * M_Affective_Instability)
        B_Adj_Affective_Instability<- SD_Retest_Affective_Instability/SD_Affective_Instability
        A_Adj_Affective_Instability<- M_Retest_Affective_Instability - (B_Adj_Affective_Instability * M_Affective_Instability)
        PTS_Affective_Instability<- (B_Adj_Affective_Instability * Score_Affective_Instability) + A_Adj_Affective_Instability
        B_Slope_Abandonment<- Rel_Abandonment * (SD_Retest_Abandonment/SD_Abandonment)
        A_Constant_Abandonment<- M_Retest_Abandonment - (B_Slope_Abandonment * M_Abandonment)
        B_Adj_Abandonment<- SD_Retest_Abandonment/SD_Abandonment
        A_Adj_Abandonment<- M_Retest_Abandonment - (B_Adj_Abandonment * M_Abandonment)
        PTS_Abandonment<- (B_Adj_Abandonment * Score_Abandonment) + A_Adj_Abandonment
        B_Slope_Relationships<- Rel_Relationships * (SD_Retest_Relationships/SD_Relationships)
        A_Constant_Relationships<- M_Retest_Relationships - (B_Slope_Relationships * M_Relationships)
        B_Adj_Relationships<- SD_Retest_Relationships/SD_Relationships
        A_Adj_Relationships<- M_Retest_Relationships - (B_Adj_Relationships * M_Relationships)
        PTS_Relationships<- (B_Adj_Relationships * Score_Relationships) + A_Adj_Relationships
        B_Slope_Self_Image<- Rel_Self_Image * (SD_Retest_Self_Image/SD_Self_Image)
        A_Constant_Self_Image<- M_Retest_Self_Image - (B_Slope_Self_Image * M_Self_Image)
        B_Adj_Self_Image<- SD_Retest_Self_Image/SD_Self_Image
        A_Adj_Self_Image<- M_Retest_Self_Image - (B_Adj_Self_Image * M_Self_Image)
        PTS_Self_Image<- (B_Adj_Self_Image * Score_Self_Image) + A_Adj_Self_Image
        B_Slope_Suicide_Mutilation<- Rel_Suicide_Mutilation * (SD_Retest_Suicide_Mutilation/SD_Suicide_Mutilation)
        A_Constant_Suicide_Mutilation<- M_Retest_Suicide_Mutilation - (B_Slope_Suicide_Mutilation * M_Suicide_Mutilation)
        B_Adj_Suicide_Mutilation<- SD_Retest_Suicide_Mutilation/SD_Suicide_Mutilation
        A_Adj_Suicide_Mutilation<- M_Retest_Suicide_Mutilation - (B_Adj_Suicide_Mutilation * M_Suicide_Mutilation)
        PTS_Suicide_Mutilation<- (B_Adj_Suicide_Mutilation * Score_Suicide_Mutilation) + A_Adj_Suicide_Mutilation
        B_Slope_Emptiness<- Rel_Emptiness * (SD_Retest_Emptiness/SD_Emptiness)
        A_Constant_Emptiness<- M_Retest_Emptiness - (B_Slope_Emptiness * M_Emptiness)
        B_Adj_Emptiness<- SD_Retest_Emptiness/SD_Emptiness
        A_Adj_Emptiness<- M_Retest_Emptiness - (B_Adj_Emptiness * M_Emptiness)
        PTS_Emptiness<- (B_Adj_Emptiness * Score_Emptiness) + A_Adj_Emptiness
        B_Slope_Anger<- Rel_Anger * (SD_Retest_Anger/SD_Anger)
        A_Constant_Anger<- M_Retest_Anger - (B_Slope_Anger * M_Anger)
        B_Adj_Anger<- SD_Retest_Anger/SD_Anger
        A_Adj_Anger<- M_Retest_Anger - (B_Adj_Anger * M_Anger)
        PTS_Anger<- (B_Adj_Anger * Score_Anger) + A_Adj_Anger
        B_Slope_Quasi_Psychotic<- Rel_Quasi_Psychotic * (SD_Retest_Quasi_Psychotic/SD_Quasi_Psychotic)
        A_Constant_Quasi_Psychotic<- M_Retest_Quasi_Psychotic - (B_Slope_Quasi_Psychotic * M_Quasi_Psychotic)
        B_Adj_Quasi_Psychotic<- SD_Retest_Quasi_Psychotic/SD_Quasi_Psychotic
        A_Adj_Quasi_Psychotic<- M_Retest_Quasi_Psychotic - (B_Adj_Quasi_Psychotic * M_Quasi_Psychotic)
        PTS_Quasi_Psychotic<- (B_Adj_Quasi_Psychotic * Score_Quasi_Psychotic) + A_Adj_Quasi_Psychotic
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS<- B_Slope * Score
        B_Slope_Impulsivity<- Rel_Impulsivity * (SD_Retest_Impulsivity/SD_Impulsivity)
        PTS_Impulsivity<- B_Slope_Impulsivity * Score_Impulsivity
        B_Slope_Affective_Instability<- Rel_Affective_Instability * (SD_Retest_Affective_Instability/SD_Affective_Instability)
        PTS_Affective_Instability<- B_Slope_Affective_Instability * Score_Affective_Instability
        B_Slope_Abandonment<- Rel_Abandonment * (SD_Retest_Abandonment/SD_Abandonment)
        PTS_Abandonment<- B_Slope_Abandonment * Score_Abandonment
        B_Slope_Relationships<- Rel_Relationships * (SD_Retest_Relationships/SD_Relationships)
        PTS_Relationships<- B_Slope_Relationships * Score_Relationships
        B_Slope_Self_Image<- Rel_Self_Image * (SD_Retest_Self_Image/SD_Self_Image)
        PTS_Self_Image<- B_Slope_Self_Image * Score_Self_Image
        B_Slope_Suicide_Mutilation<- Rel_Suicide_Mutilation * (SD_Retest_Suicide_Mutilation/SD_Suicide_Mutilation)
        PTS_Suicide_Mutilation<- B_Slope_Suicide_Mutilation * Score_Suicide_Mutilation
        B_Slope_Emptiness<- Rel_Emptiness * (SD_Retest_Emptiness/SD_Emptiness)
        PTS_Emptiness<- B_Slope_Emptiness * Score_Emptiness
        B_Slope_Anger<- Rel_Anger * (SD_Retest_Anger/SD_Anger)
        PTS_Anger<- B_Slope_Anger * Score_Anger 
        B_Slope_Quasi_Psychotic<- Rel_Quasi_Psychotic * (SD_Retest_Quasi_Psychotic/SD_Quasi_Psychotic)
        PTS_Quasi_Psychotic<- B_Slope_Quasi_Psychotic * Score_Quasi_Psychotic
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS<- Score + (M_Retest - M)
        PTS_Impulsivity<- Score_Impulsivity + (M_Retest_Impulsivity - M_Impulsivity)
        PTS_Affective_Instability<- Score_Affective_Instability + (M_Retest_Affective_Instability - M_Affective_Instability)
        PTS_Abandonment<- Score_Abandonment + (M_Retest_Abandonment - M_Abandonment)
        PTS_Relationships<- Score_Relationships + (M_Retest_Relationships - M_Relationships)
        PTS_Self_Image<- Score_Self_Image + (M_Retest_Self_Image - M_Self_Image)
        PTS_Suicide_Mutilation<- Score_Suicide_Mutilation + (M_Retest_Suicide_Mutilation - M_Suicide_Mutilation)
        PTS_Emptiness<- Score_Emptiness + (M_Retest_Emptiness - M_Emptiness)
        PTS_Anger<- Score_Anger + (M_Retest_Anger - M_Anger)
        PTS_Quasi_Psychotic<- Score_Quasi_Psychotic + (M_Retest_Quasi_Psychotic - M_Quasi_Psychotic)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Impulsivity<- round(PTS_Impulsivity, digits = 2)
      PTS_Affective_Instability<- round(PTS_Affective_Instability, digits = 2)
      PTS_Abandonment<- round(PTS_Abandonment, digits = 2)
      PTS_Relationships<- round(PTS_Relationships, digits = 2)
      PTS_Self_Image<- round(PTS_Self_Image, digits = 2)
      PTS_Suicide_Mutilation<- round(PTS_Suicide_Mutilation, digits = 2)
      PTS_Emptiness<- round(PTS_Emptiness, digits = 2)
      PTS_Anger<- round(PTS_Anger, digits = 2)
      PTS_Quasi_Psychotic<- round(PTS_Quasi_Psychotic, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score - M)^2/(SD^2*(SampleN-1))))
        SE_Impulsivity<- McSweeny_SE_Impulsivity*sqrt(1 + (1/SampleN) + ((Score_Impulsivity - M_Impulsivity)^2/(SD_Impulsivity^2*(SampleN-1))))
        SE_Affective_Instability<- McSweeny_SE_Affective_Instability*sqrt(1 + (1/SampleN) + ((Score_Affective_Instability - M_Affective_Instability)^2/(SD_Affective_Instability^2*(SampleN-1))))
        SE_Abandonment<- McSweeny_SE_Abandonment*sqrt(1 + (1/SampleN) + ((Score_Abandonment - M_Abandonment)^2/(SD_Abandonment^2*(SampleN-1))))
        SE_Relationships<- McSweeny_SE_Relationships*sqrt(1 + (1/SampleN) + ((Score_Relationships - M_Relationships)^2/(SD_Relationships^2*(SampleN-1))))
        SE_Self_Image<- McSweeny_SE_Self_Image*sqrt(1 + (1/SampleN) + ((Score_Self_Image - M_Self_Image)^2/(SD_Self_Image^2*(SampleN-1))))
        SE_Suicide_Mutilation<- McSweeny_SE_Suicide_Mutilation*sqrt(1 + (1/SampleN) + ((Score_Suicide_Mutilation - M_Suicide_Mutilation)^2/(SD_Suicide_Mutilation^2*(SampleN-1))))
        SE_Emptiness<- McSweeny_SE_Emptiness*sqrt(1 + (1/SampleN) + ((Score_Emptiness - M_Emptiness)^2/(SD_Emptiness^2*(SampleN-1))))
        SE_Anger<- McSweeny_SE_Anger*sqrt(1 + (1/SampleN) + ((Score_Anger - M_Anger)^2/(SD_Anger^2*(SampleN-1))))
        SE_Quasi_Psychotic<- McSweeny_SE_Quasi_Psychotic*sqrt(1 + (1/SampleN) + ((Score_Quasi_Psychotic - M_Quasi_Psychotic)^2/(SD_Quasi_Psychotic^2*(SampleN-1))))
        SE<- round(SE, digits = 2)
        SE_Impulsivity<- round(SE_Impulsivity, digits = 2)
        SE_Affective_Instability<- round(SE_Affective_Instability, digits = 2)
        SE_Abandonment<- round(SE_Abandonment, digits = 2)
        SE_Relationships<- round(SE_Relationships, digits = 2)
        SE_Self_Image<- round(SE_Self_Image, digits = 2)
        SE_Suicide_Mutilation<- round(SE_Suicide_Mutilation, digits = 2)
        SE_Emptiness<- round(SE_Emptiness, digits = 2)
        SE_Anger<- round(SE_Anger, digits = 2)
        SE_Quasi_Psychotic<- round(SE_Quasi_Psychotic, digits = 2)
        CI<- (Conf*SE)
        CI<- round(CI, digits = 2)
        CI_Impulsivity<- (Conf*SE_Impulsivity)
        CI_Impulsivity<- round(CI_Impulsivity, digits = 2)
        CI_Affective_Instability<- (Conf*SE_Affective_Instability)
        CI_Affective_Instability<- round(CI_Affective_Instability, digits = 2)
        CI_Abandonment<- (Conf*SE_Abandonment)
        CI_Abandonment<- round(CI_Abandonment, digits = 2)
        CI_Relationships<- (Conf*SE_Relationships)
        CI_Relationships<- round(CI_Relationships, digits = 2)
        CI_Self_Image<- (Conf*SE_Self_Image)
        CI_Self_Image<- round(CI_Self_Image, digits = 2)
        CI_Suicide_Mutilation<- (Conf*SE_Suicide_Mutilation)
        CI_Suicide_Mutilation<- round(CI_Suicide_Mutilation, digits = 2)
        CI_Emptiness<- (Conf*SE_Emptiness)
        CI_Emptiness<- round(CI_Emptiness, digits = 2)
        CI_Anger<- (Conf*SE_Anger)
        CI_Anger<- round(CI_Anger, digits = 2)
        CI_Quasi_Psychotic<- (Conf*SE_Quasi_Psychotic)
        CI_Quasi_Psychotic<- round(CI_Quasi_Psychotic, digits = 2)
      }
      CI<- (Conf*SE)
      CI<- round(CI, digits = 2)
      CI<- round(CI, digits = 2)
      CI_Impulsivity<- (Conf*SE_Impulsivity)
      CI_Impulsivity<- round(CI_Impulsivity, digits = 2)
      CI_Affective_Instability<- (Conf*SE_Affective_Instability)
      CI_Affective_Instability<- round(CI_Affective_Instability, digits = 2)
      CI_Abandonment<- (Conf*SE_Abandonment)
      CI_Abandonment<- round(CI_Abandonment, digits = 2)
      CI_Relationships<- (Conf*SE_Relationships)
      CI_Relationships<- round(CI_Relationships, digits = 2)
      CI_Self_Image<- (Conf*SE_Self_Image)
      CI_Self_Image<- round(CI_Self_Image, digits = 2)
      CI_Suicide_Mutilation<- (Conf*SE_Suicide_Mutilation)
      CI_Suicide_Mutilation<- round(CI_Suicide_Mutilation, digits = 2)
      CI_Emptiness<- (Conf*SE_Emptiness)
      CI_Emptiness<- round(CI_Emptiness, digits = 2)
      CI_Anger<- (Conf*SE_Anger)
      CI_Anger<- round(CI_Anger, digits = 2)
      CI_Quasi_Psychotic<- (Conf*SE_Quasi_Psychotic)
      CI_Quasi_Psychotic<- round(CI_Quasi_Psychotic, digits = 2)
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Impulsivity<- PTS_Impulsivity + CI_Impulsivity
      CI_Upper_Lim_Impulsivity<- round(CI_Upper_Lim_Impulsivity, digits = 2)
      CI_Lower_Lim_Impulsivity<-PTS_Impulsivity - CI_Impulsivity
      CI_Lower_Lim_Impulsivity<- round(CI_Lower_Lim_Impulsivity, digits = 2)
      CI_Upper_Lim_Affective_Instability<- PTS_Affective_Instability + CI_Affective_Instability
      CI_Upper_Lim_Affective_Instability<- round(CI_Upper_Lim_Affective_Instability, digits = 2)
      CI_Lower_Lim_Affective_Instability<-PTS_Affective_Instability - CI_Affective_Instability
      CI_Lower_Lim_Affective_Instability<- round(CI_Lower_Lim_Affective_Instability, digits = 2)
      CI_Upper_Lim_Abandonment<- PTS_Abandonment + CI_Abandonment
      CI_Upper_Lim_Abandonment<- round(CI_Upper_Lim_Abandonment, digits = 2)
      CI_Lower_Lim_Abandonment<-PTS_Abandonment - CI_Abandonment
      CI_Lower_Lim_Abandonment<- round(CI_Lower_Lim_Abandonment, digits = 2)
      CI_Upper_Lim_Relationships<- PTS_Relationships + CI_Relationships
      CI_Upper_Lim_Relationships<- round(CI_Upper_Lim_Relationships, digits = 2)
      CI_Lower_Lim_Relationships<-PTS_Relationships - CI_Relationships
      CI_Lower_Lim_Relationships<- round(CI_Lower_Lim_Relationships, digits = 2)
      CI_Upper_Lim_Self_Image<- PTS_Self_Image + CI_Self_Image
      CI_Upper_Lim_Self_Image<- round(CI_Upper_Lim_Self_Image, digits = 2)
      CI_Lower_Lim_Self_Image<-PTS_Self_Image - CI_Self_Image
      CI_Lower_Lim_Self_Image<- round(CI_Lower_Lim_Self_Image, digits = 2)
      CI_Upper_Lim_Suicide_Mutilation<- PTS_Suicide_Mutilation + CI_Suicide_Mutilation
      CI_Upper_Lim_Suicide_Mutilation<- round(CI_Upper_Lim_Suicide_Mutilation, digits = 2)
      CI_Lower_Lim_Suicide_Mutilation<- PTS_Suicide_Mutilation - CI_Suicide_Mutilation
      CI_Lower_Lim_Suicide_Mutilation<- round(CI_Lower_Lim_Suicide_Mutilation, digits = 2)
      CI_Upper_Lim_Emptiness<- PTS_Emptiness + CI_Emptiness
      CI_Upper_Lim_Emptiness<- round(CI_Upper_Lim_Emptiness, digits = 2)
      CI_Lower_Lim_Emptiness<-PTS_Emptiness - CI_Emptiness
      CI_Lower_Lim_Emptiness<- round(CI_Lower_Lim_Emptiness, digits = 2)
      CI_Upper_Lim_Anger<- PTS_Anger + CI_Anger
      CI_Upper_Lim_Anger<- round(CI_Upper_Lim_Anger, digits = 2)
      CI_Lower_Lim_Anger<-PTS_Anger - CI_Anger
      CI_Lower_Lim_Anger<- round(CI_Lower_Lim_Anger, digits = 2)
      CI_Upper_Lim_Quasi_Psychotic<- PTS_Quasi_Psychotic + CI_Quasi_Psychotic
      CI_Upper_Lim_Quasi_Psychotic<- round(CI_Upper_Lim_Quasi_Psychotic, digits = 2)
      CI_Lower_Lim_Quasi_Psychotic<-PTS_Quasi_Psychotic - CI_Quasi_Psychotic
      CI_Lower_Lim_Quasi_Psychotic<- round(CI_Lower_Lim_Quasi_Psychotic, digits = 2)
      
      if(input$Select_CI == "2") {
        CI<- input$Man_CI
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Impulsivity == "2") {
        CI_Impulsivity<- input$Man_CI_Impulsivity
        CI_Impulsivity<- round(CI_Impulsivity, digits = 2)
        CI_Upper_Lim_Impulsivity<- Score_Impulsivity + CI_Impulsivity
        CI_Upper_Lim_Impulsivity<- round(CI_Upper_Lim_Impulsivity, digits = 2)
        CI_Lower_Lim_Impulsivity<- Score_Impulsivity - CI_Impulsivity
        CI_Lower_Lim_Impulsivity<- round(CI_Lower_Lim_Impulsivity, digits = 2)
      }
      if(input$Select_CI_Affective_Instability == "2") {
        CI_Affective_Instability<- input$Man_CI_Affective_Instability
        CI_Affective_Instability<- round(CI_Affective_Instability, digits = 2)
        CI_Upper_Lim_Affective_Instability<- Score_Affective_Instability + CI_Affective_Instability
        CI_Upper_Lim_Affective_Instability<- round(CI_Upper_Lim_Affective_Instability, digits = 2)
        CI_Lower_Lim_Affective_Instability<- Score_Affective_Instability - CI_Affective_Instability
        CI_Lower_Lim_Affective_Instability<- round(CI_Lower_Lim_Affective_Instability, digits = 2)
      }
      if(input$Select_CI_Abandonment == "2") {
        CI_Abandonment<- input$Man_CI_Abandonment
        CI_Abandonment<- round(CI_Abandonment, digits = 2)
        CI_Upper_Lim_Abandonment<- Score_Abandonment + CI_Abandonment
        CI_Upper_Lim_Abandonment<- round(CI_Upper_Lim_Abandonment, digits = 2)
        CI_Lower_Lim_Abandonment<- Score_Abandonment - CI_Abandonment
        CI_Lower_Lim_Abandonment<- round(CI_Lower_Lim_Abandonment, digits = 2)
      }
      if(input$Select_CI_Relationships == "2") {
        CI_Relationships<- input$Man_CI_Relationships
        CI_Relationships<- round(CI_Relationships, digits = 2)
        CI_Upper_Lim_Relationships<- Score_Relationships + CI_Relationships
        CI_Upper_Lim_Relationships<- round(CI_Upper_Lim_Relationships, digits = 2)
        CI_Lower_Lim_Relationships<- Score_Relationships - CI_Relationships
        CI_Lower_Lim_Relationships<- round(CI_Lower_Lim_Relationships, digits = 2)
      }
      if(input$Select_CI_Self_Image == "2") {
        CI_Self_Image<- input$Man_CI_Self_Image
        CI_Self_Image<- round(CI_Self_Image, digits = 2)
        CI_Upper_Lim_Self_Image<- Score_Self_Image + CI_Self_Image
        CI_Upper_Lim_Self_Image<- round(CI_Upper_Lim_Self_Image, digits = 2)
        CI_Lower_Lim_Self_Image<- Score_Self_Image - CI_Self_Image
        CI_Lower_Lim_Self_Image<- round(CI_Lower_Lim_Self_Image, digits = 2)
      }
      if(input$Select_CI_Suicide_Mutilation == "2") {
        CI_Suicide_Mutilation<- input$Man_CI_Suicide_Mutilation
        CI_Suicide_Mutilation<- round(CI_Suicide_Mutilation, digits = 2)
        CI_Upper_Lim_Suicide_Mutilation<- Score_Suicide_Mutilation + CI_Suicide_Mutilation
        CI_Upper_Lim_Suicide_Mutilation<- round(CI_Upper_Lim_Suicide_Mutilation, digits = 2)
        CI_Lower_Lim_Suicide_Mutilation<- Score_Suicide_Mutilation - CI_Suicide_Mutilation
        CI_Lower_Lim_Suicide_Mutilation<- round(CI_Lower_Lim_Suicide_Mutilation, digits = 2)
      }
      if(input$Select_CI_Emptiness == "2") {
        CI_Emptiness<- input$Man_CI_Emptiness
        CI_Emptiness<- round(CI_Emptiness, digits = 2)
        CI_Upper_Lim_Emptiness<- Score_Emptiness + CI_Emptiness
        CI_Upper_Lim_Emptiness<- round(CI_Upper_Lim_Emptiness, digits = 2)
        CI_Lower_Lim_Emptiness<- Score_Emptiness - CI_Emptiness
        CI_Lower_Lim_Emptiness<- round(CI_Lower_Lim_Emptiness, digits = 2)
      }
      if(input$Select_CI_Anger == "2") {
        CI_Anger<- input$Man_CI_Anger
        CI_Anger<- round(CI_Anger, digits = 2)
        CI_Upper_Lim_Anger<- Score_Anger + CI_Anger
        CI_Upper_Lim_Anger<- round(CI_Upper_Lim_Anger, digits = 2)
        CI_Lower_Lim_Anger<- Score_Anger - CI_Anger
        CI_Lower_Lim_Anger<- round(CI_Lower_Lim_Anger, digits = 2)
      }
      if(input$Select_CI_Quasi_Psychotic == "2") {
        CI_Quasi_Psychotic<- input$Man_CI_Quasi_Psychotic
        CI_Quasi_Psychotic<- round(CI_Quasi_Psychotic, digits = 2)
        CI_Upper_Lim_Quasi_Psychotic<- Score_Quasi_Psychotic + CI_Quasi_Psychotic
        CI_Upper_Lim_Quasi_Psychotic<- round(CI_Upper_Lim_Quasi_Psychotic, digits = 2)
        CI_Lower_Lim_Quasi_Psychotic<- Score_Quasi_Psychotic - CI_Quasi_Psychotic
        CI_Lower_Lim_Quasi_Psychotic<- round(CI_Lower_Lim_Quasi_Psychotic, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Impulsivity_1<- round(input$Cutoff_Impulsivity_1, digits = 2)
      Cutoff_Score_Impulsivity_2<- round(input$Cutoff_Impulsivity_2, digits = 2)
      Cutoff_Score_Impulsivity_3<- round(input$Cutoff_Impulsivity_3, digits = 2)
      Cutoff_Score_Affective_Instability_1<- round(input$Cutoff_Affective_Instability_1, digits = 2)
      Cutoff_Score_Affective_Instability_2<- round(input$Cutoff_Affective_Instability_2, digits = 2)
      Cutoff_Score_Affective_Instability_3<- round(input$Cutoff_Affective_Instability_3, digits = 2)
      Cutoff_Score_Abandonment_1<- round(input$Cutoff_Abandonment_1, digits = 2)
      Cutoff_Score_Abandonment_2<- round(input$Cutoff_Abandonment_2, digits = 2)
      Cutoff_Score_Abandonment_3<- round(input$Cutoff_Abandonment_3, digits = 2)
      Cutoff_Score_Relationships_1<- round(input$Cutoff_Relationships_1, digits = 2)
      Cutoff_Score_Relationships_2<- round(input$Cutoff_Relationships_2, digits = 2)
      Cutoff_Score_Relationships_3<- round(input$Cutoff_Relationships_3, digits = 2)
      Cutoff_Score_Self_Image_1<- round(input$Cutoff_Self_Image_1, digits = 2)
      Cutoff_Score_Self_Image_2<- round(input$Cutoff_Self_Image_2, digits = 2)
      Cutoff_Score_Self_Image_3<- round(input$Cutoff_Self_Image_3, digits = 2)
      Cutoff_Score_Suicide_Mutilation_1<- round(input$Cutoff_Suicide_Mutilation_1, digits = 2)
      Cutoff_Score_Suicide_Mutilation_2<- round(input$Cutoff_Suicide_Mutilation_2, digits = 2)
      Cutoff_Score_Suicide_Mutilation_3<- round(input$Cutoff_Suicide_Mutilation_3, digits = 2)
      Cutoff_Score_Emptiness_1<- round(input$Cutoff_Emptiness_1, digits = 2)
      Cutoff_Score_Emptiness_2<- round(input$Cutoff_Emptiness_2, digits = 2)
      Cutoff_Score_Emptiness_3<- round(input$Cutoff_Emptiness_3, digits = 2)
      Cutoff_Score_Anger_1<- round(input$Cutoff_Anger_1, digits = 2)
      Cutoff_Score_Anger_2<- round(input$Cutoff_Anger_2, digits = 2)
      Cutoff_Score_Anger_3<- round(input$Cutoff_Anger_3, digits = 2)
      Cutoff_Score_Quasi_Psychotic_1<- round(input$Cutoff_Quasi_Psychotic_1, digits = 2)
      Cutoff_Score_Quasi_Psychotic_2<- round(input$Cutoff_Quasi_Psychotic_2, digits = 2)
      Cutoff_Score_Quasi_Psychotic_3<- round(input$Cutoff_Quasi_Psychotic_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Impulsivity,Change_Impulsivity,PTS_Impulsivity, SE_Impulsivity, CI_Upper_Lim_Impulsivity, CI_Lower_Lim_Impulsivity, Cutoff_Score_Impulsivity_1,Cutoff_Score_Impulsivity_2,Cutoff_Score_Impulsivity_3,
                                      Score_Affective_Instability,Change_Affective_Instability, PTS_Affective_Instability, SE_Affective_Instability, CI_Upper_Lim_Affective_Instability, CI_Lower_Lim_Affective_Instability, Cutoff_Score_Affective_Instability_1,Cutoff_Score_Affective_Instability_2,Cutoff_Score_Affective_Instability_3, 
                                      Score_Abandonment,Change_Abandonment,PTS_Abandonment, SE_Abandonment, CI_Upper_Lim_Abandonment, CI_Lower_Lim_Abandonment, Cutoff_Score_Abandonment_1,Cutoff_Score_Abandonment_2,Cutoff_Score_Abandonment_3, 
                                      Score_Relationships,Change_Relationships,PTS_Relationships, SE_Relationships, CI_Upper_Lim_Relationships, CI_Lower_Lim_Relationships, Cutoff_Score_Relationships_1,Cutoff_Score_Relationships_2,Cutoff_Score_Relationships_3, 
                                      Score_Self_Image,Change_Self_Image,PTS_Self_Image, SE_Self_Image, CI_Upper_Lim_Self_Image, CI_Lower_Lim_Self_Image, Cutoff_Score_Self_Image_1,Cutoff_Score_Self_Image_2,Cutoff_Score_Self_Image_3,
                                      Score_Suicide_Mutilation,Change_Suicide_Mutilation, PTS_Suicide_Mutilation, SE_Suicide_Mutilation, CI_Upper_Lim_Suicide_Mutilation, CI_Lower_Lim_Suicide_Mutilation, Cutoff_Score_Suicide_Mutilation_1,Cutoff_Score_Suicide_Mutilation_2,Cutoff_Score_Suicide_Mutilation_3, 
                                      Score_Emptiness,Change_Emptiness,PTS_Emptiness, SE_Emptiness, CI_Upper_Lim_Emptiness, CI_Lower_Lim_Emptiness, Cutoff_Score_Emptiness_1,Cutoff_Score_Emptiness_2,Cutoff_Score_Emptiness_3, 
                                      Score_Anger,Change_Anger,PTS_Anger, SE_Anger, CI_Upper_Lim_Anger, CI_Lower_Lim_Anger, Cutoff_Score_Anger_1,Cutoff_Score_Anger_2,Cutoff_Score_Anger_3, 
                                      Score_Quasi_Psychotic,Change_Quasi_Psychotic,PTS_Quasi_Psychotic, SE_Quasi_Psychotic, CI_Upper_Lim_Quasi_Psychotic, CI_Lower_Lim_Quasi_Psychotic, Cutoff_Score_Quasi_Psychotic_1,Cutoff_Score_Quasi_Psychotic_2,Cutoff_Score_Quasi_Psychotic_3)
    } else if(input$Timepoint == "2") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date<- c(Date_1, Date_2)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Recode_1<- car::recode(Score_1a[c(10,43,28,4,45,60,52,67,53,54,8,32,48)],'0=1; 1=0')
      Score_1a[c(10,43,28,4,45,60,52,67,53,54,8,32,48)]<- Recode_1
      Score_1<- sum(Score_1a, na.rm = TRUE)
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_2a), Score = Score_2a)
      Recode_2<- car::recode(Score_2a[c(10,43,28,4,45,60,52,67,53,54,8,32,48)],'0=1; 1=0')
      Score_2a[c(10,43,28,4,45,60,52,67,53,54,8,32,48)]<- Recode_2
      Score_2<- sum(Score_2a, na.rm = TRUE)
      Score<- c(Score_1, Score_2)
      Score<- round(Score, digits = 2)
      Score_Impulsivity_1<- sum(Score_1a[c(1,10,26,34,42,57,64,68,71)], na.rm = TRUE)
      Score_Impulsivity_2<- sum(Score_2a[c(1,10,26,34,42,57,64,68,71)], na.rm = TRUE)
      Score_Impulsivity<- c(Score_Impulsivity_1, Score_Impulsivity_2)
      Score_Impulsivity<- round(Score_Impulsivity, digits = 2)
      Score_Affective_Instability_1<- sum(Score_1a[c(2, 11, 19, 27, 35, 43, 49, 58, 65, 72)], na.rm = TRUE)
      Score_Affective_Instability_2<- sum(Score_2a[c(2, 11, 19, 27, 35, 43, 49, 58, 65, 72)], na.rm = TRUE)
      Score_Affective_Instability<- c(Score_Affective_Instability_1,Score_Affective_Instability_2)
      Score_Affective_Instability<- round(Score_Affective_Instability, digits = 2)
      Score_Abandonment_1<- sum(Score_1a[c(3, 12, 20, 28, 44, 50, 59, 66, 73, 78)], na.rm = TRUE)
      Score_Abandonment_2<- sum(Score_2a[c(3, 12, 20, 28, 44, 50, 59, 66, 73, 78)], na.rm = TRUE)
      Score_Abandonment<- c(Score_Abandonment_1, Score_Abandonment_2)
      Score_Abandonment<- round(Score_Abandonment, digits = 2)
      Score_Relationships_1<- sum(Score_1a[c(4, 13, 21, 29, 36, 45, 51, 60)], na.rm = TRUE)
      Score_Relationships_2<- sum(Score_2a[c(4, 13, 21, 29, 36, 45, 51, 60)], na.rm = TRUE)
      Score_Relationships<- c(Score_Relationships_1, Score_Relationships_2)
      Score_Relationships<- round(Score_Relationships, digits = 2)
      Score_Self_Image_1<- sum(Score_1a[c(5, 14, 37, 46, 52, 61, 67, 70, 74)], na.rm = TRUE)
      Score_Self_Image_2<- sum(Score_2a[c(5, 14, 37, 46, 52, 61, 67, 70, 74)], na.rm = TRUE)
      Score_Self_Image<- c(Score_Self_Image_1, Score_Self_Image_2)
      Score_Self_Image<- round(Score_Self_Image, digits = 2)
      Score_Suicide_Mutilation_1<- sum(Score_1a[c(6, 15, 22, 30, 38, 53, 75)], na.rm = TRUE)
      Score_Suicide_Mutilation_2<- sum(Score_2a[c(6, 15, 22, 30, 38, 53, 75)], na.rm = TRUE)
      Score_Suicide_Mutilation<- c(Score_Suicide_Mutilation_1, Score_Suicide_Mutilation_2)
      Score_Suicide_Mutilation<- round(Score_Suicide_Mutilation, digits = 2)
      Score_Emptiness_1<- sum(Score_1a[c(7, 16, 23, 31, 39, 54, 62, 69, 76, 79)], na.rm = TRUE)
      Score_Emptiness_2<- sum(Score_2a[c(7, 16, 23, 31, 39, 54, 62, 69, 76, 79)], na.rm = TRUE)
      Score_Emptiness<- c(Score_Emptiness_1, Score_Emptiness_2)
      Score_Emptiness<- round(Score_Emptiness, digits = 2)
      Score_Anger_1<- sum(Score_1a[c(8, 17, 24, 32, 40, 47, 55, 63, 77, 80)], na.rm = TRUE)
      Score_Anger_2<- sum(Score_2a[c(8, 17, 24, 32, 40, 47, 55, 63, 77, 80)], na.rm = TRUE)
      Score_Anger<- c(Score_Anger_1, Score_Anger_2)
      Score_Anger<- round(Score_Anger, digits = 2)
      Score_Quasi_Psychotic_1<- sum(Score_1a[c(9, 18, 25, 33, 41, 48, 56)], na.rm = TRUE)
      Score_Quasi_Psychotic_2<- sum(Score_2a[c(9, 18, 25, 33, 41, 48, 56)], na.rm = TRUE)
      Score_Quasi_Psychotic<- c(Score_Quasi_Psychotic_1, Score_Quasi_Psychotic_2)
      Score_Quasi_Psychotic<- round(Score_Quasi_Psychotic, digits = 2)
      Change<- c(0, (Score_2 - Score_1))
      Change<- round(Change, digits = 2)
      Change_Impulsivity<- c(0, (Score_Impulsivity_2 - Score_Impulsivity_1))
      Change_Impulsivity<- round(Change_Impulsivity, digits = 2)
      Change_Affective_Instability<- c(0, (Score_Affective_Instability_2 - Score_Affective_Instability_1))
      Change_Affective_Instability<- round(Change_Affective_Instability, digits = 2)
      Change_Abandonment<- c(0, (Score_Abandonment_2 - Score_Abandonment_1))
      Change_Abandonment<- round(Change_Abandonment, digits = 2)
      Change_Relationships<- c(0, (Score_Relationships_2 - Score_Relationships_1))
      Change_Relationships<- round(Change_Relationships, digits = 2)
      Change_Self_Image<- c(0, (Score_Self_Image_2 - Score_Self_Image_1))
      Change_Self_Image<- round(Change_Self_Image, digits = 2)
      Change_Suicide_Mutilation<- c(0, (Score_Suicide_Mutilation_2 - Score_Suicide_Mutilation_1))
      Change_Suicide_Mutilation<- round(Change_Suicide_Mutilation, digits = 2)
      Change_Emptiness<- c(0, (Score_Emptiness_2 - Score_Emptiness_1))
      Change_Emptiness<- round(Change_Emptiness, digits = 2)
      Change_Anger<- c(0, (Score_Anger_2 - Score_Anger_1))
      Change_Anger<- round(Change_Anger, digits = 2)
      Change_Quasi_Psychotic<- c(0, (Score_Quasi_Psychotic_2 - Score_Quasi_Psychotic_1))
      Change_Quasi_Psychotic<- round(Change_Quasi_Psychotic, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2)
        PTS_Impulsivity_1<- (Rel_Impulsivity * Score_Impulsivity_1) + (M_Impulsivity * (1 - Rel_Impulsivity))
        PTS_Impulsivity_2<- (Rel_Impulsivity * Score_Impulsivity_2) + (M_Impulsivity * (1 - Rel_Impulsivity))
        PTS_Impulsivity<- c(PTS_Impulsivity_1, PTS_Impulsivity_2)
        PTS_Affective_Instability_1<- (Rel_Affective_Instability * Score_Affective_Instability_1) + (M_Affective_Instability * (1 - Rel_Affective_Instability))
        PTS_Affective_Instability_2<- (Rel_Affective_Instability * Score_Affective_Instability_2) + (M_Affective_Instability * (1 - Rel_Affective_Instability))
        PTS_Affective_Instability<- c(PTS_Affective_Instability_1, PTS_Affective_Instability_2)
        PTS_Abandonment_1<- (Rel_Abandonment * Score_Abandonment_1) + (M_Abandonment * (1 - Rel_Abandonment))
        PTS_Abandonment_2<- (Rel_Abandonment * Score_Abandonment_2) + (M_Abandonment * (1 - Rel_Abandonment))
        PTS_Abandonment<- c(PTS_Abandonment_1, PTS_Abandonment_2)
        PTS_Relationships_1<- (Rel_Relationships * Score_Relationships_1) + (M_Relationships * (1 - Rel_Relationships))
        PTS_Relationships_2<- (Rel_Relationships * Score_Relationships_2) + (M_Relationships * (1 - Rel_Relationships))
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2)
        PTS_Self_Image_1<- (Rel_Self_Image * Score_Self_Image_1) + (M_Self_Image * (1 - Rel_Self_Image))
        PTS_Self_Image_2<- (Rel_Self_Image * Score_Self_Image_2) + (M_Self_Image * (1 - Rel_Self_Image))
        PTS_Self_Image<- c(PTS_Self_Image_1, PTS_Self_Image_2)
        PTS_Suicide_Mutilation_1<- (Rel_Suicide_Mutilation * Score_Suicide_Mutilation_1) + (M_Suicide_Mutilation * (1 - Rel_Suicide_Mutilation))
        PTS_Suicide_Mutilation_2<- (Rel_Suicide_Mutilation * Score_Suicide_Mutilation_2) + (M_Suicide_Mutilation * (1 - Rel_Suicide_Mutilation))
        PTS_Suicide_Mutilation<- c(PTS_Suicide_Mutilation_1, PTS_Suicide_Mutilation_2)
        PTS_Emptiness_1<- (Rel_Emptiness * Score_Emptiness_1) + (M_Emptiness * (1 - Rel_Emptiness))
        PTS_Emptiness_2<- (Rel_Emptiness * Score_Emptiness_2) + (M_Emptiness * (1 - Rel_Emptiness))
        PTS_Emptiness<- c(PTS_Emptiness_1, PTS_Emptiness_2)
        PTS_Anger_1<- (Rel_Anger * Score_Anger_1) + (M_Anger * (1 - Rel_Anger))
        PTS_Anger_2<- (Rel_Anger * Score_Anger_2) + (M_Anger * (1 - Rel_Anger))
        PTS_Anger<- c(PTS_Anger_1, PTS_Anger_2)
        PTS_Quasi_Psychotic_1<- (Rel_Quasi_Psychotic * Score_Quasi_Psychotic_1) + (M_Quasi_Psychotic * (1 - Rel_Quasi_Psychotic))
        PTS_Quasi_Psychotic_2<- (Rel_Quasi_Psychotic * Score_Quasi_Psychotic_2) + (M_Quasi_Psychotic * (1 - Rel_Quasi_Psychotic))
        PTS_Quasi_Psychotic<- c(PTS_Quasi_Psychotic_1, PTS_Quasi_Psychotic_2)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2)
        PTS_Impulsivity_1<- Score_Impulsivity_1 + (M_Retest_Impulsivity - M_Impulsivity)  
        PTS_Impulsivity_2<- Score_Impulsivity_2 + (M_Retest_Impulsivity - M_Impulsivity) 
        PTS_Impulsivity<- c(PTS_Impulsivity_1, PTS_Impulsivity_2)
        PTS_Affective_Instability_1<- Score_Affective_Instability_1 + (M_Retest_Affective_Instability - M_Affective_Instability)  
        PTS_Affective_Instability_2<- Score_Affective_Instability_2 + (M_Retest_Affective_Instability - M_Affective_Instability) 
        PTS_Affective_Instability<- c(PTS_Affective_Instability_1, PTS_Affective_Instability_2)
        PTS_Abandonment_1<- Score_Abandonment_1 + (M_Retest_Abandonment - M_Abandonment)  
        PTS_Abandonment_2<- Score_Abandonment_2 + (M_Retest_Abandonment - M_Abandonment) 
        PTS_Abandonment<- c(PTS_Abandonment_1, PTS_Abandonment_2)
        PTS_Relationships_1<- Score_Relationships_1 + (M_Retest_Relationships - M_Relationships)  
        PTS_Relationships_2<- Score_Relationships_2 + (M_Retest_Relationships - M_Relationships) 
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2)
        PTS_Self_Image_1<- Score_Self_Image_1 + (M_Retest_Self_Image - M_Self_Image)  
        PTS_Self_Image_2<- Score_Self_Image_2 + (M_Retest_Self_Image - M_Self_Image) 
        PTS_Self_Image<- c(PTS_Self_Image_1, PTS_Self_Image_2)
        PTS_Suicide_Mutilation_1<- Score_Suicide_Mutilation_1 + (M_Retest_Suicide_Mutilation - M_Suicide_Mutilation)  
        PTS_Suicide_Mutilation_2<- Score_Suicide_Mutilation_2 + (M_Retest_Suicide_Mutilation - M_Suicide_Mutilation) 
        PTS_Suicide_Mutilation<- c(PTS_Suicide_Mutilation_1, PTS_Suicide_Mutilation_2)
        PTS_Emptiness_1<- Score_Emptiness_1 + (M_Retest_Emptiness - M_Emptiness)  
        PTS_Emptiness_2<- Score_Emptiness_2 + (M_Retest_Emptiness - M_Emptiness) 
        PTS_Emptiness<- c(PTS_Emptiness_1, PTS_Emptiness_2)
        PTS_Anger_1<- Score_Anger_1 + (M_Retest_Anger - M_Anger)  
        PTS_Anger_2<- Score_Anger_2 + (M_Retest_Anger - M_Anger) 
        PTS_Anger<- c(PTS_Anger_1, PTS_Anger_2)
        PTS_Quasi_Psychotic_1<- Score_Quasi_Psychotic_1 + (M_Retest_Quasi_Psychotic - M_Quasi_Psychotic)  
        PTS_Quasi_Psychotic_2<- Score_Quasi_Psychotic_2 + (M_Retest_Quasi_Psychotic - M_Quasi_Psychotic) 
        PTS_Quasi_Psychotic<- c(PTS_Quasi_Psychotic_1, PTS_Quasi_Psychotic_2)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Impulsivity<- Score_Impulsivity
        PTS_Affective_Instability<- Score_Affective_Instability
        PTS_Abandonment<- Score_Abandonment
        PTS_Relationships<- Score_Relationships
        PTS_Self_Image<- Score_Self_Image
        PTS_Suicide_Mutilation<- Score_Suicide_Mutilation
        PTS_Emptiness<- Score_Emptiness
        PTS_Anger<- Score_Anger
        PTS_Quasi_Psychotic<- Score_Quasi_Psychotic
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Impulsivity<- Rel_Impulsivity * (SD_Retest_Impulsivity/SD_Impulsivity)
        A_Constant_Impulsivity<- M_Retest_Impulsivity - (B_Slope_Impulsivity * M_Impulsivity)
        B_Adj_Impulsivity<- SD_Retest_Impulsivity/SD_Impulsivity
        A_Adj_Impulsivity<- M_Retest_Impulsivity - (B_Adj_Impulsivity * M_Impulsivity)
        PTS_Impulsivity_1<- (B_Adj_Impulsivity * Score_Impulsivity_1) + A_Adj_Impulsivity
        PTS_Impulsivity_2<- (B_Adj_Impulsivity * Score_Impulsivity_2) + A_Adj_Impulsivity
        PTS_Impulsivity<- c(PTS_Impulsivity_1,PTS_Impulsivity_2)
        B_Slope_Affective_Instability<- Rel_Affective_Instability * (SD_Retest_Affective_Instability/SD_Affective_Instability)
        A_Constant_Affective_Instability<- M_Retest_Affective_Instability - (B_Slope_Affective_Instability * M_Affective_Instability)
        B_Adj_Affective_Instability<- SD_Retest_Affective_Instability/SD_Affective_Instability
        A_Adj_Affective_Instability<- M_Retest_Affective_Instability - (B_Adj_Affective_Instability * M_Affective_Instability)
        PTS_Affective_Instability_1<- (B_Adj_Affective_Instability * Score_Affective_Instability_1) + A_Adj_Affective_Instability
        PTS_Affective_Instability_2<- (B_Adj_Affective_Instability * Score_Affective_Instability_2) + A_Adj_Affective_Instability
        PTS_Affective_Instability<- c(PTS_Affective_Instability_1,PTS_Affective_Instability_2)
        B_Slope_Abandonment<- Rel_Abandonment * (SD_Retest_Abandonment/SD_Abandonment)
        A_Constant_Abandonment<- M_Retest_Abandonment - (B_Slope_Abandonment * M_Abandonment)
        B_Adj_Abandonment<- SD_Retest_Abandonment/SD_Abandonment
        A_Adj_Abandonment<- M_Retest_Abandonment - (B_Adj_Abandonment * M_Abandonment)
        PTS_Abandonment_1<- (B_Adj_Abandonment * Score_Abandonment_1) + A_Adj_Abandonment
        PTS_Abandonment_2<- (B_Adj_Abandonment * Score_Abandonment_2) + A_Adj_Abandonment
        PTS_Abandonment<- c(PTS_Abandonment_1,PTS_Abandonment_2)
        B_Slope_Relationships<- Rel_Relationships * (SD_Retest_Relationships/SD_Relationships)
        A_Constant_Relationships<- M_Retest_Relationships - (B_Slope_Relationships * M_Relationships)
        B_Adj_Relationships<- SD_Retest_Relationships/SD_Relationships
        A_Adj_Relationships<- M_Retest_Relationships - (B_Adj_Relationships * M_Relationships)
        PTS_Relationships_1<- (B_Adj_Relationships * Score_Relationships_1) + A_Adj_Relationships
        PTS_Relationships_2<- (B_Adj_Relationships * Score_Relationships_2) + A_Adj_Relationships
        PTS_Relationships<- c(PTS_Relationships_1,PTS_Relationships_2)
        B_Slope_Self_Image<- Rel_Self_Image * (SD_Retest_Self_Image/SD_Self_Image)
        A_Constant_Self_Image<- M_Retest_Self_Image - (B_Slope_Self_Image * M_Self_Image)
        B_Adj_Self_Image<- SD_Retest_Self_Image/SD_Self_Image
        A_Adj_Self_Image<- M_Retest_Self_Image - (B_Adj_Self_Image * M_Self_Image)
        PTS_Self_Image_1<- (B_Adj_Self_Image * Score_Self_Image_1) + A_Adj_Self_Image
        PTS_Self_Image_2<- (B_Adj_Self_Image * Score_Self_Image_2) + A_Adj_Self_Image
        PTS_Self_Image<- c(PTS_Self_Image_1,PTS_Self_Image_2)
        B_Slope_Suicide_Mutilation<- Rel_Suicide_Mutilation * (SD_Retest_Suicide_Mutilation/SD_Suicide_Mutilation)
        A_Constant_Suicide_Mutilation<- M_Retest_Suicide_Mutilation - (B_Slope_Suicide_Mutilation * M_Suicide_Mutilation)
        B_Adj_Suicide_Mutilation<- SD_Retest_Suicide_Mutilation/SD_Suicide_Mutilation
        A_Adj_Suicide_Mutilation<- M_Retest_Suicide_Mutilation - (B_Adj_Suicide_Mutilation * M_Suicide_Mutilation)
        PTS_Suicide_Mutilation_1<- (B_Adj_Suicide_Mutilation * Score_Suicide_Mutilation_1) + A_Adj_Suicide_Mutilation
        PTS_Suicide_Mutilation_2<- (B_Adj_Suicide_Mutilation * Score_Suicide_Mutilation_2) + A_Adj_Suicide_Mutilation
        PTS_Suicide_Mutilation<- c(PTS_Suicide_Mutilation_1, PTS_Suicide_Mutilation_2)
        B_Slope_Emptiness<- Rel_Emptiness * (SD_Retest_Emptiness/SD_Emptiness)
        A_Constant_Emptiness<- M_Retest_Emptiness - (B_Slope_Emptiness * M_Emptiness)
        B_Adj_Emptiness<- SD_Retest_Emptiness/SD_Emptiness
        A_Adj_Emptiness<- M_Retest_Emptiness - (B_Adj_Emptiness * M_Emptiness)
        PTS_Emptiness_1<- (B_Adj_Emptiness * Score_Emptiness_1) + A_Adj_Emptiness
        PTS_Emptiness_2<- (B_Adj_Emptiness * Score_Emptiness_2) + A_Adj_Emptiness
        PTS_Emptiness<- c(PTS_Emptiness_1,PTS_Emptiness_2)
        B_Slope_Anger<- Rel_Anger * (SD_Retest_Anger/SD_Anger)
        A_Constant_Anger<- M_Retest_Anger - (B_Slope_Anger * M_Anger)
        B_Adj_Anger<- SD_Retest_Anger/SD_Anger
        A_Adj_Anger<- M_Retest_Anger - (B_Adj_Anger * M_Anger)
        PTS_Anger_1<- (B_Adj_Anger * Score_Anger_1) + A_Adj_Anger
        PTS_Anger_2<- (B_Adj_Anger * Score_Anger_2) + A_Adj_Anger
        PTS_Anger<- c(PTS_Anger_1,PTS_Anger_2)
        B_Slope_Quasi_Psychotic<- Rel_Quasi_Psychotic * (SD_Retest_Quasi_Psychotic/SD_Quasi_Psychotic)
        A_Constant_Quasi_Psychotic<- M_Retest_Quasi_Psychotic - (B_Slope_Quasi_Psychotic * M_Quasi_Psychotic)
        B_Adj_Quasi_Psychotic<- SD_Retest_Quasi_Psychotic/SD_Quasi_Psychotic
        A_Adj_Quasi_Psychotic<- M_Retest_Quasi_Psychotic - (B_Adj_Quasi_Psychotic * M_Quasi_Psychotic)
        PTS_Quasi_Psychotic_1<- (B_Adj_Quasi_Psychotic * Score_Quasi_Psychotic_1) + A_Adj_Quasi_Psychotic
        PTS_Quasi_Psychotic_2<- (B_Adj_Quasi_Psychotic * Score_Quasi_Psychotic_2) + A_Adj_Quasi_Psychotic
        PTS_Quasi_Psychotic<- c(PTS_Quasi_Psychotic_1,PTS_Quasi_Psychotic_2)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS<- c(PTS_1,PTS_2)
        B_Slope_Impulsivity<- Rel_Impulsivity * (SD_Retest_Impulsivity/SD_Impulsivity)
        PTS_Impulsivity_1<- B_Slope_Impulsivity * Score_Impulsivity_1
        PTS_Impulsivity_2<- B_Slope_Impulsivity * Score_Impulsivity_2
        PTS_Impulsivity<- c(PTS_Impulsivity_1, PTS_Impulsivity_2)
        B_Slope_Affective_Instability<- Rel_Affective_Instability * (SD_Retest_Affective_Instability/SD_Affective_Instability)
        PTS_Affective_Instability_1<- B_Slope_Affective_Instability * Score_Affective_Instability_1
        PTS_Affective_Instability_2<- B_Slope_Affective_Instability * Score_Affective_Instability_2
        PTS_Affective_Instability<- c(PTS_Affective_Instability_1, PTS_Affective_Instability_2)
        B_Slope_Abandonment<- Rel_Abandonment * (SD_Retest_Abandonment/SD_Abandonment)
        PTS_Abandonment_1<- B_Slope_Abandonment * Score_Abandonment_1
        PTS_Abandonment_2<- B_Slope_Abandonment * Score_Abandonment_2
        PTS_Abandonment<- c(PTS_Abandonment_1, PTS_Abandonment_2)
        B_Slope_Relationships<- Rel_Relationships * (SD_Retest_Relationships/SD_Relationships)
        PTS_Relationships_1<- B_Slope_Relationships * Score_Relationships_1
        PTS_Relationships_2<- B_Slope_Relationships * Score_Relationships_2
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2)
        B_Slope_Self_Image<- Rel_Self_Image * (SD_Retest_Self_Image/SD_Self_Image)
        PTS_Self_Image_1<- B_Slope_Self_Image * Score_Self_Image_1
        PTS_Self_Image_2<- B_Slope_Self_Image * Score_Self_Image_2
        PTS_Self_Image<- c(PTS_Self_Image_1, PTS_Self_Image_2)
        B_Slope_Suicide_Mutilation<- Rel_Suicide_Mutilation * (SD_Retest_Suicide_Mutilation/SD_Suicide_Mutilation)
        PTS_Suicide_Mutilation_1<- B_Slope_Suicide_Mutilation * Score_Suicide_Mutilation_1
        PTS_Suicide_Mutilation_2<- B_Slope_Suicide_Mutilation * Score_Suicide_Mutilation_2
        PTS_Suicide_Mutilation<- c(PTS_Suicide_Mutilation_1, PTS_Suicide_Mutilation_2)
        B_Slope_Emptiness<- Rel_Emptiness * (SD_Retest_Emptiness/SD_Emptiness)
        PTS_Emptiness_1<- B_Slope_Emptiness * Score_Emptiness_1
        PTS_Emptiness_2<- B_Slope_Emptiness * Score_Emptiness_2
        PTS_Emptiness<- c(PTS_Emptiness_1, PTS_Emptiness_2) 
        B_Slope_Anger<- Rel_Anger * (SD_Retest_Anger/SD_Anger)
        PTS_Anger_1<- B_Slope_Anger * Score_Anger_1
        PTS_Anger_2<- B_Slope_Anger * Score_Anger_2
        PTS_Anger<- c(PTS_Anger_1, PTS_Anger_2)
        B_Slope_Quasi_Psychotic<- Rel_Quasi_Psychotic * (SD_Retest_Quasi_Psychotic/SD_Quasi_Psychotic)
        PTS_Quasi_Psychotic_1<- B_Slope_Quasi_Psychotic * Score_Quasi_Psychotic_1
        PTS_Quasi_Psychotic_2<- B_Slope_Quasi_Psychotic * Score_Quasi_Psychotic_2
        PTS_Quasi_Psychotic<- c(PTS_Quasi_Psychotic_1, PTS_Quasi_Psychotic_2)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2)
        PTS_Impulsivity_1<- Score_Impulsivity_1 + (M_Retest_Impulsivity - M_Impulsivity)
        PTS_Impulsivity_2<- Score_Impulsivity_2 + (M_Retest_Impulsivity - M_Impulsivity)
        PTS_Impulsivity<- c(PTS_Impulsivity_1, PTS_Impulsivity_2)
        PTS_Affective_Instability_1<- Score_Affective_Instability_1 + (M_Retest_Affective_Instability - M_Affective_Instability)
        PTS_Affective_Instability_2<- Score_Affective_Instability_2 + (M_Retest_Affective_Instability - M_Affective_Instability)
        PTS_Affective_Instability<- c(PTS_Affective_Instability_1, PTS_Affective_Instability_2)
        PTS_Abandonment_1<- Score_Abandonment_1 + (M_Retest_Abandonment - M_Abandonment)
        PTS_Abandonment_2<- Score_Abandonment_2 + (M_Retest_Abandonment - M_Abandonment)
        PTS_Abandonment<- c(PTS_Abandonment_1, PTS_Abandonment_2)
        PTS_Relationships_1<- Score_Relationships_1 + (M_Retest_Relationships - M_Relationships)
        PTS_Relationships_2<- Score_Relationships_2 + (M_Retest_Relationships - M_Relationships)
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2)
        PTS_Self_Image_1<- Score_Self_Image_1 + (M_Retest_Self_Image - M_Self_Image)
        PTS_Self_Image_2<- Score_Self_Image_2 + (M_Retest_Self_Image - M_Self_Image)
        PTS_Self_Image<- c(PTS_Self_Image_1, PTS_Self_Image_2)
        PTS_Suicide_Mutilation_1<- Score_Suicide_Mutilation_1 + (M_Retest_Suicide_Mutilation - M_Suicide_Mutilation)
        PTS_Suicide_Mutilation_2<- Score_Suicide_Mutilation_2 + (M_Retest_Suicide_Mutilation - M_Suicide_Mutilation)
        PTS_Suicide_Mutilation<- c(PTS_Suicide_Mutilation_1, PTS_Suicide_Mutilation_2)
        PTS_Emptiness_1<- Score_Emptiness_1 + (M_Retest_Emptiness - M_Emptiness)
        PTS_Emptiness_2<- Score_Emptiness_2 + (M_Retest_Emptiness - M_Emptiness)
        PTS_Emptiness<- c(PTS_Emptiness_1, PTS_Emptiness_2)
        PTS_Anger_1<- Score_Anger_1 + (M_Retest_Anger - M_Anger)
        PTS_Anger_2<- Score_Anger_2 + (M_Retest_Anger - M_Anger)
        PTS_Anger<- c(PTS_Anger_1, PTS_Anger_2)
        PTS_Quasi_Psychotic_1<- Score_Quasi_Psychotic_1 + (M_Retest_Quasi_Psychotic - M_Quasi_Psychotic)
        PTS_Quasi_Psychotic_2<- Score_Quasi_Psychotic_2 + (M_Retest_Quasi_Psychotic - M_Quasi_Psychotic)
        PTS_Quasi_Psychotic<- c(PTS_Quasi_Psychotic_1, PTS_Quasi_Psychotic_2)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Impulsivity<- round(PTS_Impulsivity, digits = 2)
      PTS_Affective_Instability<- round(PTS_Affective_Instability, digits = 2)
      PTS_Abandonment<- round(PTS_Abandonment, digits = 2)
      PTS_Relationships<- round(PTS_Relationships, digits = 2)
      PTS_Self_Image<- round(PTS_Self_Image, digits = 2)
      PTS_Suicide_Mutilation<- round(PTS_Suicide_Mutilation, digits = 2)
      PTS_Emptiness<- round(PTS_Emptiness, digits = 2)
      PTS_Anger<- round(PTS_Anger, digits = 2)
      PTS_Quasi_Psychotic<- round(PTS_Quasi_Psychotic, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2)
        SE_Impulsivity_1<- McSweeny_SE_Impulsivity*sqrt(1 + (1/SampleN) + ((Score_Impulsivity_1 - M_Impulsivity)^2/(SD_Impulsivity^2*(SampleN-1))))
        SE_Impulsivity_2<- McSweeny_SE_Impulsivity*sqrt(1 + (1/SampleN) + ((Score_Impulsivity_2 - M_Impulsivity)^2/(SD_Impulsivity^2*(SampleN-1))))
        SE_Impulsivity<-c(SE_Impulsivity_1, SE_Impulsivity_2)
        SE_Affective_Instability_1<- McSweeny_SE_Affective_Instability*sqrt(1 + (1/SampleN) + ((Score_Affective_Instability_1 - M_Affective_Instability)^2/(SD_Affective_Instability^2*(SampleN-1))))
        SE_Affective_Instability_2<- McSweeny_SE_Affective_Instability*sqrt(1 + (1/SampleN) + ((Score_Affective_Instability_2 - M_Affective_Instability)^2/(SD_Affective_Instability^2*(SampleN-1))))
        SE_Affective_Instability<-c(SE_Affective_Instability_1, SE_Affective_Instability_2)
        SE_Abandonment_1<- McSweeny_SE_Abandonment*sqrt(1 + (1/SampleN) + ((Score_Abandonment_1 - M_Abandonment)^2/(SD_Abandonment^2*(SampleN-1))))
        SE_Abandonment_2<- McSweeny_SE_Abandonment*sqrt(1 + (1/SampleN) + ((Score_Abandonment_2 - M_Abandonment)^2/(SD_Abandonment^2*(SampleN-1))))
        SE_Abandonment<-c(SE_Abandonment_1, SE_Abandonment_2)
        SE_Relationships_1<- McSweeny_SE_Relationships*sqrt(1 + (1/SampleN) + ((Score_Relationships_1 - M_Relationships)^2/(SD_Relationships^2*(SampleN-1))))
        SE_Relationships_2<- McSweeny_SE_Relationships*sqrt(1 + (1/SampleN) + ((Score_Relationships_2 - M_Relationships)^2/(SD_Relationships^2*(SampleN-1))))
        SE_Relationships<-c(SE_Relationships_1, SE_Relationships_2)
        SE_Self_Image_1<- McSweeny_SE_Self_Image*sqrt(1 + (1/SampleN) + ((Score_Self_Image_1 - M_Self_Image)^2/(SD_Self_Image^2*(SampleN-1))))
        SE_Self_Image_2<- McSweeny_SE_Self_Image*sqrt(1 + (1/SampleN) + ((Score_Self_Image_2 - M_Self_Image)^2/(SD_Self_Image^2*(SampleN-1))))
        SE_Self_Image<-c(SE_Self_Image_1, SE_Self_Image_2)
        SE_Suicide_Mutilation_1<- McSweeny_SE_Suicide_Mutilation*sqrt(1 + (1/SampleN) + ((Score_Suicide_Mutilation_1 - M_Suicide_Mutilation)^2/(SD_Suicide_Mutilation^2*(SampleN-1))))
        SE_Suicide_Mutilation_2<- McSweeny_SE_Suicide_Mutilation*sqrt(1 + (1/SampleN) + ((Score_Suicide_Mutilation_2 - M_Suicide_Mutilation)^2/(SD_Suicide_Mutilation^2*(SampleN-1))))
        SE_Suicide_Mutilation<-c(SE_Suicide_Mutilation_1, SE_Suicide_Mutilation_2)
        SE_Emptiness_1<- McSweeny_SE_Emptiness*sqrt(1 + (1/SampleN) + ((Score_Emptiness_1 - M_Emptiness)^2/(SD_Emptiness^2*(SampleN-1))))
        SE_Emptiness_2<- McSweeny_SE_Emptiness*sqrt(1 + (1/SampleN) + ((Score_Emptiness_2 - M_Emptiness)^2/(SD_Emptiness^2*(SampleN-1))))
        SE_Emptiness<-c(SE_Emptiness_1, SE_Emptiness_2)
        SE_Anger_1<- McSweeny_SE_Anger*sqrt(1 + (1/SampleN) + ((Score_Anger_1 - M_Anger)^2/(SD_Anger^2*(SampleN-1))))
        SE_Anger_2<- McSweeny_SE_Anger*sqrt(1 + (1/SampleN) + ((Score_Anger_2 - M_Anger)^2/(SD_Anger^2*(SampleN-1))))
        SE_Anger<-c(SE_Anger_1, SE_Anger_2)
        SE_Quasi_Psychotic_1<- McSweeny_SE_Quasi_Psychotic*sqrt(1 + (1/SampleN) + ((Score_Quasi_Psychotic_1 - M_Quasi_Psychotic)^2/(SD_Quasi_Psychotic^2*(SampleN-1))))
        SE_Quasi_Psychotic_2<- McSweeny_SE_Quasi_Psychotic*sqrt(1 + (1/SampleN) + ((Score_Quasi_Psychotic_2 - M_Quasi_Psychotic)^2/(SD_Quasi_Psychotic^2*(SampleN-1))))
        SE_Quasi_Psychotic<-c(SE_Quasi_Psychotic_1, SE_Quasi_Psychotic_2)
        SE<- round(SE, digits = 2)
        SE_Impulsivity<- round(SE_Impulsivity, digits = 2)
        SE_Affective_Instability<- round(SE_Affective_Instability, digits = 2)
        SE_Abandonment<- round(SE_Abandonment, digits = 2)
        SE_Relationships<- round(SE_Relationships, digits = 2)
        SE_Self_Image<- round(SE_Self_Image, digits = 2)
        SE_Suicide_Mutilation<- round(SE_Suicide_Mutilation, digits = 2)
        SE_Emptiness<- round(SE_Emptiness, digits = 2)
        SE_Anger<- round(SE_Anger, digits = 2)
        SE_Quasi_Psychotic<- round(SE_Quasi_Psychotic, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2))
        CI<- round(CI, digits = 2)
        CI_Impulsivity<- c((Conf*SE_Impulsivity_1), (Conf*SE_Impulsivity_2))
        CI_Impulsivity<- round(CI_Impulsivity, digits = 2)
        CI_Affective_Instability<- c((Conf*SE_Affective_Instability_1), (Conf*SE_Affective_Instability_2))
        CI_Affective_Instability<- round(CI_Affective_Instability, digits = 2)
        CI_Abandonment<- c((Conf*SE_Abandonment_1), (Conf*SE_Abandonment_2))
        CI_Abandonment<- round(CI_Abandonment, digits = 2)
        CI_Relationships<- c((Conf*SE_Relationships_1), (Conf*SE_Relationships_2))
        CI_Relationships<- round(CI_Relationships, digits = 2)
        CI_Self_Image<- c((Conf*SE_Self_Image_1), (Conf*SE_Self_Image_2))
        CI_Self_Image<- round(CI_Self_Image, digits = 2)
        CI_Suicide_Mutilation<- c((Conf*SE_Suicide_Mutilation_1), (Conf*SE_Suicide_Mutilation_2))
        CI_Suicide_Mutilation<- round(CI_Suicide_Mutilation, digits = 2)
        CI_Emptiness<- c((Conf*SE_Emptiness_1), (Conf*SE_Emptiness_2))
        CI_Emptiness<- round(CI_Emptiness, digits = 2)
        CI_Anger<- c((Conf*SE_Anger_1), (Conf*SE_Anger_2))
        CI_Anger<- round(CI_Anger, digits = 2)
        CI_Quasi_Psychotic<- c((Conf*SE_Quasi_Psychotic_1), (Conf*SE_Quasi_Psychotic_2))
        CI_Quasi_Psychotic<- round(CI_Quasi_Psychotic, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
      CI<- c((Conf*SE), (Conf*SE))
      CI<- round(CI, digits = 2)
      CI_Impulsivity<- c((Conf*SE_Impulsivity), (Conf*SE_Impulsivity))
      CI_Impulsivity<- round(CI_Impulsivity, digits = 2)
      CI_Affective_Instability<- c((Conf*SE_Affective_Instability), (Conf*SE_Affective_Instability))
      CI_Affective_Instability<- round(CI_Affective_Instability, digits = 2)
      CI_Abandonment<- c((Conf*SE_Abandonment), (Conf*SE_Abandonment))
      CI_Abandonment<- round(CI_Abandonment, digits = 2)
      CI_Relationships<- c((Conf*SE_Relationships), (Conf*SE_Relationships))
      CI_Relationships<- round(CI_Relationships, digits = 2)
      CI_Self_Image<- c((Conf*SE_Self_Image), (Conf*SE_Self_Image))
      CI_Self_Image<- round(CI_Self_Image, digits = 2)
      CI_Suicide_Mutilation<- c((Conf*SE_Suicide_Mutilation), (Conf*SE_Suicide_Mutilation))
      CI_Suicide_Mutilation<- round(CI_Suicide_Mutilation, digits = 2)
      CI_Emptiness<- c((Conf*SE_Emptiness), (Conf*SE_Emptiness))
      CI_Emptiness<- round(CI_Emptiness, digits = 2)
      CI_Anger<- c((Conf*SE_Anger), (Conf*SE_Anger))
      CI_Anger<- round(CI_Anger, digits = 2)
      CI_Quasi_Psychotic<- c((Conf*SE_Quasi_Psychotic), (Conf*SE_Quasi_Psychotic))
      CI_Quasi_Psychotic<- round(CI_Quasi_Psychotic, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Impulsivity<- PTS_Impulsivity + CI_Impulsivity
      CI_Upper_Lim_Impulsivity<- round(CI_Upper_Lim_Impulsivity, digits = 2)
      CI_Lower_Lim_Impulsivity<-PTS_Impulsivity - CI_Impulsivity
      CI_Lower_Lim_Impulsivity<- round(CI_Lower_Lim_Impulsivity, digits = 2)
      CI_Upper_Lim_Affective_Instability<- PTS_Affective_Instability + CI_Affective_Instability
      CI_Upper_Lim_Affective_Instability<- round(CI_Upper_Lim_Affective_Instability, digits = 2)
      CI_Lower_Lim_Affective_Instability<-PTS_Affective_Instability - CI_Affective_Instability
      CI_Lower_Lim_Affective_Instability<- round(CI_Lower_Lim_Affective_Instability, digits = 2)
      CI_Upper_Lim_Abandonment<- PTS_Abandonment + CI_Abandonment
      CI_Upper_Lim_Abandonment<- round(CI_Upper_Lim_Abandonment, digits = 2)
      CI_Lower_Lim_Abandonment<-PTS_Abandonment - CI_Abandonment
      CI_Lower_Lim_Abandonment<- round(CI_Lower_Lim_Abandonment, digits = 2)
      CI_Upper_Lim_Relationships<- PTS_Relationships + CI_Relationships
      CI_Upper_Lim_Relationships<- round(CI_Upper_Lim_Relationships, digits = 2)
      CI_Lower_Lim_Relationships<-PTS_Relationships - CI_Relationships
      CI_Lower_Lim_Relationships<- round(CI_Lower_Lim_Relationships, digits = 2)
      CI_Upper_Lim_Self_Image<- PTS_Self_Image + CI_Self_Image
      CI_Upper_Lim_Self_Image<- round(CI_Upper_Lim_Self_Image, digits = 2)
      CI_Lower_Lim_Self_Image<-PTS_Self_Image - CI_Self_Image
      CI_Lower_Lim_Self_Image<- round(CI_Lower_Lim_Self_Image, digits = 2)
      CI_Upper_Lim_Suicide_Mutilation<- PTS_Suicide_Mutilation + CI_Suicide_Mutilation
      CI_Upper_Lim_Suicide_Mutilation<- round(CI_Upper_Lim_Suicide_Mutilation, digits = 2)
      CI_Lower_Lim_Suicide_Mutilation<- PTS_Suicide_Mutilation - CI_Suicide_Mutilation
      CI_Lower_Lim_Suicide_Mutilation<- round(CI_Lower_Lim_Suicide_Mutilation, digits = 2)
      CI_Upper_Lim_Emptiness<- PTS_Emptiness + CI_Emptiness
      CI_Upper_Lim_Emptiness<- round(CI_Upper_Lim_Emptiness, digits = 2)
      CI_Lower_Lim_Emptiness<-PTS_Emptiness - CI_Emptiness
      CI_Lower_Lim_Emptiness<- round(CI_Lower_Lim_Emptiness, digits = 2)
      CI_Upper_Lim_Anger<- PTS_Anger + CI_Anger
      CI_Upper_Lim_Anger<- round(CI_Upper_Lim_Anger, digits = 2)
      CI_Lower_Lim_Anger<-PTS_Anger - CI_Anger
      CI_Lower_Lim_Anger<- round(CI_Lower_Lim_Anger, digits = 2)
      CI_Upper_Lim_Quasi_Psychotic<- PTS_Quasi_Psychotic + CI_Quasi_Psychotic
      CI_Upper_Lim_Quasi_Psychotic<- round(CI_Upper_Lim_Quasi_Psychotic, digits = 2)
      CI_Lower_Lim_Quasi_Psychotic<-PTS_Quasi_Psychotic - CI_Quasi_Psychotic
      CI_Lower_Lim_Quasi_Psychotic<- round(CI_Lower_Lim_Quasi_Psychotic, digits = 2)
      
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Impulsivity == "2") {
        CI_Impulsivity<- input$Man_CI_Impulsivity
        CI_Impulsivity<- c(CI_Impulsivity, CI_Impulsivity)
        CI_Impulsivity<- round(CI_Impulsivity, digits = 2)
        CI_Upper_Lim_Impulsivity<- Score_Impulsivity + CI_Impulsivity
        CI_Upper_Lim_Impulsivity<- round(CI_Upper_Lim_Impulsivity, digits = 2)
        CI_Lower_Lim_Impulsivity<- Score_Impulsivity - CI_Impulsivity
        CI_Lower_Lim_Impulsivity<- round(CI_Lower_Lim_Impulsivity, digits = 2)
      }
      if(input$Select_CI_Affective_Instability == "2") {
        CI_Affective_Instability<- input$Man_CI_Affective_Instability
        CI_Affective_Instability<- c(CI_Affective_Instability, CI_Affective_Instability)
        CI_Affective_Instability<- round(CI_Affective_Instability, digits = 2)
        CI_Upper_Lim_Affective_Instability<- Score_Affective_Instability + CI_Affective_Instability
        CI_Upper_Lim_Affective_Instability<- round(CI_Upper_Lim_Affective_Instability, digits = 2)
        CI_Lower_Lim_Affective_Instability<- Score_Affective_Instability - CI_Affective_Instability
        CI_Lower_Lim_Affective_Instability<- round(CI_Lower_Lim_Affective_Instability, digits = 2)
      }
      if(input$Select_CI_Abandonment == "2") {
        CI_Abandonment<- input$Man_CI_Abandonment
        CI_Abandonment<- c(CI_Abandonment, CI_Abandonment)
        CI_Abandonment<- round(CI_Abandonment, digits = 2)
        CI_Upper_Lim_Abandonment<- Score_Abandonment + CI_Abandonment
        CI_Upper_Lim_Abandonment<- round(CI_Upper_Lim_Abandonment, digits = 2)
        CI_Lower_Lim_Abandonment<- Score_Abandonment - CI_Abandonment
        CI_Lower_Lim_Abandonment<- round(CI_Lower_Lim_Abandonment, digits = 2)
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
      if(input$Select_CI_Self_Image == "2") {
        CI_Self_Image<- input$Man_CI_Self_Image
        CI_Self_Image<- c(CI_Self_Image,  CI_Self_Image)
        CI_Self_Image<- round(CI_Self_Image, digits = 2)
        CI_Upper_Lim_Self_Image<- Score_Self_Image + CI_Self_Image
        CI_Upper_Lim_Self_Image<- round(CI_Upper_Lim_Self_Image, digits = 2)
        CI_Lower_Lim_Self_Image<- Score_Self_Image - CI_Self_Image
        CI_Lower_Lim_Self_Image<- round(CI_Lower_Lim_Self_Image, digits = 2)
      }
      if(input$Select_CI_Suicide_Mutilation == "2") {
        CI_Suicide_Mutilation<- input$Man_CI_Suicide_Mutilation
        CI_Suicide_Mutilation<- c(CI_Suicide_Mutilation, CI_Suicide_Mutilation)
        CI_Suicide_Mutilation<- round(CI_Suicide_Mutilation, digits = 2)
        CI_Upper_Lim_Suicide_Mutilation<- Score_Suicide_Mutilation + CI_Suicide_Mutilation
        CI_Upper_Lim_Suicide_Mutilation<- round(CI_Upper_Lim_Suicide_Mutilation, digits = 2)
        CI_Lower_Lim_Suicide_Mutilation<- Score_Suicide_Mutilation - CI_Suicide_Mutilation
        CI_Lower_Lim_Suicide_Mutilation<- round(CI_Lower_Lim_Suicide_Mutilation, digits = 2)
      }
      if(input$Select_CI_Emptiness == "2") {
        CI_Emptiness<- input$Man_CI_Emptiness
        CI_Emptiness<- c(CI_Emptiness, CI_Emptiness)
        CI_Emptiness<- round(CI_Emptiness, digits = 2)
        CI_Upper_Lim_Emptiness<- Score_Emptiness + CI_Emptiness
        CI_Upper_Lim_Emptiness<- round(CI_Upper_Lim_Emptiness, digits = 2)
        CI_Lower_Lim_Emptiness<- Score_Emptiness - CI_Emptiness
        CI_Lower_Lim_Emptiness<- round(CI_Lower_Lim_Emptiness, digits = 2)
      }
      if(input$Select_CI_Anger == "2") {
        CI_Anger<- input$Man_CI_Anger
        CI_Anger<- round(CI_Anger, digits = 2)
        CI_Anger<- c(CI_Anger,  CI_Anger)
        CI_Upper_Lim_Anger<- Score_Anger + CI_Anger
        CI_Upper_Lim_Anger<- round(CI_Upper_Lim_Anger, digits = 2)
        CI_Lower_Lim_Anger<- Score_Anger - CI_Anger
        CI_Lower_Lim_Anger<- round(CI_Lower_Lim_Anger, digits = 2)
      }
      if(input$Select_CI_Quasi_Psychotic == "2") {
        CI_Quasi_Psychotic<- input$Man_CI_Quasi_Psychotic
        CI_Quasi_Psychotic<- c(CI_Quasi_Psychotic, CI_Quasi_Psychotic)
        CI_Quasi_Psychotic<- round(CI_Quasi_Psychotic, digits = 2)
        CI_Upper_Lim_Quasi_Psychotic<- Score_Quasi_Psychotic + CI_Quasi_Psychotic
        CI_Upper_Lim_Quasi_Psychotic<- round(CI_Upper_Lim_Quasi_Psychotic, digits = 2)
        CI_Lower_Lim_Quasi_Psychotic<- Score_Quasi_Psychotic - CI_Quasi_Psychotic
        CI_Lower_Lim_Quasi_Psychotic<- round(CI_Lower_Lim_Quasi_Psychotic, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Impulsivity_1<- round(input$Cutoff_Impulsivity_1, digits = 2)
      Cutoff_Score_Impulsivity_2<- round(input$Cutoff_Impulsivity_2, digits = 2)
      Cutoff_Score_Impulsivity_3<- round(input$Cutoff_Impulsivity_3, digits = 2)
      Cutoff_Score_Affective_Instability_1<- round(input$Cutoff_Affective_Instability_1, digits = 2)
      Cutoff_Score_Affective_Instability_2<- round(input$Cutoff_Affective_Instability_2, digits = 2)
      Cutoff_Score_Affective_Instability_3<- round(input$Cutoff_Affective_Instability_3, digits = 2)
      Cutoff_Score_Abandonment_1<- round(input$Cutoff_Abandonment_1, digits = 2)
      Cutoff_Score_Abandonment_2<- round(input$Cutoff_Abandonment_2, digits = 2)
      Cutoff_Score_Abandonment_3<- round(input$Cutoff_Abandonment_3, digits = 2)
      Cutoff_Score_Relationships_1<- round(input$Cutoff_Relationships_1, digits = 2)
      Cutoff_Score_Relationships_2<- round(input$Cutoff_Relationships_2, digits = 2)
      Cutoff_Score_Relationships_3<- round(input$Cutoff_Relationships_3, digits = 2)
      Cutoff_Score_Self_Image_1<- round(input$Cutoff_Self_Image_1, digits = 2)
      Cutoff_Score_Self_Image_2<- round(input$Cutoff_Self_Image_2, digits = 2)
      Cutoff_Score_Self_Image_3<- round(input$Cutoff_Self_Image_3, digits = 2)
      Cutoff_Score_Suicide_Mutilation_1<- round(input$Cutoff_Suicide_Mutilation_1, digits = 2)
      Cutoff_Score_Suicide_Mutilation_2<- round(input$Cutoff_Suicide_Mutilation_2, digits = 2)
      Cutoff_Score_Suicide_Mutilation_3<- round(input$Cutoff_Suicide_Mutilation_3, digits = 2)
      Cutoff_Score_Emptiness_1<- round(input$Cutoff_Emptiness_1, digits = 2)
      Cutoff_Score_Emptiness_2<- round(input$Cutoff_Emptiness_2, digits = 2)
      Cutoff_Score_Emptiness_3<- round(input$Cutoff_Emptiness_3, digits = 2)
      Cutoff_Score_Anger_1<- round(input$Cutoff_Anger_1, digits = 2)
      Cutoff_Score_Anger_2<- round(input$Cutoff_Anger_2, digits = 2)
      Cutoff_Score_Anger_3<- round(input$Cutoff_Anger_3, digits = 2)
      Cutoff_Score_Quasi_Psychotic_1<- round(input$Cutoff_Quasi_Psychotic_1, digits = 2)
      Cutoff_Score_Quasi_Psychotic_2<- round(input$Cutoff_Quasi_Psychotic_2, digits = 2)
      Cutoff_Score_Quasi_Psychotic_3<- round(input$Cutoff_Quasi_Psychotic_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Impulsivity,Change_Impulsivity,PTS_Impulsivity, SE_Impulsivity, CI_Upper_Lim_Impulsivity, CI_Lower_Lim_Impulsivity, Cutoff_Score_Impulsivity_1,Cutoff_Score_Impulsivity_2,Cutoff_Score_Impulsivity_3,
                                      Score_Affective_Instability,Change_Affective_Instability, PTS_Affective_Instability, SE_Affective_Instability, CI_Upper_Lim_Affective_Instability, CI_Lower_Lim_Affective_Instability, Cutoff_Score_Affective_Instability_1,Cutoff_Score_Affective_Instability_2,Cutoff_Score_Affective_Instability_3, 
                                      Score_Abandonment,Change_Abandonment,PTS_Abandonment, SE_Abandonment, CI_Upper_Lim_Abandonment, CI_Lower_Lim_Abandonment, Cutoff_Score_Abandonment_1,Cutoff_Score_Abandonment_2,Cutoff_Score_Abandonment_3, 
                                      Score_Relationships,Change_Relationships,PTS_Relationships, SE_Relationships, CI_Upper_Lim_Relationships, CI_Lower_Lim_Relationships, Cutoff_Score_Relationships_1,Cutoff_Score_Relationships_2,Cutoff_Score_Relationships_3, 
                                      Score_Self_Image,Change_Self_Image,PTS_Self_Image, SE_Self_Image, CI_Upper_Lim_Self_Image, CI_Lower_Lim_Self_Image, Cutoff_Score_Self_Image_1,Cutoff_Score_Self_Image_2,Cutoff_Score_Self_Image_3,
                                      Score_Suicide_Mutilation,Change_Suicide_Mutilation, PTS_Suicide_Mutilation, SE_Suicide_Mutilation, CI_Upper_Lim_Suicide_Mutilation, CI_Lower_Lim_Suicide_Mutilation, Cutoff_Score_Suicide_Mutilation_1,Cutoff_Score_Suicide_Mutilation_2,Cutoff_Score_Suicide_Mutilation_3, 
                                      Score_Emptiness,Change_Emptiness,PTS_Emptiness, SE_Emptiness, CI_Upper_Lim_Emptiness, CI_Lower_Lim_Emptiness, Cutoff_Score_Emptiness_1,Cutoff_Score_Emptiness_2,Cutoff_Score_Emptiness_3, 
                                      Score_Anger,Change_Anger,PTS_Anger, SE_Anger, CI_Upper_Lim_Anger, CI_Lower_Lim_Anger, Cutoff_Score_Anger_1,Cutoff_Score_Anger_2,Cutoff_Score_Anger_3, 
                                      Score_Quasi_Psychotic,Change_Quasi_Psychotic,PTS_Quasi_Psychotic, SE_Quasi_Psychotic, CI_Upper_Lim_Quasi_Psychotic, CI_Lower_Lim_Quasi_Psychotic, Cutoff_Score_Quasi_Psychotic_1,Cutoff_Score_Quasi_Psychotic_2,Cutoff_Score_Quasi_Psychotic_3)
    } else if(input$Timepoint == "3") {
      Date_1<- input$Date_1
      Date_2<- input$Date_2
      Date_3<- input$Date_3
      Date<- c(Date_1, Date_2, Date_3)
      Date<- format(as.Date(Date), "%d/%m/%Y")
      Score_1a<<-as.numeric(unlist(strsplit(input$Text_1,",")))
      Recode_1<- car::recode(Score_1a[c(10,43,28,4,45,60,52,67,53,54,8,32,48)],'0=1; 1=0')
      Score_1a[c(10,43,28,4,45,60,52,67,53,54,8,32,48)]<- Recode_1
      Score_1<- sum(Score_1a, na.rm = TRUE)
      Score_2a<<- as.numeric(unlist(strsplit(input$Text_2,",")))
      Recode_2<- car::recode(Score_2a[c(10,43,28,4,45,60,52,67,53,54,8,32,48)],'0=1; 1=0')
      Score_2a[c(10,43,28,4,45,60,52,67,53,54,8,32,48)]<- Recode_2
      Score_2<- sum(Score_2a, na.rm = TRUE)
      Score_3a<<- as.numeric(unlist(strsplit(input$Text_3,",")))
      Item_Df<<- data.frame(Item = 1:length(Score_3a), Score = Score_3a)
      Recode_3<- car::recode(Score_3a[c(10,43,28,4,45,60,52,67,53,54,8,32,48)],'0=1; 1=0')
      Score_3a[c(10,43,28,4,45,60,52,67,53,54,8,32,48)]<- Recode_3
      Score_3<- sum(Score_3a, na.rm = TRUE)
      Score<- c(Score_1, Score_2, Score_3)
      Score<- round(Score, digits = 2)
      Score_Impulsivity_1<- sum(Score_1a[c(1,10,26,34,42,57,64,68,71)], na.rm = TRUE)
      Score_Impulsivity_2<- sum(Score_2a[c(1,10,26,34,42,57,64,68,71)], na.rm = TRUE)
      Score_Impulsivity_3<- sum(Score_3a[c(1,10,26,34,42,57,64,68,71)], na.rm = TRUE)
      Score_Impulsivity<- c(Score_Impulsivity_1, Score_Impulsivity_2, Score_Impulsivity_3)
      Score_Impulsivity<- round(Score_Impulsivity, digits = 2)
      Score_Affective_Instability_1<- sum(Score_1a[c(2, 11, 19, 27, 35, 43, 49, 58, 65, 72)], na.rm = TRUE)
      Score_Affective_Instability_2<- sum(Score_2a[c(2, 11, 19, 27, 35, 43, 49, 58, 65, 72)], na.rm = TRUE)
      Score_Affective_Instability_3<- sum(Score_3a[c(2, 11, 19, 27, 35, 43, 49, 58, 65, 72)], na.rm = TRUE)
      Score_Affective_Instability<- c(Score_Affective_Instability_1,Score_Affective_Instability_2, Score_Affective_Instability_3)
      Score_Affective_Instability<- round(Score_Affective_Instability, digits = 2)
      Score_Abandonment_1<- sum(Score_1a[c(3, 12, 20, 28, 44, 50, 59, 66, 73, 78)], na.rm = TRUE)
      Score_Abandonment_2<- sum(Score_2a[c(3, 12, 20, 28, 44, 50, 59, 66, 73, 78)], na.rm = TRUE)
      Score_Abandonment_3<- sum(Score_3a[c(3, 12, 20, 28, 44, 50, 59, 66, 73, 78)], na.rm = TRUE)
      Score_Abandonment<- c(Score_Abandonment_1, Score_Abandonment_2, Score_Abandonment_3)
      Score_Abandonment<- round(Score_Abandonment, digits = 2)
      Score_Relationships_1<- sum(Score_1a[c(4, 13, 21, 29, 36, 45, 51, 60)], na.rm = TRUE)
      Score_Relationships_2<- sum(Score_2a[c(4, 13, 21, 29, 36, 45, 51, 60)], na.rm = TRUE)
      Score_Relationships_3<- sum(Score_3a[c(4, 13, 21, 29, 36, 45, 51, 60)], na.rm = TRUE)
      Score_Relationships<- c(Score_Relationships_1, Score_Relationships_2, Score_Relationships_3)
      Score_Relationships<- round(Score_Relationships, digits = 2)
      Score_Self_Image_1<- sum(Score_1a[c(5, 14, 37, 46, 52, 61, 67, 70, 74)], na.rm = TRUE)
      Score_Self_Image_2<- sum(Score_2a[c(5, 14, 37, 46, 52, 61, 67, 70, 74)], na.rm = TRUE)
      Score_Self_Image_3<- sum(Score_3a[c(5, 14, 37, 46, 52, 61, 67, 70, 74)], na.rm = TRUE)
      Score_Self_Image<- c(Score_Self_Image_1, Score_Self_Image_2, Score_Self_Image_3)
      Score_Self_Image<- round(Score_Self_Image, digits = 2)
      Score_Suicide_Mutilation_1<- sum(Score_1a[c(6, 15, 22, 30, 38, 53, 75)], na.rm = TRUE)
      Score_Suicide_Mutilation_2<- sum(Score_2a[c(6, 15, 22, 30, 38, 53, 75)], na.rm = TRUE)
      Score_Suicide_Mutilation_3<- sum(Score_3a[c(6, 15, 22, 30, 38, 53, 75)], na.rm = TRUE)
      Score_Suicide_Mutilation<- c(Score_Suicide_Mutilation_1, Score_Suicide_Mutilation_2, Score_Suicide_Mutilation_3)
      Score_Suicide_Mutilation<- round(Score_Suicide_Mutilation, digits = 2)
      Score_Emptiness_1<- sum(Score_1a[c(7, 16, 23, 31, 39, 54, 62, 69, 76, 79)], na.rm = TRUE)
      Score_Emptiness_2<- sum(Score_2a[c(7, 16, 23, 31, 39, 54, 62, 69, 76, 79)], na.rm = TRUE)
      Score_Emptiness_3<- sum(Score_3a[c(7, 16, 23, 31, 39, 54, 62, 69, 76, 79)], na.rm = TRUE)
      Score_Emptiness<- c(Score_Emptiness_1, Score_Emptiness_2, Score_Emptiness_3)
      Score_Emptiness<- round(Score_Emptiness, digits = 2)
      Score_Anger_1<- sum(Score_1a[c(8, 17, 24, 32, 40, 47, 55, 63, 77, 80)], na.rm = TRUE)
      Score_Anger_2<- sum(Score_2a[c(8, 17, 24, 32, 40, 47, 55, 63, 77, 80)], na.rm = TRUE)
      Score_Anger_3<- sum(Score_3a[c(8, 17, 24, 32, 40, 47, 55, 63, 77, 80)], na.rm = TRUE)
      Score_Anger<- c(Score_Anger_1, Score_Anger_2, Score_Anger_3)
      Score_Anger<- round(Score_Anger, digits = 2)
      Score_Quasi_Psychotic_1<- sum(Score_1a[c(9, 18, 25, 33, 41, 48, 56)], na.rm = TRUE)
      Score_Quasi_Psychotic_2<- sum(Score_2a[c(9, 18, 25, 33, 41, 48, 56)], na.rm = TRUE)
      Score_Quasi_Psychotic_3<- sum(Score_3a[c(9, 18, 25, 33, 41, 48, 56)], na.rm = TRUE)
      Score_Quasi_Psychotic<- c(Score_Quasi_Psychotic_1, Score_Quasi_Psychotic_2, Score_Quasi_Psychotic_3)
      Score_Quasi_Psychotic<- round(Score_Quasi_Psychotic, digits = 2)
      Change<- c(0, (Score_2 - Score_1), (Score_3 - Score_2))
      Change_Impulsivity<- round(Change, digits = 2)
      Change_Affective_Instability<- c(0, (Score_Affective_Instability_2 - Score_Affective_Instability_1), (Score_Affective_Instability_3 - Score_Affective_Instability_2))
      Change_Affective_Instability<- round(Change_Affective_Instability, digits = 2)
      Change_Abandonment<- c(0, (Score_Abandonment_2 - Score_Abandonment_1),  (Score_Abandonment_3 - Score_Abandonment_2))
      Change_Abandonment<- round(Change_Abandonment, digits = 2)
      Change_Relationships<- c(0, (Score_Relationships_2 - Score_Relationships_1), (Score_Relationships_3 - Score_Relationships_2))
      Change_Relationships<- round(Change_Relationships, digits = 2)
      Change_Self_Image<- c(0, (Score_Self_Image_2 - Score_Self_Image_1), (Score_Self_Image_3 - Score_Self_Image_2))
      Change_Self_Image<- round(Change_Self_Image, digits = 2)
      Change_Suicide_Mutilation<- c(0, (Score_Suicide_Mutilation_2 - Score_Suicide_Mutilation_1), (Score_Suicide_Mutilation_3 - Score_Suicide_Mutilation_2))
      Change_Suicide_Mutilation<- round(Change_Suicide_Mutilation, digits = 2)
      Change_Emptiness<- c(0, (Score_Emptiness_2 - Score_Emptiness_1), (Score_Emptiness_3 - Score_Emptiness_2))
      Change_Emptiness<- round(Change_Emptiness, digits = 2)
      Change_Anger<- c(0, (Score_Anger_2 - Score_Anger_1), (Score_Anger_3 - Score_Anger_2))
      Change_Anger<- round(Change_Anger, digits = 2)
      Change_Quasi_Psychotic<- c(0, (Score_Quasi_Psychotic_2 - Score_Quasi_Psychotic_1),  (Score_Quasi_Psychotic_3 - Score_Quasi_Psychotic_2))
      Change_Quasi_Psychotic<- round(Change_Quasi_Psychotic, digits = 2)
      if (input$RelChangeMethod == "Nunnally & Bernstein (1994)") {
        PTS_1<- (Rel * Score_1) + (M * (1 - Rel))
        PTS_2<- (Rel * Score_2) + (M * (1 - Rel))
        PTS_3<- (Rel * Score_3) + (M * (1 - Rel))
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Impulsivity_1<- (Rel_Impulsivity * Score_Impulsivity_1) + (M_Impulsivity * (1 - Rel_Impulsivity))
        PTS_Impulsivity_2<- (Rel_Impulsivity * Score_Impulsivity_2) + (M_Impulsivity * (1 - Rel_Impulsivity))
        PTS_Impulsivity_3<- (Rel_Impulsivity * Score_Impulsivity_3) + (M_Impulsivity * (1 - Rel_Impulsivity))
        PTS_Impulsivity<- c(PTS_Impulsivity_1, PTS_Impulsivity_2, PTS_Impulsivity_3)
        PTS_Affective_Instability_1<- (Rel_Affective_Instability * Score_Affective_Instability_1) + (M_Affective_Instability * (1 - Rel_Affective_Instability))
        PTS_Affective_Instability_2<- (Rel_Affective_Instability * Score_Affective_Instability_2) + (M_Affective_Instability * (1 - Rel_Affective_Instability))
        PTS_Affective_Instability_3<- (Rel_Affective_Instability * Score_Affective_Instability_3) + (M_Affective_Instability * (1 - Rel_Affective_Instability))
        PTS_Affective_Instability<- c(PTS_Affective_Instability_1, PTS_Affective_Instability_2, PTS_Affective_Instability_3)
        PTS_Abandonment_1<- (Rel_Abandonment * Score_Abandonment_1) + (M_Abandonment * (1 - Rel_Abandonment))
        PTS_Abandonment_2<- (Rel_Abandonment * Score_Abandonment_2) + (M_Abandonment * (1 - Rel_Abandonment))
        PTS_Abandonment_3<- (Rel_Abandonment * Score_Abandonment_3) + (M_Abandonment * (1 - Rel_Abandonment))
        PTS_Abandonment<- c(PTS_Abandonment_1, PTS_Abandonment_2, PTS_Abandonment_3)
        PTS_Relationships_1<- (Rel_Relationships * Score_Relationships_1) + (M_Relationships * (1 - Rel_Relationships))
        PTS_Relationships_2<- (Rel_Relationships * Score_Relationships_2) + (M_Relationships * (1 - Rel_Relationships))
        PTS_Relationships_3<- (Rel_Relationships * Score_Relationships_3) + (M_Relationships * (1 - Rel_Relationships))
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2,  PTS_Relationships_3)
        PTS_Self_Image_1<- (Rel_Self_Image * Score_Self_Image_1) + (M_Self_Image * (1 - Rel_Self_Image))
        PTS_Self_Image_2<- (Rel_Self_Image * Score_Self_Image_2) + (M_Self_Image * (1 - Rel_Self_Image))
        PTS_Self_Image_3<- (Rel_Self_Image * Score_Self_Image_3) + (M_Self_Image * (1 - Rel_Self_Image))
        PTS_Self_Image<- c(PTS_Self_Image_1, PTS_Self_Image_2, PTS_Self_Image_3)
        PTS_Suicide_Mutilation_1<- (Rel_Suicide_Mutilation * Score_Suicide_Mutilation_1) + (M_Suicide_Mutilation * (1 - Rel_Suicide_Mutilation))
        PTS_Suicide_Mutilation_2<- (Rel_Suicide_Mutilation * Score_Suicide_Mutilation_2) + (M_Suicide_Mutilation * (1 - Rel_Suicide_Mutilation))
        PTS_Suicide_Mutilation_3<- (Rel_Suicide_Mutilation * Score_Suicide_Mutilation_3) + (M_Suicide_Mutilation * (1 - Rel_Suicide_Mutilation))
        PTS_Suicide_Mutilation<- c(PTS_Suicide_Mutilation_1, PTS_Suicide_Mutilation_2, PTS_Suicide_Mutilation_3)
        PTS_Emptiness_1<- (Rel_Emptiness * Score_Emptiness_1) + (M_Emptiness * (1 - Rel_Emptiness))
        PTS_Emptiness_2<- (Rel_Emptiness * Score_Emptiness_2) + (M_Emptiness * (1 - Rel_Emptiness))
        PTS_Emptiness_3<- (Rel_Emptiness * Score_Emptiness_3) + (M_Emptiness * (1 - Rel_Emptiness))
        PTS_Emptiness<- c(PTS_Emptiness_1, PTS_Emptiness_2, PTS_Emptiness_3)
        PTS_Anger_1<- (Rel_Anger * Score_Anger_1) + (M_Anger * (1 - Rel_Anger))
        PTS_Anger_2<- (Rel_Anger * Score_Anger_2) + (M_Anger * (1 - Rel_Anger))
        PTS_Anger_3<- (Rel_Anger * Score_Anger_3) + (M_Anger * (1 - Rel_Anger))
        PTS_Anger<- c(PTS_Anger_1, PTS_Anger_2, PTS_Anger_3)
        PTS_Quasi_Psychotic_1<- (Rel_Quasi_Psychotic * Score_Quasi_Psychotic_1) + (M_Quasi_Psychotic * (1 - Rel_Quasi_Psychotic))
        PTS_Quasi_Psychotic_2<- (Rel_Quasi_Psychotic * Score_Quasi_Psychotic_2) + (M_Quasi_Psychotic * (1 - Rel_Quasi_Psychotic))
        PTS_Quasi_Psychotic_3<- (Rel_Quasi_Psychotic * Score_Quasi_Psychotic_3) + (M_Quasi_Psychotic * (1 - Rel_Quasi_Psychotic))
        PTS_Quasi_Psychotic<- c(PTS_Quasi_Psychotic_1, PTS_Quasi_Psychotic_2, PTS_Quasi_Psychotic_3)
      } else if (input$RelChangeMethod == "Chelune et al. (1993)") {
        PTS_1<- Score_1 + (M_Retest - M)  
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1, PTS_2, PTS_3)
        PTS_Impulsivity_1<- Score_Impulsivity_1 + (M_Retest_Impulsivity - M_Impulsivity)  
        PTS_Impulsivity_2<- Score_Impulsivity_2 + (M_Retest_Impulsivity - M_Impulsivity) 
        PTS_Impulsivity_3<- Score_Impulsivity_3 + (M_Retest_Impulsivity - M_Impulsivity) 
        PTS_Impulsivity<- c(PTS_Impulsivity_1, PTS_Impulsivity_2, PTS_Impulsivity_3)
        PTS_Affective_Instability_1<- Score_Affective_Instability_1 + (M_Retest_Affective_Instability - M_Affective_Instability)  
        PTS_Affective_Instability_2<- Score_Affective_Instability_2 + (M_Retest_Affective_Instability - M_Affective_Instability) 
        PTS_Affective_Instability_3<- Score_Affective_Instability_3 + (M_Retest_Affective_Instability - M_Affective_Instability) 
        PTS_Affective_Instability<- c(PTS_Affective_Instability_1, PTS_Affective_Instability_2, PTS_Affective_Instability_3)
        PTS_Abandonment_1<- Score_Abandonment_1 + (M_Retest_Abandonment - M_Abandonment)  
        PTS_Abandonment_2<- Score_Abandonment_2 + (M_Retest_Abandonment - M_Abandonment) 
        PTS_Abandonment_3<- Score_Abandonment_3 + (M_Retest_Abandonment - M_Abandonment) 
        PTS_Abandonment<- c(PTS_Abandonment_1, PTS_Abandonment_2, PTS_Abandonment_3)
        PTS_Relationships_1<- Score_Relationships_1 + (M_Retest_Relationships - M_Relationships)  
        PTS_Relationships_2<- Score_Relationships_2 + (M_Retest_Relationships - M_Relationships) 
        PTS_Relationships_3<- Score_Relationships_3 + (M_Retest_Relationships - M_Relationships) 
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2, PTS_Relationships_3)
        PTS_Self_Image_1<- Score_Self_Image_1 + (M_Retest_Self_Image - M_Self_Image)  
        PTS_Self_Image_2<- Score_Self_Image_2 + (M_Retest_Self_Image - M_Self_Image) 
        PTS_Self_Image_3<- Score_Self_Image_3 + (M_Retest_Self_Image - M_Self_Image) 
        PTS_Self_Image<- c(PTS_Self_Image_1, PTS_Self_Image_2, PTS_Self_Image_3)
        PTS_Suicide_Mutilation_1<- Score_Suicide_Mutilation_1 + (M_Retest_Suicide_Mutilation - M_Suicide_Mutilation)  
        PTS_Suicide_Mutilation_2<- Score_Suicide_Mutilation_2 + (M_Retest_Suicide_Mutilation - M_Suicide_Mutilation) 
        PTS_Suicide_Mutilation_3<- Score_Suicide_Mutilation_3 + (M_Retest_Suicide_Mutilation - M_Suicide_Mutilation) 
        PTS_Suicide_Mutilation<- c(PTS_Suicide_Mutilation_1, PTS_Suicide_Mutilation_2, PTS_Suicide_Mutilation_3)
        PTS_Emptiness_1<- Score_Emptiness_1 + (M_Retest_Emptiness - M_Emptiness)  
        PTS_Emptiness_2<- Score_Emptiness_2 + (M_Retest_Emptiness - M_Emptiness) 
        PTS_Emptiness_3<- Score_Emptiness_3 + (M_Retest_Emptiness - M_Emptiness) 
        PTS_Emptiness<- c(PTS_Emptiness_1, PTS_Emptiness_2, PTS_Emptiness_3)
        PTS_Anger_1<- Score_Anger_1 + (M_Retest_Anger - M_Anger)  
        PTS_Anger_2<- Score_Anger_2 + (M_Retest_Anger - M_Anger) 
        PTS_Anger_3<- Score_Anger_3 + (M_Retest_Anger - M_Anger) 
        PTS_Anger<- c(PTS_Anger_1, PTS_Anger_2, PTS_Anger_3)
        PTS_Quasi_Psychotic_1<- Score_Quasi_Psychotic_1 + (M_Retest_Quasi_Psychotic - M_Quasi_Psychotic)  
        PTS_Quasi_Psychotic_2<- Score_Quasi_Psychotic_2 + (M_Retest_Quasi_Psychotic - M_Quasi_Psychotic) 
        PTS_Quasi_Psychotic_3<- Score_Quasi_Psychotic_3 + (M_Retest_Quasi_Psychotic - M_Quasi_Psychotic) 
        PTS_Quasi_Psychotic<- c(PTS_Quasi_Psychotic_1, PTS_Quasi_Psychotic_2, PTS_Quasi_Psychotic_3)
      } else if (input$RelChangeMethod == "Jacobson & Truax (1991)") {
        PTS<- Score
        PTS_Impulsivity<- Score_Impulsivity
        PTS_Affective_Instability<- Score_Affective_Instability
        PTS_Abandonment<- Score_Abandonment
        PTS_Relationships<- Score_Relationships
        PTS_Self_Image<- Score_Self_Image
        PTS_Suicide_Mutilation<- Score_Suicide_Mutilation
        PTS_Emptiness<- Score_Emptiness
        PTS_Anger<- Score_Anger
        PTS_Quasi_Psychotic<- Score_Quasi_Psychotic
      } else if (input$RelChangeMethod == "Maassen et al. (2006)"){
        B_Slope<- Rel * (SD_Retest/SD)
        A_Constant<- M_Retest - (B_Slope * M)
        B_Adj<- SD_Retest/SD
        A_Adj<- M_Retest - (B_Adj * M)
        PTS_1<- (B_Adj * Score_1) + A_Adj
        PTS_2<- (B_Adj * Score_2) + A_Adj
        PTS_3<- (B_Adj * Score_3) + A_Adj
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Impulsivity<- Rel_Impulsivity * (SD_Retest_Impulsivity/SD_Impulsivity)
        A_Constant_Impulsivity<- M_Retest_Impulsivity - (B_Slope_Impulsivity * M_Impulsivity)
        B_Adj_Impulsivity<- SD_Retest_Impulsivity/SD_Impulsivity
        A_Adj_Impulsivity<- M_Retest_Impulsivity - (B_Adj_Impulsivity * M_Impulsivity)
        PTS_Impulsivity_1<- (B_Adj_Impulsivity * Score_Impulsivity_1) + A_Adj_Impulsivity
        PTS_Impulsivity_2<- (B_Adj_Impulsivity * Score_Impulsivity_2) + A_Adj_Impulsivity
        PTS_Impulsivity_3<- (B_Adj_Impulsivity * Score_Impulsivity_3) + A_Adj_Impulsivity
        PTS_Impulsivity<- c(PTS_Impulsivity_1,PTS_Impulsivity_2, PTS_Impulsivity_3)
        B_Slope_Affective_Instability<- Rel_Affective_Instability * (SD_Retest_Affective_Instability/SD_Affective_Instability)
        A_Constant_Affective_Instability<- M_Retest_Affective_Instability - (B_Slope_Affective_Instability * M_Affective_Instability)
        B_Adj_Affective_Instability<- SD_Retest_Affective_Instability/SD_Affective_Instability
        A_Adj_Affective_Instability<- M_Retest_Affective_Instability - (B_Adj_Affective_Instability * M_Affective_Instability)
        PTS_Affective_Instability_1<- (B_Adj_Affective_Instability * Score_Affective_Instability_1) + A_Adj_Affective_Instability
        PTS_Affective_Instability_2<- (B_Adj_Affective_Instability * Score_Affective_Instability_2) + A_Adj_Affective_Instability
        PTS_Affective_Instability_3<- (B_Adj_Affective_Instability * Score_Affective_Instability_3) + A_Adj_Affective_Instability
        PTS_Affective_Instability<- c(PTS_Affective_Instability_1,PTS_Affective_Instability_2, PTS_Affective_Instability_3)
        B_Slope_Abandonment<- Rel_Abandonment * (SD_Retest_Abandonment/SD_Abandonment)
        A_Constant_Abandonment<- M_Retest_Abandonment - (B_Slope_Abandonment * M_Abandonment)
        B_Adj_Abandonment<- SD_Retest_Abandonment/SD_Abandonment
        A_Adj_Abandonment<- M_Retest_Abandonment - (B_Adj_Abandonment * M_Abandonment)
        PTS_Abandonment_1<- (B_Adj_Abandonment * Score_Abandonment_1) + A_Adj_Abandonment
        PTS_Abandonment_2<- (B_Adj_Abandonment * Score_Abandonment_2) + A_Adj_Abandonment
        PTS_Abandonment_3<- (B_Adj_Abandonment * Score_Abandonment_3) + A_Adj_Abandonment
        PTS_Abandonment<- c(PTS_Abandonment_1,PTS_Abandonment_2, PTS_Abandonment_3)
        B_Slope_Relationships<- Rel_Relationships * (SD_Retest_Relationships/SD_Relationships)
        A_Constant_Relationships<- M_Retest_Relationships - (B_Slope_Relationships * M_Relationships)
        B_Adj_Relationships<- SD_Retest_Relationships/SD_Relationships
        A_Adj_Relationships<- M_Retest_Relationships - (B_Adj_Relationships * M_Relationships)
        PTS_Relationships_1<- (B_Adj_Relationships * Score_Relationships_1) + A_Adj_Relationships
        PTS_Relationships_2<- (B_Adj_Relationships * Score_Relationships_2) + A_Adj_Relationships
        PTS_Relationships_3<- (B_Adj_Relationships * Score_Relationships_3) + A_Adj_Relationships
        PTS_Relationships<- c(PTS_Relationships_1,PTS_Relationships_2, PTS_Relationships_3)
        B_Slope_Self_Image<- Rel_Self_Image * (SD_Retest_Self_Image/SD_Self_Image)
        A_Constant_Self_Image<- M_Retest_Self_Image - (B_Slope_Self_Image * M_Self_Image)
        B_Adj_Self_Image<- SD_Retest_Self_Image/SD_Self_Image
        A_Adj_Self_Image<- M_Retest_Self_Image - (B_Adj_Self_Image * M_Self_Image)
        PTS_Self_Image_1<- (B_Adj_Self_Image * Score_Self_Image_1) + A_Adj_Self_Image
        PTS_Self_Image_2<- (B_Adj_Self_Image * Score_Self_Image_2) + A_Adj_Self_Image
        PTS_Self_Image_3<- (B_Adj_Self_Image * Score_Self_Image_3) + A_Adj_Self_Image
        PTS_Self_Image<- c(PTS_Self_Image_1,PTS_Self_Image_2, PTS_Self_Image_3)
        B_Slope_Suicide_Mutilation<- Rel_Suicide_Mutilation * (SD_Retest_Suicide_Mutilation/SD_Suicide_Mutilation)
        A_Constant_Suicide_Mutilation<- M_Retest_Suicide_Mutilation - (B_Slope_Suicide_Mutilation * M_Suicide_Mutilation)
        B_Adj_Suicide_Mutilation<- SD_Retest_Suicide_Mutilation/SD_Suicide_Mutilation
        A_Adj_Suicide_Mutilation<- M_Retest_Suicide_Mutilation - (B_Adj_Suicide_Mutilation * M_Suicide_Mutilation)
        PTS_Suicide_Mutilation_1<- (B_Adj_Suicide_Mutilation * Score_Suicide_Mutilation_1) + A_Adj_Suicide_Mutilation
        PTS_Suicide_Mutilation_2<- (B_Adj_Suicide_Mutilation * Score_Suicide_Mutilation_2) + A_Adj_Suicide_Mutilation
        PTS_Suicide_Mutilation_3<- (B_Adj_Suicide_Mutilation * Score_Suicide_Mutilation_3) + A_Adj_Suicide_Mutilation
        PTS_Suicide_Mutilation<- c(PTS_Suicide_Mutilation_1,PTS_Suicide_Mutilation_2, PTS_Suicide_Mutilation_3)
        B_Slope_Emptiness<- Rel_Emptiness * (SD_Retest_Emptiness/SD_Emptiness)
        A_Constant_Emptiness<- M_Retest_Emptiness - (B_Slope_Emptiness * M_Emptiness)
        B_Adj_Emptiness<- SD_Retest_Emptiness/SD_Emptiness
        A_Adj_Emptiness<- M_Retest_Emptiness - (B_Adj_Emptiness * M_Emptiness)
        PTS_Emptiness_1<- (B_Adj_Emptiness * Score_Emptiness_1) + A_Adj_Emptiness
        PTS_Emptiness_2<- (B_Adj_Emptiness * Score_Emptiness_2) + A_Adj_Emptiness
        PTS_Emptiness_3<- (B_Adj_Emptiness * Score_Emptiness_3) + A_Adj_Emptiness
        PTS_Emptiness<- c(PTS_Emptiness_1,PTS_Emptiness_2, PTS_Emptiness_3)
        B_Slope_Anger<- Rel_Anger * (SD_Retest_Anger/SD_Anger)
        A_Constant_Anger<- M_Retest_Anger - (B_Slope_Anger * M_Anger)
        B_Adj_Anger<- SD_Retest_Anger/SD_Anger
        A_Adj_Anger<- M_Retest_Anger - (B_Adj_Anger * M_Anger)
        PTS_Anger_1<- (B_Adj_Anger * Score_Anger_1) + A_Adj_Anger
        PTS_Anger_2<- (B_Adj_Anger * Score_Anger_2) + A_Adj_Anger
        PTS_Anger_3<- (B_Adj_Anger * Score_Anger_3) + A_Adj_Anger
        PTS_Anger<- c(PTS_Anger_1,PTS_Anger_2, PTS_Anger_3)
        B_Slope_Quasi_Psychotic<- Rel_Quasi_Psychotic * (SD_Retest_Quasi_Psychotic/SD_Quasi_Psychotic)
        A_Constant_Quasi_Psychotic<- M_Retest_Quasi_Psychotic - (B_Slope_Quasi_Psychotic * M_Quasi_Psychotic)
        B_Adj_Quasi_Psychotic<- SD_Retest_Quasi_Psychotic/SD_Quasi_Psychotic
        A_Adj_Quasi_Psychotic<- M_Retest_Quasi_Psychotic - (B_Adj_Quasi_Psychotic * M_Quasi_Psychotic)
        PTS_Quasi_Psychotic_1<- (B_Adj_Quasi_Psychotic * Score_Quasi_Psychotic_1) + A_Adj_Quasi_Psychotic
        PTS_Quasi_Psychotic_2<- (B_Adj_Quasi_Psychotic * Score_Quasi_Psychotic_2) + A_Adj_Quasi_Psychotic
        PTS_Quasi_Psychotic_3<- (B_Adj_Quasi_Psychotic * Score_Quasi_Psychotic_3) + A_Adj_Quasi_Psychotic
        PTS_Quasi_Psychotic<- c(PTS_Quasi_Psychotic_1,PTS_Quasi_Psychotic_2, PTS_Quasi_Psychotic_3)
      } else if (input$RelChangeMethod == "McSweeny et al. (1993)" | input$RelChangeMethod == "Crawford & Howell (1998)") {
        B_Slope<- Rel * (SD_Retest/SD)
        PTS_1<- B_Slope*Score_1
        PTS_2<- B_Slope*Score_2
        PTS_3<- B_Slope*Score_3
        PTS<- c(PTS_1,PTS_2, PTS_3)
        B_Slope_Impulsivity<- Rel_Impulsivity * (SD_Retest_Impulsivity/SD_Impulsivity)
        PTS_Impulsivity_1<- B_Slope_Impulsivity * Score_Impulsivity_1
        PTS_Impulsivity_2<- B_Slope_Impulsivity * Score_Impulsivity_2
        PTS_Impulsivity_3<- B_Slope_Impulsivity * Score_Impulsivity_3
        PTS_Impulsivity<- c(PTS_Impulsivity_1, PTS_Impulsivity_2, PTS_Impulsivity_3)
        B_Slope_Affective_Instability<- Rel_Affective_Instability * (SD_Retest_Affective_Instability/SD_Affective_Instability)
        PTS_Affective_Instability_1<- B_Slope_Affective_Instability * Score_Affective_Instability_1
        PTS_Affective_Instability_2<- B_Slope_Affective_Instability * Score_Affective_Instability_2
        PTS_Affective_Instability_3<- B_Slope_Affective_Instability * Score_Affective_Instability_3
        PTS_Affective_Instability<- c(PTS_Affective_Instability_1, PTS_Affective_Instability_2, PTS_Affective_Instability_3)
        B_Slope_Abandonment<- Rel_Abandonment * (SD_Retest_Abandonment/SD_Abandonment)
        PTS_Abandonment_1<- B_Slope_Abandonment * Score_Abandonment_1
        PTS_Abandonment_2<- B_Slope_Abandonment * Score_Abandonment_2
        PTS_Abandonment_3<- B_Slope_Abandonment * Score_Abandonment_3
        PTS_Abandonment<- c(PTS_Abandonment_1, PTS_Abandonment_2, PTS_Abandonment_3)
        B_Slope_Relationships<- Rel_Relationships * (SD_Retest_Relationships/SD_Relationships)
        PTS_Relationships_1<- B_Slope_Relationships * Score_Relationships_1
        PTS_Relationships_2<- B_Slope_Relationships * Score_Relationships_2
        PTS_Relationships_3<- B_Slope_Relationships * Score_Relationships_3
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2, PTS_Relationships_3)
        B_Slope_Self_Image<- Rel_Self_Image * (SD_Retest_Self_Image/SD_Self_Image)
        PTS_Self_Image_1<- B_Slope_Self_Image * Score_Self_Image_1
        PTS_Self_Image_2<- B_Slope_Self_Image * Score_Self_Image_2
        PTS_Self_Image_3<- B_Slope_Self_Image * Score_Self_Image_3
        PTS_Self_Image<- c(PTS_Self_Image_1, PTS_Self_Image_2, PTS_Self_Image_3)
        B_Slope_Suicide_Mutilation<- Rel_Suicide_Mutilation * (SD_Retest_Suicide_Mutilation/SD_Suicide_Mutilation)
        PTS_Suicide_Mutilation_1<- B_Slope_Suicide_Mutilation * Score_Suicide_Mutilation_1
        PTS_Suicide_Mutilation_2<- B_Slope_Suicide_Mutilation * Score_Suicide_Mutilation_2
        PTS_Suicide_Mutilation_3<- B_Slope_Suicide_Mutilation * Score_Suicide_Mutilation_3
        PTS_Suicide_Mutilation<- c(PTS_Suicide_Mutilation_1, PTS_Suicide_Mutilation_2, PTS_Suicide_Mutilation_3)
        B_Slope_Emptiness<- Rel_Emptiness * (SD_Retest_Emptiness/SD_Emptiness)
        PTS_Emptiness_1<- B_Slope_Emptiness * Score_Emptiness_1
        PTS_Emptiness_2<- B_Slope_Emptiness * Score_Emptiness_2
        PTS_Emptiness_3<- B_Slope_Emptiness * Score_Emptiness_3
        PTS_Emptiness<- c(PTS_Emptiness_1, PTS_Emptiness_2, PTS_Emptiness_3) 
        B_Slope_Anger<- Rel_Anger * (SD_Retest_Anger/SD_Anger)
        PTS_Anger_1<- B_Slope_Anger * Score_Anger_1
        PTS_Anger_2<- B_Slope_Anger * Score_Anger_2
        PTS_Anger_3<- B_Slope_Anger * Score_Anger_3
        PTS_Anger<- c(PTS_Anger_1, PTS_Anger_2, PTS_Anger_3)
        B_Slope_Quasi_Psychotic<- Rel_Quasi_Psychotic * (SD_Retest_Quasi_Psychotic/SD_Quasi_Psychotic)
        PTS_Quasi_Psychotic_1<- B_Slope_Quasi_Psychotic * Score_Quasi_Psychotic_1
        PTS_Quasi_Psychotic_2<- B_Slope_Quasi_Psychotic * Score_Quasi_Psychotic_2
        PTS_Quasi_Psychotic_3<- B_Slope_Quasi_Psychotic * Score_Quasi_Psychotic_3
        PTS_Quasi_Psychotic<- c(PTS_Quasi_Psychotic_1, PTS_Quasi_Psychotic_2, PTS_Quasi_Psychotic_3)
      } else if (input$RelChangeMethod == "Speer (1992)") {
        PTS_1<- Score_1 + (M_Retest - M)
        PTS_2<- Score_2 + (M_Retest - M)
        PTS_3<- Score_3 + (M_Retest - M)
        PTS<- c(PTS_1,PTS_2, PTS_3)
        PTS_Impulsivity_1<- Score_Impulsivity_1 + (M_Retest_Impulsivity - M_Impulsivity)
        PTS_Impulsivity_2<- Score_Impulsivity_2 + (M_Retest_Impulsivity - M_Impulsivity)
        PTS_Impulsivity_3<- Score_Impulsivity_3 + (M_Retest_Impulsivity - M_Impulsivity)
        PTS_Impulsivity<- c(PTS_Impulsivity_1, PTS_Impulsivity_2, PTS_Impulsivity_3)
        PTS_Affective_Instability_1<- Score_Affective_Instability_1 + (M_Retest_Affective_Instability - M_Affective_Instability)
        PTS_Affective_Instability_2<- Score_Affective_Instability_2 + (M_Retest_Affective_Instability - M_Affective_Instability)
        PTS_Affective_Instability_3<- Score_Affective_Instability_3 + (M_Retest_Affective_Instability - M_Affective_Instability)
        PTS_Affective_Instability<- c(PTS_Affective_Instability_1, PTS_Affective_Instability_2, PTS_Affective_Instability_3)
        PTS_Abandonment_1<- Score_Abandonment_1 + (M_Retest_Abandonment - M_Abandonment)
        PTS_Abandonment_2<- Score_Abandonment_2 + (M_Retest_Abandonment - M_Abandonment)
        PTS_Abandonment_3<- Score_Abandonment_3 + (M_Retest_Abandonment - M_Abandonment)
        PTS_Abandonment<- c(PTS_Abandonment_1, PTS_Abandonment_2, PTS_Abandonment_3)
        PTS_Relationships_1<- Score_Relationships_1 + (M_Retest_Relationships - M_Relationships)
        PTS_Relationships_2<- Score_Relationships_2 + (M_Retest_Relationships - M_Relationships)
        PTS_Relationships_3<- Score_Relationships_3 + (M_Retest_Relationships - M_Relationships)
        PTS_Relationships<- c(PTS_Relationships_1, PTS_Relationships_2, PTS_Relationships_3)
        PTS_Self_Image_1<- Score_Self_Image_1 + (M_Retest_Self_Image - M_Self_Image)
        PTS_Self_Image_2<- Score_Self_Image_2 + (M_Retest_Self_Image - M_Self_Image)
        PTS_Self_Image_3<- Score_Self_Image_3 + (M_Retest_Self_Image - M_Self_Image)
        PTS_Self_Image<- c(PTS_Self_Image_1, PTS_Self_Image_2, PTS_Self_Image_3)
        PTS_Suicide_Mutilation_1<- Score_Suicide_Mutilation_1 + (M_Retest_Suicide_Mutilation - M_Suicide_Mutilation)
        PTS_Suicide_Mutilation_2<- Score_Suicide_Mutilation_2 + (M_Retest_Suicide_Mutilation - M_Suicide_Mutilation)
        PTS_Suicide_Mutilation_3<- Score_Suicide_Mutilation_3 + (M_Retest_Suicide_Mutilation - M_Suicide_Mutilation)
        PTS_Suicide_Mutilation<- c(PTS_Suicide_Mutilation_1, PTS_Suicide_Mutilation_2, PTS_Suicide_Mutilation_3)
        PTS_Emptiness_1<- Score_Emptiness_1 + (M_Retest_Emptiness - M_Emptiness)
        PTS_Emptiness_2<- Score_Emptiness_2 + (M_Retest_Emptiness - M_Emptiness)
        PTS_Emptiness_3<- Score_Emptiness_3 + (M_Retest_Emptiness - M_Emptiness)
        PTS_Emptiness<- c(PTS_Emptiness_1, PTS_Emptiness_2, PTS_Emptiness_3)
        PTS_Anger_1<- Score_Anger_1 + (M_Retest_Anger - M_Anger)
        PTS_Anger_2<- Score_Anger_2 + (M_Retest_Anger - M_Anger)
        PTS_Anger_3<- Score_Anger_3 + (M_Retest_Anger - M_Anger)
        PTS_Anger<- c(PTS_Anger_1, PTS_Anger_2, PTS_Anger_3)
        PTS_Quasi_Psychotic_1<- Score_Quasi_Psychotic_1 + (M_Retest_Quasi_Psychotic - M_Quasi_Psychotic)
        PTS_Quasi_Psychotic_2<- Score_Quasi_Psychotic_2 + (M_Retest_Quasi_Psychotic - M_Quasi_Psychotic)
        PTS_Quasi_Psychotic_3<- Score_Quasi_Psychotic_3 + (M_Retest_Quasi_Psychotic - M_Quasi_Psychotic)
        PTS_Quasi_Psychotic<- c(PTS_Quasi_Psychotic_1, PTS_Quasi_Psychotic_2, PTS_Quasi_Psychotic_3)
      }
      PTS<- round(PTS, digits = 2)
      PTS_Impulsivity<- round(PTS_Impulsivity, digits = 2)
      PTS_Affective_Instability<- round(PTS_Affective_Instability, digits = 2)
      PTS_Abandonment<- round(PTS_Abandonment, digits = 2)
      PTS_Relationships<- round(PTS_Relationships, digits = 2)
      PTS_Self_Image<- round(PTS_Self_Image, digits = 2)
      PTS_Suicide_Mutilation<- round(PTS_Suicide_Mutilation, digits = 2)
      PTS_Emptiness<- round(PTS_Emptiness, digits = 2)
      PTS_Anger<- round(PTS_Anger, digits = 2)
      PTS_Quasi_Psychotic<- round(PTS_Quasi_Psychotic, digits = 2)
      if(input$RelChangeMethod == "Crawford & Howell (1998)") {
        SE1<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_1 - M)^2/(SD^2*(SampleN-1))))
        SE2<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_2 - M)^2/(SD^2*(SampleN-1))))
        SE3<- McSweeny_SE*sqrt(1 + (1/SampleN) + ((Score_3 - M)^2/(SD^2*(SampleN-1))))
        SE<- c(SE1, SE2, SE3)
        SE_Impulsivity_1<- McSweeny_SE_Impulsivity*sqrt(1 + (1/SampleN) + ((Score_Impulsivity_1 - M_Impulsivity)^2/(SD_Impulsivity^2*(SampleN-1))))
        SE_Impulsivity_2<- McSweeny_SE_Impulsivity*sqrt(1 + (1/SampleN) + ((Score_Impulsivity_2 - M_Impulsivity)^2/(SD_Impulsivity^2*(SampleN-1))))
        SE_Impulsivity_3<- McSweeny_SE_Impulsivity*sqrt(1 + (1/SampleN) + ((Score_Impulsivity_3 - M_Impulsivity)^2/(SD_Impulsivity^2*(SampleN-1))))
        SE_Impulsivity<-c(SE_Impulsivity_1, SE_Impulsivity_2, SE_Impulsivity_3)
        SE_Affective_Instability_1<- McSweeny_SE_Affective_Instability*sqrt(1 + (1/SampleN) + ((Score_Affective_Instability_1 - M_Affective_Instability)^2/(SD_Affective_Instability^2*(SampleN-1))))
        SE_Affective_Instability_2<- McSweeny_SE_Affective_Instability*sqrt(1 + (1/SampleN) + ((Score_Affective_Instability_2 - M_Affective_Instability)^2/(SD_Affective_Instability^2*(SampleN-1))))
        SE_Affective_Instability_3<- McSweeny_SE_Affective_Instability*sqrt(1 + (1/SampleN) + ((Score_Affective_Instability_3 - M_Affective_Instability)^2/(SD_Affective_Instability^2*(SampleN-1))))
        SE_Affective_Instability<-c(SE_Affective_Instability_1, SE_Affective_Instability_2, SE_Affective_Instability_3)
        SE_Abandonment_1<- McSweeny_SE_Abandonment*sqrt(1 + (1/SampleN) + ((Score_Abandonment_1 - M_Abandonment)^2/(SD_Abandonment^2*(SampleN-1))))
        SE_Abandonment_2<- McSweeny_SE_Abandonment*sqrt(1 + (1/SampleN) + ((Score_Abandonment_2 - M_Abandonment)^2/(SD_Abandonment^2*(SampleN-1))))
        SE_Abandonment_3<- McSweeny_SE_Abandonment*sqrt(1 + (1/SampleN) + ((Score_Abandonment_3 - M_Abandonment)^2/(SD_Abandonment^2*(SampleN-1))))
        SE_Abandonment<-c(SE_Abandonment_1, SE_Abandonment_2, SE_Abandonment_3)
        SE_Relationships_1<- McSweeny_SE_Relationships*sqrt(1 + (1/SampleN) + ((Score_Relationships_1 - M_Relationships)^2/(SD_Relationships^2*(SampleN-1))))
        SE_Relationships_2<- McSweeny_SE_Relationships*sqrt(1 + (1/SampleN) + ((Score_Relationships_2 - M_Relationships)^2/(SD_Relationships^2*(SampleN-1))))
        SE_Relationships_3<- McSweeny_SE_Relationships*sqrt(1 + (1/SampleN) + ((Score_Relationships_3 - M_Relationships)^2/(SD_Relationships^2*(SampleN-1))))
        SE_Relationships<-c(SE_Relationships_1, SE_Relationships_2, SE_Relationships_3)
        SE_Self_Image_1<- McSweeny_SE_Self_Image*sqrt(1 + (1/SampleN) + ((Score_Self_Image_1 - M_Self_Image)^2/(SD_Self_Image^2*(SampleN-1))))
        SE_Self_Image_2<- McSweeny_SE_Self_Image*sqrt(1 + (1/SampleN) + ((Score_Self_Image_2 - M_Self_Image)^2/(SD_Self_Image^2*(SampleN-1))))
        SE_Self_Image_3<- McSweeny_SE_Self_Image*sqrt(1 + (1/SampleN) + ((Score_Self_Image_3 - M_Self_Image)^2/(SD_Self_Image^2*(SampleN-1))))
        SE_Self_Image<-c(SE_Self_Image_1, SE_Self_Image_2, SE_Self_Image_3)
        SE_Suicide_Mutilation_1<- McSweeny_SE_Suicide_Mutilation*sqrt(1 + (1/SampleN) + ((Score_Suicide_Mutilation_1 - M_Suicide_Mutilation)^2/(SD_Suicide_Mutilation^2*(SampleN-1))))
        SE_Suicide_Mutilation_2<- McSweeny_SE_Suicide_Mutilation*sqrt(1 + (1/SampleN) + ((Score_Suicide_Mutilation_2 - M_Suicide_Mutilation)^2/(SD_Suicide_Mutilation^2*(SampleN-1))))
        SE_Suicide_Mutilation_3<- McSweeny_SE_Suicide_Mutilation*sqrt(1 + (1/SampleN) + ((Score_Suicide_Mutilation_3 - M_Suicide_Mutilation)^2/(SD_Suicide_Mutilation^2*(SampleN-1))))
        SE_Suicide_Mutilation<-c(SE_Suicide_Mutilation_1, SE_Suicide_Mutilation_2, SE_Suicide_Mutilation_3)
        SE_Emptiness_1<- McSweeny_SE_Emptiness*sqrt(1 + (1/SampleN) + ((Score_Emptiness_1 - M_Emptiness)^2/(SD_Emptiness^2*(SampleN-1))))
        SE_Emptiness_2<- McSweeny_SE_Emptiness*sqrt(1 + (1/SampleN) + ((Score_Emptiness_2 - M_Emptiness)^2/(SD_Emptiness^2*(SampleN-1))))
        SE_Emptiness_3<- McSweeny_SE_Emptiness*sqrt(1 + (1/SampleN) + ((Score_Emptiness_3 - M_Emptiness)^2/(SD_Emptiness^2*(SampleN-1))))
        SE_Emptiness<-c(SE_Emptiness_1, SE_Emptiness_2, SE_Emptiness_3)
        SE_Anger_1<- McSweeny_SE_Anger*sqrt(1 + (1/SampleN) + ((Score_Anger_1 - M_Anger)^2/(SD_Anger^2*(SampleN-1))))
        SE_Anger_2<- McSweeny_SE_Anger*sqrt(1 + (1/SampleN) + ((Score_Anger_2 - M_Anger)^2/(SD_Anger^2*(SampleN-1))))
        SE_Anger_3<- McSweeny_SE_Anger*sqrt(1 + (1/SampleN) + ((Score_Anger_3 - M_Anger)^2/(SD_Anger^2*(SampleN-1))))
        SE_Anger<-c(SE_Anger_1, SE_Anger_2, SE_Anger_3)
        SE_Quasi_Psychotic_1<- McSweeny_SE_Quasi_Psychotic*sqrt(1 + (1/SampleN) + ((Score_Quasi_Psychotic_1 - M_Quasi_Psychotic)^2/(SD_Quasi_Psychotic^2*(SampleN-1))))
        SE_Quasi_Psychotic_2<- McSweeny_SE_Quasi_Psychotic*sqrt(1 + (1/SampleN) + ((Score_Quasi_Psychotic_2 - M_Quasi_Psychotic)^2/(SD_Quasi_Psychotic^2*(SampleN-1))))
        SE_Quasi_Psychotic_3<- McSweeny_SE_Quasi_Psychotic*sqrt(1 + (1/SampleN) + ((Score_Quasi_Psychotic_3 - M_Quasi_Psychotic)^2/(SD_Quasi_Psychotic^2*(SampleN-1))))
        SE_Quasi_Psychotic<-c(SE_Quasi_Psychotic_1, SE_Quasi_Psychotic_2, SE_Quasi_Psychotic_3)
        SE<- round(SE, digits = 2)
        SE_Impulsivity<- round(SE_Impulsivity, digits = 2)
        SE_Affective_Instability<- round(SE_Affective_Instability, digits = 2)
        SE_Abandonment<- round(SE_Abandonment, digits = 2)
        SE_Relationships<- round(SE_Relationships, digits = 2)
        SE_Self_Image<- round(SE_Self_Image, digits = 2)
        SE_Suicide_Mutilation<- round(SE_Suicide_Mutilation, digits = 2)
        SE_Emptiness<- round(SE_Emptiness, digits = 2)
        SE_Anger<- round(SE_Anger, digits = 2)
        SE_Quasi_Psychotic<- round(SE_Quasi_Psychotic, digits = 2)
        CI<- c((Conf*SE1), (Conf*SE2), (Conf*SE3))
        CI<- round(CI, digits = 2)
        CI_Impulsivity<- c((Conf*SE_Impulsivity_1), (Conf*SE_Impulsivity_2), (Conf*SE_Impulsivity_3))
        CI_Impulsivity<- round(CI_Impulsivity, digits = 2)
        CI_Affective_Instability<- c((Conf*SE_Affective_Instability_1), (Conf*SE_Affective_Instability_2), (Conf*SE_Affective_Instability_3))
        CI_Affective_Instability<- round(CI_Affective_Instability, digits = 2)
        CI_Abandonment<- c((Conf*SE_Abandonment_1), (Conf*SE_Abandonment_2), (Conf*SE_Abandonment_3))
        CI_Abandonment<- round(CI_Abandonment, digits = 2)
        CI_Relationships<- c((Conf*SE_Relationships_1), (Conf*SE_Relationships_2), (Conf*SE_Relationships_3))
        CI_Relationships<- round(CI_Relationships, digits = 2)
        CI_Self_Image<- c((Conf*SE_Self_Image_1), (Conf*SE_Self_Image_2), (Conf*SE_Self_Image_3))
        CI_Self_Image<- round(CI_Self_Image, digits = 2)
        CI_Suicide_Mutilation<- c((Conf*SE_Suicide_Mutilation_1), (Conf*SE_Suicide_Mutilation_2), (Conf*SE_Suicide_Mutilation_3))
        CI_Suicide_Mutilation<- round(CI_Suicide_Mutilation, digits = 2)
        CI_Emptiness<- c((Conf*SE_Emptiness_1), (Conf*SE_Emptiness_2), (Conf*SE_Emptiness_3))
        CI_Emptiness<- round(CI_Emptiness, digits = 2)
        CI_Anger<- c((Conf*SE_Anger_1), (Conf*SE_Anger_2), (Conf*SE_Anger_3))
        CI_Anger<- round(CI_Anger, digits = 2)
        CI_Quasi_Psychotic<- c((Conf*SE_Quasi_Psychotic_1), (Conf*SE_Quasi_Psychotic_2), (Conf*SE_Quasi_Psychotic_3))
        CI_Quasi_Psychotic<- round(CI_Quasi_Psychotic, digits = 2)
      }
      if(input$RelChangeMethod != "Crawford & Howell (1998)") {
        CI<- c((Conf*SE), (Conf*SE), (Conf*SE))
        CI<- round(CI, digits = 2)
        CI_Impulsivity<- c((Conf*SE_Impulsivity), (Conf*SE_Impulsivity), (Conf*SE_Impulsivity))
        CI_Impulsivity<- round(CI_Impulsivity, digits = 2)
        CI_Affective_Instability<- c((Conf*SE_Affective_Instability), (Conf*SE_Affective_Instability), (Conf*SE_Affective_Instability))
        CI_Affective_Instability<- round(CI_Affective_Instability, digits = 2)
        CI_Abandonment<- c((Conf*SE_Abandonment), (Conf*SE_Abandonment), (Conf*SE_Abandonment))
        CI_Abandonment<- round(CI_Abandonment, digits = 2)
        CI_Relationships<- c((Conf*SE_Relationships), (Conf*SE_Relationships), (Conf*SE_Relationships))
        CI_Relationships<- round(CI_Relationships, digits = 2)
        CI_Self_Image<- c((Conf*SE_Self_Image), (Conf*SE_Self_Image), (Conf*SE_Self_Image))
        CI_Self_Image<- round(CI_Self_Image, digits = 2)
        CI_Suicide_Mutilation<- c((Conf*SE_Suicide_Mutilation), (Conf*SE_Suicide_Mutilation), (Conf*SE_Suicide_Mutilation))
        CI_Suicide_Mutilation<- round(CI_Suicide_Mutilation, digits = 2)
        CI_Emptiness<- c((Conf*SE_Emptiness), (Conf*SE_Emptiness), (Conf*SE_Emptiness))
        CI_Emptiness<- round(CI_Emptiness, digits = 2)
        CI_Anger<- c((Conf*SE_Anger), (Conf*SE_Anger))
        CI_Anger<- round(CI_Anger, digits = 2)
        CI_Quasi_Psychotic<- c((Conf*SE_Quasi_Psychotic), (Conf*SE_Quasi_Psychotic), (Conf*SE_Quasi_Psychotic))
        CI_Quasi_Psychotic<- round(CI_Quasi_Psychotic, digits = 2)
      }
      CI_Upper_Lim<- PTS + CI
      CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
      CI_Lower_Lim<- PTS - CI
      CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      CI_Upper_Lim_Impulsivity<- PTS_Impulsivity + CI_Impulsivity
      CI_Upper_Lim_Impulsivity<- round(CI_Upper_Lim_Impulsivity, digits = 2)
      CI_Lower_Lim_Impulsivity<-PTS_Impulsivity - CI_Impulsivity
      CI_Lower_Lim_Impulsivity<- round(CI_Lower_Lim_Impulsivity, digits = 2)
      CI_Upper_Lim_Affective_Instability<- PTS_Affective_Instability + CI_Affective_Instability
      CI_Upper_Lim_Affective_Instability<- round(CI_Upper_Lim_Affective_Instability, digits = 2)
      CI_Lower_Lim_Affective_Instability<-PTS_Affective_Instability - CI_Affective_Instability
      CI_Lower_Lim_Affective_Instability<- round(CI_Lower_Lim_Affective_Instability, digits = 2)
      CI_Upper_Lim_Abandonment<- PTS_Abandonment + CI_Abandonment
      CI_Upper_Lim_Abandonment<- round(CI_Upper_Lim_Abandonment, digits = 2)
      CI_Lower_Lim_Abandonment<-PTS_Abandonment - CI_Abandonment
      CI_Lower_Lim_Abandonment<- round(CI_Lower_Lim_Abandonment, digits = 2)
      CI_Upper_Lim_Relationships<- PTS_Relationships + CI_Relationships
      CI_Upper_Lim_Relationships<- round(CI_Upper_Lim_Relationships, digits = 2)
      CI_Lower_Lim_Relationships<-PTS_Relationships - CI_Relationships
      CI_Lower_Lim_Relationships<- round(CI_Lower_Lim_Relationships, digits = 2)
      CI_Upper_Lim_Self_Image<- PTS_Self_Image + CI_Self_Image
      CI_Upper_Lim_Self_Image<- round(CI_Upper_Lim_Self_Image, digits = 2)
      CI_Lower_Lim_Self_Image<-PTS_Self_Image - CI_Self_Image
      CI_Lower_Lim_Self_Image<- round(CI_Lower_Lim_Self_Image, digits = 2)
      CI_Upper_Lim_Suicide_Mutilation<- PTS_Suicide_Mutilation + CI_Suicide_Mutilation
      CI_Upper_Lim_Suicide_Mutilation<- round(CI_Upper_Lim_Suicide_Mutilation, digits = 2)
      CI_Lower_Lim_Suicide_Mutilation<-PTS_Suicide_Mutilation - CI_Suicide_Mutilation
      CI_Lower_Lim_Suicide_Mutilation<- round(CI_Lower_Lim_Suicide_Mutilation, digits = 2)
      CI_Upper_Lim_Emptiness<- PTS_Emptiness + CI_Emptiness
      CI_Upper_Lim_Emptiness<- round(CI_Upper_Lim_Emptiness, digits = 2)
      CI_Lower_Lim_Emptiness<-PTS_Emptiness - CI_Emptiness
      CI_Lower_Lim_Emptiness<- round(CI_Lower_Lim_Emptiness, digits = 2)
      CI_Upper_Lim_Anger<- PTS_Anger + CI_Anger
      CI_Upper_Lim_Anger<- round(CI_Upper_Lim_Anger, digits = 2)
      CI_Lower_Lim_Anger<-PTS_Anger - CI_Anger
      CI_Lower_Lim_Anger<- round(CI_Lower_Lim_Anger, digits = 2)
      CI_Upper_Lim_Quasi_Psychotic<- PTS_Quasi_Psychotic + CI_Quasi_Psychotic
      CI_Upper_Lim_Quasi_Psychotic<- round(CI_Upper_Lim_Quasi_Psychotic, digits = 2)
      CI_Lower_Lim_Quasi_Psychotic<-PTS_Quasi_Psychotic - CI_Quasi_Psychotic
      CI_Lower_Lim_Quasi_Psychotic<- round(CI_Lower_Lim_Quasi_Psychotic, digits = 2)
      
      if(input$Select_CI == "2") {
        Manual_CI<- input$Man_CI
        CI<- c(Manual_CI, Manual_CI, Manual_CI)
        CI<- round(CI, digits = 2)
        CI_Upper_Lim<- Score + CI
        CI_Upper_Lim<- round(CI_Upper_Lim, digits = 2)
        CI_Lower_Lim<- Score - CI
        CI_Lower_Lim<- round(CI_Lower_Lim, digits = 2)
      }
      if(input$Select_CI_Impulsivity == "2") {
        CI_Impulsivity<- input$Man_CI_Impulsivity
        CI_Impulsivity<- c(CI_Impulsivity, CI_Impulsivity, CI_Impulsivity)
        CI_Impulsivity<- round(CI_Impulsivity, digits = 2)
        CI_Upper_Lim_Impulsivity<- Score_Impulsivity + CI_Impulsivity
        CI_Upper_Lim_Impulsivity<- round(CI_Upper_Lim_Impulsivity, digits = 2)
        CI_Lower_Lim_Impulsivity<- Score_Impulsivity - CI_Impulsivity
        CI_Lower_Lim_Impulsivity<- round(CI_Lower_Lim_Impulsivity, digits = 2)
      }
      if(input$Select_CI_Affective_Instability == "2") {
        CI_Affective_Instability<- input$Man_CI_Affective_Instability
        CI_Affective_Instability<- c(CI_Affective_Instability, CI_Affective_Instability, CI_Affective_Instability)
        CI_Affective_Instability<- round(CI_Affective_Instability, digits = 2)
        CI_Upper_Lim_Affective_Instability<- Score_Affective_Instability + CI_Affective_Instability
        CI_Upper_Lim_Affective_Instability<- round(CI_Upper_Lim_Affective_Instability, digits = 2)
        CI_Lower_Lim_Affective_Instability<- Score_Affective_Instability - CI_Affective_Instability
        CI_Lower_Lim_Affective_Instability<- round(CI_Lower_Lim_Affective_Instability, digits = 2)
      }
      if(input$Select_CI_Abandonment == "2") {
        CI_Abandonment<- input$Man_CI_Abandonment
        CI_Abandonment<- c(CI_Abandonment, CI_Abandonment, CI_Abandonment)
        CI_Abandonment<- round(CI_Abandonment, digits = 2)
        CI_Upper_Lim_Abandonment<- Score_Abandonment + CI_Abandonment
        CI_Upper_Lim_Abandonment<- round(CI_Upper_Lim_Abandonment, digits = 2)
        CI_Lower_Lim_Abandonment<- Score_Abandonment - CI_Abandonment
        CI_Lower_Lim_Abandonment<- round(CI_Lower_Lim_Abandonment, digits = 2)
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
      if(input$Select_CI_Self_Image == "2") {
        CI_Self_Image<- input$Man_CI_Self_Image
        CI_Self_Image<- c(CI_Self_Image,  CI_Self_Image, CI_Self_Image)
        CI_Self_Image<- round(CI_Self_Image, digits = 2)
        CI_Upper_Lim_Self_Image<- Score_Self_Image + CI_Self_Image
        CI_Upper_Lim_Self_Image<- round(CI_Upper_Lim_Self_Image, digits = 2)
        CI_Lower_Lim_Self_Image<- Score_Self_Image - CI_Self_Image
        CI_Lower_Lim_Self_Image<- round(CI_Lower_Lim_Self_Image, digits = 2)
      }
      if(input$Select_CI_Suicide_Mutilation == "2") {
        CI_Suicide_Mutilation<- input$Man_CI_Suicide_Mutilation
        CI_Suicide_Mutilation<- c(CI_Suicide_Mutilation, CI_Suicide_Mutilation, CI_Suicide_Mutilation)
        CI_Suicide_Mutilation<- round(CI_Suicide_Mutilation, digits = 2)
        CI_Upper_Lim_Suicide_Mutilation<- Score_Suicide_Mutilation + CI_Suicide_Mutilation
        CI_Upper_Lim_Suicide_Mutilation<- round(CI_Upper_Lim_Suicide_Mutilation, digits = 2)
        CI_Lower_Lim_Suicide_Mutilation<- Score_Suicide_Mutilation - CI_Suicide_Mutilation
        CI_Lower_Lim_Suicide_Mutilation<- round(CI_Lower_Lim_Suicide_Mutilation, digits = 2)
      }
      if(input$Select_CI_Emptiness == "2") {
        CI_Emptiness<- input$Man_CI_Emptiness
        CI_Emptiness<- c(CI_Emptiness, CI_Emptiness, CI_Emptiness)
        CI_Emptiness<- round(CI_Emptiness, digits = 2)
        CI_Upper_Lim_Emptiness<- Score_Emptiness + CI_Emptiness
        CI_Upper_Lim_Emptiness<- round(CI_Upper_Lim_Emptiness, digits = 2)
        CI_Lower_Lim_Emptiness<- Score_Emptiness - CI_Emptiness
        CI_Lower_Lim_Emptiness<- round(CI_Lower_Lim_Emptiness, digits = 2)
      }
      if(input$Select_CI_Anger == "2") {
        CI_Anger<- input$Man_CI_Anger
        CI_Anger<- round(CI_Anger, digits = 2)
        CI_Anger<- c(CI_Anger,  CI_Anger, CI_Anger)
        CI_Upper_Lim_Anger<- Score_Anger + CI_Anger
        CI_Upper_Lim_Anger<- round(CI_Upper_Lim_Anger, digits = 2)
        CI_Lower_Lim_Anger<- Score_Anger - CI_Anger
        CI_Lower_Lim_Anger<- round(CI_Lower_Lim_Anger, digits = 2)
      }
      if(input$Select_CI_Quasi_Psychotic == "2") {
        CI_Quasi_Psychotic<- input$Man_CI_Quasi_Psychotic
        CI_Quasi_Psychotic<- c(CI_Quasi_Psychotic, CI_Quasi_Psychotic, CI_Quasi_Psychotic)
        CI_Quasi_Psychotic<- round(CI_Quasi_Psychotic, digits = 2)
        CI_Upper_Lim_Quasi_Psychotic<- Score_Quasi_Psychotic + CI_Quasi_Psychotic
        CI_Upper_Lim_Quasi_Psychotic<- round(CI_Upper_Lim_Quasi_Psychotic, digits = 2)
        CI_Lower_Lim_Quasi_Psychotic<- Score_Quasi_Psychotic - CI_Quasi_Psychotic
        CI_Lower_Lim_Quasi_Psychotic<- round(CI_Lower_Lim_Quasi_Psychotic, digits = 2)
      }
      Cutoff_Score_1<- round(input$Cutoff_1, digits = 2)
      Cutoff_Score_2<- round(input$Cutoff_2, digits = 2)
      Cutoff_Score_3<- round(input$Cutoff_3, digits = 2)
      Cutoff_Score_Impulsivity_1<- round(input$Cutoff_Impulsivity_1, digits = 2)
      Cutoff_Score_Impulsivity_2<- round(input$Cutoff_Impulsivity_2, digits = 2)
      Cutoff_Score_Impulsivity_3<- round(input$Cutoff_Impulsivity_3, digits = 2)
      Cutoff_Score_Affective_Instability_1<- round(input$Cutoff_Affective_Instability_1, digits = 2)
      Cutoff_Score_Affective_Instability_2<- round(input$Cutoff_Affective_Instability_2, digits = 2)
      Cutoff_Score_Affective_Instability_3<- round(input$Cutoff_Affective_Instability_3, digits = 2)
      Cutoff_Score_Abandonment_1<- round(input$Cutoff_Abandonment_1, digits = 2)
      Cutoff_Score_Abandonment_2<- round(input$Cutoff_Abandonment_2, digits = 2)
      Cutoff_Score_Abandonment_3<- round(input$Cutoff_Abandonment_3, digits = 2)
      Cutoff_Score_Relationships_1<- round(input$Cutoff_Relationships_1, digits = 2)
      Cutoff_Score_Relationships_2<- round(input$Cutoff_Relationships_2, digits = 2)
      Cutoff_Score_Relationships_3<- round(input$Cutoff_Relationships_3, digits = 2)
      Cutoff_Score_Self_Image_1<- round(input$Cutoff_Self_Image_1, digits = 2)
      Cutoff_Score_Self_Image_2<- round(input$Cutoff_Self_Image_2, digits = 2)
      Cutoff_Score_Self_Image_3<- round(input$Cutoff_Self_Image_3, digits = 2)
      Cutoff_Score_Suicide_Mutilation_1<- round(input$Cutoff_Suicide_Mutilation_1, digits = 2)
      Cutoff_Score_Suicide_Mutilation_2<- round(input$Cutoff_Suicide_Mutilation_2, digits = 2)
      Cutoff_Score_Suicide_Mutilation_3<- round(input$Cutoff_Suicide_Mutilation_3, digits = 2)
      Cutoff_Score_Emptiness_1<- round(input$Cutoff_Emptiness_1, digits = 2)
      Cutoff_Score_Emptiness_2<- round(input$Cutoff_Emptiness_2, digits = 2)
      Cutoff_Score_Emptiness_3<- round(input$Cutoff_Emptiness_3, digits = 2)
      Cutoff_Score_Anger_1<- round(input$Cutoff_Anger_1, digits = 2)
      Cutoff_Score_Anger_2<- round(input$Cutoff_Anger_2, digits = 2)
      Cutoff_Score_Anger_3<- round(input$Cutoff_Anger_3, digits = 2)
      Cutoff_Score_Quasi_Psychotic_1<- round(input$Cutoff_Quasi_Psychotic_1, digits = 2)
      Cutoff_Score_Quasi_Psychotic_2<- round(input$Cutoff_Quasi_Psychotic_2, digits = 2)
      Cutoff_Score_Quasi_Psychotic_3<- round(input$Cutoff_Quasi_Psychotic_3, digits = 2)
      Entered_Scores_Df<<- data.frame(Date, Score, Change, PTS, SE, CI_Upper_Lim, CI_Lower_Lim, Cutoff_Score_1, Cutoff_Score_2,Cutoff_Score_3,Score_Impulsivity,Change_Impulsivity,PTS_Impulsivity, SE_Impulsivity, CI_Upper_Lim_Impulsivity, CI_Lower_Lim_Impulsivity, Cutoff_Score_Impulsivity_1,Cutoff_Score_Impulsivity_2,Cutoff_Score_Impulsivity_3,
                                      Score_Affective_Instability,Change_Affective_Instability, PTS_Affective_Instability, SE_Affective_Instability, CI_Upper_Lim_Affective_Instability, CI_Lower_Lim_Affective_Instability, Cutoff_Score_Affective_Instability_1,Cutoff_Score_Affective_Instability_2,Cutoff_Score_Affective_Instability_3, 
                                      Score_Abandonment,Change_Abandonment,PTS_Abandonment, SE_Abandonment, CI_Upper_Lim_Abandonment, CI_Lower_Lim_Abandonment, Cutoff_Score_Abandonment_1,Cutoff_Score_Abandonment_2,Cutoff_Score_Abandonment_3, 
                                      Score_Relationships,Change_Relationships,PTS_Relationships, SE_Relationships, CI_Upper_Lim_Relationships, CI_Lower_Lim_Relationships, Cutoff_Score_Relationships_1,Cutoff_Score_Relationships_2,Cutoff_Score_Relationships_3, 
                                      Score_Self_Image,Change_Self_Image,PTS_Self_Image, SE_Self_Image, CI_Upper_Lim_Self_Image, CI_Lower_Lim_Self_Image, Cutoff_Score_Self_Image_1,Cutoff_Score_Self_Image_2,Cutoff_Score_Self_Image_3,
                                      Score_Suicide_Mutilation,Change_Suicide_Mutilation, PTS_Suicide_Mutilation, SE_Suicide_Mutilation, CI_Upper_Lim_Suicide_Mutilation, CI_Lower_Lim_Suicide_Mutilation, Cutoff_Score_Suicide_Mutilation_1,Cutoff_Score_Suicide_Mutilation_2,Cutoff_Score_Suicide_Mutilation_3, 
                                      Score_Emptiness,Change_Emptiness,PTS_Emptiness, SE_Emptiness, CI_Upper_Lim_Emptiness, CI_Lower_Lim_Emptiness, Cutoff_Score_Emptiness_1,Cutoff_Score_Emptiness_2,Cutoff_Score_Emptiness_3, 
                                      Score_Anger,Change_Anger,PTS_Anger, SE_Anger, CI_Upper_Lim_Anger, CI_Lower_Lim_Anger, Cutoff_Score_Anger_1,Cutoff_Score_Anger_2,Cutoff_Score_Anger_3, 
                                      Score_Quasi_Psychotic,Change_Quasi_Psychotic,PTS_Quasi_Psychotic, SE_Quasi_Psychotic, CI_Upper_Lim_Quasi_Psychotic, CI_Lower_Lim_Quasi_Psychotic, Cutoff_Score_Quasi_Psychotic_1,Cutoff_Score_Quasi_Psychotic_2,Cutoff_Score_Quasi_Psychotic_3)
    }
    
    
    #Create dataframes to be displayed as tables in pdf report (statistical values used in reliable change analyses)
    
    if(input$RelChangeMethod == 'Nunnally & Bernstein (1994)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Impulsivity<<- data.frame(Pop,  M_Impulsivity, SD_Impulsivity, RelChangeMethod, Rel_Impulsivity, ConfInt)
      names(Stats_Table_Impulsivity)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Affective_Instability<<- data.frame(Pop,  M_Affective_Instability, SD_Affective_Instability, RelChangeMethod, Rel_Affective_Instability, ConfInt)
      names(Stats_Table_Affective_Instability)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Abandonment<<- data.frame(Pop,  M_Abandonment, SD_Abandonment, RelChangeMethod, Rel_Abandonment, ConfInt)
      names(Stats_Table_Abandonment)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Relationships<<- data.frame(Pop,  M_Relationships, SD_Relationships, RelChangeMethod, Rel_Relationships, ConfInt)
      names(Stats_Table_Relationships)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Self_Image<<- data.frame(Pop,  M_Self_Image, SD_Self_Image, RelChangeMethod, Rel_Self_Image, ConfInt)
      names(Stats_Table_Self_Image)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Suicide_Mutilation<<- data.frame(Pop,  M_Suicide_Mutilation, SD_Suicide_Mutilation, RelChangeMethod, Rel_Suicide_Mutilation, ConfInt)
      names(Stats_Table_Suicide_Mutilation)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Emptiness<<- data.frame(Pop,  M_Emptiness, SD_Emptiness, RelChangeMethod, Rel_Emptiness, ConfInt)
      names(Stats_Table_Emptiness)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Anger<<- data.frame(Pop,  M_Anger, SD_Anger, RelChangeMethod, Rel_Anger, ConfInt)
      names(Stats_Table_Anger)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Quasi_Psychotic<<- data.frame(Pop,  M_Quasi_Psychotic, SD_Quasi_Psychotic, RelChangeMethod, Rel_Quasi_Psychotic, ConfInt)
      names(Stats_Table_Quasi_Psychotic)<<- c("Reference Population","M", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Chelune et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Impulsivity<<- data.frame(Pop,  M_Impulsivity, M_Retest_Impulsivity, SD_Impulsivity, RelChangeMethod, Rel_Impulsivity, ConfInt)
      names(Stats_Table_Impulsivity)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Affective_Instability<<- data.frame(Pop,  M_Affective_Instability, M_Retest_Affective_Instability, SD_Affective_Instability, RelChangeMethod, Rel_Affective_Instability, ConfInt)
      names(Stats_Table_Affective_Instability)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Abandonment<<- data.frame(Pop,  M_Abandonment, M_Retest_Abandonment, SD_Abandonment, RelChangeMethod, Rel_Abandonment, ConfInt)
      names(Stats_Table_Abandonment)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Relationships<<- data.frame(Pop,  M_Relationships, M_Retest_Relationships, SD_Relationships, RelChangeMethod, Rel_Relationships, ConfInt)
      names(Stats_Table_Relationships)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      
      Stats_Table_Self_Image<<- data.frame(Pop,  M_Self_Image, M_Retest_Self_Image, SD_Self_Image, RelChangeMethod, Rel_Self_Image, ConfInt)
      names(Stats_Table_Self_Image)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Suicide_Mutilation<<- data.frame(Pop,  M_Suicide_Mutilation, M_Retest_Suicide_Mutilation, SD_Suicide_Mutilation, RelChangeMethod, Rel_Suicide_Mutilation, ConfInt)
      names(Stats_Table_Suicide_Mutilation)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Emptiness<<- data.frame(Pop,  M_Emptiness, M_Retest_Emptiness, SD_Emptiness, RelChangeMethod, Rel_Emptiness, ConfInt)
      names(Stats_Table_Emptiness)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Anger<<- data.frame(Pop,  M_Anger, M_Retest_Anger, SD_Anger, RelChangeMethod, Rel_Anger, ConfInt)
      names(Stats_Table_Anger)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Quasi_Psychotic<<- data.frame(Pop,  M_Quasi_Psychotic, M_Retest_Quasi_Psychotic, SD_Quasi_Psychotic, RelChangeMethod, Rel_Quasi_Psychotic, ConfInt)
      names(Stats_Table_Quasi_Psychotic)<<- c("Reference Population","M", "M (Retest)", "Sd", "Reliable Change Method", "Reliability", "Confidence")
    } else if (input$RelChangeMethod == 'Maassen et al. (2006)' | input$RelChangeMethod == 'McSweeny et al. (1993)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Impulsivity<<- data.frame(Pop,  M_Impulsivity, M_Retest_Impulsivity, SD_Impulsivity, SD_Retest_Impulsivity, RelChangeMethod, Rel_Impulsivity, ConfInt)
      names(Stats_Table_Impulsivity)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Affective_Instability<<- data.frame(Pop,  M_Affective_Instability, M_Retest_Affective_Instability, SD_Affective_Instability, SD_Retest_Affective_Instability, RelChangeMethod, Rel_Affective_Instability, ConfInt)
      names(Stats_Table_Affective_Instability)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Abandonment<<- data.frame(Pop,  M_Abandonment, M_Retest_Abandonment, SD_Abandonment, SD_Retest_Abandonment, RelChangeMethod, Rel_Abandonment, ConfInt)
      names(Stats_Table_Abandonment)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Relationships<<- data.frame(Pop,  M_Relationships, M_Retest_Relationships, SD_Relationships, SD_Retest_Relationships, RelChangeMethod, Rel_Relationships, ConfInt)
      names(Stats_Table_Relationships)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Self_Image<<- data.frame(Pop,  M_Self_Image, M_Retest_Self_Image, SD_Self_Image, SD_Retest_Self_Image, RelChangeMethod, Rel_Self_Image, ConfInt)
      names(Stats_Table_Self_Image)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Suicide_Mutilation<<- data.frame(Pop,  M_Suicide_Mutilation, M_Retest_Suicide_Mutilation, SD_Suicide_Mutilation, SD_Retest_Suicide_Mutilation, RelChangeMethod, Rel_Suicide_Mutilation, ConfInt)
      names(Stats_Table_Suicide_Mutilation)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Emptiness<<- data.frame(Pop,  M_Emptiness, M_Retest_Emptiness, SD_Emptiness, SD_Retest_Emptiness, RelChangeMethod, Rel_Emptiness, ConfInt)
      names(Stats_Table_Emptiness)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Anger<<- data.frame(Pop,  M_Anger, M_Retest_Anger, SD_Anger, SD_Retest_Anger, RelChangeMethod, Rel_Anger, ConfInt)
      names(Stats_Table_Anger)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Quasi_Psychotic<<- data.frame(Pop,  M_Quasi_Psychotic, M_Retest_Quasi_Psychotic, SD_Quasi_Psychotic, SD_Retest_Quasi_Psychotic, RelChangeMethod, Rel_Quasi_Psychotic, ConfInt)
      names(Stats_Table_Quasi_Psychotic)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "Confidence")
      
    } else if (input$RelChangeMethod == 'Crawford & Howell (1998)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  M, M_Retest, SD, SD_Retest, RelChangeMethod, Rel, SampleN, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Impulsivity<<- data.frame(Pop,  M_Impulsivity, M_Retest_Impulsivity, SD_Impulsivity, SD_Retest_Impulsivity, RelChangeMethod, Rel_Impulsivity, SampleN,ConfInt)
      names(Stats_Table_Impulsivity)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Affective_Instability<<- data.frame(Pop,  M_Affective_Instability, M_Retest_Affective_Instability, SD_Affective_Instability, SD_Retest_Affective_Instability, RelChangeMethod, Rel_Affective_Instability, SampleN, ConfInt)
      names(Stats_Table_Affective_Instability)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Abandonment<<- data.frame(Pop,  M_Abandonment, M_Retest_Abandonment, SD_Abandonment, SD_Retest_Abandonment, RelChangeMethod, Rel_Abandonment, SampleN,ConfInt)
      names(Stats_Table_Abandonment)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Relationships<<- data.frame(Pop,  M_Relationships, M_Retest_Relationships, SD_Relationships, SD_Retest_Relationships, RelChangeMethod, Rel_Relationships, SampleN,ConfInt)
      names(Stats_Table_Relationships)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Self_Image<<- data.frame(Pop,  M_Self_Image, M_Retest_Self_Image, SD_Self_Image, SD_Retest_Self_Image, RelChangeMethod, Rel_Self_Image, SampleN,ConfInt)
      names(Stats_Table_Self_Image)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Suicide_Mutilation<<- data.frame(Pop,  M_Suicide_Mutilation, M_Retest_Suicide_Mutilation, SD_Suicide_Mutilation, SD_Retest_Suicide_Mutilation, RelChangeMethod, Rel_Suicide_Mutilation, SampleN, ConfInt)
      names(Stats_Table_Suicide_Mutilation)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Emptiness<<- data.frame(Pop,  M_Emptiness, M_Retest_Emptiness, SD_Emptiness, SD_Retest_Emptiness, RelChangeMethod, Rel_Emptiness, SampleN,ConfInt)
      names(Stats_Table_Emptiness)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Anger<<- data.frame(Pop,  M_Anger, M_Retest_Anger, SD_Anger, SD_Retest_Anger, RelChangeMethod, Rel_Anger, SampleN,ConfInt)
      names(Stats_Table_Anger)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
      Stats_Table_Quasi_Psychotic<<- data.frame(Pop,  M_Quasi_Psychotic, M_Retest_Quasi_Psychotic, SD_Quasi_Psychotic, SD_Retest_Quasi_Psychotic, RelChangeMethod, Rel_Quasi_Psychotic, SampleN,ConfInt)
      names(Stats_Table_Quasi_Psychotic)<<- c("Reference Population","M", "M (Retest)", "Sd", "Sd (Retest)", "Reliable Change Method", "Reliability", "N", "Confidence")
    } else if (input$RelChangeMethod == 'Jacobson & Truax (1991)' | input$RelChangeMethod == 'Speer (1992)') {
      Stats_Table_Fullscale<<- data.frame(Pop,  SD, RelChangeMethod, Rel, ConfInt)
      names(Stats_Table_Fullscale)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Impulsivity<<- data.frame(Pop,  SD_Impulsivity, RelChangeMethod, Rel_Impulsivity, ConfInt)
      names(Stats_Table_Impulsivity)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Affective_Instability<<- data.frame(Pop,  SD_Affective_Instability, RelChangeMethod, Rel_Affective_Instability, ConfInt)
      names(Stats_Table_Affective_Instability)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Abandonment<<- data.frame(Pop,  SD_Abandonment, RelChangeMethod, Rel_Abandonment, ConfInt)
      names(Stats_Table_Abandonment)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Relationships<<- data.frame(Pop,  SD_Relationships, RelChangeMethod, Rel_Relationships, ConfInt)
      names(Stats_Table_Relationships)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Self_Image<<- data.frame(Pop,  SD_Self_Image, RelChangeMethod, Rel_Self_Image, ConfInt)
      names(Stats_Table_Self_Image)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Suicide_Mutilation<<- data.frame(Pop,  SD_Suicide_Mutilation, RelChangeMethod, Rel_Suicide_Mutilation, ConfInt)
      names(Stats_Table_Suicide_Mutilation)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Emptiness<<- data.frame(Pop,  SD_Emptiness, RelChangeMethod, Rel_Emptiness, ConfInt)
      names(Stats_Table_Emptiness)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Anger<<- data.frame(Pop,  SD_Anger, RelChangeMethod, Rel_Anger, ConfInt)
      names(Stats_Table_Anger)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      Stats_Table_Quasi_Psychotic<<- data.frame(Pop,  SD_Quasi_Psychotic, RelChangeMethod, Rel_Quasi_Psychotic, ConfInt)
      names(Stats_Table_Quasi_Psychotic)<<- c("Reference Population", , "Sd", "Reliable Change Method", "Reliability", "Confidence")
      
    }
    
    #If custom confidence intervals options are selected, make sure that no values appear for PS and SE in pdf tables
    #Change the look of tables in report if custom confidence intervals options are selected
    
    if (input$Select_CI == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS = NA, SE = NA)
      Stats_Table_Fullscale<<- Stats_Table_Fullscale %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI[1])
    }
    if (input$Select_CI_Impulsivity == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Impulsivity = NA, SE_Impulsivity = NA)
      Stats_Table_Impulsivity<<- Stats_Table_Impulsivity %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Impulsivity[1])
    }
    if (input$Select_CI_Affective_Instability == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Affective_Instability = NA, SE_Affective_Instability = NA)
      Stats_Table_Affective_Instability<<- Stats_Table_Affective_Instability %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                      "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Affective_Instability[1])
    }
    if (input$Select_CI_Abandonment == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Abandonment = NA, SE_Abandonment = NA)
      Stats_Table_Abandonment<<- Stats_Table_Abandonment %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                      "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Abandonment[1])
    }
    if (input$Select_CI_Relationships == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Relationships = NA, SE_Relationships = NA)
      Stats_Table_Relationships<<- Stats_Table_Relationships %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Relationships[1])
    }
    if (input$Select_CI_Self_Image == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Self_Image = NA, SE_Self_Image = NA)
      Stats_Table_Self_Image<<- Stats_Table_Self_Image %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                    "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Self_Image[1])
    }
    if (input$Select_CI_Suicide_Mutilation == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Suicide_Mutilation = NA, SE_Suicide_Mutilation = NA)
      Stats_Table_Suicide_Mutilation<<- Stats_Table_Suicide_Mutilation %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                                        "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Suicide_Mutilation[1])
    }
    if (input$Select_CI_Emptiness == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Emptiness = NA, SE_Emptiness = NA)
      Stats_Table_Emptiness<<- Stats_Table_Emptiness %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                    "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Emptiness[1])
    }
    if (input$Select_CI_Anger == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Anger = NA, SE_Anger = NA)
      Stats_Table_Anger<<- Stats_Table_Anger %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                        "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Anger[1])
    }
    if (input$Select_CI_Quasi_Psychotic == '2') {
      Entered_Scores_Df<<- Entered_Scores_Df %>% mutate(PTS_Quasi_Psychotic = NA, SE_Quasi_Psychotic = NA)
      Stats_Table_Quasi_Psychotic<<- Stats_Table_Quasi_Psychotic %>% as_tibble() %>% mutate("Reference Population" = NULL, M = NULL, Sd = NULL, Confidence = NULL, Reliability = NULL,
                                                                                "Reliable Change Method" = "Custom Confidence Intervals", "Width of Confidence Interval" = CI_Quasi_Psychotic[1])
    }
    
    
    
    
    #Create a dataframe to be used in the analytics spreadsheet.
    
    #Calculate variables for significant improvement
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$CI_Lower_Lim[1]) {
      BPQ.Fullscale.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$CI_Lower_Lim[1]) {
      BPQ.Fullscale.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$CI_Upper_Lim[1]) {
      BPQ.Fullscale.Sig.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$CI_Upper_Lim[1]) {
      BPQ.Fullscale.Sig.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Impulsivity[length(Entered_Scores_Df$Score_Impulsivity)] < Entered_Scores_Df$CI_Lower_Lim_Impulsivity[1]) {
      BPQ.Impulsivity.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Impulsivity[length(Entered_Scores_Df$Score_Impulsivity)] >= Entered_Scores_Df$CI_Lower_Lim_Impulsivity[1]) {
      BPQ.Impulsivity.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Impulsivity[length(Entered_Scores_Df$Score_Impulsivity)] > Entered_Scores_Df$CI_Upper_Lim_Impulsivity[1]) {
      BPQ.Impulsivity.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Impulsivity[length(Entered_Scores_Df$Score_Impulsivity)] <= Entered_Scores_Df$CI_Upper_Lim_Impulsivity[1]) {
      BPQ.Impulsivity.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Affective_Instability[length(Entered_Scores_Df$Score_Affective_Instability)] < Entered_Scores_Df$CI_Lower_Lim_Affective_Instability[1]) {
      BPQ.Affective.Instability.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Affective_Instability[length(Entered_Scores_Df$Score_Affective_Instability)] >= Entered_Scores_Df$CI_Lower_Lim_Affective_Instability[1]) {
      BPQ.Affective.Instability.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Affective_Instability[length(Entered_Scores_Df$Score_Affective_Instability)] > Entered_Scores_Df$CI_Upper_Lim_Affective_Instability[1]) {
      BPQ.Affective.Instability.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Affective_Instability[length(Entered_Scores_Df$Score_Affective_Instability)] <= Entered_Scores_Df$CI_Upper_Lim_Affective_Instability[1]) {
      BPQ.Affective.Instability.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Abandonment[length(Entered_Scores_Df$Score_Abandonment)] < Entered_Scores_Df$CI_Lower_Lim_Abandonment[1]) {
      BPQ.Abandonment.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Abandonment[length(Entered_Scores_Df$Score_Abandonment)] >= Entered_Scores_Df$CI_Lower_Lim_Abandonment[1]) {
      BPQ.Abandonment.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Abandonment[length(Entered_Scores_Df$Score_Abandonment)] > Entered_Scores_Df$CI_Upper_Lim_Abandonment[1]) {
      BPQ.Abandonment.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Abandonment[length(Entered_Scores_Df$Score_Abandonment)] <= Entered_Scores_Df$CI_Upper_Lim_Abandonment[1]) {
      BPQ.Abandonment.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] < Entered_Scores_Df$CI_Lower_Lim_Relationships[1]) {
      BPQ.Relationships.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] >= Entered_Scores_Df$CI_Lower_Lim_Relationships[1]) {
      BPQ.Relationships.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] > Entered_Scores_Df$CI_Upper_Lim_Relationships[1]) {
      BPQ.Relationships.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] <= Entered_Scores_Df$CI_Upper_Lim_Relationships[1]) {
      BPQ.Relationships.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Self_Image[length(Entered_Scores_Df$Score_Self_Image)] < Entered_Scores_Df$CI_Lower_Lim_Self_Image[1]) {
      BPQ.Self.Image.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Self_Image[length(Entered_Scores_Df$Score_Self_Image)] >= Entered_Scores_Df$CI_Lower_Lim_Self_Image[1]) {
      BPQ.Self.Image.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Self_Image[length(Entered_Scores_Df$Score_Self_Image)] > Entered_Scores_Df$CI_Upper_Lim_Self_Image[1]) {
      BPQ.Self.Image.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Self_Image[length(Entered_Scores_Df$Score_Self_Image)] <= Entered_Scores_Df$CI_Upper_Lim_Self_Image[1]) {
      BPQ.Self.Image.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Suicide_Mutilation[length(Entered_Scores_Df$Score_Suicide_Mutilation)] < Entered_Scores_Df$CI_Lower_Lim_Suicide_Mutilation[1]) {
      BPQ.Suicide.Mutilation.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Suicide_Mutilation[length(Entered_Scores_Df$Score_Suicide_Mutilation)] >= Entered_Scores_Df$CI_Lower_Lim_Suicide_Mutilation[1]) {
      BPQ.Suicide.Mutilation.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Suicide_Mutilation[length(Entered_Scores_Df$Score_Suicide_Mutilation)] > Entered_Scores_Df$CI_Upper_Lim_Suicide_Mutilation[1]) {
      BPQ.Suicide.Mutilation.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Suicide_Mutilation[length(Entered_Scores_Df$Score_Suicide_Mutilation)] <= Entered_Scores_Df$CI_Upper_Lim_Suicide_Mutilation[1]) {
      BPQ.Suicide.Mutilation.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Emptiness[length(Entered_Scores_Df$Score_Emptiness)] < Entered_Scores_Df$CI_Lower_Lim_Emptiness[1]) {
      BPQ.Emptiness.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Emptiness[length(Entered_Scores_Df$Score_Emptiness)] >= Entered_Scores_Df$CI_Lower_Lim_Emptiness[1]) {
      BPQ.Emptiness.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Emptiness[length(Entered_Scores_Df$Score_Emptiness)] > Entered_Scores_Df$CI_Upper_Lim_Emptiness[1]) {
      BPQ.Emptiness.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Emptiness[length(Entered_Scores_Df$Score_Emptiness)] <= Entered_Scores_Df$CI_Upper_Lim_Emptiness[1]) {
      BPQ.Emptiness.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Anger[length(Entered_Scores_Df$Score_Anger)] < Entered_Scores_Df$CI_Lower_Lim_Anger[1]) {
      BPQ.Anger.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Anger[length(Entered_Scores_Df$Score_Anger)] >= Entered_Scores_Df$CI_Lower_Lim_Anger[1]) {
      BPQ.Anger.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Anger[length(Entered_Scores_Df$Score_Anger)] > Entered_Scores_Df$CI_Upper_Lim_Anger[1]) {
      BPQ.Anger.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Anger[length(Entered_Scores_Df$Score_Anger)] <= Entered_Scores_Df$CI_Upper_Lim_Anger[1]) {
      BPQ.Anger.Sig.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Quasi_Psychotic[length(Entered_Scores_Df$Score_Quasi_Psychotic)] < Entered_Scores_Df$CI_Lower_Lim_Quasi_Psychotic[1]) {
      BPQ.Quasi.Psychotic.Sig.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Quasi_Psychotic[length(Entered_Scores_Df$Score_Quasi_Psychotic)] >= Entered_Scores_Df$CI_Lower_Lim_Quasi_Psychotic[1]) {
      BPQ.Quasi.Psychotic.Sig.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Quasi_Psychotic[length(Entered_Scores_Df$Score_Quasi_Psychotic)] > Entered_Scores_Df$CI_Upper_Lim_Quasi_Psychotic[1]) {
      BPQ.Quasi.Psychotic.Sig.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Quasi_Psychotic[length(Entered_Scores_Df$Score_Quasi_Psychotic)] <= Entered_Scores_Df$CI_Upper_Lim_Quasi_Psychotic[1]) {
      BPQ.Quasi.Psychotic.Sig.Deterioration<- "No"
    }
    
    
    #Calculate variables for improvement
    
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] < Entered_Scores_Df$Score[1]) {
      BPQ.Fullscale.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] >= Entered_Scores_Df$Score[1]) {
      BPQ.Fullscale.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] > Entered_Scores_Df$Score[1]) {
      BPQ.Fullscale.Deterioration<- "Yes"
    } else if(Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] <= Entered_Scores_Df$Score[1]) {
      BPQ.Fullscale.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Impulsivity[length(Entered_Scores_Df$Score_Impulsivity)] < Entered_Scores_Df$Score_Impulsivity[1]) {
      BPQ.Impulsivity.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Impulsivity[length(Entered_Scores_Df$Score_Impulsivity)] >= Entered_Scores_Df$Score_Impulsivity[1]) {
      BPQ.Impulsivity.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Impulsivity[length(Entered_Scores_Df$Score_Impulsivity)] > Entered_Scores_Df$Score_Impulsivity[1]) {
      BPQ.Impulsivity.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Impulsivity[length(Entered_Scores_Df$Score_Impulsivity)] <= Entered_Scores_Df$Score_Impulsivity[1]) {
      BPQ.Impulsivity.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Affective_Instability[length(Entered_Scores_Df$Score_Affective_Instability)] < Entered_Scores_Df$Score_Affective_Instability[1]) {
      BPQ.Affective.Instability.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Affective_Instability[length(Entered_Scores_Df$Score_Affective_Instability)] >= Entered_Scores_Df$Score_Affective_Instability[1]) {
      BPQ.Affective.Instability.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Affective_Instability[length(Entered_Scores_Df$Score_Affective_Instability)] > Entered_Scores_Df$Score_Affective_Instability[1]) {
      BPQ.Affective.Instability.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Affective_Instability[length(Entered_Scores_Df$Score_Affective_Instability)] <= Entered_Scores_Df$Score_Affective_Instability[1]) {
      BPQ.Affective.Instability.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Abandonment[length(Entered_Scores_Df$Score_Abandonment)] < Entered_Scores_Df$Score_Abandonment[1]) {
      BPQ.Abandonment.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Abandonment[length(Entered_Scores_Df$Score_Abandonment)] >= Entered_Scores_Df$Score_Abandonment[1]) {
      BPQ.Abandonment.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Abandonment[length(Entered_Scores_Df$Score_Abandonment)] > Entered_Scores_Df$Score_Abandonment[1]) {
      BPQ.Abandonment.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Abandonment[length(Entered_Scores_Df$Score_Abandonment)] <= Entered_Scores_Df$Score_Abandonment[1]) {
      BPQ.Abandonment.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] < Entered_Scores_Df$Score_Relationships[1]) {
      BPQ.Relationships.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] >= Entered_Scores_Df$Score_Relationships[1]) {
      BPQ.Relationships.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] > Entered_Scores_Df$Score_Relationships[1]) {
      BPQ.Relationships.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] <= Entered_Scores_Df$Score_Relationships[1]) {
      BPQ.Relationships.Deterioration<- "No"
    }
    
    
    if(Entered_Scores_Df$Score_Self_Image[length(Entered_Scores_Df$Score_Self_Image)] < Entered_Scores_Df$Score_Self_Image[1]) {
      BPQ.Self.Image.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Self_Image[length(Entered_Scores_Df$Score_Self_Image)] >= Entered_Scores_Df$Score_Self_Image[1]) {
      BPQ.Self.Image.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Self_Image[length(Entered_Scores_Df$Score_Self_Image)] > Entered_Scores_Df$Score_Self_Image[1]) {
      BPQ.Self.Image.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Self_Image[length(Entered_Scores_Df$Score_Self_Image)] <= Entered_Scores_Df$Score_Self_Image[1]) {
      BPQ.Self.Image.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Suicide_Mutilation[length(Entered_Scores_Df$Score_Suicide_Mutilation)] < Entered_Scores_Df$Score_Suicide_Mutilation[1]) {
      BPQ.Suicide.Mutilation.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Suicide_Mutilation[length(Entered_Scores_Df$Score_Suicide_Mutilation)] >= Entered_Scores_Df$Score_Suicide_Mutilation[1]) {
      BPQ.Suicide.Mutilation.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Suicide_Mutilation[length(Entered_Scores_Df$Score_Suicide_Mutilation)] > Entered_Scores_Df$Score_Suicide_Mutilation[1]) {
      BPQ.Suicide.Mutilation.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Suicide_Mutilation[length(Entered_Scores_Df$Score_Suicide_Mutilation)] <= Entered_Scores_Df$Score_Suicide_Mutilation[1]) {
      BPQ.Suicide.Mutilation.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Emptiness[length(Entered_Scores_Df$Score_Emptiness)] < Entered_Scores_Df$Score_Emptiness[1]) {
      BPQ.Emptiness.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Emptiness[length(Entered_Scores_Df$Score_Emptiness)] >= Entered_Scores_Df$Score_Emptiness[1]) {
      BPQ.Emptiness.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Emptiness[length(Entered_Scores_Df$Score_Emptiness)] > Entered_Scores_Df$Score_Emptiness[1]) {
      BPQ.Emptiness.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Emptiness[length(Entered_Scores_Df$Score_Emptiness)] <= Entered_Scores_Df$Score_Emptiness[1]) {
      BPQ.Emptiness.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Anger[length(Entered_Scores_Df$Score_Anger)] < Entered_Scores_Df$Score_Anger[1]) {
      BPQ.Anger.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Anger[length(Entered_Scores_Df$Score_Anger)] >= Entered_Scores_Df$Score_Anger[1]) {
      BPQ.Anger.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Anger[length(Entered_Scores_Df$Score_Anger)] > Entered_Scores_Df$Score_Anger[1]) {
      BPQ.Anger.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Anger[length(Entered_Scores_Df$Score_Anger)] <= Entered_Scores_Df$Score_Anger[1]) {
      BPQ.Anger.Deterioration<- "No"
    }
    
    if(Entered_Scores_Df$Score_Quasi_Psychotic[length(Entered_Scores_Df$Score_Quasi_Psychotic)] < Entered_Scores_Df$Score_Quasi_Psychotic[1]) {
      BPQ.Quasi.Psychotic.Improvement<- "Yes"
    } else if (Entered_Scores_Df$Score_Quasi_Psychotic[length(Entered_Scores_Df$Score_Quasi_Psychotic)] >= Entered_Scores_Df$Score_Quasi_Psychotic[1]) {
      BPQ.Quasi.Psychotic.Improvement<- "No"
    }
    
    if(Entered_Scores_Df$Score_Quasi_Psychotic[length(Entered_Scores_Df$Score_Quasi_Psychotic)] > Entered_Scores_Df$Score_Quasi_Psychotic[1]) {
      BPQ.Quasi.Psychotic.Deterioration<- "Yes"
    } else if (Entered_Scores_Df$Score_Quasi_Psychotic[length(Entered_Scores_Df$Score_Quasi_Psychotic)] <= Entered_Scores_Df$Score_Quasi_Psychotic[1]) {
      BPQ.Quasi.Psychotic.Deterioration<- "No"
    }
    
    
    BPQ.Fullscale.Change<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)] - Entered_Scores_Df$Score[1]
    BPQ.Impulsivity.Change<- Entered_Scores_Df$Score_Impulsivity[length(Entered_Scores_Df$Score_Impulsivity)] - Entered_Scores_Df$Score_Impulsivity[1]
    BPQ.Affective.Instability.Change<- Entered_Scores_Df$Score_Affective_Instability[length(Entered_Scores_Df$Score_Affective_Instability)] - Entered_Scores_Df$Score_Affective_Instability[1]
    BPQ.Abandonment.Change<- Entered_Scores_Df$Score_Abandonment[length(Entered_Scores_Df$Score_Abandonment)] - Entered_Scores_Df$Score_Abandonment[1]
    BPQ.Relationships.Change<- Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)] - Entered_Scores_Df$Score_Relationships[1]
    BPQ.Fullscale.Comparisons<- length(Entered_Scores_Df$Change) - 1
    BPQ.Impulsivity.Comparisons<- length(Entered_Scores_Df$Change_Impulsivity) - 1
    BPQ.Affective.Instability.Comparisons<- length(Entered_Scores_Df$Change_Affective_Instability) - 1
    BPQ.Abandonment.Comparisons<- length(Entered_Scores_Df$Change_Abandonment) - 1
    BPQ.Relationships.Comparisons<- length(Entered_Scores_Df$Change_Relationships) - 1
    BPQ.Fullscale.First.Date<- Entered_Scores_Df$Date[1]
    BPQ.Impulsivity.First.Date<- Entered_Scores_Df$Date[1]
    BPQ.Affective.Instability.First.Date<- Entered_Scores_Df$Date[1]
    BPQ.Abandonment.First.Date<- Entered_Scores_Df$Date[1]
    BPQ.Relationships.First.Date<- Entered_Scores_Df$Date[1]
    BPQ.Fullscale.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    BPQ.Impulsivity.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    BPQ.Affective.Instability.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    BPQ.Abandonment.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    BPQ.Relationships.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    BPQ.Fullscale.First.Score<- Entered_Scores_Df$Score[1]
    BPQ.Impulsivity.First.Score<- Entered_Scores_Df$Score_Impulsivity[1]
    BPQ.Affective.Instability.First.Score<- Entered_Scores_Df$Score_Affective_Instability[1]
    BPQ.Abandonment.First.Score<- Entered_Scores_Df$Score_Abandonment[1]
    BPQ.Relationships.First.Score<- Entered_Scores_Df$Score_Relationships[1]
    BPQ.Fullscale.Last.Score<- Entered_Scores_Df$Score[length(Entered_Scores_Df$Score)]
    BPQ.Impulsivity.Last.Score<- Entered_Scores_Df$Score_Impulsivity[length(Entered_Scores_Df$Score_Impulsivity)]
    BPQ.Affective.Instability.Last.Score<- Entered_Scores_Df$Score_Affective_Instability[length(Entered_Scores_Df$Score_Affective_Instability)]
    BPQ.Abandonment.Last.Score<- Entered_Scores_Df$Score_Abandonment[length(Entered_Scores_Df$Score_Abandonment)]
    BPQ.Relationships.Last.Score<- Entered_Scores_Df$Score_Relationships[length(Entered_Scores_Df$Score_Relationships)]
    
    
    BPQ.Self.Image.Change<- Entered_Scores_Df$Score_Self_Image[length(Entered_Scores_Df$Score_Self_Image)] - Entered_Scores_Df$Score_Self_Image[1]
    BPQ.Suicide.Mutilation.Change<- Entered_Scores_Df$Score_Suicide_Mutilation[length(Entered_Scores_Df$Score_Suicide_Mutilation)] - Entered_Scores_Df$Score_Suicide_Mutilation[1]
    BPQ.Emptiness.Change<- Entered_Scores_Df$Score_Emptiness[length(Entered_Scores_Df$Score_Emptiness)] - Entered_Scores_Df$Score_Emptiness[1]
    BPQ.Anger.Change<- Entered_Scores_Df$Score_Anger[length(Entered_Scores_Df$Score_Anger)] - Entered_Scores_Df$Score_Anger[1]
    BPQ.Quasi.Psychotic.Change<- Entered_Scores_Df$Score_Quasi_Psychotic[length(Entered_Scores_Df$Score_Quasi_Psychotic)] - Entered_Scores_Df$Score_Quasi_Psychotic[1]
    BPQ.Self.Image.Comparisons<- length(Entered_Scores_Df$Change) - 1
    BPQ.Suicide.Mutilation.Comparisons<- length(Entered_Scores_Df$Change_Suicide_Mutilation) - 1
    BPQ.Emptiness.Comparisons<- length(Entered_Scores_Df$Change_Emptiness) - 1
    BPQ.Anger.Comparisons<- length(Entered_Scores_Df$Change_Anger) - 1
    BPQ.Quasi.Psychotic.Comparisons<- length(Entered_Scores_Df$Change_Quasi_Psychotic) - 1
    BPQ.Self.Image.First.Date<- Entered_Scores_Df$Date[1]
    BPQ.Suicide.Mutilation.First.Date<- Entered_Scores_Df$Date[1]
    BPQ.Emptiness.First.Date<- Entered_Scores_Df$Date[1]
    BPQ.Anger.First.Date<- Entered_Scores_Df$Date[1]
    BPQ.Quasi.Psychotic.First.Date<- Entered_Scores_Df$Date[1]
    BPQ.Self.Image.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    BPQ.Suicide.Mutilation.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    BPQ.Emptiness.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    BPQ.Anger.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    BPQ.Quasi.Psychotic.Last.Date<- Entered_Scores_Df$Date[length(Entered_Scores_Df$Date)]
    BPQ.Self.Image.First.Score<- Entered_Scores_Df$Score_Self_Image[1]
    BPQ.Suicide.Mutilation.First.Score<- Entered_Scores_Df$Score_Suicide_Mutilation[1]
    BPQ.Emptiness.First.Score<- Entered_Scores_Df$Score_Emptiness[1]
    BPQ.Anger.First.Score<- Entered_Scores_Df$Score_Anger[1]
    BPQ.Quasi.Psychotic.First.Score<- Entered_Scores_Df$Score_Quasi_Psychotic[1]
    BPQ.Self.Image.Last.Score<- Entered_Scores_Df$Score_Self_Image[length(Entered_Scores_Df$Score_Self_Image)]
    BPQ.Suicide.Mutilation.Last.Score<- Entered_Scores_Df$Score_Suicide_Mutilation[length(Entered_Scores_Df$Score_Suicide_Mutilation)]
    BPQ.Emptiness.Last.Score<- Entered_Scores_Df$Score_Emptiness[length(Entered_Scores_Df$Score_Emptiness)]
    BPQ.Anger.Last.Score<- Entered_Scores_Df$Score_Anger[length(Entered_Scores_Df$Score_Anger)]
    BPQ.Quasi.Psychotic.Last.Score<- Entered_Scores_Df$Score_Quasi_Psychotic[length(Entered_Scores_Df$Score_Quasi_Psychotic)]
    
    
    
    
    
    
    Analytics_Df<<- data.frame(BPQ.Fullscale.First.Date, BPQ.Fullscale.First.Score, BPQ.Fullscale.Comparisons, BPQ.Fullscale.Change, BPQ.Fullscale.Last.Date, BPQ.Fullscale.Last.Score, BPQ.Fullscale.Improvement,BPQ.Fullscale.Sig.Improvement, BPQ.Fullscale.Deterioration, BPQ.Fullscale.Sig.Deterioration,
                               BPQ.Impulsivity.First.Date, BPQ.Impulsivity.First.Score, BPQ.Impulsivity.Comparisons, BPQ.Impulsivity.Change, BPQ.Impulsivity.Last.Date, BPQ.Impulsivity.Last.Score, BPQ.Impulsivity.Improvement, BPQ.Impulsivity.Sig.Improvement, BPQ.Impulsivity.Deterioration, BPQ.Impulsivity.Sig.Deterioration,
                               BPQ.Affective.Instability.First.Date, BPQ.Affective.Instability.First.Score, BPQ.Affective.Instability.Comparisons, BPQ.Affective.Instability.Change, BPQ.Affective.Instability.Last.Date, BPQ.Affective.Instability.Last.Score, BPQ.Affective.Instability.Improvement, BPQ.Affective.Instability.Sig.Improvement, BPQ.Affective.Instability.Deterioration, BPQ.Affective.Instability.Sig.Deterioration, 
                               BPQ.Abandonment.First.Date, BPQ.Abandonment.First.Score, BPQ.Abandonment.Comparisons, BPQ.Abandonment.Change, BPQ.Abandonment.Last.Date, BPQ.Abandonment.Last.Score, BPQ.Abandonment.Improvement, BPQ.Abandonment.Sig.Improvement, BPQ.Abandonment.Deterioration, BPQ.Abandonment.Sig.Deterioration, 
                               BPQ.Relationships.First.Date, BPQ.Relationships.First.Score, BPQ.Relationships.Comparisons, BPQ.Relationships.Change, BPQ.Relationships.Last.Date, BPQ.Relationships.Last.Score, BPQ.Relationships.Improvement, BPQ.Relationships.Sig.Improvement, BPQ.Relationships.Deterioration, BPQ.Relationships.Sig.Deterioration,
                               BPQ.Self.Image.First.Date, BPQ.Self.Image.First.Score, BPQ.Self.Image.Comparisons, BPQ.Self.Image.Change, BPQ.Self.Image.Last.Date, BPQ.Self.Image.Last.Score, BPQ.Self.Image.Improvement,BPQ.Self.Image.Sig.Improvement, BPQ.Self.Image.Deterioration, BPQ.Self.Image.Sig.Deterioration,
                               BPQ.Suicide.Mutilation.First.Date, BPQ.Suicide.Mutilation.First.Score, BPQ.Suicide.Mutilation.Comparisons, BPQ.Suicide.Mutilation.Change, BPQ.Suicide.Mutilation.Last.Date, BPQ.Suicide.Mutilation.Last.Score, BPQ.Suicide.Mutilation.Improvement, BPQ.Suicide.Mutilation.Sig.Improvement, BPQ.Suicide.Mutilation.Deterioration, BPQ.Suicide.Mutilation.Sig.Deterioration,
                               BPQ.Emptiness.First.Date, BPQ.Emptiness.First.Score, BPQ.Emptiness.Comparisons, BPQ.Emptiness.Change, BPQ.Emptiness.Last.Date, BPQ.Emptiness.Last.Score, BPQ.Emptiness.Improvement, BPQ.Emptiness.Sig.Improvement, BPQ.Emptiness.Deterioration, BPQ.Emptiness.Sig.Deterioration, 
                               BPQ.Anger.First.Date, BPQ.Anger.First.Score, BPQ.Anger.Comparisons, BPQ.Anger.Change, BPQ.Anger.Last.Date, BPQ.Anger.Last.Score, BPQ.Anger.Improvement, BPQ.Anger.Sig.Improvement, BPQ.Anger.Deterioration, BPQ.Anger.Sig.Deterioration, 
                               BPQ.Quasi.Psychotic.First.Date, BPQ.Quasi.Psychotic.First.Score, BPQ.Quasi.Psychotic.Comparisons, BPQ.Quasi.Psychotic.Change, BPQ.Quasi.Psychotic.Last.Date, BPQ.Quasi.Psychotic.Last.Score, BPQ.Quasi.Psychotic.Improvement, BPQ.Quasi.Psychotic.Sig.Improvement, BPQ.Quasi.Psychotic.Deterioration, BPQ.Quasi.Psychotic.Sig.Deterioration)
    
    
    
  })
  
  
  
  #Create an expression to activate the entered data & create notifications to indicate inaccurate data entry
  
  Entered_Scores_Notifications<- observeEvent(input$Action_Submit_Data, {
    
    Entered_Scores_Reac()
    
    if(length(Score_1a) < 80) {
      showNotification("The BPQ is an 80-item scale. You have entered less than 80 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(length(Score_1a) > 80) {
      showNotification("The BPQ is an 80-item scale. You have entered more than 80 scores for the first timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
    }
    
    if(input$Timepoint != "1") {
      if(length(Score_2a) < 80) {
        showNotification("The BPQ is an 80-item scale. You have entered less than 80 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_2a) > 80) {
        showNotification("The BPQ is an 80-item scale. You have entered more than 80 scores for the second timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      }
    }
    
    if(input$Timepoint == "3") {
      if(length(Score_3a) < 80) {
        showNotification("The BPQ is an 80-item scale. You have entered less than 80 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
      } else if(length(Score_3a) > 80) {
        showNotification("The BPQ is an 80-item scale. You have entered more than 80 scores for the third timepoint. Enter the correct number of scores and then reclick the 'Submit Newly-Entered Data' button. Alternatively, proceed despite the missing data.", type = "error", duration = NULL)
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
    
    Gap_Impulsivity<- Entered_Scores_Df[1,11] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),11]
    Entered_Scores_Df[1,12]<- Gap_Impulsivity
    
    Gap_Affective_Instability<- Entered_Scores_Df[1,20] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),20]
    Entered_Scores_Df[1,21]<- Gap_Affective_Instability
    
    Gap_Abandonment<- Entered_Scores_Df[1,29] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),29]
    Entered_Scores_Df[1,30]<- Gap_Abandonment
    
    Gap_Relationships<- Entered_Scores_Df[1,38] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),38]
    Entered_Scores_Df[1,39]<- Gap_Relationships
    
    Gap_Self_Image<- Entered_Scores_Df[1,47] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),47]
    Entered_Scores_Df[1,48]<- Gap_Self_Image
    
    Gap_Suicide_Mutilation<- Entered_Scores_Df[1,56] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),56]
    Entered_Scores_Df[1,57]<- Gap_Suicide_Mutilation
    
    Gap_Emptiness<- Entered_Scores_Df[1,65] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),65]
    Entered_Scores_Df[1,66]<- Gap_Emptiness
    
    Gap_Anger<- Entered_Scores_Df[1,74] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),74]
    Entered_Scores_Df[1,75]<- Gap_Anger
    
    Gap_Quasi_Psychotic<- Entered_Scores_Df[1,83] - Imported_Scores_CSV_ToCombine[nrow(Imported_Scores_CSV_ToCombine),83]
    Entered_Scores_Df[1,84]<- Gap_Quasi_Psychotic
    

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
    
    filename = paste0(" BPQ Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
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
        Stats_Table_Impulsivity = Stats_Table_Impulsivity,
        Stats_Table_Affective_Instability = Stats_Table_Affective_Instability,
        Stats_Table_Abandonment = Stats_Table_Abandonment,
        Stats_Table_Relationships = Stats_Table_Relationships,
        Stats_Table_Self_Image = Stats_Table_Self_Image,
        Stats_Table_Suicide_Mutilation = Stats_Table_Suicide_Mutilation,
        Stats_Table_Emptiness = Stats_Table_Emptiness,
        Stats_Table_Anger = Stats_Table_Anger,
        Stats_Table_Quasi_Psychotic = Stats_Table_Quasi_Psychotic,
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
      paste(paste0(" BPQ Data ", format(Sys.time(), '%d/%m/%y')), input$Output_Filetype1, sep = ".")
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













