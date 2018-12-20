

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
library(plotly)
library(tidyr)
library(radiant.data)
library(stringr)
library(stats)
library(stringi)
library(shinyjs)
library(readr)
library(corrplot)
library(htmltools)
library(Hmisc)


addUIDep <- function(x) {
  jqueryUIDep <- htmlDependency("jqueryui", "1.10.4", c(href="shared/jqueryui/1.10.4"),
                                script = "jquery-ui.min.js",
                                stylesheet = "jquery-ui.min.css")
  
  attachDependencies(x, c(htmlDependencies(x), list(jqueryUIDep)))
}


source("Corr_Test.R", local = TRUE)

ui <- navbarPage("PsychlytX | Analytics", selected = "Individual Cases", 
                 tags$head(            #Link to the css style sheet
                   tags$link(rel = "stylesheet", type = "text/css", href = "Styling.css")
                 ),
                 tabPanel("Individual Cases",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         fileInput('Analytics_File', span(tagList(icon("folder-open", lib = "font-awesome", "fa-2x")), h4(tags$b("Import Analaytics Dataset"))),
                                                   accept=c('text/csv',
                                                            'text/comma-separated-values,text/plain',
                                                            '.csv')),
                                         h5(tags$em("* Allow up to 30 seconds for the dataset to display in the white space.")),
                                         hr(),
                                         radioButtons("Period_Filter", h4(tags$strong("Select Reporting Period")), c("No", "Yes"), inline = TRUE),
                                         uiOutput("Report_Period"),
                                         h5(tags$em("* If yes, analyses are only applied to cases entered during the timeframe you specified.")),
                                         hr(),
                                         radioButtons("Filter", h4(tags$strong("Apply Filter To Data")), c("No", "Yes"), inline = TRUE),
                                         conditionalPanel(condition = "input.Filter == 'Yes'",
                                                          textInput("Filter_Express", h4(tags$strong("Use a Filtering Expression")), value = ""), 
                                                          h5("E.g. Age == 30"),
                                                          h5("E.g. Age < 30"),
                                                          h5("E.g. Age > 30"),
                                                          h5("E.g. Score > 30 | Score < 10"),
                                                          h5("E.g. Score > 30 & Age < 20"),
                                                          h5("E.g. Sex == 'Female'")
                                         ),
                                         radioButtons("Ind_All_Vars", h4(tags$strong("View Specific Variables")), c("No", "Yes"), inline = TRUE),
                                         uiOutput("Ind_Vars1"),
                                         actionButton("Show", "Show Data")
                            ),
                            mainPanel(
                              DT::dataTableOutput("Entire_Spreadsheet")
                            )
                          )),
                 
                 tabPanel("Frequencies",
                          sidebarLayout(
                            sidebarPanel(width = 3, icon("table", lib = "font-awesome", "fa-2x"),
                                         h4(tags$strong("Table")),
                                         hr(),
                                         selectizeInput("Cat_Vars_Freq_Tab", "Select Variables", choices = "", options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button"))),
                                         uiOutput("Freq_Norm"),
                                         checkboxInput("Freq_Perc", h5(tags$strong("Express Values as Percentage")))
                                         
                                         
                            ),
                            mainPanel(
                              DT::dataTableOutput("Freq_Tab")
                            )
                          ),
                          
                          sidebarLayout(
                            sidebarPanel(width = 3, icon("bar-chart-o", lib = "font-awesome", "fa-2x"),
                                         h4(tags$strong("Plots")),
                                         hr(),
                                         selectizeInput("Cat_Vars_Freq_Plot", "Select Variables", choices = "", options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button")))
                            ),
                            mainPanel(
                              plotlyOutput("Freq_Plot")
                            )
                          )
                 ),
                 
                 
                 tabPanel("Summaries", 
                          sidebarLayout(
                            sidebarPanel(width = 3, icon("table", lib = "font-awesome", "fa-2x"),
                                         h4(tags$strong("Table")),
                                         hr(),
                                         selectizeInput("Num_Vars_Sum_Tab", "Numeric Variables", choices = "", options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button"))),
                                         selectizeInput("Cat_Vars_Sum_Tab", "Categorical Variables", choices = "", options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button"))),
                                         selectizeInput("Sum_Funs", "Select Function", multiple = TRUE, choices = c("Mean" = "mean_rm", 
                                                                                                                    "Median" = "median_rm", "Mode" = "mode_rm", "Standard Deviation" = "sd_rm", "Minimum" = "min_rm", "Maximum" = "max_rm", "Variance" = "var_rm", 
                                                                                                                    "Sum" = "sum_rm", "5th Percentile" = "p05", "10th Percentile" = "p10", "25th Percentile" = "p25", "75th Percentile" = "p75",
                                                                                                                    "90th Percentile" = "p90", "95th Percentile" = "p95", "Missing Cases" = "n_missing", "Skew" = "skew", "Kurtosis" = "kurtosi"))
                                       
                                         
                                         
                            ),
                            mainPanel(
                              DT::dataTableOutput("Sum_Tab")
                              
                            )
                          ),
                          
                          sidebarLayout(
                            sidebarPanel(width = 3, icon("bar-chart-o", lib = "font-awesome", "fa-2x"),
                                         h4(tags$strong("Plots")),
                                         hr(),
                                         radioButtons("Sum_Plot_Type", h5(tags$strong("Select Plot Type")), choices = c("View Variable Distributions", "Compare Averages", "Explore Associations"), selected = "Compare Averages"),
                                         conditionalPanel(condition = "input.Sum_Plot_Type == 'View Variable Distributions'",
                                                          h5(tags$em("Select 1 numeric variable below & no categorical variables"))
                                         ),
                                         conditionalPanel(condition = "input.Sum_Plot_Type == 'Compare Averages'",
                                                          h5(tags$em("Select 1 numeric variable & up to 3 categorical variables below"))
                                         ),
                                         conditionalPanel(condition = "input.Sum_Plot_Type == 'Explore Associations'",
                                                          h5(tags$em("Select two numeric variables below"))
                                         ),
                                         br(),
                                         selectizeInput("Num_Vars_Sum_Plot", "Select Numeric Variables", choices = "", options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button"))),
                                         selectizeInput("Cat_Vars_Sum_Plot", "Select Categorical Variables", choices = "", options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button"))),
                                         uiOutput("Sum_Assoc_Factor"),
                                         uiOutput("Sum_Text_Select_Cat")
                            ),
                            
                            mainPanel(
                              plotlyOutput("Sum_Plot")
                            )
                          )
       
                 ),
                 
                 tabPanel("Relationships",
                          sidebarLayout(
                            sidebarPanel(width = 4, icon("sitemap", lib = "font-awesome", "fa-2x"),
                                         h4(tags$strong("Correlation")),
                                         hr(),
                                         selectizeInput("Corr_Variables", "Select Variables", choices = "", options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button"))),
                                         selectInput("Corr_Method", "Correlation Method",
                                                     c("Pearson" = "pearson", "Kendall" = "kendall", "Spearman" = "spearman")),
                                         selectInput("Corr_Use", "Missing Values Method",
                                                     c("Complete Observations" = "complete.obs", "Everything"="everything", "All Observations" = "all.obs", "NA Values or Complete Observations" = "na.or.complete", "Pairwise Complete Observations" = "pairwise.complete.obs")),
                                         tags$hr(),
                                         selectInput("Plot_Method", "Plot Method",
                                                     c("Number" = "number", "Circle" = "circle", "Shade" = "shade", "Pie" = "pie")),
                                         conditionalPanel("input.plotMethod !== 'mixed'",
                                                          selectInput("Plot_Type", "Plot Type",
                                                                      c("Full Plot" = "full", "Lower Half" = "lower", "Upper Half" = "upper"))
                                         ),
                                         checkboxInput("Sig_Test", "Significance Test"),
                                         conditionalPanel("input.Sig_Test",
                                                          selectInput("Sig_Action", "Significance Display Method",
                                                                      c("Cross Out Non-Significant Values" = "pch", "Asterix Significant Values" = "label_sig", "Show P-Values" = "p-value")))
                            ),
                            mainPanel(
                              plotOutput("Corr_Plot", height = 600)
                            )
                          )
                          
                          
                          
                 ),
                 tabPanel("Benchmarking",
                          sidebarLayout(
                            sidebarPanel(width = 4, icon("bullseye", lib = "font-awesome", "fa-2x"),
                                         h4(tags$strong("Set Benchmarking Parameters")),
                                         hr(),
                                         selectInput("T1_Benchmark_Var", "Select Variable (First Score)", choices = ""),
                                         selectInput("T2_Benchmark_Var", "Select Variable (Last Score)", choices = ""),
                                         selectInput("Benchmark", "Select Improvement Benchmark", choices = c("10%"= "0.1", "20%" = "0.2", "30%" = "0.3", "40%" = "0.4", 
                                                                                                              "50%" = "0.5", "60%" = "0.6", "70%" = "0.7", "80%" = "0.8", "90%" = "0.9", "100%" = "1")),
                                         selectInput("Direction", "Select Direction of Improvement", choices = c("Reduction", "Increase")),
                                         actionButton("Show_Benchmark_Data", "Process Results")
                                         
                                         
                            ),
                            mainPanel(
                              DT::dataTableOutput("Benchmark_Table")
                            )
                          )
                          ),
                 
                 
                 tabPanel("Analytics Reports",
                          sidebarLayout(
                            sidebarPanel(width = 12, icon("file-pdf-o", lib = "font-awesome", "fa-2x"),
                                         h4(tags$strong("Analytics Report: Outcome Measures")),
                                         h5(tags$strong("Step 1:")),
                                         selectInput("Report_Select", "Select An Outcome Measure", choices = c("Assessment of Quality of Life - 8 Dimensions (AQoL-8D)", 
                                                                                                            "Borderline Personality Questionnaire (BPQ)",
                                                                                                            "Chronic Pain Acceptance Questionnaire (CPAQ)",
                                                                                                            "Depression Anxiety Stress Scales - 21 (DASS-21)",
                                                                                                            "Eating Disorder Examination Questionnaire (EDE-Q)",
                                                                                                            "Edinborough Postnatal Depression Scale (EDPS)",
                                                                                                            "Generalized Anxiety Disorder 7-Item Scale (GAD-7)",
                                                                                                            "Geriatric Depression Scale - 15 (GDS-15)",
                                                                                                            "Illness Attitude Scales (IAS)",
                                                                                                            "Injustice Experience Questionnaire (IEQ)",
                                                                                                            "Insomnia Severity Index (ISI)",
                                                                                                            "Mindful Attention Awareness Scale (MAAS)",
                                                                                                            "Obsessive Compulsive Inventory - Revised (OCI-R)",
                                                                                                            "Pain Self-Perception Scale (PSPS)",
                                                                                                            "Panic Disorder Severity Scale-Self-Report (PDSS-SR)",
                                                                                                            "Patient Health Questionnaire-9 (PHQ-9)",
                                                                                                            "Perceived Stress Scale - 10 (PSS-10)",
                                                                                                            "Prodromal Questionnaire - Brief (PQ-B)",
                                                                                                            "Psychological Inflexibility in Pain Scale (PIPS)",
                                                                                                            "PTSD Checklist for DSM-5 (PCL-5)",
                                                                                                            "Severity of Dependence Scale (SDS)",
                                                                                                            "Social Interaction Anxiety Scale (SIAS)",
                                                                                                            "Social Phobia Scale (SPS)",
                                                                                                            "Somatic Symptom Disorder - B Criteria Scale (SSD-12)",
                                                                                                            "Tampa Scale of Kinesiophobia (TSK)"), width = '400px'),
                                         h5(tags$strong("Step 2:")),
                                         actionButton("Action_Scale_Data", "Submit Data", width = '270px'),
                                         h5(tags$strong("Step 3:")),
                                         downloadButton("Scale_Report","Generate Report", width = '270px'),
                                         hr(),
                                         h4(tags$strong("Analytics Report: Practice Demographics & Performance")),
                                         h5(tags$strong("Step 1:")),
                                         actionButton("Action_Basic_Data", "Submit Data", width = '270px'),
                                         h5(tags$strong("Step 2:")),
                                         downloadButton("Basic_Report", "Generate Report", width = '270px')
                            ),
                            mainPanel()
                          )
                       )
                 
)



server <- function(input, output, session) {
  
  
  options(shiny.maxRequestSize=30*1024^2)

  #Import the data and convert character variables to vactor
  
  Import<- reactive({
    
    Infile<- input$Analytics_File
    if (is.null(Infile))
      return(NULL)
    Import_Df<- read_csv(Infile$datapath, na=c(""," ","NA"))
  
    Import_Df$Entry.Date<- strptime(as.character(Import_Df$Entry.Date), "%d/%m/%Y")
    Import_Df$Entry.Date<- as.factor(format(as.Date(Import_Df$Entry.Date), "%b-%y"))
    Import_Df$Entry.Date<- factor(Import_Df$Entry.Date, levels = Import_Df$Entry.Date)
    
    
    Import_Df<- dplyr::select(Import_Df, Entry.Date, everything())
    
  })
  
  
  
  output$Report_Period<- renderUI({
    
    Period_Vars<- levels(Import()$Entry.Date)
    
    if(input$Period_Filter == "Yes") {
      selectizeInput("Select_Period", h5(tags$em("Select Reporting Period(s)")), multiple = TRUE,
                     choices  = Period_Vars)
    }
  })
  
  
  
  Analytics<- reactive({
    
    SpreadSheet<- Import()
    
    Periods<<-input$Select_Period
    
    isolate(
    if(input$Period_Filter == "Yes") {
      SpreadSheet<- dplyr::filter(SpreadSheet, Entry.Date %in% Periods)
      
    }
    )
    
    if(input$Filter == "Yes"){
      SpreadSheet<- filterdata(SpreadSheet,  input$Filter_Express)
    } 
    
  
    SpreadSheet<<- as.data.frame(SpreadSheet)
    

  })
  
  
  observe({
  
    Analytics() 
    
    updateSelectizeInput(session, "Cat_Vars_Freq_Tab", choices = names(SpreadSheet), options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button")))
    updateSelectizeInput(session, "Cat_Vars_Freq_Plot", choices = names(SpreadSheet), options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button")))
    updateSelectizeInput(session, "Num_Vars_Sum_Tab", choices = names(SpreadSheet), options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button")))
    updateSelectizeInput(session, "Cat_Vars_Sum_Tab", choices = names(SpreadSheet), options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button")))
    updateSelectizeInput(session, "Num_Vars_Sum_Plot", choices = names(SpreadSheet), options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button")))
    updateSelectizeInput(session, "Cat_Vars_Sum_Plot", choices = names(SpreadSheet), options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button")))
    
    updateSelectInput(session, "T1_Benchmark_Var", choices = names(select(SpreadSheet, contains("First.Score"))))
    updateSelectInput(session, "T2_Benchmark_Var", choices = names(select(SpreadSheet, contains("Last.Score"))))
    
    updateSelectizeInput(session, "Corr_Variables", choices = names(select_if(SpreadSheet, is.numeric)), options = list(maxItems = Inf, plugins = list("drag_drop", "remove_button")))
    
    
  })
  
  
  #Create Widgt to specify variables appearing in datatable
  
  output$Ind_Vars1<- renderUI({
    
    if(input$Ind_All_Vars == "Yes") {
      div(
      selectizeInput("Select_Vars", h5(tags$em("Select Variables to View")), multiple = TRUE,
                     choices  = names(Import())),
      h5(tags$em("Use quotation marks (e.g. 'Age')"))
      )
    }
    
  })
  
  
  Show_Data_Reactive<- eventReactive(input$Show, {
    
    Analytics()
    
  })
  
  
  
  #Render the data and create filtering capacity 
  
  output$Entire_Spreadsheet<- DT::renderDataTable({
    
    if(input$Ind_All_Vars == "No") {
      DT::datatable(Show_Data_Reactive(),extensions = 'FixedColumns', rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10000, dom = "t", fixedColumns = list(leftColumns = 2)))
    } else if(input$Ind_All_Vars == "Yes") {
      DT::datatable(Show_Data_Reactive()[,input$Select_Vars, drop = FALSE], extensions = 'FixedColumns', rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10000, dom = "t", fixedColumns = list(leftColumns = 2)))
    }
    
  })
  
  
  
  #Render the normalise Widgt based on whether user wants percentages
  
  output$Freq_Norm<- renderUI({
    if(is.null(input$Cat_Vars_Freq_Tab))
      return()
    
    Cat<- input$Cat_Vars_Freq_Tab
    
    if(length(Cat) > 1 & input$Freq_Perc == TRUE) {
      selectInput("Freq_Norm", "Calculate Percentage as Proportion of:", choices = c("Row Variable(s)" = "row", "Column Variable" = "column"))
    }
    
  })
  
  
  #Create and render the pivot table
  
  Piv_Tab_Reac<- reactive({
    
    if(is.null(input$Cat_Vars_Freq_Tab))
      return()
    
    Cat<- input$Cat_Vars_Freq_Tab
    Percent<- input$Freq_Perc
    
    if(input$Freq_Perc == TRUE) {
      One_Var_Norm<- "total"
    } else if(input$Freq_Perc == FALSE) {
      One_Var_Norm<- "none"
    }
    
    if(input$Freq_Perc == FALSE) {
      Multi_Var_Norm<- "none"
    } else if (input$Freq_Perc == TRUE) {
      Multi_Var_Norm<- input$Freq_Norm
    }
    
    if(length(Cat) == 1) {
      pivotr("SpreadSheet", cvars = c(paste0(Cat)), normalize = paste0(One_Var_Norm)) %>% dtab(perc = Percent, dec = 0)
    } else if(length(Cat) > 1) {
      pivotr("SpreadSheet", cvars = c(paste0(Cat)), normalize = paste0(Multi_Var_Norm)) %>% dtab(perc = Percent, dec = 0) 
    }
    
  })
  
  
  
  output$Freq_Tab<- renderDataTable({
    
    validate(need(Piv_Tab_Reac(), ""))
    
    Piv_Tab_Reac()
    
  })
  

  
  #Create the frequencies plots
  
  Freq_Plots_Reac<- reactive({
    
    validate(need(input$Cat_Vars_Freq_Plot, ""))
    
    Cat<- input$Cat_Vars_Freq_Plot 
    
    m = list( l = 80, r = 200, b = 80, t = 100, pad = 0 )
    
    if(length(Cat) == 1) {
      
      
      Var1<- SpreadSheet[,paste0(Cat[1])]
      Df_Freq<- data.frame(Var1)
      Df_Freq<- na.omit(Df_Freq)
      names(Df_Freq)<-  paste0(Cat[1])
      
      
      Plot_Freq <- ggplot(Df_Freq, aes_string(x = paste0(Cat[1]))) + 
        geom_bar() + 
        scale_fill_brewer(palette = "Set3") + xlab(paste0(Cat[1]))
      
      ggplotly() %>% layout(autosize = F, margin = m)
      
      
      
    } else if(length(Cat) == 2) {
      
      Var1<- SpreadSheet[,paste0(Cat[1])]
      Var2<- SpreadSheet[,paste0(Cat[2])]
      Df_Freq<- data.frame(Var1, Var2) 
      Df_Freq<- na.omit(Df_Freq)
      names(Df_Freq)<-  c(paste0(Cat[1]), paste0(Cat[2])) 
      
      Plot_Freq <- ggplot(Df_Freq, aes_string(x = paste0(Cat[1]), fill = paste0(Cat[2]))) + 
        geom_bar() + xlab(paste0(Cat[1])) + theme(axis.text.x = element_text(angle = 20, hjust = 1)) 
      
      ggplotly() %>% layout(autosize = F, margin = m)
      
    } else if(length(Cat) > 2) {
      
      Var1<- SpreadSheet[,paste0(Cat[1])]
      Var2<- SpreadSheet[,paste0(Cat[2])]
      Var3<- SpreadSheet[,paste0(Cat[3])]
      Df_Freq<- data.frame(Var1, Var2, Var3) 
      Df_Freq<- na.omit(Df_Freq)
      names(Df_Freq)<-  c(paste0(Cat[1]), paste0(Cat[2]), paste0(Cat[3])) 
      
      Plot_Freq <- ggplot(Df_Freq, aes_string(x = paste0(Cat[1]), fill = paste0(Cat[2]))) + 
        geom_bar() + facet_grid(.~Df_Freq[,3]) + xlab(paste0(Cat[1])) + theme(axis.text.x = element_text(angle = 20, hjust = 1)) 
      
      ggplotly() %>% layout(autosize = F, margin = m)
      
    }
    
    
  })
  
  
  output$Freq_Plot<- renderPlotly({
    
    if(is.null(Freq_Plots_Reac()))
      return(NULL)
    
    Freq_Plots_Reac()
    
  })
  

  
  output$Sum_Tab<- renderDataTable({
    
    validate(need(input$Sum_Funs, ""))
    
    Num<- input$Num_Vars_Sum_Tab
    Cat<- input$Cat_Vars_Sum_Tab
    Fun<- input$Sum_Funs
    
    explore("SpreadSheet", vars = paste0(Num), byvar = paste0(Cat), fun = paste0(Fun), shiny = TRUE) %>% dtab(dec = 2)
    
  })
  

  
  output$Sum_Assoc_Factor<- renderUI({
    
    
    if(input$Sum_Plot_Type == "Explore Associations"){
      checkboxInput("Sum_Assoc_Fac", h5(tags$strong("Examine Associations Within Levels of Categorical Variables")))
    }
    
  })
  
  
  output$Sum_Text_Select_Cat<- renderUI({
    
    validate(need(input$Sum_Assoc_Fac, ""))
    
    if(input$Sum_Assoc_Fac == TRUE) {
      h5(tags$em("Select 1 categorical variable above"))
    }
    
  }) 
  
  
  
  #Reactive expression to generate graphs 
  
  Sum_Plots_Reac<- reactive({ 
    
    
    Cat<- input$Cat_Vars_Sum_Plot
    Num<- input$Num_Vars_Sum_Plot
    
    #Bar graphs to compare means
    
    if(input$Sum_Plot_Type == "Compare Averages") {
      
      if(is.null(input$Cat_Vars_Sum_Plot))
        return(NULL)
      
      if(is.null(input$Num_Vars_Sum_Plot))
        return(NULL)
      
      if(length(Cat) < 2) {
        
        Var1<- SpreadSheet[,paste0(Cat[1])]
        Var2<- SpreadSheet[,paste0(Num[1])]
        
        Df_Comp_Means<- data.frame(Var1, Var2)
        names(Df_Comp_Means)<- c(paste0(Cat[1]), paste0(Num[1]))
        Df_Comp_Means<- na.omit(Df_Comp_Means)
        
        m = list( l = 80, r = 200, b = 80, t = 100, pad = 0 )
        
        Plot_Comp_Means<- ggplot(Df_Comp_Means) + geom_bar(aes_string(paste0(Cat[1]), paste0(Num[1])), 
                                                           position = "dodge", stat = "Summary", fun.y = "mean")
        
        ggplotly() %>% layout(autosize = F, margin = m)
        
      } else if(length(Cat) == 2) {
        
        Var1<- SpreadSheet[,paste0(Cat[1])]
        Var2<- SpreadSheet[,paste0(Cat[2])]
        Var3<- SpreadSheet[,paste0(Num[1])]
        
        Df_Comp_Means<- data.frame(Var1, Var2, Var3)
        names(Df_Comp_Means)<- c(paste0(Cat[1]), paste0(Cat[2]), paste0(Num[1])) 
        Df_Comp_Means<- na.omit(Df_Comp_Means)
        
        m = list( l = 80, r = 200, b = 80, t = 100, pad = 0 )
        
        Plot_Comp_Means<- ggplot(Df_Comp_Means) + 
          geom_bar(aes_string(paste0(Cat[1]), paste0(Num[1]), fill = paste0(Cat[2])), 
                   position = "dodge", stat = "Summary", fun.y = "mean")
        ggplotly() %>% layout(autosize = F, margin = m)
        
        
      } else if(length(Cat)  > 2) {
        
        Var1<- SpreadSheet[,paste0(Cat[1])]
        Var2<- SpreadSheet[,paste0(Cat[2])]
        Var3<- SpreadSheet[,paste0(Cat[3])]
        Var4<- SpreadSheet[,paste0(Num[1])]
        
        Df_Comp_Means<- data.frame(Var1, Var2, Var3, Var4)
        names(Df_Comp_Means)<- c(paste0(Cat[1]), paste0(Cat[2]), paste0(Cat[3]), paste0(Num[1]))
        Df_Comp_Means<- na.omit(Df_Comp_Means)
        
        m = list( l = 120, r = 200, b = 160, t = 20, pad = 0)
        
        Plot_Comp_Means<- ggplot(Df_Comp_Means) + 
          geom_bar(aes_string(paste0(Cat[1]), paste0(Num[1]), fill = paste0(Cat[2])), 
                   position = "dodge", stat = "Summary", fun.y = "mean")
        
        Plot_Comp_Means<- Plot_Comp_Means + facet_grid(.~ Df_Comp_Means[,3]) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
        
        ggplotly() %>% layout(autosize = F, margin = m) 
        
        
      }
      
    } else if (input$Sum_Plot_Type == "Explore Associations") {
      
      
      
      if(length(Num) > 1 & length(Cat) == 0) {
        
        Var1<- SpreadSheet[,paste0(Num[1])]
        Var2<- SpreadSheet[,paste0(Num[2])]
        Df_Assoc<- data.frame(Var1, Var2)
        names(Df_Assoc)<- c(paste0(Num[1]), paste0(Num[2]))
        Df_Assoc<- na.omit(Df_Assoc)
        
        m = list( l = 80, r = 200, b = 80, t = 100, pad = 0 )
        
        Plot_Assoc<- ggplot(Df_Assoc, aes_string(x = paste0(Num[1]), y = paste0(Num[2]))) + geom_point() +
          geom_smooth(method = "lm", se = FALSE)
        
        ggplotly() %>% layout(autosize = F, margin = m) 
        
        
      } else if(length(Num) > 1 & length(Cat) >= 1) {
        
        Var1<- SpreadSheet[,paste0(Num[1])]
        Var2<- SpreadSheet[,paste0(Num[2])]
        Var3<- SpreadSheet[,paste0(Cat[1])]
        Df_Assoc<- data.frame(Var1, Var2, Var3)
        names(Df_Assoc)<- c(paste0(Num[1]), paste0(Num[2]), paste0(Cat[1]))
        Df_Assoc<- na.omit(Df_Assoc)
        
        m = list( l = 80, r = 200, b = 80, t = 100, pad = 0 )
        
          
          Plot_Assoc<- ggplot(Df_Assoc, aes_string(x = paste0(Num[1]), y = paste0(Num[2]), group = paste0(Cat[1]),
                                                   color = paste0(Cat[1]))) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
          
          ggplotly() %>% layout(autosize = F, margin = m)
          
        
        
      } 
      
      
      
    } else if(input$Sum_Plot_Type == "View Variable Distributions") {
      
   
      if(length(Num) == 1) {
        
        Var1<- SpreadSheet[,paste0(Num[1])]
        Df_Dist<- data.frame(Var1)
        names(Df_Dist)<- paste0(Num[1])
        Df_Dist<- na.omit(Df_Dist)
        
        m = list( l = 80, r = 200, b = 80, t = 100, pad = 0 )
        
        Plot_Dist<- ggplot(Df_Dist, aes_string(x = paste0(Num[1]))) + geom_histogram()
        
        ggplotly() %>% layout(autosize = F, margin = m)
        
        
      }
      
    }
    
  })
  
  output$Sum_Plot<- renderPlotly({
    
    validate(need(Sum_Plots_Reac(), ""))
    
    
    Sum_Plots_Reac()
    
  })
  
  
  #Correlations 
  
  
  Correlation <- reactive({
    data <- SpreadSheet
    Corr_Variables <- input$Corr_Variables
    if(is.null(data) || !length(intersect(Corr_Variables, colnames(data)))) {
      NULL
    } else {
      cor(SpreadSheet[,input$Corr_Variables], use = input$Corr_Use, method = input$Corr_Method)
    }
  })
  
  Sig_Conf_Mat <- reactive({
    val <- Correlation()
    if(!is.null(val))
      corTest(val, 0.95)
  })
  
  
  output$Corr_Plot <- renderPlot({
    val <- Correlation()
    if(is.null(val)) return(NULL)
    
    val[is.na(val)] <- 0
    Args <- list(val,
                 order = "original", 
                 p.mat = Sig_Conf_Mat()[[1]],
                 sig.level = if(input$Sig_Test) 0.05 else NULL,
                 insig = if(input$Sig_Test) input$Sig_Action else NULL,
                 lowCI.mat = Sig_Conf_Mat()[[2]],
                 uppCI.mat = Sig_Conf_Mat()[[3]],
                 plotCI = "n"
    )
    
    do.call(corrplot, c(list(method = input$Plot_Method, type = input$Plot_Type), Args))
    
  })
  
#Benchmarking
  
  Benchmark_Reac<- reactive({
    
    B1<- SpreadSheet[,input$T1_Benchmark_Var]
    B2<- SpreadSheet[,input$T2_Benchmark_Var]
    
    Benchmark<- as.numeric(input$Benchmark)
    
    Percentages_Convert<- as.numeric(B1* Benchmark)
    
    Data_Cases<- length(Percentages_Convert[!is.na(Percentages_Convert)])
    
    if(input$Direction == "Reduction") {
      Comparison_Vector<- B1 - Percentages_Convert
      Initial_Benchmark_Df<- data.frame(B2, Comparison_Vector)
      True_Cases<- filter(Initial_Benchmark_Df, B2 < Comparison_Vector)
      } 
    else if(input$Direction == "Increase") {
      Comparison_Vector<- B1 + Percentages_Convert
      Initial_Benchmark_Df<- data.frame(B2, Comparison_Vector)
      True_Cases<- filter(Initial_Benchmark_Df, B2 > Comparison_Vector)
      }
    
    
    N_True_Cases<- nrow(True_Cases)
    
    Percentage_True_Cases<- paste0(100*(N_True_Cases/Data_Cases), "%")
    
    Benchmark_Df<- data.frame("N" = N_True_Cases, "Percentage" = Percentage_True_Cases)
    
  })
  
  
  
  Benchmark_E_Reac<- eventReactive(input$Show_Benchmark_Data, {
    
    Benchmark_Reac()
    
  })
  
  
  output$Benchmark_Table<- DT::renderDataTable({
    
    DT::datatable(Benchmark_E_Reac(), extensions = 'FixedColumns', caption = "Number of cases that achieved the benchmark percentage improvement.", rownames = FALSE, options = list(scrollX = TRUE, dom = "t", fixedColumns = list(leftColumns = 2)))
    
  })
  
  
  
  
  Basic_Report_Data<- reactive({
    
    Report_Df<<-  Show_Data_Reactive()
    
    Report_Df<<-   Report_Df %>% mutate_if(is.character,as.factor)
    
    Demographs_Factor_Df<- dplyr::select(Report_Df, Sex, Sexuality, Relationship.Status, Workforce.Status, Education, Suburb, Attendance.Arrangement,
                                  Attendance.Quality, Principal.Diagnosis, Secondary.Diagnosis, Referrer, Therapy, Early.Dropout)
    
    
    Sex_Df<<-Demographs_Factor_Df %>% count(Sex) %>% mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%")) %>% dplyr::rename(., N = n, V = Sex)
    Sexuality_Df<<-Demographs_Factor_Df %>% count(Sexuality) %>% mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%")) %>% dplyr::rename(., N = n, V = Sexuality)
    Relationship_Df<<-Demographs_Factor_Df %>% count(Relationship.Status) %>% mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%")) %>% dplyr::rename(., N = n, V = Relationship.Status)
    Workforce_Df<<-Demographs_Factor_Df %>% count(Workforce.Status) %>% mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%")) %>% dplyr::rename(., N = n, V = Workforce.Status)
    Education_Df<<-Demographs_Factor_Df %>% count(Education) %>% mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%")) %>% dplyr::rename(., N = n, V = Education)
    Suburb_Df<<-Demographs_Factor_Df %>% count(Suburb) %>% mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%")) %>% dplyr::rename(., N = n, V = Suburb)
    Basic_Dem_Df<<- rbind(Sex_Df,Sexuality_Df, Relationship_Df, Workforce_Df,Education_Df,Suburb_Df)
    names(Basic_Dem_Df)<<- c("Categorical Variables","N","Percentage")
    
    
    Attendance_Arr_Df<<-Demographs_Factor_Df %>% count(Attendance.Arrangement) %>% mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%")) %>% dplyr::rename(., N = n, V = Attendance.Arrangement)
    Attendance_Qual_Df<<-Demographs_Factor_Df %>% count(Attendance.Quality) %>% mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%")) %>% dplyr::rename(., N = n, V = Attendance.Quality)
    Prin_Diagnosis_Df<<-Demographs_Factor_Df %>% count(Principal.Diagnosis) %>% mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%")) %>% dplyr::rename(., N = n, V = Principal.Diagnosis)
    Sec_Diagnosis_Df<<-Demographs_Factor_Df %>% count(Secondary.Diagnosis) %>% mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%")) %>% dplyr::rename(., N = n, V = Secondary.Diagnosis)
    Referrer_Df<<-Demographs_Factor_Df %>% count(Referrer) %>% mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%")) %>% dplyr::rename(., N = n, V = Referrer)
    Therapy_Df<<-Demographs_Factor_Df %>% count(Therapy) %>% mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%")) %>% dplyr::rename(., N = n, V = Therapy)
    Dropout_Df<<-Demographs_Factor_Df %>% count(Early.Dropout) %>% mutate(Percentage = paste0(round(100 * n/sum(n), 0), "%")) %>% dplyr::rename(., N = n, V = Early.Dropout)
    Clinical_Dem_Df<<- rbind(Prin_Diagnosis_Df,Sec_Diagnosis_Df,Therapy_Df,Referrer_Df,Attendance_Arr_Df,Attendance_Qual_Df,Dropout_Df)
    names(Clinical_Dem_Df)<<- c("Categorical Variables", "N", "Percentage")
    

    Num_Dem_Df<-  select(Report_Df, Age, Children) %>%
      summarise_each(funs(
        Mean = mean(., na.rm = TRUE), 
        Sd = sd(., na.rm = TRUE), 
        Min = min(., na.rm = TRUE), 
        Max = max(., na.rm = TRUE)))
    
    
    Num_Dem_Df<<-Num_Dem_Df %>% gather(stat, val) %>%
      separate(stat, into = c("Variable", "stat"), sep = "_") %>%
      spread(stat, val) %>%
      select(Variable, Mean, Sd, Min, Max) %>% mutate_if(is.numeric, round, 2) %>% rename(., "Numeric Variables" = Variable)
    
    
    Num_Dem_Clin_Df<<-  dplyr::summarise(Report_Df, Variable = "Therapy Duration", Mean = mean(Therapy.Duration, na.rm = TRUE), Sd = sd(Therapy.Duration, na.rm = TRUE), 
                                        Min = min(Therapy.Duration), Max = max(Therapy.Duration))  %>% mutate_if(is.numeric, round, 2) %>% rename(., "Numeric Variables" = Variable)
    
    
    #Custom variables
    
    Custom_Vars_Df<<- select(Report_Df, contains("Custom"))
    
    if(!is.null(Custom_Vars_Df)) {
      
      names(Custom_Vars_Df)<<- stri_replace_last_regex(names(Custom_Vars_Df),"\\.Custom$","")
      
      Custom_Fac_Df<- select_if(Custom_Vars_Df, is.factor)
    
    if(ncol(Custom_Fac_Df) >= 1) {
      Custom_Table<<-  as.data.frame(Custom_Fac_Df) %>%  
      select_if(is.factor) %>% 
      gather("Outcome", "Value") %>% 
      group_by(Outcome, Value) %>%
      summarise(count = n()) %>% 
      mutate(Percentage = paste0(round(100*count/sum(count), 0), "%" )) %>% dplyr::rename(., N = count, "Categorical Variables" = Outcome)
      
    } else if(ncol(Custom_Fac_Df) == 0) {
      
      Custom_Table<<- NULL
    }
    
    Custom_Num_Table<<-  select_if(Custom_Vars_Df, is.numeric)

    if(ncol(Custom_Num_Table) > 1) {
    Custom_Num_Table<-  Custom_Num_Table %>%
      select_if(is.numeric) %>%
      summarise_each(funs(
                          Mean = mean(., na.rm = TRUE), 
                          Sd = sd(., na.rm = TRUE), 
                          Min = min(., na.rm = TRUE), 
                          Max = max(., na.rm = TRUE)))
    

    Custom_Num_Table<<-Custom_Num_Table %>% gather(stat, val) %>%
      separate(stat, into = c("Variable", "stat"), sep = "_") %>%
      spread(stat, val) %>%
      select(Variable, Mean, Sd, Min, Max) %>% mutate_if(is.numeric, round, 2) %>% rename(., "Numeric Variables" = Variable)
    
    } else if(ncol(Custom_Num_Table) == 1) {
      
      Custom_Num_Table<<-  dplyr::summarise(Custom_Num_Table, Variable = names(Custom_Num_Table[1]), Mean = mean(Custom_Num_Table[,1], na.rm = TRUE), Sd = sd(Custom_Num_Table[,1], na.rm = TRUE), 
                                           Min = min(Custom_Num_Table[,1], na.rm = TRUE), Max = max(Custom_Num_Table[,1], na.rm = TRUE))  %>% mutate_if(is.numeric, round, 2) %>% rename(., "Numeric Variables" = Variable)
      
    } else if(ncol(Custom_Num_Table) == 0) {
      Custom_Num_Table<<- NULL
    } 
    
    }  
    
    #Set up initial df for improvment calculations
    Improve_Df<- dplyr::select(Report_Df, contains("Improvement"))
    Improve_Dx_Df<- dplyr::select(Report_Df, Principal.Diagnosis, contains("Improvement"))
    
    #Non sig improvement df
    Imp_Df<<-dplyr::select(Improve_Df, -contains("Sig.Improvement"))
    
    #Nonsig improvement df by diagnosis
    Imp_Dx_Df<- dplyr::select(Improve_Dx_Df, -contains("Sig.Improvement"))
 
    #Sig improvement df
    
    Sig_Imp_Df<<-dplyr::select(Improve_Df, contains("Sig.Improvement"))
    
    #Sig improvmeent df by diagnosis
    Sig_Imp_Dx_Df<-dplyr::select(Improve_Dx_Df, Principal.Diagnosis, contains("Sig.Improvement"))
    
    #Create table showing non-sig global improvement 
    Global_Improvement_Df<- filter_at(Imp_Df, vars(ends_with("Improvement")), any_vars(. == "Yes"))
    N<- nrow(Global_Improvement_Df) 
    Percentage<- paste0(round(100 * nrow(Global_Improvement_Df)/nrow(Report_Df), digits = 0), "%")
    Global_Improvement_Df<<-cbind("Symptom Improvement", N, Percentage)
    
    #Create table showing sig global improvement 
    Global_Sig_Improvement_Df<- filter_at(Sig_Imp_Df, vars(ends_with("Improvement")), any_vars(. == "Yes"))
    N<- nrow(Global_Sig_Improvement_Df) 
    Percentage<- paste0(round(100 * nrow(Global_Sig_Improvement_Df)/nrow(Report_Df), digits = 0), "%")
    Global_Sig_Improvement_Df<<-cbind("Statistically Significant Symptom Improvement", N, Percentage)
    
    #Create table showing non-sig global improvement by diagnosis
    Dx_Totals<<- Report_Df %>% count(Principal.Diagnosis) 
    Global_Improvement_Dx_Df<<- filter_at(Imp_Dx_Df, vars(ends_with("Improvement")), any_vars(. == "Yes")) 
    Global_Improvement_Dx_Df<<- Global_Improvement_Dx_Df %>% dplyr::group_by(Principal.Diagnosis) %>%  dplyr::summarise(n = n()) %>% complete(Principal.Diagnosis)
    P<<- paste0(round(100*Global_Improvement_Dx_Df$n/Dx_Totals$n, 0), "%")
    Global_Improvement_Dx_Df<<- cbind(Global_Improvement_Dx_Df, P) %>% dplyr::rename(., "Primary Diagnosis" = Principal.Diagnosis, N = n, Percentage = P)
    
    #Create table showing sig global improvement by diagnosis
    Global_Sig_Improvement_Dx_Df<<- filter_at(Sig_Imp_Dx_Df, vars(ends_with("Sig.Improvement")), any_vars(. == "Yes"))
    Global_Sig_Improvement_Dx_Df<<- Global_Sig_Improvement_Dx_Df %>% dplyr::group_by(Principal.Diagnosis) %>%  dplyr::summarise(n = n()) %>% complete(Principal.Diagnosis)
    P_Sig<<- paste0(round(100*Global_Sig_Improvement_Dx_Df$n/Dx_Totals$n, 0), "%")
    Global_Sig_Improvement_Dx_Df<<- cbind(Global_Sig_Improvement_Dx_Df, P_Sig) %>% dplyr::rename(., "Primary Diagnosis" = Principal.Diagnosis, N = n, Percentage = P_Sig)
    
  })
  
  
  
  Generate_Basic_Report<- observeEvent(input$Action_Basic_Data, {
    
    Basic_Report_Data()
    
  })
  
  
  
  output$Basic_Report<- downloadHandler(
    
    filename = paste0(" Basic Analytics Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Basic_Report.RMD")
      file.copy("Basic_Report.RMD", tempReport, overwrite = TRUE)
      
      # Pass data objects to Rmd document
      params <- list(
        Sex_Df = Sex_Df,
        Sexuality_Df = Sexuality_Df,
        Relationship_Df = Relationship_Df,
        Workforce_Df = Workforce_Df,
        Education_Df = Education_Df,
        Suburb_Df = Suburb_Df,
        Basic_Dem_Df = Basic_Dem_Df,
        Attendance_Arr_Df = Attendance_Arr_Df,
        Attendance_Qual_Df = Attendance_Qual_Df,
        Prin_Diagnosis_Df = Prin_Diagnosis_Df,
        Sec_Diagnosis_Df = Sec_Diagnosis_Df,
        Therapy_Df = Therapy_Df,
        Referrer_Df = Referrer_Df,
        Dropout_Df = Dropout_Df,
        Clinical_Dem_Df = Clinical_Dem_Df,
        Num_Dem_Df,
        Num_Dem_Clin_Df,
        Global_Improvement_Df = Global_Improvement_Df,
        Global_Sig_Improvement_Df,
        Global_Improvement_Dx_Df,
        Global_Sig_Improvement_Dx_Df,
        Custom_Vars_Df = Custom_Vars_Df,
        Custom_Table = Custom_Table, 
        Custom_Num_Table = Custom_Num_Table
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
  
  

  
  Scale_Report_Data<- reactive({
    
    Scale_Report_Df<<- Analytics()
    
    #Set up initial df for improvmeent calculations
    Improve_Df<- dplyr::select(Scale_Report_Df, contains("Improvement"))
    
    #Non sig improvement df
    Imp_Df<<-dplyr::select(Improve_Df, -contains("Sig.Improvement"))
    
    #Sig improvement df
    
    Sig_Imp_Df<<-dplyr::select(Improve_Df, contains("Sig.Improvement"))

  })
  
  
  Generate_Scale_Report<- observeEvent(input$Action_Scale_Data, {
    
    Scale_Report_Data()
    
  })
  
  
  #Create pdf download functionality
  
  
  output$Scale_Report<- downloadHandler(
    
    filename = paste0(" Outcome Measure Analytics Report ", format(Sys.time(), '%d/%m/%y'),".pdf"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), switch(input$Report_Select, "Assessment of Quality of Life - 8 Dimensions (AQoL-8D)" = "AQOL-8D_Report.Rmd", 
                                                "Borderline Personality Questionnaire (BPQ)" = "BPQ_Report.Rmd",
                                                "Chronic Pain Acceptance Questionnaire (CPAQ)" = "CPAQ_Report.Rmd",
                                                "Depression Anxiety Stress Scales - 21 (DASS-21)" = "DASS21_Report.Rmd",
                                                "Eating Disorder Examination Questionnaire (EDE-Q)" = "EDEQ_Report.Rmd",
                                                "Edinborough Postnatal Depression Scale (EDPS)" = "EDPS_Report.Rmd",
                                                "Generalized Anxiety Disorder 7-Item Scale (GAD-7)" = "GAD7_Report.Rmd",
                                                "Geriatric Depression Scale - 15 (GDS-15)" = "GDS15_Report.Rmd",
                                                "Illness Attitude Scales (IAS)" = "IAS_Report.Rmd",
                                                "Injustice Experience Questionnaire (IEQ)" = "IEQ_Report.Rmd",
                                                "Insomnia Severity Index (ISI)" = "ISI_Report.Rmd",
                                                "Mindful Attention Awareness Scale (MAAS)" = "MAAS_Report.Rmd",
                                                "Obsessive Compulsive Inventory - Revised (OCI-R)" = "OCIR_Report.Rmd",
                                                "Pain Self-Perception Scale (PSPS)" = "PSPS_Report.Rmd",
                                                "Panic Disorder Severity Scale-Self-Report (PDSS-SR)" = "PDSSSR_Report.Rmd",
                                                "Patient Health Questionnaire-9 (PHQ-9)" = "PHQ9_Report.Rmd",
                                                "Perceived Stress Scale - 10 (PSS-10)" = "PSS10_Report.Rmd",
                                                "Prodromal Questionnaire - Brief (PQ-B)" = "PQB_Report.Rmd",
                                                "Psychological Inflexibility in Pain Scale (PIPS)" = "PIPS_Report.Rmd",
                                                "PTSD Checklist for DSM-5 (PCL-5)" = "PCL5_Report.Rmd",
                                                "Severity of Dependence Scale (SDS)" = "SDS_Report.Rmd",
                                                "Social Interaction Anxiety Scale (SIAS)" = "SIAS_Report.Rmd",
                                                "Social Phobia Scale (SPS)" = "SPS_Report.Rmd",
                                                "Somatic Symptom Disorder - B Criteria Scale (SSD-12)" = "SSD12_Report.Rmd",
                                                "Tampa Scale of Kinesiophobia (TSK)" = "TSK_Report.Rmd"))
      
      file.copy(switch(input$Report_Select, "Assessment of Quality of Life - 8 Dimensions (AQoL-8D)" = "AQOL-8D_Report.Rmd",  
                                            "Borderline Personality Questionnaire (BPQ)" = "BPQ_Report.Rmd", 
                                            "Chronic Pain Acceptance Questionnaire (CPAQ)" = "CPAQ_Report.Rmd",
                                            "Depression Anxiety Stress Scales - 21 (DASS-21)" = "DASS21_Report.Rmd",
                                            "Eating Disorder Examination Questionnaire (EDE-Q)" = "EDEQ_Report.Rmd",
                                            "Edinborough Postnatal Depression Scale (EDPS)" = "EDPS_Report.Rmd",
                                            "Generalized Anxiety Disorder 7-Item Scale (GAD-7)" = "GAD7_Report.Rmd",
                                            "Geriatric Depression Scale - 15 (GDS-15)" = "GDS15_Report.Rmd",
                                            "Illness Attitude Scales (IAS)" = "IAS_Report.Rmd",
                                            "Injustice Experience Questionnaire (IEQ)" = "IEQ_Report.Rmd",
                                            "Insomnia Severity Index (ISI)" = "ISI_Report.Rmd",
                                            "Mindful Attention Awareness Scale (MAAS)" = "MAAS_Report.Rmd",
                                            "Obsessive Compulsive Inventory - Revised (OCI-R)" = "OCIR_Report.Rmd",
                                            "Pain Self-Perception Scale (PSPS)" = "PSPS_Report.Rmd",
                                            "Panic Disorder Severity Scale-Self-Report (PDSS-SR)" = "PDSSSR_Report.Rmd",
                                            "Patient Health Questionnaire-9 (PHQ-9)" = "PHQ9_Report.Rmd",
                                            "Perceived Stress Scale - 10 (PSS-10)" = "PSS10_Report.Rmd",
                                            "Prodromal Questionnaire - Brief (PQ-B)" = "PQB_Report.Rmd",
                                            "Psychological Inflexibility in Pain Scale (PIPS)" = "PIPS_Report.Rmd",
                                            "PTSD Checklist for DSM-5 (PCL-5)" = "PCL5_Report.Rmd",
                                            "Severity of Dependence Scale (SDS)" = "SDS_Report.Rmd",
                                            "Social Interaction Anxiety Scale (SIAS)" = "SIAS_Report.Rmd",
                                            "Social Phobia Scale (SPS)" = "SPS_Report.Rmd",
                                            "Somatic Symptom Disorder - B Criteria Scale (SSD-12)" = "SSD12_Report.Rmd",
                                            "Tampa Scale of Kinesiophobia (TSK)" = "TSK_Report.Rmd"), tempReport, overwrite = TRUE)
      
      # Pass data objects to Rmd document
      params <- list(
        Scale_Report_Df =  Scale_Report_Df,
        Imp_Df = Imp_Df,
        Sig_Imp_Df =  Sig_Imp_Df,
        Periods = Periods
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
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

