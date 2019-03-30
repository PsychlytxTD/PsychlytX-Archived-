library(shinydashboard)
library(magrittr)
library(purrr)
library(tidyr)
library(dplyr)
library(lubridate)
library(chron)
library(shinyjs)
library(RPostgreSQL)
library(DBI)
library(pool)
library(ggplot2)
library(postGIStools)
library(DT)
library(shinyjs)

pool <- dbPool( #Set up the pool connection management
  drv = dbDriver("PostgreSQL"),
  dbname = "scaladb",
  host = "scaladb.cdanbvyi6gfm.ap-southeast-2.rds.amazonaws.com",
  user = "jameslovie",
  password = "e2534e41-bbb6-4e2b-b687-71c5be7c7d35"
)

onStop(function() { 
                    #Close pool object when session ends
  poolClose(pool)
  
})


clinician_id<- '12345.000000' #Temp storage of client and clinician id


################################ From db, pull in clients associated with the clinician, to be displayed in a dropdown

client_list_sql<- "SELECT clinician_id, client_id, first_name, last_name, birth_date
                                       FROM client
                                       WHERE clinician_id = ?clinician_id;"

client_list_query<- sqlInterpolate(pool, client_list_sql, clinician_id = clinician_id)

client_list<- dbGetQuery(pool, client_list_query)

################################



ui<- function(request) {
  sidebar <- dashboardSidebar(
    sidebarMenu(
      br(),
      menuItem("Home", icon = icon("line-chart"), tabName = "Home"),
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
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| Generalized Anxiety Disorder - 7-Item Scale (GAD-7)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 820),
    sidebar,
    dashboardBody(
      
      tags$head(            #Link to the css style sheet
        tags$link(rel = "stylesheet", type = "text/css", href = "Styling.css")
      ),
      
      tabItems(
        tabItem(tabName = "About", br(), br(), br(), br(),br(), br(),br(),br(), br(),
                column(12, offset = 4, h1(tags$a(href = "http://psychlytx.com.au", "Visit PsychlytX here.",  style = "color: #827717;")))
        ),
        
        tabItem(tabName = "References"
                
                
        ),
        
        
        
        tabItem(tabName = "Home",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel(tags$strong("Register New Client "),
                             psychlytx::analytics_pretherapy_UI("analytics_pretherapy")
                             
                             
                    ),
                    
                    tabPanel(tags$strong("Select Existing Client"),
                             
                             sidebarLayout(
                               sidebarPanel(
                                 
                                 uiOutput("client_dropdown"),
                                 
                                 actionButton("retrieve_client_data", "Select This Client", class = "submit_data"),
                                 
                                 tags$head(tags$style(".submit_data{color:#d35400;}"))
                                 
                               ),
                               
                               mainPanel(
                                 
                                 DT::dataTableOutput("selected_client_data_out")
                                 
                               ))
                             
                    ),
                    
                    tabPanel(tags$strong("Complete Scale Items"),
                             
                             psychlytx::analytics_posttherapy_UI("analytics_posttherapy"),
                             psychlytx::gad7_scale_UI("gad7_scale"),
                             psychlytx::manual_data_UI("manual_data"),
                             psychlytx::calculate_subscale_UI("calculate_subscales"),
                             psychlytx::collect_input_UI("collect_input_1"),
                             psychlytx::combine_all_input_UI("combine_all_input")
                             
       
                    ),
                    
                    tabPanel(tags$strong("Generate Clinical Report"),
                             
                             psychlytx::download_report_UI("download_report")
                             
                             
                    ),
                    
                    tabPanel(tags$strong("Customisation (Optional)"),
                             fluidPage(
                               fluidRow(
                                 tabsetPanel(type = "pills",
                                             
                                             tabPanel("Reliable Change Method", width = 12,
                                                      psychlytx::method_widget_UI("method_widget")
                                             ),
                                             
                                             
                                             
                                             tabPanel("Mean", width = 12, 
                                                      psychlytx::generate_mean_widget_UI("mean_widget_1")
                                             ),
                                             
                                             tabPanel("Sd", width = 12,
                                                      psychlytx::generate_sd_widget_UI("sd_widget_1")
                                             ),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      psychlytx::generate_reliability_widget_UI("reliability_widget_1"),
                                                      psychlytx::reliability_calc_UI("reliability_derivation")
                                                      
                                             ),
                                             
                                             tabPanel("Confidence Level", width = 12,
                                                      psychlytx::confidence_level_UI("confidence_widget"),
                                                      uiOutput("test")
                                                      
                                                      
                                             ),
                                             tabPanel("User-Defined Cut-Off Scores", width = 12,
                                                      psychlytx::generate_cutoff_widget_UI("cutoff_widget_1")
                                                
                                                      
                                                      
                                             )
                                             
                                             
                                             
                                 ))))

                    
                   )
                  
                  
                  
                ),
                
                column(span(tagList(icon("copyright", lib = "font-awesome")), "Timothy Deitz 2018 | PsychlytX") , offset = 4, width = 12)
        ))))
  
}






server <- function(input, output, session) {


    
    
  #Write to pre-therapy analytics data to db
  
  analytics_pretherapy<- callModule(psychlytx::analytics_pretherapy, "analytics_pretherapy")
  
  observe({ 
                       #pass the pretherapy analytics dataframe in and append the client table in db
  dbWriteTable(pool, "client",  data.frame(analytics_pretherapy()), row.names = FALSE, append = TRUE)
  
  
  })
  
  
  
  callModule(psychlytx::reliability_calc, "reliability_derivation")
  
  confidence<- callModule(psychlytx::confidence_level, "confidence_widget")
  
  
  #Yields the reliable change method (a string)
  
  method<- callModule(psychlytx::method_widget, "method_widget")
  
  
  input_population<- reactive({ input$pop }) #Store the selected population
  
  
  
  scale_entry<- callModule(psychlytx::gad7_scale, "gad7_scale") #Store the responses to the online scale and pass them to the manul entry module
  
  manual_entry<- callModule(psychlytx::manual_data, "manual_data", scale_entry)
  
  
  #Make a list of aggregate scores across subscales (in this case there is only one)

  aggregate_scores<- callModule(psychlytx::calculate_subscale, "calculate_subscales", manual_entry = manual_entry, item_index = list( c(1:7) ), aggregation_method = "sum")
  
  selected_client<- reactive({ input$client_selection }) #Store the id of the client selected in the dropdown
  
 ################################## 
  
  #For each subscale individually, collect the values from widgets and store them in a list 
  
  mean_input_1<- do.call(callModule, c(psychlytx::generate_mean_widget, "mean_widget_1", input_population, psychlytx::gad7_info))
  sd_input_1<- do.call(callModule, c(psychlytx::generate_sd_widget, "sd_widget_1", input_population, psychlytx::gad7_info))
  reliability_input_1<- do.call(callModule, c(psychlytx::generate_reliability_widget, "reliability_widget_1", input_population, psychlytx::gad7_info))
  cutoff_input_1<- do.call(callModule, c(psychlytx::generate_cutoff_widget, "cutoff_widget_1", input_population, psychlytx::gad7_info))
  
  #Create list of input values for the first subscale 
  #Need to extract the date from manual_entry data & need the first aggregate score
  
  input_list_1<- callModule(psychlytx::collect_input, "collect_input_1", clinician_id, client_id = selected_client, psychlytx::gad7_info$measure, psychlytx::gad7_info$subscale, manual_entry, aggregate_scores, mean_input_1, sd_input_1, 
                            reliability_input_1, confidence, method, input_population, cutoff_input_1, 1)
  
  
 #Currently, the code in between hashes must be written for each subscale 
  
 ##################################
  
  #Store each list of input values in a larger list object (i.e. all_input)
  #If there were more than one subscale it would look like this: input_list<- reactive({ list( input_list_1(), input_list_2(), etc. ) })
  #Have to store the list of sublists as a reactive object
  
  
  input_list<- reactive({ list( input_list_1() ) })
  
  
  #After creating the list of lists, flatten each sublist and set the names of sublist elements so that they are the same across lists -
  # this will ensure we can iterate over the lists using purrr. So pass the input_list object to the combine_all_input module.
  
  
  #Generate a dataframe with all necessary scale data (date, score, pts, se, ci etc.). This dataframe will be sent to the db
  
  client_data_to_db<- callModule(psychlytx::combine_all_input, "combine_all_input", input_list)
  
  
  observe({ 
    
    #pass the client_data_to_db dataframe in and append the scale table in db
    
    dbWriteTable(pool, "scale",  data.frame(client_data_to_db()), row.names = FALSE, append = TRUE)
    
    
  })
  

 
  
  output$client_dropdown<- renderUI({
  
    
    req(client_list)
    
    #The code below takes data from the db (for all clients linked to this clinician) and wrangles it into a
    # nice dropdown for client selection. Selection returns the unique client id (not the client's name) so that
    #the correct client info is subsequently pulled down from the db.
    
    client_list <- client_list %>%
      tidyr::unite(dropdown_client, first_name, last_name, birth_date, sep = " ", remove = FALSE)
    
    client_list<- client_list %>%
      collect  %>%
      split( .$dropdown_client ) %>%    # Field that will be used for the labels
      purrr::map(~.$client_id) #Field that will be returned when the clinician actually chooses the client
    
    selectInput(
    inputId = "client_selection",
    label = "Select Your Client",
    choices = client_list,
    selectize = FALSE)
  

  })
  
  
  
  selected_client_data<- eventReactive(input$retrieve_client_data, {
    
    
    selected_client_sql<- "SELECT clinician_id, client_id, date, measure, subscale, score
                           FROM scale
                           WHERE client_id = ?client_id AND measure = ?measure;"
    
    selected_client_query<- sqlInterpolate(pool, selected_client_sql, client_id = selected_client(), measure = "GAD-7")
    
    selected_client_data<- dbGetQuery(pool, selected_client_query)
    
    selected_client_data %>% dplyr::select(date, measure, subscale, score) %>% dplyr::rename_all(toupper)
    
    
  })
  
  
  output$selected_client_data_out<- DT::renderDataTable({
    
    
    DT::datatable(
      
                   selected_client_data(), extensions = 'Scroller', rownames = FALSE,
                  options = list(initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#827717', 'color': '#fff'});",
                    "}"), deferRender = TRUE, scrollY = 200, scroller = TRUE, dom = "t" ) 
                  
                  )
    
    
  })
  
  
  
  
  
  #Write post-therapy analytics data to db
  
  analytics_posttherapy<- callModule(psychlytx::analytics_posttherapy, "analytics_posttherapy", selected_client)
  
  
  observe({ 
    
    #pass the analytics dataframe in and append the client table in db
    
    dbWriteTable(pool, "posttherapy_analytics",  data.frame(analytics_posttherapy()), row.names = FALSE, append = TRUE)
    
    
  })
  
  
  
  
  
  #The module below takes the specific client's data pulled from the db, creates a nested df and sends that df
  #to the R Markdown report
  
  #callModule(psychlytx::download_report, "download_report", client_db_data)
  

  
  }



shinyApp(ui, server)













