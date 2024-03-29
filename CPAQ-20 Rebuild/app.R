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
library(memor)
library(extrafont)
library(extrafontdb)
library(shinyhelper)
library(shinyWidgets)
library(shinycssloaders)
library(grid)


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


clinician_id<- "a71c6d9c-10e2-4247-b704-50d72ad14783" #Temp storage of clinician id



ui<- function(request) {
  
  
  dashboardPage(
    
    header<- psychlytx::make_header_UI("header", header_text = psychlytx::CPAQ_20$title), #Make the header
    
    sidebar <- psychlytx::make_sidebar_UI("sidebar"), #Make the sidebar
    
    dashboardBody(
      
      tags$head( 
        
        tags$link(rel = "stylesheet", type = "text/css", href = "Styling.css") #Link to the css style sheet,
        
      ),
      
      tabItems(
        
        tabItem(tabName = "Home", 
                
                fluidRow(
                  
                  tabBox(id = "tabset", width = 12,
                         
                         tabPanel(tags$strong("Register A New Client "),
                                  
                                  psychlytx::analytics_pretherapy_UI("analytics_pretherapy"), #Make the client registration panel
                                  
                                  psychlytx::write_pretherapy_analytics_to_db_UI("write_pretherapy_to_db")
                                  
                         ),
                         
                         
                         tabPanel(tags$strong("Select An Existing Client"),
                                  
                                  sidebarLayout(
                                    
                                    sidebarPanel(
                                      
                                      psychlytx::render_client_dropdown_UI("client_dropdown"),
                                      
                                      actionButton("retrieve_client_data", "Select Client", class = "submit_data"),
                                      
                                      br(),
                                      br(),
                                      
                                      tags$a(href = "https:://psychlytx.com.au", "Edit client records here.", style = "color:#d35400; text-decoration: underline;")

                                    ),
                                    
                                    mainPanel(
                                      
                                      psychlytx::display_client_data_UI("display_client_data")
                                      
                                      
                                    ))),
                         
                         tabPanel(tags$strong("Complete Questionnaire"),
                                  
                                  psychlytx::apply_initial_population_UI("apply_population"),

                                  psychlytx::cpaq_20_scale_UI("cpaq_20_scale"), #Item of the specific measure
                                  
                                  psychlytx::analytics_posttherapy_UI("analytics_posttherapy"), #End-of-therapy clinical outcomes panel
                                  
                                  psychlytx::write_posttherapy_to_db_UI("write_posttherapy_to_db"),
                                
                                  psychlytx::manual_data_UI("manual_data"), #Items of the specific measure are passed here as a string of numbers
                                  
                                  psychlytx::calculate_subscale_UI("calculate_subscales"), #Calculate all aggregate subscale scores for the measure
                                  
                                  psychlytx::collect_input_UI("collect_input_1"), #Collect all input for a subscale
                                  
                                  psychlytx::collect_input_UI("collect_input_2"), #Collect all input for a subscale
                                  
                                  psychlytx::collect_input_UI("collect_input_3"), #Collect all input for a subscale
                                  
                                  psychlytx::combine_all_input_UI("combine_all_input"), #Combine collected inputs from all subscales
                                  
                                  psychlytx::write_measure_data_to_db_UI("write_measure_data")
                         ),
                         
                         tabPanel(tags$strong("Client Settings"), value = "go_custom_settings",
                                  
                                  fluidPage(
                                      
                                    psychlytx::show_population_message_UI("show_population_message"),
                                    
                                    
                                    fluidRow(
                                      
                                      tabsetPanel(type = "pills",
                                                  
                                                  tabPanel("Reliable Change Method", width = 12,
                                                           
                                                           psychlytx::method_widget_UI("method_widget") #Reliable change method widget for one subscale
                                                  ),
                                                  
                                                  tabPanel("Mean", width = 12, 
                                                           
                                                           psychlytx::generate_mean_widget_UI("mean_widget_1"), #Mean widget for one subscale
                                                           
                                                           psychlytx::generate_mean_widget_UI("mean_widget_2"), #Mean widget for one subscale
                                                           
                                                           psychlytx::generate_mean_widget_UI("mean_widget_3") #Mean widget for one subscale
                                                  ),
                                                  
                                                  tabPanel("Sd", width = 12,
                                                           
                                                           psychlytx::generate_sd_widget_UI("sd_widget_1"), #Sd widget for one subscale
                                                           
                                                           psychlytx::generate_sd_widget_UI("sd_widget_2"), #Sd widget for one subscale
                                                           
                                                           psychlytx::generate_sd_widget_UI("sd_widget_3") #Sd widget for one subscale
                                                  ),
                                                  
                                                  tabPanel("Test-Retest Reliability", width = 12,
                                                           
                                                           psychlytx::generate_reliability_widget_UI("reliability_widget_1"), #Reliability widget for one subscale
                                                           
                                                           psychlytx::generate_reliability_widget_UI("reliability_widget_2"), #Reliability widget for one subscale
                                                           
                                                           psychlytx::generate_reliability_widget_UI("reliability_widget_3"), #Reliability widget for one subscale
                                                           
                                                           psychlytx::reliability_calc_UI("reliability_derivation") #Derive reliability from stats (if required)
                                                           
                                                  ),
                                                  
                                                  tabPanel("Confidence Level", width = 12,
                                                           
                                                           psychlytx::confidence_level_UI("confidence_widget") #Confidence level widget for one subscale
                                                           
                                                  ),
                                                  
                                                  tabPanel("User-Defined Cut-Off Scores", width = 12,
                                                           
                                                           psychlytx::generate_cutoff_widget_UI("cutoff_widget_1"), #Cutoff widgets for one subscale
                                                           
                                                           psychlytx::generate_cutoff_widget_UI("cutoff_widget_2"), #Cutoff widgets for one subscale
                                                           
                                                           psychlytx::generate_cutoff_widget_UI("cutoff_widget_3") #Cutoff widgets for one subscale
                                                           
                                                  )
                                                  
                                                  
                                                  
                                      ),
                                      
                                      verbatimTextOutput("reference_sample")
                                      
                                      
                                    ))),
                         
                         useShinyjs(),
                         
                         tabPanel(tags$strong("Download Clinical Report", id = "trigger_most_recent_data"), 
                                  
                                  psychlytx::download_report_UI("download_report") #Report download
                                  
                         )
                         
                      )),
                
                column(span(tagList(icon("copyright", lib = "font-awesome")), "PsychlytX | 2019") , offset = 4, width = 12)),
        
        
        about_tab<- psychlytx::make_about_tab_UI("about_tab") #Make the 'About Psychlytx' tab
        
        
      )))
  
}






server <- function(input, output, session) {
  
  observe_helpers()#Needed for use of the shinyhelpers package
  
  callModule(psychlytx::make_sidebar, "sidebar") #Make sidebar
  
  callModule(psychlytx::make_header, "header") #Make header
  
  callModule(psychlytx::make_about_tab, "about_tab") #Make 'About' tab
  
  
  #Register a new client with pretherapy analytics data. Module 
  #creates unique client id. Need clinician id needs to be available 
  #to module so pass it in.
  
  analytics_pretherapy<- callModule(psychlytx::analytics_pretherapy, "analytics_pretherapy", clinician_id) #Return the pretherapy analytics responses
  
  
  callModule(psychlytx::write_pretherapy_analytics_to_db, "write_pretherapy_to_db", pool, analytics_pretherapy) #Write pre-therapy analytics responses data to db
  
  
  callModule(psychlytx::reliability_calc, "reliability_derivation")  #If selected, derive reliability value from statistics
  
  
  selected_client<- callModule(psychlytx::render_client_dropdown, "client_dropdown", pool, clinician_id) #Create client selection dropdown & return selection
  
  
  input_retrieve_client_data<- reactive({input$retrieve_client_data}) #Store the value of the client selection button
  
  
  existing_data<- callModule(psychlytx::display_client_data, "display_client_data", pool, selected_client, measure = psychlytx::CPAQ_20$measure,
                             input_retrieve_client_data) #Return the selected client's previous scores on this measure
  
  
  input_population<- do.call(callModule, c(psychlytx::apply_initial_population, "apply_population", 
                                           psychlytx::CPAQ_20, existing_data)) #Store the selected population for downstream use in other modules
  
  
  callModule(psychlytx::show_population_message, "show_population_message", input_population) #Prompt user to select a population to generate settings for this client
  
  
  scale_entry<- callModule(psychlytx::cpaq_20_scale, "cpaq_20_scale") #Return the raw responses to the online scale
  
  
  manual_entry<- callModule(psychlytx::manual_data, "manual_data", scale_entry) #Raw item responses are stored as vector manual_entry to be used downstream
  
  
  aggregate_scores<- callModule(psychlytx::calculate_subscale, "calculate_subscales",  manual_entry = manual_entry, 
                                item_index = list( psychlytx::CPAQ_20$items, psychlytx::CPAQ_20_Activity_Engagement$items, psychlytx::CPAQ_20_Pain_Willingness$items ), 
                                aggregation_method = "sum")   #Make a list of aggregate scores across subscales (in this case there is only one subscale)
  

  confidence<- callModule(psychlytx::confidence_level, "confidence_widget", existing_data)  #Return confidence level for intervals. Existing data passed in order to 
                                                                                            #access & pull in the client's settings from db & prepopulate settings
                                                                                            #widgets with these settings. Do same things for method, mean, sd, 
                                                                                            #reliability and cutoffs
  
  
  method<- callModule(psychlytx::method_widget, "method_widget", existing_data) #Return reliable change method (a string)
  
  
#_________________________________________________________________________________________________
  
  #For each subscale individually, collect the values from widgets and store them in a list 
  
  mean_input_1<- do.call(callModule, c(psychlytx::generate_mean_widget, "mean_widget_1", input_population, psychlytx::CPAQ_20, existing_data))
  sd_input_1<- do.call(callModule, c(psychlytx::generate_sd_widget, "sd_widget_1", input_population, psychlytx::CPAQ_20, existing_data))
  reliability_input_1<- do.call(callModule, c(psychlytx::generate_reliability_widget, "reliability_widget_1", input_population, psychlytx::CPAQ_20, existing_data))
  cutoff_input_1<- do.call(callModule, c(psychlytx::generate_cutoff_widget, "cutoff_widget_1", input_population, psychlytx::CPAQ_20, existing_data))
  
  #Create list of input values for a subscale 
  
  input_list_1<- callModule(psychlytx::collect_input, "collect_input_1", clinician_id, client_id = selected_client, measure = psychlytx::CPAQ_20$measure, 
                            subscale = psychlytx::CPAQ_20$subscale, manual_entry, aggregate_scores, mean_input_1, sd_input_1, reliability_input_1, confidence, 
                            method, input_population, cutoff_input_1, subscale_number = 1)
  
  
#_______________________________________________________________________________Currently, the code in between hashes must be written for each subscale 
  
  
  #_________________________________________________________________________________________________
  
  #For each subscale individually, collect the values from widgets and store them in a list 
  
  mean_input_2<- do.call(callModule, c(psychlytx::generate_mean_widget, "mean_widget_2", input_population, psychlytx::CPAQ_20_Activity_Engagement, existing_data))
  sd_input_2<- do.call(callModule, c(psychlytx::generate_sd_widget, "sd_widget_2", input_population, psychlytx::CPAQ_20_Activity_Engagement, existing_data))
  reliability_input_2<- do.call(callModule, c(psychlytx::generate_reliability_widget, "reliability_widget_2", input_population, psychlytx::CPAQ_20_Activity_Engagement, existing_data))
  cutoff_input_2<- do.call(callModule, c(psychlytx::generate_cutoff_widget, "cutoff_widget_2", input_population, psychlytx::CPAQ_20_Activity_Engagement, existing_data))
  
  #Create list of input values for a subscale 
  
  input_list_2<- callModule(psychlytx::collect_input, "collect_input_2", clinician_id, client_id = selected_client, measure = psychlytx::CPAQ_20_Activity_Engagement$measure, 
                            subscale = psychlytx::CPAQ_20_Activity_Engagement$subscale, manual_entry, aggregate_scores, mean_input_2, sd_input_2, reliability_input_2, confidence, 
                            method, input_population, cutoff_input_2, subscale_number = 2)
  
  
  #_______________________________________________________________________________Currently, the code in between hashes must be written for each subscale 
  

  #_________________________________________________________________________________________________
  
  #For each subscale individually, collect the values from widgets and store them in a list 
  
  mean_input_3<- do.call(callModule, c(psychlytx::generate_mean_widget, "mean_widget_3", input_population, psychlytx::CPAQ_20_Pain_Willingness, existing_data))
  sd_input_3<- do.call(callModule, c(psychlytx::generate_sd_widget, "sd_widget_3", input_population, psychlytx::CPAQ_20_Pain_Willingness, existing_data))
  reliability_input_3<- do.call(callModule, c(psychlytx::generate_reliability_widget, "reliability_widget_3", input_population, psychlytx::CPAQ_20_Pain_Willingness, existing_data))
  cutoff_input_3<- do.call(callModule, c(psychlytx::generate_cutoff_widget, "cutoff_widget_3", input_population, psychlytx::CPAQ_20_Pain_Willingness, existing_data))
  
  #Create list of input values for a subscale 
  
  input_list_3<- callModule(psychlytx::collect_input, "collect_input_3", clinician_id, client_id = selected_client, measure = psychlytx::CPAQ_20_Pain_Willingness$measure, 
                            subscale = psychlytx::CPAQ_20_Pain_Willingness$subscale, manual_entry, aggregate_scores, mean_input_3, sd_input_3, reliability_input_3, confidence, 
                            method, input_population, cutoff_input_3, subscale_number = 3)
  
  
  #_______________________________________________________________________________Currently, the code in between hashes must be written for each subscale 
  
  
  
  
  
  #Have to store the list of sublists as a reactive object
  
  
  input_list<- reactive({ list( input_list_1(), input_list_2(), input_list_3() ) })  #Store each list of input values in a larger list object (i.e. all_input). If there were more than one 
  #subscale it would look like this: input_list<- reactive({ list( input_list_1(), input_list_2(), etc. ) })
  #After creating the list of lists, flatten each sublist and set the names of sublist elements so that they 
  #are the same across lists - this will ensure we can iterate over the lists using purrr. 
  #So pass the input_list object to the combine_all_input module.
  
  
  
  measure_data<- callModule(psychlytx::combine_all_input, "combine_all_input", input_list)  #Generate a dataframe with all necessary scale data (date, score, pts, se,
  #ci etc.). This dataframe will be sent to the db
  
  
  
  callModule(psychlytx::write_measure_data_to_db, "write_measure_data", pool, measure_data, manual_entry)  #Write newly entered item responses from measure to db
  
  
  most_recent_client_data<- reactiveValues()
  
onclick("trigger_most_recent_data",  #Query database when user clicks report tab to make sure that the most recent data is pulled in before report generation
          
          observe({ 
            
          most_recent_client_sql<- "SELECT *
          FROM scale
          WHERE client_id = ?client_id;"
          
          most_recent_client_query<- sqlInterpolate(pool, most_recent_client_sql, client_id = selected_client())
          
          most_recent_client_data$value<- dbGetQuery(pool, most_recent_client_query)
          
          })
          
          )
  
  
  #Write post-therapy analytics data to db
  
  analytics_posttherapy<- callModule(psychlytx::analytics_posttherapy, "analytics_posttherapy", clinician_id, selected_client) #Collect posttherapy info
  
  callModule(psychlytx::write_posttherapy_to_db, "write_posttherapy_to_db", pool, analytics_posttherapy) #Write posttherapy info to db
  
  
  
  #Pull selected client's data from db, create a nested df containing all necessary info for report (plots and tables) and send to R Markdown doc.
  
  callModule( psychlytx::download_report, "download_report", pool, selected_client, psychlytx::global_subscale_info, most_recent_client_data)
  
  
}



shinyApp(ui, server)