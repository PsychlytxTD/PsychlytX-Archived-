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


clinician_id<- "a71c6d9c-10e2-4247-b704-50d72ad14783" #Temp storage of client and clinician id



ui<- function(request) {
  

  dashboardPage(
    
    header<- psychlytx::make_header_UI("header"), #Make the header
    
    sidebar <- psychlytx::make_sidebar_UI("sidebar"), #Make the sidebar
    
    dashboardBody(
      
      tags$head( 
        
        tags$link(rel = "stylesheet", type = "text/css", href = "Styling.css") #Link to the css style sheet
        
              ),
      
      tabItems(
        
        about_tab<- psychlytx::make_about_tab_UI("about_tab"), #Make the 'About Psychlytx' tab
        
        references_tab<- psychlytx::make_references_tab_UI("references_tab"), #Make the references tab
        
        tabItem(tabName = "Home",
                
                fluidRow(
                  
                 tabBox(id = "tabset", width = 12,
                         
                    tabPanel(tags$strong("Register New Client "),
                             
                             psychlytx::analytics_pretherapy_UI("analytics_pretherapy") #Make the client registration panel
                             
                             
                    ),
                    
                    useShinyjs(),
                    
                    tabPanel(tags$strong("Select Existing Client"), #Clicking on the 'existing client' tab sends query, pulling clients from db into dropdown menu
                             
                             sidebarLayout(
                               
                               sidebarPanel(
                              
                                 psychlytx::render_client_dropdown_UI("client_dropdown"),
                                 
                                 psychlytx::select_population_UI("select_population")
                                 
                               ),
                               
                               mainPanel(
                               
                                psychlytx::display_client_data_UI("display_client_data") 
                               
                               )
                               
                               )
                             
                    ),
                    
                    tabPanel(tags$strong("Complete Scale Items"),
                             
                             psychlytx::analytics_posttherapy_UI("analytics_posttherapy"), #End-of-therapy clinical outcomes panel
                             
                             psychlytx::gad7_scale_UI("gad7_scale"), #Item of the specific measure
                             
                             psychlytx::manual_data_UI("manual_data"), #Items of the specific measure are passed here as a string of numbers
                             
                             psychlytx::calculate_subscale_UI("calculate_subscales"), #Calculate all aggregate subscale scores for the measure
                             
                             psychlytx::collect_input_UI("collect_input_1"), #Collect all input for a subscale
                             
                             psychlytx::combine_all_input_UI("combine_all_input") #Combine collected inputs from all subscales
                             
       
                    ),
                    
                    tabPanel(tags$strong("Generate Clinical Report"),
                             
                             psychlytx::download_report_UI("download_report") #Report download
                             
                             
                    ),
                    
                    tabPanel(tags$strong("Customisation (Optional)"),
                             
                             fluidPage(
                               
                               fluidRow(
                                 
                                 tabsetPanel(type = "pills",
                                        
                                             tabPanel("Reliable Change Method", width = 12,
                                                      
                                                      psychlytx::method_widget_UI("method_widget") #Reliable change method widget for one subscale
                                             ),
                                             
                                             tabPanel("Mean", width = 12, 
                                                      
                                                      psychlytx::generate_mean_widget_UI("mean_widget_1") #Mean widget for one subscale
                                             ),
                                             
                                             tabPanel("Sd", width = 12,
                                                      
                                                      psychlytx::generate_sd_widget_UI("sd_widget_1") #Sd widget for one subscale
                                             ),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      
                                                      psychlytx::generate_reliability_widget_UI("reliability_widget_1"), #Reliability widget for one subscale
                                                      
                                                      psychlytx::reliability_calc_UI("reliability_derivation") #Derive reliability from stats (if required)
                                                      
                                             ),
                                             
                                             tabPanel("Confidence Level", width = 12,
                                                      
                                                      psychlytx::confidence_level_UI("confidence_widget") #Confidence level widget for one subscale
                                                      
                                             ),
                                             
                                             tabPanel("User-Defined Cut-Off Scores", width = 12,
                                                      
                                                      psychlytx::generate_cutoff_widget_UI("cutoff_widget_1") #Cutoff widgets for one subscale
                                                
                                             )
                                             
                                             
                                             
                                 )))))),
                
                column(span(tagList(icon("copyright", lib = "font-awesome")), "PsychlytX | 2019") , offset = 4, width = 12)))))
  
}






server <- function(input, output, session) {
  

 callModule(psychlytx::make_sidebar, "sidebar") #Make sidebar
 
 callModule(psychlytx::make_header, "header") #Make header
 
 callModule(psychlytx::make_about_tab, "about_tab") #Make 'About' tab
 
 callModule(psychlytx::make_references_tab, "references_tab") #Make references tab
  
  
                                                                                                          #Register a new client with pretherapy analytics data. Module 
                                                                                                          #creates unique client id. Need clinician id needs to be available 
                                                                                                          #to module so pass it in.
  
 analytics_pretherapy<- callModule(psychlytx::analytics_pretherapy, "analytics_pretherapy", clinician_id) 
  
  
 psychlytx::write_pretherapy_analytics_to_db(pool, analytics_pretherapy) #Write pre-therapy analytics data to db
  
  
  callModule(psychlytx::reliability_calc, "reliability_derivation")  #If selected, derive reliability value from statistics
  

  confidence<- callModule(psychlytx::confidence_level, "confidence_widget")  #Confidence level for intervals
  

  method<- callModule(psychlytx::method_widget, "method_widget") #Reliable change method (a string)
  
  
  input_population<- do.call(callModule, c(psychlytx::select_population, "select_population", psychlytx::gad7_info)) #Store the selected population for downstream use in other modules
  
  
  scale_entry<- callModule(psychlytx::gad7_scale, "gad7_scale") #Store the responses to the online scale and pass them to the manul entry module
  
  
  manual_entry<- callModule(psychlytx::manual_data, "manual_data", scale_entry) #Raw item scores are stored in manual_entry for use by other modules
  
  
  aggregate_scores<- callModule(psychlytx::calculate_subscale, "calculate_subscales",  
                                manual_entry = manual_entry, item_index = list( psychlytx::gad7_info$items ), 
                                aggregation_method = "sum")                                #Make a list of aggregate scores across subscales 
                                                                                           #(in this case there is only one subscale)
  
  
 ################################## 
  
  #For each subscale individually, collect the values from widgets and store them in a list 
  
  mean_input_1<- do.call(callModule, c(psychlytx::generate_mean_widget, "mean_widget_1", input_population, psychlytx::gad7_info))
  sd_input_1<- do.call(callModule, c(psychlytx::generate_sd_widget, "sd_widget_1", input_population, psychlytx::gad7_info))
  reliability_input_1<- do.call(callModule, c(psychlytx::generate_reliability_widget, "reliability_widget_1", input_population, psychlytx::gad7_info))
  cutoff_input_1<- do.call(callModule, c(psychlytx::generate_cutoff_widget, "cutoff_widget_1", input_population, psychlytx::gad7_info))
  
  #Create list of input values for a subscale 
  
  input_list_1<- callModule(psychlytx::collect_input, "collect_input_1", clinician_id, client_id = selected_client, psychlytx::gad7_info$measure, psychlytx::gad7_info$subscale, manual_entry, aggregate_scores, mean_input_1, sd_input_1, 
                            reliability_input_1, confidence, method, input_population, cutoff_input_1, 1)
  
  
 #Currently, the code in between hashes must be written for each subscale 
  
 ##################################
  

  #Have to store the list of sublists as a reactive object
  
  
  input_list<- reactive({ list( input_list_1() ) })  #Store each list of input values in a larger list object (i.e. all_input). If there were more than one 
                                                     #subscale it would look like this: input_list<- reactive({ list( input_list_1(), input_list_2(), etc. ) })
                                                     #After creating the list of lists, flatten each sublist and set the names of sublist elements so that they 
                                                     #are the same across lists - this will ensure we can iterate over the lists using purrr. 
                                                     #So pass the input_list object to the combine_all_input module.
  
  
  
  measure_data<- callModule(psychlytx::combine_all_input, "combine_all_input", input_list)  #Generate a dataframe with all necessary scale data (date, score, pts, se,
                                                                                            #ci etc.). This dataframe will be sent to the db
  
  
  psychlytx::write_measure_data_to_db(pool, measure_data)  #Write newly entered item responses from measure to db

   
           
  selected_client<- callModule(psychlytx::render_client_dropdown, "client_dropdown", pool, clinician_id)
           
         
  
  selected_client_data<- callModule(psychlytx::display_client_data, "display_client_data", pool, selected_client, psychlytx::gad7_info$measure)
  
  
  #Write post-therapy analytics data to db
  
  analytics_posttherapy<- callModule(psychlytx::analytics_posttherapy, "analytics_posttherapy", clinician_id, selected_client) #Collect posttherapy info
  
  psychlytx::write_posttherapy_to_db(pool, analytics_posttherapy) #Write posttherapy info to db

  
  #Pull selected client's data from db, create a nested df containing all necessary info for report (plots and tables) and send to R Markdown doc.
  
  callModule( psychlytx::download_report, "download_report", selected_client_data )
  

  
  }



shinyApp(ui, server)













