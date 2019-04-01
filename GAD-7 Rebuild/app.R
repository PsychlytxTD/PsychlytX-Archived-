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



# Querying code to be called when clinician clicks 'existing client' tab further down. To generate dropdown of clients.

client_list_sql<- "SELECT clinician_id, client_id, first_name, last_name, birth_date
                                       FROM client
                                       WHERE clinician_id = ?clinician_id;"

client_list_query<- sqlInterpolate(pool, client_list_sql, clinician_id = clinician_id)





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
                    
                    tabPanel(tags$strong("Select Existing Client", id = "trigger_query"), #Clicking on the 'existing client' tab sends query, pulling clients from db into dropdown menu
                             
                             sidebarLayout(
                               
                               sidebarPanel(
                                 
                                 uiOutput("client_dropdown"),
                                 
                                 actionButton("retrieve_client_data", "Select This Client", class = "submit_data"),
                                 
                                 tags$head(tags$style(".submit_data{color:#d35400;}")),
                                 
                                 br(),
                                 br(),
                                 
                                 psychlytx::select_population_UI("select_population")
                                 
                               ),
                               
                               mainPanel(
                                 
                                 DT::dataTableOutput("selected_client_data_out"),
                                 verbatimTextOutput("client_selection_message")
                                 
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
                                                      
                                                      psychlytx::confidence_level_UI("confidence_widget")
                                                      
                                             ),
                                             
                                             tabPanel("User-Defined Cut-Off Scores", width = 12,
                                                      
                                                      psychlytx::generate_cutoff_widget_UI("cutoff_widget_1")
                                                
                                             )
                                             
                                             
                                             
                                 )))))),
                
                column(span(tagList(icon("copyright", lib = "font-awesome")), "PsychlytX | 2019") , offset = 4, width = 12)))))
  
}






server <- function(input, output, session) {

 callModule(psychlytx::make_sidebar, "sidebar")
 callModule(psychlytx::make_sidebar, "sidebar")
 callModule(psychlytx::make_header, "header")
 
 callModule(psychlytx::make_about_tab, "about_tab")
 callModule(psychlytx::make_references_tab, "references_tab")
  
  
                                                               #Need clinician id to be available to module
  
  analytics_pretherapy<- callModule(psychlytx::analytics_pretherapy, "analytics_pretherapy", clinician_id) 
  
  
  #Write to pre-therapy analytics data to db
                                                                                                           
  observe({ 
    
    client_check_sql<- "SELECT *
                        FROM client
                        WHERE first_name = ?inputted_first_name AND last_name = ?inputted_last_name AND birth_date = ?inputted_birth_date;"
    
    client_check_query<- sqlInterpolate(pool, client_check_sql, inputted_first_name = analytics_pretherapy()$first_name, 
                                        inputted_last_name = analytics_pretherapy()$last_name, inputted_birth_date = analytics_pretherapy()$birth_date)
    
    client_check_data<- dbGetQuery(pool, client_check_query)
    
    if(length(client_check_data) == 0) {

                       #pass the pretherapy analytics dataframe in and append the client table in db
       dbWriteTable(pool, "client",  data.frame(analytics_pretherapy()), row.names = FALSE, append = TRUE) ; showModal(modalDialog(title = "Registration Successful", 
                                                        footer = modalButton("Okay"), "The client can now complete a measure using any PsychlytX web application."))
  
    } else(showModal(modalDialog(title = "Registration Unsuccessful", footer = modalButton("Okay"), 
                                 "An entry already exists for this client. Please check the details you inputted and resubmit.")))
  
  })
  
  
  #If selected, derive reliability value from statistics
  
  callModule(psychlytx::reliability_calc, "reliability_derivation")
  
  #Confidence level for intervals
  
  confidence<- callModule(psychlytx::confidence_level, "confidence_widget")
  
  #Yields the reliable change method (a string)
  
  method<- callModule(psychlytx::method_widget, "method_widget")
  
  
  
  input_population<- callModule(psychlytx::select_population, "select_population")  #Store the selected population
  
  
  
  scale_entry<- callModule(psychlytx::gad7_scale, "gad7_scale") #Store the responses to the online scale and pass them to the manul entry module
  
  manual_entry<- callModule(psychlytx::manual_data, "manual_data", scale_entry) #Raw item scores are stored in manual_entry for use by other modules
  
  
  
  #Make a list of aggregate scores across subscales (in this case there is only one subscale)

  aggregate_scores<- callModule(psychlytx::calculate_subscale, "calculate_subscales", manual_entry = manual_entry, item_index = list( c(1:7) ), aggregation_method = "sum")
  
  selected_client<- reactive({ input$client_selection }) #Store the id of the client selected in the dropdown
  
 ################################## 
  
  #For each subscale individually, collect the values from widgets and store them in a list 
  
  mean_input_1<- do.call(callModule, c(psychlytx::generate_mean_widget, "mean_widget_1", input_population, psychlytx::gad7_info))
  sd_input_1<- do.call(callModule, c(psychlytx::generate_sd_widget, "sd_widget_1", input_population, psychlytx::gad7_info))
  reliability_input_1<- do.call(callModule, c(psychlytx::generate_reliability_widget, "reliability_widget_1", input_population, psychlytx::gad7_info))
  cutoff_input_1<- do.call(callModule, c(psychlytx::generate_cutoff_widget, "cutoff_widget_1", input_population, psychlytx::gad7_info))
  
  #Create list of input values for the first subscale 
  
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
    
    dbWriteTable(pool, "scale",  data.frame(client_data_to_db()), row.names = FALSE, append = TRUE) ; 
    showModal(modalDialog(title = "Successful Completion", footer = modalButton("Okay"), 
                          "Responses have been submitted."))
    
    
  })
  

 
  
  onclick( "trigger_query", client_list<- reactive({ dbGetQuery( pool, client_list_query ) })  )
  
  
  
  output$client_dropdown<- renderUI({
  
    
    #The code below takes data from the db (for all clients linked to this clinician) and wrangles it into a
    # nice dropdown for client selection. Selection returns the unique client id (not the client's name) so that
    #the correct client info is subsequently pulled down from the db.
    
    client_list <- client_list() %>%
      tidyr::unite(dropdown_client, first_name, last_name, birth_date, sep = " ", remove = FALSE)
    
    client_list<- client_list %>%
      collect  %>%
      split( .$dropdown_client ) %>%    # Field that will be used for the labels
      purrr::map(~.$client_id) #Field that will be returned when the clinician actually chooses the client
    
    selectInput(
    inputId = "client_selection",
    label = "Find Your Client",
    choices = client_list,
    selectize = FALSE)
  

  })
  
  
  
  selected_client_data<- eventReactive(input$retrieve_client_data, {
    
    
    selected_client_sql<- "SELECT clinician_id, client_id, date, measure, subscale, score
                           FROM scale
                           WHERE client_id = ?client_id AND measure = ?measure;"
    
    selected_client_query<- sqlInterpolate(pool, selected_client_sql, client_id = selected_client(), measure = "GAD-7")
    
    selected_client_data<- dbGetQuery(pool, selected_client_query)
    
    
    if(length(selected_client_data)  < 1) {
      return(NULL) } else {selected_client_data %>% dplyr::select(date, measure, subscale, score) %>% dplyr::rename_all(toupper) }
    
  
  })
  
  
  output$selected_client_data_out<- DT::renderDataTable({
    
    
    
    DT::datatable(
      
                   selected_client_data(), 
                   extensions = 'Scroller', rownames = FALSE,
                  options = list(initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#827717', 'color': '#fff'});",
                    "}"), deferRender = TRUE, scrollY = 200, scroller = TRUE, dom = "t" ) 
                  
                  )
    
    
  })
  
  
  output$client_selection_message<- renderText({
    
    if(length(selected_client_data() >= 1)) {
      
      "Client selected."
      
    } else {
      
      "No data to show yet for this client."
      
    }
    
    
  })
  
  
  #Write post-therapy analytics data to db
  
  analytics_posttherapy<- callModule(psychlytx::analytics_posttherapy, "analytics_posttherapy", clinician_id, selected_client)
  
  
  observe({ 
    
    #pass the analytics dataframe in and append the client table in db
    
    dbWriteTable(pool, "posttherapy_analytics",  data.frame(analytics_posttherapy()), row.names = FALSE, append = TRUE) ; 
    showModal(modalDialog(title = "Successful Completion", footer = modalButton("Okay"), "End-of-therapy outcome data has been submitted."))
    
    
  })
  
  
  
  #The module below takes the specific client's data pulled from the db, creates a nested df and sends that df
  #to the R Markdown report
  
  #callModule(psychlytx::download_report, "download_report", client_db_data)
  

  
  }



shinyApp(ui, server)













