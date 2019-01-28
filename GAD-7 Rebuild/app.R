library(shinydashboard)


ui<- function(request) {
  sidebar <- dashboardSidebar(
    sidebarMenu(
      br(),
      menuItem(actionButton("Disclaimer", "Get Started", icon = icon("line-chart")), tabName = "Scale"),
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
        
        
        
        tabItem(tabName = "Scale",
                fluidRow(
                  tabBox(
                    id = "Box",
                    width = 12,
                    tabPanel(tags$strong("Create Client Profile"),
                             psychlytx::analytics_clientstatus_UI("analytics_clientstatus_id"),
                             psychlytx::analytics_widgets_UI("analytics_widgets_id"),
                             psychlytx::analytics_newcustom_widgets_UI("analytics_newcustom_widgets_id")
                             
                    ),
                    tabPanel(tags$strong("Submit Data & Generate Report"),
                             
                             selectInput("pop", "Select Population", choices = c("male general population", "female general population", "older adult", "primary care",
                                                                                 "psychiatric", "Generalized Anxiety Disorder", "chronic musculoskeletal pain", 
                                                                                 "coronary heart disease", "type 1 diabetes", "type 2 diabetes", "stroke")),
                             
                             psychlytx::manual_data_UI("manual_data_id"),
                             psychlytx::gad7_scale_UI("gad7_scale_id"),
                             psychlytx::download_buttons_UI("download_buttons_id")
       
                    ),
                    
                    tabPanel(tags$strong("Customise Parameters"),
                             fluidPage(
                               
                               fluidRow(
                                 tabsetPanel(type = "pills",
                                             
                                             tabPanel("Reliable Change Method", width = 12,
                                                      psychlytx::interval_widgets_UI("interval_widgets_id")
                                             ),
                                             
                                             
                                             
                                             tabPanel("Mean", width = 12, 
                                                      psychlytx::generate_mean_widget_UI("mean_widget_id")
                                             ),
                                             
                                             tabPanel("Sd", width = 12,
                                                      psychlytx::stats_widgets_UI("sd_widgets_id")
                                             ),
                                             
                                             tabPanel("Test-Retest Reliability", width = 12,
                                                      psychlytx::stats_widgets_UI("rel_widgets_id"),
                                                      psychlytx::reliability_calc_UI("rel_calcs_id")
                                                      
                                             ),
                                             
                                             tabPanel("Confidence Level", width = 12,
                                                      psychlytx::confidence_level_UI("confidence_widget_id")
                                                      
                                                      
                                             ),
                                             tabPanel("User-Defined Cut-Off Scores", width = 12
                                                
                                                      
                                                      
                                             )
                                             
                                             
                                             
                                 ))))

                    
                   )
                  
                  
                  
                ),
                
                column(span(tagList(icon("copyright", lib = "font-awesome")), "Timothy Deitz 2018 | PsychlytX") , offset = 4, width = 12)
        ))))
  
}



























server <- function(input, output, session) {

  clientstatus_module<- callModule(psychlytx::analytics_clientstatus, "analytics_clientstatus_id")
  callModule(psychlytx::analytics_widgets, "analytics_widgets_id", clientstatus_module)
  callModule(psychlytx::analytics_newcustom_widgets, "analytics_newcustom_widgets_id")
  
  input_population<- reactive({ input$pop })
  
  
  callModule(psychlytx::generate_mean_widget, "mean_widget_id", 
             panel_name = "GAD-7 Total Scale", 
             subscale_name = "GAD-7", 
             population_quantity = 11,
             populations = list("male_general_population", "female_general_population", "older_adult", "primary_care", "psychiatric", "Generalized_Anxiety_Disorder",
                                "chronic_musculoskeletal_pain", "coronary_heart_disease", "type_1_diabetes", "type_2_diabetes", "stroke"), 
             input_population = input_population,
             means = list(3.01, 4.07, 2, 5.75, 10.86, 12.59, 2.6, 11.9, 4.7, 4.5, 3.87), sds = list(3.12, 3.53, 2.88, 4.76, 5.62, 3.96, 2.3, 5.3, 4.6, 4.9, 5.2), 
             mean_sd_references = list("Hinz, Klein, Brähler, Glaesmer et al (2017)", "Hinz, Klein, Brähler, Glaesmer et al (2017)", "Wild, Eckl, Herzog, Niehoff et al (2012)",
                                       "Jordan, Shedden-Mora & Löwe (2017)", "Beard & Björgvinsson (2014)", "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)",
                                       "Bair, Wu, Damush, Sutherland & Kroenke (2008)", "Conventry, Lovell, Dickens, Bower et al (2015)", "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)",
                                       "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)", "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)"),
             reliabilities = list(.83, .83, .83, .83, .83, .83, .83, .83, .83, .83, .83), 
             reliability_references = list("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", 
                                           "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                           "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                           "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)"),
             cutoffs = list(c(5, 10, 15, 3.01, 3.01 + 3.12), c(5, 10, 15, 4.07, 4.07 + 3.53), c(5, 10, 15, 2, 2 + 2.88), c(5, 10, 15, 4.75, 4.75 + 4.76), 
                            c(5, 10, 15, 10.86, 10.86 + 5.62), c(5, 10, 15, 12.59, 12.59 + 3.96), c(5, 10, 15, 2.6, 2.6 + 2.3),
                            c(5, 10, 15, 11.9, 11.9 + 5.3), c(5, 10, 15, 4.7, 4.7 + 4.6), c(5, 10, 15, 4.5, 4.5 + 4.9), 
                            c(5, 10, 15, 3.87, 3.87 + 4.52)), 
             cutoff_names = list(rep(c("Mild", "Moderate: for further evaluation", "Severe", "Mean", "Mean + 1 Sd"), 11)),
             cutoff_references = list(c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                        "Hinz, Klein, Brähler, Glaesmer et al (2017)", "Hinz, Klein, Brähler, Glaesmer et al (2017)"),
                                      c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                        "Hinz, Klein, Brähler, Glaesmer et al (2017)", "Hinz, Klein, Brähler, Glaesmer et al (2017)"),
                                      c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                        "Wild, Eckl, Herzog, Niehoff et al (2012)", "Wild, Eckl, Herzog, Niehoff et al (2012)"),
                                      c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                        "Jordan, Shedden-Mora & Löwe (2017)", "Jordan, Shedden-Mora & Löwe (2017)"),
                                      c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                        "Beard & Björgvinsson (2014)", "Beard & Björgvinsson (2014)"),
                                      c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                        "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)", "Dear, Titov, Sunderland, McMillan, Anderson, Lorian & Robinson (2011)"),
                                      c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                        "Bair, Wu, Damush, Sutherland & Kroenke (2008)", "Bair, Wu, Damush, Sutherland & Kroenke (2008)"),
                                      c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                        "Conventry, Lovell, Dickens, Bower et al (2015)", "Conventry, Lovell, Dickens, Bower et al (2015)"),
                                      c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                        "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)", "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)"),
                                      c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                        "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)" , "Fenwick, Rees, Homes-Truscott, Browne, Pouwer & Speight (2016)"),
                                      c("Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)", "Spitzer, Kroenke, Williams & Löwe (2006)",
                                        "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)" , "Schmid, Arnold, Jones, Ritter, Sapp & Van Puymbroeck (2015)")),
             cutoff_quantity = 5)
  
  
  callModule(psychlytx::stats_widgets, "sd_widgets_id", list("GAD-7 Total Scale"), psychlytx::gad7_sds_df, psychlytx::gad7_refs_df, input_population)
  callModule(psychlytx::stats_widgets, "rel_widgets_id", list("GAD-7 Total Scale"), psychlytx::gad7_rels_df, psychlytx::gad7_refs_rels_df, input_population)
  callModule(psychlytx::reliability_calc, "reliability_calcs_id")
  
  callModule(psychlytx::gad7_scale, "gad7_scale_id")
  
  callModule(psychlytx::manual_data, "manual_data_id")
  
  callModule(psychlytx::download_buttons, "download_buttons_id")
  
  callModule(psychlytx::confidence_level, "confidence_widget_id")
  
  callModule(psychlytx::interval_widgets, "interval_widgets_id", list("GAD-7 Total Scale", "other scale"))

  }



shinyApp(ui, server)













