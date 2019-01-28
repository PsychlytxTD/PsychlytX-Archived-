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
    dashboardHeader(title = span(tagList(tags$a(href = "http://psychlytx.com.au", "PsychlytX", style = "color: white; font-size: 26px; letter-spacing: 7.8px;font-weight: bolder;"), tags$sup("®"), "| Generalized Anxiety Disorder 7-item scale (GAD-7)"), style = "color: white; letter-spacing: 1.8px;"), titleWidth = 820),
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
                                                      psychlytx::stats_widgets_UI("mean_widgets_id")
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
  
  pop<- reactive({ input$pop })
  
  callModule(psychlytx::stats_widgets, "mean_widgets_id", list("GAD-7 Total Scale"), psychlytx::gad7_means_df, psychlytx::gad7_refs_df, pop)
  callModule(psychlytx::stats_widgets, "sd_widgets_id", list("GAD-7 Total Scale"), psychlytx::gad7_sds_df, psychlytx::gad7_refs_df, pop)
  callModule(psychlytx::stats_widgets, "rel_widgets_id", list("GAD-7 Total Scale"), psychlytx::gad7_rels_df, psychlytx::gad7_refs_rels_df, pop)
  callModule(psychlytx::reliability_calc, "reliability_calcs_id")
  
  callModule(psychlytx::gad7_scale, "gad7_scale_id")
  
  callModule(psychlytx::manual_data, "manual_data_id")
  
  callModule(psychlytx::download_buttons, "download_buttons_id")
  
  callModule(psychlytx::confidence_level, "confidence_widget_id")
  
  callModule(psychlytx::interval_widgets, "interval_widgets_id", list("GAD-7 Total Scale", "other scale"))
  
}



shinyApp(ui, server)













