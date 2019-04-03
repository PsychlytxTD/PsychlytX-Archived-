#' Client Dropdown & Select
#'
#' Create client dropdown menu and enable client selection from db
#'
#' @param id A string to create the namespace
#'
#' @export

select_client_UI<- function(id) {

  ns<- NS(id) #Set the namespace


  tagList(

  tabPanel(tags$strong("Select Existing Client", useShinyjs(), id = ns("trigger_query")), #Clicking on the 'existing client' tab sends query, pulling clients from db into dropdown menu

           sidebarLayout(

             sidebarPanel(

               uiOutput(ns("client_dropdown")),

               tags$head(tags$style(".submit_data{color:#d35400;}")),

               br(),
               br(),

               actionButton(ns("retrieve_client_data"), "Select This Client", class = "submit_data")

             ),

             mainPanel(

               DT::dataTableOutput("selected_client_data_out"),

               verbatimTextOutput("client_selection_message")


             ))))

}



#' Client Dropdown & Select
#'
#' Create client dropdown menu and enable client selection from db
#'
#' @param pool Pool database connection
#'
#' @param clinician_id Id of the clinician passed from Shiny Proxy login
#'
#' @export

select_client<- function(input, output, session, pool, clinician_id) {

  ns <- session$ns #Set the namespace


  # Querying code to be called when clinician clicks 'existing client' tab further down. To generate dropdown of clients.

  client_list_sql<- "SELECT clinician_id, client_id, first_name, last_name, birth_date
  FROM client
  WHERE clinician_id = ?clinician_id;"

  client_list_query<- sqlInterpolate(pool, client_list_sql, clinician_id = clinician_id)


  shinyjs::onclick( "trigger_query", client_list<- reactive({ dbGetQuery( pool, client_list_query ) })  )



  output$client_dropdown<- renderUI({


    client_list <- client_list() %>%
      tidyr::unite(dropdown_client, first_name, last_name, birth_date, sep = " ", remove = FALSE)

    client_list<- client_list %>%
      collect  %>%
      split( .$dropdown_client ) %>%    # Field that will be used for the labels
      purrr::map(~.$client_id)          #Field that will be returned when the clinician actually chooses the client

    selectInput(
      inputId = ns("client_selection"),
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



}
