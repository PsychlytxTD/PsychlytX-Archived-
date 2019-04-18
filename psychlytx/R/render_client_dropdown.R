#'Render Client Dropdown
#'
#'#Render client dropdown menu
#'
#'@param id A string to create the namespace
#'
#'@export

render_client_dropdown_UI<- function(id) {

ns <- NS(id)

tagList(

actionButton(ns("refresh"), "Refresh"),

br(),

uiOutput(ns("client_dropdown"))

)

}



#'Render Client Dropdown
#'
#'#Render client dropdown menu
#'
#' @param pool A pool db connection object
#'
#' @param clinician_id Clinician's unique identifier
#'
#'@export


render_client_dropdown<- function(input, output, session, pool, clinician_id) {

clients<- reactive({


  input$refresh

  # Querying code to be called when clinician clicks 'existing client' tab further down. To generate dropdown of clients.

  client_list_sql<- "SELECT clinician_id, client_id, first_name, last_name, birth_date
  FROM client
  WHERE clinician_id = ?clinician_id;"

  client_list_query<- sqlInterpolate(pool, client_list_sql, clinician_id = clinician_id)

  client_list<- dbGetQuery( pool, client_list_query )

  validate(need(length(client_list) >= 1, "No clients registered yet."))

  client_list <- client_list %>%
    tidyr::unite(dropdown_client, first_name, last_name, birth_date, sep = " ", remove = FALSE)

  client_list<- client_list %>%
    collect  %>%
    split( .$dropdown_client ) %>%    # Field that will be used for the labels
    purrr::map(~.$client_id)          #Field that will be returned when the clinician actually chooses the client


})



output$client_dropdown<- renderUI({

  ns <- session$ns

  req(clients())

  selectInput(
    inputId = ns("client_selection"),
    label = "Find Your Client",
    choices = clients(),
    selectize = FALSE)


})

outputOptions(output, "client_dropdown", suspendWhenHidden = FALSE)


reactive({ input$client_selection })

}