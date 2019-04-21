#' Select Client
#'
#' Select client and display his/her data in abbreviated form.
#'
#' @param id A string to creat the namespace
#'
#' @export

display_client_data_UI<- function(id) {

  ns <- NS(id)

tagList(

actionButton(ns("retrieve_client_data"), "Retrieve Outcomes", class = "submit_data"),

br(),
br(),

DT::dataTableOutput(ns("selected_client_data_out")),

verbatimTextOutput(ns("client_data_availability_message"))

)

}



#' Select Client
#'
#' Select client and display his/her data in abbreviated form.
#'
#' @param pool A pooled db object
#'
#' @param selected_client A string indicating selected client's unique identifier
#'
#' @param measure A string indiating name of the measure
#'
#' @export


display_client_data<- function(input, output, session, pool, selected_client, measure) {


  selected_client_data<- eventReactive(input$retrieve_client_data, {

    selected_client_sql<- "SELECT *
  FROM scale
  WHERE client_id = ?client_id AND measure = ?measure;"

    selected_client_query<- sqlInterpolate(pool, selected_client_sql, client_id = selected_client(), measure = measure)

    dbGetQuery(pool, selected_client_query)


  })



output$selected_client_data_out<- DT::renderDataTable({

  req( selected_client_data() )

  snapshot_selected_client_data<- if(length( selected_client_data() )  < 1) {
    return(NULL) } else {selected_client_data() %>% dplyr::select(date, measure, subscale, score) %>% dplyr::rename_all(toupper) }


  DT::datatable(

    snapshot_selected_client_data,
    extensions = 'Scroller', rownames = FALSE,
    options = list(initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#827717', 'color': '#fff'});",
      "}"), deferRender = TRUE, scrollY = 200, scroller = TRUE, dom = "t" )

  )

})


  output$client_data_availability_message<- renderText({

    req( selected_client_data() )

    if(length(selected_client_data()) >= 1) {

      "Client selected."

    } else {

       "No data to show yet for this client"

    }

  })



  reactive({ selected_client_data() })

}
