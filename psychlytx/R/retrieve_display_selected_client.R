#' Retrieve Selected Client Data
#'
#' Query database to retrive abbreviated client score information
#'
#' @param id A string to create the namespace
#'
#' @export


retrive_selected_client_UI<- function(id) {


  ns <- NS(id)

  tagList(

  actionButton(ns("retrieve_client_data"), "Select This Client", class = "submit_data"),

  tags$head(tags$style(".submit_data{color:#d35400;}")),

  br(),
  br()

)

}


#' Retrieve & Show Client Data
#'
#' Query database to retrive client scores and display in table
#'
#' @param selected_client Id of the client selected in dropdown
#'
#' @param measure Measure for which scores are pulled in from db
#'
#' @export


retrive_selected_client<- function(input, output, session, pool, selected_client, measure) {


    eventReactive(input$retrieve_client_data, {


    selected_client_sql<- "SELECT clinician_id, client_id, date, measure, subscale, score
    FROM scale
    WHERE client_id = ?client_id AND measure = ?measure;"

    selected_client_query<- sqlInterpolate(pool, selected_client_sql, client_id = selected_client(), measure = measure)

    selected_client_data<- dbGetQuery(pool, selected_client_query)


    if(length(selected_client_data)  < 1) {
      return(NULL) } else {selected_client_data %>% dplyr::select(date, measure, subscale, score) %>% dplyr::rename_all(toupper) }


  })


}

