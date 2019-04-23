#'Trigger Client Data Query On Responses Submission
#'
#'Ensure that client data query is rerun when user submits scale responses, to ensure linear workflow.
#'
#' @param id A string to create the namespace
#'
#' @export

refresh_scale_query_UI<- function(id) {

  ns<- NS(id)

  return(NULL)

}




#'Trigger Client Data Query On Responses Submission
#'
#'Ensure that client data query is rerun when user submits scale responses, to ensure linear workflow.
#'
#'@param pool Pooled db connection
#'
#'@param input_submit_responses Reactive object representing id of scale data submission action button
#'
#'@param measure_data Reactive object representing the newest ready-for-submission scale data.
#'
#'@param measure String representing the scale;
#'
#'@param selected_client Reactive object indicating client id that has been selected from client dropdown menu.
#'
#' @export

refresh_scale_query<- function(input, output, session, pool, input_submit_responses, measure_data, measure, selected_client) {


  observeEvent(input_submit_responses(), {

    dbWriteTable(pool, "scale",  data.frame(measure_data()), row.names = FALSE, append = TRUE) ;
    showModal(modalDialog(title = "Successful Completion", footer = modalButton("Okay"),
                          "Responses have been submitted."))

  })



  refreshed_client_data<- eventReactive(input_submit_responses(), {

    selected_client_sql<- "SELECT *
    FROM scale
    WHERE client_id = ?client_id AND measure = ?measure;"

    selected_client_query<- sqlInterpolate(pool, selected_client_sql, client_id = selected_client(), measure = measure)

    dbGetQuery(pool, selected_client_query)

  })

  reactive({ refreshed_client_data() })


}
