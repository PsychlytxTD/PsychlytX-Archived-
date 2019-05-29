#' Write client's measure data to db
#'
#' Write the client's new scores and associated statistics to the db
#'
#' @param id A string to create the namespace
#'
#' @export

write_measure_data_to_db_UI<- function(id) {

  return(NULL)

}




#' Write client's measure data to db
#'
#' Write the client's new scores and associated statistics to the db
#'
#' @param measure_data A dataframe indicating the dataframe to send to the db
#'
#' @param manual_entry A list of values returned when clinician submits new scale responses. Of list items, only accessing the button value, to trigger the query.
#'
#' @export




write_measure_data_to_db<- function(input, ouput, session, pool, measure_data, manual_entry) {


  observeEvent(manual_entry()$submit_scores_button_value, {


    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "Questionnaire responses have been submitted",
      type = "success",
      confirmButtonColor = '#d35400'
    )

    #pass the client_data_to_db dataframe in and append the scale table in db

    dbWriteTable(pool, "scale",  data.frame(measure_data()), row.names = FALSE, append = TRUE)


  })




}
