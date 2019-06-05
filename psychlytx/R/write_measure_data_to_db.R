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
#' @param pool A pooled db connection
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
      title = "Success!!",
      text = "Questionnaire responses have been submitted.",
      type = "success"
    )

    #pass the client_data_to_db dataframe in and append the scale table in db

    dbWriteTable(pool, "scale",  data.frame(measure_data()), row.names = FALSE, append = TRUE)


      items<- as.list( manual_entry()$item_scores)
      item_names<- c(paste("item", "_", 1:length(items), sep = ""))
      named_items<- items %>% purrr::set_names(item_names)
      item_data<- list( date = manual_entry()$date, item_id = measure_data()$scale_id[1], measure = measure_data()$measure[1], clinician_id = measure_data()$clinician_id,
                        client_id = measure_data()$client_id, named_items ) %>% purrr::flatten()

      dbWriteTable(pool, "item",  as.data.frame(item_data), row.names = FALSE, append = TRUE)



  })




}
