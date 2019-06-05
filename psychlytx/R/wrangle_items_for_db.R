#' Make Item-Level Entry For Item Table In Database
#'
#' Wrangle the individual item scores and other key information for writing to the item table in the database.
#'
#' @param id A string to create the namespace
#'
#' @export


wrangle_items_for_db_UI<- function(id) {

  return(NULL)

}




#' Make Item-Level Entry For Item Table In Database
#'
#' Wrangle the individual item scores and other key information for writing to the item table in the database.
#'
#' @param measure_data A dataframe indicating the dataframe to send to the db
#'
#' @param manual_entry A list of values returned when clinician submits new scale responses. Of list items, only accessing the button value, to trigger the query.
#'
#' @export


wrangle_items_for_db<- function(input, output, session, manual_entry, measure_data) {

  observe({

    req(manual_entry(), measure_data())

  print(

  list( manual_entry()$date, measure_data()$scale_id[1], measure_data()$clinician_id, measure_data()$client_id, manual_entry()$item_scores)

   )

  })


}

