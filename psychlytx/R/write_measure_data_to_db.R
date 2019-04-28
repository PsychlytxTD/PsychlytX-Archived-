write_measure_data_to_db<- function(pool, measure_data, manual_entry) {


  observeEvent(manual_entry()$submit_scores_button_value, {

    #pass the client_data_to_db dataframe in and append the scale table in db

    dbWriteTable(pool, "scale",  data.frame(measure_data()), row.names = FALSE, append = TRUE) ;
    showModal(modalDialog(title = "Successful Completion", footer = modalButton("Okay"),
                          "Responses have been submitted."))


  })




}
