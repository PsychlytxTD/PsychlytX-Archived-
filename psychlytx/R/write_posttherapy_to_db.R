write_posttherapy_to_db<- function(pool, analytics_posttherapy) {

  observe({

    #pass the analytics dataframe in and append the client table in db

    dbWriteTable(pool, "posttherapy_analytics",  data.frame(analytics_posttherapy()), row.names = FALSE, append = TRUE) ;
    showModal(modalDialog(title = "Successful Completion", footer = modalButton("Okay"), "End-of-therapy outcome data has been submitted."))


  })


}
