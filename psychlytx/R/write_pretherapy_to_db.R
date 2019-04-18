write_pretherapy_analytics_to_db<- function(pool, analytics_pretherapy) {


  observe({

    client_check_sql<- "SELECT *
    FROM client
    WHERE first_name = ?inputted_first_name AND last_name = ?inputted_last_name AND birth_date = ?inputted_birth_date;"

    client_check_query<- sqlInterpolate(pool, client_check_sql, inputted_first_name = analytics_pretherapy()$first_name,
                                        inputted_last_name = analytics_pretherapy()$last_name, inputted_birth_date = analytics_pretherapy()$birth_date)

    client_check_data<- dbGetQuery(pool, client_check_query)

    if(length(client_check_data) == 0) {

      #pass the pretherapy analytics dataframe in and append the client table in db
      dbWriteTable(pool, "client",  data.frame(analytics_pretherapy()), row.names = FALSE, append = TRUE) ; showModal(modalDialog(title = "Registration Successful",
                                                                                                                                  footer = modalButton("Okay"), "The client can now complete a measure using any PsychlytX web application."))

    } else(showModal(modalDialog(title = "Registration Unsuccessful", footer = modalButton("Okay"),
                                 "An entry already exists for this client. Please check the details you inputted and resubmit.")))

  })






}