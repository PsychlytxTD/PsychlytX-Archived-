
show_data_availability_message<- function(selected_client_data) {


  renderText({

    if(length(selected_client_data()) >= 1) {

      "Client selected."

    } else {

      validate(need(is.null(selected_client_data()), "No data to show yet for this client"))

    }

  })




}
