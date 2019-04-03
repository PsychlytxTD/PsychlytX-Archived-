
show_data_availability_message<- function(selected_client_data) {


  renderText({


    if(length(selected_client_data() >= 1)) {

      "Client selected."

    } else {

      "No data to show yet for this client."

    }


  })




}
