#' Report Availability Message
#'
#' Make message about data availability for report
#'
#' @param id String to create the namespace
#'
#' @export


report_availability_message_UI<- function(id) {

  ns<- NS(id)

  htmlOutput(ns("report_availability"))

}


#' Report Availability Message
#'
#' Make message about data availability for report
#'
#' @param selected_client_data Dataframe of client's outcomes


report_availability_message<- function(input, output, session, selected_client_data) {


  output$report_availability<- renderText({

    if(length(selected_client_data()) < 1) {

return(paste("<span style=\"color:red\"> A report is not available yet. You have selected a client with no existing outcomes,
                    or else haven't clicked the <code>Retrieve Outcomes</code> button when selecting the current client.</span>"))

    }

})

}
