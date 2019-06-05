#' Write client's statistics to holding table
#'
#' Write the client's statistics (selected by clinician) to holding table in DB if sending measure via email link.
#'
#' @param id A string to create the namespace
#'
#' @export

write_statistics_to_holding_UI<- function(id) {

  ns <- NS(id)

  tagList(

  h4(tags$strong("Send this questionnaire to your client via email.")),

  actionButton(ns("submit_holding_data"), "Send")

  )

}




#' Write client's statistics to holding table
#'
#' Write the client's statistics (selected by clinician) to holding table in DB if sending measure via email link.
#'
#' @param pool A pooled database connection
#'
#' @param holding_data A dataframe indicating the dataframe to send to the db
#'
#' @export




write_statistics_to_holding<- function(input, ouput, session, pool, holding_data) {


  observeEvent(input$submit_holding_data, {


    sendSweetAlert(                 #Holding stats are sent to the db and an email is sent to the client for questionnaire completion.
      session = session,
      title = "Success !!",
      text = "The questionnaire has been sent to your client for completion.",
      type = "success"
    )

    headers = c(
      `Authorization` = "bearer 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",
      `Content-Type` = "application/json"
    )

    data = '{"personalizations": [{"to": [{"email": "tim@effectivepsych.com.au"}]}],"from": {"email": "timothydeitz@gmail.com"},"subject":
    "Sending with SendGrid is Fun","content": [{"type": "text/plain","value": "Please click the button below to begin. Make sure to copy your client ID and paste into the app to begin."}],"c2a_link": "www.theage.com.au","c2a_button": "Complete Measure","template_id": "d-c102ab1090724b6a90a269479f37e943"}'

    httr::POST(url = 'https://api.sendgrid.com/v3/mail/send', httr::add_headers(.headers=headers), body = data)


    #pass the client_data_to_db dataframe in and append the scale table in db

    dbWriteTable(pool, "holding",  data.frame(holding_data()), row.names = FALSE, append = TRUE)


  })




}
