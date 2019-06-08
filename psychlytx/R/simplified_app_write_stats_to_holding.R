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

    url <- c("https://api.sendgrid.com/v3/mail/send")

    headers = c(
      `Authorization` = "bearer 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",
      `Content-Type` = "application/json"
    )


    body = sprintf('{"from": {"email":"psychlytx@gmail.com"},
     "personalizations": [{"to": [{"email":"timothydeitz@gmail.com"}],
"dynamic_template_data":{
"header":"A measure is ready to be completed",
"text": "Before you begin, please copy copy the following unique key to your clipboard: %s"
"c2a_button":"Begin",
"c2a_link":"www.psychlytx.com.au"}}],
"template_id":"d-c102ab1090724b6a90a269479f37e943"}', holding_data()$client_id)



    result <- httr::POST(url,
                         add_headers(headers),
                         body = body,
                         encode="json",
                         verbose())



      result <- POST(url,
                   add_headers(headers),
                   body = body,
                   encode="json",
                   verbose())



    #pass the client_data_to_db dataframe in and append the scale table in db

    dbWriteTable(pool, "holding",  data.frame(holding_data()), row.names = FALSE, append = TRUE)


  })




}
