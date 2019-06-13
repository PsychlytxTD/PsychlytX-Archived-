#' Convert GAD-7 Measure Responses
#'
#' Convert GAD-7 raw measure responses into a readable table, to be send in an email to the clinician.
#'
#' @param id A string to create the namespace
#'
#' @export


format_gad7_responses_for_email_UI<- function(id) {

  ns<- NS(id)

  return(NULL)
}


#' Convert GAD-7 Measure Responses
#'
#' Convert GAD-7 raw measure responses into a readable table, to be send in an email to the clinician.
#'
#' @param manual_entry A list of raw item scores and the date of measure completion.
#'
#' @export

format_gad7_responses_for_email<- function(input, output, session, manual_entry, measure_data) {

  reactive({

  formatted_item_responses<- dplyr::case_when( #Convert the client's responses from numerical form to readable responses, teo appear in the email.

   manual_entry()$item_scores == 0 ~ "Not at all",

   manual_entry()$item_scores == 1 ~ "Several days",

   manual_entry()$item_scores == 2 ~ "More than half the days",

   manual_entry()$item_scores == 3 ~ "Nearly every day",

   TRUE ~ as.character(manual_entry()$item_scores)

 )


 measure_data<- measure_data()

 score_severity_range<- psychlytx::find_severity_range(measure_data) #use the find_severity_range() function to make a single vector of strings
                                                                     #containing (in order) the scores and the severity range descriptions.

 body_values<- c(score_severity_range, formatted_item_responses) #Join the previous score/severity range description strings with the item responses to make one vector.


                    #Need to replage "to:" field with clinician's email address, pulled from Autho

    body<- do.call(sprintf, c(list('{"from": {"email":"measurely@psychlytx.com","name":"Measurely"},
        "personalizations": [{"to": [{"email":"tim@effectivepsych.com.au"}],
                   "dynamic_template_data":{
                   "header":"Your client has completed a measure.",

                   "score": "%s",
                   "severity_range": "%s",

                   "response_1":"%s",
                   "response_2":"%s",
                   "response_3":"%s",
                   "response_4":"%s",
                   "response_5":"%s",
                   "response_6":"%s",
                   "response_7":"%s",

                   "content": "text/html",
                   "c2a_button":"Download Full Clinical Report",
                   "c2a_link":"www.psychlytx.com.au"}}],
                   "template_id":"d-c102ab1090724b6a90a269479f37e943"}'), body_values)) #Pass in the vector of strings to replace placeholders in order.

    return(body)


  })

}
