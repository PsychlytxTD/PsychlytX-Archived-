#' Convert GAD-7 Measure Responses
#'
#' Convert GAD-7 raw measure responses into a readable table, to be send in an email to the clinician.
#'
#' @param id A string to create the namespace
#'
#' @export


make_gad7_item_table_UI<- function(id) {

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

make_gad7_item_table<- function(input, output, session, manual_entry) {

  reactive({

 converted_responses<- dplyr::case_when( #Convert the client's responses from numerical form to readable responses, teo appear in the email.

   manual_entry()$item_scores == 0 ~ "Not at all",

   manual_entry()$item_scores == 1 ~ "Several days",

   manual_entry()$item_scores == 2 ~ "More than half the days",

   manual_entry()$item_scores == 3 ~ "Nearly every day",

   TRUE ~ as.character(manual_entry()$item_scores)

 )

 questions<- c("1. Feeling nervous, anxious or on edge", "2. Not being able to stop or control worrying", #Store the item numbers and statements in a vector
               "3. Worrying too much about different things", "4. Trouble relaxing", "5. Being so restless it is hard to sit still",
               "6. Becoming easily annoyed or irritable", "7. Feeling afraid as if something awful might happen")

 item_df<- dplyr::data_frame("Item" = questions, "Response" = converted_responses)


return(item_df)

  })

}
