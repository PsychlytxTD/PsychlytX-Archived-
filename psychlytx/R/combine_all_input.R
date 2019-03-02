#' Combine All Input
#'
#' Combine all lists of widget input values
#'
#' @param id A string to create the namespace

combine_all_input_UI<- function(id) {

  ns<- NS(id)

  return(NULL)

}




#' Combine All Input
#'
#' Combine all lists of widget input values
#'
#' @param input_list A list of input lists (one for each subscale)
#'
#' @export


combine_all_input<- function(input, output, session, input_list) {

  reactive({ list( input_list() ) %>% purrr::map( ~ purrr::flatten(.x) %>% purrr::set_names(c("date", "score", "mean_value",
          "mean_reference","sd_value", "sd_reference", "reliability_value", "reliability_reference", "cutoff_label",
           "cutoff_value", "cutoff_reference", "confidence", "method")) ) })



}
