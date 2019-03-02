#' Collect subscale input
#'
#' Store subscale widget input values in a list
#'
#' @param id String to create a unique namespace.
#'
#' @export



collect_input_UI<-  function(id) {

  ns<- NS(id)

  return(NULL)

}

#' Collect subscale input
#'
#' Store subscale widget input values in a list
#'
#' @param id A string to create a unique namespace.
#'
#' @param manual_entry A list containing the date and original item scores.
#'
#' @param aggregate_scores A list of aggregated subscale scores
#'
#' @param mean_input A list containing the mean value and reference (widget inputs)
#'
#' @param sd_input A list containing the sd value and reference (widget inputs)
#'
#' @param reliability_input A list containing the test-retest value and reference (widget inputs)
#'
#' @param cutoff_input A list containing the cutoff values, labels and references (widget inputs)
#'
#' @param confidence A numeric value indicating the level of confidence selected for intervals (widget input)
#'
#' @param method A string value indicating the reliable change method selected (widget value)



collect_input<- function(input, output, session, manual_entry, aggregate_scores, mean_input, sd_input, reliability_input, cutoff_input, confidence, method, subscale_number) {

reactive({ list( manual_entry()[["date"]], aggregate_scores()[[subscale_number]] , mean_input(), sd_input(), reliability_input(), cutoff_input(), confidence(), method() ) })

}
