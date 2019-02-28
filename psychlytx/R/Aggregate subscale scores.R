#' Aggregate subscale scores
#'
#' Calculates the total score of each subscale
#'
#' @param item_scores A numeric vector of item scores for the entire scale
#'
#' @param item_index A list of numeric vectors representing the item indices for each subscale
#'
#' @param aggregation_method A string value indicating the function that should be used to calculate aggregate scores.
#'
#' @examples calculate_subscale(item_scores = gad7_scores, item_index = list(c(1,2,3,4,5), c(6,7,8)), aggregation_method = "sum")
#'
#' @export


calculate_subscale<- function(item_scores, item_index, aggregation_method) {

subscale_scores<- purrr::map(.x = item_index, ~ do.call(aggregation_method, list(item_scores[.x], na.rm = TRUE)))

return(subscale_scores)

}
