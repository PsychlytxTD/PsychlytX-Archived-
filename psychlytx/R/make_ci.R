#' Confidence Interval Calculation
#'
#' Calculate the confidence interval
#'
#' @param confidence A numeric value representing the user-selected level of confidence for estimation
#'
#' @param se A numeric value representing the standard error of prediction for the subscale
#'
#' @examples make_ci(1.96, 0.14)
#'
#' @export


make_ci<- function(confidence, se) {

  ci<- confidence * se

  return(ci)

}
