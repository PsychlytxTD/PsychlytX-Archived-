#' Predicted True Score Calculation
#'
#' Calculate the predicted true score
#'
#' @param score A numeric value representing the aggregate subscale score
#'
#' @param mean A numeric value representing the mean score for the subscale
#'
#' @param reliability numeric value representing the test-retest reliability for the subscale
#'
#' @param reliable_change_method A string representing the method of reliable change calculation
#'
#' @examples make_pts(8, 7.2, 0.90, "Nunnally & Bernstein (1994)")
#'
#' @export


make_pts<- function(score, mean, reliability, reliable_change_method) {

  pts<- switch(reliable_change_method,

               "Nunnally & Bernstein (1994)" = round( (reliability * score) + (mean * (1 - reliability)), digits = 2),

               "Jacobson & Truax (1991)" = score,

               "Custom Confidence Intervals" = NA_real_


  )

  return(pts)

}
