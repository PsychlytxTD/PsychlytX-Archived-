#' Standard Error Calculation
#'
#' Calculate the standard error of prediction
#'
#' @param sd A numeric value representing the standard deviation of the subscale
#'
#' @param reliability numeric value representing the test-retest reliability for the subscale
#'
#' @param reliable_change_method A string representing the method of reliable change calculation
#'
#' @examples make_se(.32, .90, "Nunnally & Bernstein (1994)")
#'
#' @export


make_se<- function(sd, reliability, reliable_change_method) {

  se<- switch(reliable_change_method,

               "Nunnally & Bernstein (1994)" = round( sd * sqrt(1 - reliability ^ 2), digits = 2 ),

               "Jacobson & Truax (1991)" = round( sqrt((2 * (sd ^ 2)) * (1 - reliability)), digits = 2),

               "Custom Confidence Intervals" = NA_real_


  )

  return(se)

}
