


#' Test-retest reliability derived from T-statistic
#'
#' Calculates test-retest reliability from the T-statistic
#'
#' @param calc_sd Numeric value representing the baseline standard deviation.
#' @param calc_retest_sd Numeric value representing retest standard deviation.
#' @param calc_m Numeric value representing the baseline mean.
#' @param calc_retest_m Numeric value representing the restest mean.
#' @param calc_t Numeric value representing the T-statistic.
#' @param calc_n Numeric value representing the sample size.
#'
#'@examples trel(1.3, 1.4, 30, 31, 2.75, 80)
#'
#' @export
t_rel <- function( calc_sd, calc_retest_sd, calc_m, calc_retest_m, calc_t, calc_n, digits = 2 ) {
  ## TODO( you may want to simplify this calculation so it can be split across multiple lines - this will make it easier to read)
  round( ( ( ( calc_sd * calc_sd ) + ( calc_retest_sd * calc_retest_sd ) ) - ( ( ( calc_retest_m - calc_m ) / calc_t ) * ( ( calc_retest_m - calc_m ) / calc_t ) * calc_n ) ) / ( 2 * calc_sd * calc_retest_sd ), digits )
}


#' Test-retest reliability derived from F-statistic
#'
#' Calculates test-retest reliability from the T-statistic
#'
#' @param calc_sd Numeric value representing the baseline standard deviation.
#' @param calc_retest_sd Numeric value representing retest standard deviation.
#' @param calc_m Numeric value representing the baseline mean.
#' @param calc_retest_m Numeric value representing the restest mean.
#' @param calc_f Numeric value representing the F-statistic.
#' @param calc_n Numeric value representing the sample size.
#'
#'@examples frel(1.3, 1.4, 30, 31, 2.75, 80)
#'
#' @export
f_rel <- function( calc_sd, calc_retest_sd, calc_m, calc_retest_m, calc_f, calc_n, digits = 2 ) {
  round( ( ( ( calc_sd * calc_sd ) + ( calc_retest_sd * calc_retest_sd ) ) - ( ( ( calc_retest_m - calc_m) / ( sqrt(calc_f) ) ) * ( ( calc_retest_m - calc_m ) / ( sqrt(calc_f) ) ) * calc_n ) ) / ( 2 * calc_sd * calc_retest_sd ), digits)
}

