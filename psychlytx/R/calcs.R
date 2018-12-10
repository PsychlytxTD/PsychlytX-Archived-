


#' T Rel
#'
#' @param calc_sd
#' @param calc_retest_sd
#' @param calc_retest_m
#' @param calc_m
#' @param calc_t
#' @param calc_n
#'
t_rel <- function( calc_sd, calc_retest_sd, calc_retest_m, calc_m, calc_t, calc_n, digits = 2 ) {
  round( ( ( ( calc_sd * calc_sd ) + ( calc_retest_sd * calc_retest_sd ) ) - ( ( ( calc_retest_m - calc_m ) / calc_t ) * ( ( calc_retest_m - calc_m ) / calc_t ) * calc_n ) ) / ( 2 * calc_sd * calc_retest_sd ), digits )
}


