


#' T Rel
#'
#' Calculates the something something
#'
#' @param calc_sd numeric representing the standard deviation
#' @param calc_retest_sd
#' @param calc_retest_m
#' @param calc_m
#' @param calc_t
#' @param calc_n
#'
#' @export
t_rel <- function( calc_sd, calc_retest_sd, calc_retest_m, calc_m, calc_t, calc_n, digits = 2 ) {
  ## TODO( you may want to simplify this calculation so it can be split across multiple lines - this will make it easier to read)
  round( ( ( ( calc_sd * calc_sd ) + ( calc_retest_sd * calc_retest_sd ) ) - ( ( ( calc_retest_m - calc_m ) / calc_t ) * ( ( calc_retest_m - calc_m ) / calc_t ) * calc_n ) ) / ( 2 * calc_sd * calc_retest_sd ), digits )
}


