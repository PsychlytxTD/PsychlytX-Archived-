% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reliability_calcs.R
\name{f_rel}
\alias{f_rel}
\title{Test-retest reliability derived from F-statistic}
\usage{
f_rel(calc_sd, calc_retest_sd, calc_mean, calc_retest_mean, calc_f, calc_n,
  digits = 2)
}
\arguments{
\item{calc_sd}{Numeric value representing the baseline standard deviation.}

\item{calc_retest_sd}{Numeric value representing retest standard deviation.}

\item{calc_f}{Numeric value representing the F-statistic.}

\item{calc_n}{Numeric value representing the sample size.}

\item{calc_m}{Numeric value representing the baseline mean.}

\item{calc_retest_m}{Numeric value representing the restest mean.}
}
\description{
Calculates test-retest reliability from the T-statistic
}
\examples{
frel(1.3, 1.4, 30, 31, 2.75, 80)

}
