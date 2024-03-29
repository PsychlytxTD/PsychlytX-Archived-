% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_pretherapy_to_db.R
\name{write_pretherapy_analytics_to_db}
\alias{write_pretherapy_analytics_to_db}
\title{Write pre-therapy analytics}
\usage{
write_pretherapy_analytics_to_db(input, output, session, pool,
  analytics_pretherapy)
}
\arguments{
\item{pool}{A pooled db connection}

\item{analytics_pretherapy}{A dataframe containing the pretherapy data to send to the db}
}
\description{
Write pre-therapy analytics data to the db
}
