% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simplified_app_collect_input.R
\name{simplified_collect_holding_input}
\alias{simplified_collect_holding_input}
\title{Collect Simplified Application Subscale Input}
\usage{
simplified_collect_holding_input(input, output, session, holding_data,
  manual_entry, aggregate_scores, mean_input, sd_input, reliability_input,
  confidence, method, cutoff_input, subscale_number)
}
\arguments{
\item{holding_data}{A dataframe containing the relevant statistics for the client selected by the clinician.}

\item{aggregate_scores}{A list of aggregated subscale scores.}

\item{mean_input}{A list containing the mean value and reference (widget inputs).}

\item{sd_input}{A list containing the sd value and reference (widget inputs).}

\item{reliability_input}{A list containing the test-retest value and reference (widget inputs).}

\item{confidence}{A numeric value indicating the level of confidence selected for intervals (widget input).}

\item{method}{A string value indicating the reliable change method selected (widget value).}

\item{cutoff_input}{A list containing the cutoff values, labels and references (widget inputs).}

\item{subscale_number}{A numeric value representing which index (i.e. which subscale) to select from the list of indices.}
}
\description{
Collect simplified application subscale data for writing to db.
}
