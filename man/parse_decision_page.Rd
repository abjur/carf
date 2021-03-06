% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_aux.R
\name{parse_decision_page}
\alias{parse_decision_page}
\title{Auxiliary function to call [parse_div()] for each div}
\usage{
parse_decision_page(file, verbose, pb)
}
\arguments{
\item{file}{A character vector of one or more paths to page files}

\item{verbose}{Whether or not to create a progress bar}

\item{pb}{Progress bar created by [parse_decision()]}
}
\value{
A tibble with the parsed data
}
\description{
Auxiliary function to call [parse_div()] for each div
}
