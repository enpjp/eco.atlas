% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Clean.dates.R
\name{Clean.Dates}
\alias{Clean.Dates}
\title{Clean.Dates}
\usage{
Clean.Dates(x)
}
\arguments{
\item{x}{A vector of dates}
}
\value{
A multicolumn vector of the same length as x with the following values:

"YYMMDD", "week", "YYYY", "DD", "MM", "MMDD", "DOY"
}
\description{
Converts dates in inconsistent formats into a consistent format.
As written it only uses base functions,
}