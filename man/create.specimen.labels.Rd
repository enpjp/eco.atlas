% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create.specimen.labels.R
\name{create.specimen.labels}
\alias{create.specimen.labels}
\title{A Function To Create Data Labels}
\usage{
create.specimen.labels(
  data.ss.for.label,
  file.for.output = "label",
  make.pdf = TRUE,
  small.label = FALSE,
  last.line.comment = FALSE
)
}
\arguments{
\item{data.ss.for.label}{Should contain datumEntity, record.date, site,
grid.ref,recorder.name, method, Code.}

\item{file.for.output}{Name of file to save. Do not add the extension.}

\item{make.pdf}{Automatically build the PDF file.}

\item{small.label}{Logical: make small labels.}

\item{last.line.comment}{Logical: Use the comments field for the last line.}
}
\description{
This function creates a skeleton file to be compiled with Latex
}
