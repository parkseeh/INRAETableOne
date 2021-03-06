% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createSummary.R
\name{createSummary}
\alias{createSummary}
\title{Create the statistics summary table}
\usage{
createSummary(
  x,
  y,
  data,
  max.x.level = 5,
  show.missing = FALSE,
  paired = FALSE,
  show.total = FALSE,
  verbose = FALSE,
  origData
)
}
\arguments{
\item{x}{a independent variable}

\item{y}{a dependent variable}

\item{data}{A \code{data.frame}}

\item{paired}{A Boolean expression (T/F) whether to perform the paired t-test.
The default value is \code{FALSE}}

\item{verbose}{Print the log message. The default value is \code{FALSE}}
}
\value{
A list containing the class of variable, total count of data, and
p-value. In addition, the min, max, median, sd, mean will be produced only
for continuous variable
}
\description{
It produces the list containing the detail of summary statistics table for
the type of \code{y} (dependent variable) either continuous or categorical
}
