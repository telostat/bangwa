\name{serialize}
\alias{serialize}
\title{Serializes a data matrix without time-stamp.}
\usage{
serialize(x, startDate, frequency)
}
\arguments{
  \item{x}{the data-frame or matrix with M columns and N
  rows}

  \item{startDate}{the starting date of x}

  \item{frequency}{the frequency of the data, either "day",
  "week", or "month"}
}
\description{
\code{serialize} is a function that produces a xts
time-series object. For a given data-frame, a starting date
and frequency - "day", "week", "month", it generates a xts
time index. It requires "xts" library.
}
\examples{
myXTS <- serialize(data.frame("A" = c(2,3,5), "B" = c(1,4,6)), "2010-01-01", "month")
}

