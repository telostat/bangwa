\name{sampleDates}
\alias{sampleDates}
\title{Returns a list with each element showing in-sample and out-of-sample dates.}
\usage{
sampleDates(indexData, startDate, endDate, lookBack)
}
\arguments{
  \item{indexData}{a vector with xts-class index data}

  \item{startDate}{the start date of the backtesting
  period}

  \item{endDate}{the end date of the backtesting period}

  \item{lookBack}{the look-back period, i.e. the number of
  in-sample observations.}
}
\description{
\code{sampleDates} is a function that returns a list with
each element of the list showing the in-sample start date,
in-sample end date and out-of-sample date of an vector with
xts class values assuming always rolling one period forward
in the sample dates. Further arguments are the start date
and end date of the backtesting and the loob-back period,
i.e. the number of historical in-sample observations.
}
\examples{
indexData <- seq(as.POSIXct("2000-01-01"), by = "month", length.out = 200)
startDate <- "2005-01-01"
endDate   <- "2006-01-01"
lookBack  <- 36
mySamples <- sampleDates(indexData,  startDate, endDate, lookBack)
}

