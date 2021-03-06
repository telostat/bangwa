% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/expWeight.R
\name{expWeighted}
\alias{expWeighted}
\title{Returns the exponentially-weighted series of the original series.}
\usage{
expWeighted(x, lambda = 0.94)
}
\arguments{
\item{x}{the original univariate series}

\item{lambda}{the parameter for the weighting scheme}
}
\value{
the exponentially-weighted series
}
\description{
\code{expWeights} is a function that calculates the exponential weight vector
and multiplied the original series. It consumes a lambda parameter.
}
\examples{
rets <- data.frame(rets=rnorm(12) / 100)
expRets <- expWeighted(as.vector(rets$rets), 0.94)
}

