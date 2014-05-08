\name{quantSelect}
\alias{quantSelect}
\title{Returns the index for desired quantiles of a numeric vector.}
\usage{
quantSelect(x, condition = "lower", prob = 0.05)
}
\arguments{
  \item{x}{a numeric vector with the distribution}

  \item{condition}{values are either "lower" or "upper"}

  \item{prob}{the percentile}
}
\description{
\code{quantSelect} is a function that selects the desired
quantiles of a numeric vector and returns the index. of
those quantiles.If the objective is to select the lower
quantiles, it will return the lesser of the indicated
probability, if upper quantile it will return the bigger of
the indicated probability. If condition is NULL, it will
retun the index between 0.4 and 0.6 of the quantiles.
}
\examples{
x         <- rnorm(1000)
condition <- "lower"
prob      <- 0.05
myQuants  <-quantSelect(x, condition, prob)
}

