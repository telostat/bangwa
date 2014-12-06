##' Returns a vector sequence of exponential weights of a lambda
##' factor.
##'
##' @param length desired length of the sequence.
##' @param lambda parameter for the weighting scheme
##' @param start starting index of the sequence, defaults to 0.
##' @return a vector of exponential weights
##' @examples
##' inVector <- rnorm(100)
##' expWeights <- expSeq(length(inVector), lambda=0.94, start=0L)
##' outVector <- inVector * expWeights
##' @export
expSeq <- function (length, lambda, start=0L) {
  (lambda ^ ((length + start - 1):start)) * (1 - lambda)
}

##' Returns the exponentially-weighted series of the original series.
##'
##' \code{expWeights} is a function that calculates the exponential weight vector
##' and multiplied the original series. It consumes a lambda parameter.
##'
##' @param x the original univariate series
##' @param lambda the parameter for the weighting scheme
##' @return the exponentially-weighted series
##' @examples
##' rets <- data.frame(rets=rnorm(12) / 100)
##' expRets <- expWeighted(as.vector(rets$rets), 0.94)
##' @export
expWeighted <- function (x, lambda) {
  stopifnot(is.vector(x))
  x * expSeq(length(x), lambda)
}

##' Returns the exponentially-weighted series of the original series.
##'
##' \code{expWeights} is a function that calculates the exponential weight vector
##' and multiplied the original series. It consumes a lambda parameter.
##'
##' @param x the original univariate series
##' @param lambda the parameter for the weighting scheme
##' @examples
##' returns   <- as.data.frame(rnorm(12) / 100)
##' lambda    <- 0.94
##' myExpW    <- expWeight(returns, lambda)
##' @export
expWeight <- function (x, lambda) {
  .Deprecated("expWeighted",
              package="ors",
              "expWeight warning: A faster version is implemented as expWeighted")

  x$wts    <- 0
  myLength <- length(x[,1])
  myWts    <- lambda^c(0:(myLength-1))
  myWts    <- myWts * (1-lambda)
  x$wts    <- sort(myWts)
  x[,1] * x[,2]
}
