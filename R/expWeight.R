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
  x$wts    <- 0
  myLength <- length(x[,1])
  myWts    <- lambda^c(0:(myLength-1))
  myWts    <- myWts * (1-lambda)
  x$wts    <- sort(myWts)
  x[,1] * x[,2]
}

