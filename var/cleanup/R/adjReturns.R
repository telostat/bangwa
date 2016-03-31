##' Calculates the adjusted returns
##'
##' \code{adjReturns} is a function that calculates the adjusted returns
##' for a given data-frame with returns.
##' @param detrendReturn TODO
##' @param paramList TODO
##' @return adjusted returns
##' @examples
##' detrendReturn <- data.frame("A" = rnorm(12) / 10, "B" = rnorm(12) / 10)
##' direction <- c(1, -1)
##' sharpeAdj <- c(0.9, 1.2)
##' volAdj    <- c(1.1, 0.9)
##' frequency <- 12
##' paramList <- list(direction, sharpeAdj, volAdj, frequency)
##' myAdjRets <- adjReturns(detrendReturn, paramList)
##' @export
adjReturns <- function (detrendReturn, paramList) {
  detrendReturn +
    1 / paramList[[4]]^0.5 *
      paramList[[1]] *
        paramList[[2]] *
          paramList[[3]] *
            apply(detrendReturn, MARGIN=2, sd)
}
