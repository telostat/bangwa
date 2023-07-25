##' Calculates the weights of position data.
##'
##' \code{getWeights} is a function that calculates the percentage
##' weights of positions data and returns a list with weights.
##' As arguments,##' it consumes the list of dates for which one wants to
##' know the weights, the position data as nominal units and
##' the starting AUM.
##' @param dateList the list with dates as POSIXct for which weights should be calculated.
##' @param actPos an xts object with position data as nominal values.
##' @param AUM a numeric value indicating the asset under management at beginning.
##' @return weights
##' @examples
##' dates <- as.POSIXct(c(("2010-01-01"), ("2010-02-01"), ("2010-03-01")))
##' AUM   <- 1000
##' myPos <- as.xts(data.frame(A=c(100, -200, 400), B=c(200, 400, -200)), order.by=dates)
##' myWeights <- getWeights(as.list(dates), myPos, AUM)
##' @export
getWeights <- function(dateList, actPos, AUM){
  lapply(dateList, function(x) actPos[which(index(actPos) == x),] / AUM)
}
