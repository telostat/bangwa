##' Serializes a data matrix without time-stamp.
##'
##' \code{serialize} is a function that produces a xts
##' time-series object. For a given data-frame, a starting
##' date and frequency - "day", "week", "month", it generates
##' a xts time index. It requires "xts" library.
##' @param x the data-frame or matrix with M columns and N rows
##' @param startDate the starting date of x
##' @param frequency the frequency of the data, either "day", "week", or "month"
##' @examples
##' myXTS <- serialize(data.frame("A" = c(2,3,5), "B" = c(1,4,6)), "2010-01-01", "month")
##' @export
##' @import xts
serialize <- function(x, startDate, frequency){
    noObs = dim(x)[1]
    myIndex <- seq(as.POSIXct(startDate), by = frequency, length.out = noObs)
    retval <- as.xts(x, order.by = myIndex)
    return(retval)
}
