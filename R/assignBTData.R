##' Assigns the data to a list of backtesting sample dates.
##'
##' \code{assignBTData} is a function that assigns the corresponding data
##' to the list with backtesting sample dates. Resulting list elements contain
##' the corresponding in-sample and out-of-sample data.
##' @param sampleDates the list with elements containing in-sample start, in-sample end and out-of-sample dates
##' @param data the xts data-frame object with the corresponding data
##' @examples
##' indexData <- seq(as.POSIXct("2000-01-01"), by = "month", length.out=200)
##' startDate <- "2005-01-01"
##' endDate   <- "2006-01-01"
##' lookBack  <- 36
##' mySamples <- sampleDates(indexData, startDate, endDate, lookBack)
##' data      <- data.frame(A=rnorm(200), B=rnorm(200), D=rnorm(200))
##' data      <- as.xts(data, order.by=indexData)
##' myBTData  <- lapply(mySamples, function(x) assignBTData(x, data))
##' ##' @export
assignBTData <- function (sampleDates, data) {
  data[which(index(data) >= as.POSIXct(as.character(sampleDates[1])) &
             index(data) <= as.POSIXct(as.character(sampleDates[3]))),]
}
