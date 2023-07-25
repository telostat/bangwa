##' A function which returns a list with each element showing chungked sample dates.
##'
##' \code{sampleDates} is a function that returns a list with each element of
##' the list showing the in-sample start date, in-sample end date and
##' out-of-sample date
##' @param data TODO
##' @param startDate TODO
##' @param endDate TODO
##' @param lookBack TODO
##' @param timeZone TODO
##' @param format TODO
##' @examples
##' myIndex   <- seq(as.POSIXct("2000-01-01"), as.POSIXct("2012-01-01"), by="month")
##' data      <- as.xts(cbind(rnorm(length(myIndex)), rnorm(length(myIndex))), myIndex)
##' startDate <- "2005-01-01"
##' endDate   <- "2008-01-01"
##' lookBack  <- 36
##' mySamples <- sampleDates(data, startDate, endDate, lookBack)
##' @import xts
##' @export
sampleDates <- function (data, startDate, endDate, lookBack, timeZone="GMT", format="%Y-%m-%d") {
    indexData    <- as.POSIXct(index(data), tz = timeZone, format = format)
    startDate    <- as.POSIXct(startDate, tz = timeZone, format = format)
    endDate      <- as.POSIXct(endDate, tz = timeZone, format = format)

    sampleEnd       <- indexData[(which(abs(indexData - endDate) == min(abs(indexData - endDate))))]
    sampleStart     <- indexData[(which(abs(indexData - startDate) == min(abs(indexData - startDate))))]
    sampleDates     <- indexData[which(indexData >= sampleStart & indexData <= sampleEnd)]
    oosObservations <- length(sampleDates) - lookBack - 1

    dateList <- NULL

    for(i in 1:oosObservations){
        isStart       <- indexData[which(indexData == sampleStart) + (i - 1)]
        isEnd         <- indexData[which(indexData == isStart) + lookBack]
        oosStart      <- indexData[which(indexData == isEnd) + 1]
        dates         <- list(isStart, isEnd, oosStart)
        dateList      <- c(dateList, list(dates))
    }

    auxFun <- function(sampleDates, data){
      data[which(as.POSIXct(index(data)) >= sampleDates[[1]] &
                 as.POSIXct(index(data)) <= sampleDates[[3]]),]
    }

    data <- lapply(dateList, auxFun , data)

    data
}
