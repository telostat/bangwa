##' Returns a list with each element showing in-sample and out-of-sample dates. 
##'
##' \code{sampleDates} is a function that returns a list with each element of
##' the list showing the in-sample start date, in-sample end date and
##' out-of-sample date of an vector with xts class values assuming always
##' rolling one period forward in the sample dates. Further arguments
##' are the start date and end date of the backtesting and the loob-back
##' period, i.e. the number of historical in-sample observations. 
##' @param indexData a vector with xts-class index data
##' @param startDate the start date of the backtesting period
##' @param endDate the end date of the backtesting period
##' @param lookBack the look-back period, i.e. the number of in-sample observations.
##' @examples
##' indexData <- seq(as.POSIXct("2000-01-01"), by = "month", length.out = 200)
##' startDate <- "2005-01-01"
##' endDate   <- "2006-01-01"
##' lookBack  <- 36
##' mySamples <- sampleDates(indexData,  startDate, endDate, lookBack)
##' @export
sampleDates <- function (indexData, startDate, endDate, lookBack) {

    startDate    <- as.POSIXct(startDate)
    endDate      <- as.POSIXct(endDate)
    
    isStartDate  <- indexData[(which(abs(indexData - startDate) ==
                                     min(abs(indexData - startDate))))-lookBack]
    isEndDate    <- indexData[(which(abs(indexData - isStartDate) ==
                                     min(abs(indexData - isStartDate))))+lookBack]
    oosStartDate <- indexData[(which(abs(indexData - isEndDate) ==
                                     min(abs(indexData - isEndDate))) + 1)]
    oosEndDate   <- indexData[which(abs(indexData - endDate) ==
                                    min(abs(indexData - endDate)))]
    oosPeriod    <- length(indexData[which(indexData >= oosStartDate & indexData <= oosEndDate)])

    dateList <- NULL
  for(i in 1:oosPeriod){
    isStartDateX  <- indexData[which(indexData == isStartDate) + (i - 1)]
    isEndDateX    <- indexData[which(indexData == isEndDate) + (i - 1)]
    oosStartDateX <- indexData[which(indexData == isEndDate) + i]
    dates         <- c(isStartDateX, isEndDateX, oosStartDateX)
    dateList      <- c(dateList, list(dates))
}
    dateList
}
