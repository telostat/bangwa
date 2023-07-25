##' Calculates the factor to annualize returns.
##'
##' \code{annFreq} is a function that calculates the factor to annulize returns.
##' Given an object with dates it determines the median difference between
##' consecutive dates and determines the factor to annulize return series.
##'
##' @param dates an object with dates
##' @return annualization factor
##' @examples
##' dates  <- seq(as.POSIXct("2010-01-01"), by="month", length.out=20)
##' myFreq <- annFreq(dates)
##' @export
annFreq <- function (dates) {
  freqDays <- NULL
  for(i in 1:(length(dates)-1)){
    freqDay  <- as.numeric(difftime(dates[i+1], dates[i], units="days"))
    freqDays <- c(freqDays, freqDay)
  }
  360 / median(freqDays)
}
