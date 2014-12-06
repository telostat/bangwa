## A function for return performance analytics:
##'
##' \code{performanceAnalytics} TODO
##' 
##' @param x TODO
##' @param frequency TODO
##' @examples
##' myIndex   <- seq(as.POSIXct("2000-01-01"), as.POSIXct("2012-01-01"), by="month")
##' data      <- as.xts(cbind(rnorm(length(myIndex)), rnorm(length(myIndex))), myIndex)
##' pAnalytix <- performanceAnalytics(data[,1], 12)
##' @import xts
##' @import PerformanceAnalytics
##' @export
performanceAnalytics <- function(x, frequency){

   x            <- na.omit(x)
   totRet       <- sum(x)
   annRet       <- mean(x) * frequency
   annVol       <- sd(x) * sqrt(frequency)
   sharpe       <- annRet / annVol
   maxDD        <- maxDrawdown(x)
   sortino      <- sum(x) / maxDD
   peaktotrough <- max(findDrawdowns(x)$peaktotrough)
   recovery     <- max(findDrawdowns(x)$recovery)

   browser()
   return(rbind("Total Return" = totRet, "Annualised Return" = annRet, "Annualised Volatility" = annVol,
               "Sharpe"   = sharpe, "Max Drawdown"  = maxDD, "Sortino" = sortino,
               "Peak-to-Through" = peaktotrough, "Recovery" = recovery))
}
