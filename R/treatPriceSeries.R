##' This function removes NA's and optionally detects and remove outliers. 
##' 
##' @param price The xts price series (either univariate or OHLC).
##' @param outlierTreatment Flag for treating outliers. 
##' @param OHLC Flag to indicate whether we are dealing with OHLC.
##' @param quantile The quantile for the outlier detection
##' @param surpressPause TODO
##' @return Cleaned price series
##' @import timeSeries
##' @export
##'
treatPriceSeries <- function(price, outlierTreatment = FALSE, OHLC = FALSE, quantile=0.998, surpressPause=FALSE){

    ## Remove NA's and replace with last observable value:
     price         <- as.xts(na.locf(price))

     ## Check if OHLC Flag is true:
     if(OHLC == TRUE){
         
         ## Store the open, high, low series and get the get the close series:
         clOpen <- as.numeric(price[,4] / price[,1])
         clHigh <- as.numeric(price[,4] / price[,2])
         clLow  <- as.numeric(price[,4] / price[,3])
         price  <- price[,4]
     }

     if(outlierTreatment == TRUE){
     ## Store the orignal NA removed series:
     originalPrice      <- price

     ## Compute the original returns using the simple method:
     originalRets       <- as.numeric(returns(price, "simple"))

     ## Replace NA for first observations with the mean return:
     originalRets[is.na(originalRets)] <- as.numeric(mean(na.omit(originalRets)))

     ## Compute the returns for our analysis using compoung method:
     rets               <- as.numeric(returns(price, "compound"))

     ## Replace NA for first observations with the mean return:
     rets[is.na(rets)]  <- as.numeric(mean(na.omit(rets)))

     ## Create the GARCH(1,1) object using fixed a0, a1, b1 parameters:
     garchObj           <- list("order"=c("p"=1, "q"=1),
                                "residuals"=rets*100,
                                "call"=call("garch", x=rets, order = c(1, 1)),
                                "coef"=c("a0"=0, "a1"=0.08, "b1"=0.90),
                                "series"=as.character("rets"))

     ## Assign the GARCH class:
     class(garchObj)    <- "garch"

     ## Fit the GARCH object to the returns and get the estimated conditional volatility:
     fittedGarch        <- predict(garchObj, rets)[,1]

     ## First estimate will be our square-root of squared return:
     fittedGarch[1]     <- sqrt(rets[1]^2)

     ## Compute the residuals, squared returns less the estimated variance:
     residuals          <- rets^2 - fittedGarch^2

     ## Compute the density of the residuals:
     resDensity         <- density(residuals)

     ## Sample the residuals using the density bandwidth parameter:
     resDraw            <- rnorm(100000, sample(as.numeric(residuals), size = 100000, replace = TRUE), resDensity$bw)

     ## Calculate the cut-off residuals using quantile parameter:
     cutOff             <- quantile(resDraw, quantile)

     ## Get the index of outliers:
     outlierIndex       <- which(residuals^2 > cutOff)

     ## Set original return outlier index to zero:
     originalRets[outlierIndex] <- 0

     ## Reconstruct the price series:
     price <- as.xts(cumprod(1+originalRets) * as.numeric(originalPrice[1]), order.by=index(originalPrice))

     ## Run the outlier plot:
     outlierPlot(originalPrice, price, outlierIndex)
     
     if(surpressPause == FALSE){
     readline(prompt = "Pause. Press <Enter> to continue...")
 }
     dev.off()
     ## For OHLC series, reconstruct the open, high, low using original distances:
     if(OHLC == TRUE){
         price <- cbind("Open" = price / clOpen, "High" = price / clHigh, "Low"=price / clLow, "Close"=price)
     }
 }
     
     price
 }
