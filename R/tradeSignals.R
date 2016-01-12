##' Function to generate signal from ADX indictor
##'
##' \code{ADXSignal} is a function that returns either 1, 0 or -1.
##' @param ADX ADX dataframe.
##' @param allowShort TODO
##' @param DIcut TODO
##' @param accelCut TODO
##' @param ADXcut TODO
##' @param rankIndicator TODO
##' @import xts
##' @export
ADXSignal <- function(ADX, allowShort=FALSE, DIcut=0, accelCut=0, ADXcut=0.50, rankIndicator="ADX"){

    ## Should ADX or DX to be ranked:    
    if (rankIndicator=="ADX") {
        myRank <- rank(as.numeric(ADX$ADX))
    } else {
        myRank <- rank(as.numeric(ADX$DX))
    }

    ## Add the normalised strength factor to the data:
    ADX       <- cbind(ADX, "rank"= myRank / max(myRank))

    ## Compute the momentum of the relative trend
    ADX$diff     <- ADX$DIp - ADX$DIn
    ADX$momentum <- diff(ADX$diff)
   
    ## Trend direction criteria:
    trendDirectionLong  <- (ADX$DIp[NROW(ADX)] - ADX$DIn[NROW(ADX)]) < DIcut
    trendDirectionShort <- (ADX$DIn[NROW(ADX)] - ADX$DIp[NROW(ADX)]) < DIcut

    ## Trend momentum criteria:
    trendMomentumLong  <- mean(tail(ADX$momentum,3)) >  accelCut
    trendMomentumShort <- mean(tail(ADX$momentum,3)) < -accelCut

    ## Trend strenth criteria:
    trendStrength <- ADX$rank[NROW(ADX)] > ADXcut

    ## Now, generate the signal:
    if (trendDirectionLong && trendMomentumLong && trendStrength) {
        signal <-  1
    } else if (trendDirectionShort && trendMomentumShort && trendStrength && allowShort) {
        signal <- -1
    } else {
        signal <- 0
    }
    
    return("ADX"=signal)
    
}

