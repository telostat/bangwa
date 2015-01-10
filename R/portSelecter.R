##' Selects a portfolio from a set of portfolios
##'
##' @param priceSeries A xts data-frame with asset prices. 
##' @param startDate TODO
##' @param endDate TODO
##' @param quote TODO
##' @param compression TODO
##' @param provider TODO
##' @param weights TODO
##' @param lookBack TODO
##' @param minWeight TODO
##' @param maxWeight TODO          
##' @param minTotalNetWeight TODO  
##' @param minTotalGrossWeight TODO
##' @param maxTotalNetWeight TODO
##' @param maxTotalGrossWeight TODO
##' @param noOfSteps TODO          
##' @param sims TODO
##' @param statsFN TODO
##' @param selectFN TODO
##' @param dataFN TODO
##' @return A list of random deviations, portfolios, lower bound,
##' and upper bound.
##' @import bangwa
##' @export
##' 
portSelecter <- function(priceSeries,
                         startDate,
                         endDate,
                         weights,
                         lookBack,
                         minWeight,
                         maxWeight,
                         minTotalNetWeight,
                         minTotalGrossWeight,
                         maxTotalNetWeight,
                         maxTotalGrossWeight,
                         noOfSteps,
                         sims,
                         statsFN,
                         selectFN,
                         dataFN=NULL,
                         ...
                         ) {

    ## Get the xts index:
    myIndex <- index(priceSeries)

    ## Compute returns:
    returns <- returns(priceSeries)

    ## Omit NA's:
    returns <- na.omit(returns)

    ## Revert to xts:
    returns <- as.xts(returns)

    ## Get sample xts index:
    sampleIndex <- index(returns)

    ## Get the sample data as list:
    sampleData <- sampleDates(returns, startDate, endDate, lookBack, timeZone = "GMT", format = "%Y-%m-%d")

    ## Generate in-sample data by omitting last observation:
    isData <- lapply(sampleData, function(x) na.omit(head(x, length(x[,1])-1)))

    ## In-sample data tranformation function applied: 
    if(is.null(dataFN) == FALSE){
        isData <- lapply(isData, function(x) apply(x, MARGIN=2, function(x) do.call(dataFN[[1]], c(list(x), dataFN[-1]))))
    }

    ## Get the out-of-sample data: 
    oosData <- lapply(sampleData, function(x) last(x))

    ## Get random portfolios:
    portWeights <- portRandomizer(weights, minWeight, maxWeight, minTotalNetWeight, minTotalGrossWeight, maxTotalNetWeight, maxTotalGrossWeight, noOfSteps, sims)

    ## Replicate random portfolios for each element in in-sample data list:
    portWeights <- rep(list(portWeights), length(isData))

    ## Compute in-sample portfolio returns:
    isPortReturns <- lapply(1:length(portWeights), function(i)
        as.matrix(isData[[i]]) %*% t(portWeights[[i]])
                              )

    ## Apply portfolio statistics function to returns:
    isPortStats <- lapply(isPortReturns, statsFN)

    ## Apply selection function to portfolio statistics and select winner:
    isPortWinner <- lapply(isPortStats, function(x) which(x == selectFN(x)))

    ## Get the winning weights for each in-samle data set:
    isPortWeights <- portWeights[[1]][as.numeric(do.call(rbind, isPortWinner)),]

    ## Calculate out-of-sample returns for each in-sample winning weights:  
    oosWinnerRets <- do.call(rbind, oosData) * isPortWeights

    ## Compute cumulative sum of out-of-sample portfolio returns:
    oosWinnerCum  <- cumsum(rowSums(oosWinnerRets))

    ## Compute all out-of-sample returns for all random portfolios:
    oosPortReturns <- lapply(1:length(portWeights), function(i)
        as.matrix(oosData[[i]]) %*% t(portWeights[[i]])
                             )

    ## Transform to random portfolios out-of-sample returns to data-frame:
    oosPortReturns <- do.call(rbind, oosPortReturns)

    ## Compute cumulative sum of out-of-sample portfolio returns for random portfolios:
    oosPortCumsum <- apply(oosPortReturns, MARGIN=2, cumsum)

    ## Compute total returns of random portfolios:
    totalReturns <- oosPortCumsum[nrow(oosPortCumsum),]

    ## Get the indices of 100 randomly chosen lower, mid and upper percentile total returns:
    lowIndex <- sample(which(totalReturns < quantile(totalReturns, 0.25)))[1:100]
    highIndex <- sample(which(totalReturns > quantile(totalReturns, 0.75)))[1:100]
    midIndex <- sample(which(totalReturns > quantile(totalReturns, 0.25) & totalReturns < quantile(totalReturns, 0.75)))[1:100]    
    selectIndex <- c(lowIndex, highIndex, midIndex)

    ## Filter cumulative returns of random portfolios:
    oosPortCumsum <- oosPortCumsum[,selectIndex]

    ## Done, return!
    list("totalReturns"=totalReturns,
         "isPortStats"=isPortStats,
         "isPortWeights"=isPortWeights,
         "oosWinnerRets"=oosWinnerRets,
         "oosPortCumsum"=oosPortCumsum
         )
    
}
   
