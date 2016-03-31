##' @param stockPeriod TODO
##' @param country TODO
##' @return fundamental cross-section dataframe.
##' @export
##' 
fundamentalCrossSection <- function(fundamentalData, stockData, commodityDescription, stockPeriod, country){
    
    fundamentalData     <- fundamentalData[which(fundamentalData$Commodity_Description == commodityDescription),]
    fundamentalData     <- fundamentalData[which(fundamentalData$Country_Name == country),]
    domesticConsumption <- fundamentalData[which(fundamentalData$Attribute_Description == "Domestic Consumption"),]
    exports             <- fundamentalData[which(fundamentalData$Attribute_Description == "Exports"),]
    marketingYears      <- exports$Market_Year
    usage               <- as.data.frame(cbind(marketingYears, domesticConsumption$Value + exports$Value))
    rownames(usage)     <- usage[,1]
    colnames(usage)     <- c("Year", "Usage")

    periodicStocks       <- as.data.frame(stockData$Value[which(stockData$Period == stockPeriod)])
    stockDataYears       <- stockData$Year[which(stockData$Period == stockPeriod)]
    stocks               <- cbind("Stocks" = periodicStocks, "Year" = stockDataYears)
    colnames(stocks)     <- c("Stocks", "Year")
    rownames(stocks)     <- stocks$Year
    crossSection         <- merge(stocks, usage, by="Year", all=TRUE)
    crossSection$Usage   <- crossSection$Usage * 36.74 * 1000
    rownames(crossSection) <- c(crossSection$Year)
    crossSection         <- na.omit(crossSection)

    if(stockPeriod == "FIRST OF DEC"){
        
        crossSection$ctu <- crossSection$Stocks / crossSection$Usage

    } else {
        
        crossSection$ctu     <- c(NA, crossSection[(2:nrow(crossSection)),]$Stocks) / crossSection$Usage
    }

    crossSection

}

##' Returns future data.
##'
##' @param year TODO
##' @param futureList TODO
##' @export
##'
getFutureData <- function(year, futureList){

    data <- futureList[[3]][[which(futureList[[2]]==as.numeric(year))]]
    data <- data[which(as.numeric(substr(index(data),1,4))<=(year))]
    data <- as.xts(as.numeric(data),order.by=as.Date(substr(index(data), 1, 10)))
    tail(na.omit(data),600)
}

##' Returns constant month series.
##'
##' @param year TODO
##' @param path TODO
##' @param futureList TODO
##' @param backtestingYears TODO
##' @export
##'
getConstantMonthSeries <- function(year, path, futureList, backtestingYears){

  load(file=paste(path, futureList,".RData",sep = ""))
  backtestingYears <- backtestingYears[which(backtestingYears <= year)]
  futureDataList   <- lapply(backtestingYears, FUN=getFutureData, futList)
  futureDataFrame  <- do.call(cbind, futureDataList)
  colnames(futureDataFrame) <- backtestingYears

  dates        <- substr(index(futureDataFrame), 1, 10)
  switchYears  <- substr(futList[[1]][,2], 1, 4)
  matchedIndex <- match(backtestingYears, as.numeric(switchYears))
  switchDates  <- futList[[1]][matchedIndex, 2]
  
  matchedNames    <- which(colnames(futureDataFrame) == year)
  nextExpiryDates <- which(as.Date(switchDates) - Sys.Date() > 0)[1]
  
  if(is.na(nextExpiryDates)){
    nextExpiryDates <- matchedNames
  }
  
  for(i in ((nextExpiryDates - 1):1)){

      if(i > 1){
          startIndex   <- which(dates == switchDates[(i-1)])
          switchDate.1 <- substr(index(last(na.omit(futureDataFrame[,(i-1)]))), 1, 10)
          
          if(length(startIndex) == 0){
              startIndex <- which(dates==switchDate.1)
          }
          
          endIndex <- which(dates==switchDates[i])          
          switchDate.2 <- substr(index(last(na.omit(futureDataFrame[,i]))), 1, 10)
          
          if(length(endIndex) == 0){
              endIndex <- which(dates==switchDate.2)
          }
          
      } else {
          
      naIndex    <- which(!is.na(futureDataFrame[1,]))
      startIndex <- min(naIndex)
      endIndex   <- which(dates==switchDates[1])
  }
      futureDataFrame[startIndex:endIndex, matchedNames] <- futureDataFrame[startIndex:endIndex,(matchedNames - ((nextExpiryDates-1)-i)-1)]
  }

  na.omit(futureDataFrame[, matchedNames])
}



##' Returns the future cross-section dataframe.
##'
##' @param path TODO
##' @param firstYear TODO
##' @param lastYear TODO
##' @param futureYear TODO
##' @param futureMonth TODO
##' @param futureNext TODO
##' @return future cross-section dataframe.
##' @export
##' 
futureCrossSection <- function(path, firstYear, lastYear, futureYear, futureMonth, futureNext) {

    years            <- seq(firstYear, lastYear)
    years            <- c(years - 1, max(years))
    futureData       <- getConstantMonthSeries(futureYear, path, gsub(" ", "", futureMonth), years)
    futureDataNext   <- getConstantMonthSeries(futureYear, path, gsub(" ", "", futureNext), years)
    futureData       <- futureData - futureDataNext
    futureDataCustom <- customPeriod(futureData, min(years), lastYear, startDate, endDate)
    futureYearStats  <- lapply(futureDataCustom, FUN=yearWiseStats, "spread")
    futureYearStats  <- as.data.frame(na.omit(do.call(rbind, futureYearStats)))

    futureYearStats
}

##' Returns custom period.
##'
##' @param timeSeries TODO
##' @param startYear TODO
##' @param endYear TODO
##' @param startDate TODO
##' @param endDate TODO
##' @import timeSeries
##' @import timeDate
##' @export
##'
customPeriod <- function(timeSeries, startYear, endYear, startDate, endDate) {
  
    customSeries <- NULL
  
  for(i in startYear:endYear){
      
      if(startDate > endDate){
        
        y = i-1
        
    } else {
        
        y = i
    }
    
    startIndex   <- as.Date(paste(y, "-", substr(startDate, 1,2), "-", substr(startDate, 3,4), sep = ""))
    endIndex     <- as.Date(paste(i, "-", substr(endDate, 1,2), "-", substr(endDate, 3,4), sep = ""))
    customTS     <- timeSeries[which(as.Date(index(timeSeries)) > startIndex)]
    customTS     <- customTS[which(as.Date(index(customTS)) < endIndex),]
    customSeries <- c(customSeries, list(customTS))
}
  
  names(customSeries)<-as.character(seq(startYear, endYear))

  customSeries

}

##' Returns yearwise stats.
##'
##' @param data TODO
##' @param type TODO
##' @import moments
##' @export
##'
yearWiseStats <- function(data, type){

    if(length(data) == 63 | length(data) == 0){
        
        retval = cbind(NA,NA,NA,NA)
        
    } else {
        
    if(type == "spread"){
        
        mean   <- mean(as.numeric((na.omit(data))))
        high   <- max(as.numeric((na.omit(data))))
        low    <- min(as.numeric((na.omit(data))))
        sd     <- sd((as.numeric(na.omit(data))))
        skew   <- skewness(as.numeric((na.omit(data))))
        kurt   <- kurtosis(as.numeric((na.omit(data))))
        
    } else if(type == "price") {
        
        mean   <- as.numeric(last(na.omit(data))) - as.numeric(first(na.omit(data)))
        high   <- sd((na.omit(data)))
        low    <- skewness((na.omit(data)))
        sd     <- sd((as.numeric(na.omit(data))))
        skew   <- skewness(as.numeric((na.omit(data))))
        kurt   <- kurtosis(as.numeric((na.omit(data))))
    }
    
    retval = cbind(mean, high, low, sd, skew, kurt)
}
    rownames(retval) = substr(first(index(data)),1,4)
    
    return(retval)
}

##' A function to transform the ctu
##'
##' @param ctu TODO
##' @param exponent TODO
##' @import moments
##' @export
##'
ctuTransform = function(ctu, exponent){
    
    1/(exp(ctu * exponent))
}

##' Model representation
##'
##' @param params TODO
##' @param dependent TODO
##' @export
##'
modelRepresentation <- function(params, dependent = "mean"){

    crossSectionData <- params$crossSectionData
    exponent         <- params$exponent
    transformedCTU   <- ctuTransform(crossSectionData$ctu, exponent)
    independent      <- crossSectionData[,which(colnames(crossSectionData) == dependent)]
    meanModel        <- lm(independent ~ transformedCTU)
    print(summary(meanModel))
    modelCoeffs      <- meanModel$coefficients
    yearIndex        <- crossSectionData$Year

    retval           <- list("crossSectionData"=crossSectionData,
                             "modelCoeffs" = modelCoeffs, 
                             "fitted" = meanModel$fitted, 
                             "model" = meanModel, 
                             "yearIndex" = yearIndex, 
                             "exponent" = exponent,
                             "transformedCTU" = ctuTransform)
    
    retval
}

