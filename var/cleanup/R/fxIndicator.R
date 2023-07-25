## library(randomForest)
## library(quantmod)
## library(tseries)
## library(bangwa)

## source("tradeSignals.R")

## ## load("../data/AUDJPY_hour.Rdata")
## ## load("../data/AUDJPY_hour.Rdata")
## ## load("../data/NZDUSD_day.Rdata")
## ## load("../data/USDCAD_day.Rdata")
## load("../data/AUDJPY_day.Rdata")

## fxData <- AUDJPY_day

## colnames(fxData) <- c("Open", "Low", "High", "Close")

## sampleData <- sampleDates(fxData, "2002-01-01", "2013-01-01", 120)
## inSample   <- lapply(sampleData, function(x) na.omit(head(x, length(x[,1])-1)))
## isReturns  <- lapply(inSample,   function(x) na.omit(diff(log(x))))
## oosReturns <- lapply(sampleData, function(x) last(diff(log(x[,'Close']))))

## .ADX <- function(x) {

##     myADX  <- na.omit(cbind(x[,'Close'], ADX(HLC(x), n=floor(NROW(x) / 3))))

##     return(myADX)

## }

## myADX      <- lapply(inSample, function(x) .ADX(x))

## DIpDIn     <- lapply(myADX, function(x) last(x$DIp - x$DIn))
## DX         <- lapply(myADX, function(x) last(x$ADX))
## momentum   <- lapply(myADX, function(x) mean(tail(diff(x$DIp - x$DIn),3)))
## momentumUp <- lapply(myADX, function(x) quantile(na.omit(diff(x$DIp - x$DIn)), 0.5))
## momentumLw <- lapply(myADX, function(x) quantile(na.omit(diff(x$DIn - x$DIp)), 0.75))

## data <- cbind(do.call(rbind, oosReturns),
##               as.numeric(do.call(rbind, DIpDIn)),
##               as.numeric(do.call(rbind, DX)),
##               as.numeric(do.call(rbind, momentum)),
##               as.numeric(do.call(rbind, momentumUp)),
##               as.numeric(do.call(rbind, momentumLw))
##               )

## browser()

## posTrendReturn   <- data[,1][which(data[,4] > data[,5])]
## posTrendStrength <- data[,3][which(data[,4] > data[,5])]
## posTrendValue    <- data[,2][which(data[,4] > data[,5])]

## negTrendReturn   <- data[,1][which(data[,4] > data[,6])]
## negTrendStrength <- data[,3][which(data[,4] > data[,6])]
## negTrendValue    <- data[,2][which(data[,4] > data[,6])]

## posTrendReturn   <- posTrendReturn[which(posTrendStrength > 0)]
## negTrendReturn   <- negTrendReturn[which(negTrendStrength > 0)]

## trading <- cbind(posTrendReturn, 0)#-negTrendReturn)

## trading[,1][is.na(trading[,1])] <- trading[,2][is.na(trading[,1])]

## sharpe  <- (mean(trading[,1]) * 250) / (sd(trading[,1])*sqrt(250))
## buyHold <- (mean(data[,1]) * 250) / (sd(data[,1])*sqrt(250))

## print(c(sharpe, buyHold, sharpe / buyHold))

## relativeValue <- cbind(data[,1], trading[,1])
## #relativeValue[,1][is.na(relativeValue[,2])] <- 0
## relativeValue[is.na(relativeValue)] <- 0

## smartBeta    <- relativeValue[,2] - relativeValue[,1]
## smartBetaCum <- cumsum(smartBeta)
