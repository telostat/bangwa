##' This function plots the original price series and the corresponding
##' outlier treated series.
##' 
##' @param originalSeries TODO
##' @param newSeries TODO
##' @param outlierIndex TODO
##' @export
##'
outlierPlot <- function(originalSeries, newSeries, outlierIndex){

    ## Some color ideas for the plot background:
    bgCol <- terrain.colors(20, alpha = 0.2)

    ## Get the y-axis limits:
    ylim1 <- c(min(na.omit(originalSeries))* 0.95, max(na.omit(originalSeries)) * 1.05)
    ylim2 <- c(min(na.omit(newSeries)) * 0.95, max(na.omit(newSeries)) * 1.05)
  
    
    ## Setting the par values for plot:
    op <- par(mfrow=c(2,1),
              mar=c(0.9,3,1.5,0.9),
              #cex.axis=0.7,
              #cex.main=1.2,
              col.main="darkblue",
              fg="darkblue",
              xaxt=c("s", "l", "t", "n")[2],
              bg=bgCol[10],
              bty=c("o", "l", "7", "c", "u", "]")[2],
              col.axis="darkblue")

    ## Get the points with the outliers:
    xPoint  <- as.POSIXct(index(originalSeries)[outlierIndex])
    yPoint  <- as.numeric(originalSeries[outlierIndex])

    ## Now, let's plot:
    plot(originalSeries, ylim=ylim1, las=1, main = "Original Series vs New Series")
    points(x=xPoint, y=yPoint, pch = 1, col = "red", bg = "red", cex = 1)
    #text(x=as.POSIXct(txnDates), y=(txnPX*1.10), labels = (txnPOS), cex = 0.5, col = myCol)
    #text(x=as.POSIXct(txnDates), y=(txnPX*0.90), labels = (txnPX), cex = 0.5, col = "black")
    par(mar=c(0.9,3,3,0.9))
    plot.xts(newSeries, main = "", xlab="", mgp = c(0, 1.2, 0), xaxt="n", ylim=ylim2, las=1)
    #points(x=xPoint, y=yPoint, pch=1, col="red",cex=1)
    
}
