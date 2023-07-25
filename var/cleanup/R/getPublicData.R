##' Constructs \code{orsDEoptim} parameters.
##'
##' @param tickers TODO
##' @param startDate TODO
##' @param endDate TODO
##' @param quote TODO
##' @param compression TODO
##' @param provider TODO
##' @return List with OHLC data.
##' @import tseries
##' @export
##' 
getPublicQuote <- function(tickers, startDate, endDate, quote, provider, compression){

   auxFun <- function(x, startDate, endDate, quote, provider, compression){
        
        get.hist.quote(instrument=x, start=startDate, end=endDate, quote=quote, provider=provider, compression=compression)
        
    }
                        
   lapply(tickers, auxFun, startDate, endDate, quote, provider, compression)
}
