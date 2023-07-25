##' Returns the index for desired quantiles of a numeric vector.  
##'
##' \code{quantSelect} is a function that selects the desired
##' quantiles of a numeric vector and returns the index.
##' of those quantiles.If the objective is to
##' select the lower quantiles, it will return the lesser of
##' the indicated probability, if upper quantile it will
##' return the bigger of the indicated probability. If
##' condition is NULL, it will retun the index between
##' 0.4 and 0.6 of the quantiles. 
##'
##' @param x a numeric vector with the distribution   
##' @param condition values are either "lower" or "upper"
##' @param prob the percentile 
##' @examples
##' x         <- rnorm(1000)
##' condition <- "lower"
##' prob      <- 0.05
##' myQuants  <-quantSelect(x, condition, prob)
##' @export
quantSelect <- function (x, condition = "lower", prob = 0.05){
    selectIndex  <- if(condition == "lower"){
        
        which(x < quantile(x, prob = prob))
        
    } else if (condition == "upper"){
        
        which(x > quantile(x, prob = prob))
        
    } else {

        which(x > quantile(x, prob = 0.40)) &
        which(x < quantile(x, prob = 0.60))

    }
}

