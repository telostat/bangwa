##' Simulates a binary event.
##'
##' \code{simEvent} is a function that simulates binary events for
##' a given number of variables and a length of observations
##' and probabilities of events occuring. It returns a list with
##' the simulations for each set
##'
##' @param eventProb a numeric vector with the probabilities of event. Length equals the number of variables.
##' @param length.out the number of rows for each variable
##' @param noSim the number of simulations for each set
##' @param nonEvent the value for the non-events
##' @return simulated binary event
##' @examples
##' eventProb <- c(0.55, 0.6)
##' length.out<- 200
##' noSim     <- 1000
##' nonEvent  <- -1
##' myEvents  <- simEvent(eventProb, length.out, noSim, nonEvent)
##' @export
simEvent <- function(eventProb, length.out, noSim, nonEvent=-1){
  auxEvent <- rep(list(eventProb), noSim)

  auxFun <- function (y){
    zs = NULL
    for(i in 1:length(y)){
      z  <- rbinom(length.out, size = 1, prob = y[i])
      z[which(z ==0)] = nonEvent
      zs <- cbind(zs, z)
    }
    zs
  }

  lapply(auxEvent, auxFun)
}
