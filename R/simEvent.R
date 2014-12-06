##' Simulates a binary events.
##'
##' \code{simEvent} TODO
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


##' TODO
##' 
##' @param hitRatio TODO
##' @param returns TODO
##' @return simulated directions.
##' @export
simPortDirections <- function(hitRatio, returns){

    ## Simulate the right calls:
    rightCall     <- do.call(cbind, simEvent(hitRatio, nrow(returns), ncol(returns), nonEvent=-1))

    ## Compute the directions associated with the right calls:
    simDirection  <- as.matrix(sign(rightCall * returns))

    ## Fill the zero directions with random signs:
    simDirection[which(simDirection == 0)] <- sign(rnorm(length(which(simDirection == 0)))) 

    ## Return the simulated directions:
    simDirection

}
