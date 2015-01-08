##' Constructs \code{orsDEoptim} parameters.
##'
##' @param weights A vector of weights as provided by the PM.
##' @param minWeight The minimum possible weight constraint per asset.
##' @param maxWeight The maximum possible weight constraint per asset.
##' @param minTotalNetWeight The minimum possible total net weight
##' constraint per portfolio.
##' @param minTotalGrossWeight The minimum possible total gross weight
##' constraint per portfolio.
##' @param maxTotalNetWeight The maximum possible total net weight
##' constraint, ie. sum of all position weights.
##' @param maxTotalGrossWeight The maximum possible total gross weight
##' constraint, ie. sum of all absolute position weights.
##' @param noOfSteps The number of steps within the min/max
##' boundaries. The greater the finer, thus more simulated deviations.
##' @param sims TODO
##' @return A list of random deviations, portfolios, lower bound,
##' and upper bound.
##' @examples
##' weights             <- c(0.10, -0.05, 0.20, 0.05, -0.01)
##' minWeight           <- 0.01
##' maxWeight           <- 0.25
##' minTotalNetWeight   <- -1
##' minTotalGrossWeight <- 0
##' maxTotalNetWeight   <- 1
##' maxTotalGrossWeight <- 1.5
##' noOfSteps           <- 10
##' sims                <- 10000
##' randPorts <- portRandomizer(weights, minWeight, maxWeight, minTotalNetWeight, minTotalGrossWeight, maxTotalNetWeight, maxTotalGrossWeight, noOfSteps, sims)
##' @export
##' 
portRandomizer <- function (weights, minWeight, maxWeight, minTotalNetWeight, minTotalGrossWeight, maxTotalNetWeight, maxTotalGrossWeight, noOfSteps, sims) {

  ## Get the number of assets:
  noAssets <- length(weights)

  ## Get the upper and lower bounds for allowed weights:
  ## TODO: Simplify following statements.
  uBounds <- ifelse(sign(weights) > 0, maxWeight, -minWeight)
  lBounds <- ifelse(sign(weights) > 0, minWeight, -maxWeight)

  ## Create the asset-wise list with upper and lower bounds:
  bounds <- apply(rbind(uBounds, lBounds), MARGIN = 2, function(x) list(x))

  ## Get the asset-wise list with the N-step (as per noOfSteps)
  ## sequence of allowed weights:
  weightSteps <- lapply(bounds, function(x) seq(x[[1]][2], x[[1]][1], length.out=noOfSteps))
  weightSteps <- t(do.call(rbind, weightSteps))

  ## Let's shuffle the index positions of the weight steps
  duplicatedWeightSteps <- rep(list(weightSteps), sims/nrow(weightSteps))
  shuffledWeightSteps   <- do.call(rbind, lapply(duplicatedWeightSteps, function(x) apply(x, MARGIN=2, sample)))

  grossScaledWeights <- shuffledWeightSteps
 
  #targetMidPoint <- ((minTotalNetWeight + maxTotalNetWeight) / 2)
  #targetMidDist  <- targetMidPoint - rowSums(grossScaledWeights)
  adj             <- 0#(targetMidPoint) / 2
  
  uBoundDist <- grossScaledWeights - do.call(rbind, rep(list(uBounds), nrow(grossScaledWeights)))
  shiftedWeights <- ((uBoundDist / rowSums(uBoundDist)) * adj) + grossScaledWeights
  
  grossScale <- rowSums(abs(shiftedWeights)) / maxTotalGrossWeight
  grossScale[which(grossScale < 1)] <- 1

  grossScaledWeights <- shiftedWeights / do.call(cbind, rep(list(grossScale), ncol(shiftedWeights)))

  simWeights <- grossScaledWeights

  ## ## Eliminate duplicates:
  #simWeights <- simWeights[!duplicated(simWeights),]

  ## Net weights:
  netWeights <- rowSums(simWeights)
  grossWeights <- rowSums(abs(simWeights))

  ## Find out which ones are permissable
  permittedIndices <- which(netWeights >= minTotalNetWeight &
                            netWeights <= maxTotalNetWeight &
                            grossWeights >= minTotalGrossWeight &
                            grossWeights <= maxTotalGrossWeight)

  ## Select only permitted ones
  simWeights <- simWeights[permittedIndices,]

  ## Now, return!:
  simWeights
       
}
