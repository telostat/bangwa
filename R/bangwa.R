##' bangwa.
##'
##' @name bangwa
##' @docType package
NULL


##' Computes simple returns of univariate level series of an asset.
##'
##' @name uvReturns
##' @param levels A numeric vector representing univariate level
##'     series of an asset.
##' @return A numeric vector representing unitvariate simple returns
##'     of an asset.
##'
##' @importFrom Rcpp evalCpp
##' @useDynLib bangwa
##' @export
NULL


##' Computes simple returns of multivariate level series per asset.
##'
##' @name mvReturns
##' @param levels A numeric matrix representing multivariate level
##'     series per column per asset.
##' @return A numeric matrix representing multivariate simple returns
##'     per column per asset.
##'
##' @importFrom Rcpp evalCpp
##' @useDynLib bangwa
##' @export
NULL


##' Simulate Target-Mean Bounded Values
##'
##' Simulates a vector of target-mean bounded values for the given target
##' mean, lower and upper bounds.
##'
##' The length of the simulated values depends on the length of lower
##' and upper bound vectors.
##'
##' @name ttbd
##' @param target The target mean.
##' @param lower A vector of lower bounds.
##' @param upper A vector of upper bounds.
##' @return A vector of simulated values.
##'
##' @importFrom Rcpp evalCpp
##' @useDynLib bangwa
##' @export
NULL
