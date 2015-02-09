##' Performs a bisection root-finding.
##' 
##' @param func TODO
##' @param lower TODO
##' @param upper TODO
##' @param tol TODO
##' @param maxiter TODO
##' @return The bisection root.
##' @export
##' 
bisectionRoot = function(func, lower, upper, tol=0.000001, maxiter=10000){
    
    if(upper < lower) {
        stop("lower cannot be higher than upper")
    }
    
    if(func(upper)*func(lower) > 0) {
        stop("no root between upper and lower")
    }
    
    for(i in 1:maxiter){        
        mid <- (lower+upper) / 2        

        if(func(mid)==0 |(upper-lower)/2 < tol){

            return(mid)

        } else {

            if(sign(func(mid))==sign(func(lower))){

                lower = mid
                
            } else {
                
                upper = mid
            }
        }        
    }   
}


##' Computes the Johnson SU distribution parameters. 
##' 
##' @param mean TODO
##' @param sd TODO
##' @param skewness TODO
##' @param excess.kurtosis TODO
##' @return The Johnson SU distribution parameters.
##' @export
##' 
johnsonSU = function(mean, sd, skewness, excess.kurtosis){

  mu    <- mean
  sigma <- sd
  tau   <- skewness + 0.01
  psi   <- excess.kurtosis + 0.01
  
  if(sigma < 0) {
      stop("sigma cannot be negative")
  }
  
  if(psi < 0) {
      stop("kurtosis cannot be negative: only leptokurtic distribution")
  }
  
  wUpper <- (-1+(2*(psi+2))^0.5)^0.5
  
  func1  <- function(x) {
      x^4 + 2*x^3 + 3*x^2 - psi - 6
  }

  func2  <- function(x) {
      (x-1)*(x+2)^2 - tau^2
  }
  
  func3 <-  function(x){
      m = (4+2*(x^2-((psi+6)/(x^2+2*x+3))))^0.5-2
      eq = (x-1-m)*(x+2+m/2)^2-tau^2
      return(eq)
  }
  
  w1 <- bisectionRoot(func=func1,lower=.00001,upper=10)
  w2 <- bisectionRoot(func=func2,lower=.00001,upper=10)
  
  wLower <- max(w1,w2)
  
  w <- try(bisectionRoot(func=func3,lower=wLower,upper=wUpper))

  if(class(w)=="try-error"){
      
    Johnson.params = NULL
    print("Johnson parameters numerically not solvable")
    
  } else {
    #kurtosis
    delta <- log(w)^(-0.5)

    m <- (4+2*(w^2-((psi+6)/(w^2+2*w+3))))^0.5-2
    theta = -sign(tau)*asinh(((w+1)*(w-1-m)/(2*w*m))^(0.5))
    #skewness
    gamma = theta*delta

    #scale parameter
    lambda = (2*sigma^2/((w-1)*(w*cosh(2*theta)+1)))^.5

    #location parameter
    chi = mu - sign(tau)*sigma*(w-m-1)^.5/(w-1)

    Johnson.params = cbind(delta,gamma,lambda,chi)
    colnames(Johnson.params)= c("kurtosis","skewness","scale","location")
    Johnson.params = as.data.frame(Johnson.params)
  }
  return(Johnson.params)
}

##' Simulates the distribution based on Johnson SU paramters
##' 
##' @param mean TODO
##' @param sd TODO
##' @param skewness TODO
##' @param excess.kurtosis TODO
##' @param sims TODO
##' @return The Johnson SU distribution parameters.
##' @import randtoolbox
##' @export
##' 
johnsonSim = function(mean, sd, skewness, excess.kurtosis,sims=1000){

    johnson.params <- johnsonSU(mean,sd,skewness,excess.kurtosis)
    
    if(!is.na(johnson.params[1])){
        #print(johnson.params)
        z.norm <- halton(sims, dim = 1, norm = TRUE)
        z.johnson <- johnson.params$scale*sinh((z.norm-johnson.params$skewness)/johnson.params$kurtosis)+johnson.params$location
        return(z.johnson)
    }else{
        return(NULL)
    }
    
}
