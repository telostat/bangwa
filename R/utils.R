##' Create a Named List with Class Attribute
##'
##' @param ..klass The class name(s).
##' @param ... Arbitrary named properties.
##' @return Classified object.
##'
##' @export
classify <- function (..klass, ...) {
    ## Get the return value:
    retval <- list(...)

    ## Set the class name(s):
    class(retval) <- ..klass

    ## Return the object:
    retval
}

##' Apply Statement over Argument
##'
##' Applias a statement over an argument.
##'
##' @param statement A callable or function name to be evaluated.
##' @param argument The argument for substitution or to function.
##' @return The result of evaluation the statement over the argument.
.applyPredicate <- function (statement, argument, parent) {
    ## Check the statement:
    if (inherits(statement, "call") | inherits(statement, "{")) {
        ## It is an R expression. We provide the `dot` argument as the
        ## argument and evaluate the statement:
        eval(statement, list("."=argument), parent)
    }
    else {
        ## It is an R function. We apply the function over the
        ## argument:
        eval(statement)(argument)
    }
}

##' Assert a value against a predicate.
##'
##' Checks the value against a predicate, takes one of the four
##' available actions (see \code{type} argument), and return the
##' original value if the procedure is not \code{stop}ped.
##'
##' TODO: Try to optimise this.
##'
##' @param x The input argument.
##' @param predicate The predicate function.
##' @param msg The message to be passed to the function in case the
##'     assertion fails.
##' @param type The action to be performed in case that the assertion
##'     fails.
##' @return The original input argument if the procedure is not
##'     stopped due to an assertion failure.
##'
##' @export
asserta <- function (x, predicate, msg="Error", type=c("stop", "warn", "info", "none")) {
    ## Check the predicate:
    if (!.applyPredicate(substitute(predicate), x, parent.frame())) {
        ## The predicate failed. Check the type and act accordingly:
        switch(match.arg(type),
               ## Stop the procedure:
               stop=stop(msg, call.=FALSE),
               ## Raise a warning and proceed:
               warn=warning(msg, call.=FALSE),
               ## Issue an info
               info=cat(sprintf("Message: %s\n", msg), file=stderr()),
               none=NULL)
    }

    ## OK, return here with the actual argument. This code will be
    ## reached as long as we did not "stop" above.
    x
}

##' Convenience Vector Cast
##'
##' Provides a convenience function to cast, name and NA-treat a given
##' vector compatible R object.
##'
##' @param x The R object.
##' @param names Name vector if any.
##' @param fill The value to replace NAs with.
##' @param type The type to cast to.
##' @return New vector to be returned.
asVector <- function (x, names=NULL, fill=NA, type=c("numeric", "logical", "integer", "complex", "character")) {
    ## Make sure that we have a vector:
    x <- as.vector(x, mode=match.arg(type))

    ## Treat NAs:
    x <- ifelse(is.na(x) | is.nan(x), fill, x)

    ## Attach names if any:
    if (!is.null(names)) {
        ## Assert if we have compatible lenths:
        stopifnot(length(names) == length(x))

        ## Assign names:
        names(x) <- names
    }

    ## Done, return:
    x
}


##' Find Effective Constraints
##'
##' Finds effective portfolio constraints.
##'
##' @param directions Asset directions.
##' @param assetMin Minimum possible absolute asset-wise weights.
##' @param assetMax Maximum possible absolute asset-wise weights.
##' @param minNet The minimum total net weight.
##' @param maxNet The maximum total net weight.
##' @param minGrs The minimum total gross weight.
##' @param maxGrs The maximum total gross weight.
##' @return A list of weights and success status.
##'
##' @examples
##' ## Successful:
##' effectify(rep(1, 10), rep(0, 10), rep(0.5, 10), -1, 1, 0, 2)
##'
##' ## Unsuccessful:
##' effectify(rep(1, 10), rep(0.4, 10), rep(0.5, 10), -1, 1, 0, 2)
##'
##' @import limSolve
##' @export
effectify <- function (directions, assetMin, assetMax, minNet, maxNet, minGrs, maxGrs) {
    ## Compute directed asset mins and maxes, then lower and upper
    ## boundaries:
    dmins <- assetMin * directions
    dmaxs <- assetMax * directions
    lower <- ifelse(dmins < dmaxs, dmins, dmaxs)
    upper <- ifelse(dmins > dmaxs, dmins, dmaxs)

    ## Compute lond/short indices:
    idxL <- which(directions > 0)
    idxS <- which(directions < 0)

    ## Compute minimum/maximum possible total long weights:
    minL <- sum(lower[idxL])
    maxL <- sum(upper[idxL])

    ## Compute minimum/maximum possible total short weights:
    minS <- sum(lower[idxS])
    maxS <- sum(upper[idxS])

    ## Create a matrix for coefficients of the equality
    ## constraints. Note that this will refer to respectively:
    ##
    ##
    ## minNet' == max(minNet,     minS  + minL)
    ## maxNet' == min(maxNet,     maxS  + maxL)
    ## minGrs' == max(minGrs, abs(maxS) + minL)
    ## maxGrs' == min(maxGrs, abs(minS) + maxL)
    E <- diag(1, 4, 4)

    ## Define the right-hand side of the equality constraints. This
    ## will follow the above defined equalities.
    F <- c(max(minNet,     minS  + minL),
           min(maxNet,     maxS  + maxL),
           max(minGrs, abs(maxS) + minL),
           min(maxGrs, abs(minS) + maxL))

    ## Now, we will define the inequalities in the same way
    ## above. This requires a little bit of care. Let's do step by
    ## step. Note that we need the `gte` inequality type for all
    ## inequalities:
    ##
    ## .minNet >=  minNet  -->   .minNet           >=  minNet  -->  c( 1,  0,  0,  0)  -->  minNet
    ## .maxNet <=  maxNet  -->  -.maxNet           >= -maxNet  -->  c( 0, -1,  0,  0)  --> -maxNet
    ## .minNet <= .maxNet  -->  -.minNet + .maxNet >=  0       -->  c(-1,  1,  0,  0)  -->  0
    ## .minGrs >=  0       -->   .minGrs           >=  0       -->  c( 0,  0,  1,  0)  -->  0
    ## .maxGrs >=  0       -->   .maxGrs           >=  0       -->  c( 0,  0,  0,  1)  -->  0
    ## .minGrs >=  minGrs  -->   .minGrs           >=  minGrs  -->  c( 0,  0,  1,  0)  -->  minGrs
    ## .maxGrs <=  maxGrs  -->  -.maxGrs           >= -maxGrs  -->  c( 0,  0,  0, -1)  --> -maxGrs
    ## .minGrs <= .maxGrs  -->  -.minGrs + .maxGrs >=  0       -->  c( 0,  0, -1,  1)  -->  0
    ## .minNet <= .minGrs  -->  -.minNet + .minGrs >=  0       -->  c(-1,  0,  1,  0)  -->  0
    ## .maxNet <= .maxGrs  -->  -.maxNet + .maxGrs >=  0       -->  c( 0, -1,  0,  1)  -->  0
    ##
    ## Now we can construct the respective matrix and vector. First
    ## start with the coefficients of the inequality constraints:
    G <- matrix(c( 1,  0,  0,  0,
                   0, -1,  0,  0,
                  -1,  1,  0,  0,
                   0,  0,  1,  0,
                   0,  0,  0,  1,
                   0,  0,  1,  0,
                   0,  0,  0, -1,
                   0,  0, -1,  1,
                  -1,  0,  1,  0,
                   0, -1,  0,  1), byrow=TRUE, ncol=4)

    ## Define the right-hand side of the inequality constraints:
    H <- c(minNet, -maxNet, 0, 0, 0, minGrs, -maxGrs, 0, 0, 0)

    ## Run the solver:
    result <- try(limSolve::lsei(E=E, F=F, G=G, H=H, type=2))

    ## Compute the result and return:
    if (inherits(result, "try-error")) {
        list(minNet=minNet, maxNet=maxNet, minGrs=minGrs, maxGrs=maxGrs, status=FALSE)
    }
    else {
        list(minNet=result$X[1], maxNet=result$X[2], minGrs=result$X[3], maxGrs=result$X[4], status=TRUE)
    }
}
