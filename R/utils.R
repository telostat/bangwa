##' Decorate an object with class
##'
##' Appends a new class name to an object's class vector and returns.
##'
##' @param x Object to be classified.
##' @param klass The class name.
##' @return Classified object.
##'
##' @export
classify <- function (x, klass) {
    ## Set the class name:
    class(x) <- append(klass, class(x))

    ## Return the object:
    x
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
