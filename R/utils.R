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
