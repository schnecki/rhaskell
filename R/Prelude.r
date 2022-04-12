

#' Ignore the output of a function. Wrapper for @invisible@
#'
#' @export void
void <- function(f) {
    invisible(f)
}

#' Standard math. function composition: (f %.% g)(x) == f(g(x))
#'
#' @export %.%
`%.%` <- function(f, g) {
    return(function(...) f(g(...)))
}


#' not :: Bool -> Bool
#'
#' @export not
not <- function(x) {
    return(!x)
}


#' Partially applied addition.
#'
#' pAdd :: Num a => a -> (a -> a)
#'
#' @export pAdd
pAdd <- function(x) {
    return(function(y) {
        return(x + y)
    })
}


#' Addition.
#'
#' add :: Num a => a -> a -> a
#'
#' @export add
add <- function(x, y) {
    return(x + y)
}


#' Creates a function which subtracts the given value.
#'
#' subtract :: Num a => a -> (a -> a)
#'
#' subtract(4)(3) = -1
#'
#' @export subtract
pSubtract <- function(x) {
    return(function(y) {
        return(y - x)
    })
}


#' Subtraction.
#'
#' subtract :: Num a => a -> a -> a
#'
#' subtract(3, 4) = -1
#'
#' @export subtract
subtract <- function(x, y) {
    return(x - y)
}


#' Apply partial function.
#'
#' pApply :: (a -> (b -> c)) -> (a -> b -> c)
#'
#' @export pApply
pApply <- function(f) {
    return(function(x, y) f(x)(y))
}


#' Flip the parameters of a function
#'
#' flip :: (a -> b -> c) -> (b -> a -> c)
#'
#' @export flip
flip <- function(f) {
    return(function(x, y) f(y, x))
}
