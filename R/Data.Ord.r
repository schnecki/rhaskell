

#' Partially applied equality, returns a function, where the first parameter of the equality is set.
#'
#' \code{pEq :: a -> b -> Bool}
#'
#' E.g. pEq(3)(4) = FALSE
#' E.g. pEq(3)(3) = TRUE
#'
#' @param x first parameter value in x == y
#'
#' @export pEq
pEq <- function(x) {
    return(function(y) {
        return(x == y)
    })
}


#' Partially applied not-equal function.
#'
#' \code{pNeq :: a -> b -> Bool}
#'
#' @param x first parameter value
#'
#' @export pNeq
pNeq <- function(x) {
    return(rhaskell::not %.% rhaskell::pEq(x))
}


#' Partially applied greater or equal, returns a function with >= x, where x is the first parameter.
#'
#' \code{pGeq :: a -> b -> Bool}
#'
#' E.g. pGeq(3)(4) = TRUE
#' E.g. pGeq(3)(2) = FALSE
#'
#' @param x parameter x in y >= x
#'
#' @export pGeq
pGeq <- function(x) {
    return(function(y) {
        return(y >= x)
    })
}


#' Partially applied less or equal, returns a function with <= x, where x is the first parameter.
#'
#' \code{pLeq :: a -> b -> Bool}
#'
#' E.g. pLeq(3)(4) = FALSE
#' E.g. pLeq(3)(2) = TRUE
#'
#' @param x parameter x in y <= x
#'
#' @export pLeq
pLeq <- function(x) {
    return(function(y) {
        return(y <= x)
    })
}


#' Partially applied greater, returns a function with > x, where x is the first parameter.
#'
#' \code{pGt :: a -> b -> Bool}
#'
#' E.g. pGt(3)(4) = TRUE
#' E.g. pGt(3)(2) = FALSE
#'
#' @param x parameter x in y > x
#'
#' @export pGt
pGt <- function(x) {
    return(function(y) {
        return(y > x)
    })
}


#' Partially applied less, returns a function with < x, where x is the first parameter.
#'
#' \code{pLt :: a -> b -> Bool}
#'
#' E.g. pLt(3)(4) = FALSE
#' E.g. pLt(3)(2) = TRUE
#'
#' @param x parameter x in y < x
#'
#' @export pLt
pLt <- function(x) {
    return(function(y) {
        return(y < x)
    })
}
