#' Partially applied equality, returns a function, where the first parameter of the equality is set.
#'
#' E.g. pEq(3)(4) = FALSE
#' E.g. pEq(3)(3) = TRUE
#'
#' @export pEq
pEq <- function(x) {
    return(function(y) {
        return(x == y)
    })
}


#' Partially applied not-equal function.
#'
#' @export pNeq
pNeq <- function(x) {
    return(not %.% pEq(x))
}


#' Partially applied greater or equal, returns a function with >= x, where x is the first parameter.
#'
#' E.g. pGeq(3)(4) = TRUE
#' E.g. pGeq(3)(2) = FALSE
#'
#' @export pGeq
pGeq <- function(x) {
    return(function(y) {
        return(y >= x)
    })
}


#' Partially applied less or equal, returns a function with <= x, where x is the first parameter.
#'
#' E.g. pLeq(3)(4) = FALSE
#' E.g. pLeq(3)(2) = TRUE
#'
#' @export pLeq
pLeq <- function(x) {
    return(function(y) {
        return(y <= x)
    })
}


#' Partially applied greater, returns a function with > x, where x is the first parameter.
#'
#' E.g. pGt(3)(4) = TRUE
#' E.g. pGt(3)(2) = FALSE
#'
#' @export pGt
pGt <- function(x) {
    return(function(y) {
        return(y > x)
    })
}


#' Partially applied less, returns a function with < x, where x is the first parameter.
#'
#' E.g. pLt(3)(4) = FALSE
#' E.g. pLt(3)(2) = TRUE
#'
#' @export pLt
pLt <- function(x) {
    return(function(y) {
        return(y < x)
    })
}
