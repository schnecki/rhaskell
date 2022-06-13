

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


#' Partially applied @identical@, returns a function, where the first parameter is set.
#'
#' \code{pIdentical :: a -> b -> Bool}
#'
#' @param x first parameter value in identical(x, y)
#'
#' @export pIdentical
pIdentical <- function(x) {
    return(function(y) {
        return(base::identical(x, y))
    })
}


#' Equality.
#'
#' \code{eq :: a -> b -> Bool}
#'
#'
#' @param x first parameter value in x == y
#' @param y second parameter value in x == y
#'
#' @export eq
eq <- function(x, y) return(x == y)


#' Partially applied not-equal function.
#'
#' \code{pNeq :: a -> b -> Bool}
#'
#' @param x first parameter value
#'
#' @export pNeq
pNeq <- function(x) return(rhaskell::comp(rhaskell::not, rhaskell::pEq(x)))

#' Partially applied @not %.% identical@, returns a function, where the first parameter is set.
#'
#' \code{pIdentical :: a -> b -> Bool}
#'
#' @param x first parameter value in @!identical(x, y)@
#'
#' @export pNIdentical
pNIdentical <- function(x) {
    return(function(y) {
        return(!base::identical(x, y))
    })
}

#' Not-equal function.
#'
#' \code{neq :: a -> b -> Bool}
#'
#' @param x first parameter value
#' @param y second parameter value
#'
#' @export neq
neq <- function(x, y) return(x != y)


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


#' Greater or equal.
#'
#' \code{geq :: a -> b -> Bool}
#'
#' @param x parameter x in y >= x
#' @param y parameter x in y >= x
#'
#' @export geq
geq <- function(x, y) return(y >= x)


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

#' Less or equal.
#'
#' \code{leq :: a -> b -> Bool}
#'
#' @param x parameter x in x <= y
#' @param y parameter y in x <= y
#'
#' @export leq
leq <- function(x, y) return(x <= y)


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


#' Greater than.
#'
#' \code{gt :: a -> b -> Bool}
#'
#' @param x parameter x in x > y
#' @param y parameter x in x > y
#'
#' @export gt
gt <- function(x, y) return(x > y)


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


#' Less than.
#'
#' \code{lt :: a -> b -> Bool}
#'
#' @param x parameter x in x < y
#' @param y parameter x in x < y
#'
#' @export lt
lt <- function(x, y) return(x < y)
