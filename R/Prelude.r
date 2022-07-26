

#' Ignore the output of a function. Wrapper for @invisible@
#'
#' \code{void :: (a -> b) -> ()}
#'
#' @param f function
#'
#' @export void
void <- function(f) {
    invisible(f)
}

#' Standard math. function composition: \code{(f %.% g)(x) == f(g(x))}
#'
#' \code{\%.\% :: (b -> c) -> (a -> b) -> (a -> c)}
#'
#' @param f function
#' @param g function
#'
#' @export %.%
`%.%` <- function(f, g) {
    return(function(...) f(g(...)))
}

#' Prefix version of \%comp\%.
#'
#' \code{comp :: (b -> c) -> (a -> b) -> (a -> c)}
#'
#' @param f function
#' @param g function
#'
#' @export comp
comp <- function(f, g) {
    return(function(...) f(g(...)))
}


#' Is element of list.
#'
#' \code{\%elem\% :: a -> [a] -> Bool}
#'
#' @param a   element to check
#' @param [a] list
#'
#' @export %elem%
`%elem%` <- function(x, xs) {
    return(x %in% xs)
}

#' Is not element of list.
#'
#' \code{\%notElem\% :: a -> [a] -> Bool}
#'
#' @param a   element to check
#' @param [a] list
#'
#' @export %notElem%
`%notElem%` <- function(x, xs) {
    return(!(x %in% xs))
}


#' Collapse a list of string
#'
#' \code{\%unlines\% :: [String] -> String}
#'
#' @param [a] list of strings
#'
#' @export unlines
unlines <- function(xs) {
    return(paste(c(xs), collapse = ""))
}


#' Sequencing of functions
#'
#' \code{\%seq\% :: a -> b -> b}
#'
#' @param f Function
#' @param g Function
#'
#' @export %seq%
`%seq%` <- function(f, g) {
    x <- f
    return(g)
}


#' Inverse for Boolean.
#'
#' \code{not :: Bool -> Bool}
#'
#' @param x Bool
#'
#' @export not
not <- function(x) {
    return(!x)
}


#' Partially applied addition.
#'
#' \code{pAdd :: Num a => a -> (a -> a)}
#'
#' @param x Numeric value
#'
#' @export pAdd
pAdd <- function(x) {
    return(function(y) {
        return(x + y)
    })
}


#' Addition.
#'
#' \code{add :: Num a => a -> a -> a}
#'
#' @param x Numeric value
#' @param y Numeric value
#'
#' @export add
add <- function(x, y) {
    return(x + y)
}


#' Creates a function which subtracts the given value.
#'
#' \code{subtract :: Num a => a -> (a -> a)}
#'
#' subtract(4)(3) = -1
#'
#' @param x Numeric value
#'
#' @export subtract
pSubtract <- function(x) {
    return(function(y) {
        return(y - x)
    })
}


#' Subtraction.
#'
#' \code{subtract :: Num a => a -> a -> a}
#'
#' subtract(3, 4) = -1
#'
#' @param x Numeric value
#' @param y Numeric value
#'
#' @export subtract
subtract <- function(x, y) {
    return(x - y)
}


#' Apply partial function.
#'
#' \code{pApply :: (a -> (b -> c)) -> (a -> b -> c)}
#'
#' @param f function
#'
#' @export pApply
pApply <- function(f) {
    return(function(x, y) f(x)(y))
}


#' Flip the parameters of a function
#'
#' \code{flip :: (a -> b -> c) -> (b -> a -> c)}
#'
#' @param f function
#'
#' @export flip
flip <- function(f) {
    return(function(x, y) f(y, x))
}
