

#' Select first component.
#'
#' \code{fst :: (a, b) -> a}
#'
#' @param x tuple
#'
#' @export fst
fst <- function(x) {
    return(x[[1]])
}

#' Select second component.
#'
#' \code{snd :: (a, b) -> b}
#'
#' @param x tuple
#'
#' @export snd
snd <- function(x) {
    return(x[[2]])
}


#' Function to apply a tuple to a function. Makes a function taking a tuple instead of two
#' parameters.
#'
#' \code{uncurry :: (a -> b -> c) -> ((a, b) -> c)}
#'
#' @param f function taking 2 parameters
#'
#' @export uncurry
uncurry <- function(f) {
    return(function(tpl) f(tpl[[1]], tpl[[2]]))
}
