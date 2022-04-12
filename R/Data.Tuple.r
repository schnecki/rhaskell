

#' Select first component.
#'
#' fst :: (a, b) -> a
#'
#' @export fst
fst <- function(x) {
    return(x[[1]])
}

#' Select second component.
#'
#' snd :: (a, b) -> b
#'
#' @export snd
snd <- function(x) {
    return(x[[2]])
}


#' Function to apply a tuple to a function. Makes a function taking a tuple instead of two
#' parameters.
#'
#' uncurry :: (a -> b -> c) -> ((a, b) -> c)
#'
#' @export uncurry
uncurry <- function(f) {
    return(function(tpl) f(tpl[[1]], tpl[[2]]))
}
