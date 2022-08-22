#' Functor class.
#'
#' @export Functor
#' @exportClass Functor
Functor <- R6::R6Class(

    classname = "Functor",

    ## Properties
    private = list(
    ),

    ## Methods
    public = list(
        initialize = function() {},

        #' Functoric bind
        #'
        #' \code{pure :: (a -> b) -> f a (=self) -> f b}
        fmap = function(fun) {
            stop("pure must be overwritten by the implementing class")
        }

    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )


)
