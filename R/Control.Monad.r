#' Monad class.
#'
#' @export Monad
#' @exportClass Monad
Monad <- R6::R6Class(

    classname = "Monad",
    inherit = Applicative,

    ## Properties
    private = list(
    ),

    ## Methods
    public = list(
        initialize = function() {},

        #' Monadic bind
        #'
        #' \code{bind :: m a (=self) -> (a -> m b) -> m b}
        bind = function(fun) {
            stop("bind must be overwritten by the implementing class")
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )


)
