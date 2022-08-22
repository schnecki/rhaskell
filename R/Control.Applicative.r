#' Applicative class.
#'
#' @export Applicative
#' @exportClass Applicative
Applicative <- R6::R6Class(

    classname = "Applicative",
    inherit = Functor,

    ## Properties
    private = list(
    ),

    ## Methods
    public = list(
        initialize = function() {},

        #' Applicativeic bind. Also add a static method `pure` to the class!
        #'
        #' \code{pure :: a -> f a }
        pure = function(x) {
            stop("pure must be overwritten by the implementing class and additionally be implemented static!")
        },

        #' Sequential application, i.e. (<*>)
        #'
        #' \code{apply :: f (a -> b) (=self) -> f a -> f b}
        apply = function(x) {
            stop("apply must be overwritten by the implementing class")
        }

    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )
)


Applicative$pure <- function(x) {

}
