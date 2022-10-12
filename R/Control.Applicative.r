#' Applicative class. This also defines the interface for `Alternative`.
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

        #' Applicativeic bind. Also add a static method `pure` to the class!
        #'
        #' @param x element
        #'
        #' \code{pure :: a -> f a }
        pure = function(x) {
            stop("pure must be overwritten by the implementing class and additionally be implemented static!")
        },

        #' Sequential application, i.e. (<*>)
        #'
        #' @param x Applicative element
        #'
        #' \code{apply :: f (a -> b) (=self) -> f a -> f b}
        apply = function(x) {
            stop("apply must be overwritten by the implementing class")
        },

        ## ALTERANTIVE
        ##
        ## Alternative class (Info: As multiple inheritance is not allowed, we define it here)

        #' Identity of `alt`
        #'
        #' \code{empty :: f a}
        empty = function() {
            stop("Alternative instance not defined. See Applicative.r")
        },

        #' An associative binary operation
        #'
        #' \code{alt :: f a (=self) -> f a -> f a}
        alt = function(other) {
            stop("Alternative instance not defined. See Applicative.r")
        }


    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )
)
