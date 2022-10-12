#' Maybe monad implementation.
#'
#' @export Maybe
#' @exportClass Maybe
Maybe <- R6::R6Class(

    classname = "Maybe",
    inherit = Monad,

    ## Properties
    private = list(
        .value = NULL
    ),

    ## Methods
    public = list(
        #' Do not use this function directly, but rather `Just::new(..)` and `Nothing$new()`!
        #'
        #' @param mValue value.
        #'
        initialize = function(mValue = NULL) {
            super$initialize()
            private$.value <- mValue
            return(self)
        },

        ## Instance implementations of Functor, Applicative, Alternative and Monad
        ## Functor
        fmap = function(fun) {
            if (!base::is.function(fun)) stop("function Maybe$fmap(..) expects a function as argument")
            if (self$isJust) res <- rhaskell::Just(fun(self$fromJust()))
            else res <- rhaskell::Nothing()
            return(res)
        },

        ## Applicative and Alternative
        pure = function(x) {
            return(rhaskell::Just(x))
        },
        apply = function(x) {
            if (self$isJust && x$isJust) return(rhaskell::Just(private$.value(x$fromJust())))
            else return(rhaskell::Nothing())
        },
        empty = function() return(rhaskell::Nothing()),
        alt = function(other) {
            if (self$isJust) return(self)
            else return(other)
        },


        ## Monad
        bind = function(fun) {
            if (self$isJust) return(fun(self$fromJust()))
            else return(rhaskell::Nothing())
        },

        #' Return the value. Fails if the encapsulated value is NULL! Better use `fromMaybe`!
        #'
        #' \code{isJust :: Maybe a (=self) -> a}
        fromJust = function() {
            if (self$isJust) return(private$.value)
            else stop("Data.Maybe.fromJust: Function `fromJust` applied to Nothing (empty Maybe)!")
        },

        #' Unwrap the value with a default.
        #'
        #' @param def default value
        #'
        #' \code{fromMaybe :: a -> Maybe a (=self) -> a}
        fromMaybe = function(def) {
            if (self$isJust) return(private$.value)
            else return(def)
        },

        #' Unwrap a maybe with default value and a function to apply.
        #'
        #' @param def default value
        #' @param f function to apply in case of `Just()` value
        #'
        #' \code{maybe :: a -> (a -> b) -> Maybe a (=self) -> a}
        maybe = function(def, f) {
            if (self$isJust) return(f(self$fromJust()))
            else return(def)
        }

    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        #' Check if a `Maybe` is of type `Just`.
        #'
        #' \code{isJust :: Maybe a (=self) -> Bool}
        isJust = function() return(!(base::is.null(private$.value))),

        #' Check if a `Maybe` is of type `Nothing`.
        #'
        #' \code{isJust :: Maybe a (=self) -> Bool}
        isNothing = function() return(base::is.null(private$.value))
    )
)

#' Static pure function.
Maybe$pure <- function(x) {
    if (base::is.null(x)) stop("NULL value in `Maybe$pure` is not allowed! Use `Nothing`")
    return(Maybe$new(x))
}

#' Create a maybe from a nullable variable
Maybe$fromNullable <- function(x) {
    if (base::is.null(x)) return(rhaskell::Nothing())
    else return(rhaskell::Just(x))
}

#' Constructor class for type `Maybe`.
#'
#' @export Just
Just <- function(value) {
    if (base::is.null(value)) stop("NULL value in `Just` is not allowed! Use `Nothing`")
    return(Maybe$new(value))
}

#' Constructor class for type `Maybe`.
#'
#' @export Nothing
Nothing <- function() {
    return(Maybe$new(NULL))
}

#' Function maybe. See also `Maybe$maybe`
#'
#' \code{maybe :: b -> (a -> b) -> Maybe a -> b}
#'
#' @export maybe
maybe <- function(def, f, ma) {
    ma$maybe(def, f)
}
