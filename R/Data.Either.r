#' Either monad implementation.
#'
#' @export Either
#' @exportClass Either
Either <- R6::R6Class(

    classname = "Either",
    inherit = Monad,

    ## Properties
    private = list(
        .value = NULL,  # value
        .isRight = NULL # is Right?
    ),

    ## Methods
    public = list(
        #' Do not use this function directly, but rather `Rigth::new(..)` and `Left$new()`!
        #'
        #' @param mValue value.
        #'
        initialize = function(mValue = NULL, isRight = TRUE) {
            super$initialize()
            private$.value <- mValue
            private$.isRight <- isRight
            return(self)
        },

        ## Instance implementations of Functor, Applicative, Alternative and Monad
        ## Functor
        fmap = function(fun) {
            if (!base::is.function(fun)) stop("function Either$fmap(..) expects a function as argument")
            if (self$isRight) return(rhaskell::Right(fun(private$.value)))
            else return(self)
        },

        ## Applicative, Alternative cannot not exist as there is no implementation for `empty`!
        pure = function(x) {
            return(rhaskell::Right(x))
        },
        apply = function(x) {
            if (self$isRight && x$isRight) return(rhaskell::Right(private$.value(x$.value)))
            else if (self$isLeft) return(self)
            else return(x)
        },

        ## Monad
        #' Bind function
        #'
        #' @param fun function to apply if it is a Right value
        #'
        #' \code{bind :: (b -> c) -> Either a b (=self) -> Either a c}
        bind = function(fun) {
            if (self$isRight) {
                res <- fun(private$.value)
                if (!"Either" %in% class(res)) stop("Either$fmap: Supplied function must return an Either object!")
                return(res)
            } else return(rhaskell::Left(private$.value))
        },

        #' Unwrap a Left value with default.
        #'
        #' @param def default value
        #'
        #' \code{fromLeft :: a -> Either a b (=self) -> a}
        fromLeft = function(def) {
            if (self$isLeft) return(private$.value)
            else return(def)
        },

        #' Unwrap a Right value with default.
        #'
        #' @param def default value
        #'
        #' \code{fromRight :: b -> Either a b (=self) -> b}
        fromRight = function(def) {
            if (self$isRight) return(private$.value)
            else return(def)
        },

        #' Unwrap an Either from Left or Right with corresponding specified functions.
        #'
        #' @param f function to apply in case of `Left` value
        #' @param g function to apply in case of `Right` value
        #'
        #' \code{either :: (a -> c) -> (b -> c) -> Either a b (=self) -> c}
        either = function(f, g) {
            if (self$isLeft) return(f(private$.value))
            else return(g(private$.value))
        },

        #' Unwrap Right value or call stop() on Left value.
        #'
        #' \code{fromRightOrStop :: Either Character b (=self) -> b}
        fromRightOrStop = function() {
            if (self$isRight) return(private$.value)
            else stop(base::as.string(private$.value))
        }

    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        #' Check if a `Either` is of type `Right`.
        #'
        #' \code{isRight :: Either a b (=self) -> Bool}
        isRight = function() return(private$.isRight),

        #' Check if a `Either` is of type `Left`.
        #'
        #' \code{isLeft :: Either a b (=self) -> Bool}
        isLeft = function() return(!private$.isRight)
    )
)

#' Static pure function.
Either$pure <- function(x) {
    if (base::is.null(x)) stop("NULL value in `Either$pure` is not allowed! Use `Left`")
    return(Either$new(x, isRight = TRUE))
}

#' Create an Either from a nullable variable
Either$fromNullable <- function(leftVal, x) {
    if (base::is.null(x)) return(rhaskell::Left(leftVal))
    else return(rhaskell::Right(x))
}

#' Constructor class for type `Either`.
#'
#' @export Right
Right <- function(value) {
    if (base::is.null(value)) stop("NULL value in `Right` is not allowed! Use `Left` or Maybe::Nothing")
    return(Either$new(value, isRight = TRUE))
}

#' Constructor class for type `Either`.
#'
#' @export Left
Left <- function(value) {
    if (base::is.null(value)) stop("NULL value in `Left` is not allowed! Use `Maybe::Nothing`")
    return(Either$new(value, isRight = FALSE))
}

#' Function either. See also `Either$either`
#'
#' \code{either :: (a -> c) -> (b -> c) -> Either a b -> c}
#'
#' @export either
either <- function(f, g, ei) {
    ei$either(f, g)
}
