

#' Returns TRUE iff the parameter is an empty list
#'
#' \code{is.Null :: [a] -> Bool}
#'
#' @param xs list
#'
#' @export is.Null
is.Null <- function(xs) return(length(xs) == 0)

#' Returns the first element of a list. Unsafe! I.e. expects a non-empty list, or will throw an error.
#'
#' \code{head :: [a] -> a}
#'
#' @param xs list
#'
#' @export head
head <- function(xs) xs[[1]]

#' Returns the last element of a list. Unsafe! I.e. expects a non-empty list, or will throw an error.
#'
#' \code{last :: [a] -> a}
#'
#' @param xs list
#'
#' @export all
last <- function(xs) xs[[length(xs)]]

#' Returns the tail of the list (all but first element).
#'
#' \code{tail :: [a] -> [a]}
#'
#' @param xs list
#'
#' @export tail
tail <- function(xs) xs[-1]


#' Returns TRUE if any element matches the predicate, otherwise FALSE.
#'
#' \code{all :: (a -> Bool) -> [a] -> Bool}
#'
#' @param pred predicate function
#' @param xs list
#'
#' @export all
any <- function(pred, xs) {
    for (i in seq_len(length(xs)))
        if (pred(xs[[i]])) return(TRUE)
    return(FALSE)
}


#' Returns TRUE iff all elements match the predicate, otherwise FALSE.
#'
#' \code{all :: (a -> Bool) -> [a] -> Bool}
#'
#' @param pred predicate function
#' @param xs list
#'
#' @export all
all <- function(pred, xs) not(any(not %.% pred, xs))


#' Take a number of elements from a list.
#'
#' \code{take :: Int -> [a] -> [a]}
#'
#' @param nr int
#' @param xs list
#'
#' @export take
take <- function(nr, xs) {
    if (nr < 0) {
        stop("Cannot take negative number of elements in a list.")
    } else if (nr == 0) {
        return(list())
    } else {
        return(as.list(xs[c(1:min(nr, length(xs)))]))
    }
}


#' Drop a number of elements from a list.
#'
#' \code{drop :: Int -> [a] -> [a]}
#'
#' @param nr int
#' @param xs list
#'
#' @export drop
drop <- function(nr, xs) {
    if (nr < 0) {
        stop("Cannot drop negative number of elements in a list.")
    } else if (nr >= length(xs)) {
        return(list())
    } else if (nr == 0) {
        return(as.list(xs))
    } else {
        return(as.list(xs[-c(0:min(nr, length(xs)))]))
    }
}


#' Fold over a list applying a function to each element and accumulating the result.
#'
#' \code{foldl :: (b -> a -> b) -> b -> [a] -> b}
#'
#' @param f accumulator function
#' @param acc accumulator
#' @param xs list
#'
#' @export foldl
foldl <- function(f, acc, xs) {
    if (length(xs) == 0) {
        return(acc)
    } else {
        for (i in seq_len(length(xs))) {
            acc <- f(acc, xs[[i]])
        }
        return(acc)
        ## ####################################################################################
        ## This would be the correct (state-less) implementation, but R does not like recusion:
        ## ####################################################################################
        ## newAcc <- f(acc, xs[[1]])
        ## return(foldl(f, newAcc, xs[-1]))
    }
}


#' Zip two lists together by a specific function.
#'
#' \code{zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]}
#'
#' @param f function. A function that should take two arguments, iterating over each element of both
#'     lists.
#' @param xs list. One list to zip.
#' @param ys list. Another list to zip.
#'
#' @export zipWith
zipWith <- function(f, xs, ys) {
    txt <- "In zipWith(f, xs, ys) the length of lists xs and ys are different. Truncating the longer list."
    if (length(xs) == 0 || length(ys) == 0) {
        if (length(xs) != length(ys)) warning(txt)
        return(list())
    } else if (length(xs) == 1 || length(ys) == 1) {
        if (length(xs) != length(ys)) warning(txt)
        return(list(f(xs[[1]], ys[[1]])))
    } else {
        if (length(xs) != length(ys)) warning(txt)
        res <- list()
        for (i in 1:min(length(xs), length(ys)))
            res <- append(res, list(f(xs[[i]], ys[[i]])))
        return(res)
        ## ####################################################################################
        ## This would be the correct (state-less) implementation, but R does not like recusion:
        ## ####################################################################################
        ## x <- xs[[1]]
        ## y <- ys[[1]]
        ## e <- f(x, y)
        ## rest <- zipWith(f, xs[-1], ys[-1])
        ## return(append(rest, list(e), after = 0))
    }
}


#' Zip two list into a list of tuples.
#'
#' \code{zip :: [a] -> [b] -> [(a, b)]}
#'
#' @param xs list. One list to zip.
#' @param ys list. Another list to zip.
#'
#' @export zip
zip <- function(xs, ys) zipWith(sets::tuple, xs, ys)


#' Unzip a list of tuples into a tuple of lists.
#'
#' \code{unzip :: [(a, b)] -> ([a], [b])}
#'
#' @param xs list of tuples
#'
#' @export unzip
unzip <- function(xs) {
    fsts <- list()
    snds <- list()
    for (i in seq_len(length(xs))) {
        fsts[[i]] <- rhaskell::fst(xs[[i]])
        snds[[i]] <- rhaskell::snd(xs[[i]])
    }
    return(sets::tuple(fsts, snds))
}


#' Zip two lists together by a specific function.
#'
#' \code{concat :: [[a]] -> [a]}
#'
#' @param f function. A function that should take two arguments, iterating over each element of both
#'     lists.
#' @param xs list.
#' @param ys list.
#' @param zs list.
#'
#' @export
## zipWith <- function(f, xs, ys) {
##     Map(f, xs, ys)
## }
zipWith3 <- function(f, xs, ys, zs) {
    txt <- "In zipWith3(f, xs, ys, zs) the length of lists xs, ys and zs are different. Truncating the longer list."
    if (length(xs) == 0 || length(ys) == 0 || length(zs) == 0) {
        if (length(xs) != length(ys) || length(xs) != length(zs)) warning(txt)
        return(list())
    } else if (length(xs) == 1 || length(ys) == 1 || length(zs) == 1) {
        if (length(xs) != length(ys) || length(xs) != length(zs)) warning(txt)
        return(list(f(xs[[1]], ys[[1]], zs[[1]])))
    } else {
        if (length(xs) != length(ys) || length(xs) != length(zs)) warning(txt)
        res <- list()
        for (i in 1:min(length(xs), length(ys), length(zs)))
            res <- append(res, list(f(xs[[i]], ys[[i]], zs[[i]])))
        return(res)
        ## ####################################################################################
        ## This would be the correct (state-less) implementation, but R does not like recusion:
        ## ####################################################################################
        ## x <- xs[[1]]
        ## y <- ys[[1]]
        ## z <- zs[[1]]
        ## e <- f(x, y, z)
        ## rest <- zipWith3(f, xs[-1], ys[-1], zs[-1])
        ## return(append(rest, list(e), after = 0))
    }
}


#' Concatenate a list of lists to a list.
#'
#' @param xxs list of lists
#'
#' @export concat
concat <- function(xxs) {
    res <- list()
    for (x in seq_len(length(xxs))) {
        res <- append(res, xxs[[x]])
    }
    ## for(y in 1:length(xxs[[x]]))
    ##     res <- append(res, xxs[[x]])
    return(res)
}


#' Create a list of @nr@ elements of @x@.
#'
#' \code{replicate :: Int -> a -> [a]}
#'
#' @param nr int
#' @param x any type
#'
#' @export replicate
replicate <- function(nr, x) {
    if (nr <= 0) {
        return(list())
    } else if (nr == 1) {
        return(list(x))
    } else {
        res <- list()
        for (i in seq_len(nr))
            res <- append(res, list(x))
        return(res)
        ## return(append(list(x), replicate(nr - 1, x)))
    }
}


#' Split a list into two separate lists wrapped in a tuple. The first list will be until inclusive
#' the provided index, the second list the rest.
#'
#' \code{splitAt :: Int -> [a] -> ([a], [a])}
#'
#' @param idx index
#' @param xs list
#'
#' @export splitAt
splitAt <- function(idx, xs) {
    if (length(xs) == 0)
        return(sets::tuple(list(), list()))
    if (idx < 1)
        return(sets::tuple(list(), as.list(xs)))
    if (idx > length(xs))
        return(sets::tuple(as.list(xs), list()))
    left <- xs[1:idx]
    right <- list()
    if (idx + 1 < length(xs))
        right <- xs[(idx + 1):length(xs)]
    return(sets::tuple(left, right))
}


#' Map with corrected parameter order.
#'
#' \code{map :: (a -> b) -> [a] -> [b]}
#'
#' @param f function to apply
#' @param xs list
#'
#' @export map
map <- function(f, xs) {
    res <- list()
    for (i in seq_len(length(xs))) {
        res <- append(res, f(xs[[i]]))
    }
    return(res)
}
