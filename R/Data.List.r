

#' Returns TRUE iff the parameter is an empty list
#'
#' \code{null :: [a] -> Bool}
#'
#' @param xs list
#'
#' @export null
null <- function(xs) return(length(xs) == 0)

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
#' @export any
any <- function(pred, xs, skipNAs = TRUE) {
    for (i in seq_len(length(xs))) {
        if (base::is.atomic(xs[[i]]) && skipNAs && base::is.na(xs[[i]])) next
        if (pred(xs[[i]])) return(TRUE)
    }
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
all <- function(pred, xs, skipNAs = TRUE) not(any(not %.% pred, xs))


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
    } else {
        if (length(xs) != length(ys)) warning(txt)
        len <- min(length(xs), length(ys))
        res <- base::vector("list", len)
        for (i in 1:len)
            res[[i]] <- f(xs[[i]], ys[[i]])
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
    len <- length(xs)
    fsts <- base::vector("list", len)
    snds <- base::vector("list", len)
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
    } else {
        if (length(xs) != length(ys) || length(xs) != length(zs)) warning(txt)
        len <- min(length(xs), length(ys), min(length(zs)))
        res <- base::vector("list", len)
        for (i in 1:len)
            res[[i]] <- f(xs[[i]], ys[[i]], zs[[i]])
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
    res <- base::vector("list", sum(unlist(rhaskell::map(length, xxs))))
    i <- 1
    for (j in seq_len(length(xxs))) {
        for (k in seq_len(length(xxs[[j]]))) {
            res[[i]] <- xxs[[j]][[k]]
            i <- i + 1
        }
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
        res <- base::vector("list", nr)
        for (i in seq_len(nr))
            res[[i]] <- x
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
    if (idx >= length(xs))
        return(sets::tuple(as.list(xs), list()))
    left <- xs[1:idx]
    right <- xs[(idx + 1):length(xs)]
    return(sets::tuple(left, right))
}


#' Map with corrected parameter order. You should only use pure functions! Use @mapM_@ in case of
#' impure functions.
#'
#' \code{map :: (a -> b) -> [a] -> [b]}
#'
#' @param f function to apply
#' @param xs list
#'
#' @export map
map <- function(f, xs) {
    res <- base::vector("list", length(xs))
    for (i in seq_len(length(xs))) {
        res[[i]] <- f(xs[[i]])
    }
    return(res)
}

#' This is @map@, but with indication that IO is happening. I.e. the program state can change and a
#' reader must have a closer look what the function does.
#'
#' \code{mapM :: (a -> IO b) -> [a] -> IO [b]}
#'
#' @param f function to apply
#' @param xs list
#'
#' @export mapM
mapM <- function(f, xs) map(f, xs)


#' This is @map@, but with indication that IO is happening. I.e. the program state can change and a
#' reader must have a closer look what the function does.
#'
#' \code{mapM_ :: (a -> IO b) -> [a] -> IO ()}
#'
#' @param f function to apply
#' @param xs list
#'
#' @export mapM_
mapM_ <- function(f, xs) void(map(f, xs))


#' concatMap is `concat` `%.%` `map`. Here the list can be used as monoid, neglecting some values,
#' while concat collects the accepted values.
#'
#' \code{concatMap :: (a -> [b]) -> [a] -> [b]}
#'
#' @param f function to apply
#' @param xs list
#'
#' @export concatMap
concatMap <- function(f, xs) {
    return(rhaskell::concat(rhaskell::map(f, xs)))
}


#' Filters a list by predicate function.
#'
#' \code{filter :: (a -> Bool) -> [a] -> [a]}
#'
#' @param f predicate function
#' @param xs list
#'
#' @export filter
filter <- function(f, xs) {
    res <- base::vector("list", length(xs))
    resLen <- 1
    for (i in seq_len(length(xs))) {
        if (f(xs[[i]])) {
            res[[resLen]] <- xs[[i]]
            resLen <- resLen + 1
        }
    }
    if (resLen == 1) return(list())
    return(res[1 : (resLen - 1)])
}


#' O(n). The intersperse function takes an element and a list and `intersperses' that element
#' between the elements of the list.
#'
#' \code{filter :: a -> [a] -> [a]}
#'
#' @param a  element
#' @param xs list of elements
#'
#' @export intersperse
intersperse <- function(x, xs) {
    res <- base::vector("list", max(0, 2 * length(xs) - 1))
    for (i in seq_len(length(xs))) {
        res[[2 * i - 1]] <- xs[[i]]
        if (i != length(xs))
            res[[2 * i]] <- x
    }
    return(res)
}


#' 'intercalate' @xs xss@ is equivalent to @('concat' ('intersperse' xs xss))@. It inserts the list
#' @xs@ in between the lists in @xss@ and concatenates the result.
#'
#' \code{filter :: a -> [a] -> [a]}
#'
#' @param xs  element
#' @param xss list of elements
#'
#' @export intercalate
intercalate <- function(xs, xss) concat(intersperse(xs, xss))


#' @delete x xs@ removes the first occurance of x in list xs.
#'
#' delete :: Eq a => a -> [a] -> [a]
#'
#' @param x element
#' @param xs list of elements
#'
#' @export delete
delete <- function(x, xs) {
    if (length(xs) < 1) return(list())
    res <- base::vector("list", length(xs) - 1)
    resLen <- 1
    deleted <- FALSE
    for (i in seq_len(length(xs))) {
        if (!deleted && base::identical(x, xs[[i]])) {
            deleted <- TRUE
        } else if (resLen <= length(res)) {
            res[[resLen]] <- xs[[i]]
            resLen <- resLen + 1
        }
    }
    if (deleted)
        return(res)
    else
        return(xs)
}
