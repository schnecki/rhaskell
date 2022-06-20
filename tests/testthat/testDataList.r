

test_that("null", {
    expect_equal(cons(1, list()), list(1))
    expect_equal(cons(1, list(2)), list(1, 2))
})


test_that("null", {
    expect_true(null(list()))
    expect_false(null(list(1)))
})

test_that("head", {
    expect_equal(head(list(2, 3, 4)), 2)
    expect_error(head(list()))
})

test_that("tail", {
    expect_equal(tail(list(2, 3, 4)), list(3, 4))
    expect_equal(tail(list()), list())
})

test_that("last", {
    expect_equal(last(list(2, 3, 4)), 4)
    expect_error(last(list()))
})


test_that("all", {
    expect_true(all(pEq(3), list(3, 3, 3, 3, 3)))
    expect_false(all(pEq(3), list(3, 3, 3, 4, 3)))
})


test_that("any", {
    expect_false(any(pGt(3), list(3, 3, 3, 3, 3, 2)))
    expect_true(any(pGt(3), list(3, 3, 3, 3, 3, 4)))
    expect_false(any(pEq(3), list(2, 2, 2, 4, 2)))
})


test_that("take", {
    expect_equal(take(0, 1:10), list())
    expect_error(take(-1, 1:10))
    expect_equal(take(1, 1:10), list(1))
    expect_equal(take(2, 1:10), list(1, 2))
})


test_that("drop", {
    expect_equal(drop(0, 1:10), as.list(1:10))
    expect_error(drop(-1, 1:10))
    expect_equal(drop(1, 1:10), as.list(2:10))
    expect_equal(drop(2, 1:10), as.list(3:10))
})


test_that("foldl", {
    expect_equal(foldl(function(acc, x) acc + x, 0, as.list(1:10)), 55)
    expect_equal(foldl(function(acc, x) pAdd(acc)(x), 0, as.list(1:10)), 55)
    expect_equal(foldl(pApply(pAdd), 0, as.list(1:10)), 55)
    expect_equal(foldl(add, 0, as.list(1:10)), 55)
    expect_equal(foldl(subtract, 55, as.list(1:10)), 0)
    expect_equal(foldl(function(acc, x) pSubtract(x)(acc), 55, as.list(1:10)), 0)
    expect_equal(foldl(flip(pApply(pSubtract)), 55, as.list(1:10)), 0)
})


test_that("zipWith", {
    expect_equal(zipWith(add, 1:10, 1:10), purrr::map(1:10, function(x) 2 * x))
    expect_warning(zipWith(add, 1:10, 1:2))
})

test_that("zipWith3", {
    add3 <- function(x, y, z) x + y + z
    expect_equal(zipWith3(add3, 1:10, 1:10, 1:10), purrr::map(1:10, function(x) 3 * x))
    expect_warning(zipWith3(add3, 1:10, 1:2, 1:10))
})


test_that("zip", {
    expect_equal(zip(1:10, 1:10), purrr::map(1:10, function(x) sets::tuple(x, x)))
})


test_that("unzip", {
    expect_equal(unzip(zip(1:10, 11:20))[[1]], as.list(1:10))
    expect_equal(unzip(zip(1:10, 11:20))[[2]], as.list(11:20))
})


test_that("concat", {
    expect_equal(concat(list()), list())
    expect_equal(concat(list(list())), list())
    expect_equal(concat(list(list(1, 2), list(3, 4))), as.list(1:4))
})


test_that("replicate", {
    expect_equal(replicate(0, 1), list())
    expect_equal(replicate(1, 1), list(1))
    expect_equal(replicate(2, 1), list(1, 1))

})


test_that("splitAt", {
    expect_equal(splitAt(1, list()), sets::tuple(list(), list()))
    expect_equal(splitAt(1, as.list(1:10)), sets::tuple(as.list(1), as.list(2:10)))
    expect_equal(splitAt(10, as.list(1:10)), sets::tuple(as.list(1:10), list()))
    expect_equal(splitAt(11, as.list(1:10)), sets::tuple(as.list(1:10), list()))
})

test_that("map", {
    expect_equal(map(pAdd(1), as.list(1:10)), as.list(2:11))
    expect_equal(map(pAdd(1), 1:10), as.list(2:11))
    expect_equal(map(pAdd(1), list()), list())
})

test_that("filter", {
    expect_equal(filter(pLt(1), as.list(1:10)), list())
    expect_equal(filter(pLeq(1), as.list(1:10)), list(1))
    expect_equal(filter(pLt(10), 1:10), as.list(1:9))
    expect_equal(filter(pLeq(10), 1:10), as.list(1:10))
})

test_that("concatMap", {
    sel <- function(x) if (x < 3) list(x) else list()
    expect_equal(concatMap(sel, as.list(1:10)), as.list(1:2))
    expect_equal(concatMap(sel, 1:10), as.list(1:2))
    expect_equal(concatMap(sel, list()), list())
})

test_that("intersperse", {
    expect_equal(intersperse(0, as.list(1:3)), as.list(c(1, 0, 2, 0, 3)))
    expect_equal(intersperse(0, list()), list())
})


test_that("intercalate", {
    expect_equal(intercalate(list(0), map(list, as.list(1:3))), as.list(c(1, 0, 2, 0, 3)))
    expect_equal(intercalate(list(0), list()), list())
})


test_that("delete", {
    expect_equal(delete(3, list()), list())
    expect_equal(delete(3, as.list(c(3))), list())
    expect_equal(delete(5, as.list(c(3))), list(c(3)))
    expect_equal(delete(3, as.list(c(1, 2, 3, 4))), as.list(c(1, 2, 4)))
    expect_equal(delete(0, as.list(c(1, 2, 3, 4))), as.list(c(1, 2, 3, 4)))
    expect_equal(delete(3, as.list(c(1, 2, 3, 3, 4))), as.list(c(1, 2, 3, 4)))
})
