

test_that("Right", {
    expect_equal(Right(1)$fromRight(2), 1)
})

test_that("Left", {
    expect_equal(Left(1)$fromLeft(2), 1)
})


test_that("Either Functor", {
    expect_equal(Right(1)$fmap(pAdd(3)), Right(4))
    expect_equal(Left("xyz")$fmap(pAdd(3)), Left("xyz"))
})

test_that("Either Applicative", {
    expect_equal(Either$pure(3), Right(3))
    expect_equal(Left(2)$pure(3), Right(3))
    expect_equal(Right(2)$fmap(pAdd)$apply(Right(3)), Right(5))
    expect_equal(Left("err")$fmap(pAdd)$apply(Right(3)), Left("err"))
    expect_equal(Right(2)$fmap(pAdd)$apply(Left(2)), Left(2))
})


test_that("Either Monad", {
    expect_equal(Right(1)$bind(function(x) Right(x + 1)), Right(2))
    expect_equal(Right(1)$bind(function(x) Left("asd")), Left("asd"))
    expect_equal(Left(1)$bind(function(x) Right(x + 1)), Left(1))
})
