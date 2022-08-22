

test_that("Just", {
    expect_equal(Just(1)$fromJust(), 1)
})


test_that("Nothing", {
    expect_equal(Nothing()$fromMaybe(NULL), NULL)
})


test_that("Maybe Functor", {
    expect_equal(Just(1)$fmap(pAdd(3)), Just(4))
    expect_equal(Nothing()$fmap(pAdd(3)), Nothing())
})

test_that("Maybe Applicative", {
    expect_equal(Just(1)$pure(3), Just(3))
    expect_equal(Maybe$pure(3), Just(3))
    expect_equal(Maybe$pure(1), Just(1))
    expect_equal(Just(2)$fmap(pAdd)$apply(Just(3)), Just(5))
    expect_equal(Nothing()$fmap(pAdd)$apply(Just(3)), Nothing())
    expect_equal(Just(2)$fmap(pAdd)$apply(Nothing()), Nothing())
})


test_that("Maybe Monad", {
    expect_equal(Just(1)$bind(function(x) Just(x + 1)), Just(2))
    expect_equal(Just(1)$bind(function(x) Nothing()), Nothing())
    expect_equal(Nothing()$bind(function(x) Just(x + 1)), Nothing())
})
