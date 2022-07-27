

## test_that("all with predicate", {
##   expect_true(str_length("a"), 1)
##   expect_equal(str_length("ab"), 2)
##   expect_equal(str_length("abc"), 3)
## })


test_that("not", {
  expect_true(not(FALSE))
  expect_false(not(TRUE))
})

test_that("pEq", {
  expect_false(pEq(3)(4))
  expect_true(pEq(3)(3))
})


test_that("unlines", {
  expect_equal(unlines(list()), "")
  expect_equal(unlines(list("a", "b", "c")), "abc")
})

test_that("seq", {
  expect_equal((1 + 1) %seq% (2 + 3), 5)
})


test_that("and", {
  expect_equal(and(list()), TRUE)
  expect_equal(and(list(TRUE)), TRUE)
  expect_equal(and(list(TRUE, FALSE)), FALSE)
})

test_that("or", {
  expect_equal(or(list()), FALSE)
  expect_equal(or(list(FALSE)), FALSE)
  expect_equal(or(list(FALSE, TRUE)), TRUE)
})
