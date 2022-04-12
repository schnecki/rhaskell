

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
