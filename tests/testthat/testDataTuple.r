

test_that("fst", {
  expect_equal(fst(sets::tuple(1, 2)), 1)
})


test_that("snd", {
  expect_equal(snd(sets::tuple(1, 2)), 2)
})


test_that("uncurry", {
  expect_equal(uncurry(function(x, y) x + y)(sets::tuple(1, 2)), 3)
})
