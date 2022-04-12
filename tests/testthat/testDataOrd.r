
test_that("pEq", {
  expect_false(pEq(3)(4))
  expect_true(pEq(3)(3))
})

test_that("pNeq", {
  expect_true(pNeq(3)(4))
  expect_false(pNeq(3)(3))
})


test_that("pGeq", {
    expect_true(pGeq(3)(4))
    expect_false(pGeq(3)(2))
    expect_true(all(pGeq(3), list(3, 3, 3, 3, 3)))
    expect_true(all(pGeq(3), list(3, 3, 3, 4, 3)))
    expect_false(all(pGeq(3), list(3, 3, 3, 2, 3)))
})


test_that("pLeq", {
    expect_false(pLeq(3)(4))
    expect_true(pLeq(3)(2))
    expect_true(all(pLeq(3), list(3, 3, 3, 3, 3)))
    expect_false(all(pLeq(3), list(3, 3, 3, 4, 3)))
    expect_true(all(pLeq(3), list(3, 3, 3, 2, 3)))
})


test_that("pGt", {
    expect_true(pGt(3)(4))
    expect_false(pGt(3)(2))
    expect_false(all(pGt(3), list(4, 4, 4, 3, 4)))
    expect_true(all(pGt(3), list(4, 4, 4, 4, 4)))
    expect_false(all(pGt(3), list(4, 4, 4, 2, 4)))
})


test_that("pLt", {
    expect_false(pLt(3)(4))
    expect_true(pLt(3)(2))
    expect_false(all(pLt(3), list(2, 2, 2, 3, 2)))
    expect_false(all(pLt(3), list(2, 2, 2, 4, 2)))
    expect_true(all(pLt(3), list(2, 2, 2, 2, 2)))
})
