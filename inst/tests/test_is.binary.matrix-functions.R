context("is.binary.matrix-functions")

m <- matrix(c(1, 2, NA, 2), nrow=2, ncol=2)
b <- matrix(c(1, 1, 0, 1), nrow=2, ncol=2)

test_that("is.binary.matrix", {
  expect_true(!is.binary.matrix(1:10))
  expect_true(!is.binary.matrix(1))
  expect_true(!is.binary.matrix("0"))
  expect_true(!is.binary.matrix(m))
  expect_true(is.binary.matrix(b))
})
