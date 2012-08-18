context("as.binary.matrix-functions")

m <- matrix(c(1, 2, NA, 2), nrow=2, ncol=2)
b <- matrix(c(1, 1, 0, 1), nrow=2, ncol=2)

test_that("as.binary.matrix", {
  expect_error(as.binary.matrix(1:10))
  expect_equal(as.binary.matrix(m), b)
})
