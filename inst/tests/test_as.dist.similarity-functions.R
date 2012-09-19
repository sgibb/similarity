context("as.dist.similarity-functions")

a <- matrix(c(1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1),
            ncol=3, nrow=4, byrow=TRUE,
            dimnames=list(LETTERS[1:4], paste("F", 1:3, sep="")))

test_that("general", {
  d <- as.dist(similarity(a))
  expect_true(class(d) == "dist")
  expect_true(attr(d, "Size") == nrow(a))
  expect_true(all(attr(d, "Labels") == LETTERS[1:nrow(a)]))
  expect_true(is.null(attr(d, "diagonalValues")))

  r <- c(0.5, 0.8, 0.8, 0, 2/3, 0.5)
  r <- sqrt(2)*sqrt(1-r)

  expect_equal(r, as.vector(d))
})

