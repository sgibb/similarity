context("similarity-functions")

a <- matrix(c(1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1),
            ncol=3, nrow=4, byrow=TRUE,
            dimnames=list(LETTERS[1:4], paste("F", 1:3, sep="")))

b <- matrix(c(2, 3, 4, 0, 0, 0, 7, 9, 0, 8, 0, 5),
            ncol=3, nrow=4, byrow=TRUE)

test_that("general", {
  expect_error(similarity(1))
  expect_error(similarity("1"))
  expect_error(similarity(list(c(1, 0), c(0, 1))))
  expect_error(similarity(b))

  w <- a
  w[2, 3] <- 0
  expect_warning(similarity(w), "It doesn't make sense to use ")

  s <- similarity(a)
  expect_true(class(s) == "similarity")
  expect_true(attr(s, "Size") == nrow(a))
  expect_true(all(attr(s, "Labels") == LETTERS[1:nrow(a)]))
  expect_true(all(attr(s, "diagonalValues") == c(1, 1, 1, 1)))

  m <- matrix(c(1.0, 0.5, 0.8, 0.8,
                0.5, 1.0, 0, 2/3,
                0.8, 0, 1.0, 0.5,
                0.8, 2/3, 0.5, 1), nrow=4, ncol=4, byrow=TRUE,
              dimnames=list(LETTERS[1:4], LETTERS[1:4]))

  expect_equal(m, as.matrix(s))
})

test_that("soerensen", {
  s <- c(0.5, 0.8, 0.8, 0, 2/3, 0.5)
  expect_true(all(similarity(a) == s))
  expect_true(all(similarity(a, method="dice") == s))
  expect_true(all(similarity(a, method="soerensen") == s))
  expect_true(all(similarity(a, method="soerensen-dice") == s))
})

test_that("jaccard", {
  s <- c(1/3, 2/3, 2/3, 0, 0.5, 1/3)
  expect_true(all(similarity(a, method="jaccard") == s))
})

test_that("simplematching", {
  s <- c(1/3, 2/3, 2/3, 0, 2/3, 1/3)
  expect_true(all(similarity(a, method="simple") == s))
  expect_true(all(similarity(a, method="simplematching") == s))
})

test_that("rogers", {
  s <- c(0.2, 0.5, 0.5, 0.0, 0.5, 0.2)
  expect_true(all(similarity(a, method="rogers") == s))
  expect_true(all(similarity(a, method="tanimoto") == s))
  expect_true(all(similarity(a, method="rogers-tanimoto") == s))
})

