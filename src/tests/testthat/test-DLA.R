# Test conditions for X

test_that("DLA: X must be numeric or complex atomic vector", {
  X <- NULL
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- list(1, 2, 3, 4)
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- c(list(1, 2, 3, 4))
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- data.frame(1:4)
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- data.frame(list(1:4))
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- logical(0)
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- character(0)
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- NA
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- NA_character_
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- c(TRUE, FALSE)
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- c("", "")
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- c("asdf", "ghjk")
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- c("42", "87")
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- c("a", "b", "c")
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- c(1L, 2, TRUE, "42", 5)
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- c(-pi, -0.1, "0", 1, 2)
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- c(1:100, "101")
  expect_error(DLA(X), "X must be numeric or complex atomic vector")

  X <- raw(1)
  expect_error(DLA(X), "X must be numeric or complex atomic vector")
})

test_that("DLA: X must have more than one value", {
  X <- integer(0)
  expect_error(DLA(X), "X must have more than one value")

  X <- double(0)
  expect_error(DLA(X), "X must have more than one value")

  X <- numeric(0)
  expect_error(DLA(X), "X must have more than one value")

  X <- complex(0)
  expect_error(DLA(X), "X must have more than one value")

  X <- integer(1)
  expect_error(DLA(X), "X must have more than one value")

  X <- double(1)
  expect_error(DLA(X), "X must have more than one value")

  X <- numeric(1)
  expect_error(DLA(X), "X must have more than one value")

  X <- complex(1)
  expect_error(DLA(X), "X must have more than one value")
})

test_that("DLA: X may not contain NAs", {
  X <- c(NaN, NaN)
  expect_error(DLA(X), "X may not contain NAs")

  X <- c(NA_integer_, NA_integer_)
  expect_error(DLA(X), "X may not contain NAs")

  X <- c(NA_real_, NA_real_)
  expect_error(DLA(X), "X may not contain NAs")

  X <- c(NA_complex_, NA_complex_)
  expect_error(DLA(X), "X may not contain NAs")

  X <- c(1, 2, NA, 4, 5)
  expect_error(DLA(X), "X may not contain NAs")

  X <- c(1, NA, NaN, NA_real_, 5)
  expect_error(DLA(X), "X may not contain NAs")
})

test_that("DLA: X may not contain Inf or -Inf values", {
  X <- c(Inf, Inf)
  expect_error(DLA(X), "X may not contain Inf or -Inf values")

  X <- c(-Inf, -Inf)
  expect_error(DLA(X), "X may not contain Inf or -Inf values")

  X <- c(1, Inf, -Inf, 4, 5)
  expect_error(DLA(X), "X may not contain Inf or -Inf values")
})

## Test conditions for m

test_that("DLA: m must be a value of length 1", {
  set.seed(1)
  X <- rnorm(4, mean = 0, sd = 1)
  expect_warning(pdl <- DLA(X), "This algorithm works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")

  m <- NULL
  expect_error(pdl(m), "m must be a value of length 1")

  m <- logical(0)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- integer(0)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- double(0)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- numeric(0)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- complex(0)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- character(0)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- raw(0)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- c()
  expect_error(pdl(m), "m must be a value of length 1")

  m <- c(1, 2)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- c(1, 2, 3)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- seq(from = 0, to = 1, by = 0.1)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- list()
  expect_error(pdl(m), "m must be a value of length 1")

  m <- list(1, 2, 3)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- c(TRUE, FALSE)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- c("a", "b", "c")
  expect_error(pdl(m), "m must be a value of length 1")

  m <- matrix(1:10, 5, 2)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- raw(2)
  expect_error(pdl(m), "m must be a value of length 1")
})

test_that("DLA: m must be numeric", {
  set.seed(1)
  X <- rnorm(4, mean = 0, sd = 1)
  expect_warning(pdl <- DLA(X), "This algorithm works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")

  m <- "a"
  expect_error(pdl(m), "m must be numeric")

  m <- TRUE
  expect_error(pdl(m), "m must be numeric")

  m <- factor(1)
  expect_error(pdl(m), "m must be numeric")

  m <- NA
  expect_error(pdl(m), "m must be numeric")
})

test_that("DLA: m must not be infinite", {
  set.seed(1)
  X <- rnorm(4, mean = 0, sd = 1)
  expect_warning(pdl <- DLA(X), "This algorithm works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")

  m <- Inf
  expect_error(pdl(m), "m must not be infinite")

  m <- -Inf
  expect_error(pdl(m), "m must not be infinite")
})

test_that("DLA: m must not be NA", {
  set.seed(1)
  X <- rnorm(4, mean = 0, sd = 1)
  expect_warning(pdl <- DLA(X), "This algorithm works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")

  m <- NA_integer_
  expect_error(pdl(m), "m must not be NA")

  m <- NA_real_
  expect_error(pdl(m), "m must not be NA")
})

test_that("DLA: m must be an integer", {
  set.seed(1)
  X <- rnorm(4, mean = 0, sd = 1)
  expect_warning(pdl <- DLA(X), "This algorithm works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")

  m <- 2.5
  expect_error(pdl(m), "m must be an integer")

  m <- pi
  expect_error(pdl(m), "m must be an integer")

  m <- 10e-6
  expect_error(pdl(m), "m must be an integer")

  m <- .Machine$double.eps
  expect_error(pdl(m), "m must be an integer")

  m <- 0.1
  expect_error(pdl(m), "m must be an integer")
})

test_that("DLA: m must be between 0 and length of X", {
  set.seed(1)
  X <- rnorm(4, mean = 0, sd = 1)
  expect_warning(pdl <- DLA(X), "This algorithm works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")

  m <- -1
  expect_error(pdl(m), "m must be between 0 and length of X")

  m <- 0
  expect_error(pdl(m), "m must be between 0 and length of X")

  m <- 5
  expect_error(pdl(m), "m must be between 0 and length of X")

  m <- 4
  expect_error(pdl(m), "m must be between 0 and length of X")
})

# Test on correctness

test_that("TEST DLA: correctness", {
  set.seed(2)
  X <- rnorm(5, mean = 0, sd = 1)
  expect_warning(pdl <- DLA(X), "This algorithm works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  result <- pdl(2)

  expect_equal(result$nu, 0.591788, tolerance = 1e-6)
  expect_equal(result$phi[1], -0.5088541, tolerance = 1e-6)
  expect_equal(result$phi[2], -0.5291920, tolerance = 1e-6)
})

# Test for the behavior of m at extreme values

test_that("Test on return", {
  # set.seed(3)
  X <- rnorm(5, mean = 0, sd = 1)
  expect_warning(pdl <- DLA(X), "This algorithm works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")

  m <- 1
  result <- pdl(m)
  expect_true(is.list(result))
  expect_true(all(names(result) %in% c("phi", "nu")))
  expect_equal(length(result$phi), 1)
  expect_equal(length(result$nu), 1)

  # Additional Test for extreme m
  m <- length(X)
  expect_error(pdl(m), "m must be between 0 and length of X")
})
