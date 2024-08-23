# TESTING periodogram

## conditions for X
test_that("periodogram: X must be an atomic vector", {
  lambda <- pi

  X <- NULL
  expect_error(periodogram(X, lambda), "X must be an atomic vector")

  X <- list(1, 2, 3, 4, 5)
  expect_error(periodogram(X, lambda), "X must be an atomic vector")

  X <- c(list(1, 2, 3, 4, 5))
  expect_error(periodogram(X, lambda), "X must be an atomic vector")
})

test_that("periodogram: X must have positive length", {
  lambda <- pi

  X <- logical(0)
  expect_error(periodogram(X, lambda), "X must have positive length")

  X <- integer(0)
  expect_error(periodogram(X, lambda), "X must have positive length")

  X <- double(0)
  expect_error(periodogram(X, lambda), "X must have positive length")

  X <- numeric(0)
  expect_error(periodogram(X, lambda), "X must have positive length")

  X <- complex(0)
  expect_error(periodogram(X, lambda), "X must have positive length")

  X <- character(0)
  expect_error(periodogram(X, lambda), "X must have positive length")

  X <- raw(0)
  expect_error(periodogram(X, lambda), "X must have positive length")
})

test_that("periodogram: X may not contain NAs", {
  lambda <- pi

  X <- NA
  expect_error(periodogram(X, lambda), "X may not contain NAs")

  X <- NaN
  expect_error(periodogram(X, lambda), "X may not contain NAs")

  X <- NA_integer_
  expect_error(periodogram(X, lambda), "X may not contain NAs")

  X <- NA_real_
  expect_error(periodogram(X, lambda), "X may not contain NAs")

  X <- NA_complex_
  expect_error(periodogram(X, lambda), "X may not contain NAs")

  X <- NA_character_
  expect_error(periodogram(X, lambda), "X may not contain NAs")

  X <- c(1, 2, NA, 4, 5)
  expect_error(periodogram(X, lambda), "X may not contain NAs")

  X <- c(1, NA, NaN, NA_real_, 5)
  expect_error(periodogram(X, lambda), "X may not contain NAs")

  X <- c(NULL, NA, Inf)
  expect_error(periodogram(X, lambda), "X may not contain NAs")
})

test_that("periodogram: X may not contain Inf or -Inf values", {
  lambda <- pi

  X <- Inf
  expect_error(periodogram(X, lambda), "X may not contain Inf or -Inf values")

  X <- -Inf
  expect_error(periodogram(X, lambda), "X may not contain Inf or -Inf values")

  X <- c(1, Inf, -Inf, 4, 5)
  expect_error(periodogram(X, lambda), "X may not contain Inf or -Inf values")

  X <- c(NULL, Inf, 3, 4, 5)
  expect_error(periodogram(X, lambda), "X may not contain Inf or -Inf values")

  X <- c(1, NULL, -Inf, 4, 5)
  expect_error(periodogram(X, lambda), "X may not contain Inf or -Inf values")
})

test_that("periodogram: The values of X must be numeric or complex", {
  lambda <- pi

  X <- c(TRUE, FALSE)
  expect_error(periodogram(X, lambda), "The values of X must be numeric or complex")

  X <- ""
  expect_error(periodogram(X, lambda), "The values of X must be numeric or complex")

  X <- "asdf"
  expect_error(periodogram(X, lambda), "The values of X must be numeric or complex")

  X <- "42"
  expect_error(periodogram(X, lambda), "The values of X must be numeric or complex")

  X <- c("a", "b", "c")
  expect_error(periodogram(X, lambda), "The values of X must be numeric or complex")

  X <- c(1L, 2, TRUE, "42", 5)
  expect_error(periodogram(X, lambda), "The values of X must be numeric or complex")

  X <- c(-pi, -0.1, "0", 1, 2)
  expect_error(periodogram(X, lambda), "The values of X must be numeric or complex")

  X <- c(1:100, "101")
  expect_error(periodogram(X, lambda), "The values of X must be numeric or complex")

  X <- raw(1)
  expect_error(periodogram(X, lambda), "The values of X must be numeric or complex")
})


## conditions for lambda
test_that("periodogram: lambda must be a value of length 1", {
  X <- 1:10

  lambda <- NULL
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- logical(0)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- integer(0)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- double(0)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- numeric(0)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- complex(0)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- character(0)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- raw(0)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- c()
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- c(1, 2)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- c(1, 2, 3)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- seq(from = 0, to = 1, by = 0.1)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- 1:10
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- list()
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- list(1)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- list(1, 2, 3)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- c(TRUE, FALSE)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- c("a", "b", "c")
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- matrix(1:10, 5, 2)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")

  lambda <- raw(2)
  expect_error(periodogram(X, lambda), "lambda must be a value of length 1")
})

test_that("periodogram: lambda must be numeric", {
  X <- 1:10

  lambda <- NA
  expect_error(periodogram(X, lambda), "lambda must be numeric")

  lambda <- NA_character_
  expect_error(periodogram(X, lambda), "lambda must be numeric")

  lambda <- NA_complex_
  expect_error(periodogram(X, lambda), "lambda must be numeric")

  lambda <- TRUE
  expect_error(periodogram(X, lambda), "lambda must be numeric")

  lambda <- F
  expect_error(periodogram(X, lambda), "lambda must be numeric")

  lambda <- ""
  expect_error(periodogram(X, lambda), "lambda must be numeric")

  lambda <- "pi"
  expect_error(periodogram(X, lambda), "lambda must be numeric")

  lambda <- "42"
  expect_error(periodogram(X, lambda), "lambda must be numeric")

  lambda <- 0i
  expect_error(periodogram(X, lambda), "lambda must be numeric")

  lambda <- 1i
  expect_error(periodogram(X, lambda), "lambda must be numeric")

  lambda <- 2 + 5i
  expect_error(periodogram(X, lambda), "lambda must be numeric")

  lambda <- raw(1)
  expect_error(periodogram(X, lambda), "lambda must be numeric")
})

test_that("periodogram: lambda must be from the interval (-pi, pi]", {
  X <- 1:10

  lambda <- NaN
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- NA_integer_
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- NA_real_
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- Inf
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- -Inf
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- log(0)
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- log(100)
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- exp(2)
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- pi + 2 * .Machine$double.eps
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- -(pi + .Machine$double.eps)
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- -pi
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- 3.141593
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- -3.141593
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- pi + 0.0001
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- -(pi + 0.0001)
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- 2 * pi
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- -2 * pi
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- 100
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )

  lambda <- -100
  expect_error(
    periodogram(X, lambda),
    "lambda must be from the interval \\(-pi, pi\\]"
  )
})


## correct values for X
test_that("periodogram: correct values for X", {
  lambda <- pi / 2

  X <- -20:-1
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 10, tolerance = 1e-6)

  X <- -20:0
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 9.52381, tolerance = 1e-6)

  X <- -20:50
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 21.42254, tolerance = 1e-6)

  X <- 1:100
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 50, tolerance = 1e-6)

  X <- 1:1000
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 500, tolerance = 1e-6)

  X <- seq(from = -5, to = 0, by = 0.05)
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 0.1237624, tolerance = 1e-6)

  X <- seq(from = -10, to = 10, by = 0.25)
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 1.234568, tolerance = 1e-6)

  X <- seq(from = 0, to = 20, by = 0.25)
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 2.469136, tolerance = 1e-6)

  X <- seq(from = 0, to = 10, by = 0.1)
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 0.4950495, tolerance = 1e-6)

  X <- seq(from = 0, to = 100, by = 2.5)
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 121.9512, tolerance = 1e-6)

  X <- c(1L, 2.5, pi, 4i, 42L)
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 258.4151, tolerance = 1e-6)

  X <- c(-0.75, 0.222, 4.681, 15.32, 1042)
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 214940.6, tolerance = 1e-6)

  X <- c(-42L, -21i, 0, pi, 563)
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 50001.97, tolerance = 1e-6)

  X <- c(-10i, -5i + 2, pi, exp(5), log(100))
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 3724.211, tolerance = 1e-6)

  X <- c(-10i, -5i + 2, pi, log(100), exp(5))
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 3946.159, tolerance = 1e-6)

  set.seed(123)
  X <- rnorm(100)
  lambda <- pi / 4
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 1.392404, tolerance = 1e-6)

  set.seed(123)
  X <- runif(100)
  lambda <- pi / 4
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 0.0346715, tolerance = 1e-6)

  X <- sin(2 * pi * 1:100 / 100)
  lambda <- 2 * pi / 100
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 25, tolerance = 1e-6)

  X <- exp(2 * pi * 1i * 1:100 / 100)
  lambda <- 2 * pi / 100
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 100, tolerance = 1e-6)
})


## correct values for lambda
test_that("periodogram: correct values for lambda", {
  X <- 1:10

  lambda <- -3.1415
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 2.5, tolerance = 1e-6)

  lambda <- -pi / 2
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 6.1, tolerance = 1e-6)

  lambda <- -pi / 4
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 15.2397, tolerance = 1e-6)

  lambda <- 0
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 302.5, tolerance = 1e-6)

  lambda <- pi / 4
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 15.2397, tolerance = 1e-6)

  lambda <- pi / 2
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 6.1, tolerance = 1e-6)

  lambda <- pi
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 2.5, tolerance = 1e-6)

  lambda <- exp(1)
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 2.967701, tolerance = 1e-6)

  lambda <- log(1)
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 302.5, tolerance = 1e-6)

  lambda <- log(pi)
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 10.27419, tolerance = 1e-6)

  lambda <- pi / 100
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 300.7134, tolerance = 1e-6)

  lambda <- 2 * pi / 100
  expect_warning(out <- periodogram(X, lambda), "This function works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  expect_equal(out, 295.4101, tolerance = 1e-6)
})
