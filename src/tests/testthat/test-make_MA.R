# TESTING make_MA

test_that("make_MA: t must be a value of length 1", {
  theta <- 1:10

  t <- NULL
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- logical(0)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- integer(0)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- double(0)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- numeric(0)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- complex(0)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- character(0)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- raw(0)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- c()
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- c(1, 2)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- c(1, 2, 3)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- seq(from = 0, to = 1, by = 0.1)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- 1:10
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- list()
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- list(1)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- list(1, 2, 3)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- c(TRUE, FALSE)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- c("a", "b", "c")
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- matrix(1:10, 5, 2)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- raw(2)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- data.frame(c(1))
  expect_error(make_MA(t, theta), "t must be a value of length 1")
})

test_that("make_MA: t must not be infinite", {
  phi <- 1:10

  t <- Inf
  expect_error(make_MA(t, phi), "t must not be infinite")

  t <- -Inf
  expect_error(make_MA(t, phi), "t must not be infinite")
})

test_that("make_MA: t must not be NA", {
  theta <- 1:10

  t <- NA
  expect_error(make_MA(t, theta), "t must not be NA")

  t <- NaN
  expect_error(make_MA(t, theta), "t must not be NA")

  t <- NA_integer_
  expect_error(make_MA(t, theta), "t must not be NA")

  t <- NA_real_
  expect_error(make_MA(t, theta), "t must not be NA")

  t <- NA_complex_
  expect_error(make_MA(t, theta), "t must not be NA")

  t <- NA_character_
  expect_error(make_MA(t, theta), "t must not be NA")
})

test_that("make_MA: t must be numeric", {
  theta <- 1:10

  t <- TRUE
  expect_error(make_MA(t, theta), "t must be numeric")

  t <- FALSE
  expect_error(make_MA(t, theta), "t must be numeric")

  t <- ""
  expect_error(make_MA(t, theta), "t must be numeric")

  t <- "pi"
  expect_error(make_MA(t, theta), "t must be numeric")

  t <- "42"
  expect_error(make_MA(t, theta), "t must be numeric")

  t <- "3.2"
  expect_error(make_MA(t, theta), "t must be numeric")

  t <- 0i
  expect_error(make_MA(t, theta), "t must be numeric")

  t <- 1i
  expect_error(make_MA(t, theta), "t must be numeric")

  t <- 2 + 5i
  expect_error(make_MA(t, theta), "t must be numeric")

  t <- raw(1)
  expect_error(make_MA(t, theta), "t must be numeric")
})

test_that("make_MA: t must be an integer", {
  phi <- 1:10

  t <- 3.2
  expect_error(make_MA(t, theta), "t must be an integer")

  t <- 10^-6
  expect_error(make_MA(t, theta), "t must be an integer")

  t <- pi
  expect_error(make_MA(t, theta), "t must be an integer")

  t <- .Machine$double.eps
  expect_error(make_MA(t, theta), "t must be an integer")

  t <- 0.1
  expect_error(make_MA(t, theta), "t must be an integer")
})

test_that("make_MA: t must be positive", {
  phi <- 1:10

  t <- -2
  expect_error(make_MA(t, theta), "t must be positive")

  t <- -1
  expect_error(make_MA(t, theta), "t must be positive")

  t <- 0
  expect_error(make_MA(t, theta), "t must be positive")

  t <- -2L
  expect_error(make_MA(t, theta), "t must be positive")

  t <- -1L
  expect_error(make_MA(t, theta), "t must be positive")

  t <- 0L
  expect_error(make_MA(t, theta), "t must be positive")
})


test_that("make_MA: theta must be an atomic vector", {
  t <- 10

  theta <- NULL
  expect_error(make_MA(t, theta), "theta must be an atomic vector")

  theta <- list(1, 2, 3, 4, 5)
  expect_error(make_MA(t, theta), "theta must be an atomic vector")

  theta <- c(list(1, 2, 3, 4, 5))
  expect_error(make_MA(t, theta), "theta must be an atomic vector")

  theta <- data.frame(c(1))
  expect_error(make_MA(t, theta), "theta must be an atomic vector")

  theta <- c()
  expect_error(make_MA(t, theta), "theta must be an atomic vector")
})

test_that("make_MA: theta must have positive length", {
  t <- 10

  theta <- logical(0)
  expect_error(make_MA(t, theta), "theta must have positive length")

  theta <- integer(0)
  expect_error(make_MA(t, theta), "theta must have positive length")

  theta <- double(0)
  expect_error(make_MA(t, theta), "theta must have positive length")

  theta <- numeric(0)
  expect_error(make_MA(t, theta), "theta must have positive length")

  theta <- complex(0)
  expect_error(make_MA(t, theta), "theta must have positive length")

  theta <- character(0)
  expect_error(make_MA(t, theta), "theta must have positive length")

  theta <- raw(0)
  expect_error(make_MA(t, theta), "theta must have positive length")
})

test_that("make_MA: theta may not contain NAs", {
  t <- 10

  theta <- NA
  expect_error(make_MA(t, theta), "theta may not contain NAs")

  theta <- NaN
  expect_error(make_MA(t, theta), "theta may not contain NAs")

  theta <- NA_integer_
  expect_error(make_MA(t, theta), "theta may not contain NAs")

  theta <- NA_real_
  expect_error(make_MA(t, theta), "theta may not contain NAs")

  theta <- NA_complex_
  expect_error(make_MA(t, theta), "theta may not contain NAs")

  theta <- NA_character_
  expect_error(make_MA(t, theta), "theta may not contain NAs")

  theta <- c(1, 2, NA, 4, 5)
  expect_error(make_MA(t, theta), "theta may not contain NAs")

  theta <- c(1, NA, NaN, NA_real_, 5)
  expect_error(make_MA(t, theta), "theta may not contain NAs")

  theta <- c(NULL, NA, Inf)
  expect_error(make_MA(t, theta), "theta may not contain NAs")
})

test_that("make_MA: theta may not contain Inf or -Inf values", {
  t <- 10

  theta <- Inf
  expect_error(make_MA(t, theta), "theta may not contain Inf or -Inf values")

  theta <- -Inf
  expect_error(make_MA(t, theta), "theta may not contain Inf or -Inf values")

  theta <- c(1, Inf, -Inf, 4, 5)
  expect_error(make_MA(t, theta), "theta may not contain Inf or -Inf values")

  theta <- c(NULL, Inf, 3, 4, 5)
  expect_error(make_MA(t, theta), "theta may not contain Inf or -Inf values")

  theta <- c(1, NULL, -Inf, 4, 5)
  expect_error(make_MA(t, theta), "theta may not contain Inf or -Inf values")
})

test_that("make_MA: The values of theta must be numeric or complex", {
  t <- 10

  theta <- c(TRUE, FALSE)
  expect_error(make_MA(t, theta), "The values of theta must be numeric or complex")

  theta <- ""
  expect_error(make_MA(t, theta), "The values of theta must be numeric or complex")

  theta <- "asdf"
  expect_error(make_MA(t, theta), "The values of theta must be numeric or complex")

  theta <- "42"
  expect_error(make_MA(t, theta), "The values of theta must be numeric or complex")

  theta <- c("a", "b", "c")
  expect_error(make_MA(t, theta), "The values of theta must be numeric or complex")

  theta <- c(1L, 2, TRUE, "42", 5)
  expect_error(make_MA(t, theta), "The values of theta must be numeric or complex")

  theta <- c(-pi, -0.1, "0", 1, 2)
  expect_error(make_MA(t, theta), "The values of theta must be numeric or complex")

  theta <- c(1:100, "101")
  expect_error(make_MA(t, theta), "The values of theta must be numeric or complex")

  theta <- raw(1)
  expect_error(make_MA(t, theta), "The values of theta must be numeric or complex")
})

test_that("make_MA: t must be greater than the length of theta", {
  t <- 5
  theta <- c(1, 2, 3, 4, 5)
  expect_error(make_MA(t, theta), "t must be greater than the length of theta")

  t <- 4
  theta <- c(12, -2, 31, 124, -10)
  expect_error(make_MA(t, theta), "t must be greater than the length of theta")

  t <- 2
  theta <- c(5, 6)
  expect_error(make_MA(t, theta), "t must be greater than the length of theta")

  t <- 1
  theta <- c(1, 2)
  expect_error(make_MA(t, theta), "t must be greater than the length of theta")

  t <- 2
  theta <- c(-3 + 1i, 3 + 2i)
  expect_error(make_MA(t, theta), "t must be greater than the length of theta")

  t <- 1
  theta <- c(1 + 1i, 2i)
  expect_error(make_MA(t, theta), "t must be greater than the length of theta")
})

test_that("make_MA: sigma must be a value of length 1", {
  theta <- 2
  t <- 3

  sigma <- NULL
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- logical(0)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- integer(0)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- double(0)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- numeric(0)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- complex(0)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- character(0)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- raw(0)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- c()
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- c(1, 2)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- c(1, 2, 3)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- seq(from = 0, to = 1, by = 0.1)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- 1:10
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- list()
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- list(1)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- list(1, 2, 3)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- c(TRUE, FALSE)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- c("a", "b", "c")
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- matrix(1:10, 5, 2)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- raw(2)
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- data.frame(c(1))
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")

  sigma <- data.frame(c(1))
  expect_error(make_MA(t, theta, sigma), "sigma must be a value of length 1")
})

test_that("make_MA: sigma must not be infinite", {
  theta <- 2
  t <- 3

  sigma <- Inf
  expect_error(make_MA(t, theta, sigma), "sigma must not be infinite")

  sigma <- -Inf
  expect_error(make_MA(t, theta, sigma), "sigma must not be infinite")
})

test_that("make_MA: sigma must not be NA", {
  theta <- 2
  t <- 3

  sigma <- NA
  expect_error(make_MA(t, theta, sigma), "sigma must not be NA")

  sigma <- NaN
  expect_error(make_MA(t, theta, sigma), "sigma must not be NA")

  sigma <- NA_integer_
  expect_error(make_MA(t, theta, sigma), "sigma must not be NA")

  sigma <- NA_real_
  expect_error(make_MA(t, theta, sigma), "sigma must not be NA")

  sigma <- NA_complex_
  expect_error(make_MA(t, theta, sigma), "sigma must not be NA")

  sigma <- NA_character_
  expect_error(make_MA(t, theta, sigma), "sigma must not be NA")
})

test_that("make_MA: sigma must be numeric", {
  theta <- 2
  t <- 3

  sigma <- TRUE
  expect_error(make_MA(t, theta, sigma), "sigma must be numeric")

  sigma <- FALSE
  expect_error(make_MA(t, theta, sigma), "sigma must be numeric")

  sigma <- ""
  expect_error(make_MA(t, theta, sigma), "sigma must be numeric")

  sigma <- "pi"
  expect_error(make_MA(t, theta, sigma), "sigma must be numeric")

  sigma <- "42"
  expect_error(make_MA(t, theta, sigma), "sigma must be numeric")

  sigma <- "3.2"
  expect_error(make_MA(t, theta, sigma), "sigma must be numeric")

  sigma <- 0i
  expect_error(make_MA(t, theta, sigma), "sigma must be numeric")

  sigma <- 1i
  expect_error(make_MA(t, theta, sigma), "sigma must be numeric")

  sigma <- 2 + 5i
  expect_error(make_MA(t, theta, sigma), "sigma must be numeric")

  sigma <- raw(1)
  expect_error(make_MA(t, theta, sigma), "sigma must be numeric")
})

test_that("make_MA: sigma must be positive", {
  theta <- 2
  t <- 3

  sigma <- -2
  expect_error(make_MA(t, theta, sigma), "sigma must be positive")

  sigma <- -1
  expect_error(make_MA(t, theta, sigma), "sigma must be positive")

  sigma <- 0
  expect_error(make_MA(t, theta, sigma), "sigma must be positive")

  sigma <- -2L
  expect_error(make_MA(t, theta, sigma), "sigma must be positive")

  sigma <- -1L
  expect_error(make_MA(t, theta, sigma), "sigma must be positive")

  sigma <- 0L
  expect_error(make_MA(t, theta, sigma), "sigma must be positive")
})

test_that("TEST make_ma: correctness", {
  # MA(1)
  set.seed(123)
  t <- 3
  theta <- 1
  sigma <- 1
  werte <- rnorm(12, mean = 0, sd = sigma)
  set.seed(123)

  sol <- make_MA(t, theta, sigma)

  expect_equal(sol[1], sum(werte[1:2]))
  expect_equal(sol[2], sum(werte[2:3]))
  expect_equal(sol[3], sum(werte[3:4]))

  # MA(2)
  set.seed(123)
  t <- 3
  theta <- c(1, 1)
  sigma <- 1
  werte <- rnorm(12, mean = 0, sd = sigma)
  set.seed(123)

  sol <- make_MA(t, theta, sigma)

  expect_equal(sol[1], sum(werte[1:3]))
  expect_equal(sol[2], sum(werte[2:4]))
  expect_equal(sol[3], sum(werte[3:5]))

  # MA(3)
  set.seed(123)
  t <- 4
  theta <- c(1, 1, 1)
  sigma <- 1
  werte <- rnorm(12, mean = 0, sd = sigma)
  set.seed(123)

  sol <- make_MA(t, theta, sigma)

  expect_equal(sol[1], sum(werte[1:4]))
  expect_equal(sol[2], sum(werte[2:5]))
  expect_equal(sol[3], sum(werte[3:6]))
  expect_equal(sol[4], sum(werte[4:7]))
})
