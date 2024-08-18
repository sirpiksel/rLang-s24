# TESTING make_MA

test_that("TEST make_ma: conditions on t", {
  theta <- 1

  t <- c(1, 2)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- list(1, 2)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- list(1)
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- data.frame(c(1))
  expect_error(make_MA(t, theta), "t must be a value of length 1")

  t <- Inf
  expect_error(make_MA(t, theta), "t must not be infinite")

  t <- -Inf
  expect_error(make_MA(t, theta), "t must not be infinite")

  t <- NA
  expect_error(make_MA(t, theta), "t must not be NA")

  t <- "3.2"
  expect_error(make_MA(t, theta), "t must be numeric")

  t <- T
  expect_error(make_MA(t, theta), "t must be numeric")

  t <- 3.2
  expect_error(make_MA(t, theta), "t must be an integer")

  t <- -1
  expect_error(make_MA(t, theta), "t must be positive")

  t <- matrix(c(1))
  expect_error(make_MA(t, theta), "t must be greater than the length of theta")
})

test_that("TEST make_ma: conditions on theta", {
  t <- 5

  theta <- list(1)
  expect_error(make_MA(t, theta), "theta must be an atomic vector")

  theta <- data.frame(c(1))
  expect_error(make_MA(t, theta), "theta must be an atomic vector")

  theta <- list()
  expect_error(make_MA(t, theta), "theta must be an atomic vector")

  theta <- c()
  expect_error(make_MA(t, theta), "theta must be an atomic vector")

  theta <- NULL
  expect_error(make_MA(t, theta), "theta must be an atomic vector")

  theta <- numeric(0)
  expect_error(make_MA(t, theta), "theta must have positive length")

  theta <- c(1, 2, NA)
  expect_error(make_MA(t, theta), "theta may not contain NAs")

  theta <- c(1, Inf, 3)
  expect_error(make_MA(t, theta), "theta may not contain Inf or -Inf values")

  theta <- c(1, 2, -Inf)
  expect_error(make_MA(t, theta), "theta may not contain Inf or -Inf values")

  theta <- c("1", "2")
  expect_error(make_MA(t, theta), "The values of theta must be numeric or complex")

  theta <- c(T, F, T)
  expect_error(make_MA(t, theta), "The values of theta must be numeric or complex")

  theta <- c(1, 2, 3, 4, 5, 6)
  expect_error(make_MA(t, theta), "t must be greater than the length of theta")

  theta <- c(1, 2, 3, 4, 5)
  expect_error(make_MA(t, theta), "t must be greater than the length of theta")
})

test_that("TEST make_ma: conditions on sigma", {
  theta <- 2
  t <- 3

  sigma <- c(1, 2)
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- list(1, 2)
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- list(1)
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- data.frame(c(1))
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- c()
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- NULL
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- list()
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- numeric(0)
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- list(1)
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- data.frame(c(1))
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- Inf
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be finite")

  sigma <- -Inf
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be finite")

  sigma <- NA
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must not be NA")

  sigma <- "1"
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be numeric")

  sigma <- T
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be numeric")

  sigma <- -1
  expect_error(make_MA(t, theta, sigma = sigma), "sigma must be positive")
})

test_that("TEST make_ma: correctness", {
  # MA(1)
  set.seed(123)
  t <- 3
  theta <- 1
  sigma <- 1
  werte <- rnorm(12, mean = 0, sd = 1)
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
  werte <- rnorm(12, mean = 0, sd = 1)
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
  werte <- rnorm(12, mean = 0, sd = 1)
  set.seed(123)

  sol <- make_MA(t, theta, sigma)

  expect_equal(sol[1], sum(werte[1:4]))
  expect_equal(sol[2], sum(werte[2:5]))
  expect_equal(sol[3], sum(werte[3:6]))
  expect_equal(sol[4], sum(werte[4:7]))
})
