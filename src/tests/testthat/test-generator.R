## TESTING: make_ar

test_that("TEST make_ar: conditions on t", {
  phi <- 1

  t <- c(1, 2)
  expect_error(make_ar(t, phi), "t must be a value of length 1")

  t <- list(1, 2)
  expect_error(make_ar(t, phi), "t must be a value of length 1")

  t <- list(1)
  expect_error(make_ar(t, phi), "t must be a value of length 1")

  t <- data.frame(c(1))
  expect_error(make_ar(t, phi), "t must be a value of length 1")

  t <- Inf
  expect_error(make_ar(t, phi), "t must not be infinite")

  t <- -Inf
  expect_error(make_ar(t, phi), "t must not be infinite")

  t <- NA
  expect_error(make_ar(t, phi), "t must not be NA")

  t <- "3.2"
  expect_error(make_ar(t, phi), "t must be numeric")

  t <- T
  expect_error(make_ar(t, phi), "t must be numeric")

  t <- 3.2
  expect_error(make_ar(t, phi), "t must be an integer")

  t <- -1
  expect_error(make_ar(t, phi), "t must be positive")

  t <- matrix(c(1))
  expect_error(make_ar(t, phi), "t must be greater than the length of phi")
})

test_that("TEST make_ar: conditions on phi", {
  t <- 5

  phi <- list(1)
  expect_error(make_ar(t, phi), "phi must be an atomic vector")

  phi <- data.frame(c(1))
  expect_error(make_ar(t, phi), "phi must be an atomic vector")

  phi <- list()
  expect_error(make_ar(t, phi), "phi must be an atomic vector")

  phi <- c()
  expect_error(make_ar(t, phi), "phi must have positive length")

  phi <- NULL
  expect_error(make_ar(t, phi), "phi must have positive length")

  phi <- numeric(0)
  expect_error(make_ar(t, phi), "phi must have positive length")

  phi <- c(1, 2, NA)
  expect_error(make_ar(t, phi), "phi may not contain NAs")

  phi <- c(1, Inf, 3)
  expect_error(make_ar(t, phi), "phi may not contain Inf or -Inf values")

  phi <- c(1, 2, -Inf)
  expect_error(make_ar(t, phi), "phi may not contain Inf or -Inf values")

  phi <- c("1", "2")
  expect_error(make_ar(t, phi), "The values of phi must be numeric or complex")

  phi <- c(T, F, T)
  expect_error(make_ar(t, phi), "The values of phi must be numeric or complex")

  phi <- c(1, 2, 3, 4, 5, 6)
  expect_error(make_ar(t, phi), "t must be greater than the length of phi")

  phi <- c(1, 2, 3, 4, 5)
  expect_error(make_ar(t, phi), "t must be greater than the length of phi")

  phi <- 1
  expect_error(make_ar(t, phi), "The polynomial phi must have no roots on the unit circle")

  phi <- -1
  expect_error(make_ar(t, phi), "The polynomial phi must have no roots on the unit circle")

  phi <- matrix(c(1))
  expect_error(make_ar(t, phi), "The polynomial phi must have no roots on the unit circle")

  phi <- c(-1, 2)
  expect_error(make_ar(t, phi), "The polynomial phi must have no roots on the unit circle")
})

test_that("TEST make_ar: conditions on start", {
  phi <- c(1, 6)
  t <- 3

  start <- list(1)
  expect_error(make_ar(t, phi, start = start), "start must be an atomic vector")

  start <- data.frame(c(1))
  expect_error(make_ar(t, phi, start = start), "start must be an atomic vector")

  start <- list()
  expect_error(make_ar(t, phi, start = start), "start must be an atomic vector")

  start <- c()
  expect_error(make_ar(t, phi, start = start), "start must have positive length")

  start <- NULL
  expect_error(make_ar(t, phi, start = start), "start must have positive length")

  start <- numeric(0)
  expect_error(make_ar(t, phi, start = start), "start must have positive length")

  start <- 1:4
  expect_error(make_ar(t, phi, start = start), "length of start must be smaller than or equal to length of phi")

  start <- 1:3
  expect_error(make_ar(t, phi, start = start), "length of start must be smaller than or equal to length of phi")

  start <- c(1, NA)
  expect_error(make_ar(t, phi, start = start), "start may not contain NAs")

  start <- c(1, Inf)
  expect_error(make_ar(t, phi, start = start), "start may not contain Inf or -Inf values")

  start <- c(-Inf, 2)
  expect_error(make_ar(t, phi, start = start), "start may not contain Inf or -Inf values")

  start <- c("1", "2")
  expect_error(make_ar(t, phi, start = start), "The values of start must be numeric or complex")

  start <- c(T, F)
  expect_error(make_ar(t, phi, start = start), "The values of start must be numeric or complex")
})

test_that("TEST make_ar: conditions on sigma", {
  phi <- 2
  t <- 3

  sigma <- c(1, 2)
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be a value of length 1")

  sigma <- list(1, 2)
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be a value of length 1")

  sigma <- list(1)
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be a value of length 1")

  sigma <- data.frame(c(1))
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be a value of length 1")

  sigma <- c()
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be a value of length 1")

  sigma <- NULL
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be a value of length 1")

  sigma <- list()
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be a value of length 1")

  sigma <- numeric(0)
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be a value of length 1")

  sigma <- list(1)
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be a value of length 1")

  sigma <- data.frame(c(1))
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be a value of length 1")

  sigma <- Inf
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be finite")

  sigma <- -Inf
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be finite")

  sigma <- NA
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must not be NA")

  sigma <- "1"
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be numeric")

  sigma <- T
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be numeric")

  sigma <- -1
  expect_error(make_ar(t, phi, sigma = sigma), "sigma must be positive")
})

test_that("TEST make_ar: correctness", {
  # AR(1)
  set.seed(123)
  t <- 3
  theta <- 2
  sigma <- 1
  start <- 1
  werte <- rnorm(12, mean = 0, sd = 1)
  set.seed(123)

  sol <- make_ar(t, theta, sigma, start)

  expect_equal(sol[1], start)
  expect_equal(sol[2], 2 * start + werte[2])
  expect_equal(sol[3], 4 * start + 2 * werte[2] + werte[3])

  # AR(2)
  set.seed(123)
  t <- 4
  theta <- c(1, 1)
  sigma <- 1
  start <- 1
  werte <- rnorm(12, mean = 0, sd = 1)
  set.seed(123)

  sol <- make_ar(t, theta, sigma, start)

  expect_equal(sol[1], start)
  expect_equal(sol[2], 0)
  expect_equal(sol[3], 1 + werte[3])
  expect_equal(sol[4], 1 + werte[3] + werte[4])

  # AR(3)
  set.seed(123)
  t <- 5
  theta <- c(1, 1, 1)
  sigma <- 1
  start <- 1
  werte <- rnorm(12, mean = 0, sd = 1)
  set.seed(123)

  sol <- make_ar(t, theta, sigma, start)

  expect_equal(sol[1], 1)
  expect_equal(sol[2], 0)
  expect_equal(sol[3], 0)
  expect_equal(sol[4], 1 + werte[4])
  expect_equal(sol[5], 1 + werte[4] + werte[5])
})

## TESTING: make_ma

test_that("TEST make_ma: conditions on t", {
  theta <- 1

  t <- c(1, 2)
  expect_error(make_ma(t, theta), "t must be a value of length 1")

  t <- list(1, 2)
  expect_error(make_ma(t, theta), "t must be a value of length 1")

  t <- list(1)
  expect_error(make_ma(t, theta), "t must be a value of length 1")

  t <- data.frame(c(1))
  expect_error(make_ma(t, theta), "t must be a value of length 1")

  t <- Inf
  expect_error(make_ma(t, theta), "t must not be infinite")

  t <- -Inf
  expect_error(make_ma(t, theta), "t must not be infinite")

  t <- NA
  expect_error(make_ma(t, theta), "t must not be NA")

  t <- "3.2"
  expect_error(make_ma(t, theta), "t must be numeric")

  t <- T
  expect_error(make_ma(t, theta), "t must be numeric")

  t <- 3.2
  expect_error(make_ma(t, theta), "t must be an integer")

  t <- -1
  expect_error(make_ma(t, theta), "t must be positive")

  t <- matrix(c(1))
  expect_error(make_ma(t, theta), "t must be greater than the length of theta")
})

test_that("TEST make_ma: conditions on theta", {
  t <- 5

  theta <- list(1)
  expect_error(make_ma(t, theta), "theta must be an atomic vector")

  theta <- data.frame(c(1))
  expect_error(make_ma(t, theta), "theta must be an atomic vector")

  theta <- list()
  expect_error(make_ma(t, theta), "theta must be an atomic vector")

  theta <- c()
  expect_error(make_ma(t, theta), "theta must have positive length")

  theta <- NULL
  expect_error(make_ma(t, theta), "theta must have positive length")

  theta <- numeric(0)
  expect_error(make_ma(t, theta), "theta must have positive length")

  theta <- c(1, 2, NA)
  expect_error(make_ma(t, theta), "theta may not contain NAs")

  theta <- c(1, Inf, 3)
  expect_error(make_ma(t, theta), "theta may not contain Inf or -Inf values")

  theta <- c(1, 2, -Inf)
  expect_error(make_ma(t, theta), "theta may not contain Inf or -Inf values")

  theta <- c("1", "2")
  expect_error(make_ma(t, theta), "The values of theta must be numeric or complex")

  theta <- c(T, F, T)
  expect_error(make_ma(t, theta), "The values of theta must be numeric or complex")

  theta <- c(1, 2, 3, 4, 5, 6)
  expect_error(make_ma(t, theta), "t must be greater than the length of theta")

  theta <- c(1, 2, 3, 4, 5)
  expect_error(make_ma(t, theta), "t must be greater than the length of theta")
})

test_that("TEST make_ma: conditions on sigma", {
  theta <- 2
  t <- 3

  sigma <- c(1, 2)
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- list(1, 2)
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- list(1)
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- data.frame(c(1))
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- c()
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- NULL
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- list()
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- numeric(0)
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- list(1)
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- data.frame(c(1))
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be a value of length 1")

  sigma <- Inf
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be finite")

  sigma <- -Inf
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be finite")

  sigma <- NA
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must not be NA")

  sigma <- "1"
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be numeric")

  sigma <- T
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be numeric")

  sigma <- -1
  expect_error(make_ma(t, theta, sigma = sigma), "sigma must be positive")
})

test_that("TEST make_ma: correctness", {
  # MA(1)
  set.seed(123)
  t <- 3
  theta <- 1
  sigma <- 1
  werte <- rnorm(12, mean = 0, sd = 1)
  set.seed(123)

  sol <- make_ma(t, theta, sigma)

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

  sol <- make_ma(t, theta, sigma)

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

  sol <- make_ma(t, theta, sigma)

  expect_equal(sol[1], sum(werte[1:4]))
  expect_equal(sol[2], sum(werte[2:5]))
  expect_equal(sol[3], sum(werte[3:6]))
  expect_equal(sol[4], sum(werte[4:7]))
})
