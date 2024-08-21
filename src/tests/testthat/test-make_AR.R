# TESTING make_AR

test_that("make_AR: t must be a value of length 1", {
  phi <- 1:10

  t <- NULL
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- logical(0)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- integer(0)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- double(0)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- numeric(0)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- complex(0)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- character(0)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- raw(0)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- c()
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- c(1, 2)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- c(1, 2, 3)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- seq(from = 0, to = 1, by = 0.1)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- 1:10
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- list()
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- list(1)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- list(1, 2, 3)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- c(TRUE, FALSE)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- c("a", "b", "c")
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- matrix(1:10, 5, 2)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- raw(2)
  expect_error(make_AR(t, phi), "t must be a value of length 1")

  t <- data.frame(c(1))
  expect_error(make_AR(t, phi), "t must be a value of length 1")
})

test_that("make_AR: t must not be infinite", {
  phi <- 1:10

  t <- Inf
  expect_error(make_AR(t, phi), "t must not be infinite")

  t <- -Inf
  expect_error(make_AR(t, phi), "t must not be infinite")
})

test_that("make_AR: t must not be NA", {
  phi <- 1:10

  t <- NA
  expect_error(make_AR(t, phi), "t must not be NA")

  t <- NaN
  expect_error(make_AR(t, phi), "t must not be NA")

  t <- NA_integer_
  expect_error(make_AR(t, phi), "t must not be NA")

  t <- NA_real_
  expect_error(make_AR(t, phi), "t must not be NA")

  t <- NA_complex_
  expect_error(make_AR(t, phi), "t must not be NA")

  t <- NA_character_
  expect_error(make_AR(t, phi), "t must not be NA")
})

test_that("make_AR: t must be numeric", {
  phi <- 1:10

  t <- TRUE
  expect_error(make_AR(t, phi), "t must be numeric")

  t <- FALSE
  expect_error(make_AR(t, phi), "t must be numeric")

  t <- ""
  expect_error(make_AR(t, phi), "t must be numeric")

  t <- "pi"
  expect_error(make_AR(t, phi), "t must be numeric")

  t <- "42"
  expect_error(make_AR(t, phi), "t must be numeric")

  t <- "3.2"
  expect_error(make_AR(t, phi), "t must be numeric")

  t <- 0i
  expect_error(make_AR(t, phi), "t must be numeric")

  t <- 1i
  expect_error(make_AR(t, phi), "t must be numeric")

  t <- 2 + 5i
  expect_error(make_AR(t, phi), "t must be numeric")

  t <- raw(1)
  expect_error(make_AR(t, phi), "t must be numeric")
})

test_that("make_AR: t must be an integer", {
  phi <- 1:10

  t <- 3.2
  expect_error(make_AR(t, phi), "t must be an integer")

  t <- 10^-6
  expect_error(make_AR(t, phi), "t must be an integer")

  t <- pi
  expect_error(make_AR(t, phi), "t must be an integer")

  t <- .Machine$double.eps
  expect_error(make_AR(t, phi), "t must be an integer")

  t <- 0.1
  expect_error(make_AR(t, phi), "t must be an integer")
})

test_that("make_AR: t must be positive", {
  phi <- 1:10

  t <- -2
  expect_error(make_AR(t, phi), "t must be positive")

  t <- -1
  expect_error(make_AR(t, phi), "t must be positive")

  t <- 0
  expect_error(make_AR(t, phi), "t must be positive")

  t <- -2L
  expect_error(make_AR(t, phi), "t must be positive")

  t <- -1L
  expect_error(make_AR(t, phi), "t must be positive")

  t <- 0L
  expect_error(make_AR(t, phi), "t must be positive")
})


test_that("make_AR: phi must be an atomic vector", {
  t <- 10

  phi <- NULL
  expect_error(make_AR(t, phi), "phi must be an atomic vector")

  phi <- list(1, 2, 3, 4, 5)
  expect_error(make_AR(t, phi), "phi must be an atomic vector")

  phi <- c(list(1, 2, 3, 4, 5))
  expect_error(make_AR(t, phi), "phi must be an atomic vector")

  phi <- data.frame(c(1))
  expect_error(make_AR(t, phi), "phi must be an atomic vector")

  phi <- c()
  expect_error(make_AR(t, phi), "phi must be an atomic vector")
})

test_that("make_AR: phi must have positive length", {
  t <- 10

  phi <- logical(0)
  expect_error(make_AR(t, phi), "phi must have positive length")

  phi <- integer(0)
  expect_error(make_AR(t, phi), "phi must have positive length")

  phi <- double(0)
  expect_error(make_AR(t, phi), "phi must have positive length")

  phi <- numeric(0)
  expect_error(make_AR(t, phi), "phi must have positive length")

  phi <- complex(0)
  expect_error(make_AR(t, phi), "phi must have positive length")

  phi <- character(0)
  expect_error(make_AR(t, phi), "phi must have positive length")

  phi <- raw(0)
  expect_error(make_AR(t, phi), "phi must have positive length")
})

test_that("make_AR: phi may not contain NAs", {
  t <- 10

  phi <- NA
  expect_error(make_AR(t, phi), "phi may not contain NAs")

  phi <- NaN
  expect_error(make_AR(t, phi), "phi may not contain NAs")

  phi <- NA_integer_
  expect_error(make_AR(t, phi), "phi may not contain NAs")

  phi <- NA_real_
  expect_error(make_AR(t, phi), "phi may not contain NAs")

  phi <- NA_complex_
  expect_error(make_AR(t, phi), "phi may not contain NAs")

  phi <- NA_character_
  expect_error(make_AR(t, phi), "phi may not contain NAs")

  phi <- c(1, 2, NA, 4, 5)
  expect_error(make_AR(t, phi), "phi may not contain NAs")

  phi <- c(1, NA, NaN, NA_real_, 5)
  expect_error(make_AR(t, phi), "phi may not contain NAs")

  phi <- c(NULL, NA, Inf)
  expect_error(make_AR(t, phi), "phi may not contain NAs")
})

test_that("make_AR: phi may not contain Inf or -Inf values", {
  t <- 10

  phi <- Inf
  expect_error(make_AR(t, phi), "phi may not contain Inf or -Inf values")

  phi <- -Inf
  expect_error(make_AR(t, phi), "phi may not contain Inf or -Inf values")

  phi <- c(1, Inf, -Inf, 4, 5)
  expect_error(make_AR(t, phi), "phi may not contain Inf or -Inf values")

  phi <- c(NULL, Inf, 3, 4, 5)
  expect_error(make_AR(t, phi), "phi may not contain Inf or -Inf values")

  phi <- c(1, NULL, -Inf, 4, 5)
  expect_error(make_AR(t, phi), "phi may not contain Inf or -Inf values")
})

test_that("make_AR: The values of phi must be numeric or complex", {
  t <- 10

  phi <- c(TRUE, FALSE)
  expect_error(make_AR(t, phi), "The values of phi must be numeric or complex")

  phi <- ""
  expect_error(make_AR(t, phi), "The values of phi must be numeric or complex")

  phi <- "asdf"
  expect_error(make_AR(t, phi), "The values of phi must be numeric or complex")

  phi <- "42"
  expect_error(make_AR(t, phi), "The values of phi must be numeric or complex")

  phi <- c("a", "b", "c")
  expect_error(make_AR(t, phi), "The values of phi must be numeric or complex")

  phi <- c(1L, 2, TRUE, "42", 5)
  expect_error(make_AR(t, phi), "The values of phi must be numeric or complex")

  phi <- c(-pi, -0.1, "0", 1, 2)
  expect_error(make_AR(t, phi), "The values of phi must be numeric or complex")

  phi <- c(1:100, "101")
  expect_error(make_AR(t, phi), "The values of phi must be numeric or complex")

  phi <- raw(1)
  expect_error(make_AR(t, phi), "The values of phi must be numeric or complex")
})

test_that("make_AR: t must be greater than the length of phi", {
  t <- 5
  phi <- c(1, 2, 3, 4, 5)
  expect_error(make_AR(t, phi), "t must be greater than the length of phi")

  t <- 4
  phi <- c(12, -2, 31, 124, -10)
  expect_error(make_AR(t, phi), "t must be greater than the length of phi")

  t <- 2
  phi <- c(5, 6)
  expect_error(make_AR(t, phi), "t must be greater than the length of phi")

  t <- 1
  phi <- c(1, 2)
  expect_error(make_AR(t, phi), "t must be greater than the length of phi")

  t <- 2
  phi <- c(-3 + 1i, 3 + 2i)
  expect_error(make_AR(t, phi), "t must be greater than the length of phi")

  t <- 1
  phi <- c(1 + 1i, 2i)
  expect_error(make_AR(t, phi), "t must be greater than the length of phi")
})

test_that("The polynomial phi must have no roots on the unit circle", {
  t <- 3
  phi <- 1
  expect_error(make_AR(t, phi), "The polynomial phi must have no roots on the unit circle")

  phi <- -1
  expect_error(make_AR(t, phi), "The polynomial phi must have no roots on the unit circle")

  phi <- matrix(c(1))
  expect_error(make_AR(t, phi), "The polynomial phi must have no roots on the unit circle")

  phi <- c(-1, 2)
  expect_error(make_AR(t, phi), "The polynomial phi must have no roots on the unit circle")
})

test_that("make_AR: start must be an atomic vector", {
  t <- 10
  phi <- c(0.5, -0.3)

  start <- NULL
  expect_error(make_AR(t, phi, start = start), "start must be an atomic vector")

  start <- list(1, 2, 3)
  expect_error(make_AR(t, phi, start = start), "start must be an atomic vector")

  start <- data.frame(1:3)
  expect_error(make_AR(t, phi, start = start), "start must be an atomic vector")
})

test_that("make_AR: start must have positive length", {
  t <- 10
  phi <- c(0.5, -0.3)

  start <- integer(0)
  expect_error(make_AR(t, phi, start = start), "start must have positive length")

  start <- numeric(0)
  expect_error(make_AR(t, phi, start = start), "start must have positive length")

  start <- logical(0)
  expect_error(make_AR(t, phi, start = start), "start must have positive length")
})

test_that("make_AR: length of start must be smaller than or equal to length of phi", {
  t <- 10
  phi <- c(0.5, -0.3)

  start <- c(1, 2, 3)
  expect_error(make_AR(t, phi, start = start), "length of start must be smaller than or equal to length of phi")

  start <- c(1, 2, 3, 4)
  expect_error(make_AR(t, phi, start = start), "length of start must be smaller than or equal to length of phi")
})

test_that("make_AR: start may not contain NAs", {
  t <- 10
  phi <- c(0.5, -0.3)

  start <- c(NA, 2)
  expect_error(make_AR(t, phi, start = start), "start may not contain NAs")

  start <- c(1, NA_real_)
  expect_error(make_AR(t, phi, start = start), "start may not contain NAs")

  start <- c(NaN, 2)
  expect_error(make_AR(t, phi, start = start), "start may not contain NAs")
})

test_that("make_AR: start may not contain Inf or -Inf values", {
  t <- 10
  phi <- c(0.5, -0.3)

  start <- c(Inf, 2)
  expect_error(make_AR(t, phi, start = start), "start may not contain Inf or -Inf values")

  start <- c(1, -Inf)
  expect_error(make_AR(t, phi, start = start), "start may not contain Inf or -Inf values")
})

test_that("make_AR: The values of start must be numeric or complex", {
  t <- 10
  phi <- c(0.5, -0.3)

  start <- c("a", "b")
  expect_error(make_AR(t, phi, start = start), "The values of start must be numeric or complex")

  start <- c(TRUE, FALSE)
  expect_error(make_AR(t, phi, start = start), "The values of start must be numeric or complex")

  start <- c(1L, "2")
  expect_error(make_AR(t, phi, start = start), "The values of start must be numeric or complex")
})

test_that("make_AR: sigma must be a value of length 1", {
  phi <- 2
  t <- 3

  sigma <- NULL
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- logical(0)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- integer(0)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- double(0)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- numeric(0)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- complex(0)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- character(0)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- raw(0)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- c()
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- c(1, 2)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- c(1, 2, 3)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- seq(from = 0, to = 1, by = 0.1)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- 1:10
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- list()
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- list(1)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- list(1, 2, 3)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- c(TRUE, FALSE)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- c("a", "b", "c")
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- matrix(1:10, 5, 2)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- raw(2)
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- data.frame(c(1))
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")

  sigma <- data.frame(c(1))
  expect_error(make_AR(t, phi, sigma), "sigma must be a value of length 1")
})

test_that("make_AR: sigma must not be infinite", {
  phi <- 2
  t <- 3

  sigma <- Inf
  expect_error(make_AR(t, phi, sigma), "sigma must not be infinite")

  sigma <- -Inf
  expect_error(make_AR(t, phi, sigma), "sigma must not be infinite")
})

test_that("make_AR: sigma must not be NA", {
  phi <- 2
  t <- 3

  sigma <- NA
  expect_error(make_AR(t, phi, sigma), "sigma must not be NA")

  sigma <- NaN
  expect_error(make_AR(t, phi, sigma), "sigma must not be NA")

  sigma <- NA_integer_
  expect_error(make_AR(t, phi, sigma), "sigma must not be NA")

  sigma <- NA_real_
  expect_error(make_AR(t, phi, sigma), "sigma must not be NA")

  sigma <- NA_complex_
  expect_error(make_AR(t, phi, sigma), "sigma must not be NA")

  sigma <- NA_character_
  expect_error(make_AR(t, phi, sigma), "sigma must not be NA")
})

test_that("make_AR: sigma must be numeric", {
  phi <- 2
  t <- 3

  sigma <- TRUE
  expect_error(make_AR(t, phi, sigma), "sigma must be numeric")

  sigma <- FALSE
  expect_error(make_AR(t, phi, sigma), "sigma must be numeric")

  sigma <- ""
  expect_error(make_AR(t, phi, sigma), "sigma must be numeric")

  sigma <- "pi"
  expect_error(make_AR(t, phi, sigma), "sigma must be numeric")

  sigma <- "42"
  expect_error(make_AR(t, phi, sigma), "sigma must be numeric")

  sigma <- "3.2"
  expect_error(make_AR(t, phi, sigma), "sigma must be numeric")

  sigma <- 0i
  expect_error(make_AR(t, phi, sigma), "sigma must be numeric")

  sigma <- 1i
  expect_error(make_AR(t, phi, sigma), "sigma must be numeric")

  sigma <- 2 + 5i
  expect_error(make_AR(t, phi, sigma), "sigma must be numeric")

  sigma <- raw(1)
  expect_error(make_AR(t, phi, sigma), "sigma must be numeric")
})

test_that("make_AR: sigma must be positive", {
  phi <- 2
  t <- 3

  sigma <- -2
  expect_error(make_AR(t, phi, sigma), "sigma must be positive")

  sigma <- -1
  expect_error(make_AR(t, phi, sigma), "sigma must be positive")

  sigma <- 0
  expect_error(make_AR(t, phi, sigma), "sigma must be positive")

  sigma <- -2L
  expect_error(make_AR(t, phi, sigma), "sigma must be positive")

  sigma <- -1L
  expect_error(make_AR(t, phi, sigma), "sigma must be positive")

  sigma <- 0L
  expect_error(make_AR(t, phi, sigma), "sigma must be positive")
})

test_that("TEST make_ar: correctness", {
  # AR(1)
  set.seed(123)
  t <- 3
  theta <- 2
  sigma <- 1
  start <- 1
  werte <- rnorm(12, mean = 0, sd = sigma)
  set.seed(123)

  sol <- make_AR(t, theta, sigma, start)

  expect_equal(sol[1], start)
  expect_equal(sol[2], 2 * start + werte[2])
  expect_equal(sol[3], 4 * start + 2 * werte[2] + werte[3])

  # AR(2)
  set.seed(123)
  t <- 4
  theta <- c(1, 1)
  sigma <- 1
  start <- 1
  werte <- rnorm(12, mean = 0, sd = sigma)
  set.seed(123)

  sol <- make_AR(t, theta, sigma, start)

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
  werte <- rnorm(12, mean = 0, sd = sigma)
  set.seed(123)

  sol <- make_AR(t, theta, sigma, start)

  expect_equal(sol[1], 1)
  expect_equal(sol[2], 0)
  expect_equal(sol[3], 0)
  expect_equal(sol[4], 1 + werte[4])
  expect_equal(sol[5], 1 + werte[4] + werte[5])
})
