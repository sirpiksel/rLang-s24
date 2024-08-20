# Test conditions for X

test_that("TEST DLA: conditions on X", {
  X <- NULL
  expect_error(DLA(X), "The values of X must be numeric or complex")

  X <- c()
  expect_error(DLA(X), "The values of X must be numeric or complex")

  x <- numeric(0)
  expect_error(DLA(X), "The values of X must be numeric or complex")

  X <- list(1, 2, 3, 4)
  expect_error(DLA(X), "The values of X must be numeric or complex")

  X <- c("s", "s", "t", "s")
  expect_error(DLA(X), "The values of X must be numeric or complex")

  X <- c(T, F, F, T)
  expect_error(DLA(X), "The values of X must be numeric or complex")

  X <- data.frame(1:4)
  expect_error(DLA(X), "The values of X must be numeric or complex")

  X <- data.frame(list(1:4))
  expect_error(DLA(X), "The values of X must be numeric or complex")

  X <- matrix(list(1:4))
  expect_error(DLA(X), "The values of X must be numeric or complex")

  X <- c(NA, 2)
  expect_error(DLA(X), "X may not contain NAs")

  X <- c(Inf, 2)
  expect_error(DLA(X), "X may not contain Inf or -Inf values")

  X <- c(2, -Inf)
  expect_error(DLA(X), "X may not contain Inf or -Inf values")
})



# Test conditions for m

test_that("TEST DLA: conditions on m", {
  set.seed(1)
  X <- rnorm(4,mean = 0, sd = 1)
  pdl <- DLA(X)

  m <- c(1, 2)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- matrix(1:4)
  expect_error(pdl(m), "m must be a value of length 1")

  m <- list(1:4)
  expect_error(pdl(m), "m must be numeric")

  m <- Inf
  expect_error(pdl(m), "m must not be infinite")

  m <- -Inf
  expect_error(pdl(m), "m must not be infinite")

  m <- NA_integer_
  expect_error(pdl(m), "m must not be NA")

  m <- NA_real_
  expect_error(pdl(m), "m must not be NA")

  m <- NA
  expect_error(pdl(m), "m must be numeric")

  m <- data.frame(c(1))
  expect_error(pdl(m), "m must be numeric")

  m <- list(2)
  expect_error(pdl(m), "m must be numeric")

  m <- "2"
  expect_error(pdl(m), "m must be numeric")

  m <- T
  expect_error(pdl(m), "m must be numeric")

  m <- 1 + 2i
  expect_error(pdl(m), "m must be numeric")

  m <- 2.3
  expect_error(pdl(m), "m must be an integer")

  m <- -1
  expect_error(pdl(m), "m must be between 0 and length of X")

  m <- 0
  expect_error(pdl(m), "m must be between 0 and length of X")

  m <- -1
  expect_error(pdl(m), "m must be between 0 and length of X")

  m <- 4
  expect_error(pdl(m), "m must be between 0 and length of X")

  m <- 5
  expect_error(pdl(m), "m must be between 0 and length of X")

  m <- numeric(0)
  expect_error(pdl(m), "m must be a value of length 1")
})

# Test on correctness

test_that("TEST DLA: correctness", {
  set.seed(2)
  X <- rnorm(5,mean = 0, sd = 1)
  pdl <- DLA(X)

  result <- pdl(2)
  expect_equal(sin(pi),0)
  expect_equal(result$v[1], 0.9243285, tolerance = 1e-6)
  expect_equal(result$v[2], 0.8219783, tolerance = 1e-6)
  expect_equal(result$phi[1], -0.5088541, tolerance = 1e-6)
  expect_equal(result$phi[2], -0.5291920, tolerance = 1e-6)
})

# Test for the behavior of m at extreme values

test_that("Test on return", {
  # set.seed(3)
  X <- rnorm(5,mean = 0, sd = 1)
  pdl <- DLA(X)

  m <- 1
  result <- pdl(m)
  expect_true(is.list(result))
  expect_true(all(names(result) %in% c("phi", "v")))
  expect_equal(length(result$phi), 1)
  expect_equal(length(result$v), 1)
})
