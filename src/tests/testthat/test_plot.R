# TESTING plot
## Test conditions on X

test_that("TEST plot: conditions on X", {
  samples <- 100

  X <- list(1, 2, 3, 4)
  expect_error(plot(X, samples), "X must be an atomic vector")

  X <- numeric(0)
  expect_error(plot(X, samples), "X must have positive length")

  X <- c(1, 2, NA, 3, 4)
  expect_error(plot(X, samples), "X may not contain NAs")

  X <- c(1, 2, Inf, 3, 4)
  expect_error(plot(X, samples), "X may not contain Inf or -Inf values")

  X <- c("s", "s", "t", "s")
  expect_error(plot(X, samples), "X must be numeric or complex")

  samples <- "s"
  X <- c(1, 2, 3, 4)
  expect_error(plot(X, samples), "samples must be numeric")

  samples <- 3.3
  X <- c(1, 2, 3, 4)
  expect_error(plot(X, samples), "samples must be an integer")
})
