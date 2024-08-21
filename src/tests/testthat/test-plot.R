# TESTING plot
## Test conditions on X

test_that("plot: conditions on X", {
  n <- 100

  X <- list(1, 2, 3, 4)
  expect_error(plot(X, n), "X must be an atomic vector")

  X <- numeric(0)
  expect_error(plot(X, n), "X must have positive length")

  X <- c(1, 2, NA, 3, 4)
  expect_error(plot(X, n), "X may not contain NAs")

  X <- c(1, 2, Inf, 3, 4)
  expect_error(plot(X, n), "X may not contain Inf or -Inf values")

  X <- c("s", "s", "t", "s")
  expect_error(plot(X, n), "X must be numeric or complex")

  n <- "s"
  X <- c(1, 2, 3, 4)
  expect_error(plot(X, n), "n must be numeric")

  n <- 3.3
  X <- c(1, 2, 3, 4)
  expect_error(plot(X, n), "n must be an integer")
})

test_that("plot: X must be an atomic vector", {
  n <- c(1, 2)

  X <- NULL
  expect_error(plot(X, n), "X must be an atomic vector")

  X <- list(1, 2, 3, 4, 5)
  expect_error(plot(X, n), "X must be an atomic vector")

  X <- c(list(1, 2, 3, 4, 5))
  expect_error(plot(X, n), "X must be an atomic vector")

  X <- data.frame(c(1))
  expect_error(plot(X, n), "X must be an atomic vector")

  X <- c()
  expect_error(plot(X, n), "X must be an atomic vector")
})

test_that("plot: X must have positive length", {
  n <- c(1, 2)

  X <- logical(0)
  expect_error(plot(X, n), "X must have positive length")

  X <- integer(0)
  expect_error(plot(X, n), "X must have positive length")

  X <- double(0)
  expect_error(plot(X, n), "X must have positive length")

  X <- numeric(0)
  expect_error(plot(X, n), "X must have positive length")

  X <- complex(0)
  expect_error(plot(X, n), "X must have positive length")

  X <- character(0)
  expect_error(plot(X, n), "X must have positive length")

  X <- raw(0)
  expect_error(plot(X, n), "X must have positive length")
})

test_that("plot: X may not contain NAs", {
  n <- c(1, 2)

  X <- NA
  expect_error(plot(X, n), "X may not contain NAs")

  X <- NaN
  expect_error(plot(X, n), "X may not contain NAs")

  X <- NA_integer_
  expect_error(plot(X, n), "X may not contain NAs")

  X <- NA_real_
  expect_error(plot(X, n), "X may not contain NAs")

  X <- NA_complex_
  expect_error(plot(X, n), "X may not contain NAs")

  X <- NA_character_
  expect_error(plot(X, n), "X may not contain NAs")

  X <- c(1, 2, NA, 4, 5)
  expect_error(plot(X, n), "X may not contain NAs")

  X <- c(1, NA, NaN, NA_real_, 5)
  expect_error(plot(X, n), "X may not contain NAs")

  X <- c(NULL, NA, Inf)
  expect_error(plot(X, n), "X may not contain NAs")
})

test_that("plot: X may not contain Inf or -Inf values", {
  n <- c(1, 2)

  X <- Inf
  expect_error(plot(X, n), "X may not contain Inf or -Inf values")

  X <- -Inf
  expect_error(plot(X, n), "X may not contain Inf or -Inf values")

  X <- c(1, Inf, -Inf, 4, 5)
  expect_error(plot(X, n), "X may not contain Inf or -Inf values")

  X <- c(NULL, Inf, 3, 4, 5)
  expect_error(plot(X, n), "X may not contain Inf or -Inf values")

  X <- c(1, NULL, -Inf, 4, 5)
  expect_error(plot(X, n), "X may not contain Inf or -Inf values")
})

test_that("plot: X must be numeric or complex", {
  n <- c(1, 2)

  X <- c(TRUE, FALSE)
  expect_error(plot(X, n), "X must be numeric or complex")

  X <- ""
  expect_error(plot(X, n), "X must be numeric or complex")

  X <- "asdf"
  expect_error(plot(X, n), "X must be numeric or complex")

  X <- "42"
  expect_error(plot(X, n), "X must be numeric or complex")

  X <- c("a", "b", "c")
  expect_error(plot(X, n), "X must be numeric or complex")

  X <- c(1L, 2, TRUE, "42", 5)
  expect_error(plot(X, n), "X must be numeric or complex")

  X <- c(-pi, -0.1, "0", 1, 2)
  expect_error(plot(X, n), "X must be numeric or complex")

  X <- c(1:100, "101")
  expect_error(plot(X, n), "X must be numeric or complex")

  X <- raw(1)
  expect_error(plot(X, n), "X must be numeric or complex")
})

test_that("plot: n must be numeric", {
  X <- 1:10

  n <- TRUE
  expect_error(plot(X, n), "n must be numeric")

  n <- FALSE
  expect_error(plot(X, n), "n must be numeric")

  n <- ""
  expect_error(plot(X, n), "n must be numeric")

  n <- "pi"
  expect_error(plot(X, n), "n must be numeric")

  n <- "42"
  expect_error(plot(X, n), "n must be numeric")

  n <- "3.2"
  expect_error(plot(X, n), "n must be numeric")

  n <- 0i
  expect_error(plot(X, n), "n must be numeric")

  n <- 1i
  expect_error(plot(X, n), "n must be numeric")

  n <- 2 + 5i
  expect_error(plot(X, n), "n must be numeric")

  n <- raw(1)
  expect_error(plot(X, n), "n must be numeric")

  n <- NA
  expect_error(plot(X, n), "n must be numeric")

  n <- NA_complex_
  expect_error(plot(X, n), "n must be numeric")

  n <- NA_character_
  expect_error(plot(X, n), "n must be numeric")
})

test_that("plot: n must be an integer", {
  X <- 1:10

  n <- 3.2
  expect_error(plot(X, n), "n must be an integer")

  n <- 10^-6
  expect_error(plot(X, n), "n must be an integer")

  n <- pi
  expect_error(plot(X, n), "n must be an integer")

  n <- .Machine$double.eps
  expect_error(plot(X, n), "n must be an integer")

  n <- 0.1
  expect_error(plot(X, n), "n must be an integer")
})

test_that("plot: 'from' must be numeric", {
  X <- 1:10
  n <- 10

  from <- "s"
  expect_error(plot(X, n, from = from), "from must be numeric")

  from <- TRUE
  expect_error(plot(X, n, from = from), "from must be numeric")
})

test_that("plot: 'from' may not be NA", {
  X <- 1:10
  n <- 10

  from <- NA
  expect_error(plot(X, n, from = from), "from may not be NA")

  from <- NA_complex_
  expect_error(plot(X, n, from = from), "from may not be NA")

  from <- NA_character_
  expect_error(plot(X, n, from = from), "from may not be NA")
})

test_that("plot: 'from' may not be an Inf or -Inf value", {
  X <- 1:10
  n <- 10

  from <- Inf
  expect_error(plot(X, n, from = from), "from may not be an Inf or -Inf value")

  from <- -Inf
  expect_error(plot(X, n, from = from), "from may not be an Inf or -Inf value")
})

test_that("plot: 'from' must be greater or equal to 0", {
  X <- 1:10
  n <- 10

  from <- -1
  expect_error(plot(X, n, from = from), "from must be greater or equal to 0")
})

test_that("plot: 'from' must be smaller than 'to'", {
  X <- 1:10
  n <- 10

  from <- pi
  to <- pi
  expect_error(plot(X, n, from = from, to = to), "from must be smaller than to")

  from <- pi + 1
  to <- pi
  expect_error(plot(X, n, from = from, to = to), "from must be smaller than to")
})

test_that("plot: 'to' must be numeric", {
  X <- 1:10
  n <- 10

  to <- "s"
  expect_error(plot(X, n, to = to), "to must be numeric")

  to <- TRUE
  expect_error(plot(X, n, to = to), "to must be numeric")
})

test_that("plot: 'to' may not be NA", {
  X <- 1:10
  n <- 10

  to <- NA
  expect_error(plot(X, n, to = to), "to may not be NA")

  to <- NA_complex_
  expect_error(plot(X, n, to = to), "to may not be NA")

  to <- NA_character_
  expect_error(plot(X, n, to = to), "to may not be NA")
})

test_that("plot: 'to' may not be an Inf or -Inf value", {
  X <- 1:10
  n <- 10

  to <- Inf
  expect_error(plot(X, n, to = to), "to may not be an Inf or -Inf value")

  to <- -Inf
  expect_error(plot(X, n, to = to), "to may not be an Inf or -Inf value")
})

test_that("plot: 'to' must be smaller or equal to pi", {
  X <- 1:10
  n <- 10

  to <- pi + 1
  expect_error(plot(X, n, to = to), "to must be smaller or equal to pi")
})
