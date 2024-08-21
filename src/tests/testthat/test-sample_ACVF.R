# TESTING sample_ACVF

test_that("sample_ACVF: X must be an atomic vector", {
  X <- NULL
  expect_error(sample_ACVF(X, h), "X must be an atomic vector")

  X <- list(1, 2, 3, 4, 5)
  expect_error(sample_ACVF(X, h), "X must be an atomic vector")

  X <- c(list(1, 2, 3, 4, 5))
  expect_error(sample_ACVF(X, h), "X must be an atomic vector")

  X <- data.frame(c(1))
  expect_error(sample_ACVF(X, h), "X must be an atomic vector")

  X <- c()
  expect_error(sample_ACVF(X, h), "X must be an atomic vector")
})

test_that("sample_ACVF: X must have positive length", {
  X <- logical(0)
  expect_error(sample_ACVF(X, h), "X must have positive length")

  X <- integer(0)
  expect_error(sample_ACVF(X, h), "X must have positive length")

  X <- double(0)
  expect_error(sample_ACVF(X, h), "X must have positive length")

  X <- numeric(0)
  expect_error(sample_ACVF(X, h), "X must have positive length")

  X <- complex(0)
  expect_error(sample_ACVF(X, h), "X must have positive length")

  X <- character(0)
  expect_error(sample_ACVF(X, h), "X must have positive length")

  X <- raw(0)
  expect_error(sample_ACVF(X, h), "X must have positive length")
})

test_that("sample_ACVF: X may not contain NAs", {
  X <- NA
  expect_error(sample_ACVF(X, h), "X may not contain NAs")

  X <- NaN
  expect_error(sample_ACVF(X, h), "X may not contain NAs")

  X <- NA_integer_
  expect_error(sample_ACVF(X, h), "X may not contain NAs")

  X <- NA_real_
  expect_error(sample_ACVF(X, h), "X may not contain NAs")

  X <- NA_complex_
  expect_error(sample_ACVF(X, h), "X may not contain NAs")

  X <- NA_character_
  expect_error(sample_ACVF(X, h), "X may not contain NAs")

  X <- c(1, 2, NA, 4, 5)
  expect_error(sample_ACVF(X, h), "X may not contain NAs")

  X <- c(1, NA, NaN, NA_real_, 5)
  expect_error(sample_ACVF(X, h), "X may not contain NAs")

  X <- c(NULL, NA, Inf)
  expect_error(sample_ACVF(X, h), "X may not contain NAs")
})

test_that("sample_ACVF: X may not contain Inf or -Inf values", {
  X <- Inf
  expect_error(sample_ACVF(X, h), "X may not contain Inf or -Inf values")

  X <- -Inf
  expect_error(sample_ACVF(X, h), "X may not contain Inf or -Inf values")

  X <- c(1, Inf, -Inf, 4, 5)
  expect_error(sample_ACVF(X, h), "X may not contain Inf or -Inf values")

  X <- c(NULL, Inf, 3, 4, 5)
  expect_error(sample_ACVF(X, h), "X may not contain Inf or -Inf values")

  X <- c(1, NULL, -Inf, 4, 5)
  expect_error(sample_ACVF(X, h), "X may not contain Inf or -Inf values")
})

test_that("sample_ACVF: The values of X must be numeric or complex", {
  h <- c(1, 2)

  X <- c(TRUE, FALSE)
  expect_error(sample_ACVF(X, h), "The values of X must be numeric or complex")

  X <- ""
  expect_error(sample_ACVF(X, h), "The values of X must be numeric or complex")

  X <- "asdf"
  expect_error(sample_ACVF(X, h), "The values of X must be numeric or complex")

  X <- "42"
  expect_error(sample_ACVF(X, h), "The values of X must be numeric or complex")

  X <- c("a", "b", "c")
  expect_error(sample_ACVF(X, h), "The values of X must be numeric or complex")

  X <- c(1L, 2, TRUE, "42", 5)
  expect_error(sample_ACVF(X, h), "The values of X must be numeric or complex")

  X <- c(-pi, -0.1, "0", 1, 2)
  expect_error(sample_ACVF(X, h), "The values of X must be numeric or complex")

  X <- c(1:100, "101")
  expect_error(sample_ACVF(X, h), "The values of X must be numeric or complex")

  X <- raw(1)
  expect_error(sample_ACVF(X, h), "The values of X must be numeric or complex")
})

test_that("sample_ACVF: h must be numeric", {
  X <- 1:10

  h <- TRUE
  expect_error(sample_ACVF(X, h), "h must be numeric")

  h <- FALSE
  expect_error(sample_ACVF(X, h), "h must be numeric")

  h <- ""
  expect_error(sample_ACVF(X, h), "h must be numeric")

  h <- "pi"
  expect_error(sample_ACVF(X, h), "h must be numeric")

  h <- "42"
  expect_error(sample_ACVF(X, h), "h must be numeric")

  h <- "3.2"
  expect_error(sample_ACVF(X, h), "h must be numeric")

  h <- 0i
  expect_error(sample_ACVF(X, h), "h must be numeric")

  h <- 1i
  expect_error(sample_ACVF(X, h), "h must be numeric")

  h <- 2 + 5i
  expect_error(sample_ACVF(X, h), "h must be numeric")

  h <- raw(1)
  expect_error(sample_ACVF(X, h), "h must be numeric")

  h <- NA
  expect_error(sample_ACVF(X, h), "h must be numeric")

  h <- NA_complex_
  expect_error(sample_ACVF(X, h), "h must be numeric")

  h <- NA_character_
  expect_error(sample_ACVF(X, h), "h must be numeric")
})

test_that("sample_ACVF: h may not contain NAs", {
  X <- 1:10

  h <- NaN
  expect_error(sample_ACVF(X, h), "h may not contain NAs")

  h <- NA_integer_
  expect_error(sample_ACVF(X, h), "h may not contain NAs")

  h <- NA_real_
  expect_error(sample_ACVF(X, h), "h may not contain NAs")

  h <- c(1, 2, NA, 4, 5)
  expect_error(sample_ACVF(X, h), "h may not contain NAs")

  h <- c(1, NA, NaN, NA_real_, 5)
  expect_error(sample_ACVF(X, h), "h may not contain NAs")

  h <- c(NULL, NA, Inf)
  expect_error(sample_ACVF(X, h), "h may not contain NAs")
})

test_that("sample_ACVF: h may not contain Inf or -Inf values", {
  X <- 1:10

  h <- Inf
  expect_error(sample_ACVF(X, h), "h may not contain Inf or -Inf values")

  h <- -Inf
  expect_error(sample_ACVF(X, h), "h may not contain Inf or -Inf values")

  h <- c(1, Inf, -Inf, 4, 5)
  expect_error(sample_ACVF(X, h), "h may not contain Inf or -Inf values")

  h <- c(NULL, Inf, 3, 4, 5)
  expect_error(sample_ACVF(X, h), "h may not contain Inf or -Inf values")

  h <- c(1, NULL, -Inf, 4, 5)
  expect_error(sample_ACVF(X, h), "h may not contain Inf or -Inf values")
})

test_that("sample_ACVF: h must be an integer vector", {
  X <- 1:10

  h <- 3.2
  expect_error(sample_ACVF(X, h), "h must be an integer")

  h <- 10^-6
  expect_error(sample_ACVF(X, h), "h must be an integer")

  h <- pi
  expect_error(sample_ACVF(X, h), "h must be an integer")

  h <- .Machine$double.eps
  expect_error(sample_ACVF(X, h), "h must be an integer")

  h <- 0.1
  expect_error(sample_ACVF(X, h), "h must be an integer")
})

test_that("sample_ACVF: The values of h must be unique", {
  X <- 1:10

  h <- rep(1, 100)
  expect_error(sample_ACVF(X, h), "The values of h must be unique")

  h <- c(-10:10, -10:10)
  expect_error(sample_ACVF(X, h), "The values of h must be unique")

  h <- c(13, 23, 43, 12, 15, 13, 75, 57)
  expect_error(sample_ACVF(X, h), "The values of h must be unique")

  h <- c(1, 2, 3, 4, 11, 1)
  expect_error(sample_ACVF(X, h), "The values of h must be unique")

  h <- c(1, 2, 2, 3, 3, 1, 4, 5, 4, 5)
  expect_error(sample_ACVF(X, h), "The values of h must be unique")

  h <- c(sample(-1:1, 100, replace = TRUE), -1:1)
  expect_error(sample_ACVF(X, h), "The values of h must be unique")
})

test_that("sample_ACVF: All values of h must be from the interval (-length(X), length(X))", {
  X <- 1:10

  h <- 10
  expect_error(sample_ACVF(X, h), "All values of h must be from the interval \\(-length\\(X\\), length\\(X\\)\\)")

  h <- -10
  expect_error(sample_ACVF(X, h), "All values of h must be from the interval \\(-length\\(X\\), length\\(X\\)\\)")

  h <- c(9, 6, 2, 13)
  expect_error(sample_ACVF(X, h), "All values of h must be from the interval \\(-length\\(X\\), length\\(X\\)\\)")

  h <- c(-3, -8, -2, 4, -61)
  expect_error(sample_ACVF(X, h), "All values of h must be from the interval \\(-length\\(X\\), length\\(X\\)\\)")

  h <- c(-9:9, 10)
  expect_error(sample_ACVF(X, h), "All values of h must be from the interval \\(-length\\(X\\), length\\(X\\)\\)")

  h <- c(10:20, -10:-20)
  expect_error(sample_ACVF(X, h), "All values of h must be from the interval \\(-length\\(X\\), length\\(X\\)\\)")
})

test_that("sample_ACVF: correctness", {
  X <- c(2, 4, 6, 8, 10)
  sample_ACVF(X)

  sample_ACVF <- sample_ACVF(X)

  sample_ACVF_neg <- sample_ACVF(X, 0:-4)

  acf_values <- acf(c(2, 4, 6, 8, 10),
    type = c("covariance"),
    plot = FALSE
  )$acf[, , 1]

  expect_true(all(acf_values == sample_ACVF))
  expect_true(all(sample_ACVF == sample_ACVF_neg))
})

# TESTING fabric_sample_ACVF

test_that("fabric_sample_ACVF: X must be an atomic vector", {
  X <- NULL
  expect_error(fabric_sample_ACVF(X), "X must be an atomic vector")

  X <- list(1, 2, 3, 4, 5)
  expect_error(fabric_sample_ACVF(X), "X must be an atomic vector")

  X <- c(list(1, 2, 3, 4, 5))
  expect_error(fabric_sample_ACVF(X), "X must be an atomic vector")

  X <- data.frame(c(1))
  expect_error(fabric_sample_ACVF(X), "X must be an atomic vector")

  X <- c()
  expect_error(fabric_sample_ACVF(X), "X must be an atomic vector")
})

test_that("fabric_sample_ACVF: X must have positive length", {
  X <- logical(0)
  expect_error(fabric_sample_ACVF(X), "X must have positive length")

  X <- integer(0)
  expect_error(fabric_sample_ACVF(X), "X must have positive length")

  X <- double(0)
  expect_error(fabric_sample_ACVF(X), "X must have positive length")

  X <- numeric(0)
  expect_error(fabric_sample_ACVF(X), "X must have positive length")

  X <- complex(0)
  expect_error(fabric_sample_ACVF(X), "X must have positive length")

  X <- character(0)
  expect_error(fabric_sample_ACVF(X), "X must have positive length")

  X <- raw(0)
  expect_error(fabric_sample_ACVF(X), "X must have positive length")
})

test_that("fabric_sample_ACVF: X may not contain NAs", {
  X <- NA
  expect_error(fabric_sample_ACVF(X), "X may not contain NAs")

  X <- NaN
  expect_error(fabric_sample_ACVF(X), "X may not contain NAs")

  X <- NA_integer_
  expect_error(fabric_sample_ACVF(X), "X may not contain NAs")

  X <- NA_real_
  expect_error(fabric_sample_ACVF(X), "X may not contain NAs")

  X <- NA_complex_
  expect_error(fabric_sample_ACVF(X), "X may not contain NAs")

  X <- NA_character_
  expect_error(fabric_sample_ACVF(X), "X may not contain NAs")

  X <- c(1, 2, NA, 4, 5)
  expect_error(fabric_sample_ACVF(X), "X may not contain NAs")

  X <- c(1, NA, NaN, NA_real_, 5)
  expect_error(fabric_sample_ACVF(X), "X may not contain NAs")

  X <- c(NULL, NA, Inf)
  expect_error(fabric_sample_ACVF(X), "X may not contain NAs")
})

test_that("fabric_sample_ACVF: X may not contain Inf or -Inf values", {
  X <- Inf
  expect_error(fabric_sample_ACVF(X), "X may not contain Inf or -Inf values")

  X <- -Inf
  expect_error(fabric_sample_ACVF(X), "X may not contain Inf or -Inf values")

  X <- c(1, Inf, -Inf, 4, 5)
  expect_error(fabric_sample_ACVF(X), "X may not contain Inf or -Inf values")

  X <- c(NULL, Inf, 3, 4, 5)
  expect_error(fabric_sample_ACVF(X), "X may not contain Inf or -Inf values")

  X <- c(1, NULL, -Inf, 4, 5)
  expect_error(fabric_sample_ACVF(X), "X may not contain Inf or -Inf values")
})

test_that("fabric_sample_ACVF: The values of X must be numeric or complex", {
  h <- c(1, 2)

  X <- c(TRUE, FALSE)
  expect_error(fabric_sample_ACVF(X), "The values of X must be numeric or complex")

  X <- ""
  expect_error(fabric_sample_ACVF(X), "The values of X must be numeric or complex")

  X <- "asdf"
  expect_error(fabric_sample_ACVF(X), "The values of X must be numeric or complex")

  X <- "42"
  expect_error(fabric_sample_ACVF(X), "The values of X must be numeric or complex")

  X <- c("a", "b", "c")
  expect_error(fabric_sample_ACVF(X), "The values of X must be numeric or complex")

  X <- c(1L, 2, TRUE, "42", 5)
  expect_error(fabric_sample_ACVF(X), "The values of X must be numeric or complex")

  X <- c(-pi, -0.1, "0", 1, 2)
  expect_error(fabric_sample_ACVF(X), "The values of X must be numeric or complex")

  X <- c(1:100, "101")
  expect_error(fabric_sample_ACVF(X), "The values of X must be numeric or complex")

  X <- raw(1)
  expect_error(fabric_sample_ACVF(X), "The values of X must be numeric or complex")
})

test_that("fabric_sample_ACVF: h must be a value of length 1", {
  X <- 1:10
  foo <- fabric_sample_ACVF(X)

  h <- NULL
  expect_error(foo(h), "h must be a value of length 1")

  h <- logical(0)
  expect_error(foo(h), "h must be a value of length 1")

  h <- integer(0)
  expect_error(foo(h), "h must be a value of length 1")

  h <- double(0)
  expect_error(foo(h), "h must be a value of length 1")

  h <- numeric(0)
  expect_error(foo(h), "h must be a value of length 1")

  h <- complex(0)
  expect_error(foo(h), "h must be a value of length 1")

  h <- character(0)
  expect_error(foo(h), "h must be a value of length 1")

  h <- raw(0)
  expect_error(foo(h), "h must be a value of length 1")

  h <- c()
  expect_error(foo(h), "h must be a value of length 1")

  h <- c(1, 2)
  expect_error(foo(h), "h must be a value of length 1")

  h <- c(1, 2, 3)
  expect_error(foo(h), "h must be a value of length 1")

  h <- seq(from = 0, to = 1, by = 0.1)
  expect_error(foo(h), "h must be a value of length 1")

  h <- 1:10
  expect_error(foo(h), "h must be a value of length 1")

  h <- list()
  expect_error(foo(h), "h must be a value of length 1")

  h <- list(1)
  expect_error(foo(h), "h must be a value of length 1")

  h <- list(1, 2, 3)
  expect_error(foo(h), "h must be a value of length 1")

  h <- c(TRUE, FALSE)
  expect_error(foo(h), "h must be a value of length 1")

  h <- c("a", "b", "c")
  expect_error(foo(h), "h must be a value of length 1")

  h <- matrix(1:10, 5, 2)
  expect_error(foo(h), "h must be a value of length 1")

  h <- raw(2)
  expect_error(foo(h), "h must be a value of length 1")

  h <- data.frame(c(1))
  expect_error(foo(h), "h must be a value of length 1")
})

test_that("fabric_sample_ACVF: h must not be infinite", {
  X <- 1:10
  foo <- fabric_sample_ACVF(X)

  h <- Inf
  expect_error(foo(h), "h must not be infinite")

  h <- -Inf
  expect_error(foo(h), "h must not be infinite")
})

test_that("fabric_sample_ACVF: h must not be NA", {
  X <- 1:10
  foo <- fabric_sample_ACVF(X)

  h <- NA
  expect_error(foo(h), "h must not be NA")

  h <- NaN
  expect_error(foo(h), "h must not be NA")

  h <- NA_integer_
  expect_error(foo(h), "h must not be NA")

  h <- NA_real_
  expect_error(foo(h), "h must not be NA")

  h <- NA_complex_
  expect_error(foo(h), "h must not be NA")

  h <- NA_character_
  expect_error(foo(h), "h must not be NA")
})

test_that("fabric_sample_ACVF: h must be numeric", {
  X <- 1:10
  foo <- fabric_sample_ACVF(X)

  h <- TRUE
  expect_error(foo(h), "h must be numeric")

  h <- FALSE
  expect_error(foo(h), "h must be numeric")

  h <- ""
  expect_error(foo(h), "h must be numeric")

  h <- "pi"
  expect_error(foo(h), "h must be numeric")

  h <- "42"
  expect_error(foo(h), "h must be numeric")

  h <- "3.2"
  expect_error(foo(h), "h must be numeric")

  h <- 0i
  expect_error(foo(h), "h must be numeric")

  h <- 1i
  expect_error(foo(h), "h must be numeric")

  h <- 2 + 5i
  expect_error(foo(h), "h must be numeric")

  h <- raw(1)
  expect_error(foo(h), "h must be numeric")
})

test_that("fabric_sample_ACVF: h must be an integer", {
  X <- 1:10
  foo <- fabric_sample_ACVF(X)

  h <- 3.2
  expect_error(foo(h), "h must be an integer")

  h <- 10^-6
  expect_error(foo(h), "h must be an integer")

  h <- pi
  expect_error(foo(h), "h must be an integer")

  h <- .Machine$double.eps
  expect_error(foo(h), "h must be an integer")

  h <- 0.1
  expect_error(foo(h), "h must be an integer")
})

test_that("fabric_sample_ACVF: h must be from the interval (-length(X), length(X))", {
  X <- 1:10
  foo <- fabric_sample_ACVF(X)

  h <- 10
  expect_error(foo(h), "h must be from the interval \\(-length\\(X\\), length\\(X\\)\\)")

  h <- -10
  expect_error(foo(h), "h must be from the interval \\(-length\\(X\\), length\\(X\\)\\)")

  h <- 11
  expect_error(foo(h), "h must be from the interval \\(-length\\(X\\), length\\(X\\)\\)")

  h <- -11
  expect_error(foo(h), "h must be from the interval \\(-length\\(X\\), length\\(X\\)\\)")
})

test_that("fabric_sample_ACVF: correctness", {
  X <- c(2, 4, 6, 8, 10)
  foo <- fabric_sample_ACVF(X)

  fabric_sample_ACVF_values <- sapply(0:4, foo)

  fabric_sample_ACVF_neg_values <- sapply(0:-4, foo)

  acf_values <- acf(c(2, 4, 6, 8, 10),
    type = c("covariance"),
    plot = FALSE
  )$acf[, , 1]

  expect_equal(acf_values, fabric_sample_ACVF_values)
  expect_equal(fabric_sample_ACVF_values, fabric_sample_ACVF_neg_values)
})
