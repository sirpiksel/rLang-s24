# TESTING fabric_ACVF
## Test conditions on X

test_that("TEST fabric_ACVF: conditions on X", {
  X <- list(1, 2, 3, 4)
  expect_error(fabric_sample_ACVF(X), "X must be numeric")

  X <- c("s", "s", "t", "s")
  expect_error(fabric_sample_ACVF(X), "X must be numeric")

  X <- data.frame(1:4)
  expect_error(fabric_sample_ACVF(X), "X must be numeric")

  X <- data.frame(list(1:4))
  expect_error(fabric_sample_ACVF(X), "X must be numeric")

  X <- c(1, 2, NA, 3, 4)
  expect_error(fabric_sample_ACVF(X), "X has NA entries")

  X <- matrix(list(1:4))
  expect_error(fabric_sample_ACVF(X), "X must be numeric")
})

## Test conditions on h

test_that("TEST fabric_ACVF: typ, length, class of h", {
  X <- c(1, 2, 3, 4)
  saf <- fabric_sample_ACVF(X)

  h <- 2.5
  expect_error(saf(h), "h must be integer and has to have length 1")

  h <- "2"
  expect_error(saf(h), "h must be numeric")

  h <- list(2.5)
  expect_error(saf(h), "h must be numeric")

  h <- c(1, 2)
  expect_error(saf(h), "h must be integer and has to have length 1")

  h <- matrix(1:4)
  expect_error(saf(h), "h must be integer and has to have length 1")

  h <- matrix(1)
  expect_true(saf(h) == 0.3125)

  h <- matrix(list(1))
  expect_error(saf(h), "h must be numeric")

  h <- matrix(c("1"))
  expect_error(saf(h), "h must be numeric")

  h <- matrix(list("1"))
  expect_error(saf(h), "h must be numeric")
})

test_that("TEST fabric_ACVF: h with NULL, NA and Inf", {
  X <- c(2, 4, 6, 8, 10)
  saf <- fabric_sample_ACVF(X)

  h <- NA
  expect_error(saf(h), "h must be numeric")

  h <- NA_real_
  expect_error(saf(h), "h must be integer and has to have length 1")

  h <- NA_integer_
  expect_error(saf(h), "h must be integer and has to have length 1")

  h <- Inf
  expect_error(saf(h), "h must be integer and has to have length 1")

  h <- -Inf
  expect_error(saf(h), "h must be integer and has to have length 1")

  h <- NULL
  expect_error(saf(h), "h must be numeric")
})

test_that("TEST fabric_ACVF: h to be smaller than length(X) and bigger than -length(X)", {
  X <- c(2, 4, 6, 8, 10)
  saf <- fabric_sample_ACVF(X)

  h <- 6
  expect_error(saf(h), "length of X > -h and h < length of X")

  h <- -6
  expect_error(saf(h), "length of X > -h and h < length of X")

  h <- 5
  expect_error(saf(h), "length of X > -h and h < length of X")

  h <- -5
  expect_error(saf(h), "length of X > -h and h < length of X")
})

test_that("TEST fabric_ACVF: correctness", {
  X <- c(2, 4, 6, 8, 10)
  sacf <- fabric_sample_ACVF(X)

  sacf_values <- sapply(0:4, sacf)

  sacf_values_neg <- sapply(0:-4, sacf)

  acf_values <- acf(c(2, 4, 6, 8, 10),
    type = c("covariance"),
    plot = FALSE
  )$acf[, , 1]

  expect_true(all(acf_values == sacf_values))
  expect_true(all(sacf_values == sacf_values_neg))
})



## TESTING lone_sample_ACVF

test_that("TEST lone_sample_ACVF: parameter X, h", {
  ## Checking for wrong Input of h

  X <- c(2, 4, 6, 8, 10)

  h <- 6
  expect_error(lone_sample_ACVF(X, h), "length of X > -h and h < length of X")

  h <- -6
  expect_error(lone_sample_ACVF(X, h), "length of X > -h and h < length of X")

  h <- 5
  expect_error(lone_sample_ACVF(X, h), "length of X > -h and h < length of X")

  h <- -5
  expect_error(lone_sample_ACVF(X, h), "length of X > -h and h < length of X")



  h <- NA
  expect_error(lone_sample_ACVF(X, h), "h must be numeric")

  h <- NA_real_
  expect_error(lone_sample_ACVF(X, h), "h must be an integer and has to have length 1")

  h <- NA_integer_
  expect_error(lone_sample_ACVF(X, h), "h must be an integer and has to have length 1")

  h <- Inf
  expect_error(lone_sample_ACVF(X, h), "h must be an integer and has to have length 1")

  h <- -Inf
  expect_error(lone_sample_ACVF(X, h), "h must be an integer and has to have length 1")

  h <- NULL
  expect_error(lone_sample_ACVF(X, h), "h must be numeric")



  X <- c(1, 2, 3, 4)


  h <- 2.5
  expect_error(lone_sample_ACVF(X, h), "h must be an integer and has to have length 1")

  h <- "2"
  expect_error(lone_sample_ACVF(X, h), "h must be numeric")

  h <- list(2.5)
  expect_error(lone_sample_ACVF(X, h), "h must be numeric")

  h <- c(1, 2)
  expect_error(lone_sample_ACVF(X, h), "h must be an integer and has to have length 1")

  h <- matrix(1:4)
  expect_error(lone_sample_ACVF(X, h), "h must be an integer and has to have length 1")

  h <- matrix(1)
  expect_true(lone_sample_ACVF(X, h) == 0.3125)

  h <- matrix(list(1))
  expect_error(lone_sample_ACVF(X, h), "h must be numeric")

  h <- matrix(c("1"))
  expect_error(lone_sample_ACVF(X, h), "h must be numeric")


  ## Checking for wrong Input of X

  h <- 0
  X <- list(1, 2, 3, 4)
  expect_error(lone_sample_ACVF(X, h), "X must be numeric")

  X <- c("s", "s", "t", "s")
  expect_error(lone_sample_ACVF(X, h), "X must be numeric")

  X <- data.frame(1:4)
  expect_error(lone_sample_ACVF(X, h), "X must be numeric")

  X <- data.frame(list(1:4))
  expect_error(lone_sample_ACVF(X, h), "X must be numeric")

  X <- matrix(list(1:4))
  expect_error(lone_sample_ACVF(X, h), "X must be numeric")

  h <- 0
  X <- c(1, 2, NA, 3, 4)
  expect_error(sample_ACVF(X, h), "X may not contain NAs")

  ## Checking for wrong Input of X, h

  h <- Inf
  X <- matrix(list(1:4))
  expect_error(lone_sample_ACVF(X, h), "X must be numeric")


  h <- "2"
  X <- data.frame(1:4)
  expect_error(lone_sample_ACVF(X, h), "X must be numeric")
})
