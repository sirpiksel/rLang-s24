# TESTING fabric_ACVF
## Test conditions on x

test_that("TEST fabric_ACVF: conditions on x", {
  x <- list(1, 2, 3, 4)
  expect_error(fabric_sample_ACVF(x), "x must be numeric")

  x <- c("s", "s", "t", "s")
  expect_error(fabric_sample_ACVF(x), "x must be numeric")

  x <- data.frame(1:4)
  expect_error(fabric_sample_ACVF(x), "x must be numeric")

  x <- data.frame(list(1:4))
  expect_error(fabric_sample_ACVF(x), "x must be numeric")

  x <- c(1, 2, NA, 3, 4)
  expect_error(fabric_sample_ACVF(x), "x has NA entries")

  x <- matrix(list(1:4))
  expect_error(fabric_sample_ACVF(x), "x must be numeric")
})

## Test conditions on h

test_that("TEST fabric_ACVF: typ, length, class of h", {
  x <- c(1, 2, 3, 4)
  saf <- fabric_sample_ACVF(x)

  h <- 2.5
  expect_error(saf(h), "h must be an integer and has to have length 1")

  h <- "2"
  expect_error(saf(h), "h must be numeric")

  h <- list(2.5)
  expect_error(saf(h), "h must be numeric")

  h <- c(1, 2)
  expect_error(saf(h), "h must be an integer and has to have length 1")

  h <- matrix(1:4)
  expect_error(saf(h), "h must be an integer and has to have length 1")

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
  x <- c(2, 4, 6, 8, 10)
  saf <- fabric_sample_ACVF(x)

  h <- NA
  expect_error(saf(h), "h must be numeric")

  h <- NA_real_
  expect_error(saf(h), "h must be an integer and has to have length 1")

  h <- NA_integer_
  expect_error(saf(h), "h must be an integer and has to have length 1")

  h <- Inf
  expect_error(saf(h), "h must be an integer and has to have length 1")

  h <- -Inf
  expect_error(saf(h), "h must be an integer and has to have length 1")

  h <- NULL
  expect_error(saf(h), "h must be numeric")
})

test_that("TEST fabric_ACVF: h to be smaller than length(x) and bigger than -length(x)", {
  x <- c(2, 4, 6, 8, 10)
  saf <- fabric_sample_ACVF(x)

  h <- 6
  expect_error(saf(h), "length of x > -h and h < length of x")

  h <- -6
  expect_error(saf(h), "length of x > -h and h < length of x")

  h <- 5
  expect_error(saf(h), "length of x > -h and h < length of x")

  h <- -5
  expect_error(saf(h), "length of x > -h and h < length of x")
})

test_that("TEST fabric_ACVF: correctness", {
  x <- c(2, 4, 6, 8, 10)
  sacf <- fabric_sample_ACVF(x)

  sacf_values <- sapply(0:4, sacf)

  sacf_values_neg <- sapply(0:-4, sacf)

  acf_values <- acf(c(2, 4, 6, 8, 10),
    type = c("covariance"),
    plot = FALSE
  )$acf[, , 1]

  expect_true(all(acf_values == sacf_values))
  expect_true(all(sacf_values == sacf_values_neg))
})



## TESTING: stand_alone_sample_autocovariance_function

test_that("TEST stand_alone_sample_autocovariance_function: parameter x, h", {
  ## Checking for wrong Input of h

  x <- c(2, 4, 6, 8, 10)

  h <- 6
  expect_error(lone_sample_ACVF(x, h), "length of x > -h and h < length of x")

  h <- -6
  expect_error(lone_sample_ACVF(x, h), "length of x > -h and h < length of x")

  h <- 5
  expect_error(lone_sample_ACVF(x, h), "length of x > -h and h < length of x")

  h <- -5
  expect_error(lone_sample_ACVF(x, h), "length of x > -h and h < length of x")



  h <- NA
  expect_error(lone_sample_ACVF(x, h), "h must be numeric")

  h <- NA_real_
  expect_error(lone_sample_ACVF(x, h), "h must be an integer and has to have length 1")

  h <- NA_integer_
  expect_error(lone_sample_ACVF(x, h), "h must be an integer and has to have length 1")

  h <- Inf
  expect_error(lone_sample_ACVF(x, h), "h must be an integer and has to have length 1")

  h <- -Inf
  expect_error(lone_sample_ACVF(x, h), "h must be an integer and has to have length 1")

  h <- NULL
  expect_error(lone_sample_ACVF(x, h), "h must be numeric")



  x <- c(1, 2, 3, 4)


  h <- 2.5
  expect_error(lone_sample_ACVF(x, h), "h must be an integer and has to have length 1")

  h <- "2"
  expect_error(lone_sample_ACVF(x, h), "h must be numeric")

  h <- list(2.5)
  expect_error(lone_sample_ACVF(x, h), "h must be numeric")

  h <- c(1, 2)
  expect_error(lone_sample_ACVF(x, h), "h must be an integer and has to have length 1")

  h <- matrix(1:4)
  expect_error(lone_sample_ACVF(x, h), "h must be an integer and has to have length 1")

  h <- matrix(1)
  expect_true(lone_sample_ACVF(x, h) == 0.3125)

  h <- matrix(list(1))
  expect_error(lone_sample_ACVF(x, h), "h must be numeric")

  h <- matrix(c("1"))
  expect_error(lone_sample_ACVF(x, h), "h must be numeric")


  ## Checking for wrong Input of x

  h <- 0
  x <- list(1, 2, 3, 4)
  expect_error(lone_sample_ACVF(x, h), "x must be numeric")

  x <- c("s", "s", "t", "s")
  expect_error(lone_sample_ACVF(x, h), "x must be numeric")

  x <- data.frame(1:4)
  expect_error(lone_sample_ACVF(x, h), "x must be numeric")

  x <- data.frame(list(1:4))
  expect_error(lone_sample_ACVF(x, h), "x must be numeric")

  x <- matrix(list(1:4))
  expect_error(lone_sample_ACVF(x, h), "x must be numeric")

  h <- 0
  x <- c(1, 2, NA, 3, 4)
  expect_error(sample_ACVF(x, h), "x has NA entries")

  ## Checking for wrong Input of x, h

  h <- Inf
  x <- matrix(list(1:4))
  expect_error(lone_sample_ACVF(x, h), "x must be numeric")


  h <- "2"
  x <- data.frame(1:4)
  expect_error(lone_sample_ACVF(x, h), "x must be numeric")
})
