## TESTING: function_fabric_sample_autocovariance_function
## Test conditions on x

test_that("TEST function_fabric_sample_autocovariance_function: conditions on x", {
  x <- list(1, 2, 3, 4)
  expect_error(function_fabric_sample_autocovariance_function(x), "x must be numeric")

  x <- c("s", "s", "t", "s")
  expect_error(function_fabric_sample_autocovariance_function(x), "x must be numeric")

  x <- data.frame(1:4)
  expect_error(function_fabric_sample_autocovariance_function(x), "x must be numeric")

  x <- data.frame(list(1:4))
  expect_error(function_fabric_sample_autocovariance_function(x), "x must be numeric")

  x <- c(1, 2, NA, 3, 4)
  expect_error(function_fabric_sample_autocovariance_function(x), "x has NA entries")

  x <- matrix(list(1:4))
  expect_error(function_fabric_sample_autocovariance_function(x), "x must be numeric")
})

## Test conditions on h

test_that("TEST function_fabric_sample_autocovariance_function: typ, length, class of h", {
  x <- c(1, 2, 3, 4)
  saf <- function_fabric_sample_autocovariance_function(x)

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

test_that("TEST function_fabric_sample_autocovariance_function: h with NULL, NA and Inf", {
  x <- c(2, 4, 6, 8, 10)
  saf <- function_fabric_sample_autocovariance_function(x)

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

test_that("TEST function_fabric_sample_autocovariance_function: h to be smaller than length(x) and bigger than -length(x)", {
  x <- c(2, 4, 6, 8, 10)
  saf <- function_fabric_sample_autocovariance_function(x)

  h <- 6
  expect_error(saf(h), "length of x > -h and h < length of x")

  h <- -6
  expect_error(saf(h), "length of x > -h and h < length of x")

  h <- 5
  expect_error(saf(h), "length of x > -h and h < length of x")

  h <- -5
  expect_error(saf(h), "length of x > -h and h < length of x")
})

test_that("TEST function_fabric_sample_autocovariance_function: correctness", {
  x <- c(2, 4, 6, 8, 10)
  sacf <- function_fabric_sample_autocovariance_function(x)

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
  expect_error(stand_alone_sample_autocovariance_function(x, h), "length of x > -h and h < length of x")

  h <- -6
  expect_error(stand_alone_sample_autocovariance_function(x, h), "length of x > -h and h < length of x")

  h <- 5
  expect_error(stand_alone_sample_autocovariance_function(x, h), "length of x > -h and h < length of x")

  h <- -5
  expect_error(stand_alone_sample_autocovariance_function(x, h), "length of x > -h and h < length of x")



  h <- NA
  expect_error(stand_alone_sample_autocovariance_function(x, h), "h must be numeric")

  h <- NA_real_
  expect_error(stand_alone_sample_autocovariance_function(x, h), "h must be an integer and has to have length 1")

  h <- NA_integer_
  expect_error(stand_alone_sample_autocovariance_function(x, h), "h must be an integer and has to have length 1")

  h <- Inf
  expect_error(stand_alone_sample_autocovariance_function(x, h), "h must be an integer and has to have length 1")

  h <- -Inf
  expect_error(stand_alone_sample_autocovariance_function(x, h), "h must be an integer and has to have length 1")

  h <- NULL
  expect_error(stand_alone_sample_autocovariance_function(x, h), "h must be numeric")



  x <- c(1, 2, 3, 4)


  h <- 2.5
  expect_error(stand_alone_sample_autocovariance_function(x, h), "h must be an integer and has to have length 1")

  h <- "2"
  expect_error(stand_alone_sample_autocovariance_function(x, h), "h must be numeric")

  h <- list(2.5)
  expect_error(stand_alone_sample_autocovariance_function(x, h), "h must be numeric")

  h <- c(1, 2)
  expect_error(stand_alone_sample_autocovariance_function(x, h), "h must be an integer and has to have length 1")

  h <- matrix(1:4)
  expect_error(stand_alone_sample_autocovariance_function(x, h), "h must be an integer and has to have length 1")

  h <- matrix(1)
  expect_true(stand_alone_sample_autocovariance_function(x, h) == 0.3125)

  h <- matrix(list(1))
  expect_error(stand_alone_sample_autocovariance_function(x, h), "h must be numeric")

  h <- matrix(c("1"))
  expect_error(stand_alone_sample_autocovariance_function(x, h), "h must be numeric")


  ## Checking for wrong Input of x

  h <- 0
  x <- list(1, 2, 3, 4)
  expect_error(stand_alone_sample_autocovariance_function(x, h), "x must be numeric")

  x <- c("s", "s", "t", "s")
  expect_error(stand_alone_sample_autocovariance_function(x, h), "x must be numeric")

  x <- data.frame(1:4)
  expect_error(stand_alone_sample_autocovariance_function(x, h), "x must be numeric")

  x <- data.frame(list(1:4))
  expect_error(stand_alone_sample_autocovariance_function(x, h), "x must be numeric")

  x <- matrix(list(1:4))
  expect_error(stand_alone_sample_autocovariance_function(x, h), "x must be numeric")

  h <- 0
  x <- c(1, 2, NA, 3, 4)
  expect_error(sample_autocovariance_function(x, h), "x has NA entries")

  ## Checking for wrong Input of x, h

  h <- Inf
  x <- matrix(list(1:4))
  expect_error(stand_alone_sample_autocovariance_function(x, h), "x must be numeric")


  h <- "2"
  x <- data.frame(1:4)
  expect_error(stand_alone_sample_autocovariance_function(x, h), "x must be numeric")
})

test_that("TEST stand_alone_sample_autocovariance_function: scf on correctness", {
  x <- c(2, 4, 6, 8, 10)

  acf_values <- acf(c(2, 4, 6, 8, 10),
    type = c("covariance"),
    plot = FALSE
  )$acf[, , 1]

  for (h in 0:(length(x) - 1)) {
    expect_true(stand_alone_sample_autocovariance_function(x, h) == acf_values[h + 1])
    expect_true(stand_alone_sample_autocovariance_function(x, -h) == acf_values[h + 1])
  }
})



## TESTING: sample_autocovariance_function

test_that("TEST sample_autocovariance_function: parameter x, h", {
  ## Checking for wrong Input of h

  x <- c(2, 4, 6, 8, 10)

  h <- 6
  expect_error(sample_autocovariance_function(x, h), "length of x > -h and h < length of x")

  h <- -6
  expect_error(sample_autocovariance_function(x, h), "length of x > -h and h < length of x")

  h <- 5
  expect_error(sample_autocovariance_function(x, h), "length of x > -h and h < length of x")

  h <- -5
  expect_error(sample_autocovariance_function(x, h), "length of x > -h and h < length of x")



  h <- NA
  expect_error(sample_autocovariance_function(x, h), "h must be numeric")

  h <- NA_real_
  expect_error(sample_autocovariance_function(x, h), "h has NA entries")

  h <- NA_integer_
  expect_error(sample_autocovariance_function(x, h), "h has NA entries")

  h <- Inf
  expect_error(sample_autocovariance_function(x, h), "h must integer vector")

  h <- -Inf
  expect_error(sample_autocovariance_function(x, h), "h must integer vector")

  h <- NULL
  expect_error(sample_autocovariance_function(x, h), "h must be numeric")



  x <- c(1, 2, 3, 4)


  h <- 2.5
  expect_error(sample_autocovariance_function(x, h), "h must integer vector")

  h <- "2"
  expect_error(sample_autocovariance_function(x, h), "h must be numeric")

  h <- list(2.5)
  expect_error(sample_autocovariance_function(x, h), "h must be numeric")

  h <- matrix(1:4)
  expect_error(sample_autocovariance_function(x, h), "length of x > -h and h < length of x, for all h")

  h <- c(NA, 13)
  expect_error(sample_autocovariance_function(x, h), "h has NA entries")

  h <- c(1, 2, 3, 4, 1)
  expect_error(sample_autocovariance_function(x, h), "length of h == length of distinct elements of h")

  h <- c(1, 2, 3, 4, 5)
  expect_error(sample_autocovariance_function(x, h), "length of x > -h and h < length of x, for all h")

  h <- c(1, 2.3)
  expect_error(sample_autocovariance_function(x, h), "h must integer vector")

  h <- matrix(1)
  expect_true(sample_autocovariance_function(x, h) == 0.3125)

  h <- matrix(list(1))
  expect_error(sample_autocovariance_function(x, h), "h must be numeric")

  h <- matrix(c("1"))
  expect_error(sample_autocovariance_function(x, h), "h must be numeric")




  ## Checking for wrong Input of x

  h <- 0
  x <- list(1, 2, 3, 4)
  expect_error(sample_autocovariance_function(x, h), "x must be numeric")

  x <- c("s", "s", "t", "s")
  expect_error(sample_autocovariance_function(x, h), "x must be numeric")

  x <- data.frame(1:4)
  expect_error(sample_autocovariance_function(x, h), "x must be numeric")

  x <- data.frame(list(1:4))
  expect_error(sample_autocovariance_function(x, h), "x must be numeric")

  x <- matrix(list(1:4))
  expect_error(sample_autocovariance_function(x, h), "x must be numeric")

  h <- 0
  x <- c(1, 2, NA, 3, 4)
  expect_error(sample_autocovariance_function(x, h), "x has NA entries")

  ## Checking for wrong Input of x, h

  h <- Inf
  x <- matrix(list(1:4))
  expect_error(sample_autocovariance_function(x, h), "x must be numeric")


  h <- "2"
  x <- data.frame(1:4)
  expect_error(sample_autocovariance_function(x, h), "x must be numeric")
})

test_that("TEST sample_autocovariance_function: scf on correctness", {
  x <- c(2, 4, 6, 8, 10)
  sacf_values <- sample_autocovariance_function(x)

  sacf_values_neg <- sample_autocovariance_function(x, 0:-(length(x) - 1))

  acf_values <- acf(c(2, 4, 6, 8, 10),
    type = c("covariance"),
    plot = FALSE
  )$acf[, , 1]

  expect_true(all(acf_values == sacf_values))
  expect_true(all(sacf_values == sacf_values_neg))
})
