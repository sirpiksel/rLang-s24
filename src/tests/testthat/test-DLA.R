# Test conditions for data

test_that("Test conditions on data", {
  data <- list(1, 2, 3, 4)
  expect_error(DLA(data), "Data must be provided and must not be empty")

  data <- c("s", "s", "t", "s")
  expect_error(DLA(data), "Data must be provided and must not be empty")

  data <- data.frame(1:4)
  expect_error(DLA(data), "Data must be provided and must not be empty")

  data <- data.frame(list(1:4))
  expect_error(DLA(data), "Data must be provided and must not be empty")

  data <- matrix(list(1:4))
  expect_error(DLA(data), "Data must be provided and must not be empty")

  data <- numeric(0)
  expect_error(DLA(data), "Data must be provided and must not be empty")
})

# Test conditions for m

test_that("Test conditions on m", {
  data <- c(1, 2, 3, 4)
  pdl <- DLA(data)

  m <- 4.5
  expect_error(pdl(m), "A valid value for m must be provided")

  m <- "2"
  expect_error(pdl(m), "A valid value for m must be provided")

  m <- list(2.5)
  expect_error(pdl(m), "A valid value for m must be provided")

  m <- c(1, 2)
  expect_error(pdl(m), "A valid value for m must be provided")

  m <- matrix(1:4)
  expect_error(pdl(m), "A valid value for m must be provided")

  m <- numeric(0)
  expect_error(pdl(m), "A valid value for m must be provided")
})

# Test for numerical stability and correctness

test_that("Test numerical stability and correctness", {
  data <- c(1, 2, 3, 4)
  pdl <- DLA(data)

  result <- pdl(2)

  expect_true(is.list(result))
  expect_true(all(names(result) %in% c("phi", "v")))
  expect_equal(length(result$phi), 2)
  expect_equal(length(result$v), 2)

  # Test for numerical stability at gamma[1] close to zero
  data <- c(0, 0, 0, 0)
  expect_error(DLA(data)(2), "gamma\\[1\\] is too close to zero, which may cause numerical instability.")

  # Test for numerical stability at v[i] close to zero
  data <- c(1, 1, 1, 1)
  pdl <- DLA(data)
  expect_error(pdl(2), "v\\[2\\] is too close to zero, which may cause numerical instability.")
})

# Test for the behavior of m at extreme values

test_that("Test conditions on extreme values of m", {
  data <- c(2, 4, 6, 8, 10)
  pdl <- DLA(data)

  m <- 5
  expect_error(pdl(m), "0 < m < length(data)")

  m <- -1
  expect_error(pdl(m), "0 < m < length(data)")

  m <- 0
  expect_error(pdl(m), "0 < m < length(data)")

  m <- 1
  result <- pdl(m)
  expect_true(is.list(result))
  expect_true(all(names(result) %in% c("phi", "v")))
  expect_equal(length(result$phi), 1)
  expect_equal(length(result$v), 1)
})
