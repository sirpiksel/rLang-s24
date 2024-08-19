#' @title The Sample Autocovariance Function
#'
#' @description
#' `sample_ACVF` computes the sample autocovariance of a given time series at a specified lag.
#'
#' @details
#' Let \eqn{\{x_1, \dots , x_n \}} be observations of a time series. The \strong{sample mean} of \eqn{x_1, \dots , x_n} is
#'
#' \deqn{\bar{x} = \displaystyle{\frac{1}{n} \sum^n_{t=1} x_t}.}
#'
#' The \strong{sample autocovariance function} measures the covariance between values of a time series across different lags and is given by
#'
#' \deqn{\hat{\gamma}(h) := n^{-1} \displaystyle{\sum_{t=1}^{n - |h|} (x_{t + |h|} - \bar{x}) \cdot (x_t - \bar{x})}, \quad -n < h < n.}
#'
#' @param X A numeric or complex atomic vector representing the time series data.
#'
#' @param h An integer atomic vector representing the lag values, where each value \eqn{h_i} must be unique and
#' fulfill the condition: \eqn{-\text{length}(X) < h_i < \text{length}(X)}.
#'
#' @returns A numeric atomic vector containing the calculated values of the sample autocovariance function corresponding to each entry in `h`.
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @examples
#' # Basic Usage
#' X <- c(1, 2, 3, 4, 5)
#' h <- c(0, 1, 2)
#' sample_ACVF(X, h)
#'
#' # Sample ACVF with a Single Lag
#' X <- c(2, 4, 6, 8, 10)
#' h <- 0
#' sample_ACVF(X, h)
#'
#' # Sample ACVF with Random Data
#' set.seed(123)
#' X <- rnorm(100)
#' h <- c(-2, 0, 2)
#' sample_ACVF(X, h)
#'
#' @export
sample_ACVF <- function(X, h = 0:(length(X) - 1)) {
  stopifnot(
    "X must be an atomic vector" = is.atomic(X),
    "X must have positive length" = length(X) > 0,
    "X may not contain NAs" = !any(is.na(X)),
    "X may not contain Inf or -Inf values" = !any(is.infinite(X)),
    "The values of X must be numeric or complex" = (is.numeric(X) | is.complex(X)),
    "h must be numeric" = is.numeric(h),
    "h may not contain NAs" = !any(is.na(h)),
    "h may not contain Inf or -Inf values" = !any(is.infinite(X)),
    "h must be an integer vector" = all(h %% 1 == 0),
    "The values of h must be unique" = length(h) == length(unique(h)),
    "All values of h must be from the interval (-length(X), length(X))" = all(h < length(X) & h > -length(X))
  )

  n <- length(X)
  xbar <- mean(X)

  solution <- sapply(h, \(h) {
    sum((X[(1 + abs(h)):n] - xbar) * (X[1:(n - abs(h))] - xbar)) / n
  })

  attr(solution, "names") <- h

  return(solution)
}


fabric_sample_ACVF <- function(X) {
  stopifnot(
    "X must be numeric" = is.numeric(X),
    "X has NA entries" = !any(is.na(X))
  )

  xbar <- mean(X)
  n <- length(X)

  function(h) {
    stopifnot(
      "h must be numeric" = is.numeric(h),
      "h must be integer and has to have length 1" = (h %% 1 == 0) & (length(h) == 1),
      "length of X > -h and h < length of X" = (h < n) && (-n < h)
    )

    solution <- sum((X[(1 + abs(h)):n] - xbar) * (X[1:(n - abs(h))] - xbar)) / n

    return(solution)
  }
}

lone_sample_ACVF <- function(X, h) {
  stopifnot(
    "X must be numeric" = is.numeric(X),
    "X has NA entries" = !any(is.na(X)),
    "h must be numeric" = is.numeric(h),
    "h must be an integer and has to have length 1" = (h %% 1 == 0) & (length(h) == 1),
    "length of X > -h and h < length of X" = (h < length(X)) && (-length(X) < h)
  )

  n <- length(X)
  xbar <- mean(X)

  solution <- sum((X[(1 + abs(h)):n] - xbar) * (X[1:(n - abs(h))] - xbar)) / n

  return(solution)
}
