#' @title sample autocovariance function
#'
#' @description
#' Computes the sample autocovariance of the lag h based on data x.
#'
#' @details
#' Consider the observed data \eqn{\{x_1,x_2,...,x_n\}}, which represents a Time Series.
#' The sample autocovariance function (\eqn{\hat \gamma}) measures the covariance between values of a Time Series over different lag.
#'
#' \eqn{\hat \gamma(h) = \sum_{t=1}^{n - |h|} (x_{t + |h|)}- \bar x) \cdot (x_t - \bar x)}, -n < h < n and
#' \eqn{\bar x = n^{-1} \cdot \sum_{t=1}^{n} x_{t}}
#'
#' @param x A numeric vector representing Time Series data. NA entries are not allowed.
#' @param h A integer vector contains the lag. NA entries are not allowed and h should be unique.
#'
#' For all entries of h: -length(x) < entries < length(x).
#'
#' @returns A numeric vector, which contains computation of the sample autocovariance function based on the entries of h.
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4)
#' h <- c(0, 1, 2)
#' sample_autocovariance_function(x, h)
sample_autocovariance_function <- function(x, h = 0:(length(x) - 1)) {
  stopifnot(
    "x must be numeric" = is.numeric(x),
    "x has NA entries" = !any(is.na(x)),
    "h must be numeric" = is.numeric(h),
    "h has NA entries" = !any(is.na(h)),
    "h must integer vector" = all(h %% 1 == 0),
    "length of h == length of distinct elements of h" = length(h) == length(unique(h)),
    "length of x > -h and h < length of x, for all h" = all(h < length(x)) && all(-length(x) < h)
  )

  xbar <- mean(x)
  n <- length(x)

  solution <- sapply(h, \(h) {
    sum((x[(1 + abs(h)):n] - xbar) * (x[1:(n - abs(h))] - xbar)) / n
  })

  attr(solution, "names") <- h

  return(solution)
}



function_fabric_sample_autocovariance_function <- function(x) {
  stopifnot(
    "x must be numeric" = is.numeric(x),
    "x has NA entries" = !any(is.na(x))
  )

  xbar <- mean(x)
  n <- length(x)
  function(h) {
    stopifnot(
      "h must be numeric" = is.numeric(h),
      "h must be an integer and has to have length 1" = (h %% 1 == 0) & (length(h) == 1),
      "length of x > -h and h < length of x" = (h < n) && (-n < h)
    )

    solution <- sum((x[(1 + abs(h)):n] - xbar) * (x[1:(n - abs(h))] - xbar)) / n
    return(solution)
  }
}



stand_alone_sample_autocovariance_function <- function(x, h) {
  stopifnot(
    "x must be numeric" = is.numeric(x),
    "x has NA entries" = !any(is.na(x)),
    "h must be numeric" = is.numeric(h),
    "h must be an integer and has to have length 1" = (h %% 1 == 0) & (length(h) == 1),
    "length of x > -h and h < length of x" = (h < length(x)) && (-length(x) < h)
  )

  xbar <- mean(x)
  n <- length(x)
  solution <- sum((x[(1 + abs(h)):n] - xbar) * (x[1:(n - abs(h))] - xbar)) / n
  return(solution)
}
