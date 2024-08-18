#' @title Estimate the Spectral Density of a Time Series using a Periodogram
#'
#' @description `periodogram` computes the periodogram of a time series, which can be used to estimate the spectral density.
#'
#' @details
#' The periodogram is a sample-based function from which we obtain estimators of the spectral density.
#' For a given time series \eqn{\{x_1, \dots, x_n\}}, the periodogram can be computed using the formula:
#'
#' \deqn{I_n(\lambda) = \displaystyle{\frac{1}{n}} \left| \displaystyle{\sum_{t=1}^n x_t e^{-it\lambda}} \right|^2.}
#'
#' @param X A numeric or complex atomic vector representing the time series data. The series should be stationary,
#' meaning that its mean and variance do not change over time.
#'
#' @param lambda A numerical value from the interval \eqn{(- \pi, \pi]}, which represents the frequency at which the periodogram is evaluated.
#'
#' @returns A numerical value that represents the periodogram.
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @examples
#' # Periodogram for a simple sine curve
#' X <- sin(2 * pi * 1:100 / 100)
#' lambda <- 2 * pi / 100
#' periodogram(X, lambda)
#'
#' # Periodogram for a complex exponential curve
#' X <- exp(2 * pi * 1i * 1:100 / 100)
#' lambda <- 2 * pi / 100
#' periodogram(X, lambda)
#'
#' # Periodogram for white noise
#' set.seed(123)
#' X <- rnorm(100)
#' lambda <- pi / 4
#' periodogram(X, lambda)
#'
#' @export
periodogram <- function(X, lambda) {
  stopifnot(
    "X must be an atomic vector" = is.atomic(X),
    "X must have positive length" = length(X) > 0,
    "X may not contain NAs" = !any(is.na(X)),
    "X may not contain Inf or -Inf values" = !any(is.infinite(X)),
    "The values of X must be numeric or complex" = (is.numeric(X) | is.complex(X)),
    "lambda must be a value of length 1" = (is.atomic(lambda) & length(lambda) == 1),
    "lambda must be numeric" = is.numeric(lambda),
    "lambda must be from the interval (-pi, pi]" = (-pi < lambda & lambda <= pi)
  )

  n <- length(X)
  exponentials <- exp(-1i * lambda * (1:n))
  result <- (abs(sum(X * exponentials))^2) / n

  return(result)
}
