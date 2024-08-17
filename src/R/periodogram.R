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
#' @param x A numeric or complex atomic vector representing the time series data. The series should be stationary,
#' meaning that its mean and variance do not change over time.
#'
#' @param lambda A numerical value from the interval \eqn{(- \pi, \pi]}, which represents the frequency at which the periodogram is evaluated.
#'
#' @returns A numerical value that represents the periodogram.
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @examples
#' # Generate a sample time series
#' x <- rnorm(100)
#'
#' # Calculate the periodogram
#' periodogram(x, pi / 2)
#'
#' @export
periodogram <- function(x, lambda) {
  stopifnot(
    "x must be an atomic vector" = is.atomic(x),
    "x must have positive length" = length(x) > 0,
    "x may not contain NAs" = !any(is.na(x)),
    "x may not contain Inf or -Inf values" = !any(is.infinite(x)),
    "The values of x must be numeric or complex" = (is.numeric(x) | is.complex(x)),
    "lambda must be a value of length 1" = (is.atomic(lambda) & length(lambda) == 1),
    "lambda must be numeric" = is.numeric(lambda),
    "lambda must be from the interval (-\u03C0, \u03C0]" = (-pi < lambda & lambda <= pi)
  )

  n <- length(x)
  exponentials <- exp(-1i * lambda * (1:n))
  result <- (abs(sum(x * exponentials))^2) / n

  return(result)
}
