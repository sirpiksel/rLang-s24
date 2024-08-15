#' @title Autocorrelation Function (ACF)
#'
#' @description `ACF` computes the autocorrelation function of a time series, which measures the correlation between the time series and its lagged versions over successive time intervals. The function also identifies the first lag where the autocorrelation drops below a specified confidence threshold.
#'
#' @details
#' The autocorrelation function (ACF) is a key tool in time series analysis that helps to identify repeating patterns, trends, or seasonal effects by quantifying the degree of similarity between a time series and its lagged versions. Given a time series \eqn{\{X_1, \dots, X_n\}}, the function calculates the autocorrelation for lags ranging from 0 up to `max_lag`. The function also checks for the first lag where the autocorrelation falls below a specified confidence threshold, returning this lag as `p`.
#'
#' @param X A numeric vector representing the time series data.
#'
#' @param max_lag An integer specifying the maximum lag to compute the autocorrelation function. By default, it is set to the length of the time series.
#'
#' @param confidence A numeric value representing the threshold below which autocorrelations are considered negligible. The default value is calculated as \eqn{1.96 / \sqrt{max\_lag}}.
#'
#' @return A list containing the following components:
#' \item{p}{An integer representing the first lag where the autocorrelation drops below the confidence threshold.}
#' \item{rho}{A numeric vector containing the autocorrelation values for each lag.}
#' \item{kappa}{A numeric value representing the autocovariance at the last computed lag.}
#' \item{sigma2}{A numeric value representing the variance of the time series.}
#'
#' @examples
#' # Generate a sample time series
#' X <- rnorm(100)
#'
#' # Calculate the autocorrelation function and the first significant lag
#' result <- ACF(X)
#' p <- result$p
#' rho <- result$rho
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @export
ACF <- function(X, max_lag = length(X), confidence = 1.96 / sqrt(max_lag)) {
  stopifnot(
    "Input X must be a numeric vector." = is.numeric(X) & all(is.numeric(X)),
    "max_lag must be an integer." = max_lag == as.integer(max_lag)
  )

  n <- length(X)
  rho <- numeric(max_lag)
  mean <- mean(X)
  sigma2 <- sum((X - mean)^2) / n
  p <- max_lag

  for (lag in 0:(max_lag - 1)) {
    if (lag == 0) {
      rho[lag + 1] <- 1
    } else {
      # calc autocovariance for lag
      kappa <- sum((X[1:(n - lag)] - mean) * (X[(lag + 1):n] - mean)) / n
      # calc autocorrelation for lag
      rho[lag + 1] <- kappa / sigma2
    }
  }

  for (lag in 1:max_lag) {
    if (rho[lag] < confidence) {
      p <- lag
      break
    }
  }
  return(list(p = p, rho = rho, kappa = kappa, sigma2 = sigma2))
}
