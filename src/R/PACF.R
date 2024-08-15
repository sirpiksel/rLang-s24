#' @title Partial Autocorrelation Function (PACF)
#'
#' @description `PACF` computes the partial autocorrelation function of a time series, which quantifies the correlation between the time series and its lagged versions, accounting for the influence of intervening lags. The function also identifies the first lag where the partial autocorrelation drops below a specified confidence threshold.
#'
#' @details
#' The partial autocorrelation function (PACF) is used in time series analysis to measure the correlation between the time series and its lagged versions, after removing the effect of shorter lags. This makes PACF particularly useful in identifying the direct effect of a lag on the time series, which is helpful in determining the order of an autoregressive (AR) model.
#'
#' For a given time series \eqn{\{X_1, \dots, X_n\}}, the PACF is computed for lags up to `max_lag` by solving the Yule-Walker equations. The function also identifies the first lag where the partial autocorrelation coefficient falls below a specified confidence threshold, returning this lag as `q`.
#'
#' @param X A numeric vector representing the time series data.
#'
#' @param max_lag An integer specifying the maximum lag to compute the partial autocorrelation function. By default, it is set to the length of the time series.
#'
#' @param confidence A numeric value representing the threshold below which partial autocorrelations are considered negligible. The default value is calculated as \eqn{1.96 / \sqrt{max\_lag}}.
#'
#' @return A list containing the following components:
#' \item{q}{An integer representing the first lag where the partial autocorrelation drops below the confidence threshold.}
#' \item{alpha}{A numeric vector containing the partial autocorrelation coefficients for each lag.}
#'
#' @examples
#' # Generate a sample time series
#' X <- rnorm(100)
#'
#' # Calculate the partial autocorrelation function and the first significant lag
#' result <- PACF(X)
#' q <- result$q
#' alpha <- result$alpha
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @export
PACF <- function(X, max_lag = length(X), confidence = 1.96 / sqrt(max_lag)) {
  stopifnot(
    "Input X must be a numeric vector." = is.numeric(X) & all(is.numeric(X)),
    "max_lag must be an integer." = max_lag == as.integer(max_lag)
  )

  alpha <- numeric(max_lag)
  q <- max_lag

  for (lag in 1:(max_lag - 1)) {
    if (lag == 1) {
      alpha[lag] <- ACF(X, max_lag = 1)[2]
    } else {
      # solve Yule-Walker for lag
      r <- ACF(X, max_lag = lag)[2:(lag + 1)]
      R <- matrix(0, nrow = lag, ncol = lag)
      for (i in 1:lag) {
        for (j in 1:lag) {
          R[i, j] <- ACF(X, max_lag = lag)[abs(i - j) + 1]
        }
      }
      # solve the linear system to get PACF
      alpha[lag] <- solve(R) %*% r
    }
  }

  for (lag in 1:max_lag) {
    if (abs(alpha[lag]) < confidence) {
      q <- lag
      break
    }
  }
  return(list(q = q, alpha = alpha))
}
