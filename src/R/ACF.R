#' Autocorrelation Function
#'
#' The Autocorrelation Function quantifies the degree of similarity between a time series and its lagged versions over successive time intervals, aiding in the detection of repeating patterns and trends.
#'
#' @param X time series as a numeric vector.
#' @param max_lag integer of the maximum lag to be checked. Set to the length of the time series by default.
#' @param confidence that below this threshold covariances are concidered negligible.
#'
#' @return p index of first lag that has dropped below confidence threshold; rho numerical vector of autocorrelation for each lag.
#'
#' @examples
#' X <- as.numeric(tsdl::tsdl[[1]])
#' p <- zeitreihen::ACF(X)$p
#'
#' P <- zeitreihen::simulate(X, 14, zeitreihen::AR, p)
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
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
