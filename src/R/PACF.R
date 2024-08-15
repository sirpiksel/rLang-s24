#' Partial Autocorrelation Function
#'
#' The Partial Autocorrelation Function measures the correlation between a time series and its lagged values, to identify the direct effect of a lag on the series.
#'
#' @param X time series as a numeric vector.
#' @param max_lag integer of the maximum lag to be checked. Set to the length of the time series by default.
#' @param confidence that below this threshold covariances are concidered negligible.
#'
#' @return q index of first lag that has dropped below confidence threshold; alpha numerical vector of autocorrelation for each lag.
#'
#' @examples
#' X <- as.numeric(tsdl::tsdl[[1]])
#' q <- zeitreihen::PACF(X)$q
#'
#' P <- zeitreihen::simulate(X, 14, zeitreihen::MA, q)
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
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
