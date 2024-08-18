#' @title Recursive calculation of coefficients using the Innovations Algorithm
#'
#' @description `IA` can be used to compute the innovations and the innovation variances for a time series.
#'
#' @details
#' The innovations algorithm is a recursive method used in time series analysis to compute the best linear predictors of a time series and their associated prediction errors, known as innovations. For a given time series \eqn{\{x_1, \dots, x_n\}}, the algorithm provides a way to decompose the series into uncorrelated components.
#'
#' The algorithm works by computing the innovations and their variances step by step, using the given autocovariance function of the time series. It is particularly useful in the context of ARMA models and other linear time series models.
#'
#' The coefficients \eqn{\theta_{n1}, \dots, \theta_{nn}} can be computed recursively from the equations
#'
#' \deqn{\nu_0 = \kappa(1, 1),}
#'
#' \deqn{\theta_{n, n-k} = \nu_k^{-1} \left( \kappa(n+1, k+1) - \displaystyle{\sum_{j=0}^{k-1} \theta_{k, k-j} \theta_{n, n-j} \nu_j} \right), \quad 0 \leq k < n,}
#'
#' and
#'
#' \deqn{\nu_n = \kappa(n+1, n+1) - \displaystyle{\sum_{j=0}^{n-1} \theta_{n, n-j}^2 \nu_j}.}
#'
#' @param X A numeric vector representing the time series data.
#' @param max_lag An integer specifying the maximum number of lags to be checked. By default, it is set to the length of the time series minus 1.
#'
#' @returns A list with three components:
#' \item{coeffs}{numeric vector containing the computed time series coefficients}
#' \item{nu}{numeric vector containing the innovations / variances}
#' \item{theta}{coefficient matrix of the Innovations algorithm}
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @examples
#' # Generate a sample time series and its autocovariance
#' X <- rnorm(100)
#'
#' # Calculate coefficients
#' out <- IA(X)
#' print(out$coeffs)
#' print(out$nu)
#' print(out$theta)
#'
#' @export
IA <- function(X, max_lag = length(X)) {
  stopifnot(
    "X must be a native vector" = is.atomic(X),
    "X must be only contain numeric or complex values" = is.numeric(X) | is.complex(X),
    "X must be filled with finite values." = is.finite(X),
    "max_lag must be an integer." = max_lag %% 1 == 0 & length(max_lag) == 1,
    "q cannot exceed length(X) - 1" = max_lag <= length(X)
  )

  nu <- numeric(max_lag)
  # Calculate autocovariance at the start of the algorithm
  autocov <- sample_ACVF(X, 0:(max_lag - 1))
  # initialize
  theta_mat <- matrix(0, ncol = max_lag, nrow = max_lag)
  theta_mat[2, 1] <- 1 / nu[1] * autocov[2]
  nu[1] <- autocov[1]
  nu[2] <- autocov[1] - theta_mat[2, 1]^2 * nu[1]

  # calculate theta_mat,n-k n rows
  for (i in 2:(max_lag - 1)) {
    # Diagonal element theta_mat,n has to be calcuated first in every iteration
    theta_mat[i + 1, 1] <- 1 / nu[1] * autocov[i + 1]
    for (k in 1:(i - 1)) {
      theta_mat[i + 1, k + 1] <- 1 / nu[k + 1] * (autocov[i - k + 1] - sum(theta_mat[k + 1, 1:k] * theta_mat[i + 1, 1:k] * nu[1:k]))
    }
    nu[i + 1] <- autocov[1] - sum(theta_mat[i + 1, 1:i]^2 * nu[1:i])
  }
  coeffs <- theta_mat[max_lag, (max_lag - 1):1]
  return(list(coeffs = coeffs, nu = nu, theta = theta_mat))
}
