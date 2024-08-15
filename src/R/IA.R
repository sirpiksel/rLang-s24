#' @title Compute the Innovations Algorithm for Time Series
#'
#' @description `IA` can be used to compute the innovations and the innovation variances for a time series.
#'
#' @details
#' The innovations algorithm is a recursive method used in time series analysis to compute the best linear predictors of a time series and their associated prediction errors, known as innovations. For a given time series \eqn{\{x_1, \dots, x_n\}}, the algorithm provides a way to decompose the series into uncorrelated components.
#'
#' The algorithm works by computing the innovations and their variances step by step, using the given autocovariance function of the time series. It is particularly useful in the context of ARMA models and other linear time series models.
#'
#' @param X time series as a numeric vector.
#' @param q integer of the maximum lag to be checked. Set to the length of the time series - 1 by default.
#' @param matrix a boolean wether or not to output a matrix
#'
#' @returns A list with two components:
#' \item{innovations}{A numeric vector containing the computed innovations.}
#' \item{innovation_variances}{A numeric vector containing the variances of the innovations.}
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @examples
#' # Generate a sample time series and its autocovariance
#' x <- rnorm(100)
#' acf <- acf(x, plot = FALSE)$acf
#'
#' # Calculate the innovations and their variances
#' results <- innovations_algorithm(x, acf)
#' innovations <- results$innovations
#' variances <- results$innovation_variances
#'
#' @export

IA <- function(X, q = (length(X) - 1), matrix = FALSE) {
  n <- length(X)
  nu <- numeric(n)
  # Calculate autocovariance at the start of the algorithm
  autocov <- sapply(0:(n - 1), function(h) zeitreihen::stand_alone_sample_autocovariance_function(X, h))
  # Value v_0
  nu[1] <- autocov[1]
  theta <- matrix(0, ncol = n, nrow = n)
  theta[2, 1] <- 1 / nu[1] * autocov[2]
  nu[2] <- autocov[1] - theta[2, 1]^2 * nu[1]

  # calculata theta_n,n-k n rows
  for (i in 2:(n - 1)) {
    # Diagonal element theta_n,n has to be calcuated first in every iteration
    theta[i + 1, 1] <- 1 / nu[1] * autocov[i + 1]
    for (k in 1:(i - 1)) {
      theta[i + 1, k + 1] <- 1 / nu[k + 1] * (autocov[i - k + 1] - sum(theta[k + 1, 1:k] * theta[i + 1, 1:k] * nu[1:k]))
    }
    nu[i + 1] <- autocov[1] - sum(theta[i + 1, 1:i]^2 * nu[1:i])
  }
  coeffs <- theta[n, (n - 1):(n - q)]
  if (matrix == TRUE) {
    return(list(theta_n = theta, nu_n = nu, coeffs = coeffs))
  } else {
    return(list(nu_n = nu, coeffs = coeffs))
  }
}
