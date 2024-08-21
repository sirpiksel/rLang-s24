#' @title Recursive Calculation of Coefficients using the Innovations Algorithm
#'
#' @description `IA` can be used to compute the innovations and the innovation variances for a time series.
#'
#' @details
#' The \strong{Innovations Algorithm} is a recursive method used in time series analysis to compute the best linear predictors of a time series and their associated prediction errors, known as innovations. For a given time series \eqn{\{x_1, \dots, x_n\}}, the algorithm provides a way to decompose the series into uncorrelated components.
#'
#' The algorithm works by computing the innovations and their variances step by step, using the given \code{\link{sample_ACVF}} function of the time series. It is particularly useful in the context of ARMA models and other linear time series models.
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
#' The algorithm iteratively calculates these coefficients, which are essential for the analysis of moving average processes
#' and can be used as inputs of the \code{\link{make_MA}} function.
#'
#' @param X A numeric or complex atomic vector representing the time series data.
#' @param max_lag An integer specifying the maximum number of lags to be checked. By default, it is set to the length of the time series.
#'
#' @returns A list with three components:
#' \item{coeffs}{A numeric atomic vector of one-step predictors.}
#' \item{nu}{A numeric atomic vector of innovations or one-step prediction errors.}
#' \item{theta}{A numeric coefficient matrix of the Innovations Algorithm.}
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @examples
#' # Example 1: Basic Usage
#' X <- rnorm(100)
#' out <- IA(X)
#' print(out)
#'
#' # Example 2: Usage for an AR(2) model
#' X <- make_AR(100, phi = c(0.1, 0.2))
#' out <- IA(X)
#' print(out)
#'
#' # Example 3: Usage for an MA(3) model
#' X <- make_MA(100, theta = c(0.2, 0.25, -0.3))
#' out <- IA(X)
#' print(out)
#'
#' @export
IA <- function(X, max_lag = length(X)) {
  message("This algorithm works for stationary time series with zero-mean.")
  message("For any other time series the results may be wrong.")
  stopifnot(
    "X must be an atomic vector" = is.atomic(X),
    "X must have more than two values" = length(X) > 2,
    "X may not contain NAs" = !any(is.na(X)),
    "X may not contain Inf or -Inf values" = !any(is.infinite(X)),
    "X must only contain numeric or complex values" = (is.numeric(X) | is.complex(X)),
    "max_lag must be numeric" = is.numeric(max_lag),
    "max_lag must be an integer" = max_lag %% 1 == 0 & length(max_lag) == 1,
    "max_lag cannot exceed length(X)" = max_lag <= length(X),
    "max_lag cannot be smaller than 3" = 3 <= max_lag,
    "max_lag may not be NA" = !(is.na(max_lag)),
    "max_lag may not be an Inf or -Inf value" = !(is.infinite(max_lag))
  )

  nu <- numeric(max_lag)

  # Calculate autocovariance at the start of the algorithm
  autocov <- sample_ACVF(X, 0:(max_lag - 1))

  # initialize
  nu[1] <- autocov[1]
  theta_mat <- matrix(0, ncol = max_lag, nrow = max_lag)
  theta_mat[2, 1] <- 1 / nu[1] * autocov[2]
  nu[2] <- autocov[1] - theta_mat[2, 1]^2 * nu[1]

  # calculate theta_mat,n-k n rows
  for (i in 2:(max_lag - 1)) {
    # Diagonal element theta_mat,n has to be calculated  first in every iteration
    theta_mat[i + 1, 1] <- 1 / nu[1] * autocov[i + 1]
    for (k in 1:(i - 1)) {
      theta_mat[i + 1, k + 1] <- 1 / nu[k + 1] * (autocov[i - k + 1] - sum(theta_mat[k + 1, 1:k] * theta_mat[i + 1, 1:k] * nu[1:k]))
    }
    nu[i + 1] <- autocov[1] - sum(theta_mat[i + 1, 1:i]^2 * nu[1:i])
  }
  coeffs <- theta_mat[max_lag, (max_lag - 1):1]
  return(list(coeffs = coeffs, nu = nu, theta = theta_mat))
}
