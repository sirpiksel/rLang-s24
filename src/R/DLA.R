#' @title Recursive calculation of coefficients using the Durbin-Levinson Algorithm
#'
#' @description `DLA` is a function to compute the Durbin-Levinson recursion for time series data.
#'
#' @details
#' The Durbin-Levinson algorithm is used to recursively compute the coefficients \eqn{\phi_{n1}, \dots, \phi_{nn}}
#' for a given time series \eqn{\{x_1, \dots, x_n\}}. The recursion can be expressed as:
#' \deqn{
#' \phi_{nn} = \left[ \gamma(n) - \sum_{j=1}^{n-1} \phi_{n-1,j} \gamma(n-j) \right] v_{n-1}^{-1},
#' }
#' \deqn{
#' \left[ \begin{array}{c}
#' \phi_{n1} \\
#' \vdots \\
#' \phi_{n,n-1}
#' \end{array} \right]
#' =
#' \left[ \begin{array}{c}
#' \phi_{n-1,1} \\
#' \vdots \\
#' \phi_{n-1,n-1}
#' \end{array} \right]
#' -
#' \phi_{nn}
#' \left[ \begin{array}{c}
#' \phi_{n-1,n-1} \\
#' \vdots \\
#' \phi_{n-1,1}
#' \end{array} \right],
#' }
#' \deqn{
#' v_n = v_{n-1} \left[1 - \phi_{nn}^2\right],
#' }
#' where \eqn{\phi_{11} = \frac{\gamma(1)}{\gamma(0)}} and \eqn{v_0 = \gamma(0)}.
#'
#' The algorithm iteratively calculates these coefficients, which are essential in the analysis of autoregressive processes.
#'
#' @param X A numeric vector representing the time series data. The data should be a stationary time series.
#'
#' @returns A list containing:
#' \tabular{lcccl}{
#'   \code{phi} \tab \tab \tab \tab A numeric vector of length \code{m}, representing the computed AR coefficients \eqn{\phi{n1}, \dots, \phi{nn}}. \cr
#'   \code{v} \tab \tab \tab \tab A numeric vector of length \code{m}, representing the innovation variances.Row 2, Col 2 \cr
#' }
#'
#' @note
#' The function includes checks for numerical stability. If any of the calculated variances \eqn{v_n} or the initial
#' autocovariance value \eqn{\gamma(0)} is too close to zero, the function will stop and return an error to prevent
#' numerical instability.
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @examples
#' # Example usage
#' data <- c(2, 4, 6, 8, 10)
#' model <- DLA(data)
#' result <- model(2)
#' print(result)
#'
#' @export
DLA <- function(X) {
  stopifnot(
    "Data must be provided and must not be empty" = !missing(X) && length(X) > 0
  )
  n <- length(X)
  saf <- fabric_sample_ACVF(X)
  epsilon <- .Machine$double.eps # smallest value for double types

  function(m) {
    stopifnot(
      "A valid value for m must be provided" = !missing(m) && length(m) > 0,
      "0 < m < length(data)" = 0 < m & m < n
    )

    gamma <- sapply(0:m, saf)
    v <- numeric(m)
    phi <- numeric(m)


    if (abs(gamma[1]) < epsilon) {
      gamma[1] <- epsilon
    }

    v[1] <- gamma[1]
    phi[1] <- gamma[2] / (gamma[1])

    if (m == 1) {
      return(list(phi = phi, v = v))
    }

    for (i in 2:m) {
      v[i] <- v[i - 1] * (1 - phi[i - 1]**2)

      if (abs(v[i]) < epsilon) {
        v[i] <- epsilon
      }

      phi[i] <- (gamma[i + 1] - sum(phi[1:(i - 1)] * gamma[i:2])) / (v[i] + epsilon)
      phi[1:(i - 1)] <- phi[1:(i - 1)] - phi[i] * phi[(i - 1):1]
    }
    return(list(phi = phi, v = v))
  }
}
