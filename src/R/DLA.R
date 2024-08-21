#' @title Recursive Calculation of Coefficients using the Durbin-Levinson Algorithm
#'
#' @description `DLA` can be used to perform the Durbin-Levinson recursion on time series data.
#'
#' @details
#' The \strong{Durbin-Levinson Algorithm} is used to recursively calculate the coefficients \eqn{\phi_{n1}, \dots, \phi_{nn}}
#' for a given time series \eqn{\{x_1, \dots, x_n\}}. The recursion can be expressed as follows:
#'
#' \deqn{\phi_{nn} = \left[ \gamma(n) - \displaystyle{\sum_{j=1}^{n-1} \phi_{n-1, j} \gamma(n-j)} \right] \nu_{n-1}^{-1},}
#'
#' \deqn{
#' \left[ \begin{array}{c}
#'        \phi_{n1} \\
#'        \vdots \\
#'        \phi_{n, n-1}
#'        \end{array}
#' \right]
#' =
#' \left[ \begin{array}{c}
#'        \phi_{n-1,1} \\
#'        \vdots \\
#'        \phi_{n-1,n-1}
#'        \end{array}
#' \right]
#' -
#' \phi_{nn}
#' \left[ \begin{array}{c}
#'        \phi_{n-1,n-1} \\
#'        \vdots \\
#'        \phi_{n-1,1}
#'        \end{array}
#' \right]}
#'
#' and
#'
#' \deqn{\nu_n = \nu_{n-1} \left[1 - \phi_{nn}^2\right],}
#'
#' where \eqn{\phi_{11} = \gamma(1)/\gamma(0)} and \eqn{\nu_0 = \gamma(0)}.
#'
#' The algorithm iteratively calculates these coefficients, which are essential for the analysis of autoregressive processes
#' and can be used as inputs of the \code{\link{make_AR}} function.
#'
#' @param X A numeric or complex atomic vector representing the time series data.
#' The series should be stationary, meaning that its mean and variance do not change over time.
#'
#' @returns The output of this function is an anonymous function. The returned function takes the value m as a parameter. The value m means that we use the last m values of the time series to predict by the Durbin-Levinson-Algorithm the next time series value. This function calculates the DLA algorithm, which then returns
#' a list with two components:
#' \item{phi}{A numeric atomic vector representing the calculated AR coefficients \eqn{\phi_{n1}, \dots, \phi_{nn}}.}
#' \item{nu}{A numeric atomic vector representing the mean squared error of the one-step predictors.}
#'
#' @note
#' The function includes checks for numerical stability. If any of the calculated variances \eqn{\nu_n} or the initial
#' autocovariance value \eqn{\gamma(0)} is too close to zero, the function terminates and returns an error to prevent numerical instability.
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @examples
#' # Example 1: Basic Usage
#' X <- rnorm(100)
#' model <- DLA(X)
#' result <- model(20)
#' print(result)
#'
#' # Example 2: Usage for an AR(4) model
#' t <- 100
#' phi <- c(1, 0, 1, 2)
#' data <- make_AR(t, phi)
#' model <- DLA(data)
#' result <- model(4)
#' print(result)
#'
#' # Example 3: Usage for an MA(4) model
#' t <- 100
#' theta <- c(0, 4, 1, 0)
#' data <- make_MA(t, theta)
#' model <- DLA(data)
#' result <- model(4)
#' print(result)
#'
#' @export
DLA <- function(X) {
  stopifnot(
    "X must not be empty" = !missing(X),
    "The values of X must be numeric or complex" = is.atomic(X) & (is.numeric(X) | is.complex(X)),
    "X must have positive length" = length(X) > 0,
    "X may not contain NAs" = !any(is.na(X)),
    "X may not contain Inf or -Inf values" = !any(is.infinite(X))
  )
  
  warning("Please note: This algorithm works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")
  
  n <- length(X)
  saf <- fabric_sample_ACVF(X)

  function(m) {
    stopifnot(
      "m must be a value of length 1" = length(m) == 1,
      "m must be numeric" = is.numeric(m),
      "m must not be infinite" = !is.infinite(m),
      "m must not be NA" = !is.na(m),
      "m must be an integer" = m %% 1 == 0,
      "m must be between 0 and length of X" = (0 < m) & (m < n)
    )

    gamma <- sapply(0:m, saf)
    nu <- numeric(m)
    phi <- numeric(m)

    nu[1] <- gamma[1]
    phi[1] <- gamma[2] / (gamma[1])

    if (m == 1) {
      return(list(phi = phi, nu = nu))
    }

    for (i in 2:m) {
      nu[i] <- nu[i - 1] * (1 - phi[i - 1]**2)
      phi[i] <- (gamma[i + 1] - sum(phi[1:(i - 1)] * gamma[i:2])) / nu[i]
      phi[1:(i - 1)] <- phi[1:(i - 1)] - phi[i] * phi[(i - 1):1]
    }
    
    return(list(phi = phi, nu = nu))
  }
}
