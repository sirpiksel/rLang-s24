#' @title Recursive Calculation of Coefficients using the Durbin-Levinson Algorithm
#'
#' @description `DLA` can be used to perform the Durbin-Levinson algorithm on stationary time series data.
#'
#' @usage 
#' model <- DLA(X)
#' model(m)
#'
#' @details
#' The \strong{Durbin-Levinson Algorithm} is used to recursively calculate the coefficients \eqn{\phi_{m1}, \dots, \phi_{mm}}
#' for a given time series \eqn{\{x_1, \dots, x_n\}}. The recursion can be expressed as follows:
#'
#' \deqn{\phi_{mm} = \left[ \gamma(m) - \displaystyle{\sum_{j=1}^{m-1} \phi_{m-1, j} \gamma(m-j)} \right] \nu_{m-1}^{-1},}
#'
#' \deqn{
#' \left[ \begin{array}{c}
#'        \phi_{m1} \\
#'        \vdots \\
#'        \phi_{m, m-1}
#'        \end{array}
#' \right]
#' =
#' \left[ \begin{array}{c}
#'        \phi_{m-1,1} \\
#'        \vdots \\
#'        \phi_{m-1,m-1}
#'        \end{array}
#' \right]
#' -
#' \phi_{mm}
#' \left[ \begin{array}{c}
#'        \phi_{m-1,m-1} \\
#'        \vdots \\
#'        \phi_{m-1,1}
#'        \end{array}
#' \right]}
#'
#' and
#'
#' \deqn{\nu_m = \nu_{m-1} \left[1 - \phi_{mm}^2\right],}
#'
#' where \eqn{\phi_{11} = \gamma(1)/\gamma(0)} and \eqn{\nu_0 = \gamma(0)}.
#'
#' The algorithm iteratively calculates these coefficients, which are essential for the analysis of autoregressive processes
#' and can be used to calculate the one-step predictor \eqn{P_m X_{m+1}} given by the formula
#' \eqn{P_m X_{m+1} = \phi_{m1} X_m + \cdots + \phi_{mm} X_1}.
#'
#' @param X A numeric or complex atomic vector representing the time series data.
#' The time series should be stationary,  meaning its mean and covariance function should be independent of time.
#'
#' @returns The output of this function is an anonymous function. The returned function takes the value `m` as a parameter. The value `m` means that we use the last `m` values of the time series to predict the next value of the time series using the Durbin-Levinson algorithm. This function calculates the Durbin-Levinson algorithm, which then returns
#' a list with two components:
#' \item{phi}{A numeric atomic vector representing the calculated coefficients \eqn{\phi_{m1}, \dots, \phi_{mm}}.}
#' \item{nu}{A numeric value representing the mean squared error \eqn{\nu_m} of the one-step predictor.}
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
    "X must be numeric or complex atomic vector" = is.atomic(X) & (is.numeric(X) | is.complex(X)),
    "X must have more than one value" = length(X) > 1,
    "X may not contain NAs" = !any(is.na(X)),
    "X may not contain Inf or -Inf values" = !any(is.infinite(X))
  )

  warning("This algorithm works for stationary time series with zero-mean.\nFor any other time series, the results may be incorrect.")

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
    nu <- numeric(1)
    phi <- numeric(m)

    nu <- gamma[1]
    phi[1] <- gamma[2] / (gamma[1])
    nu <- nu * (1 - phi[1]**2)

    if (m == 1) {
      return(list(phi = phi, nu = nu))
    }

    for (i in 2:m) {
      phi[i] <- (gamma[i + 1] - sum(phi[1:(i - 1)] * gamma[i:2])) / nu
      phi[1:(i - 1)] <- phi[1:(i - 1)] - phi[i] * phi[(i - 1):1]
      nu <- nu * (1 - phi[i]**2)
    }

    return(list(phi = phi, nu = nu))
  }
}
