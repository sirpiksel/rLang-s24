#' @title  Generator for AR(p) processes
#'
#' @description `make_AR` generates a synthetic time series that follows an AR(p) model.
#'
#' @details
#' The AR(p) model can be defined by the equation
#'
#' \deqn{X_t = \phi_1 X_{t-1} + \phi_2 X_{t-2} + \dots + \phi_p X_{t-p} + Z_t,}
#'
#' where \eqn{X_t} represents the time series at time \eqn{t} and \eqn{Z_t} is white noise with a mean of zero and a variance of \eqn{\sigma^2}.
#'
#' @param t An integer specifying the desired length of the time series.
#'
#' @param phi A numeric or complex atomic vector of length `p` containing the coefficients of the AR(p) process.
#'
#' @param sigma A numeric value representing the standard deviation of the white noise, with a default value of 1.
#'
#' @param start A numeric atomic vector containing the first `p` observations, which are 0 by default.
#'
#' @returns An atomic numeric vector containing the generated AR(p) time series.
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @examples
#' # Example 1: Simple AR(1) model
#' ar_one <- make_AR(t = 10, phi = -0.7, sigma = 9, start = 7)
#' zeitreihen::plot(ar_one, 100)
#' sample_ACVF(ar_one)
#'
#' # Example 2: Simple AR(2) model
#' ar_two <- make_AR(t = 10, phi = c(1 + 1i, 0))
#' zeitreihen::plot(ar_two, 100)
#' sample_ACVF(ar_two)
#' 
#' # Example 3: Simple AR(3) model
#' ar_three <- make_AR(t = 10, phi = c(6, 9, 1))
#' zeitreihen::plot(ar_three, 100)
#' sample_ACVF(ar_three)
#' 
#' # Example 4: Simple AR(4) model
#' ar_four <- make_AR(t = 10, phi = c(0, 4, 1, 2), start = c(9, 5))
#' zeitreihen::plot(ar_four, 100)
#' sample_ACVF(ar_four)
#'
#' @export
make_AR <- function(t, phi, sigma = 1, start = numeric(length(phi))) {
  stopifnot(
    "t must be a value of length 1" = (is.atomic(t) & length(t) == 1),
    "t must not be infinite" = !is.infinite(t),
    "t must not be NA" = !is.na(t),
    "t must be numeric" = is.numeric(t),
    "t must be an integer" = t %% 1 == 0,
    "t must be positive" = t > 0,
    "phi must be an atomic vector" = is.atomic(phi),
    "phi must have positive length" = length(phi) > 0,
    "phi may not contain NAs" = !any(is.na(phi)),
    "phi may not contain Inf or -Inf values" = !any(is.infinite(phi)),
    "The values of phi must be numeric or complex" = (is.numeric(phi) | is.complex(phi)),
    "t must be greater than the length of phi" = t > length(phi),
    "The polynomial phi must have no roots on the unit circle" = all(abs(polyroot(c(1, -phi))) != 1),
    "start must be an atomic vector" = is.atomic(start),
    "start must have positive length" = length(start) > 0,
    "length of start must be smaller than or equal to length of phi" = length(phi) >= length(start),
    "start may not contain NAs" = !any(is.na(start)),
    "start may not contain Inf or -Inf values" = !any(is.infinite(start)),
    "The values of start must be numeric or complex" = (is.numeric(start) | is.complex(start)),
    "sigma must be a value of length 1" = (is.atomic(sigma) & length(sigma) == 1),
    "sigma must be finite" = !is.infinite(sigma),
    "sigma must not be NA" = !is.na(sigma),
    "sigma must be numeric" = is.numeric(sigma),
    "sigma must be positive" = sigma > 0
  )

  p <- length(phi)
  X <- c(start, numeric(t - length(start)))
  Z <- stats::rnorm(t, mean = 0, sd = sigma)

  for (i in (p + 1):t) {
    X[i] <- sum(phi * X[(i - 1):(i - p)]) + Z[i]
  }

  return(X)
}
