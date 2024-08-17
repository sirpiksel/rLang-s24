#' @title  Ar(p) generator (`make_ar`)
#'
#' @description `make_ar` generates a synthetic time series that follows an Ar(p) model.
#'
#' @details
#' The Ar(p) model can be defined by the equation
#' \deqn{X_t = \phi_1 X_{t-1} + \phi_2 X_{t-2} + \dots + \phi_p X_{t-p} + Z_t}
#' where \eqn{t} is the time and \eqn{Z_t} is white
#' noise with mean zero and variance \eqn{\sigma^2}.
#'
#' @param t An integer that gives the desired length of the time series.
#'
#' @param phi A numeric or complex vector of length \eqn{p} that contains the coefficients of the Ar(p) process.
#'
#' @param sigma A numeric value representing the variance of the white noise. By default it is set to 1.
#'
#' @param start A numeric vector, that contains the first \eqn{p} observations. By default these observations are set to 0.
#'
#' @returns A numeric vector that contains the generated Ar(p) time series.
#'
#' @examples
#' # Generate a sample AR(1) time series of length 100 with coefficient \eqn{\phi=-0.7}.
#' ar_one <- make_ar(100, -0.7)
#' ts.plot(ar_one)
#' acf(ar_one)
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @export
make_ar <- function(t, phi, sigma = 1, start = numeric(length(phi))) {
  stopifnot(
    "t must be a value of length 1" = (is.atomic(t) & length(t) == 1),
    "t must be finite" = !is.infinite(t),
    "t must not be NA" = !is.na(t),
    "t must be an integer" = t %% 1 == 0,
    "t must be positive" = t > 0,
    "phi must be an atomic vector" = is.atomic(phi),
    "phi must have positive length" = length(phi) > 0,
    "phi may not contain NAs" = !any(is.na(phi)),
    "phi may not contain Inf or -Inf values" = !any(is.infinite(phi)),
    "The values of phi must be numeric or complex" = (is.numeric(phi) | is.complex(phi)),
    "t must be greater than the length of phi" = t > length(phi),
    "The polynomial phi must have no roots on the unit circle" = all(abs(polyroot(c(1, -phi))) != 1)
  )

  p <- length(phi)
  X <- c(start, numeric(t - length(start)))
  Z <- rnorm(t, mean = 0, sd = sigma)

  for (i in (p + 1):t) {
    X[i] <- sum(phi * X[(i - 1):(i - p)]) + Z[i]
  }

  return(X)
}

#' @title  MA(q) generator (`make_ma`)
#'
#' @description `make_ma` generates a synthetic time series that follows a MA(q) model.
#'
#' @details
#' The MA(q) model can be defined by the equation
#' \deqn{X_t = Z_t+\theta_1 Z_{t-1} + \theta_2 Z_{t-2} + \dots + \theta_q Z_{t-q}}
#' where \eqn{t} is the time and \eqn{Z_t} is white
#' noise with mean zero and variance \eqn{\sigma^2}.
#'
#' @param t An integer that gives the desired length of the time series.
#'
#' @param theta A numeric or complex vector of length \eqn{q} that contains the coefficients of the MA(q) process.
#'
#' @param sigma A numeric value representing the variance of the white noise. By default it is set to 1.
#'
#' @returns A numeric vector that contains the generated Ma(q) time series.
#'
#' @examples
#' # Generate a sample MA(1) time series of length 100 with coefficient \eqn{\phi=-0.25}.
#' ma_one <- make_ma(100, -0.25)
#' ts.plot(ma_one)
#' acf(ma_one)
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @export
make_ma <- function(t, theta, sigma = 1) {
  stopifnot(
    "t must be a value of length 1" = (is.atomic(t) & length(t) == 1),
    "t must be finite" = !is.infinite(t),
    "t must not be NA" = !is.na(t),
    "t must be an integer" = t %% 1 == 0,
    "t must be positive" = t > 0,
    "theta must be an atomic vector" = is.atomic(theta),
    "theta must have positive length" = length(theta) > 0,
    "theta may not contain NAs" = !any(is.na(theta)),
    "theta may not contain Inf or -Inf values" = !any(is.infinite(theta)),
    "The values of theta must be numeric or complex" = (is.numeric(theta) | is.complex(theta)),
    "t must be greater than the length of theta" = t > length(theta),
    "sigma must be a value of length 1" = (is.atomic(sigma) & length(sigma) == 1),
    "sigma must be finite" = !is.infinite(sigma),
    "sigma must not be NA" = !is.na(sigma),
    "sigma must be numeric" = is.numeric(sigma),
    "sigma must be positive" = sigma > 0
  )

  q <- length(theta)
  X <- numeric(t)
  Z <- rnorm(t + q, mean = 0, sd = sigma)

  for (i in (q + 1):(t + q)) {
    X[i - q] <- sum(theta * Z[(i - 1):(i - q)]) + Z[i]
  }

  return(X)
}
