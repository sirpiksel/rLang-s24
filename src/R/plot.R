#' @title Custom Spectral Density Plotting Function
#'
#' @description `plot` computes periodogram values, plots the spectral density and contains a legend. Samples over \eqn{(0, \pi]} are selected and errors are managed.
#'
#' @details
#' The function computes periodogram values using the `periodogram` function from the `stats` package. It then generates a plot of the spectral density estimate with the `plot` function from the `graphics` package, including a legend to distinguish between observed and predicted values. By default the samples are evaluated at evenly spaced points over the left-open interval \eqn{(0, \pi]}, the interval can be changed using the `from` and `to` parameters. Additionally, the function incorporates error handling to manage issues such as invalid inputs or missing packages.
#'
#' @param X A numeric or complex atomic vector representing the time series data, including the predictions.
#' @param n An integer specifying the number of samples to evaluate the periodogram with.
#' @param from Startpoint for the half-open interval greater than `from`.
#' @param to Endpoint for the half-open interval less than or equal to `to`.
#'
#' @return A line plot displaying the spectral density estimate of the time series.
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @examples
#' # Generate a random time series with 100 elements
#' X <- rnorm(100)
#'
#' # Plot the spectral density
#' zeitreihen::plot(X, 250)
#'
#' @export
plot <- function(X, n, from = 0, to = pi) {
  stopifnot(
    "X must be an atomic vector" = is.atomic(X),
    "X must have positive length" = length(X) > 0,
    "X may not contain NAs" = !any(is.na(X)),
    "X may not contain Inf or -Inf values" = !any(is.infinite(X)),
    "X must be numeric or complex" = (is.numeric(X) | is.complex(X)),
    "X may not contain Inf or -Inf values" = !any(is.infinite(X)),
    "n must be numeric" = is.numeric(n) & is.finite(n),
    "n must be an integer" = n %% 1 == 0 & length(n) == 1,
    "from may not be NA" = !is.na(from),
    "from must be numeric" = is.numeric(from),
    "from may not be an Inf or -Inf value" = !is.infinite(from),
    "from must be greater or equal to 0" = 0 <= from,
    "to may not be NA" = !is.na(to),
    "to must be numeric" = is.numeric(to),
    "to may not be an Inf or -Inf value" = !is.infinite(to),
    "to must be smaller or equal to pi" = to <= pi,
    "from must be smaller than to" = from < to
  )

  lambda <- seq(from = from, to = to, length.out = n + 1)[-1]

  warn <- getOption("warn")
  options(warn = -1)
  samples <- sapply(lambda, \(i) {
    periodogram(X, i)
  })
  options(warn = warn)

  graphics::plot(lambda, samples, type = "l", col = "blue", xlab = "Sample", ylab = "Estimate", main = "The spectral density estimate of X")
}
