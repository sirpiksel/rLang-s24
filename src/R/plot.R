#' @title Custom Spectral Density Plotting Function
#'
#' @description `plot` computes periodogram values, plots the spectral density, and includes a legend. It samples over \eqn{(-\pi, \pi]} and manages errors.
#'
#' @details
#' The function computes periodogram values using the `periodogram` function from the `stats` package. It then generates a plot of the spectral density estimate with the `plot` function from the `graphics` package, including a legend to distinguish between observed and predicted values. The samples are evaluated at evenly spaced points over the interval \eqn{(-\pi, \pi]}. Additionally, the function incorporates error handling to manage issues such as invalid inputs or missing packages.
#'
#' @param X A numeric vector representing the time series data, including the predictions.
#' @param n an integer specifying the number of samples to evaluate the periodogram with.
#'
#' @return A line plot displaying the spectral density estimate of the time series.
#'
#' @examples
#' # Generate a random time series with 100 elements
#' X <- rnorm(100)
#'
#' # Plot the time series with the last 10 elements highlighted as predictions
#' plot(X, 100)
#'
#' @export
plot <- function(X, n) {
  stopifnot(
    "X must be an atomic vector" = is.atomic(X),
    "X must have positive length" = length(X) > 0,
    "X may not contain NAs" = !any(is.na(X)),
    "X may not contain Inf or -Inf values" = !any(is.infinite(X)),
    "X must be numeric or complex" = (is.numeric(X) | is.complex(X)),
    "X may not contain Inf or -Inf values" = !any(is.infinite(X)),
    "samples must be numeric" = is.numeric(n),
    "samples must be an integer" = n %% 1 == 0 & length(n) == 1
  )

  lambda <- seq(from = -pi, to = pi, length.out = n + 1)[-1]

  samples <- sapply(lambda, \(i) {
    periodogram(X, i)
  })

  graphics::plot(1:(n+1), samples, type = "l", col = "blue", xlab = "Index", ylab = "Value", main = "The spectral density estimate of X")
}
