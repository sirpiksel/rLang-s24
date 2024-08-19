#' @title Custom Time Series Plotting Function
#'
#' @description `plot` generates a custom line plot of a time series vector, highlighting the last `h` values as predictions. The plot displays the time series in blue and the predictions in red, with an appropriate legend.
#'
#' @details
#' This function is designed to visualize a time series, where the last `h` values are considered predictions and are highlighted in a different color. The main part of the time series is plotted in blue, while the predicted values are plotted in red. The function automatically generates a legend to distinguish between the observed and predicted values.
#'
#' @param X A numeric vector representing the time series data, including the predictions.
#' @param samples an integer specifying the number of samples to evaluate the periodogram with.
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
plot <- function(X, samples) {
  stopifnot(
    "X must be an atomic vector" = is.atomic(X),
    "X must have positive length" = length(X) > 0,
    "X may not contain NAs" = !any(is.na(X)),
    "X may not contain Inf or -Inf values" = !any(is.infinite(X)),
    "X must be numeric or complex" = (is.numeric(X) | is.complex(X)),
    "X may not contain Inf or -Inf values" = !any(is.infinite(X)),
    "samples must be numeric" = is.numeric(samples),
    "samples must be an integer" = samples %% 1 == 0 & length(samples) == 1
  )

  lambdas <- seq(from = -pi, to = pi, length.out = samples + 1)[-1]

  I <- sapply(lambdas, \(i) {
    periodogram(X, i)
  })

  graphics::plot(1:length(I), I, type = "l", col = "blue", xlab = "Index", ylab = "Value", main = "The spectral density estimate of X")
}
