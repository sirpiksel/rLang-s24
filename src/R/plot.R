#' @title Custom Time Series Plotting Function
#'
#' @description `plot` generates a custom line plot of a time series vector, highlighting the last `h` values as predictions. The plot displays the time series in blue and the predictions in red, with an appropriate legend.
#'
#' @details
#' This function is designed to visualize a time series, where the last `h` values are considered predictions and are highlighted in a different color. The main part of the time series is plotted in blue, while the predicted values are plotted in red. The function automatically generates a legend to distinguish between the observed and predicted values.
#'
#' @param X A numeric vector representing the time series data, including the predictions.
#'
#' @param h An integer specifying the number of predictions to be highlighted in the plot. The value of `h` must be a positive integer and should not exceed the length of `X_hat`.
#'
#' @return A line plot displaying the time series with the last `h` values highlighted as predictions in red.
#'
#' @examples
#' # Generate a random time series with 100 elements
#' X_hat <- rnorm(100)
#'
#' # Plot the time series with the last 10 elements highlighted as predictions
#' plot(X_hat, 10)
#'
#' @export
plot <- function(X, h) {
  stopifnot(
    "h is not a numeric integer" = n == as.integer(h),
    "h cannot be greater than the length of X_hat" = h <= length(X)
  )

  P <- (length(X) - h + 1):length(X)

  plot(1:length(X), X, type = "l", col = "blue", xlab = "Index", ylab = "Value", main = "Custom Line Plot")
  lines(P, X[P], col = "red")
  legend("topright", legend = c("X", "P"), col = c("blue", "red"), lty = 1)
}
