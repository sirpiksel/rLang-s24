#' Custom Time Series Plotting Function
#'
#' This function plots a vector `X` as a line graph, with the last `h` indicated to be the predictions.
#'
#' @param X A numeric vector.
#' @param h An integer specifying the number of predictions in X.
#' @return A plot of X with `h` predictions marked in red.
#' @examples
#' X <- rnorm(100)          # Generate a random vector of 100 elements
#' zeitreihen::plot(X, 10)  # Plot with the last 10 elements in a different color
#' @export
plot <- function(X_hat, h) {
  stopifnot(
    "h is not a numeric integer" = n == as.integer(h),
    "h cannot be greater than the length of X_hat" = h <= length(X_hat)
  )

  X <- 1:(length(X_hat) - h)
  P <- (length(X_hat) - h + 1):length(X_hat)

  plot(1:length(X_hat), X_hat, type = "l", col = "blue", xlab = "Index", ylab = "Value", main = "Custom Line Plot")
  lines(P, X_hat[P], col = "red")
  legend("topright", legend = c("X", "P"), col = c("blue", "red"), lty = 1)
}
