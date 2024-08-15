#' @title Time Series Simulation Function
#'
#' @description `simulate` generates future values (predictions) for a given time series by applying a specified model. The function can also plot the original series with the predictions appended if desired.
#'
#' @details
#' This function simulates future observations of a time series by using a specified model from the `zeitreihen` package. The user can specify the number of predictions (`h`), the model to be used, the lag order, and the model coefficients. If `plot` is set to `TRUE`, the function will generate a plot showing the time series with the predictions highlighted.
#'
#' @param X A numeric vector representing the time series data.
#'
#' @param h An integer specifying the number of predictions to be computed. The default value is `1`.
#'
#' @param model A function from the `zeitreihen` package to be used as the predictive model. The default is `zeitreihen::auto_model`.
#'
#' @param lag An integer specifying the lag order for the model. This parameter can be used to override the default lag of the chosen model.
#'
#' @param coeffs A numeric vector specifying the coefficients for the model. This parameter can be used to override the default coefficients of the chosen model.
#'
#' @param plot A logical value indicating whether to plot the time series along with its predictions. The default value is `FALSE`.
#'
#' @return A numeric vector containing the `h` predicted values appended to the original time series.
#'
#' @examples
#' X <- rnorm(100) # Generate a random time series
#'
#' # Generate 31 predictions using the default model
#' predictions <- simulate(X, 31)
#'
#' # Generate predictions and plot the results
#' predictions <- simulate(X, 31, plot = TRUE)
#'
#' # Use a specific AR model with a lag of 1
#' predictions <- simulate(X, 14, model = zeitreihen::AR, lag = 1)
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
#'
#' @export
simulate <- function(X, h = 1, model = zeitreihen::auto_model, lag, coeffs, plot = False) {
  stopifnot(
    "X must be numeric vector." = is.vector(X) & all(is.numeric(X)),
    "h must be integer of at least 1." = 0 < h & h == as.integer(h),
    "incompatible model. Only accepts models from zeitreihen package" = exists(deparse(substitute(model)), where = "package:zeitreihen", mode = "function"),
    "lag must be integer." = lag == as.integer(lag),
    "coeffs must be numeric vector." = is.vector(coeffs) & all(is.numeric(coeffs)),
    "plot must be a boolean." = is.boolean(plot)
  )

  for (i in 1:h) {
    X <- c(X, model(X, lag, coeffs))
  }

  if (plot) {
    zeitreihen::plot(X, h)
  }

  return(tail(X, h))
}
