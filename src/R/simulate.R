#' Simulation Function for Timeseries
#'
#' This function starts the simulation process, provided a time series `X` it returns `h`-amount of predictions `P`.
#'
#' @param X time series as a numeric vector.
#' @param h integer specifying the number of predictions to be computed. Set to `1` by default.
#' @param model function of the zeitreihen package to use as a model. Set to `zeitreihen::auto_model` by default.
#' @param lag optionally overwrite default model lag to compute for time series.
#' @param coeffs optionaly overwrite default model coeffs to compute for time series.
#' @param plot optionally output a plot of the time series and its predictions. Set to `False` by default.
#'
#' @return `P` predictions as a numerical vector
#'
#' @examples
#' X <- as.numeric(tsdl::tsdl[[1]])
#' # minimal function call with 31 predictions
#' zeitreihen::simulate(X, 31)
#' # output a plot
#' zeitreihen::simulate(X, 31, plot = True)
#' # specify AR model and force p = 1
#' zeitreihen::simulate(X, 14, zeitreihen::AR, 1)
#'
#' @references Brockwell, P.J., Davis, R.A. (2016) \emph{Introduction to Time Series and Forecasting}. Springer.
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
