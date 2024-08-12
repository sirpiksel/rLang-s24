simulate <- function(X, h = 1, model, param, plot = false) {
  stopifnot(
    "X must be numeric" = is.vector(X) & all(is.numeric(X)),
    "h must be at least 1" = 0 < h & h == as.integer(h),
    "incompatible model" = T
    # TODO: more tests
  )

  for (i in 1:h) {
    X = c(X, model(X, param))
  }

  if(plot) {
    zeitreihen::plot(X, h)
  }

  return(X)
}
