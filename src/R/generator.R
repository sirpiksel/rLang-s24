#' @export
make_ar <- function(t, phi, sigma = 1, start = numeric(length(phi))) {
    stopifnot("t must be a value of length 1" = (is.atomic(t) & length(t) == 1),
              "t must be finite" = !is.infinite(t),
              "t must not be NA" = !is.na(t),
              "t must be an integer" = t %% 1 == 0,
              "t must be positive" = t > 0,
              "phi must be an atomic vector" = is.atomic(phi),
              "phi must have positive length" = length(phi) > 0,
              "phi may not contain NAs" = !any(is.na(phi)),
              "phi may not contain Inf or -Inf values" = !any(is.infinite(phi)),
              "The values of phi must be numeric or complex" = (is.numeric(phi) | is.complex(phi)),
              "t must be greater than the length of phi"= t > length(phi),
              "The polynomial phi must have no roots on the unit circle" = all(abs(polyroot(c(1, -phi))) != 1))
    
    p <- length(phi)
    X <- c(start, numeric(t - length(start)))
    Z <- rnorm(t, mean = 0, sd = sigma)
    
    for (i in (p + 1):t) {
        X[i] <- sum(phi * X[(i - 1):(i - p)]) + Z[i]
    }
    
    return(X)
}


#' @export
make_ma<-function(t, theta, sigma = 1) {
    stopifnot("t must be a value of length 1" = (is.atomic(t) & length(t) == 1),
              "t must be finite" = !is.infinite(t),
              "t must not be NA" = !is.na(t),
              "t must be an integer" = t %% 1 == 0,
              "t must be positive" = t > 0,
              "theta must be an atomic vector" = is.atomic(theta),
              "theta must have positive length" = length(theta) > 0,
              "theta may not contain NAs" = !any(is.na(theta)),
              "theta may not contain Inf or -Inf values" = !any(is.infinite(theta)),
              "The values of theta must be numeric or complex" = (is.numeric(theta) | is.complex(theta)),
              "t must be greater than the length of theta"= t > length(theta),
              "sigma must be a value of length 1" = (is.atomic(sigma) & length(sigma) == 1),
              "sigma must be finite" = !is.infinite(sigma),
              "sigma must not be NA" = !is.na(sigma),
              "sigma must be numeric" = is.numeric(sigma),
              "sigma must be positive" = sigma > 0,
              
    q <- length(theta)
    X <- numeric(t)
    Z <- rnorm(t + q, mean = 0, sd = sigma)
  
    for (i in (q + 1):(t + q)) {
        X[i - q] <- sum(theta * Z[(i - 1):(i - q)]) + Z[i]
    }
    
    return(X)
}
