#' Density of the Triangular Distribution

dtriang <- function(x, min, max, mode) {

  if (any(min >= max)) {
    stop("min must be strictly smaller than max")
  }
  if (any(mode < min | mode > max)) {
    stop("mode must be between min and max")
  }

  dens <- numeric(length(x))

  left  <- x >= min & x <= mode
  right <- x > mode & x <= max

  dens[left]  <- 2 * (x[left] - min) /
    ((max - min) * (mode - min))

  dens[right] <- 2 * (max - x[right]) /
    ((max - min) * (max - mode))

  dens
}






ptriang <- function(q, min, max, mode) {

  if (any(min >= max)) {
    stop("min must be strictly smaller than max")
  }
  if (any(mode < min | mode > max)) {
    stop("mode must be between min and max")
  }

  prob <- numeric(length(q))

  left  <- q >= min & q <= mode
  right <- q > mode & q <= max

  prob[q > max] <- 1

  prob[left] <-
    (q[left] - min)^2 / ((max - min) * (mode - min))

  prob[right] <-
    1 - (max - q[right])^2 / ((max - min) * (max - mode))

  prob
}



qtriang <- function(p, min, max, mode) {

  if (any(p < 0 | p > 1)) {
    stop("p must be between 0 and 1")
  }
  if (any(min >= max)) {
    stop("min must be strictly smaller than max")
  }
  if (any(mode < min | mode > max)) {
    stop("mode must be between min and max")
  }

  pc <- (mode - min) / (max - min)
  q  <- numeric(length(p))

  left <- p <= pc

  q[left]  <- min + sqrt(p[left] *
                           (max - min) * (mode - min))

  q[!left] <- max - sqrt((1 - p[!left]) *
                           (max - min) * (max - mode))

  q
}



rtriang <- function(n, min, max, mode) {
  qtriang(runif(n), min, max, mode)
}
