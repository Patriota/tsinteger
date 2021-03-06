
#' Function poinar.sim
#'
#' Simulate from an Inar model
#'
#' @param n the length of outputs series. A strictly positive integer.
#' @param order.max the integer component p is the INAR order.
#' @param alpha a vector of INAR coefficients.
#' @param lambda the mean of the poisson distribution.
#' @param n.start the length of 'burn-in' period. If na, the default, a reasonable valve is computed.
#'
#'
#'@return A time-series object of class "ts".
#'
#'@seealso   \code{\link{poinar}}
#'
#'@references
#'  Du, J.G. and Li,Y.(1991):
#'  The integer-valued autorregressive (INAR(p)) model.
#'  \emph{Journal of time series analysis} \bold{12}, 129--142.
#'
#'@examples
#'# A Poisson INAR simulation
#'ts.sim <- poinar.sim(n = 100, order.max = 2, alpha = c(0.1,0.4),lambda = 2, n.start=200)
#'ts.plot(ts.sim)
#'
#' @export
poinar.sim <- function(n, order.max, alpha,lambda, n.start=NA){
  length. <- n + n.start
  x <- rep(NA, times = length.)
  error <- rpois(length., lambda)
  for (i in 1:order.max) {
    x[i] <- error[i]
  }
  for (t in (order.max + 1):length.) {
    x[t] <- 0
    for (j in 1:order.max) {
      x[t] <- x[t] + rbinom(1, x[t - j], alpha[j])
    }
    x[t] <- x[t] + error[t]
  }
  ts(x[(n.start+1):length.],frequency = 1,start=1)
}
