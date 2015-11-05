
#' Function poinar.sim
#'
#' Simulate from an Inar model
#'
#' @param n the length of outputs series. A strictly positive integer.
#' @param order.max the integer component p is the INAR order.
#' @param alpha a vector of INAR coefficients.
#' @param lambda the mean of the poisson distribution.
#' @param n.start the length of 'burn-m' period. If na, the default, a reasonable valve is computed.
#'
#'
#'@return For \code{inar} and its methods a list of class \code{"inar"} with the following elements:
#'
#'@examples
#'
#'\dontrum{
#'function (blah)
#'}
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
