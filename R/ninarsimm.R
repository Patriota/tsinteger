
#' Function nginar.sim
#'
#' definir
#'
#' @param n A strictly positive integer.
#' @param alpha a vector of INAR coefficients.
#' @param mu a vector.
#' @param n.start the length of 'burn-m' period. If na, the default, a reasonable valve is computed.
#'
#'@return Resultados
#'
#'@examples
#'
#'\dontrum{
#'function (blah)
#'}
#' @export
nginar.sim <- function(n, alpha,mu, n.start=150){
  length. <- n + n.start
  x <- rep(NA, times = length.)
  epsilon <- error.nginar(length.,alpha,mu)
  x[1] = rgeom(1, 1-(mu/(1+mu)))
  for (t in 2:length.){
    if(x[t-1]==0){
      x[t] = epsilon[t]
    }
    else{
      x[t] = rnbinom(n = 1, size = x[t-1], prob = 1 - (alpha/(1+alpha))) + epsilon[t]
    }
  }
  ts(x[(n.start+1):length.],frequency = 1,start=1)
}





