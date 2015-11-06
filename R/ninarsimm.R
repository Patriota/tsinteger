
#' Function nginar.sim
#'
#' Simulate from an Inar model
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
#' @export
nginar.sim <- function(n, alpha,mu, n.start=150){
  length. <- n + n.start
  error.nginar <- function(length.,alpha,mu){
    epsilon <- rep(NA, times = n)
    a = (alpha*mu)/(mu - alpha)
    for(i in 1:length.){
      u <- runif(1,0,1)
      ifelse(u < a,epsilon[i] <- rgeom(1, 1 - (alpha/(1 + alpha))),epsilon[i] <- rgeom(1, 1 - (mu/(1+mu))))
    }
    return(epsilon)
  }

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





