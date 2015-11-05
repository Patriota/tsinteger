
#' Function error.nginar
#'
#' definir
#'
#' @param n the length of outputs series. A strictly positive integer.
#' @param alpha a vector of INAR coefficients.
#' @param mu a vector.
#'
#'@return Resultados
#'
#'@examples
#'
#'\dontrum{
#'function (blah)
#'}
#' @export
error.nginar <- function(n,alpha,mu){
  epsilon <- rep(NA, times = n)
  a = (alpha*mu)/(mu - alpha)
  for(i in 1:n){
    u <- runif(1,0,1)
    ifelse(u < a,epsilon[i] <- rgeom(1, 1 - (alpha/(1 + alpha))),epsilon[i] <- rgeom(1, 1 - (mu/(1+mu))))
  }
  return(epsilon)
}
