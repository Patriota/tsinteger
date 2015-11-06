
#' Function nginar
#'
#' Fit in inar model to a univariate time series by yule-walker method
#'
#' @param x a numeric vector or time series.
#' @param order.max the integer component p is the INAR order.
#' @param mu the mean of the geometric distribution.
#'
#'@return
#'
#'@examples
#'
#'data(sexoffences)
#'mean(sexoffences)
#'var(sexoffences)
#'nginar(sexoffences)
#'
#' @export
nginar <-
  function(x, order.max = 1,series=NULL)
  {
    if (is.null(series))
      series <- deparse(substitute(x))
    xfreq <- frequency(x)
    n <- length(x)

    r <- acf(x, plot = FALSE)$acf[2:(order.max+1)]
    R <- diag(order.max)
    for(i in 1:order.max){
      for(j in 1:order.max){
        if(i!=j){
          R[i,j] <- r[abs(i-j)]
        }
      }
    }

    coef <- round(solve(R, r), 4)
    mubar <- mean(x)


    res <- list(order = order.max, ar = coef, var.pred = mubar,
                x.mean = mubar, bic= NULL, aicc= NULL,aic = NULL, n.used = n, order.max = order.max,
                partialacf = NULL, resid = NULL, method = "yule-walker", series = series,
                frequency = xfreq, call = match.call())
    class(res) <- "ar"
    res
  }
