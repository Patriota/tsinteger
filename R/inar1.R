
#' Function poinar
#'
#' Fit in inar model to a univariate time series by yule-walker method
#'
#' @param x a numeric vector or time series.
#' @param order.max a one dimensional integer vestor giving the order of the model to fit. This value corresponds the INAR order.
#' @param series name for the series.
#'
#'@return Resultados
#'
#'@references
#'
#'Du, J.G. and Li,Y.(1991):
#'The integer-valued autorregressive (INAR(p)) model.
#'\emph{Journal of time series analysis} \bold{12}, 129--142.
#'
#'Freeland RK. Statistical analysis of discrete time series with applications to the analysis of workers compensation
#'claims data [unpublished doctoral dissertation]. Vancouver (Canada): University of British Columbia; 1998.
#'
#'@examples
#'data(claims)
#'mean(claims[,5])
#'var(claims[,5])
#'var(claims[,5])/mean(claims[,5])  # dispersion index
#'acf(claims[,5])
#'pacf(claims[,5])
#'poinar(claims[,5], 1)
#'
#' @export
poinar <-
  function(x, order.max,series=NULL)
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
    xbar <- mean(x)
    meanerrohat <- xbar*(1-sum(coef))
    Vp.1 <- ((1-sum(coef^2))/(1-sum(coef)))

    AICc. <- n*log(Vp.1) + n*((1+order.max/n)/(1-(order.max+2)/n))
    AIC. <-  -n*log(Vp.1) + 2*order.max
    BIC. <-  n*log(Vp.1) + (order.max/n)*log(n)

    res <- list(order = order.max, ar = coef, var.pred = meanerrohat,
                x.mean = xbar, bic= BIC., aicc=AICc.,aic = AIC., n.used = n, order.max = order.max,
                partialacf = NULL, resid = NULL, method = "yule-walker", series = series,
                frequency = xfreq, call = match.call())
    class(res) <- "ar"
    res
  }


alpha=c(0.5)
order.max=1
n=300
lambda=2
n.start = 150
