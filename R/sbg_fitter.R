## Functions for shifted beta geometric model
## Created by ES (eli.manfred.stein@gmail.com) on 6.26.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 6.30.2013

sbg.lik <- function(lpars, num.customers) {

  ## Calculates log likelihood for sbg model

  ## Arguments
  ## lpars, naturnal logarithm of paramters of sbg model
  ## incremental.churners, incremental churners (data)
  ## survivors, survivors (data)

  ## Returns

  if(missing(lpars)) stop("Argument 'lpars' is required. \n")
  if(missing(num.customers)) stop("Argument 'num.customers' is required. \n")

  if(!is.numeric(lpars) || length(lpars) != 2) stop("Argument 'lpars' must be a numeric vector of length 2. \n")
  if(!is.numeric(num.customers)) stop("Argument 'num.customers' must be numeric. \n")

  ## Calculate incremental churners and seperate customers
  incremental.churners <- -diff(num.customers)
  survivors <- min(num.customers)

  ## Extract Parameters
  a <- exp(lpars['Alpha'])
  b <- exp(lpars['Beta'])

  ## Calculate times of churn
  t.churn <- 1:length(incremental.churners)
  t.survive <- max(t.churn)

  ## Calculate log probabilities
  lp.survive <- lbeta(a,b+t.survive) - lbeta(a,b)
  lp.churn <- lbeta(a+1,b+t.churn-1) - lbeta(a,b)

  contrib.survivors <- survivors*lp.survive
  contrib.churners <- crossprod(lp.churn,incremental.churners)
  
  lik <- contrib.survivors + contrib.churners
  return(lik)
  
}


sbg.fitter <- function(num.customers, pars.start, hessian,
                       method, ...) {

  ## Fits an sbg model for customers in a single cohort

  ## Arguments
  ## num.customers, numeric vector of number of customers in each period
  ## hessian, to indicate whether optim should return hessian
  ## method, method argument for optim
  ## ..., additional arguments for optim

  ## Returns
  ## alpha and beta, parameters for shifted beta geometric model
  ## message, quoted message from optim

  if(missing(num.customers)) stop("Argument 'num.customers' is required. \n")
  if(missing(pars.start)) stop("Argument 'pars.start' is required. \n")
  if(missing(hessian)) stop("Argument 'hessian' is required. \n")
  if(missing(method)) stop("Argument 'method' is required. \n")

  if(!is.numeric(num.customers)) stop("Argument 'num.customers' must be numeric. \n")
  if(!is.numeric(pars.start) || length(pars.start) !=2 || pars.start <= 0 ) stop("Argument 'pars.start' must be a positive numeric vector of length 2. \n")
  if(!is.logical(hessian)) stop("Argument 'hessian' must be either TRUE or FALSE. \n")
  if(!is.element(method,c("Nelder-Mead", "BFGS", "CG", "SANN", "Brent"))) stop("Argument 'method' can only take the values 'Nelder-Mead','BFGS','CG','SANN', or 'Brent.' \n")


  lpars.start <- log(pars.start)

  ## Check if start is okay

  test.start.val <- sbg.lik(lpars.start,num.customers)

  if(!is.finite(test.start.val)) stop("The likelihood function could not be optimized because log-likelihood function is not finite at the starting values specified. Select other starting values. \n")
  
  ## Optimize

  opt.out <- tryCatch(
                      optim(par=lpars.start,
                            fn=sbg.lik,
                            num.customers=num.customers,
                            method=method,
                            hessian=hessian,
                            control=list(fnscale=-1),
                            ...),
                      error=optim.error.handler
                      )

  if(!is.null(opt.out)) {

    pars.mle <- exp(opt.out$par)
    names(pars.mle) <- c('Alpha','Beta')
    
    convergence <- opt.out$convergence

    if(hessian) {
      
      hessian.opt <- opt.out$hessian

      res <- list(pars.mle=pars.mle,
                  hessian.opt=hessian.opt,
                  convergence=convergence)
      
    }
    else res <- list(pars.mle=pars.mle,
                     convergence=convergence)

    return(res)
  }
}
