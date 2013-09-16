## Functions for shifted beta geometric model
## Created by ES (eli.manfred.stein@gmail.com) on 6.28.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 8.21.2013
#'
#' @title shifted Beta Geometric Model
#' @name sbg
#'
#' @author Eli Stein (eli.manfred.stein@@gmail.com)
#'
#' @description Maximum likelihood estimation of a shifted Beta Geometric model.
#'
#' @param num.customers A numeric vector indicating the number of subscribing customers in each period.
#' @param pars.start Starting values for the parameters alpha and beta. Order assumed to be c(alpha, beta).
#' @param conf.int Logical parameter indicating whether confidence intervals for parameters should be returned.
#' @param conf.level Confidence level for confidence intervals, if \code{conf.int} is set to \code{TRUE}.
#' @param method Maximization method passed to \code{optim}.
#' @param holdout Number of periods of data to be withheld in the estimation of the model. Periods at the end of the dataset (the last periods to occur) are withheld first.
#' @param ... Additional arguments passed directly to \code{optim}.
#'
#' @return S3 \code{sbg} object, a list consisting of:
#'      \item{call}{The call used to create the object.}
#'      \item{convergence}{A scalar indicating the convergence status of the optimization routine. Code 0 indicates convergence was achieved. See documentation for \code{optim} for additional details.}
#'     \item{holdout.ind}{A binary vector indicating whether a period was heldout in the estimation of the model. The value 1 indicates the corresponding period was not used to estimate the model.}
#'    \item{par.info}{Numeric matrix describing the estimated model parameters.}
#'    \item{incremental.churn.info}{Numeric matrix describing the model fit as it relates to incremental churn.}
#'    \item{survivor.info}{Numeric matrix describing the model fit as it relates to surviving customers.}
#'    \item{retention.info}{Numeric matrix describing the model fit as it relates to retention rates.}
#'    \item{lv.moments}{Numeric matrix describing the model fit as it relates to the moments of the latent variable.}
#'    \item{predict.date.constructor}{If argument num.customers is of class \code{ts}, the function predict.date.constructor is used to generate labels for plotting.}
#'
#' @export
#'
#' @examples
#' num.customers <- c(1000,869,743,653,593,551,517,481)
#' ts.num.customers <- ts(num.customers,
#'                       frequency=4,start=c(2008,1))
#'
#' sbg.out <- sbg(ts.num.customers,holdout=2)
#' summary(sbg.out)
#'
#' @seealso \code{\link{sbg.sim}}

sbg <- function(num.customers,pars.start=c(1,1),
                        conf.int=FALSE, conf.level=.95,
                        method=c("Nelder-Mead", "BFGS", "CG", "SANN", "Brent"),
                        holdout=0, ...) UseMethod("sbg")


# @keyword internal
sbg.numeric <- function(num.customers,pars.start=c(1,1),
                        conf.int=FALSE, conf.level = .95,
                        method=c("Nelder-Mead", "BFGS", "CG", "SANN", "Brent"),
                        holdout=0, ...) {

  ## Part 1: Format Arguments & Ensure They Are Appropriate ##

  ## 1.1 Num.Customers
  if(missing(num.customers)) stop("Argument 'num.customers' must be provided. \n")
  if(!is.vector(num.customers)) stop("Argument 'num.customers' must be a vector. \n")
  if(!all(-diff(num.customers) >= 0)) stop("Argument 'num.customers' cannot contain increasing elements. \n")
  if(!all(floor(num.customers)==ceiling(num.customers))) stop("Argument 'num.customers' must contain only integers. \n")
  
  ## 1.2 Pars.Start
  if(!is.vector(pars.start,mode='numeric') || length(pars.start)!=2) stop("Argument pars.start must be a numeric vector of length 2. \n")
  if(is.null(names(pars.start))) {names(pars.start) <- c('Alpha','Beta')}
  
  ## 1.3 Conf.Int
  if(!is.element(conf.int,c(TRUE,FALSE))) stop("Argument 'conf.int' can only take the values TRUE or FALSE. \n")

  if(conf.int & length(num.customers) < 49) warning("Asymptotic confidence intervals are unreliable when you have data on less than 50 periods.")

  ## Conf.Level

  if(conf.level>1) conf.level <- conf.level/100
  if(conf.level >= 1 || conf.level <= 0) stop("Argument 'conf.level' must be in (0,1). \n")

  
  ## 1.4 Method
  method <- match.arg(method)
  if(!is.element(method,c("Nelder-Mead", "BFGS", "CG", "SANN", "Brent"))) stop("Argument 'method' can only take the values 'Nelder-Mead','BFGS','CG','SANN', or 'Brent.' \n")

  ## 1.5 Holdout
  if(floor(holdout) != ceiling(holdout)) stop("Argument 'holdout' must be an integer.")
  if(holdout > length(num.customers)-2) stop("Argument 'holdout' must be less than the number of periods minus two.")

  ## 1.6 Names(num.customers)
  if(is.null(names(num.customers))) names(num.customers) <- paste("Period",1:length(num.customers))
  
  ## Part 2: Estimate Parameters ##
  
  ## 2.1 Store Call, Assign Hessian
  call <- match.call()
  hessian <- conf.int
  assign("hessian",hessian,envir=.GlobalEnv) ## Assign hessian to global environment so it can be used to provide customized error messages

  ## 2.2 Form training (t.) and holdout (h.) datasets
  t.num.customers <- num.customers[1:(length(num.customers)-holdout)]

  if(holdout>0){
    
    h.num.customers <- num.customers[(length(t.num.customers)+1):length(num.customers)]
    holdout.ind <- c(rep(0,length(t.num.customers)),rep(1,length(h.num.customers)))
  } else {

    h.num.customers <- NULL
    holdout.ind <- rep(0,length(t.num.customers))
    
  }

  ## 2.3 Call SBG.Fitter
  optimized.sbg <- sbg.fitter(num.customers=t.num.customers,
                              pars.start=pars.start,
                              hessian=hessian,
                              method=method,
                              ...)

  pars.mle <- optimized.sbg$pars.mle
  convergence <- optimized.sbg$convergence

  ## For convenience later in coding fit statistics
  a <- pars.mle[1] ## alpha
  b <- pars.mle[2] ## beta

  if(convergence!= 0 & hessian) warning('The numerical optimization of the likelihood function did not converge. Consider not requesting the Hessian, trying different starting values, using a different optimization method, or identifying problematic observations. \n')

  if(convergence != 0 & !hessian) warning('The numerical optimization of the likeihood function did not converge. Consider trying different starting values, using a different optimization method, or identifying problematic observations. \n')

  ## Part 3: Prepare Parameter Summaries ##

  ## 3.1 Hyperparameters
  if(hessian) {hessian.opt <- optimized.sbg$hessian
               expected.inf.mat <- -solve(hessian.opt)

               la <- log(a)
               lb <- log(b)

               grad.a <- c(exp(la),0)
               grad.b <- c(0,exp(lb))

               se.a <- t(grad.a)%*%expected.inf.mat%*%grad.a
               se.b <- t(grad.b)%*%expected.inf.mat%*%grad.b
               z <- -qnorm(p=(1-conf.level)/2)

               lowers <- pars.mle - z*c(se.a,se.b)
               uppers <- pars.mle + z*c(se.a,se.b)

               par.info=matrix(data=c(pars.mle,
                                 lowers,
                                 uppers),
                 byrow=FALSE,nrow=2)
               rownames(par.info)=c('Alpha','Beta')
               colnames(par.info)=c('Estimate',
                         paste0(conf.level*100, "% Lower"),
                         paste0(conf.level*100, "% Upper"))

             }  else {
               par.info=matrix(data=pars.mle,byrow=FALSE)
               rownames(par.info)=c('Alpha','Beta')
               colnames(par.info)=c('Estimate')}

  ## 3.2 Moments of Latent Variable

  e <- a/(a + b)
  p <- 1/(1 + a + b)

  v.num <- a*b
  v.denom <- ((a + b)^2)*(a + b + 1)
  v <- v.num/v.denom
    
  if(hessian) {

    de.dla <- exp(la)/(exp(la) + exp(lb)) - exp(la) * exp(la)/(exp(la) + exp(lb))^2
    de.dlb <- -(exp(la) * exp(lb)/(exp(la) + exp(lb))^2)

    grad.e <- c(de.dla,de.dlb)
    se.e <- t(grad.e)%*%expected.inf.mat%*%grad.e

    dp.dla <- -(exp(la)/(1 + exp(la) + exp(lb))^2)
    dp.dlb <- -(exp(lb)/(1 + exp(la) + exp(lb))^2)

    grad.p <- c(dp.dla,dp.dlb)
    se.p <- t(grad.p)%*%expected.inf.mat%*%grad.p

    dv.dla <- exp(la) * exp(lb)/(((exp(la) + exp(lb))^2) * (exp(la) + exp(lb) + 1)) - (exp(la) * exp(lb)) * (2 * (exp(la) * (exp(la) + exp(lb))) * (exp(la) + exp(lb) + 1) +((exp(la) + exp(lb))^2) * exp(la))/(((exp(la) + exp(lb))^2) * (exp(la) + exp(lb) + 1))^2

    dv.dlb <- exp(la) * exp(lb)/(((exp(la) + exp(lb))^2) * (exp(la) + exp(lb) + 1)) - (exp(la) * exp(lb)) * (2 * (exp(lb) * (exp(la) + exp(lb))) * (exp(la) + exp(lb) + 1) + ((exp(la) + exp(lb))^2) * exp(lb))/(((exp(la) + exp(lb))^2) * (exp(la) + exp(lb) + 1))^2

    grad.v <- c(dv.dla,dv.dlb)
    se.v <- t(grad.v)%*%expected.inf.mat%*%grad.v

    z <- -qnorm(p=(1-conf.level)/2)

    lowers <- c(e,v,p) - z*c(se.e,se.v,se.p)
    uppers <- c(e,v,p) + z*c(se.e,se.v,se.p)

    lv.moments <- matrix(data=c(e,v,p,
                           lowers,
                           uppers),
                         byrow=FALSE,nrow=3)
    rownames(lv.moments) <- c('E[p]','V[p]','Pol[p]')
    colnames(lv.moments) <- c('Estimate',
                              paste0(conf.level*100, "% Lower"),
                              paste0(conf.level*100, "% Upper"))
    
  } else {

    lv.moments <- matrix(data=c(e,v,p),byrow=FALSE)
    rownames(lv.moments) <- c('E[p]','V[p]','Pol[p]')
    colnames(lv.moments)=c('Estimate')
    
  }

  ## Part 4: Calculate Fitted Values & Fit Statistics

  ## 4.1 Incremental Churners
  observed.incremental.churners <- -diff(num.customers)
  names(observed.incremental.churners) <- names(num.customers)[-length(num.customers)]
  
  time.churn <- 1:(length(num.customers) - 1) ## minus 1 because a customer can't churn in the first period
  lp.churn <- lbeta(a+1, b+time.churn-1) - lbeta(a, b)
  expected.incremental.churners <- num.customers[1]*exp(lp.churn)
  names(expected.incremental.churners) <- names(observed.incremental.churners)

  ic.holdout.ind <- holdout.ind[-1] ## 'incremental churners (ic)' holdout.ind adjustment because we're differencing

  incremental.churn.df <- data.frame(observed=observed.incremental.churners,
                                     expected=expected.incremental.churners,
                                     holdout.ind=ic.holdout.ind)

  ## 4.1.1 Training Data
  t.expected.incremental.churners <- expected.incremental.churners[ic.holdout.ind==0]
  t.observed.incremental.churners <- observed.incremental.churners[ic.holdout.ind==0]

  t.incremental.churn.rmse <- rmse(t.expected.incremental.churners,
                                   t.observed.incremental.churners) 
  t.incremental.churn.mae <- mae(t.expected.incremental.churners,
                                 t.observed.incremental.churners) 
  t.incremental.churn.mape <- mape(t.expected.incremental.churners,
                                   t.observed.incremental.churners) 

  t.incremental.churn.fit <- fit.stats(expected=t.expected.incremental.churners,
                                       observed=t.observed.incremental.churners,
                                       type='all') ## fit.stats from ../common.R
  
  if(holdout==0) { ## If there is no heldout data
    
    incremental.churn.stats <- matrix(data=t.incremental.churn.fit,
                                      byrow=FALSE)
    colnames(incremental.churn.stats) <- c('In-sample')
    rownames(incremental.churn.stats) <- c('RMSE','MAE','MAPE')

  } else { ## If there is heldout data
    
    ## 4.1.2 Heldout Data

    h.expected.incremental.churners <- expected.incremental.churners[ic.holdout.ind==1]
    h.observed.incremental.churners <- observed.incremental.churners[ic.holdout.ind==1]

    h.incremental.churn.fit <- fit.stats(expected=h.expected.incremental.churners,
                                       observed=h.observed.incremental.churners,
                                       type='all')
    
    incremental.churn.stats <- matrix(
                                      data=c(t.incremental.churn.fit,
                                        h.incremental.churn.fit),
                                      byrow=FALSE,ncol=2,nrow=3)
    colnames(incremental.churn.stats) <- c('In-sample','Out-of-Sample')
    rownames(incremental.churn.stats) <- c('RMSE','MAE','MAPE')
    
  }
  
  incremental.churn.info <- list(incremental.churn.df=incremental.churn.df,
                                 incremental.churn.stats=incremental.churn.stats)

  ## 4.2 Survivors
  observed.survivors <- num.customers
  expected.survivors <- c(num.customers[1],
                          num.customers[1] - cumsum(expected.incremental.churners))
  names(expected.survivors) <- names(observed.survivors)

  survivor.df <- data.frame(observed=observed.survivors,
                            expected=expected.survivors,
                            holdout.ind=holdout.ind)

  ## 4.2.1 Training Data
  t.observed.survivors <- observed.survivors[holdout.ind==0]
  t.expected.survivors <- expected.survivors[holdout.ind==0]

  t.survivors.stats <- fit.stats(expected=t.expected.survivors,
                                 observed=t.observed.survivors,
                                 type='all')
  
  if(holdout==0) { ## If there is no heldout data

    survivor.stats <- matrix(data=t.survivors.stats,byrow=FALSE,nrow=3,ncol=1)
    colnames(survivor.stats) <- c('In-Sample')
    rownames(survivor.stats) <- c('RMSE','MAE','MAPE')
    
  } else { ## If there is heldout data

    ## 4.2.2 Holdout Data

    h.observed.survivors <- observed.survivors[holdout.ind==1]
    h.expected.survivors <- expected.survivors[holdout.ind==1]

    h.survivors.stats <- fit.stats(expected=h.expected.survivors,
                                   observed=h.observed.survivors,
                                   type='all')

    survivor.stats <- matrix(data=c(t.survivors.stats,h.survivors.stats),
                             byrow=FALSE,nrow=3,ncol=2)
    colnames(survivor.stats) <- c('In-Sample','Out-of-Sample')
    rownames(survivor.stats) <- c('RMSE','MAE','MAPE')

  }

  survivor.info <- list(survivor.df=survivor.df,
                        survivor.stats=survivor.stats)

  ## 4.3 Retention Rates
  
  observed.retention <- 100*(num.customers[-length(num.customers)] - observed.incremental.churners)/num.customers[-length(num.customers)]
  expected.retention <- 100*(b + time.churn - 1)/(a + b + time.churn - 1)
  names(expected.retention) <- names(observed.retention)
  
  retention.df <- data.frame(observed=observed.retention,
                             expected=expected.retention,
                             holdout.ind=ic.holdout.ind) ## note ic.holdout.ind
  
  ## 4.3.1 Training Data

  t.observed.retention <- observed.retention[ic.holdout.ind==0]
  t.expected.retention <- expected.retention[ic.holdout.ind==0]

  t.retention.stats <- fit.stats(expected=t.expected.retention,
                                 observed=t.observed.retention,
                                 type='all')

  if(holdout==0) { ## If there is no heldout data

    retention.stats <- matrix(data=t.retention.stats,byrow=FALSE,nrow=3,ncol=1)
    colnames(retention.stats) <- c('In-Sample')
    rownames(retention.stats) <- c('RMSE','MAE','MAPE')

    
  } else { ## If there is heldout data

    ## 4.3.2 Holdout Data

    h.observed.retention <- observed.retention[ic.holdout.ind==1]
    h.expected.retention <- expected.retention[ic.holdout.ind==1]

    h.retention.stats <- fit.stats(expected=h.expected.retention,
                                   observed=h.observed.retention,
                                   type='all')

    retention.stats <- matrix(data=c(t.retention.stats,h.retention.stats),
                             byrow=FALSE,nrow=3,ncol=2)
    colnames(retention.stats) <- c('In-Sample','Out-of-Sample')
    rownames(retention.stats) <- c('RMSE','MAE','MAPE')
    

  }
    
  retention.info <- list(retention.df=retention.df,
                         retention.stats=retention.stats)

  ## Part 5: Wrap to Return
  
  res <- list(call=call,
              convergence=convergence,
              num.customers=num.customers,
              holdout.ind=holdout.ind,
              par.info=par.info,
              incremental.churn.info=incremental.churn.info,
              survivor.info=survivor.info,
              retention.info=retention.info,
              lv.moments=lv.moments)

  class(res) <- c('sbg')
  return(res)
  
}

# @keyword internal
sbg.ts <- function(num.customers,pars.start=c(1,1),
                        conf.int=FALSE, conf.level=.95,
                        method=c("Nelder-Mead", "BFGS", "CG", "SANN", "Brent"),
                        holdout=0, ...) {

  call <- match.call()
  
  if(!is.element(frequency(num.customers),c(1,4,12))) stop("Currently 'num.customers' of ts class accepted only in yearly, quarterly, or monthly form. Try instead using a numeric 'num.customers' with a character names.")

  ## Part 1: create numeric num.customers
  num.customers.numeric <- as.numeric(num.customers)
  names(num.customers.numeric) <- ts.to.date.procs(num.customers)$lab

  res <- sbg.numeric(num.customers=num.customers.numeric,
                     pars.start=pars.start,
                     conf.int=conf.int,
                     conf.level=conf.level,
                     method=method,
                     holdout=holdout,
                     ...)

  res$call <- call

  ## Part 2: create constructor for predict function
  res$predict.date.constructor <- function(n.ahead, include.last) {

    start.date <- tail(names(num.customers.numeric),1)
    start.date <- as.Date(start.date,format="%Y-%m-%d")

    if(include.last) {

      res <- seq.Date(from=start.date,
                      by=ts.to.date.procs(num.customers)$by,
                      length.out=n.ahead)
      
    } else {

      res <- seq.Date(from=start.date,
                      by=ts.to.date.procs(num.customers)$by,
                      length.out=n.ahead+1)[-1]
      
    }

    return(res)
    
  }

  return(res)
  
}

# @keyword internal
ts.to.date.procs <- function(ts) { ## Adapted from zoo (Zeieis and Grothendieck 2005)

  times <- unclass(time(ts))

  res <- list()
  
  if(frequency(ts) == 1) {
    res$lab <- paste(times, 1, 1, sep = "-")
    res$by <- "year"
  } else if(frequency(ts) == 4) {
    res$lab <- paste((times + 0.001)%/%1,
                     3 * (cycle(ts) - 1) + 1, 1, sep = "-")
    res$by <- "3 months"
  } else if(frequency(ts) == 12) {
    res$lab <- paste((times + 0.001)%/%1, cycle(ts), 1, sep = "-")
    res$by <- "month"
  } else stop('Cannot execute ts to date procs.')

  return(res)

}
