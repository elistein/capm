## Functions used across the package
## Created by ES (eli.manfred.stein@gmail.com) on 6.28.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 7.23.2013

optim.error.handler <- function(e) {

  ## Custom Error Handling for Optim
  
  if(hessian) cat("Error: Could not numerically optimize the likelihood function, and/or could not calculate the Hessian. Consider not requesting the Hessian, trying different starting values, using a different optimization method, or identifying problematic observations. Message from optim:",as.character(e[[1]]),"\n")
  
  else cat("Error: Could not numerically optimize the likelihood function. Consider trying different starting values, using a different optimization method, or identifying problematic observations. Message from optim:",as.character(e[[1]]),"\n")  
}
  
rmse <- function(expected,observed) {

  ## Root mean squared error

  res <- sqrt(mean((expected-observed)^2))
  return(res)
  
}

mae <- function(expected,observed) {

  ## Mean absolute error

  res <- mean(abs(expected-observed))
  return(res)
  
}

mape <- function(expected,observed) {

  ## Mean absolute percent error

  res <- mean(abs(100*(expected-observed)/observed))
  return(res)
  
}

fit.stats <- function(expected,observed,
                      type=c('all','rmse','mae','mape')) {


  type <- match.arg(type)

  .rmse <- rmse(expected,observed)
  names(.rmse) <- c('RMSE')
  
  .mae <- mae(expected,observed)
  names(.mae) <- c('MAE')
  
  .mape <- mape(expected,observed)
  names(.mape) <- c('MAPE')

  .all <- c(.rmse, .mae, .mape)
  names(.all) <- c('RMSE','MAE','MAPE')

  res <- switch(type,
                rmse=.rmse,
                mae=.mae,
                mape=.mape,
                all=.all)

  return(res)
  
}

inv.logit <- function(p) {

  ## logistic function

  num <- 1
  denom <- 1 + exp(-1*p)

  res <- num/denom
  return(res)
  
}

logit <- function(p) {

  ## logit function

  res <- log(p) - log(1-p)
  return(res)
  
}
