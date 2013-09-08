## CE function for shifted beta geometric model
## Created by ES (eli.manfred.stein@gmail.com) on 8.14.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 8.14.2013

ce <- function(x,...) UseMethod('ce')

ce.sbg <- function(sbg.object,
                   type=c('response','lv'),
                   newdata=NULL) {


  ## Check Arguments

  if(missing(sbg.object)) stop("Argument 'sbg.object' is required. \n")
  if(class(sbg.object) != 'sbg') stop("Argument 'sbg.object' must be of class 'sbg'. \n")
  
  type <- match.arg(type)

  ## Extract

  a <- sbg.object$par.info['Alpha','Estimate'] ## alpha
  b <- sbg.object$par.info['Beta','Estimate'] ## beta

  if(a<=1 && type=='response') stop("If alpha is estimated to be less than 1, the conditional expectation of 'response' does not converge. \n")

  if(is.null(newdata)) {n.renewals <- length(sbg.object$num.customers) - 1} else if(is.numeric(newdata)) {n.renewals <- c(newdata) - 1} else stop("If provided, argument 'newdata' must be numeric.")

  ## Calculate
  
  post.mean.p <- a/(a + b + n.renewals)
  ## expected.churn.period <- 1/post.mean.p + 1 ## note the + 1 because this is a shifted distribution
  expected.churn.period <- (a + b + n.renewals - 1)/(a - 1) ## note the + 1 because this is a shifted distribution

  res <- switch(type,
                response=expected.churn.period,
                lv=post.mean.p)

  return(res)
    
}
