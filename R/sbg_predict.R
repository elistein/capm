## Prediction functions for shifted beta geometric model
## Created by ES (eli.manfred.stein@gmail.com) on 7.13.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 7.23.2013

predict.sbg <- function(sbg.object,type=c('incr.churn','survivors','ret.rate'),
                        n.ahead=length(sbg.object$num.customers)/4,labs.ahead=NULL,
                        ...) {

  ## Check Arguments

  if(missing(sbg.object)) stop("Argument 'sbg.object' is required. \n")
  if(class(sbg.object) != 'sbg') stop("Argument 'sbg.object' must be of class 'sbg'. \n")
  
  if(floor(n.ahead) != ceiling(n.ahead) || length(n.ahead) != 1) stop("Argument 'n.ahead' must be an integer. \n")
  if(!is.element(type,c('incr.churn','survivors','ret.rate'))) warning("Argument 'type' is not exactly one of 'incr.churn,' 'survivors,' or 'ret.rate,' so the nearest match will be used.")

  ## Grab type argument
  type=match.arg(type)

  ## Extract from sbg.object
  a <- sbg.object$par.info['Alpha','Estimate']
  b <- sbg.object$par.info['Beta','Estimate']

  last.observed.period.expected.survivors <- min(sbg.object$survivor.info$survivor.df[,'expected'])

  ## Calculate

  end.period.num <- length(sbg.object$num.customers) - 1 ## First element of num.customers corresponds to period zero
  t.churn <- end.period.num + 1:n.ahead
  
  lp.churn <- lbeta(a+1,b+t.churn-1) - lbeta(a,b)

  ## Incremental churners
  expected.incremental.churners <- max(num.customers)*exp(lp.churn)

  ## Survivors
  
  expected.cum.churners <- cumsum(expected.incremental.churners)
  expected.survivors <- last.observed.period.expected.survivors - expected.cum.churners

  ## Retention Rate
  expected.retention.rate <- (b + t.churn - 1)/(a + b + t.churn - 1)
  
  ## Return the desired prediction
  res <- switch(type,
                incr.churn=expected.incremental.churners,
                survivors=expected.survivors,
                ret.rate=expected.retention.rate)

  ## Create Labels
  if(!is.null(labs.ahead)) {
    
    names(res) <- labs.ahead

  } else if(is.null(labs.ahead) & !is.null(sbg.object$predict.date.constructor)) {

    
    names(res) <- sbg.object$predict.date.constructor(n.ahead,
                                                      include.last=switch(
                                                        type,
                                                        incr.churn=TRUE,
                                                        survivors=FALSE,
                                                        ret.rate=TRUE)
                                                      )
    
  } else {

    last.excluded <- paste('Period', length(sbg.object$num.customers) + 1:n.ahead)
    last.included <- paste('Period', length(sbg.object$num.customers) + 0:(n.ahead-1))
    
    names(res) <- switch(type,
                         incr.churn=last.included,
                         survivors=last.excluded,
                         ret.rate=last.included)
    
  }

  return(res)
}
