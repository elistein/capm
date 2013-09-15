## CLV function for shifted beta geometric model
## Created by ES (eli.manfred.stein@gmail.com) on 8.4.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 8.4.2013

clv <- function(x,...) UseMethod('clv')

clv.sbg <- function(sbg.object,
                    type=c('posterior','prior'),
                    when=c('after','before'),
                    disc.rate,
                    spend.object=NULL,
                    newdata=NULL) {
  
  require('gsl')

  ## Check and Format Arguments

  if(missing(sbg.object)) stop("Argument 'sbg.object' is required. \n")
  if(class(sbg.object) != 'sbg') stop("Argument 'sbg.object' must be of class 'sbg'. \n")

  if(missing(disc.rate)) stop("Argument 'disc.rate' is required. \n")
  if(!is.numeric(disc.rate) || length(disc.rate) != 1) stop("Argument 'disc.rate' must be numeric and of length 1. \n")

  type <- match.arg(type)
  when <- match.arg(when)
  typewhen <- paste(type,when,sep="")

  ## Translate spend.object into average spend
  
  if(missing(spend.object)) {
    spend.avg <- 1
    warning("Argument 'spend.object' is not provided. Assuming $1 average purchase transactions and returning DERT. \n")
  } else {

    spend.avg <- mean(spend.object)
    
  }

  if(is.numeric(spend.object)) spend.avg <- mean(spend.object)

  if(class(spend.object) == "gg") stop("Spend objects of class gg are not yet supported. \n")

  ## Extract

  a <- sbg.object$par.info['Alpha','Estimate'] ## alpha
  b <- sbg.object$par.info['Beta','Estimate'] ## beta

  if(is.null(newdata)) {end.period.num <- length(sbg.object$num.customers) - 1} else if(is.numeric(newdata)) {end.period.num <- c(newdata)} else stop("If provided, argument 'newdata' must be numeric.")


  ##

  disc.ratio <- 1/(1+disc.rate)
  
  priorbefore <- hyperg_2F1(1,b,a+b,disc.ratio)
  priorbefore <- priorbefore*spend.avg

  denom <- (a+b)*(1+disc.rate)
  priorafter <- (b/denom)*hyperg_2F1(1,b+1,a+b+1,disc.ratio)
  priorafter <- priorafter*spend.avg

  num <- b + end.period.num - 1
  denom <- num + a
  posteriorbefore <- (num/denom)*hyperg_2F1(1,
                                            b+end.period.num,
                                            a + b + end.period.num,
                                            disc.ratio)
  posteriorbefore <- posteriorbefore*spend.avg

  num <- b + end.period.num
  denom <- (a + b + end.period.num)*(1 + disc.rate)
  posteriorafter <- (num/denom)*hyperg_2F1(1,
                                           b+ end.period.num + 1,
                                           a + b + end.period.num + 1,
                                           disc.ratio)
  posteriorafter <- posteriorafter*spend.avg

  res <- switch(typewhen,
                priorbefore=priorbefore,
                priorafter=priorafter,
                posteriorbefore=posteriorbefore,
                posteriorafter=posteriorafter)

  return(res)
    
}
