## Survivor functions for shifted beta geometric model
## Created by ES (eli.manfred.stein@gmail.com) on 7.31.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 7.31.2013

survivors <- function(x, ...) UseMethod('survivors')

survivors.sbg <- function(sbg.object, ...) {

  if(missing(sbg.object)) stop("Argument 'sbg.object' is required. \n")
  if(class(sbg.object) != 'sbg') stop("Argument 'sbg.object' must be of class 'sbg'. \n")

  res <- list()
  res$survivor.stats <- sbg.object$survivor.info$survivor.stats
  res$survivor.df <- sbg.object$survivor.info$survivor.df
  res$call <- sbg.object$call
  res$par.info <-sbg.object$par.info
  res$end.period.num <- length(sbg.object$num.customers) - 1
  res$last.observed.period.expected.survivors <- min(sbg.object$survivor.info$survivor.df[,'expected'])
  res$num.customers <- sbg.object$num.customers


  if(!is.null(sbg.object$predict.date.constructor)) res$predict.date.constructor <- sbg.object$predict.date.constructor

  class(res) <- c("survivors")
  
  return(res)
  
}

summary.survivors <- function(x) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'survivors') stop("Argument 'x' must be of class 'survivors'. \n")

  res <- list(survivor.stats=x$survivor.stats,
              survivor.df=x$survivor.df,
              call=x$call)
  
  class(res) <- c('survivors.summary')

  return(res)
  
}

print.survivors.summary <- function(x, digits=3, ...) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'survivors.summary') stop("Argument 'x' must be of class 'survivors.summary'. \n")

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "\n", sep = "")

  cat("\nFit Statistics:\n")
  print(x$survivor.stats,digits=digits,...)

  cat("\nSurvivor Data:\n")
  print(x$survivor.df[,c('observed','expected')],digits=digits,...)
  
  invisible(x)
  
}

fitted.survivors <- function(x) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'survivors') stop("Argument 'x' must be of class 'survivors'. \n")
  
  res <- x$survivor.df[,'expected']
  names(res) <- rownames(x$survivor.df)
  
  return(res)
  
}

predict.survivors <- function(x, n.ahead, labs.ahead=NULL) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'survivors') stop("Argument 'x' must be of class 'survivors'. \n")

  if(floor(n.ahead) != ceiling(n.ahead) || length(n.ahead) != 1) stop("Argument 'n.ahead' must be an integer scalar. \n")

  t.churn <- x$end.period.num + 1:n.ahead
  a <- x$par.info['Alpha','Estimate']
  b <- x$par.info['Beta','Estimate']
  
  lp.churn <- lbeta(a+1,b+t.churn-1) - lbeta(a,b)
  expected.incremental.churners <- max(x$num.customers)*exp(lp.churn)
  
  expected.cum.churners <- cumsum(expected.incremental.churners)
  
  res <- x$last.observed.period.expected.survivors - expected.cum.churners
  
  if(!is.null(labs.ahead)) {
    
    names(res) <- labs.ahead
    
  } else if(is.null(labs.ahead) & !is.null(x$predict.date.constructor)) {
    
    
    names(res) <- x$predict.date.constructor(n.ahead,
                                             include.last=FALSE)
    
  } else {
    
    names(res) <- paste('Period', x$end.period.num + 1 + 1:n.ahead)
    
  }
  
  return(res)
  
}

plot.survivors <- function(x, n.ahead=0, labs.ahead=NULL, type='b',
                           col.data='black',col.fitted='red',
                           col.holdoutLine='darkred',
                           col.predictLine='darkred',
                           legend.position='topright',
                           main='Observed vs Expected Survivors', xlab='Date',
                           ylab='Survivors',legend.show=TRUE,...) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'survivors') stop("Argument 'x' must be of class 'survivors'. \n")
  
  if(floor(n.ahead) != ceiling(n.ahead) || length(n.ahead) != 1) stop("Argument 'n.ahead' must be an integer scalar.")

  if(!is.element(type,c('p','l','b','c','o','h','s','S','n'))) stop("Argument 'type' is not supported by the generic plot.default() function.")
  
  holdout.ind <- x$survivor.df$holdout.ind
  
  observed <- x$survivor.df[,'observed']
  expected <- x$survivor.df[,'expected']
  x.axis.labs <- rownames(x$survivor.df)

  if(n.ahead>0) {

    observed <- c(observed,
                  rep(NA,n.ahead))
    expected <- c(expected,
                  suppressWarnings(predict(x,n.ahead)))
    x.axis.labs <- c(x.axis.labs,tail(names(expected),n.ahead))
    
  }
  
  ymin <- min(observed,expected,na.rm=TRUE) -
    .1*max(range(observed,expected,na.rm=TRUE))
  ymax <- max(observed,expected,na.rm=TRUE) +
    .1*max(range(observed,expected,na.rm=TRUE))
  
  ymin <- max(0,ymin)
  
  plot(observed,type=type,ylim=c(ymin,ymax),col=col.data,main=main,
       xlab=xlab,ylab=ylab,xaxt='n',
       ...)
  points(expected,type=type,col=col.fitted)
  axis(side=1,labels=x.axis.labs,at=1:length(observed))

  if(sum(holdout.ind)>0) abline(v=min(which(holdout.ind==1)) - .5,
                                col=col.holdoutLine, lty='dashed', lwd=2)

  if(n.ahead>0) abline(v=nrow(x$survivor.df) + .5,
                       col=col.predictLine,lty='dashed',lwd=2)

  if(legend.show) legend(legend.position,c('Observed','Expected'),
                         col=c('black','red'),lty=rep(1,2),pch=rep(21,2))
  
}

  
  
