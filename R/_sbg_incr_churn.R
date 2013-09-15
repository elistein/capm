## Incremental Churn functions for shifted beta geometric model
## Created by ES (eli.manfred.stein@gmail.com) on 7.31.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 7.31.2013

incr.churn <- function(x, ...) UseMethod('incr.churn')

incr.churn.sbg <- function(sbg.object, ...) {

  if(missing(sbg.object)) stop("Argument 'sbg.object' is required. \n")
  if(class(sbg.object) != 'sbg') stop("Argument 'sbg.object' must be of class 'sbg'. \n")

  res <- list()
  res$incremental.churn.stats <- sbg.object$incremental.churn.info$incremental.churn.stats
  res$incremental.churn.df <- sbg.object$incremental.churn.info$incremental.churn.df
  res$call <- sbg.object$call
  res$par.info <-sbg.object$par.info
  res$end.period.num <- length(sbg.object$num.customers) - 1
  res$num.customers <- sbg.object$num.customers

  if(!is.null(sbg.object$predict.date.constructor)) res$predict.date.constructor <- sbg.object$predict.date.constructor

  class(res) <- c("incr.churn")
  
  return(res)
  
}

summary.incr.churn <- function(x) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'incr.churn') stop("Argument 'x' must be of class 'incr.churn'. \n")

  res <- list(incremental.churn.stats=x$incremental.churn.stats,
              incremental.churn.df=x$incremental.churn.df,
              call=x$call)
  
  class(res) <- c('incr.churn.summary')

  return(res)
  
}

print.incr.churn.summary <- function(x, digits=3, ...) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'incr.churn.summary') stop("Argument 'x' must be of class 'incr.churn.summary'. \n")

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "\n", sep = "")

  cat("\nFit Statistics:\n")
  print(x$incremental.churn.stats,digits=digits,...)

  cat("\nIncremental Churn Data:\n")
  print(x$incremental.churn.df[,c('observed','expected')],digits=digits,...)
  
  invisible(x)
  
}

fitted.incr.churn <- function(x) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'incr.churn') stop("Argument 'x' must be of class 'incr.churn'. \n")

  res <- x$incremental.churn.df[,'expected']
  names(res) <- rownames(x$incremental.churn.df)
  
  return(res)
  
}

predict.incr.churn <- function(x, n.ahead, labs.ahead=NULL) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'incr.churn') stop("Argument 'x' must be of class 'incr.churn'. \n")

  if(missing(n.ahead)) stop("Argument 'n.ahead' is required. \n")
  if(floor(n.ahead) != ceiling(n.ahead) || length(n.ahead) != 1) stop("Argument 'n.ahead' must be an integer scalar. \n")
  
  t.churn <- x$end.period.num + 1:n.ahead
  a <- x$par.info['Alpha','Estimate']
  b <- x$par.info['Beta','Estimate']

  lp.churn <- lbeta(a+1,b+t.churn-1) - lbeta(a,b)
  res <- max(x$num.customers)*exp(lp.churn)
  
    if(!is.null(labs.ahead)) {
    
    names(res) <- labs.ahead

  } else if(is.null(labs.ahead) & !is.null(x$predict.date.constructor)) {

    
    names(res) <- x$predict.date.constructor(n.ahead,
                                             include.last=TRUE)
    
  } else {
    
    names(res) <- paste('Period', x$end.period.num + 1:n.ahead)
    
  }
  
  return(res)
  
}

plot.incr.churn <- function(x, n.ahead=0, labs.ahead=NULL, type='b',
                            col.data='black',col.fitted='red',
                            col.holdoutLine='darkred',
                            col.predictLine='darkred',
                            legend.position='topright',
                            main='Observed vs Expected Incremental Churners',
                            xlab='Date',
                            ylab='Incremental Churners',legend.show=TRUE,...) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'incr.churn') stop("Argument 'x' must be of class 'incr.churn'. \n")
  
  if(floor(n.ahead) != ceiling(n.ahead) || length(n.ahead) != 1) stop("Argument 'n.ahead' must be an integer scalar.")

  if(!is.element(type,c('p','l','b','c','o','h','s','S','n'))) stop("Argument 'type' is not supported by the generic plot.default() function.")


  holdout.ind <- x$incremental.churn.df$holdout.ind
  
  observed <- x$incremental.churn.df[,'observed']
  expected <- x$incremental.churn.df[,'expected']
  x.axis.labs <- rownames(x$incremental.churn.df)

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

  if(n.ahead>0) abline(v=nrow(x$incremental.churn.df) + .5,
                       col=col.predictLine,lty='dashed',lwd=2)

  if(legend.show) legend(legend.position,c('Observed','Expected'),
                         col=c(col.data,col.fitted),lty=rep(1,2),pch=rep(21,2))
  
}

  
  
