## Rate Rate functions for shifted beta geometric model
## Created by ES (eli.manfred.stein@gmail.com) on 7.28.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 7.28.2013

ret.rate <- function(x, ...) UseMethod('ret.rate')

ret.rate.sbg <- function(sbg.object, ...) {

  if(missing(sbg.object)) stop("Argument 'sbg.object' is required. \n")
  if(class(sbg.object) != 'sbg') stop("Argument 'sbg.object' must be of class 'sbg'. \n")

  res <- list()
  res$retention.stats <- sbg.object$retention.info$retention.stats
  res$retention.df <- sbg.object$retention.info$retention.df
  res$call <- sbg.object$call
  res$par.info <-sbg.object$par.info
  res$end.period.num <- length(sbg.object$num.customers) - 1

  if(!is.null(sbg.object$predict.date.constructor)) res$predict.date.constructor <- sbg.object$predict.date.constructor

  class(res) <- c("ret.rate")
  
  return(res)
  
}

summary.ret.rate <- function(x) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'ret.rate') stop("Argument 'x' must be of class 'ret.rate'. \n")

  res <- list(retention.stats=x$retention.stats,
              retention.df=x$retention.df,
              call=x$call)
  
  class(res) <- c('ret.rate.summary')

  return(res)
  
}

print.ret.rate.summary <- function(x, digits=3, ...) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'ret.rate.summary') stop("Argument 'x' must be of class 'ret.rate.summary'. \n")

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "\n", sep = "")

  cat("\nFit Statistics:\n")
  print(x$retention.stats,digits=digits,...)

  cat("\nRetention Rate Data:\n")
  print(x$retention.df[,c('observed','expected')],digits=digits,...)
  
  invisible(x)
  
}

fitted.ret.rate <- function(x) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'ret.rate') stop("Argument 'x' must be of class 'ret.rate'. \n")

  res <- x$retention.df[,'expected']
  names(res) <- rownames(x$retention.df)
  
  return(res)
  
}

predict.ret.rate <- function(x, n.ahead, labs.ahead=NULL) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'ret.rate') stop("Argument 'x' must be of class 'ret.rate'. \n")

  if(floor(n.ahead) != ceiling(n.ahead) || length(n.ahead) != 1) stop("Argument 'n.ahead' must be an integer scalar. \n")
  
  t.churn <- x$end.period.num + 1:n.ahead
  a <- x$par.info['Alpha','Estimate']
  b <- x$par.info['Beta','Estimate']
  
  res <- 100*(b + t.churn - 1)/(a + b + t.churn - 1)

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

plot.ret.rate <- function(x, n.ahead=0, labs.ahead=NULL, type='b',
                          col.data='black',col.fitted='red',
                          col.holdoutLine='darkred',
                          col.predictLine='darkred',
                          main='Observed vs Expected Retention Rates', xlab='Date',
                          ylab='Retention Rate',legend.show=TRUE,
                          legend.position='topright',...) {

  if(missing(x)) stop("Argument 'x' is required. \n")
  if(class(x) != 'ret.rate') stop("Argument 'x' must be of class 'ret.rate'. \n")
  
  if(floor(n.ahead) != ceiling(n.ahead) || length(n.ahead) != 1) stop("Argument 'n.ahead' must be an integer scalar.")

  if(!is.element(type,c('p','l','b','c','o','h','s','S','n'))) stop("Argument 'type' is not supported by the generic plot.default() function.")

  holdout.ind <- x$retention.df$holdout.ind
  
  observed <- x$retention.df[,'observed']
  expected <- x$retention.df[,'expected']
  x.axis.labs <- rownames(x$retention.df)

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
  ymax <- min(100,ymax)
  
  plot(observed,type=type,ylim=c(ymin,ymax),col=col.data,main=main,
       xlab=xlab,ylab=ylab,xaxt='n',
       ...)
  points(expected,type=type,col=col.fitted)
  axis(side=1,labels=x.axis.labs,at=1:length(observed))

  if(sum(holdout.ind)>0) abline(v=min(which(holdout.ind==1)) - .5,
                                col=col.holdoutLine, lty='dashed', lwd=2)

  if(n.ahead>0) abline(v=nrow(x$retention.df) + .5,
                       col=col.predictLine,lty='dashed',lwd=2)

  if(legend.show) legend(legend.position,c('Observed','Expected'),
                         col=c(col.data,col.fitted),lty=rep(1,2),pch=rep(21,2))

  
}

  
  
