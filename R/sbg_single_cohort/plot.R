## Plot function for shifted beta geometric model
## Created by ES (eli.manfred.stein@gmail.com) on 7.13.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 8.4.2013

plot.sbg <- function(sbg.object, which.plots = c('both','all','lv','fit'),
                     n.ahead=0, labs.ahead=NULL,
                     col.data='black',col.fitted='red',
                     col.holdoutLine='darkred',
                     col.predictLine='darkred',
                     legend.position='topright',
                     ask = "default",
                     legend.show=TRUE)  {

  if(missing(sbg.object)) stop("Argument 'sbg.object' is required. \n")
  if(class(sbg.object) != 'sbg') stop("Argument 'sbg.object' must be of class 'sbg'. \n")

  if(floor(n.ahead) != ceiling(n.ahead) || length(n.ahead) != 1) stop("Argument 'n.ahead' must be an integer scalar. \n")
  
  which.plots <- match.arg(which.plots)
  
  if(!is.logical(ask)) { ## that is, if ask is not specified as TRUE or FALSE
    
    num.plots.requested <- switch(which.plots,
                                  both=2,
                                  all=4,
                                  lv=1,
                                  fit=1)
    
    ask <- dev.interactive() && prod(par("mfcol")) < num.plots.requested
    
  }
  
  if(ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  
  if(which.plots == "both" || which.plots == "all" || which.plots == "lv") {
    
    plot(function(x) dbeta(x,shape1=sbg.object$par.info['Alpha',1],
                           shape2=sbg.object$par.info['Beta',1]),
         from=0,to=1,
         xlab="p",ylab="Probability Density",
         main="Population Distribution of Latent Variable")
    
  }
  
  if(which.plots == "both" || which.plots == "all" || which.plots == "fit") plot.survivors(survivors(sbg.object),n.ahead=n.ahead,labs.ahead=labs.ahead,legend.show=legend.show,col.data=col.data,col.fitted=col.fitted,col.holdoutLine=col.holdoutLine,col.predictLine=col.predictLine,legend.position=legend.position)
  
  if(which.plots == "all") plot.incr.churn(incr.churn(sbg.object),n.ahead=n.ahead,labs.ahead=labs.ahead,legend.show=legend.show,col.data=col.data,col.fitted=col.fitted,col.holdoutLine=col.holdoutLine,col.predictLine=col.predictLine,legend.position=legend.position)
  
  if(which.plots == "all") plot.ret.rate(ret.rate(sbg.object),n.ahead=n.ahead,labs.ahead=labs.ahead,legend.show=legend.show,col.data=col.data,col.fitted=col.fitted,col.holdoutLine=col.holdoutLine,col.predictLine=col.predictLine,legend.position=legend.position)
  
  return(invisible())
}
