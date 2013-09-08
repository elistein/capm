## 'Extractor' fitted function for shifted beta geometric model
## Created by ES (eli.manfred.stein@gmail.com) on 7.13.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 7.13.2013

fitted.sbg <- function(sbg.object, type = c('incr.churn','survivors','ret.rate')) {

  if(missing(sbg.object)) stop("Argument 'sbg.object' is required. \n")
  if(class(sbg.object) != 'sbg') stop("Argument 'sbg.object' must be of class 'sbg'. \n")
  
  type <- match.arg(type)

  res <- switch(type,
                incr.churn=sbg.object$incremental.churn.info$incremental.churn.df[,'expected'],
                survivors=sbg.object$survivor.info$survivor.df[,'expected'],
                ret.rate=sbg.object$retention.info$retention.df[,'expected'])

  if(length(res)==nrow(sbg.object$survivor.info$survivor.df)) {

    names(res) <- rownames(sbg.object$survivor.info$survivor.df)
    
  } else {

    names(res) <- rownames(out$retention.info$retention.df)
    
  }
  
  return(res)
  
}
