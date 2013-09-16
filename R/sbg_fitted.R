## 'Extractor' fitted function for shifted beta geometric model
## Created by ES (eli.manfred.stein@gmail.com) on 7.13.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 7.13.2013

#' @title Extraction of Fitted Values from \code{sbg} Object
#'
#' @name fitted.sbg
#'
#' @rdname fitted
#' @method fitted sbg
#' @S3method fitted sbg
#'
#' @param sbg.object An object of class \code{sbg} resulting from a call to \code{sbg}.
#' @param type Character string of 'incr.churn,' 'survivors,' or 'ret.rate' indicating the fitted values to return.
#'
#' @return Vector of fitted values.
#' @export
#'
#' @examples
#' simul.data <- sim.sbg(a=5,b=10,obs.per.end=7)
#' sbg.model <- sbg(simul.data)
#' fitted(sbg.model,type='survivors')
#'
#' @seealso \code{\link{sbg}}

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
