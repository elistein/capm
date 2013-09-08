## Functions used across the package
## Created by ES (eli.manfred.stein@gmail.com) on 7.23.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 7.23.2013

summary.sbg <- function(sbg.object) {

  if(missing(sbg.object)) stop("Argument 'sbg.object' is required. \n")
  if(class(sbg.object) != 'sbg') stop("Argument 'sbg.object' must be of class 'sbg'. \n")

  if(sbg.object$convergence != 0) warning("The numerical optimization of the sbg likelihood did not converge. Parameter estimates are likely meaningless.")

  res <- list(call=sbg.object$call,
              par.info=sbg.object$par.info,
              lv.info=sbg.object$lv.moments,
              survivor.df=sbg.object$survivor.info$survivor.df,
              survivor.stats=sbg.object$survivor.info$survivor.stats)
  
  class(res) <- c('sbg.summary')
  return(res)

}

print.sbg.summary <- function(x,digits=3,...) {

  if(missing(x)) stop("Argument 'sbg.object' is required. \n")
  if(class(x) != 'sbg.summary') stop("Argument 'sbg.object' must be of class 'sbg.summary'. \n")

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "\n", sep = "")

  cat("\nParameter Estimates:\n")
  print(x$par.info,digits=digits,...)

  cat("\nMoments of Latent Variable:\n")
  print(x$lv.info,digits=digits,...)

  cat("\nFit Statistics for Retained Customers:\n")
  print(x$survivor.stats,digits=digits,...)
  
  invisible(x)
  
}


