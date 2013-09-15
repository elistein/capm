## sim function for shifted beta geometric model
## Created by ES (eli.manfred.stein@gmail.com) on 8.14.2013
## Last edited by ES (eli.manfred.stein@gmail.com) on 8.14.2013

#' @name sbg.sim
#'
#' @author Eli Stein (eli.manfred.stein@gmail.com)
#'
#' @description Simulate from a shifted Beta Geometric model.
#'
#' @param a The 'alpha' parameter of the shifted Beta Geometric model.
#' @param b The 'beta' parameter of the shifted Beta Geometric model.
#' @param obs.per.end The index of the final period in which customers are observed.
#' @param n The number of customers at the start of the first period. (Defaults to 1000.)
#'
#' @return Vector indicating number of surviving customers in each period.
#'
#' @export
#'
#' @examples
#' simul.data <- sim.sbg(a=5,b=10,obs.per.end=7)


sbg.sim <- function(a,b,obs.per.end,n=1000) {


  ## Check Arguments

  if(missing(a)) stop("Argument 'a' must be provided. \n")
  if(missing(b)) stop("Argument 'b' must be provided. \n")
  if(missing(obs.per.end)) stop("Argument 'obs.per.end' must be provided. \n")
  
  if(!is.numeric(a) || length(a) != 1) stop("Argument 'a' must be a numeric vector of length 1. \n")
  if(!is.numeric(b) || length(b) != 1) stop("Argument 'b' must be a numeric vector of length 1. \n")
  if(!is.numeric(obs.per.end) || length(obs.per.end) != 1) stop("Argument 'obs.per.end' must be a numeric vector of length 1. \n")
  if(!is.numeric(n) || length(n) != 1) stop("Argument 'n' must be a numeric vector of length 1. \n")

  ## Simulations

  p <- rbeta(n=n, shape1=a, shape2=b)
  death.after.period <- rgeom(n=n,prob=p) + 1

  ## Format as Survival Vector

  incremental.churners <- tabulate(death.after.period)
  cum.incremental.churners <- cumsum(incremental.churners)

  surv.vec <- c(n,n-cum.incremental.churners)
  trun.surv.vec <- ifelse(rep(obs.per.end <= length(surv.vec),obs.per.end),
                          head(surv.vec,obs.per.end),
                          c(surv.vec,rep(0,obs.per.end-length(surv.vec))))
  
  names(trun.surv.vec) <- paste("Period",1:length(trun.surv.vec))
  
  return(trun.surv.vec)
  
}
