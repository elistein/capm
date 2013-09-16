\name{fitted.sbg}
\alias{fitted.sbg}
\title{Extraction of Fitted Values from \code{sbg} Object}
\usage{
  \method{fitted}{sbg} (sbg.object,
    type = c("incr.churn", "survivors", "ret.rate"))
}
\arguments{
  \item{sbg.object}{An object of class \code{sbg} resulting
  from a call to \code{sbg}.}

  \item{type}{Character string of 'incr.churn,'
  'survivors,' or 'ret.rate' indicating the fitted values
  to return.}
}
\value{
  Vector of fitted values.
}
\description{
  Extraction of Fitted Values from \code{sbg} Object
}
\examples{
simul.data <- sim.sbg(a=5,b=10,obs.per.end=7)
sbg.model <- sbg(simul.data)
fitted(sbg.model,type='survivors')
}
\seealso{
  \code{\link{sbg}}
}

