\name{sbg.sim}
\alias{sbg.sim}
\title{Simulation from a shifted Beta Geometric model.}
\usage{
  sbg.sim(a, b, obs.per.end, n = 1000)
}
\arguments{
  \item{a}{The 'alpha' parameter of the shifted Beta
  Geometric model.}

  \item{b}{The 'beta' parameter of the shifted Beta
  Geometric model.}

  \item{obs.per.end}{The index of the final period in which
  customers are observed.}

  \item{n}{The number of customers at the start of the
  first period.}
}
\value{
  Vector indicating number of surviving customers in each
  period.
}
\description{
  Simulate from a shifted Beta Geometric model.
}
\examples{
simul.data <- sim.sbg(a=5,b=10,obs.per.end=7)
}
\author{
  Eli Stein (eli.manfred.stein@gmail.com)
}
\seealso{
  \code{\link{sbg}}
}

