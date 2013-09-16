\name{sbg}
\alias{sbg}
\title{shifted Beta Geometric Model}
\usage{
  sbg(num.customers, pars.start = c(1, 1),
    conf.int = FALSE, conf.level = 0.95,
    method = c("Nelder-Mead", "BFGS", "CG", "SANN", "Brent"),
    holdout = 0, ...)
}
\arguments{
  \item{num.customers}{A numeric vector indicating the
  number of subscribing customers in each period.}

  \item{pars.start}{Starting values for the parameters
  alpha and beta. Order assumed to be c(alpha, beta).}

  \item{conf.int}{Logical parameter indicating whether
  confidence intervals for parameters should be returned.}

  \item{conf.level}{Confidence level for confidence
  intervals, if \code{conf.int} is set to \code{TRUE}.}

  \item{method}{Maximization method passed to
  \code{optim}.}

  \item{holdout}{Number of periods of data to be withheld
  in the estimation of the model. Periods at the end of the
  dataset (the last periods to occur) are withheld first.}

  \item{...}{Additional arguments passed directly to
  \code{optim}.}
}
\value{
  S3 \code{sbg} object, a list consisting of:
  \item{call}{The call used to create the object.}
  \item{convergence}{A scalar indicating the convergence
  status of the optimization routine. Code 0 indicates
  convergence was achieved. See documentation for
  \code{optim} for additional details.}
  \item{holdout.ind}{A binary vector indicating whether a
  period was heldout in the estimation of the model. The
  value 1 indicates the corresponding period was not used
  to estimate the model.} \item{par.info}{Numeric matrix
  describing the estimated model parameters.}
  \item{incremental.churn.info}{Numeric matrix describing
  the model fit as it relates to incremental churn.}
  \item{survivor.info}{Numeric matrix describing the model
  fit as it relates to surviving customers.}
  \item{retention.info}{Numeric matrix describing the model
  fit as it relates to retention rates.}
  \item{lv.moments}{Numeric matrix describing the model fit
  as it relates to the moments of the latent variable.}
  \item{predict.date.constructor}{If argument num.customers
  is of class \code{ts}, the function
  predict.date.constructor is used to generate labels for
  plotting.}
}
\description{
  Maximum likelihood estimation of a shifted Beta Geometric
  model.
}
\examples{
num.customers <- c(1000,869,743,653,593,551,517,481)
ts.num.customers <- ts(num.customers,
                      frequency=4,start=c(2008,1))

sbg.out <- sbg(ts.num.customers,holdout=2)
summary(sbg.out)
}
\author{
  Eli Stein (eli.manfred.stein@gmail.com)
}
\seealso{
  \code{\link{sbg.sim}}
}

