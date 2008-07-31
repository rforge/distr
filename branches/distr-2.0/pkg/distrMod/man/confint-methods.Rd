\name{confint-methods}
\docType{methods}
\alias{confint-methods}
\alias{confint}
\alias{confint,ANY-method}
\alias{confint,Estimate-method}
\title{ Methods for function confint in Package `distrMod' }
\description{
  Methods for function \code{confint} in package \pkg{distrMod};
  by default uses \code{\link[stats]{confint}} and its corresponding
  S3-methods, but also computes
  (asymptotic) confidence intervals for objects of class \code{Estimate}.
  Computes confidence intervals for one or more parameters in a fitted
  model.
}
\usage{
confint(object, \dots)
\S4method{confint}{ANY}(object, parm, level = 0.95, \dots)
\S4method{confint}{Estimate}(object, level = 0.95, \dots)

}
\arguments{
  \item{object}{a fitted model object.}
  \item{parm}{a specification of which parameters are to be given
    confidence intervals, either a vector of numbers or a vector of
    names.  If missing, all parameters are considered.}
  \item{level}{the confidence level required.}
  \item{\dots}{additional argument(s) for methods.}
}
\value{
  A matrix (or vector) with columns giving lower and upper confidence
  limits for each parameter. These will be labelled as (1-level)/2 and
  1 - (1-level)/2 in \% (by default 2.5\% and 97.5\%).
}
\details{
  \code{confint} is a generic function.  The default method
  (signature \code{ANY}) uses the S3 generic of package \pkg{stats},
  see \code{\link[stats]{confint}}. The method for signature \code{Estimate}
  will return a corresponding confidence interval assuming
  asymptotic normality, and hence needs suitably filled slot \code{asvar}
  in argument \code{object}.
}
\seealso{
  \code{\link[stats]{confint}},
  \code{\link[MASS]{confint.glm}} and \code{\link[MASS]{confint.nls}} in
  package \pkg{MASS}.
}
\examples{
## for signature ANY examples confer stats::confint
## (empirical) Data
x <- rgamma(50, scale = 0.5, shape = 3)

## parametric family of probability measures
G <- GammaFamily(scale = 1, shape = 2)

## Maximum likelihood estimator
(res <- MLEstimator(x = x, ParamFamily = G))

 confint(res)
}
\keyword{models}
