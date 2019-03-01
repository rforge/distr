\name{random.number.test}
\alias{random.number.test}
\title{Test the random number generator}
\description{Test the random number generator.}
\usage{
    random.number.test()
    }
\arguments{None}
\value{logical}
\details{
    This function checks that the
    RNG is working properly and has not been changed. If the RNG does not return
    values as in previous versions of R then the function executes 
    \code{stop()}. Since changes to the RNG will cause comparisons of 
    simulation results to fail, this is a useful check before investigating more
    complicated problems that may be a result of other "improvements" in your
    simulation or estimation programs.
}
\section{Side Effects}{Executes \code{stop()} if the tests fail.}
\seealso{
    \code{\link{set.seed}}
    \code{\link{RNGkind}}
    \code{\link{runif}}
    \code{\link{rnorm}}
    \code{\link{setRNG}}
}
\examples{
  random.number.test()
}
\keyword{programming}
\keyword{utilities}

