\name{setRNG}
\alias{setRNG}
\title{Set the Random Number Generator}
\description{Set the RNG or return information about the setting of the RNG.}
\usage{
    setRNG(kind=NULL, seed=NULL, normal.kind=NULL)
    }
\arguments{None required
    \item{kind}{a character string.}
    \item{seed}{a vector of numbers (depending on kind).}
    \item{normal.kind}{a character string.}
}
\value{The old setting.}
\details{
    Sets the uniform and normal random number generators and the seed.
    The old setting is returned using \code{invisible()} in a format
    which can be used in another call to \code{setRNG}. (This would reset to the
    original value.) If no arguments are given the current setting is returned, 
    not using \code{invisible()}. In R see \code{RNGkind} for more details.

    Note that in a function using \code{setRNG} it is good practice to
    assign the old setting to a variable, then reset to the old value on exiting
    the function (using \code{on.exit}). This avoids the possibility that
    overall RNG behaviour in a session, other than within your function, may be 
    disrupted by your function.
}
\section{Side Effects}{Sets global variables controlling the uniform and normal random 
    number generators and the global seed.}
\seealso{
    \code{\link{RNGkind}},
    \code{\link{set.seed}},
    \code{\link{runif}},
    \code{\link{rnorm}},
    \code{\link{random.number.test}}
}
\examples{
setRNG(kind="Wichmann-Hill", seed=c(979,1479,1542), normal.kind="Box-Muller")
rnorm(10)
}
\keyword{programming}
\keyword{utilities}

