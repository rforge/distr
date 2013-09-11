\name{internalGenerics}
\alias{internalGenerics}
\alias{distribution}
\alias{samplesize}
\title{Internal: Common Generics 'distribution' and 'samplesize'}
\description{
  In order to be able to use packages \pkg{distrSim} and \pkg{distrMod} independently,
  it is necessary to import the respective generic from a prior package, i.e.,
  \pkg{distr}.
  }
\usage{
distribution(object)
samplesize(object, ...)
}
\arguments{
  \item{object}{ the first argument to dispatch on in the actual methods. }
  \item{\dots}{ additional arguments for function \code{samplesize}. }
}

\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}

\keyword{internal}
