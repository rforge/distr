\name{setToBeDefinedPkgs}
\alias{setToBeDefinedPkgs}
\title{setToBeDefinedPkgs}
\description{
sets up / updates a table of keywordstyles to different packages}

\usage{
setToBeDefinedPkgs(pkgs, keywordstyles)
}

\arguments{
  \item{pkgs}{character; the packages for which keywordstyle information is to be stored}
  \item{keywordstyles}{character or missing; the corresponding keywordstyle format
   strings; if missing the corresponding option \code{Keywordstyle} is read off
   by using\cr \code{getSweaveListingOption("Keywordstyle")}.\cr
   Internally, it is being cast to the same length as \code{pkgs} by
  \code{rep(keywordstyles, length.out = length(pkgs))}.
  }
}

\value{
\code{invisible()}
}

\details{
The corresponding table is stored globally in the (non-exported) object
\code{.tobeDefinedPkgs}, which is hidden in the namespace of this package.

It is used afterwords by the masked versions of
\code{\link[SweaveListingUtils]{require}} and
\code{\link[SweaveListingUtils]{library}} of this package to allow for
defining a set of keywordstyle formats for different packages
right in the preamble of a \file{.Rnw} file.

This transfer of information to \code{require} and \code{library} clearly
is a deviation from the functional programming paradigm but is necessary at this place,
as otherwise (although this is still allowed) \code{require} and \code{library}
would have to be called with non-standard (i.e. package \pkg{base}-) arguments,
which is \emph{not} the goal of including \R code sniplets by Sweave.
}

\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
\examples{
setToBeDefinedPkgs(pkgs = c("distr","distrEx"),
                   keywordstyles = paste("\\bfseries\\color{",c("blue","red"),"}",
                         sep="", collapse=""))
### not to be used:
print(SweaveListingUtils:::.tobeDefinedPkgs)
}
\keyword{utilities}
