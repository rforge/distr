\name{changeKeywordstyles}
\alias{changeKeywordstyles}
\title{changeKeywordstyles}
\description{
sets up / updates a table of keywordstyles to different packages}

\usage{
changeKeywordstyles(pkgs, keywordstyles)
}

\arguments{
  \item{pkgs}{character; the packages for which keywordstyle information is to be changed}
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
Before changing the keywordstyles, we first check whether the corresponding
package is registered at all ---by looking up the
(non-exported) vector object \code{.alreadyDefinedPkgs}, which is hidden in the
namespace of this package.

For changing the keywordstyle, we write out a
\preformatted{
 \\lstdefinelanguage{R}\%
     {keywordstyle=[<order number>]<keywordstyle as format string>,\%
      }
}
directive to standard out, where \code{<keywordstyle as format string>}
is a string containing any sequence of TeX formatting commands like
\code{"\\\\bfseries\\\\footnotesize"}. Note that backslashes have to be escaped.
and \code{<order number>} is just \code{num+2} where \code{num}
is the index of the package in the \code{.alreadyDefinedPkgs} vector.\cr

For use in an \code{.Rnw} file, the call to \code{lstsetlanguage} should be
wrapped into a corresponding Sweave chunk in the form
\preformatted{
<< /chunkname/, results=tex, echo=FALSE>>=
changeKeywordstyles( ..... )
@
}
for example
\preformatted{
<<distrRegisterKeywords, results=tex, echo=FALSE>>=
changeKeywordstyles(pkgs = "distr",
                    keywordstyles = "\\\\bfseries\\\\color{blue}")
@
}
%
}
%
\seealso{\code{\link{lstsetLanguage}}}
%
\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
%
\examples{
require(MASS)
\donttest{
lstsetLanguage(pkgs = c("MASS","stats"),
               keywordstyles  = paste("\\\\bfseries\\\\color{",c("blue","red"),"}",
                         sep="", collapse=""))
changeKeywordstyles(pkgs = c("distr","distrEx"),
                    keywordstyles = paste("\\\\bfseries\\\\color{",c("green","blue"),"}",
                    collapse="", sep = ""))
}
}
%
\keyword{utilities}
