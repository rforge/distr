\name{lstsetLanguage}
\alias{lstsetLanguage}
\title{lstsetLanguage}
\description{
registers the symbols of a package or a position in the search list
as \emph{morekeywords} for the TeX-package \file{listings} language
definition of \R}

\usage{
lstsetLanguage(pkgs, posIdx, keywordstyles, overwrite = FALSE)
}

\arguments{
  \item{pkgs}{character; the packages the symbols of which are to be registered.}
  \item{posIdx}{numeric; positions in the \code{search()} list from which the
  symbols are to be registered.}
  \item{keywordstyles}{character or missing; the corresponding keywordstyle format
   strings; if missing the corresponding option \code{Keywordstyle} is read off
   by using\cr \code{getSweaveListingOption("Keywordstyle")}.\cr
   Internally, it is being cast to the same length as \code{pkgs} by
  \code{rep(keywordstyles, length.out = length(pkgs))}.
  }
  \item{overwrite}{logical; before registering the new symbols shall we check
        if there already is a registration of this symbol in the original
        \R language definition for TeX package \file{listings} of file
        \file{lstlang3.sty} provided by Robert Denham; in package
        \pkg{SweaveListingUtils}, this information is available in
        the non-exported global object \code{.keywordsR}
        in the namespace of this package; if \code{TRUE} we
        overwrite existing registrations; default is \code{FALSE}.}
}

\value{
\code{invisible()}
}

\details{
Arguments \code{pkgs} and \code{posIdx} can be used independently from each other:
If there is an argument \code{pkgs}, after checking whether these packages are
already on the search list, we unite the corresponding search list positions
with those of argument \code{posIdx} (if the latter is given);
the positions corresponding to packages already in the \code{.alreadyDefinedPkgs}
vector (see below), are filtered out, however. If argument
\code{pkgs} is missing, by default the whole list of attached packages gained
from \code{.packages()} is taken in the beginning.
For registering the new symbols, we write out a
\preformatted{
 \\lstdefinelanguage{R}\%
     {morekeywords=[<order number>]{ <comma-separated keywordlist> },\%
      sensitive=true,\%
      keywordstyle=[<order number>]<keywordstyle as format string>,\%
      }
}
directive to standard out, where \code{<comma-separated keywordlist>} is
a comma-separated list of the keywords to be registered printed out as
five items per line; we get this list by a corresponding
\code{ls(pos=<position>)} command. If argument \code{overwrite} is \code{FALSE},
before registration, we filter out the keywords already in the original
\file{listings} \R language definition.
\code{<keywordstyle as format string>} may be a string containing
any sequence of TeX formatting commands like
\code{"\\\\bfseries\\\\footnotesize"}. Note that backslashes have to be escaped.
\cr

To be able to distinguish/manage several keyword format styles on \R-side, we
append the name of each package, the symbols of which are registered, to the
(non-exported) vector object \code{.alreadyDefinedPkgs}, which is hidden in the
namespace of this package.

On TeX/\file{listings}-side, the different keyword format styles are managed
by the corresponding \code{<order number>} information in the
\code{morekeywords} tag; it is identified with \code{num+2} where \code{num}
is the index of the package in the \code{.alreadyDefinedPkgs} vector.\cr

The settings of these format styles may afterwords be overwritten
using \code{\link{changeKeywordstyles}}.\cr

For use in an \code{.Rnw} file, the call to \code{lstsetlanguage} should be
wrapped into a corresponding Sweave chunk in the form
\preformatted{
<< /chunkname/, results=tex, echo=FALSE>>=
lstsetLanguage( ..... )
@
}
for example
\preformatted{
<<distrRegisterKeywords, results=tex, echo=FALSE>>=
lstsetLanguage("distr", keywordstyles = "\\\\bfseries\\\\color{green}")
@
}
%
}

\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
\examples{
require(MASS)
lstsetLanguage(pkgs = c("MASS","stats"),
               keywordstyles  = paste("\\\\bfseries\\\\color{",c("blue","red"),"}",
                         sep="", collapse=""))
### not to be used:
print(SweaveListingUtils:::.alreadyDefinedPkgs)
print(SweaveListingUtils:::.keywordsR)
}
\keyword{utilities}
