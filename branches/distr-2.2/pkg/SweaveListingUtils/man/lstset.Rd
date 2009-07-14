\name{lstset}
\alias{lstset}
\alias{lstsetRd}
\alias{lstsetR}
\alias{lstsetRin}
\alias{lstsetRout}
\alias{lstsetRcode}

\title{lstset and friends}
\description{
Functions for defining how listings prints R and Rd source code}
\usage{
lstset(taglist, LineLength = getOption("width"), startS = "\\\\lstset\{")
lstsetR(Rset = NULL, LineLength = getOption("width"), add = getSweaveListingOption("addRset"),
        startS = "\\\\lstset\{")
lstsetRd(Rdset = NULL, LineLength = getOption("width"), add = getSweaveListingOption("addRdset"),
         startS = "\\\\lstset\{")
lstsetRin(Rinset = NULL, LineLength = getOption("width"), add = getSweaveListingOption("addRinset"),
        startS = "\\\\lstdefinestyle\{Rinstyle\}\{")
lstsetRout(Routset = NULL, LineLength = getOption("width"), add = getSweaveListingOption("addRoutset"),
        startS = "\\\\lstdefinestyle\{Routstyle\}\{")
lstsetRcode(Rcodeset = NULL, LineLength = getOption("width"), add = getSweaveListingOption("addRcodeset"),
        startS = "\\\\lstdefinestyle\{Rcodestyle\}\{")
}

\arguments{
  \item{LineLength}{\code{numeric} number of characters per line for lstset and friends;
  defaults to getOption("width")}
  \item{taglist}{S3-object of class \code{taglist}; arguments for
                \command{\\lstset} of TeX package \emph{listings}.}
\item{Rset}{object of S3-class \code{taglist} or named \code{list} of 
            \code{character}s; the listings settings for R-code
            (for \command{\\lstset}); defaults to \code{NULL}.}
\item{Rinset}{object of S3-class \code{taglist} or named \code{list} of
            \code{character}s; the listings settings for R-code in
            environment \code{Sinput}; defaults to \code{NULL}.}
\item{Routset}{object of S3-class \code{taglist} or named \code{list} of
            \code{character}s; the listings settings for R-code in
            environment \code{Soutput}; defaults to \code{NULL}.}
\item{Rcodeset}{object of S3-class \code{taglist} or named \code{list} of
            \code{character}s; the listings settings for R-code in
            environment \code{Scode}; defaults to \code{NULL}.}
\item{Rdset}{object of S3-class \code{taglist} or named \code{list} of
             \code{character}s;
             the listings settings for Rd-code
            (for \command{\\lstset}); defaults to \code{NULL}.}
\item{add}{boolean; defaults to \code{TRUE}; if \code{TRUE}, argument list 
           \code{Rset} resp. \code{Rdset} will be appended to default value 
           lists \code{getSweaveListingOption("Rset")} resp.  
           \code{getSweaveListingOption("Rdset")} (see below),
           overwriting respective entries of the default value lists.} 
\item{startS}{character; defaults to \code{"\\\\lstset\{"}; what to do
              by default we use \code{\\lstset}; an alternative is
              to use \code{\\lstdefinestyle} which amounts to
              \code{"\\\\lstdefinestyle\{"}.}
  }

\details{
\code{lstset} writes out to stdout a call to TeX command 
\command{\\lstset\{arg1 = val1, arg2 = val2, .....\}}
and doing so respects a maximal number of characters per line and does not
break \command{arg=val} tags.

\code{lstsetR} and \code{lstsetRd} expect either
objects of S3 class \code{taglist}, or \code{lists} of named characters as
first arguments, which in the latter case are then converted to \code{taglist};
both \code{lstsetR} and \code{lstsetRd} use particular default values to
define R resp. Rd output format. More
specifically for R code, it uses 
\link[=SweaveListingOptions]{getSweaveListingOption("Rset")},
and for Rd code, it 
\link[=SweaveListingOptions]{getSweaveListingOption("Rdset")};
\code{lstsetRin}, \code{lstsetRout}, and \code{lstsetRcode}
are corresponding specialized commands for 'listings' environments
\code{Sinput}, \code{Soutput}, and \code{Scode}, respectively.

The output to stdout can be captured in an \file{.Rnw} file as
\preformatted{
<< lstsetR, results=tex, echo=FALSE>>=
lstsetR()
@
}
to insert the corresponding \command{\\lstset} command to the produced
TeX file.
}

\value{
\code{invisible()}
}

\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
\examples{
lstset(taglist(A="H", b=2, 3),30)
lstset(taglist(A="H", b=2, 3),30, startS = "\\\\lstdefinestyle{Rstyle}\{")
lstsetR()
lstsetRd()
}

\keyword{utilities}

