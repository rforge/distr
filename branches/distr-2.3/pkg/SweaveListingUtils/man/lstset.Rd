\name{lstset}
\alias{lstset}
\alias{lstsetRd}
\alias{lstsetR}
\alias{lstsetRin}
\alias{lstsetRout}
\alias{lstsetRcode}
\alias{lstdefRstyle}
\alias{lstsetRall}

\title{lstset and friends}
\description{
Functions for defining how listings prints R and Rd source code}
\usage{
lstset(taglist, LineLength = getOption("width"), startS = "\\\\lstset\{")
lstsetR(Rset = NULL, LineLength = getOption("width"),
        add = getSweaveListingOption("addRset"),
        startS = "\\\\lstset\{", append = TRUE, withRstyle = FALSE)
lstsetRd(Rdset = NULL, LineLength = getOption("width"),
         add = getSweaveListingOption("addRdset"),
         startS = "\\\\lstset\{",
                    append = TRUE)
lstsetRin(Rinset = NULL, LineLength = getOption("width"),
          add = getSweaveListingOption("addRinset"),
          startS = "\\\\lstdefinestyle\{Rinstyle\}\{",
                    append = TRUE)
lstsetRout(Routset = NULL, LineLength = getOption("width"),
           add = getSweaveListingOption("addRoutset"),
           startS = "\\\\lstdefinestyle\{Routstyle\}\{",
                    append = TRUE)
lstsetRcode(Rcodeset = NULL, LineLength = getOption("width"),
            add = getSweaveListingOption("addRcodeset"),
            startS = "\\\\lstdefinestyle\{Rcodestyle\}\{",
                    append = TRUE)
lstsetRall(Rallset = NULL, LineLength = getOption("width"),
           add = c("in" = getSweaveListingOption("addRinset"),
                   "out" = getSweaveListingOption("addRoutset"),
                   "code" = getSweaveListingOption("addRcodeset")),
           startS = c("in" = "\\\\lstdefinestyle\{Rinstyle\}\{",
                      "out" = "\\\\lstdefinestyle\{Routstyle\}\{",
                      "code" = "\\\\lstdefinestyle\{Rcodestyle\}\{"),
           append = c("in" = TRUE, "out" = TRUE, "code" = TRUE),
           withOptionsDefAppend = TRUE)
lstdefRstyle(Rset = NULL, LineLength = getOption("width"),
             add = TRUE)
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
\item{Rallset}{object of S3-class \code{taglist} or named \code{list} of
            \code{character}s; the listings settings for R-code in
            environments \code{Sinput}, \code{Soutput}, \code{Scode},
            simultaneously; defaults to \code{NULL}.}
\item{Rdset}{object of S3-class \code{taglist} or named \code{list} of
             \code{character}s;
             the listings settings for Rd-code
            (for \command{\\lstset}); defaults to \code{NULL}.}
\item{add}{boolean; defaults to \code{TRUE}; if \code{TRUE}, argument list 
           \code{Rset} resp. \code{Rdset} will be appended to default value 
           lists \code{getSweaveListingOption("Rset")} resp.  
           \code{getSweaveListingOption("Rdset")} (see below),
           overwriting respective entries of the default value lists;
           in case of \code{lstsetRall} may be named vector of length 3
           with names \code{c("in","out","code")}.}
\item{startS}{character; defaults to \code{"\\\\lstset\{"}; what to do
              by default we use \code{\\lstset}; an alternative is
              to use \code{\\lstdefinestyle} which amounts to
              \code{"\\\\lstdefinestyle\{"};
              in case of \code{lstsetRall} may be named vector of length 3
              with names \code{c("in","out","code")}.}
\item{append}{logical; if \code{TRUE} (default) the new settings are appended
to the old ones; otherwise they are prepended; in case of \code{lstsetRall}
may be named vector of length 3 with names \code{c("in","out","code")}.}
\item{withRstyle}{logical; if \code{TRUE} \code{"style = Rstyle,"} is prepended.}
\item{withOptionsDefAppend}{logical: shall definitions from global options
(see \code{\link{SweaveListingoptions}}) be included?}
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

\code{lstsetRall} simultaneously sets/modifies settings for
'listings' environments
\code{Sinput}, \code{Soutput}, and \code{Scode}.

\code{lstsetdefRstyle} redefines listings style
definition for \code{Rstyle}.

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

