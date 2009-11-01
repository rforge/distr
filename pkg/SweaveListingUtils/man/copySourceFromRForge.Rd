\name{copySourceFromRForge}
\alias{copySourceFromRForge}

\title{copySourceFromRForge}
\description{
copies lines of a source file (usually \file{.R} oder \file{.Rd})
from R forge repository}
\usage{
copySourceFromRForge(PKG, TYPE, FILENAME, PROJECT, from, to,
                     offset.before = 0, offset.after = 0,
                     fromRForge = getSweaveListingOption("fromRForge"),
                     base.url = getSweaveListingOption("base.url") )
}
\arguments{
  \item{PKG}{character; name of package to be downloaded}
  \item{TYPE}{character; style of the source code --- "man" or "R"}
  \item{FILENAME}{character; the name of the source file to be downloaded}
  \item{PROJECT}{character; the name of the R-Forge project}
  \item{from}{single character  or single numeric or missing; if character,
  the starting string being searched (by \code{grep}, hence as regular expression);
  if numeric, the starting line number, if missing we begin with the first line
  of the file}
  \item{to}{single character  or single numeric or missing; if character,
  the ending string being searched (by \code{grep}, hence as regular expression);
  if numeric, the ending line number, if missing we end with the last line
  of the file}
  \item{offset.before}{numeric; number of lines to be included before the first
                       match; defaults to 0}
  \item{offset.after}{numeric; number of lines to be included after the first
                       match; defaults to 0}
  \item{fromRForge}{logical; shall code be downloaded from an R-Forge mirror?
                    Defaults to the corresponding global option}
  \item{base.url}{character; base url from where to download the code sniplet}
  }

\details{
produces a vector of characters where each component is one line of
the original source file;
arguments \code{from}, \code{to} may be
\itemize{
\item missing: then the whole file (resp. from the beginning or to the end) is used
\item numbers: then the limits are just given as line numbers
\item characters: then file is searched for the first occurrance of a passage
      framed by \code{from}, \code{to}; uses \code{grep}; hence
      \link[=regexp]{regular expressions} may be used which involves masking
      of (,\{,\\, etc.  as described in the cited reference;
}
with offsets, additional lines may be pasted before and after the search result
}

\value{
the character content of the filtered source file,
if nothing is found   it returns \code{invisible()}.
}

\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
\examples{
copySourceFromRForge("distr","R","AllClasses.R","distr", from =2, to =3,
                     offset.after=2)
copySourceFromRForge("distr","R","AllClasses.R","distr", from ="setClass",
                      to ="\}")
}
\keyword{utilities}


