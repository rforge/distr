\name{lstinputSourceFromRForge}
\alias{lstinputSourceFromRForge}

\title{lstinputSourceFromRForge}
\description{
copies lines of a source file (usually \file{.R} oder \file{.Rd})
from R forge repository}
\usage{
lstinputSourceFromRForge(PKG, TYPE, FILENAME, PROJECT, from, to,
                                 offset.before = 0, offset.after = 0,
                                 LineLength = 80,
                                 withLines = ifelse(TYPE=="R", TRUE, FALSE))
}
\arguments{
  \item{PKG}{character; name of package to be downloaded}
  \item{TYPE}{character; style of the source code --- "man" or "R"}
  \item{FILENAME}{character; the name of the source file to be downloaded}
  \item{PROJECT}{character; the name of the R-Forge project}
  \item{from}{vector of characters  or vector of numerics or missing;
  beginnings of the code sniplets; for details see \code{\link{copySourceFromRForge}}
  }
  \item{to}{vector of characters  or vector of numerics or missing;
  endings of the code sniplets; for details see \code{\link{copySourceFromRForge}}
  }
  \item{offset.before}{numeric; numbers of lines to be included before the code
                       sniplets; defaults to 0}
  \item{offset.after}{numeric; numbers of lines to be included after the the code
                       sniplets; defaults to 0}
  \item{LineLength}{\code{numeric} number of characters per line;
  defaults to 80;}
  \item{withLines}{logical; shall line-numbers be issued}
  }

\details{
includes [downloaded] code sniplets in \file{.R} \file{.Rd} format
in some TeX-\command{lstlistings} environment;
output is issued on stdout, hence included in \file{.Rnw} file if wrapped to
\preformatted{
<< /chunkname/, results=tex, echo=FALSE>>=
 ... R code ...
@
}
For example
\preformatted{
<<BinomParam, results=tex, echo=FALSE>>=
lstinputSourceFromRForge("distr","man","BinomParameter-class.Rd","distr")
@
%
<<skew, results=tex, echo=FALSE>>=
lstinputSourceFromRForge("distrEx","R","Skewness.R","distr",
                     from = "\\"skewness\\", signature\\\\(x = \\"Binom\\"",
                     to = "\\ \}\\\\)")
@
}
within \file{.Rd} code, examples are type-set in \file{.R} style;
vectors are allowed for arguments \code{from}, \code{to};
\code{lstinputSourceFromRForge} uses
\code{\link{copySourceFromRForge}} for download / reading from cache;
line numbers in the downloaded source may be printed out
}

\value{
\code{invisible()}
}

\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
\examples{
lstinputSourceFromRForge("distr","R","AllClasses.R","distr",
                     "## Class: BinomParameter", "#-")

lstinputSourceFromRForge("distr","R","AllClasses.R","distr",
                     from = "## Class: binomial distribution",
                     to = "contains = \\"LatticeDistribution\\"", offset.after = 1)
lstinputSourceFromRForge("distr","man","Binom-class.Rd","distr")

lstinputSourceFromRForge("distr","R","BinomialDistribution.R","distr",
                     from = c("## Access Methods", "## wrapped access methods"),
                     to = c("setReplaceMethod\\\\(\\"prob\\", \\"BinomParameter\\"",
                            "size = value\\\\)\\\\)") ,
                     offset.after = c(1,1))
lstinputSourceFromRForge("distr","R","BinomialDistribution.R","distr",
                     from = c(8,43,45), to = c(16,53,45))
lstinputSourceFromRForge("distr","R","BinomialDistribution.R","distr",
                     from = c("## Access Methods", "## wrapped access methods"),
                     to = c("setReplaceMethod\\\\(\\"prob\\", \\"BinomParameter\\"",
                            "size = value\\\\)\\\\)") ,
                     offset.after = c(1,1))
}

\keyword{utilities}


