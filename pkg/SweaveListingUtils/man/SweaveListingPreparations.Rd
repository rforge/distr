\name{SweaveListingPreparations}
\alias{SweaveListingPreparations}

\title{SweaveListingPreparations}
\description{
helping tool for writing the corresponding TeX preamble commands
to integrate Sweave and package listings}
\usage{
SweaveListingPreparations(LineLength = 80,
   Rset = getSweaveListingOption("Rset"),
   Rdset = getSweaveListingOption("Rdset"),
   Rcolor = getSweaveListingOption("Rcolor"),
   Rout = getSweaveListingOption("Rout"),
   Rcomment = getSweaveListingOption("Rcomment"),
   pkg = getSweaveListingOption("pkg"),
   pkv = getSweaveListingOption("pkv"),
   lib.loc = NULL)
}
\arguments{
  \item{LineLength}{numeric; defaults to 80}
  \item{Rset}{list or taglist; defaults to global option  \code{"Rset"}}
  \item{Rdset}{list or taglist; defaults to global option  \code{"Rdset"}}
  \item{Rcolor}{numeric of length 3; defaults to global option  \code{"Rcolor"};
                rgb coordinates of the color in which to print R-code}
  \item{Rout}{numeric of length 3; defaults to global option  \code{"Rout"};
                rgb coordinates of the color in which to print R output}
  \item{Rcomment}{numeric of length 3; defaults to global option \code{"Rcomment"};
                rgb coordinates of the color in which to print comments
                in R-code}
  \item{pkg}{character; name of the packageto be described (e.g. in a vignette);
             defaults to global option \code{"pkg"}}
  \item{pkv}{character; package version to be described (e.g. in a vignette);
             defaults to global option \code{"pkv"}}
  \item{lib.loc}{location of a local library in which the described package resides}
  }

\details{
\code{SweaveListingPreparations}
writes a corresponding preamble to the \file{.Rnw}-file for the simultaneous use
of Sweave and package listings; note that the lines
\preformatted{
\usepackage{Sweave}
\SweaveOpts{keep.source=TRUE}
}
still have to appear in the \file{.Rnw} file --- before the corresponding
\code{SweaveListingPreparations}-chunk.\cr
The TeX code inserted by \code{SweaveListingPreparations} sets colors,
requires TeX-package \file{listings},
defines listings-settings for \file{.R}- and \file{.Rd}-code.
It is to be called in an \file{.Rnw} file and [re-]defines \code{Sinput},
\code{Soutput}, \code{Scode} environments for use with TeX-package \file{listings},
defines commands \code{\\code}, \code{\\file}, \code{\\pkg} and sets
the corresponding package version.\cr
The default values are taken from
\link[=SweaveListingOptions]{SweaveListingOptions()}.
The output to stdout can be captured in an \file{.Rnw} file as
\preformatted{
<< lstPreamble, results=tex, echo=FALSE>>=
require(SweaveListingUtils)
SweaveListingPreparations()
@
}
to insert the corresponding preamble parts to the produced
TeX file.

}

\value{\code{invisible()}
}

\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
\examples{
SweaveListingPreparations()
SweaveListingPreparations(pkv="2.1")
}

\keyword{utilities}


