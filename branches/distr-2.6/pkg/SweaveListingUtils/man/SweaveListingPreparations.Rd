\name{SweaveListingPreparations}
\alias{SweaveListingPreparations}

\title{SweaveListingPreparations}
\description{
helping tool for writing the corresponding TeX preamble commands
to integrate Sweave and package listings}
\usage{
SweaveListingPreparations(
   withOwnFileSection = FALSE,
   withVerbatim = FALSE,
   withSchunkDef = TRUE,
   gin = TRUE,
   ae = TRUE,
   LineLength = getOption("width"),
   Rset = getSweaveListingOption("Rset"),
   Rdset = getSweaveListingOption("Rdset"),
   Rin = getSweaveListingOption("Rin"),
   Rout = getSweaveListingOption("Rout"),
   Rcode = getSweaveListingOption("Rcode"),
   Rcolor = getSweaveListingOption("Rcolor"),
   RRecomdcolor = getSweaveListingOption("RRecomdcolor"),
   Rbcolor = getSweaveListingOption("Rbcolor"),
   Routcolor = getSweaveListingOption("Routcolor"),
   Rcommentcolor = getSweaveListingOption("Rcommentcolor"),
   pkg = getSweaveListingOption("pkg"),
   pkv = getSweaveListingOption("pkv"),
   fileCommand = getSweaveListingOption("fileCommand"),
   pkgCommand = getSweaveListingOption("pkgCommand"),
   lib.loc = NULL)
}
\arguments{
  \item{withOwnFileSection}{logical: Does one want to use an own definition file/
                     section to define \code{Sinput}, \code{Soutput},
                     \code{Scode} environments; if \code{TRUE} you should write some
                     \kbd{\\include} directive to include your own
                     definitions / or write them in your \file{.Rnw} file.
                     }
  \item{withVerbatim}{logical of length 3 (filled by recycling if of shorter length)
                      either named ("Sinput", "Soutput", "Scode") or taken in
                      order (Sinput, Soutput, Scode):
                       Should we use Verbatim from TeX package 'fancyvrb'
                      (i.e. the original
                      \kbd{"\\DefineVerbatimEnvironment{Sinput}{Verbatim}"} by
                      Fritz Leisch) or just TeX package 'listings'
                      (i.e. we define our own \code{Sinput} environment
                       based on 'listings' command 'lstnewenvironment';
                       this option is due to a suggestion by
                       Andrew Ellis (thank you!).}
  \item{withSchunkDef}{logical: shall environment \verb{Schunk} be defined?}
  \item{gin}{logical: shall we use \code{gin}}
  \item{ae}{logical: shall we use \code{ae}}
  \item{LineLength}{numeric; defaults to 80}
  \item{Rset}{list or taglist; defaults to global option  \code{"Rset"}}
  \item{Rdset}{list or taglist; defaults to global option  \code{"Rdset"}}
  \item{Rin}{list or taglist; format string for \R input code; defaults to global option  \code{"Rin"}}
  \item{Rout}{list or taglist; format string for \R output code; defaults to global option  \code{"Rout"}}
  \item{Rcode}{list or taglist; format string for \R code; defaults to global option  \code{"Rcode"}}
  \item{Rcolor}{numeric of length 3; [color for R-input-code] defaults to global option  \code{"Rcolor"};
                rgb coordinates of the color in which to print R-code}
  \item{RRecomdcolor}{numeric of length 3; [color for R-symbols from recommended packages]
                defaults to global option  \code{"RRecomdcolor"};
                rgb coordinates of the color in which to print R-code}
  \item{Rbcolor}{numeric of length 3; [color for R-symbols from intermediate packages] defaults to global option  \code{"Rbcolor"};
                 rgb coordinates of the color in which to print \R keywords
                 induced by intermediate packages.}
  \item{Routcolor}{numeric of length 3; [color for R-output-code] defaults to global option  \code{"Rout"};
                rgb coordinates of the color in which to print R output}
  \item{Rcommentcolor}{numeric of length 3; [color for R-comments] defaults to global option \code{"Rcomment"};
                rgb coordinates of the color in which to print comments
                in R-code}
  \item{pkg}{character; name of the packageto be described (e.g. in a vignette);
             defaults to global option \code{"pkg"}}
  \item{pkv}{character; package version to be described (e.g. in a vignette);
             defaults to global option \code{"pkv"}}
  \item{fileCommand}{character; the TeX code to define TeX command \verb{\\file}}
  \item{pkgCommand}{character; the TeX code to define TeX command \verb{\\pkg}}
  \item{lib.loc}{location of a local library in which the described package resides}
  }

\details{
\code{SweaveListingPreparations}
writes a corresponding preamble to the \file{.Rnw}-file for the simultaneous use
of Sweave and package listings; note that so far, even if you do not want to
use the default style file \file{Sweave.sty}, the lines
\preformatted{
\%\\usepackage{Sweave}
\\SweaveOpts{keep.source=TRUE}
}
still have to appear in the \file{.Rnw} file --- before the corresponding
\code{SweaveListingPreparations}-chunk.\cr
The TeX code inserted by \code{SweaveListingPreparations} sets colors,
requires TeX-package \file{listings},
defines listings-settings for \file{.R}- and \file{.Rd}-code.
For \file{.Rd} files it uses a new 'listings'-language definition file, 
\file{Rdlisting.sty} to be found in subfolder \file{TeX} in the 
\pkg{SweaveListingUtils} package folder.
\code{SweaveListingPreparations} is to be called in an \file{.Rnw} file and 
[re-]defines \code{Sinput},
\code{Soutput}, \code{Scode} environments for use with TeX-package \file{listings},
defines commands \code{\\code}, \code{\\file}, \code{\\pkg} and sets
the corresponding package version.\cr
The default values are taken from
\link[=SweaveListingOptions]{SweaveListingOptions}.
The output to stdout can be captured in an \file{.Rnw} file as
\preformatted{
<< lstPreamble, results=tex, echo=FALSE>>=
require(SweaveListingUtils)
SweaveListingPreparations()
@
}
to insert the corresponding preamble parts to the produced
TeX file.\cr
If you one want to use destinct keywordstyles for packages loaded
by \code{require} or \code{library} without adding extra arguments
to these commands (and hence displaying the actual \R code),
you have to set up a global formatting matrix  
\code{.tobeDefinedPkgs} by \code{\link{setToBeDefinedPkgs}}.
}

\value{\code{invisible()}
}
\section{Acknowledgement}{
The author wants to thank Frank E. Harrel and Andrew Ellis for
very valuable suggestions to enhance this package.
}

\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
\examples{
SweaveListingPreparations()
SweaveListingPreparations(pkv="2.1")
}

\keyword{utilities}


