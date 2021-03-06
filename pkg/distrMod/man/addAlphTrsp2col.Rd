\name{addAlphTrsp2col}
\alias{addAlphTrsp2col}

\title{"addAlphTrsp2col"}
\description{Adds alpha transparency to a given color.}
\usage{addAlphTrsp2col(col, alpha=255)}
\arguments{
  \item{col}{any valid color}
  \item{alpha}{tranparancy; an integer value in [0,255]}
}
\value{a color in rgb coordinates}
%\references{}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
%\note{}
\examples{
## IGNORE_RDIFF_BEGIN
  addAlphTrsp2col(rgb(1,0.3,0.03), 25)
  ## gives "#FF4C0819" on 32bit and "#FF4D0819" on 64bit
## IGNORE_RDIFF_END
  addAlphTrsp2col("darkblue", 25)
  addAlphTrsp2col("#AAAAAAAA",25)
  palette(rainbow(6))
  addAlphTrsp2col(2, 25)
}
\keyword{distribution}
\concept{S4 distribution class}

