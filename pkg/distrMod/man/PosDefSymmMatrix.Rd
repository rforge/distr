\name{PosDefSymmMatrix}
\alias{PosDefSymmMatrix}
\alias{PosSemDefSymmMatrix}

\title{Generating functions for PosSemDefSymmMatrix-class resp. PosDefSymmMatrix-class}
\description{Generates an object of class \code{"PosSemDefSymmMatrix"} resp. of class \code{"PosDefSymmMatrix"}.}
\usage{PosSemDefSymmMatrix(mat)
       PosDefSymmMatrix(mat)}
\arguments{
  \item{mat}{A numeric positive-[semi-]definite, symmetric 
    matrix with finite entries.}
}
\details{
  If \code{mat} is no matrix, \code{as.matrix} is applied.
}
\value{Object of class \code{"PosSemDefSymmMatrix"} resp. of class \code{"PosDefSymmMatrix"}}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},
Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}}
%\note{}
\seealso{\code{\link{PosDefSymmMatrix-class}}}
\examples{
PosSemDefSymmMatrix(1)
PosSemDefSymmMatrix(diag(2))
PosDefSymmMatrix(1)
PosDefSymmMatrix(diag(2))

## The functions are currently defined as
function(mat){ 
    if(!is.matrix(mat)) mat <- as.matrix(mat)
    new("PosSemDefSymmMatrix", mat)
}
#resp.
function(mat){ 
    if(!is.matrix(mat)) mat <- as.matrix(mat)
    new("PosDefSymmMatrix", mat)
}
}
\concept{positive-semi-definite matrix}
\concept{positive-definite matrix}
\concept{symmetric matrix}
\keyword{array}
