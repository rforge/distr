\name{Lattice-class}
\docType{class}
\alias{Lattice-class}
\alias{Lattice}

\title{Class "Lattice" }
\description{Class \code{Lattice} formalizes an affine linearly generated grid
   of (support) points  \code{pivot + (0:(Length-1)) * width};
 this is used for subclass \code{LatticeDistribution} of class
 \code{DiscreteDistribution} which in addition to the latter contains a slot 
 \code{lattice} of class \code{Lattice}. 
 }

\section{Objects from the Class}{
Objects may be generated by calling the generating function \code{Lattice}.
}

\usage{
  Lattice(pivot = 0, width = 1, Length = 2, name = "a lattice")
}
\arguments{
  \item{pivot}{the (finite) utmost left or right value of the lattice}
  \item{width}{the (finite) grid-width; if negative the lattice is expanded to 
               the left, else to the right}
  \item{Length}{the (possibly infinite) length of the lattice}
  \item{name}{the (possibly empty) name of the lattice 
              (inherited from class \code{rSpace})}
}

\section{Slots}{
  \describe{
    \item{\code{pivot}}{Object of class \code{"numeric"}: 
                         --- the pivot of the lattice; must be of length 1}
    \item{\code{width}}{Object of class \code{"numeric"}: 
                         --- the width of the lattice; must be of length 1 and 
                         must not be 0}
    \item{\code{Length}}{Object of class \code{"numeric"}: 
                          --- the width of the lattice; must be an integer > 0 
                          of length 1}
    \item{\code{name}}{Object of class \code{"character"}: 
                        the name of the space, by default = "a lattice"}
  }
}
\section{Extends}{
Class \code{"rSpace"}, directly.\cr
}

\section{Methods}{
  \describe{
    \item{pivot}{\code{signature(.Object = "Lattice")}: 
         returns the \code{'pivot'} slot}
    \item{pivot<-}{\code{signature(.Object = "Lattice")}:
         modifies the \code{'pivot'} slot}
    \item{width}{\code{signature(.Object = "Lattice")}: 
         returns the \code{'width'} slot }
    \item{width<-}{\code{signature(.Object = "Lattice")}: 
         modifies the \code{'width'} slot}
    \item{Length}{\code{signature(.Object = "Lattice")}: 
         returns the \code{'Length'} slot }
    \item{Length<-}{\code{signature(.Object = "Lattice")}: 
         modifies the \code{'Length'} slot }
}
}

\author{
  Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}

\seealso{
\code{\link{rSpace-class}}
\code{\link{LatticeDistribution-class}}
}

\examples{
L <- Lattice(pivot = 0, width = 1, Length = Inf, name = "the Naturals")
name(L)
pivot(L) <- 1 ### now starting from 1
}

\keyword{distribution}
\concept{space}
\concept{S4 space class}
\concept{set}
