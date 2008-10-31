\name{SweaveListingOptions}
\alias{SweaveListingOptions}
\alias{SweaveListingoptions}
\alias{getSweaveListingOption}
\alias{Rset}
\alias{Rdset}
\alias{Rcolor}
\alias{Rout}
\alias{Rcomment}
\alias{pkg}
\alias{pkv}


\title{Function to change the global options of the package `SweaveListingUtils' }
\description{With \code{SweaveListingOptions} you can inspect and change 
  the global variables of the package \pkg{SweaveListingUtils}. }
\usage{
SweaveListingOptions(...)
SweaveListingoptions(...)
getSweaveListingOption(x)
}
\arguments{
  \item{\dots}{ any options can be defined, using name = value or by passing a list of such tagged values. }
  \item{x}{ a character string holding an option name.}
}
%\details{}
\value{
  \code{SweaveListingOptions()} returns a list of the global variables.\cr
  \code{SweaveListingOptions(x)} returns the global variable \var{x}.\cr
  \code{getSweaveListingOption(x)} returns the global variable \var{x}.\cr
  \code{SweaveListingOptions(x=y)} sets the value of the global variable \var{x} to \var{y}.
}
\section{SweaveListingoptions}{
For compatibility with spelling in package \pkg{distr}, \code{SweaveListingoptions} is 
just a synonym to \code{SweaveListingOptions}.
}
\section{Global Options}{
\describe{
\item{Rset:}{default setting for \\lstset-definition of R-code, default value is
\preformatted{
list("language"="R","escapechar"="`", 
        "fancyvrb"="true","basicstyle"="\\color\{Rcolor\}\\footnotesize",
        "commentstyle"="\\color\{Rcomment\}\\ttfamily\\itshape",
        "literate"="\{<-\}\{\{\$\\leftarrow\$\}\}2",
        "morekeywords"="[2]\{Norm,Pois,lambda,p,d,r,distroptions\}"
        )
}
\item{Rdset:}{default setting for \\lstset-definition of Rd-code, default value is
\preformatted{
list("language"="TeX", "basicstyle"="\\color\{black\}\\tiny",
               "commentstyle"="\\ttfamily\\itshape"
               )
}}
\item{Rcolor:}{default setting for color of R-code in rgb-coordinates; 
               defaults to \code{c(0,0.5,0.5)}}
\item{Rout:}{default setting for color of R-output in rgb-coordinates; 
               defaults to \code{c(0.461,0.039,0.102)}}
\item{Rcomment:}{default setting for color of R-comments in rgb-coordinates; 
               defaults to \code{c(0.101,0.043,0.432)}}
\item{pkv:}{default setting for package version as character; 
               defaults to \code{"2.0.2"}}
\item{pkg:}{default setting for package name as character; 
               defaults to \code{"distr"}}
}
}}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{\code{\link[base]{options}}, \code{\link[base]{getOption}}}
\examples{
SweaveListingOptions()
SweaveListingOptions("Rout")
SweaveListingOptions("Rout" = c(0,0,0))
# or
SweaveListingOptions(Rout = c(0,1,0))
getSweaveListingOption("Rout")
}
\keyword{misc}
\keyword{utilities}
\concept{global options}
\concept{options}
