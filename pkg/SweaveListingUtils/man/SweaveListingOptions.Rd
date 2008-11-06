\name{SweaveListingOptions}
\alias{SweaveListingOptions}
\alias{SweaveListingoptions}
\alias{getSweaveListingOption}
\alias{Rset}
\alias{Rdset}
\alias{Rcolor}
\alias{Rbcolor}
\alias{Rout}
\alias{Rcomment}
\alias{pkg}
\alias{pkv}
\alias{Keywordstyle}
\alias{interm.Keywordstyle}
\alias{intermediate}
\alias{overwrite}
\alias{inSweave}
\alias{fromRForge}
\alias{base.url}

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
\item{Rset:}{%
default setting for \\lstset-definition of R-code, default value is\cr
\code{list("fancyvrb" = "true", "language" = "R", "escapechar" = "`", }\cr 
\code{        "basicstyle" = "{\\\\color{Rcolor}\\\\footnotesize}",}\cr
\code{        "keywordstyle" = "{\\\\bf\\\\color{Rcolor}}",}\cr
\code{        "commentstyle" = "{\\\\color{Rcomment}\\\\ttfamily\\\\itshape}",}\cr
\code{        "literate" = "{<-}{{$\\\\leftarrow$}}2{<<-}{{$\\\\twoheadleftarrow$}}2",}\cr
\code{        "alsoother" = "{$}","alsoletter" = "{.<-}"}\cr
\code{        "otherkeywords" = "{!,!=,~,$,*,\\\\&,\\\\ \%/\\\\ \%,\\\\ \%*\\\\ \%,\\\\ \%\\\\ \%,<-,<<-,/}")}\cr
(CAVEAT: the spaces between the double backslashes and the percent sign in the last line 
are \emph{not} intended; I simply did not find another work-around)
}
\item{Rdset:}{%
default setting for \\lstset-definition of Rd-code, default value is\cr
\code{list("fancyvrb" = "true", "language" = "Rd", "basicstyle" = "{\\\\color{black}\\\\tiny}",}\cr
\code{        "keywordstyle" = "{\\\\bf}",}\cr
\code{        "commentstyle" = "\\\\ttfamily\\\\itshape",}\cr
\code{        "alsolanguage" = "R")}
}
\item{Rcolor:}{default setting for color of R-code in rgb-coordinates; 
               defaults to \code{c(0,0.5,0.5)}}
\item{Rbcolor:}{default setting for color of R-code symbols of intermediate packages
                in rgb-coordinates;  defaults to \code{c(0,0.4,0.4)}}
\item{Rout:}{default setting for color of R-output in rgb-coordinates; 
               defaults to \code{c(0.461,0.039,0.102)}}
\item{Rcomment:}{default setting for color of R-comments in rgb-coordinates; 
               defaults to \code{c(0.101,0.043,0.432)}}
\item{pkv:}{default setting for package version as character; 
               defaults to \code{"2.0.2"}}
\item{inSweave:}{logical; default setting for masked functions \code{library} and \code{require};
                 shall they show Sweave-behaviour? defaults to \code{FALSE}}
\item{Keywordstyle:}{default setting for \R-symbols as character; 
               defaults to \code{"{\\\\bf}"}}
\item{interm.Keywordstyle:}{default setting for package name as character; 
               defaults to \code{"{\\\\bf\\\\color{Rbcolor}}"}}
\item{intermediate:}{default setting: shall symbols from automatically loaded
               intermediate packages also be registered for listings-printing? 
               defaults to \code{TRUE}}
\item{overwrite:}{default setting: shall \R symbols already defined in the
               original \R language definition in \file{lstlang3.sty} be overwritten
               if they reappear in a required package (to be printed in a 
               different keywordstyle)?
               defaults to \code{FALSE}}
\item{fromRForge:}{default setting: logical: shall code sniplets be drawn from
                   "r-forge.r-project.org" (or a mirror) or not; defaults to 
                   \code{TRUE}.}
\item{base.url:}{default setting for the base url to download code sniplets from
            (character); defaults to
            "http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/*checkout*/pkg/"
            }

}
}
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
