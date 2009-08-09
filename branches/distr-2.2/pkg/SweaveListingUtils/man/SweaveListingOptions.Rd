\name{SweaveListingOptions}
\alias{SweaveListingOptions}
\alias{SweaveListingoptions}
\alias{getSweaveListingOption}
\alias{Rset}
\alias{Rdset}
\alias{Rin}
\alias{Rout}
\alias{Rcode}
\alias{Rcolor}
\alias{Rbcolor}
\alias{Routcolor}
\alias{RRecomdcolor}
\alias{Rcommentcolor}
\alias{pkg}
\alias{pkv}
\alias{Keywordstyle}
\alias{Recomd.Keywordstyle}
\alias{interm.Keywordstyle}
\alias{intermediate}
\alias{overwrite}
\alias{inSweave}
\alias{fromRForge}
\alias{base.url}
\alias{addRset}
\alias{addRdset}

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
\code{        "otherkeywords" = }%
\code{        "otherkeywords" = "{!,!=,~,$,*,\\\\&,\\\\ \%/\\\\ \%,\\\\ \%*\\\\ \%,\\\\ \%\\\\ \%,<-,<<-,/}")}\cr
(CAVEAT: the spaces between the double backslashes and the percent sign in the last line
are \emph{not} intended; I simply did not find another work-around)
}
\item{Rdset:}{%
default setting for \\lstset-definition of Rd-code, default value is\cr
\code{list("fancyvrb" = "true", "language" = "Rd",}\cr
\code{        "basicstyle" = "{\\\\color{black}\\\\tiny}",}\cr
\code{        "keywordstyle" = "{\\\\bf}",}\cr
\code{        "commentstyle" = "\\\\ttfamily\\\\itshape",}\cr
\code{        "alsolanguage" = "R")}
}
\item{Rin:}{%
default setting for \\lstdefinestyle-definition of \code{Rinstyle} (for \R input),
default value is\cr
\code{list("style" = "Rstyle", "fancyvrb" = "true",}\cr
\code{        "basicstyle" = "\\\\color{Rcolor}\\\\small")}
}
\item{Rout:}{%
default setting for \\lstdefinestyle-definition of \code{Routstyle} (for \R output),
default value is\cr
\code{list("fancyvrb" = "false", "basicstyle" = "\\\\color{Rout}\\\\small")}
}
\item{Rcode:}{%
default setting for \\lstdefinestyle-definition of \code{Rcodestyle} (for \R code),
default value is\cr
\code{list("style" = "Rstyle", "fancyvrb" = "true",}\cr
\code{        "basicstyle" = "\\\\color{Rcolor}\\\\small")}
\code{        "fontshape"= "sl", "basicstyle" = "\\\\color{Rcolor}")}
}
\item{Rcolor:}{default setting for color of R-code in rgb-coordinates; 
               defaults to \code{c(0,0.5,0.5)}}
\item{RRecomdcolor:}{default setting for color of R-code from recommended packages in rgb-coordinates;
               defaults to \code{c(0,0.6,0.4)}}
\item{Rbcolor:}{default setting for color of R-code symbols of intermediate packages
                in rgb-coordinates;  defaults to \code{c(0,0.6,0.6)}}
\item{Routcolor:}{default setting for color of R-output in rgb-coordinates;
               defaults to \code{c(0.461,0.039,0.102)}}
\item{Rcommentcolor:}{default setting for color of R-comments in rgb-coordinates;
               defaults to \code{c(0.101,0.043,0.432)}}
\item{pkv:}{default setting for package version as character; 
               defaults to \code{"2.0.2"}}
\item{inSweave:}{logical; default setting for masked functions \code{library} and \code{require};
                 shall they show Sweave-behaviour? defaults to \code{FALSE}}
\item{Keywordstyle:}{default setting for \R-symbols as character; 
               defaults to \code{"{\\\\bf}"}}
\item{interm.Keywordstyle:}{default setting for \R-symbols in an intermediate package as character;
               defaults to \code{"{\\\\bf\\\\color{Rbcolor}}"}}
\item{Recomd.Keywordstyle:}{default setting for \R-symbols in package \pkg{base} or
       another recommended package as character;
               defaults to \code{"{\\\\bf\\\\color{Recomdcolor}}"}}
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
            (character); defaults to\cr
            "http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/*checkout*/pkg/"
            }

\item{addRset:}{logical; default for argument \code{add} in command \code{Rset};
        defaults to \code{TRUE}}
\item{addRdset:}{logical; default for argument \code{add} in command \code{Rdset};
        defaults to \code{TRUE}}
\item{addRinset:}{logical; default for argument \code{add} in command
       \code{Rinset}; defaults to \code{TRUE}}
\item{addRoutset:}{logical; default for argument \code{add} in command
       \code{Routset}; defaults to \code{TRUE}}
\item{addRcodeset:}{logical; default for argument \code{add} in command
       \code{Rcodeset}; defaults to \code{TRUE}}
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
