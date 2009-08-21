\name{SweaveListingUtils-package}
\alias{SweaveListingUtils-package}
\alias{SweaveListingUtils}
\docType{package}
\title{
Package SweaveListingUtils
}
\description{
Package \pkg{SweaveListingUtils}  
provides utilities for defining \R / \file{Rd} as TeX-package-\file{listings}
"language" and including \R / \file{.Rd} source file (sniplets) copied
from an url, by default from svn server at R-forge in its most recent version, 
thereby avoiding inconsistencies between vignette and documented source code  
}
\details{
\tabular{ll}{
Package: \tab SweaveListingUtils\cr
Version: \tab 0.4\cr
Date: \tab 2009-08-10\cr
Depends: \tab R(>= 2.0.0), startupmsg\cr
LazyLoad: \tab yes\cr
License: \tab LGPL-3\cr
}

TeX-package \file{listings}, confer \url{http://www.ctan.org/tex-archive/macros/latex/contrib/listings/},
\url{http://www.ctan.org/tex-archive/macros/latex/contrib/listings/listings.pdf}
offers a very powerful setup for typesetting [program] code in TeX.

For quite some time there has already been a corresponding language definition
for \R. We enhance this definition and also introduce a corresponding 
"language" definition file to typeset \file{.Rd} code 
(file \file{Rdlistings.sty} in the \file{TeX} subfolder of this package,
which is according to Duncan Murdoch's ``Parsing Rd Files'' as of Nov. 4 2008.).

In recent versions \file{listings} also cooperates with TeX package 
\file{fancyvrb}, so it can be configured to enhance Sweave typesetting.

Just as a first simple example, comments are recognized automatically and
hence typeset in particular format.

For pretty printing, or moreover literate programming, 
with \pkg{SweaveListingUtils}, assignment operators \code{<-} and 
\code{<<-} get typeset by one symbol each.  

For a corresponding TeX preamble combining Sweave and \file{listings}, 
we provide command \cr
\code{\link{SweaveListingPreparations}}.

As for the \R language definition, we allow for different keywordstyles 
to typeset symbols from different packages. This is useful to distinguish
mark-up for newly defined functions and alread existing ones. 

More specifically, whenever in some \R code sniplet in some Sweave chunk, there
is some \code{library} or \code{require} command, the corresponding symbols
found by \code{ls()} afterwords in some \code{search()} entry position, are 
registered as \verb{<comma-separated keywordlist>} (printed as 5 items per line)
as a new "higher order" group of keywords by corresponding
\preformatted{
 \\lstdefinestyle{RstyleO<numi>}\% RstyleO<numi> is the current order of Rstyle
     {style = RstyleO<numi-1>,
      morekeywords=[<order number>]{ <comma-separated keywordlist> },\%
      sensitive=true,\%
      keywordstyle=[<order number>]<keywordstyle as format string>,\%
      \% [ possibly more
      morekeywords=[<order number + 1>]{ <comma-separated keywordlist> },\%
      sensitive=true,\%
      keywordstyle=[<order number + 1>]<keywordstyle as format string>,\%
      \% .... ]
      }
 \\lstdefinestyle{Rstyle}{style = RstyleO<numi>}
}
TeX directives in the automatically generated \file{.tex} file
where \verb{<order number>} is incremented (resp. gets looked up from
the global, non-exported  variable \code{.alreadyDefinedPkgs} in the package
name space) at each instance of
a \code{library} or \code{require} command; an analogue incrementation
present in \verb{<numi>} is done for subsequent (incremental)
redefinitions of style \verb{Rstyle},
which is controlled by the again non-exported global variable
\code{.numberofRequires}. \verb{<order number>} and \verb{<numi>} will
in general differ, as in one call to \code{require}/\code{library}, several
packages may be registered at once.

To this end commands
\code{\link[SweaveListingUtils]{library}} and 
\code{\link[SweaveListingUtils]{require}} are masked. See also
\code{\link[SweaveListingUtils]{lstsetLanguage}}, 
\code{\link[SweaveListingUtils]{changeKeywordstyles}},
and \code{\link[SweaveListingUtils]{setToBeDefinedPkgs}} 

As for the integration of code sniplets from an url (by default, we use
the svn server at R-forge in its most recent version), this can be 
useful to stay consistent with the current version of the code without
having to update vignettes all the time. To this end, besides referencing
by line numbers, \code{\link{lstinputSourceFromRForge}} also offers
referencing by matching regular expressions.

}


\section{Functions}{
\preformatted{
lstset
lstsetR
lstsetRd
SweaveListingPreparations
readSourceFromRForge
copySourceFromRForge
lstinputSourceFromRForge
readPkgVersion
SweaveListingOptions
getSweaveListingOption
SweaveListingoptions
SweaveListingMASK
setToBeDefinedPkgs
lstsetLanguage
changeKeywordstyles
library
require
}
}
\section{S3 classes and methods}{
\preformatted{
taglist
print.taglist
}
}
\section{Example - .Rnw vignette file}{
An example \file{.Rnw} file, \file{ExampleSweaveListingUtils.Rnw}, may 
be found in the \file{doc} folder of this package.
}

\author{
Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}\cr
\emph{Maintainer:} Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}
}
\note{This version already uses 'new-style' Rd-format version 1.1.
For versions installable for \eqn{<}\R-2.10.0, try and get a version
0.3.x of this package from a suitable repository.}

\keyword{package}
\keyword{utilities}
