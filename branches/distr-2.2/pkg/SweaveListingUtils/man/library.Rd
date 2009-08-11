% just modified:
% File src/library/base/man/library.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{library}
\title{Loading Packages with registering symbols for TeX package 'listing'}
\alias{library}
\alias{require}
\description{
  \code{library} and \code{require} load add-on packages.
  In the masked versions of package \pkg{SweaveListingUtils} documented here, they
  also register corresponding symbols. Besides this registration, they
  behave identically to the original versions of package \pkg{base}.
  Hence much of this help is also just copied from the original help page.
}
\usage{
library(package, help, pos = 2, lib.loc = NULL,
        character.only = FALSE, logical.return = FALSE,
        warn.conflicts = TRUE,
        keep.source = getOption("keep.source.pkgs"),
        verbose = getOption("verbose"),
        version, inSweave,
        keywordstyles, interm.keywordstyles, overwrite, intermediate)

require(package, lib.loc = NULL, quietly = FALSE,
        warn.conflicts = TRUE,
        keep.source = getOption("keep.source.pkgs"),
        character.only = FALSE, version, save = TRUE, inSweave,
        keywordstyles, interm.keywordstyles, overwrite, intermediate)
}
\arguments{
  \item{package, help}{the name of a package, given as a \link{name} or
    literal character string, or a character string, depending on
    whether \code{character.only} is \code{FALSE} (default) or
    \code{TRUE}).}
  \item{pos}{the position on the search list at which to attach the
    loaded package.  Note that \code{.First.lib} may attach other
    packages, and \code{pos} is computed \emph{after} \code{.First.lib}
    has been run.  Can also be the name of a position on the current
    search list as given by \code{\link{search}()}.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known.
    Non-existent library trees are silently ignored.}
  \item{character.only}{a logical indicating whether \code{package} or
    \code{help} can be assumed to be character strings.}
  \item{version}{A character string denoting a version number of the
     package to be loaded, for use with \emph{versioned installs}: see
     the section later in this document.}
  \item{logical.return}{logical.  If it is \code{TRUE},  \code{FALSE} or
    \code{TRUE} is returned to indicate success.}
  \item{warn.conflicts}{logical.  If \code{TRUE}, warnings are
    printed about \code{\link{conflicts}} from attaching the new
    package, unless that package contains an object
    \code{.conflicts.OK}.  A conflict is a function masking a function,
    or a non-function masking a non-function.
  }
  \item{keep.source}{logical.  If \code{TRUE}, functions
    \sQuote{keep their source} including comments, see argument
    \code{keep.source} to \code{\link{options}}.  This applies only to
    the named package, and not to any packages or name spaces which might be
    loaded to satisfy dependencies or imports.

    This argument does not apply to packages using lazy-loading.
    Whether they have kept source is determined when they are installed
    (and is most likely false).
  }
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed.}
  \item{quietly}{a logical.  If \code{TRUE}, no message confirming
    package loading is printed.}
  \item{save}{logical or environment.  If \code{TRUE}, a call to
    \code{require} from the source for a package will save the name of
    the required package in the variable \code{".required"}, allowing
    function \code{\link{detach}} to warn if a required package is
    detached. See section \sQuote{Packages that require other packages} below.}
%
  \item{inSweave}{shall the command show Sweave behaviour (no startup messages;
                  instead issuing new symbols for \code{morekeywords} tag in
                  language definition of \R in TeX package \file{listings})?
                  By default argument is taken from
                  \code{\link{getSweaveListingOption}}.}
%
  \item{keywordstyles}{character or missing or \code{NULL}; added argument in masked
  versions of \code{library} and \code{require} of package
  \pkg{SweaveListingUtils}; if given, the keywordstyle to be
  used by TeX package 'listings' for the symbols attached in this package
  (to be found with \code{ls()}). Remember that special characters like \\
  have to be escaped. If missing, a special strategy is used to get
  sensible value (see details).}
%
  \item{interm.keywordstyles}{character or missing or \code{NULL}; added argument in masked
  versions of \code{library} and \code{require} of package
  \pkg{SweaveListingUtils}; if given, the keywordstyle to be
  used by TeX package 'listings' for the symbols to be found by automatically
  loaded intermediate packages (mentioned in 'depends' field of the corresponding
  DESCRIPTION file). This will only be used if effective argument
  \code{intermediate} is \code{TRUE}. Remember that special characters like \\
  have to be escaped. If missing, a special strategy is used to get
  sensible value (see details).}
%
  \item{overwrite}{logical or missing or \code{NULL}; added argument in masked
  versions of \code{library} and \code{require} of package
  \pkg{SweaveListingUtils}; if missing, the corresponding global option is read
  out by \code{\link{getSweaveListingOption}}. Let us call the corresponding
  possibly imputed value \emph{effective} value. If this effective value is
  \code{TRUE}, before registering a new symbol for TeX package 'listings',
  we first check whether this symbol had already been registered in
  the original 'listings'-language definition of  \R in file \file{lstlang3.sty}
  [which we have copied to the internal (un-exported) constant
  \code{.keywordsR}] and if so do not include the symbol once again; if the value is \code{FALSE},
  no matter of the original language definition all symbols to be found by \code{ls()}
  are registered and hence typeset in the corresponding package-keywordstyle.}
  %
  \item{intermediate}{logical or missing or \code{NULL}; added argument in masked
  versions of \code{library} and \code{require} of package
  \pkg{SweaveListingUtils}; if missing, the corresponding global option is read
  out by \code{\link{getSweaveListingOption}}. If the effective value is
  \code{TRUE}, also the symbols to be found in newly, automatically loaded
  packages (loaded, because the required/"library"-ed package features them
  in its corresponding \code{Depends} field in its DESCRIPTION file)
  are registered for TeX package 'listings'.}
  %
}
\details{
  \code{library} and \code{require} are masked versions of the original
  versions in package \pkg{base}. This masking is necessary to allow for
  extra arguments \code{keywordstyles}, \code{interm.keywordstyles},
  \code{overwrite}, \code{intermediate}. Hence please confer
  \code{\link[base]{library}}, \code{\link[base]{require}}.\cr

  If argument \code{inSweave} is \code{FALSE} or if argument
  \code{inSweave} is missing and the corresponding global
  option \code{getSweaveListingOption("inSweave")} is \code{FALSE},
  then \code{library} and \code{require} behave exactly as the \pkg{base}
  package versions; only if the effective argument is \code{TRUE},
  the corresponding startup-messages are suppressed, and the TeX
  code for registering all new symbols to be found with \code{ls} as
  \code{morekeywords} for the \R language definition in TeX package
  \file{listings} is issued (and hence can be used in \file{.Rnw} files.

  The strategy to fill arguments \code{keywordstyles} and
  \code{interm.keywordstyles} if missing is as follows: First we check whether
  for the corresponding package, there is an enty in the global
  option variable \code{.tobeDefinedPkgs} (which is a matrix residing in the
  package namespace with one column of package names and one column of
  corresponding keywordstyle format strings)
  and if so use its corresponding keywordstyle; else we use option
  \code{Keywordstyles} resp. \code{interm.Keywordstyles} (which we get
  by \code{getSweaveListingOption}). New entries in the global variable
  \code{.tobeDefinedPkgs} may be appended by \code{\link{setToBeDefinedPkgs}}.\cr


  The masked versions work as follows: first the call is written to an
  object and split into a \pkg{base}-package-part and into the extra arguments
  mentioned above. Then we call \code{base::<fct>} where \code{<fct>} is either
  \code{require} or \code{library}. By noting the list of attached packages
  before and after this call we also get hand on the intermediate packages,
  which are automatically attached packages by dependence. After this call
  we register the new symbols by corresponding calls to
  \code{\link{lstsetLanguage}}.
  
  For use in an \code{.Rnw} file, the call to \code{require} or \code{library}
  should be wrapped into a corresponding Sweave chunk in the form
  \preformatted{
<< /chunkname/, results=tex, echo=TRUE>>=
require( ..... )
@
}
,for example
\preformatted{
<< distrRequire, results=tex, echo=TRUE>>=
require(distr)
@
}
In order to unmask the original versions of \code{library} and \code{require}
again after Sweave-ing your \code{.Rnw} file, at the end of your \code{.Rnw}
file you should include an Sweave chunk like
\preformatted{
<<cleanup, echo=FALSE>>=
unloadNamespace("SweaveListingUtils")
@
}
or, if you want to keep the namespace of \pkg{SweaveListingUtils} loaded
\preformatted{
<<cleanup2, echo=FALSE>>=
SweaveListingUtils(inSweave = FALSE)
@
}
}
\value{
   as the unmasked versions.
}
\seealso{
  \code{\link[base]{library}}.
}
\examples{
require(survival)
}
\keyword{utilities}
