\name{internals_for_distrEx}
\alias{internals_for_distrEx}
\alias{.getIntbounds}
\alias{.filterFunargs}
\alias{.filterEargs}
\alias{.nmsToGather}
\alias{.showallNamesDiagnosticList}
\alias{.reorganizeDiagnosticList}

\title{Internal functions of package distrEx}

\description{
These functions are used internally by package distrEx.}

\usage{
.getIntbounds(object, low, upp, lowTQ, uppTQ, IQR.fac, ...)
.filterFunargs(dots, fun, neg = FALSE)
.filterEargs(dots, neg = FALSE)
.reorganizeDiagnosticList(liste, .depth=1, names0, prenames = "",
          nmstoGather="", nmstoGatherNS="",  withprint=TRUE,
          .GatherList = NULL, .GatherListNS = NULL)
.showallNamesDiagnosticList(liste, .depth=1)
.nmsToGather
}


\arguments{
  \item{object}{an object of class \code{"AbscontDistribution"}}
  \item{low}{given lower integration bound}
  \item{upp}{given lower integration bound}
  \item{lowTQ}{lower quantile for quantile based integration range. }
  \item{uppTQ}{upper quantile for quantile based integration range. }
  \item{IQR.fac}{factor for scale based integration range (i.e.; 
  median of the distribution \eqn{\pm}{+-}\code{IQR.fac}\eqn{\times}{*}IQR).}
 \item{dots}{ a list as obtained by \code{list(...)}. }
 \item{\dots}{arguments passed through to other functions (in particular argument
  \code{cond} in \code{.getIntbounds})}
  \item{neg}{logical: if \code{FALSE} (default) the items in \code{dots} with argument
     names coinciding with formal argument names used by \code{distrExIntegrate},
     \code{integrate}, \code{GLIntegrate}, except for \code{\dots}, \code{f},
     \code{distr}, are returned; otherwise the logical complement, i.e., those
     items not coinciding with the mentioned names are returned.}
\item{fun}{ a function the formals of which are to be filtered from argument list
  \code{dots}. }
  \item{liste}{ a list (usually of S3 class \code{"DiagnosticClass"})
       as filled from various functions with diagnostic information; it usually
       has a tree structure and is the to be reorganized; this is done in
       function \code{.reorganizeDiagnosticList} which groups items of the
       same name in several sublists. }
  \item{.depth}{ an argument to be used internally to determine the depth
              of the recursion when moving through the tree. }
  \item{names0}{ a character vector with the names of the items to be regrouped
                 in new sublists. }
  \item{prenames}{ to determine the position in the original tree, the names
     of parent nodes are concatenated, separated by "$" and passed on
     to children nodes by argument \code{prenames}. }
  \item{namestoGather}{ we have two kind of diagnostic list items, ones which
    are easily grouped (numeric, character, logical) and ones which are calls,
    lists or functions. \code{namestoGather} takes the names of items
    to be regrouped which are of the first kind. }
  \item{namestoGatherNS}{ the names of items to be regrouped which are of the
    second kind (NS standing for "non-shown"). }
  \item{.GatherList}{ the list of new regrouped sublists (of first kind).
          This argument is not filled by the user but rather passed on within the
          recursion when moving through the tree. }
  \item{.GatherListNS}{ the list of new regrouped sublists (of second kind).
        This argument is not filled by the user but rather passed on within the
          recursion when moving through the tree. }
  \item{withprint}{ logical; should intermediate information when moving
     through the tree be printed? }
}

\details{
\code{.getIntbounds} integration bounds are obtained
as \code{lowB <- max(low, q.l(object)(lowTQ), median(object)-IQR.fac*IQR(object))}
and \code{uppB <- min(upp, q.l(object)(1-uppTQ), median(object)+IQR.fac*IQR(object))}

\code{.filterEargs} filters out arguments that are used in numerical integration
   and returns only these (or returns all items except for them, if argument
   \code{neg} is \code{TRUE}).

\code{.filterFunargs} filters out arguments the names of which are within the
   names of the formal arguments of \code{fun} (or returns all items of
   \code{dots} except for the matched ones, if argument
   \code{neg} is \code{TRUE}).

\code{.nmsToGather} is a character vector containing the names of items
   which are easily regrouped (of first kind in the distinction in the
   arguments \code{namestoGather} and \code{namestoGatherNS}); the distinction
   in these two categories is based on this vector; those items not in this
   vector are put into the second kind.

\code{.showallNamesDiagnosticList} runs through all the nodes in the list and
   collects all names and then returns the list of all (unique) names found.

\code{.reorganizeDiagnosticList} reorganizes the nodes of the nodes in the list,
  regrouping them into groups with the names.

}


\value{
\item{.getIntbounds}{a named numeric vector with coordinates \code{low} and \code{upp}.}
}

\author{
  Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de},
}

\seealso{
\code{\link[distr]{AbscontDistribution}},
\code{\link{distrExIntegrate}}
}
\keyword{internal}
\concept{utilities}
