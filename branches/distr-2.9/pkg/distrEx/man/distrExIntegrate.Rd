\name{distrExIntegrate}
\alias{distrExIntegrate}
\alias{print.DiagnosticClass}
\alias{showDiagnostic}
\alias{getDiagnostic}

\title{Integration of One-Dimensional Functions}
\description{
  Numerical integration via \code{integrate}. In case \code{integrate}
  fails a Gauss-Legendre quadrature is performed.
}
\usage{
distrExIntegrate(f, lower, upper, subdivisions = 100, 
                 rel.tol = .Machine$double.eps^0.25, 
                 abs.tol = rel.tol, stop.on.error = TRUE, 
                 distr, order, ..., diagnostic = FALSE)
showDiagnostic(x, what, withNonShows = FALSE, ...)
getDiagnostic(x, what, reorganized=TRUE)
\method{print}{DiagnosticClass}(x, what, withNonShows = FALSE, xname, ...)
}

\arguments{
  \item{f}{ an R function taking a numeric first argument and returning a
            numeric vector of the same length.  Returning a non-finite
            element will generate an error. }
  \item{lower}{ lower limit of integration. Can be \code{-Inf}. }
  \item{upper}{ upper limit of integration. Can be \code{Inf}. }
  \item{subdivisions}{ the maximum number of subintervals. }
  \item{rel.tol}{ relative accuracy requested. }
  \item{abs.tol}{ absolute accuracy requested. }
  \item{stop.on.error}{ logical. If \code{TRUE} (the default) an error 
    stops the function. If false some errors will give a result with a 
    warning in the \code{message} component. }
  \item{distr}{ object of class \code{UnivariateDistribution}. }
  \item{order}{ order of Gauss-Legendre quadrature. }
  \item{diagnostic}{ logical; if \code{TRUE}, the return value obtains
    an attribute \code{"diagnostic"} with diagnostic information on the
    integration, i.e., a list with entries \code{method} (\code{"integrate"}
    or \code{"GLIntegrate"}), \code{call}, \code{result} (the complete return
    value of the method),  \code{args} (the args with which the
    method was called), and \code{time} (the time to compute the integral). }
  \item{\dots}{ In case of integrators: additional arguments to be passed to \code{f}. Remember
    to use argument names not matching those of \code{integrate}
    and \code{GLIntegrate}! In case of \code{showDiagnostic}, \code{print.DiagnosticClass}:
    additional arguments to be passed on to print methods called for particular
    items in the diagnostic list. }
   \item{x}{ the item for which the diagnostic is to be shown. }
   \item{what}{ a character vector with all the diagnostic items to be
     selected/shown. If empty or missing all items are selected/shown. }
   \item{withNonShows}{ internally we distinguish items which are easily
     printed (first kind) (numeric, logical, character)
     and more difficult ones (second kind), e.g., calls, functions, lists. The
     distinction is made according to the list item name. If
     \code{withNonShows==TRUE} one also attempts to show the selected items
     of the second kind, otherwise they are not shown (but returned). }
   \item{xname}{ an optional name for the diagnostic object to be shown. }
   \item{reorganized}{should the diagnostic information be reorganized
   (using internal function \code{\link{.reorganizeDiagnosticList}}? }
}
\details{
  \code{distrExIntegrate} calls \code{integrate}. In case \code{integrate}
  returns an error a Gauss-Legendre integration is performed using
  \code{GLIntegrate}. If \code{lower} or (and) \code{upper} are infinite
  the \code{GLIntegrateTruncQuantile}, respectively the 
  \code{1-GLIntegrateTruncQuantile} quantile of \code{distr} is used 
  instead.

  \code{distrExIntegrate} is called from many places in the distr and robast
  families of packages. At every such instance, diagnostic information can
  be collected (setting a corresponding argument \code{diagnostic} to \code{TRUE}
  in the calling function. This diagnostic information is originally stored in
  a tree like list structure of S3 class \code{DiagnosticClass} which is then
  attached as attribute \code{diagnostic} to the respective object. It can be
  inspected and accessed through \code{showDiagnostic} and  \code{getDiagnostic}.
  More specifically, for any object with attribute \code{diagnostic},
  \code{showDiagnostic} shows the diagnostic collected during integration, and
  \code{getDiagnostic} returns the diagnostic collected during integration.
  To this end, \code{print.DiagnosticClass} is an S3 method for \code{print}
  for objects of S3 class \code{DiagnosticClass}.
}

\value{

The value of \code{distrExIntegrate} is a numeric approximation of the integral.
If argument \code{diagnostic==TRUE} in \code{distrExIntegrate}, the return
value has an attribute \code{diagnostic} of S3 class \code{DiagnosticClass}
containing diagnostic information on the integration.

\code{showDiagnostic}, \code{getDiagnostic}, \code{print.DiagnosticClass} all
return (invisibly) a list with the selected items, reorganized by internal
function \code{.reorganizeDiagnosticList}, respectively, in case
of argument \code{reorganized==FALSE}, \code{getDiagnostic} returns (invisibly)
the diagnostic information as is.
}
\references{ 
  Based on QUADPACK routines \code{dqags} and \code{dqagi} by
  R. Piessens and E. deDoncker-Kapenga, available from Netlib.

  R. Piessens, E. deDoncker-Kapenga, C. Uberhuber, D. Kahaner (1983)
  \emph{Quadpack: a Subroutine Package for Automatic Integration}.
  Springer Verlag.

  W.H. Press, S.A. Teukolsky, W.T. Vetterling, B.P. Flannery (1992)
  \emph{Numerical Recipies in C}. The Art of Scientific Computing.
  Second Edition. Cambridge University Press.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\seealso{\code{\link[stats]{integrate}}, \code{\link{GLIntegrate}}, 
\code{\link{distrExOptions}}}
\examples{
fkt <- function(x){x*dchisq(x+1, df = 1)}
integrate(fkt, lower = -1, upper = 3)
GLIntegrate(fkt, lower = -1, upper = 3)
try(integrate(fkt, lower = -1, upper = 5))
distrExIntegrate(fkt, lower = -1, upper = 5)
}
\concept{integration}
\keyword{math}
\keyword{utilities}
