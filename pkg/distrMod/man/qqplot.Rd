\name{qqplot}
\docType{methods}
\title{Methods for Function qqplot in Package `distrMod'}
\usage{
qqplot(x, y, ...)
\S4method{qqplot}{ANY,UnivariateDistribution}(x,y,
    n = length(x), withIdLine = TRUE,
    withConf = TRUE, withConf.pw  = withConf, withConf.sim = withConf,
    plot.it = TRUE, datax = FALSE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)),
    ..., width = 10, height = 5.5, withSweave = getdistrOption("withSweave"),
    mfColRow = TRUE, n.CI = n, withLab = FALSE, lab.pts = NULL, which.lbs = NULL,
    which.Order = NULL, order.traf = NULL, 
    col.IdL = "red", lty.IdL = 2, lwd.IdL = 2, alpha.CI = .95,
    exact.pCI = (n<100), exact.sCI = (n<100), nosym.pCI = FALSE,
    col.pCI = "orange", lty.pCI = 3, lwd.pCI = 2, pch.pCI = par("pch"),
    cex.pCI = par("cex"),
    col.sCI = "tomato2", lty.sCI = 4, lwd.sCI = 2, pch.sCI = par("pch"),
    cex.sCI = par("cex"), added.points.CI = TRUE,
    cex.pch = par("cex"), col.pch = par("col"),
    cex.lbl = par("cex"), col.lbl = par("col"), adj.lbl = par("adj"),
    alpha.trsp = NA, jit.fac = 0, jit.tol = .Machine$double.eps,
    check.NotInSupport = TRUE, col.NotInSupport = "red",
    with.legend = TRUE, legend.bg = "white",
    legend.pos = "topleft", legend.cex = 0.8, 
    legend.pref = "", legend.postf = "",  legend.alpha = alpha.CI,
    debug = FALSE, withSubst = TRUE)
\S4method{qqplot}{ANY,ProbFamily}(x, y,
   n = length(x), withIdLine = TRUE, withConf = TRUE,
   withConf.pw  = withConf,  withConf.sim = withConf,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...)
\S4method{qqplot}{ANY,Estimate}(x, y,
   n = length(x), withIdLine = TRUE, withConf = TRUE,
   withConf.pw  = withConf,  withConf.sim = withConf,
    plot.it = TRUE, xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), ...)
}
\alias{qqplot}
\alias{qqplot-methods}
\alias{qqplot,ANY,ProbFamily-method}
\alias{qqplot,ANY,Estimate-method}
\alias{qqplot,ANY,UnivariateDistribution-method}

\arguments{
\item{x}{data to be checked for compatibility with distribution/model \code{y}.}
\item{y}{object of class \code{"UnivariateDistribution"} or of
class \code{"ProbFamily"}.}
\item{n}{numeric; assumed sample size (by default length of \code{x}).}
\item{withIdLine}{logical; shall line \code{y = x} be plotted in?}
\item{withConf}{logical; shall confidence lines be plotted?}
\item{withConf.pw}{logical; shall pointwise confidence lines be plotted?}
\item{withConf.sim}{logical; shall simultaneous confidence lines be plotted?}
\item{plot.it}{logical; shall be plotted at all (inherited from
\code{\link[stats:qqnorm]{qqplot}})?}
\item{datax}{logical; shall data be plotted on x-axis?}
\item{xlab}{x-label}
\item{ylab}{y-label}
\item{\dots}{further parameters for method \code{qqplot} with signature
\code{ANY,UnivariateDistribution} or with function \code{plot}}
\item{width}{width (in inches) of the graphics device opened}
\item{height}{height (in inches) of the graphics device opened}
\item{withSweave}{logical: if \code{TRUE} (for working with \command{Sweave})
        no extra device is opened and height/width are not set}
\item{mfColRow}{shall default partition in panels be used --- defaults to \code{TRUE}}
\item{n.CI}{numeric; number of points to be used for confidence interval}
\item{withLab}{logical; shall observation labels be plotted in?}
\item{lab.pts}{character or \code{NULL}; observation labels to be used}
\item{which.lbs}{integer or \code{NULL}; which observations shall be labelled}
\item{which.Order}{integer or \code{NULL}; which of the ordered (remaining)
observations shall be labelled}
\item{order.traf}{function or \code{NULL}; an optional trafo by which the
observations are ordered (as order(trafo(obs)).}
\item{col.IdL}{color for the identity line}
\item{lty.IdL}{line type for the identity line}
\item{lwd.IdL}{line width for the identity line}
\item{alpha.CI}{confidence level}
\item{exact.pCI}{logical; shall pointwise CIs be determined with exact
Binomial distribution?}
\item{exact.sCI}{logical; shall simultaneous CIs be determined with
exact Kolmogorov distribution?}
\item{nosym.pCI}{logical; shall we use (shortest) asymmetric CIs?}
\item{col.pCI}{color for the pointwise CI}
\item{lty.pCI}{line type for the pointwise CI}
\item{lwd.pCI}{line width for the pointwise CI}
\item{pch.pCI}{symbol for points (for discrete mass points) in pointwise CI}
\item{cex.pCI}{magnification factor for points (for discrete mass points) in
pointwise CI}
\item{col.sCI}{color for the simultaneous CI}
\item{lty.sCI}{line type for the simultaneous CI}
\item{lwd.sCI}{line width for the simultaneous CI}
\item{pch.sCI}{symbol for points (for discrete mass points) in simultaneous CI}
\item{cex.sCI}{magnification factor for points (for discrete mass points) in
simultaneous CI}
\item{added.points.CI}{logical; should CIs be plotted through additional points
      (and not only through data points)? }
\item{cex.pch}{magnification factor for the plotted symbols}
\item{col.pch}{color for the plotted symbols}
\item{cex.lbl}{magnification factor for the plotted observation labels}
\item{col.lbl}{color for the plotted observation labels}
\item{adj.lbl}{adj parameter for the plotted observation labels}
\item{alpha.trsp}{alpha transparency to be added ex post to colors
        \code{col.pch} and \code{col.lbl}; if one-dim and NA all colors are
        left unchanged. Otherwise, with usual recycling rules \code{alpha.trsp}
        gets shorted/prolongated to length the data-symbols to be plotted.
        Coordinates of this vector \code{alpha.trsp} with NA are left unchanged,
        while for the remaining ones, the alpha channel in rgb space is set
        to the respective coordinate value of \code{alpha.trsp}. The non-NA
        entries must be integers in [0,255] (0 invisible, 255 opaque).}
\item{jit.fac}{jittering factor used for discrete distributions.}
\item{jit.tol}{threshold for jittering: if distance between points is smaller
               than \code{jit.tol}, points are considered replicates.}
\item{check.NotInSupport}{logical; shall we check if all \code{x}-quantiles lie
in \code{support(y)}?}
\item{col.NotInSupport}{logical; if preceding check \code{TRUE} color of
\code{x}-quantiles if not in \code{support(y)}}
\item{with.legend}{logical; shall a legend be plotted?}
\item{legend.bg}{background color for the legend}
\item{legend.pos}{position for the legend}
\item{legend.cex}{magnification factor for the legend}
\item{legend.pref}{character to be prepended to legend text}
\item{legend.postf}{character to be appended to legend text}
\item{legend.alpha}{nominal coverage probability}
\item{debug}{logical; if \code{TRUE} additional output to debug confidence bounds.}
\item{withSubst}{logical; if \code{TRUE} (default) pattern substitution for
      titles and axis lables is used; otherwise no substitution is used. }
}
\description{
  We generalize function \code{\link[stats:qqnorm]{qqplot}} from package \pkg{stats} to
  be applicable to distribution and probability model objects, as well as
  to estimate objects. In this context,
  \code{qqplot} produces a QQ plot of data (argument \code{x}) against
   a (model) distribution. If the second argument is of class \code{'Estimate'},
   \code{qqplot} looks at the \code{estimate.call}-slot and checks whether
   it can use an argument \code{ParamFamily} to conclude on the model
   distribution. Graphical parameters may be given as arguments to
   \code{qqplot}.
  In all title and label arguments, if \code{withSubst} is \code{TRUE},
  the following patterns are substituted:
\describe{
\item{\code{"\%C"}}{class of argument \code{x}}
\item{\code{"\%A"}}{deparsed argument \code{x}}
\item{\code{"\%D"}}{time/date-string when the plot was generated}
}
}
\value{
  A list of elements containing the information needed to compute the 
  respective QQ plot, in particular it extends the elements of the 
  return value of function \code{\link[stats:qqnorm]{qqplot}} 
  from package \pkg{stats}, i.e., a
  list with components \code{x} and \code{y} for x and y coordinates
  of the plotted points; this list is of S3 class 
  \code{c("qqplotInfo","DiagnInfo")}, and more specifically it contains
\item{x}{The x coordinates of the points that were/would be plotted}
  \item{y}{The corresponding quantiles of the second distribution,
           \emph{including \code{\link{NA}}s}.}
           
 \item{xy.0}{xy} 
 \item{y.0}{y}
 \item{datax}{argument \code{datax} from the call.} 
 \item{withConf.pw}{argument \code{withConf.pw} from the call.} 
 \item{withConf.sim}{argument \code{withConf.sim} from the call.} 
 \item{alpha.CI}{argument \code{alpha.CI } from the call.}
 \item{col.pCI}{argument \code{col.pCI} from the call.} 
 \item{lty.pCI}{argument \code{lty.pCI} from the call.} 
 \item{lwd.pCI}{argument \code{lwd.pCI} from the call.} 
 \item{pch.pCI}{argument \code{pch.pCI} from the call.} 
 \item{cex.pCI}{argument \code{cex.pCI} from the call.} 
 \item{col.sCI}{argument \code{col.sCI} from the call.} 
 \item{lty.sCI}{argument \code{lty.sCI} from the call.} 
 \item{lwd.sCI}{argument \code{lwd.sCI} from the call.} 
 \item{pch.sCI}{argument \code{pch.sCI} from the call.} 
 \item{cex.sCI}{argument \code{cex.sCI} from the call.} 
 \item{n}{argument \code{n} from the call.} 
 \item{exact.sCI}{argument \code{exact.sCI} from the call.}
 \item{exact.pCI}{argument \code{exact.pCI} from the call.}
 \item{nosym.pCI}{argument \code{nosym.pCI} from the call.}
 \item{with.legend}{argument \code{with.legend} from the call.}
 \item{legend.bg}{argument \code{legend.bg} from the call.}
 \item{legend.pos}{argument \code{legend.pos} from the call.}
 \item{legend.cex}{argument \code{legend.cex} from the call.} 
 \item{legend.pref}{argument \code{legend.pref} from the call.}
 \item{legend.postf}{argument \code{legend.postf} from the call.} 
 \item{legend.alpha}{argument \code{legend.alpha} from the call.} 
 \item{debug}{argument \code{debug} from the call.}
 \item{args.stats.qqplot}{the arguments of the call to \code{mcl} from the call.}
 \item{withLab}{argument \code{withLab} from the call to
 \code{stats::qqplot} from within this QQ plot method.}
 \item{lbprep}{the return value of the label preparation from within
               this function, i.e., a list with elements 
      \code{x0}, \code{y0}, \code{lab}, \code{col}, \code{cex}, 
      \code{adj}.}

 \item{crit}{A matrix with the lower and upper confidence bounds
               (computed by \code{qqbounds}).}
  \item{err}{logical vector of length 2.}
  (elements \code{crit} and \code{err} are taken from the return
   value(s) of \code{qqbounds}).
}
\details{
\describe{
\item{qqplot}{\code{signature(x = "ANY", y = "UnivariateDistribution")}:
produces a QQ plot of a dataset \code{x} against the theoretical
quantiles of distribution \code{y}.}
\item{qqplot}{\code{signature(x = "ANY", y = "ProbFamily")}:
produces a QQ plot of a dataset \code{x} against the theoretical
quantiles of the model distribution of model \code{y}. Passed through
the \code{\dots} argument, all arguments valid for
\code{signature(x = "ANY", y = "UnivariateDistribution")}
are also valid for this signature.}
\item{qqplot}{\code{signature(x = "ANY", y = "Estimate")}:
produces a QQ plot of a dataset \code{x} against the theoretical
quantiles of the model distribution of the model that can be reconstructed
from the estimator \code{y}; more specifically, it tries to get hand at the
argument \code{'ParamFamily'} of the esimator's call; if this is available,
internally this model is shifted to the estimated parameter by a call to
\code{modifyModel}, and then this shifted model is used in a call to the
\code{(x = "ANY", y = "UnivariateDistribution")}-method. Passed through
the \code{\dots} argument, all arguments valid for
\code{signature(x = "ANY", y = "UnivariateDistribution")}
are also valid for this signature.}
}
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\author{
  Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}
}
\seealso{
  \code{\link[stats:qqnorm]{qqplot}} from package \pkg{stats} -- the standard QQ plot
  function,  \code{\link[distr]{qqplot}} from package \pkg{distr} for
  comparisons of distributions, and
  \code{\link{qqbounds}}, used by \code{qqplot} to produce confidence
   intervals.
}
\examples{
set.seed(123)
x <- rnorm(40,mean=15,sd=30)
qqplot(x, Chisq(df=15))
NF <- NormLocationScaleFamily(mean=15, sd=30)
qqplot(x, NF)
mlE <- MLEstimator(x, NF)
qqplot(x, mlE)
}
\keyword{hplot}
\keyword{distribution}
