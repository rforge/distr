\name{illustrateLLN}
\alias{illustrateLLN}

\title{Functions for Illustrating the LLN}
\description{Functions for generating a sequence of plots of
randomly generated replicates of 
\eqn{\bar X_n= \frac{1}{n} \sum_{i=1}^n X_i}{arithmetic means}
for sums of iid r.v. distributed according to a prescribed discrete or 
absolutely continuous distribution. A line for the expectation and CLT based
(pointwise) 95\%-confidence bands are also plotted and the empirical coverage
of this band by the replicated plotted so far is indicated.}
%
\usage{illustrateLLN(Distr = Norm(),n = c(1,3,5,10,25,50,100,500,1000,10000), 
            m = 50, step = 1, sleep = 0, withConf = TRUE, 
            withCover = (length(n)<=12), withEline = TRUE, withLegend = TRUE,
            CLTorCheb = "CLT",  coverage = 0.95, ...,  col.Eline = "blue", 
            lwd.Eline = par("lwd"), lty.Eline = par("lty"), col.Conf = "red", 
            lwd.Conf = par("lwd"), lty.Conf = 2, cex.Cover = 0.7, 
            cex.legend = 0.8)
}
\arguments{
  \item{Distr}{object of class \code{"UnivariateDistribution"}: 
               distribution of the summands}
  \item{n}{vector of integers: sample sizes to be considered}
  \item{m}{integer: (total) number of replicates to be plotted subsequently}
  \item{step}{integer: number of replicates to be drawn at once}
  \item{sleep}{numeric: pause in seconds between subsequent plots }
  \item{withEline}{logical: shall a line for the limiting expectation 
                   (in case of class \code{Cauchy} instead: median)
                   be drawn?}
  \item{withConf}{logical: shall (CLT-based) confidence bands be plotted?}
  \item{withCover}{logical: shall empirical coverage of (CLT-based) confidence 
                   bands be printed?}
  \item{withLegend}{logical: shall a legend be included?}
  \item{CLTorCheb}{character: type of confidence interval ---"CLT" or
                   "Chebyshev"; partial matching is used; if this fails
                    "CLT" is used.}
  \item{coverage}{numerical: nominal coverage of the confidence bands 
                  ---to be in (0,1)}
  \item{col.Eline}{character or integer code; color for confidence bands}
  \item{lwd.Eline}{integer code (see \code{\link[graphics]{par}}); 
                  line width of the confidence bands}
  \item{lty.Eline}{integer code (see \code{\link[graphics]{par}}); 
                  line type of the confidence bands}
  \item{col.Conf}{character or integer code; color for confidence bands}
  \item{lwd.Conf}{integer code (see \code{\link[graphics]{par}}); 
                  line width of the confidence bands}
  \item{lty.Conf}{integer code (see \code{\link[graphics]{par}}); 
                  line type of the confidence bands}
  \item{cex.Cover}{magnification w.r.t. the current setting of \code{cex} 
                   to be used for empirical coverages; as in 
                   \code{\link[graphics]{par}}}
  \item{cex.legend}{magnification w.r.t. the current setting of \code{cex} 
                   to be used for the legend as in 
                   \code{\link[graphics]{par}}}
  \item{\dots}{further arguments to be passed to \code{matplot},
               \code{matlines}, \code{abline}}
}
%\details{}
\value{void}
\details{\code{illustrateLLN} generates a sequence of plots.
 Any parameters of \code{plot.default} may be passed on to this particular
 \code{plot} method. 

There are default \code{main} titles as well as \code{xlab} and \code{ylab}
annotations.

In all title arguments, the following patterns are substituted:
\describe{
\item{\code{"\%C"}}{class of argument \code{x}}
\item{\code{"\%P"}}{parameters of \code{x} in form of a comma-separated list of
                   <value>'s coerced to character}
\item{\code{"\%Q"}}{parameters of \code{x} in form of a comma-separated list of
                   <value>'s coerced to character and in parenthesis --- unless
                   empty; then ""}
\item{\code{"\%N"}}{parameters of \code{x} in form of a comma-separated list
                   <name> = <value> coerced to character}
\item{\code{"\%A"}}{deparsed argument \code{x}}
\item{\code{"\%D"}}{time/date-string when the plot was generated}
\item{\code{"\%X"}}{the expression
\eqn{\bar X_n=\sum_{i=1}^n X_i/n}{\code{bar(X)[n]==~~sum(X[i],i==1,n)/n}}}
}

If not explicitly set, \code{col.Eline}, \code{col.Conf} are set 
to \code{col} if this arg is given and else to their default values as given
above. Similarly for \code{cex}, \code{lwd} and \code{lty}.
}


\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
\examples{
illustrateLLN(Distr = Unif())
illustrateLLN(Distr = Pois(lambda = 2))
illustrateLLN(Distr = Pois(lambda = 2)+Unif())
illustrateLLN(Td(3), m = 50, col.Eline = "green", lwd = 2, cex = 0.6, main = 
 "My LLN \%C\%Q", sub = "generated \%D")
illustrateLLN(Td(3), m = 50, CLTorCheb = "Chebyshev") 
illustrateLLN(Td(3), m = 50, CLTorCheb = "Chebyshev", coverage = 0.75) 
}
\keyword{distribution}
\keyword{methods}
\concept{illustration}
\concept{teaching}
\concept{LLN}
\keyword{dynamic}
