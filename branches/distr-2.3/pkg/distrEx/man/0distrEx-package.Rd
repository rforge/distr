\name{distrEx-package}
\alias{distrEx-package}
\alias{distrEx}
\docType{package}
\title{
distrEx -- Extensions of package distr
}
\description{
\pkg{distrEx} proides some extensions of package \pkg{distr}:
\itemize{\item  extreme value distribution classes,
         \item expectations in the form 
         \itemize{\item \code{E(X)} for the expectation of a 
                        distribution object \code{X} 
                  \item \code{E(X,f)} for the expectation of \code{f(X)} 
                         where \code{X} is some distribution object and 
                         \code{f} some function in \code{X} }
\item further functionals: var, sd, IQR, mad, median, skewness, kurtosis
\item truncated moments,
\item distances between distributions
     (Hellinger, Cramer von Mises, Kolmogorov, total variation, "convex contamination")
\item lists of distributions,
\item conditional distributions in factorized form 
\item conditional expectations in factorized form
}}

\details{
\tabular{ll}{
Package: \tab distrEx \cr
Version: \tab 2.3 \cr
Date: \tab 2009-11-01 \cr
Depends: \tab R(>= 2.6.0), methods, distr(>= 2.2), evd, actuar, startupmsg \cr
LazyLoad: \tab yes \cr
License: \tab LGPL-3 \cr
URL: \tab http://distr.r-forge.r-project.org/\cr
SVNRevision: \tab 684  \cr
}
}
\section{Classes}{
\preformatted{

Distribution Classes

"Distribution" (from distr)
|>"UnivariateDistribution" (from distr)
|>|>"AbscontDistribution" (from distr)
|>|>|>"Gumbel"
|>|>|>"Pareto"
|>|>|>"GPareto"
|>"MultivariateDistribution"
|>|>"DiscreteMVDistribution-class"
|>"UnivariateCondDistribution"
|>|>"AbscontCondDistribution"
|>|>|>"PrognCondDistribution"
|>|>"DiscreteCondDistribution"


Condition Classes

"Condition"
|>"EuclCondition"
|>"PrognCondition"

Parameter Classes

"OptionalParameter" (from distr)
|>"Parameter" (from distr)
|>|>"LMParameter"
|>|>"GumbelParameter"
|>|>"ParetoParameter"
}
}
\section{Functions}{

\preformatted{

Integration:
GLIntegrate             Gauss-Legendre quadrature
distrExIntegrate        Integration of one-dimensional functions

Options:
distrExOptions          Function to change the global variables of the
                        package 'distrEx'
Standardization:
make01                  Centering and standardization of univariate
                        distributions

}}

\section{Generating Functions}{
\preformatted{

Distribution Classes
ConvexContamination     Generic function for generating convex
                        contaminations
DiscreteMVDistribution
                        Generating function for
                        DiscreteMVDistribution-class
Gumbel                  Generating function for Gumbel-class
LMCondDistribution      Generating function for the conditional
                        distribution of a linear regression model.

Condition Classes
EuclCondition           Generating function for EuclCondition-class

Parameter Classes
LMParameter             Generating function for LMParameter-class

}}
\section{Methods}{
\preformatted{
Distances:
ContaminationSize       Generic function for the computation of the
                        convex contamination (Pseudo-)distance of two
                        distributions
HellingerDist           Generic function for the computation of the
                        Hellinger distance of two distributions
KolmogorovDist          Generic function for the computation of the
                        Kolmogorov distance of two distributions
TotalVarDist            Generic function for the computation of the
                        total variation distance of two distributions
AsymTotalVarDist        Generic function for the computation of the
                        asymmetric total variation distance of two distributions
                        (for given ratio rho of negative to positive part of deviation)
OAsymTotalVarDist       Generic function for the computation of the minimal (in rho)
                        asymmetric total variation distance of two distributions
vonMisesDist            Generic function for the computation of the
                        von Mises distance of two distributions

liesInSupport           Generic function for testing the support of a
                        distribution


Functionals:
E                       Generic function for the computation of
                        (conditional) expectations
var                     Generic functions for the computation of
                        functionals
IQR                     Generic functions for the computation of
                        functionals
sd                      Generic functions for the computation of
                        functionals
mad                     Generic functions for the computation of
                        functionals
median                  Generic functions for the computation of
                        functionals
skewness                Generic functions for the computation of
                        functionals
kurtosis                Generic functions for the computation of
                        Functionals


truncated Moments:
m1df                    Generic function for the computation of clipped
                        first moments
m2df                    Generic function for the computation of clipped
                        second moments

}
}

\section{Demos}{
Demos are available --- see \code{demo(package="distrEx")}.}

\section{Acknowledgement}{
G. Jay Kerns, \email{gkerns@ysu.edu}, has provided a major contribution,
in particular the functionals \code{skewness} and \code{kurtosis} are due to him.
Natalyia Horbenko, \email{natalyia.horbenko@itwm.fraunhofer.de} has ported
the \pkg{actuar} code for the Pareto distribution to this setup. 
}

\note{
Some functions of package \pkg{stats} have intentionally been masked, but 
completely retain their functionality --- see \code{distrExMASK()}.

If any of the packages \pkg{e1071}, \pkg{moments}, \pkg{fBasics} is to be used 
together with \pkg{distrEx} the latter must be attached \emph{after} any of the 
first mentioned. Otherwise \code{kurtosis()} and \code{skewness()}
defined as \emph{methods} in \pkg{distrEx} may get masked.\cr  To re-mask, you 
may use  \code{kurtosis <- distrEx::kurtosis; skewness <- distrEx::skewness}. 
See also \code{distrExMASK()}
}

\section{Start-up-Banner}{
You may suppress the start-up banner/message completely by setting 
\code{options("StartupBanner"="off")} somewhere before loading this package by 
\code{library} or \code{require} in your R-code / R-session.

If option \code{"StartupBanner"} is not defined (default) or setting    
\code{options("StartupBanner"=NULL)} or 
\code{options("StartupBanner"="complete")} the complete start-up banner is 
displayed.

For any other value of option \code{"StartupBanner"} (i.e., not in 
\code{c(NULL,"off","complete")}) only the version information is displayed.

The same can be achieved by wrapping the \code{library} or \code{require}  call 
into either \code{suppressStartupMessages()} or 
\code{onlytypeStartupMessages(.,atypes="version")}.  
 
As for general \code{packageStartupMessage}'s, you may also suppress all
 the start-up banner by wrapping the \code{library} or \code{require} 
 call into \code{suppressPackageStartupMessages()} from 
 \pkg{startupmsg}-version 0.5 on.
 }

\section{Package versions}{
Note: The first two numbers of package versions do not necessarily reflect
 package-individual development, but rather are chosen for the
 distrXXX family as a whole in order to ease updating "depends"
 information.
}

\author{
Matthias Kohl \email{Matthias.Kohl@stamats.de} and \cr
Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr 

\emph{Maintainer:}  Matthias Kohl \email{Matthias.Kohl@stamats.de}
}
\references{
P. Ruckdeschel, M. Kohl, T. Stabla, F. Camphausen (2006):
S4 Classes for Distributions, \emph{R News}, \emph{6}(2), 2-6. 
\url{http://CRAN.R-project.org/doc/Rnews/Rnews_2006-2.pdf}

%a more detailed manual for \pkg{distr}, \pkg{distrSim}, \pkg{distrTEst}, and \pkg{distrEx} may be downloaded from  
%\url{http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf}\cr

a vignette for packages \pkg{distr}, \pkg{distrSim}, \pkg{distrTEst}, 
and \pkg{distrEx} is included into the mere documentation package \pkg{distrDoc} 
and may be called by \code{require("distrDoc");vignette("distr")}

a homepage to this package is available under\cr
\url{http://distr.r-forge.r-project.org/}

M. Kohl (2005): \emph{Numerical Contributions to the Asymptotic 
Theory of Robustness.} PhD Thesis. Bayreuth. Available as 
\url{http://www.stamats.de/ThesisMKohl.pdf}
}
\keyword{package}
\concept{S4 condition class}
\concept{S4 distribution class}
\concept{functional}
\concept{kurtosis}
\concept{median}
\concept{skewness}
\concept{mad}
\concept{IQR}
\concept{var}
\concept{E}
\concept{distribution distance}
\concept{multivariate distribution}
\concept{conditional distribution}
\seealso{
\code{\link[distr:0distr-package]{distr-package}} 
}
