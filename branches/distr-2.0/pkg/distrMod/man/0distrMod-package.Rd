\name{distrMod-package}
\alias{distrMod-package}
\alias{distrMod}
\docType{package}

\title{
distrMod -- probability models
}
\description{
Based on the packages \pkg{distr} and \pkg{distrEx} package \pkg{distrMod} 
provides a flexible framework which allows computation of estimators like 
maximum likelihood or minimum distance estimators for probability models.
}

\details{
\tabular{ll}{
Package: \tab distrMod\cr
Version: \tab 2.0 \cr
Date: \tab 2008-05-08 \cr
Depends: \tab R(>= 2.6.0), methods, startupmsg, distr(>= 2.0), distrEx(>= 2.0), RandVar(>= 0.6.2)\cr
LazyLoad: \tab yes\cr
License: \tab GPL (version 2 or later)\cr
URL: \tab http://distr.r-forge.r-project.org/\cr
}}

\section{Classes}{
\preformatted{

##########################
ProbFamily classes
##########################
slots: [<name>(<class>)] 
name(character), distribution(Distribution),
distrSym(DistributionSymmetry), props(character)

"ProbFamily"
|>"ParamFamily"
additional slots:
param(ParamFamParameter), modifyParam(function)
|>|>"L2ParamFamily"
additional slots:
L2deriv(EuclRandVarList), L2deriv.fct(function), 
L2derivSymm(FunSymmList), L2derivDistr(DistrList),
L2derivDistrSymm(DistrSymmList), FisherInfo(PosSemDefSymmMatrix),
FisherInfo.fct(function)
|>|>|>"L2GroupParamFamily"
additional slots:
LogDeriv(function)
|>|>|>|>"L2LocationFamily"
|>|>|>|>"L2ScaleFamily"
|>|>|>|>"L2LocationScaleFamily"

##########################
ParamFamParameter
##########################
"ParamFamParameter" is subclass of class "Parameter" of package "distr".
Additional slots:
main(numeric), nuisance(OptionalNumeric), trafo(MatrixorFunction)

##########################
Class unions
##########################
"OptionalNumeric" = union("numeric", "NULL")
"OptionalMatrix" = union("matrix", "NULL")
"MatrixorFunction" = union("matrix", "OptionalFunction")

##########################
Symmetry classes
##########################
slots:
type(character), SymmCenter(ANY)

"Symmetry"
|>"NoSymmetry"
|>"EllipticalSymmetry"
|>|>"SphericalSymmetry"
|>"DistributionSymmetry"
|>"FunctionSymmetry"
|>|>"NonSymmetric"
|>|>"EvenSymmetric"
|>|>"OddSymmetric"

list thereof
"DistrSymmList"
"FunSymmList"

##########################
Matrix classes
##########################
slots:
none
"PosSemDefSymmMatrix" is subclass of class "matrix" of package "base".
|>"PosDefSymmMatrix"


##########################
Norm Classes
##########################
slots:
name(character), fct(function)

"NormType"
|>"QFNorm"
Additional slots:
QuadForm(PosSemDefSymmMatrix)
|>|>"InfoNorm"
|>|>"SelfNorm"

##########################
Bias Classes
##########################
slots:
name(character)

"BiasType"
|>"symmetricBias"
|>"onesidedBias"
Additional slots:
sign(numeric)
|>"asymmetricBias", 
Additional slots:
nu(numeric)

##########################
Risk Classes
##########################
slots:
type(character)

"RiskType"
|>"asRisk"
|>|>"asCov"
|>|>"trAsCov"
|>"fiRisk"
|>|>"fiCov"
|>|>"trfiCov"
|>|>"fiHampel"
Additional slots:
bound(numeric)
|>|>"fiMSE"
|>|>"fiBias"
|>|>"fiUnOvShoot"
Additional slots:
width(numeric)

Risk with Bias:
"asRiskwithBias"
slots: biastype(BiasType), normtype(NormType),
|>"asHampel"
Additional slots:
bound(numeric)
|>"asBias"
|>"asGRisk"
|>|>"asMSE"
|>|>"asUnOvShoot"
Additional slots:
width(numeric)
|>|>"asSemivar"


##########################
Estimate Classes
##########################
slots:
name(character), estimate(ANY),
samplesize(numeric), asvar(OptionalMatrix),
Infos(matrix), nuis.idx(OptionalNumeric)

"Estimate"
|>"MCEstimate", 
Additional slots:
criterion(numeric)


}
}
\section{Methods}{
not specified yet...
}
\author{
Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr 
Matthias Kohl \email{Matthias.Kohl@stamats.de}\cr

\emph{Maintainer:} Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}
}

\references{
P. Ruckdeschel, M. Kohl, T. Stabla, F. Camphausen (2006):
S4 Classes for Distributions, {\em R News}, {\em 6}(2), 2-6. 
\url{http://CRAN.R-project.org/doc/Rnews/Rnews_2006-2.pdf}

a vignette for packages \pkg{distr}, \pkg{distrSim}, \pkg{distrTEst}, and 
\pkg{distrEx} is included into the mere documentation package \pkg{distrDoc} 
and may be called by 
\code{require("distrDoc");vignette("distr")}
}

\section{Start-up-Banner}{
You may suppress the start-up banner/message completely by setting 
\code{options("StartupBanner"="off")} somewhere before loading this 
package by \code{library} or \code{require} in your R-code / R-session.

If option \code{"StartupBanner"} is not defined (default) or setting    
\code{options("StartupBanner"=NULL)} or  
\code{options("StartupBanner"="complete")} the complete start-up banner is 
displayed.

For any other value of option \code{"StartupBanner"} 
(i.e., not in \code{c(NULL,"off","complete")})
only the version information is displayed.

The same can be achieved by wrapping the \code{library} or \code{require} 
call into either \code{suppressStartupMessages()} or 
\code{onlytypeStartupMessages(.,atypes="version")}. 

As for general \code{packageStartupMessage}'s, you may also suppress all
the start-up banner by wrapping the \code{library} or \code{require} 
call into \code{suppressPackageStartupMessages()} from 
\pkg{startupmsg}-version 0.5 on.
}

\section{Demos}{
none so far ...
}
\keyword{package}
