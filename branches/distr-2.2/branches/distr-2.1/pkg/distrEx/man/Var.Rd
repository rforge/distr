\name{var}
\alias{var}
\alias{var-methods}
\alias{var,ANY-method}
\alias{var,UnivariateDistribution-method}
\alias{var,AffLinDistribution-method}
\alias{var,AffLinAbscontDistribution-method}
\alias{var,AffLinDiscreteDistribution-method}
\alias{var,AffLinLatticeDistribution-method}
\alias{var,CompoundDistribution-method}
\alias{var,Beta-method}
\alias{var,Binom-method}
\alias{var,Cauchy-method}
\alias{var,Chisq-method}
\alias{var,Dirac-method}
\alias{var,DExp-method}
\alias{var,Exp-method}
\alias{var,Fd-method}
\alias{var,Gammad-method}
\alias{var,Geom-method}
\alias{var,Hyper-method}
\alias{var,Logis-method}
\alias{var,Lnorm-method}
\alias{var,Nbinom-method}
\alias{var,Norm-method}
\alias{var,Pois-method}
\alias{var,Unif-method}
\alias{var,Weibull-method}
\alias{var,Td-method}
\alias{var,Arcsine-method}
\alias{var,Pareto-method}
\alias{sd}
\alias{sd-methods}
\alias{sd,UnivariateDistribution-method}
\alias{sd,Norm-method}
\alias{median}
\alias{median,ANY-method}
\alias{median-methods}
\alias{median,UnivariateDistribution-method}
\alias{median,UnivariateCondDistribution-method}
\alias{median,AffLinDistribution-method}
\alias{median,AffLinAbscontDistribution-method}
\alias{median,AffLinDiscreteDistribution-method}
\alias{median,AffLinLatticeDistribution-method}
\alias{median,Cauchy-method}
\alias{median,Dirac-method}
\alias{median,DExp-method}
\alias{median,Exp-method}
\alias{median,Geom-method}
\alias{median,Logis-method}
\alias{median,Lnorm-method}
\alias{median,Norm-method}
\alias{median,Unif-method}
\alias{median,Arcsine-method}
\alias{median,Pareto-method}
\alias{IQR}
\alias{IQR-methods}
\alias{IQR,ANY-method}
\alias{IQR,UnivariateDistribution-method}
\alias{IQR,UnivariateCondDistribution-method}
\alias{IQR,AffLinDistribution-method}
\alias{IQR,AffLinAbscontDistribution-method}
\alias{IQR,AffLinDiscreteDistribution-method}
\alias{IQR,AffLinLatticeDistribution-method}
\alias{IQR,DiscreteDistribution-method}
\alias{IQR,Cauchy-method}
\alias{IQR,Dirac-method}
\alias{IQR,DExp-method}
\alias{IQR,Exp-method}
\alias{IQR,Geom-method}
\alias{IQR,Logis-method}
\alias{IQR,Norm-method}
\alias{IQR,Unif-method}
\alias{IQR,Arcsine-method}
\alias{IQR,Pareto-method}
\alias{mad}
\alias{mad,ANY-method}
\alias{mad-methods}
\alias{mad,UnivariateDistribution-method}
\alias{mad,AffLinDistribution-method}
\alias{mad,AffLinAbscontDistribution-method}
\alias{mad,AffLinDiscreteDistribution-method}
\alias{mad,AffLinLatticeDistribution-method}
\alias{mad,Cauchy-method}
\alias{mad,Dirac-method}
\alias{mad,DExp-method}
\alias{mad,Exp-method}
\alias{mad,Geom-method}
\alias{mad,Logis-method}
\alias{mad,Norm-method}
\alias{mad,Unif-method}
\alias{mad,Arcsine-method}
\alias{skewness}
\alias{skewness-methods}
\alias{skewness,ANY-method}
\alias{skewness,UnivariateDistribution-method}
\alias{skewness,AffLinDistribution-method}
\alias{skewness,AffLinAbscontDistribution-method}
\alias{skewness,AffLinDiscreteDistribution-method}
\alias{skewness,AffLinLatticeDistribution-method}
\alias{skewness,Beta-method}
\alias{skewness,Binom-method}
\alias{skewness,Cauchy-method}
\alias{skewness,Chisq-method}
\alias{skewness,Dirac-method}
\alias{skewness,DExp-method}
\alias{skewness,Exp-method}
\alias{skewness,Fd-method}
\alias{skewness,Gammad-method}
\alias{skewness,Geom-method}
\alias{skewness,Hyper-method}
\alias{skewness,Logis-method}
\alias{skewness,Lnorm-method}
\alias{skewness,Nbinom-method}
\alias{skewness,Norm-method} 
\alias{skewness,Pois-method}
\alias{skewness,Unif-method}
\alias{skewness,Weibull-method}
\alias{skewness,Td-method}
\alias{skewness,Arcsine-method}
\alias{skewness,Pareto-method}
\alias{kurtosis}
\alias{kurtosis-methods}
\alias{kurtosis,ANY-method}
\alias{kurtosis,UnivariateDistribution-method}
\alias{kurtosis,AffLinDistribution-method}
\alias{kurtosis,AffLinAbscontDistribution-method}
\alias{kurtosis,AffLinDiscreteDistribution-method}
\alias{kurtosis,AffLinLatticeDistribution-method}
\alias{kurtosis,Beta-method}
\alias{kurtosis,Binom-method}
\alias{kurtosis,Cauchy-method}
\alias{kurtosis,Chisq-method}
\alias{kurtosis,Dirac-method}
\alias{kurtosis,DExp-method}
\alias{kurtosis,Exp-method}
\alias{kurtosis,Fd-method}
\alias{kurtosis,Gammad-method}
\alias{kurtosis,Geom-method}
\alias{kurtosis,Hyper-method}
\alias{kurtosis,Logis-method}
\alias{kurtosis,Lnorm-method}
\alias{kurtosis,Nbinom-method}
\alias{kurtosis,Norm-method}
\alias{kurtosis,Pois-method}
\alias{kurtosis,Unif-method}
\alias{kurtosis,Weibull-method}
\alias{kurtosis,Td-method}
\alias{kurtosis,Arcsine-method}
\alias{kurtosis,Pareto-method}

\title{Generic Functions for the Computation of Functionals}
\description{
  Generic functions for the computation of functionals on distributions.
}
\usage{
IQR(x, ...)

\S4method{IQR}{UnivariateDistribution}(x)
\S4method{IQR}{UnivariateCondDistribution}(x,cond)
\S4method{IQR}{AffLinDistribution}(x)
\S4method{IQR}{DiscreteDistribution}(x)
\S4method{IQR}{Cauchy}(x)
\S4method{IQR}{Dirac}(x)
\S4method{IQR}{DExp}(x)
\S4method{IQR}{Exp}(x)
\S4method{IQR}{Geom}(x)
\S4method{IQR}{Logis}(x)
\S4method{IQR}{Norm}(x)
\S4method{IQR}{Unif}(x)
\S4method{IQR}{Arcsine}(x)
\S4method{IQR}{Pareto}(x)

median(x, ...)

\S4method{median}{UnivariateDistribution}(x)
\S4method{median}{UnivariateCondDistribution}(x,cond)
\S4method{median}{AffLinDistribution}(x)
\S4method{median}{Cauchy}(x)
\S4method{median}{Dirac}(x)
\S4method{median}{DExp}(x)
\S4method{median}{Exp}(x)
\S4method{median}{Geom}(x)
\S4method{median}{Logis}(x)
\S4method{median}{Lnorm}(x)
\S4method{median}{Norm}(x)
\S4method{median}{Unif}(x)
\S4method{median}{Arcsine}(x)
\S4method{median}{Pareto}(x)

mad(x, ...)

\S4method{mad}{UnivariateDistribution}(x)
\S4method{mad}{AffLinDistribution}(x)
\S4method{mad}{Cauchy}(x)
\S4method{mad}{Dirac}(x)
\S4method{mad}{DExp}(x)
\S4method{mad}{Exp}(x)
\S4method{mad}{Geom}(x)
\S4method{mad}{Logis}(x)
\S4method{mad}{Norm}(x)
\S4method{mad}{Unif}(x)
\S4method{mad}{Arcsine}(x)

sd(x, ...)

\S4method{sd}{UnivariateDistribution}(x, fun, cond, withCond, useApply, ...)
\S4method{sd}{Norm}(x, fun, cond, withCond = FALSE, useApply = TRUE, ...)

var(x, ...)

\S4method{var}{UnivariateDistribution}(x, fun, cond, withCond, useApply, ...)
\S4method{var}{AffLinDistribution}(x, fun, cond, withCond, useApply, ...)
\S4method{var}{CompoundDistribution}(x, ...)
\S4method{var}{Binom}(x, ...)
\S4method{var}{Beta}(x, ...)
\S4method{var}{Cauchy}(x, ...)
\S4method{var}{Chisq}(x, ...)
\S4method{var}{Dirac}(x, ...)
\S4method{var}{DExp}(x, ...)
\S4method{var}{Exp}(x, ...)
\S4method{var}{Fd}(x, ...)
\S4method{var}{Gammad}(x, ...)
\S4method{var}{Geom}(x, ...)
\S4method{var}{Hyper}(x, ...)
\S4method{var}{Logis}(x, ...)
\S4method{var}{Lnorm}(x, ...)
\S4method{var}{Nbinom}(x, ...)
\S4method{var}{Norm}(x, ...)
\S4method{var}{Pois}(x, ...)
\S4method{var}{Td}(x, ...)
\S4method{var}{Unif}(x, ...)
\S4method{var}{Weibull}(x, ...)
\S4method{var}{Arcsine}(x, ...)
\S4method{var}{Pareto}(x, ...)

skewness(x, ...)
\S4method{skewness}{UnivariateDistribution}(x, fun, cond, withCond, useApply, ...)
\S4method{skewness}{AffLinDistribution}(x, fun, cond, withCond, useApply, ...)
\S4method{skewness}{Binom}(x, ...)
\S4method{skewness}{Beta}(x, ...)
\S4method{skewness}{Cauchy}(x, ...)
\S4method{skewness}{Chisq}(x, ...)
\S4method{skewness}{Dirac}(x, ...)
\S4method{skewness}{DExp}(x, ...)
\S4method{skewness}{Exp}(x, ...)
\S4method{skewness}{Fd}(x, ...)
\S4method{skewness}{Gammad}(x, ...)
\S4method{skewness}{Geom}(x, ...)
\S4method{skewness}{Hyper}(x, ...)
\S4method{skewness}{Logis}(x, ...)
\S4method{skewness}{Lnorm}(x, ...)
\S4method{skewness}{Nbinom}(x, ...)
\S4method{skewness}{Norm}(x, ...)
\S4method{skewness}{Pois}(x, ...)
\S4method{skewness}{Td}(x, ...)
\S4method{skewness}{Unif}(x, ...)
\S4method{skewness}{Weibull}(x, ...)
\S4method{skewness}{Arcsine}(x, ...)
\S4method{skewness}{Pareto}(x, ...)

kurtosis(x, ...)
\S4method{kurtosis}{UnivariateDistribution}(x, fun, cond, withCond, useApply, ...)
\S4method{kurtosis}{AffLinDistribution}(x, fun, cond, withCond, useApply, ...)
\S4method{kurtosis}{Binom}(x, ...)
\S4method{kurtosis}{Beta}(x, ...)
\S4method{kurtosis}{Cauchy}(x, ...)
\S4method{kurtosis}{Chisq}(x, ...)
\S4method{kurtosis}{Dirac}(x, ...)
\S4method{kurtosis}{DExp}(x, ...)
\S4method{kurtosis}{Exp}(x, ...)
\S4method{kurtosis}{Fd}(x, ...)
\S4method{kurtosis}{Gammad}(x, ...)
\S4method{kurtosis}{Geom}(x, ...)
\S4method{kurtosis}{Hyper}(x, ...)
\S4method{kurtosis}{Logis}(x, ...)
\S4method{kurtosis}{Lnorm}(x, ...)
\S4method{kurtosis}{Nbinom}(x, ...)
\S4method{kurtosis}{Norm}(x, ...)
\S4method{kurtosis}{Pois}(x, ...)
\S4method{kurtosis}{Td}(x, ...)
\S4method{kurtosis}{Unif}(x, ...)
\S4method{kurtosis}{Weibull}(x, ...)
\S4method{kurtosis}{Arcsine}(x, ...)
\S4method{kurtosis}{Pareto}(x, ...)
}
\arguments{
  \item{x}{ object of class \code{"UnivariateDistribution"}}
  \item{fun}{ if missing the (conditional) variance resp. standard deviation is computed
    else the (conditional) variance resp. standard deviation of \code{fun} is computed. }
  \item{cond}{ if not missing the conditional variance resp. standard deviation  
    given \code{cond} is computed. }
  \item{\dots}{ additional arguments to \code{fun} or \code{E}}
  \item{useApply}{ logical: should \code{sapply}, respectively \code{apply} 
    be used to evaluate \code{fund}.}
  \item{withCond}{ logical: is \code{cond} in the argument list of \code{fun}. }
  }
\value{
  The value of the corresponding functional at the distribution in the argument is computed.
}
\section{Methods}{
\describe{
  \item{\code{var}, \code{signature(x = "Any")}:}{
    interface to the \pkg{stats}-function \code{var} --- see \code{\link[stats]{var}}
    resp. \code{help(var,package="stats")}.}
  \item{\code{var}, \code{signature(x = "UnivariateDistribution")}:}{ 
    variance of univariate distributions using corresponding \code{E()}-method.}
  \item{\code{var}, \code{signature(x = "AffLinDistribution")}:}{ 
    if arguments \code{fun}, \code{cond} are missing: \code{x@a^2 * var(x@X0)}
    else uses method for \code{signature(x = "UnivariateDistribution")}}
  \item{\code{var}, \code{signature(x = "CompoundDistribution")}:}{ 
        if we are in i.i.d. situation (i.e., slot \code{SummandsDistr} is of
        class \code{UnivariateDistribution}) the formula 
        \eqn{{\rm E}[N]{\rm var}[S]+({\rm E}[S]^2+{\rm var}(S)){\rm var}(N)}{E[N]var[S]+E[S*S]var(N)} 
        for \eqn{N} the frequency distribution and
        \eqn{S} the summand distribution; else we coerce to 
        \code{"UnivarLebDecDistribution"}.
    }

 \item{\code{sd}, \code{signature(x = "Any")}:}{
    interface to the \pkg{stats}-function \code{sd} --- see \code{\link[stats]{sd}}
    resp. \code{help(sd,package="stats")}.}
  \item{\code{sd}, \code{signature(x = "NormParameter")}:}{
    returns the slot \code{sd} of the parameter of a normal distribution --- see 
    \code{\link[distr]{sd}} resp. \code{help(sd,package="distr")}.}    
  \item{\code{sd}, \code{signature(x = "Norm")}:}{
    returns the slot \code{sd} of the parameter of a normal distribution --- see 
    \code{\link[distr]{sd}} resp. \code{help(sd,package="distr")}.}    
  \item{\code{sd}, \code{signature(x = "UnivariateDistribution")}:}{ 
    standard deviation of univariate distributions using corresponding \code{E()}-method.}

  \item{\code{IQR}, \code{signature(x = "Any")}:}{
    interface to the \pkg{stats}-function \code{IQR} --- see \code{\link[stats]{IQR}}
    resp. \code{help(IQR,package="stats")}.}
  \item{\code{IQR}, \code{signature(x = "UnivariateDistribution")}:}{ 
    interquartile range of univariate distributions using corresponding \code{q()}-method.}
  \item{\code{IQR}, \code{signature(x = "UnivariateCondDistribution")}:}{ 
    interquartile range of univariate conditional distributions using corresponding \code{q()}-method. }
  \item{\code{IQR}, \code{signature(x = "DiscreteDistribution")}:}{ 
    interquartile range of discrete distributions using corresponding 
    \code{q()}-method but taking care that between upper and lower quartile 
    there is 50\% probability }
  \item{\code{IQR}, \code{signature(x = "AffLinDistribution")}:}{\code{abs(x@a) * IQR(x@X0)}}

  \item{\code{median}, \code{signature(x = "Any")}:}{
    interface to the \pkg{stats}-function \code{median} --- see \code{\link[stats]{median}}
    resp. \code{help(var,package="stats")}.}
  \item{\code{median}, \code{signature(x = "UnivariateDistribution")}:}{ 
    median of univariate distributions using corresponding \code{q()}-method. }
  \item{\code{median}, \code{signature(x = "UnivariateCondDistribution")}:}{ 
    median of univariate conditional distributions using corresponding \code{q()}-method. }
  \item{\code{median}, \code{signature(x = "AffLinDistribution")}:}{\code{x@a * median(x@X0) + x@b }}

   \item{\code{mad}, \code{signature(x = "Any")}:}{
    interface to the \pkg{stats}-function \code{mad} --- see \code{\link[stats]{mad}}.}
   \item{\code{mad}, \code{signature(x = "UnivariateDistribution")}:}{ 
    mad of univariate distributions using corresponding \code{q()}-method applied to \code{abs(x-median(x))}.}
  \item{\code{mad}, \code{signature(x = "AffLinDistribution")}:}{\code{abs(x@a) * mad(x@X0)}}

  \item{\code{skewness}, \code{signature(x = "Any")}:}{
    bias free estimation of skewness under normal distribution (default) as
    well as sample version (by argument \code{sample.version = TRUE}).}
  \item{\code{skewness}, \code{signature(x = "UnivariateDistribution")}:}{ 
    skewness of univariate distributions using corresponding \code{E()}-method.}
  \item{\code{skewness}, \code{signature(x = "AffLinDistribution")}:}{ 
    if arguments \code{fun}, \code{cond} are missing: \code{skewness(x@X0)}
    else uses method for \code{signature(x = "UnivariateDistribution")}}

  \item{\code{kurtosis}, \code{signature(x = "Any")}:}{
    bias free estimation of kurtosis under normal distribution (default) as
    well as sample version (by argument \code{sample.version = TRUE}).}
  \item{\code{kurtosis}, \code{signature(x = "UnivariateDistribution")}:}{ 
    kurtosis of univariate distributions using corresponding \code{E()}-method.}
  \item{\code{kurtosis}, \code{signature(x = "AffLinDistribution")}:}{ 
    if arguments \code{fun}, \code{cond} are missing: \code{kurtosis(x@X0)}
    else uses method for \code{signature(x = "UnivariateDistribution")}}

\item{\code{var}, \code{signature(x = "Beta")}:}{ 
    for noncentrality 0 exact evaluation using explicit expressions.}  
  \item{\code{var}, \code{signature(x = "Binom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Cauchy")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Chisq")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Dirac")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "DExp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Exp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Fd")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Gammad")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Geom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Hyper")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Logis")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Lnorm")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Nbinom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Norm")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Pois")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Td")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Unif")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Weibull")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Arcsine")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Pareto")}:}{ 
    exact evaluation using explicit expressions.}

  \item{\code{IQR}, \code{signature(x = "Cauchy")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{IQR}, \code{signature(x = "Dirac")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{IQR}, \code{signature(x = "DExp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{IQR}, \code{signature(x = "Exp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{IQR}, \code{signature(x = "Geom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{IQR}, \code{signature(x = "Logis")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{IQR}, \code{signature(x = "Norm")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{IQR}, \code{signature(x = "Unif")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{IQR}, \code{signature(x = "Arcsine")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{IQR}, \code{signature(x = "Pareto")}:}{ 
    exact evaluation using explicit expressions.}

  \item{\code{median}, \code{signature(x = "Cauchy")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{median}, \code{signature(x = "Dirac")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{median}, \code{signature(x = "DExp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{median}, \code{signature(x = "Exp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{median}, \code{signature(x = "Geom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{median}, \code{signature(x = "Logis")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{median}, \code{signature(x = "Lnorm")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{median}, \code{signature(x = "Norm")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{median}, \code{signature(x = "Unif")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{median}, \code{signature(x = "Arcsine")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{median}, \code{signature(x = "Pareto")}:}{ 
    exact evaluation using explicit expressions.}

  \item{\code{mad}, \code{signature(x = "Cauchy")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{mad}, \code{signature(x = "Dirac")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{mad}, \code{signature(x = "DExp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{mad}, \code{signature(x = "Exp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{mad}, \code{signature(x = "Geom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{mad}, \code{signature(x = "Logis")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{mad}, \code{signature(x = "Norm")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{mad}, \code{signature(x = "Unif")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{mad}, \code{signature(x = "Arcsine")}:}{ 
    exact evaluation using explicit expressions.}

  \item{\code{skewness}, \code{signature(x = "Beta")}:}{ 
    for noncentrality 0 exact evaluation using explicit expressions.}  
  \item{\code{skewness}, \code{signature(x = "Binom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Cauchy")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Chisq")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Dirac")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "DExp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Exp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Fd")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Gammad")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Geom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Hyper")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Logis")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Lnorm")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Nbinom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Norm")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Pois")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Td")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Unif")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Weibull")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Arcsine")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{skewness}, \code{signature(x = "Pareto")}:}{ 
    exact evaluation using explicit expressions.}
  
  \item{\code{kurtosis}, \code{signature(x = "Beta")}:}{ 
    for noncentrality 0 exact evaluation using explicit expressions.}  
  \item{\code{kurtosis}, \code{signature(x = "Binom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Cauchy")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Chisq")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Dirac")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "DExp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Exp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Fd")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Gammad")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Geom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Hyper")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Logis")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Lnorm")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Nbinom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Norm")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Pois")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Td")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Unif")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Weibull")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Arcsine")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{kurtosis}, \code{signature(x = "Pareto")}:}{ 
    exact evaluation using explicit expressions.}

}}
%\references{ ~put references to the literature/web site here ~ }
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{ ~~further notes~~ }

\section{Caveat}{
If any of the packages \pkg{e1071}, \pkg{moments}, \pkg{fBasics} is to be used together with \pkg{distrEx} 
the latter must be attached \emph{after} any of the first mentioned. Otherwise \code{kurtosis()} and \code{skewness()}
defined as \emph{methods} in \pkg{distrEx} may get masked.\cr  
To re-mask, you may use 
\code{kurtosis <- distrEx::kurtosis; skewness <- distrEx::skewness}. 
See also \code{distrExMASK()}.
}

\section{Acknowledgement}{
G. Jay Kerns, \email{gkerns@ysu.edu}, has provided a major contribution,
in particular the functionals \code{skewness} and \code{kurtosis} are due to him.
}


\examples{
# Variance of Exp(1) distribution
var(Exp())

#median(Exp())
IQR(Exp())
mad(Exp())

# Variance of N(1,4)^2
var(Norm(mean=1, sd=2), fun = function(x){x^2})
var(Norm(mean=1, sd=2), fun = function(x){x^2}, useApply = FALSE)

## sd -- may equivalently be replaced by var
sd(Pois()) ## uses explicit terms
sd(as(Pois(),"DiscreteDistribution")) ## uses sums
sd(as(Pois(),"UnivariateDistribution")) ## uses simulations
sd(Norm(mean=2), fun = function(x){2*x^2}) ## uses simulations
#
mad(sin(exp(Norm()+2*Pois()))) ## weird
}
\seealso{\code{\link{distrExIntegrate}}, \code{\link{m1df}}, \code{\link{m2df}},
         \code{\link[distr]{Distribution-class}},\cr
 \code{\link[stats]{sd}}, \code{\link[stats]{var}}, \code{\link[stats]{IQR}},\cr
 \code{\link[stats]{median}}, \code{\link[stats]{mad}},  \code{\link[distr]{sd}} }
\concept{functional}
\concept{var}
\concept{sd}
\concept{IQR}
\concept{mad}
\concept{median}
\concept{skewness}
\concept{kurtosis}
\keyword{methods}
\keyword{distribution}
\concept{integration}
