\name{setRNG-package}
\alias{setRNG-package}
\alias{setRNG.Intro}

\docType{package}

\title{setRNG}

\section{Usage}{
   library("setRNG") 
}

\description{
  Programs to set random number generator (and seed) in R and S.}
\section{Introduction}{
This library provides tools to simplify recording and resetting the random 
number generator, to help make monte carlo experiments easily reproducible. 
It uses the R/S tools for setting the seed, but also records and sets
the mechanism for converting uniform numbers to normally distributed numbers.
(It could be extended to other transformations, but I have not done that.)

The setRNG function would typically be called by simulation programs (see
example) to set the 
RNG information if given, and record the RNG information
in all cases. This information can be returned with the result
of the simulation. That way the simulation can always be reproduced if
necessary.
 
The library also implements an approach to random number generation 
which allows the same random experiments to be replicated in S and R.
The functions in the S/ directory allow the R results using 
Wichmann-Hill and Box-Muller to be replicated in S. These were done with 
the aid of an example from B. D. Ripley. (The files in the S/ 
directory of the package are for use with S not R.)
These functions are intended primarily as a way to confirm that simulations and
estimations with simulated data work in 
the same way in both S and R, not as an improved RNG. (It has only been tested
in Splus 3.3) Default and other
RNGs can still be used and are preferred for both speed and 
theoretical reasons. 
}
  

\examples{
setRNG(kind="Wichmann-Hill", seed=c(979,1479,1542), normal.kind="Box-Muller")
rnorm(10)

sim <-function(rng=NULL)
 {if(!require("setRNG")) stop("This function requires the setRNG package.")
  if(is.null(rng)) rng <- setRNG() # returns setting so don't skip if NULL
  else        {old.rng <- setRNG(rng);  on.exit(setRNG(old.rng))  }
  x <- list(numbers=rnorm(10))
  x$rng <- rng
  x
 }

z <- sim()
sim()$numbers
sim(rng=getRNG(z))$numbers
z$numbers

}
\keyword{programming}
\keyword{interface}
\keyword{utilities}
\keyword{distribution}

