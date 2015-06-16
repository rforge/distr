\name{Simulation-class}
\docType{class}
\alias{Simulation-class}
\alias{Simulation}
\alias{initialize,Simulation-method}

\title{Class "Simulation" }
\description{  In an object of type \code{Simulation} data can be simulated in any distribution and size.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{Simulation(filename, samplesize, runs, seed, distribution)}
(observation dimension is deduced from slot \code{distribution}).
A \code{Simulation}-object includes a filename, a name for the simulation, the number of runs, the size of the sample, the seed and the distribution of the
random numbers. The slot \code{Data} stays empty until the method simulate has been used.}
\section{Slots}{
  \describe{
    \item{\code{seed}:}{Object of class \code{"list"}: the seed the simulation has been generated with }
    \item{\code{distribution}:}{Object of class \code{"UnivariateDistribution"}: the distribution of the random numbers}
    \item{\code{filename}:}{Object of class \code{"character"}: the filename the simulation shall be saved }
    \item{\code{name}:}{Object of class \code{"character"}: a name for the Simulation}
    \item{\code{Data}:}{Object of class \code{"ArrayorNULLorVector"}: the simulated data }
    \item{\code{samplesize}:}{Object of class \code{"numeric"}: the size of the sample}
    \item{\code{obsDim}:}{Object of class \code{"numeric"}: the dimension of the observations of the data }
    \item{\code{runs}:}{Object of class \code{"numeric"}: the number of runs of the data }
    \item{\code{version}:}{Object of class \code{"character"}: the version of this package, under which this object was generated}
  }
}
\section{Extends}{
Class \code{"Dataclass"}, directly.
}
\section{Methods}{
  \describe{
    \item{Data}{\code{signature(object = "Simulation")}: returns the simulated data.}
    \item{Data<-}{\code{signature(object = "Simulation")}: ERROR: A modification of simulated data is not allowed. }
    \item{filename}{\code{signature(object = "Simulation")}:  returns the the filename}
    \item{filename<-}{\code{signature(object = "Simulation")}: changes the the filename}
    \item{name}{\code{signature(object = "Simulation")}:  returns the the name}
    \item{name<-}{\code{signature(object = "Simulation")}:   changes the the name}
    \item{distribution}{\code{signature(object = "Simulation")}: returns the distribution }
    \item{distribution<-}{\code{signature(object = "Simulation")}: changes the distribution }
    \item{seed}{\code{signature(object = "Simulation")}: returns the seed }
    \item{seed<-}{\code{signature(object = "Simulation")}: changes the seed }
    \item{obsDim}{\code{signature(object = "Simulation")}: returns the dimension of the observations}
    \item{getVersion}{\code{signature(object = "Simulation")}: returns the version of this package, under which this object was generated}
    \item{runs}{\code{signature(object = "Simulation")}: returns the number of runs }
    \item{runs<-}{\code{signature(object = "Simulation")}: changes the number of runs }
    \item{samplesize}{\code{signature(object = "Simulation")}: returns the size of the sample }
    \item{samplesize<-}{\code{signature(object = "Simulation")}: changes the size of the sample }
    \item{savedata}{\code{signature(object = "Simulation")}: saves the object without the data in the directory of R
    (After loading the data can be reproduced by using simulate.) }
    \item{initialize}{\code{signature(.Object = "Simulation")}: initialize method }
    \item{plot}{\code{signature(x = "Simulation")}: produces a plot of the data matrix; for details confer \code{\link{plot-methods}} }
    \item{print}{\code{signature(x = "Simulation")}: returns filename, seed, the observation dimension, 
     the number of runs, the size of the sample, the distribution generating the simulations, 
     and, if from a version > 1.8, also the package version under which the object was generated }
    \item{show}{\code{signature(x = "Simulation")}: the same as \code{print}.}
    \item{simulate}{\code{signature(x = "Simulation")}: generates the random numbers for the simulation }
    \item{summary}{\code{signature(object = "Simulation")}: returns filename, seed, number of runs, the size of the sample
    and a statistical summary for each run }
}}

\note{ Changing distribution, seed, runs or samplesize deletes possibly simulated data, as it would not fit to the new
parameters. }


\author{
  Thomas Stabla \email{statho3@web.de},\cr 
  Florian Camphausen \email{fcampi@gmx.de},\cr
  Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de},\cr 
  Matthias Kohl \email{Matthias.Kohl@stamats.de}
  }


\seealso{
\code{\link{Dataclass-class}}
\code{\link{Contsimulation-class}}
\code{\link{plot-methods}}
\code{\link{print-methods}}
\code{\link{summary-methods}}
\code{\link{simulate-methods}}
\code{\link{getVersion-methods}}
}
\examples{
N=Norm() # N is a standard normal distribution.
S=Simulation(filename="xyz",runs=10,samplesize=3,seed=setRNG(),distribution=N)
Data(S) # no data yet
simulate(S)
Data(S) # now there are random numbers
Data(S) # the same data as before because the seed has not changed
seed(S)=setRNG()
simulate(S)
Data(S) # different data
savedata(S) # saves the object in the directory of R...
load("xyz") # loads it again...
Data(S) # ...without the data - use simulate to return it!
}
\keyword{manip}
\concept{S4 data class}
\concept{S4 simulation class}
