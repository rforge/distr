pkgname <- "distrSim"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('distrSim')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Contsimulation-class")
### * Contsimulation-class

flush(stderr()); flush(stdout())

### Name: Contsimulation-class
### Title: Class "Contsimulation"
### Aliases: Contsimulation-class Contsimulation
###   initialize,Contsimulation-method
### Keywords: manip

### ** Examples

N <- Norm() # N is a standard normal distribution.
C <- Cauchy() # C is a Cauchy distribution
cs <- Contsimulation(filename = "csim",
                     runs = 10,
                     samplesize = 3,
                     seed = setRNG(),
                     distribution.id = N,
                     distribution.c = C,
                     rate = 0.1)
simulate(cs)
# Each of the 30 random numbers is ideal (N-distributed) with
# probability 0.9 and contaminated (C-distributed) with
# probability = 0.1
Data(cs)
Data.id(cs)
Data.c(cs)
ind(cs)
summary(cs)
Data(cs) # different data
savedata(cs) # saves the object in the working directory of R...
load("csim") # loads it again...
Data(cs) # ...without the data - use simulate to return it!



cleanEx()
nameEx("Dataclass-class")
### * Dataclass-class

flush(stderr()); flush(stdout())

### Name: Dataclass-class
### Title: Class "Dataclass"
### Aliases: Dataclass-class initialize,Dataclass-method
### Keywords: manip

### ** Examples

D66 <- Dataclass(filename="N74", Data = matrix(1:36,6))
D66
#
D <- Dataclass(Data = array(c(1,2,3,4,5,6),c(samplesize=2,obsdim=3,Runs=1)),
               filename = "xyz.sav")
# A new object of type "Dataclass" is created.
#
isOldVersion(D) ##NO!
#
savedata(D)
# creates a file called "xyz.sav" where the information is saved and a
# copy "xyz.sav.comment" without data
Data(D) <- array(c(11,12,13,14,15,16),c(samplesize=2,obsdim=3,Runs=1)) # changes the data of D
cload("xyz.sav") # loads the object without data - it is called "D.comment"
D.comment
load("xyz.sav") # loads the original object "D"
Data(D) # the original data: matrix(c(1,2,3,4,5,6),2)
#if you have distrTEst available:
#evaluate(object = D, estimator = mean) # returns the mean of each variable



cleanEx()
nameEx("Simulation-class")
### * Simulation-class

flush(stderr()); flush(stdout())

### Name: Simulation-class
### Title: Class "Simulation"
### Aliases: Simulation-class Simulation initialize,Simulation-method
### Keywords: manip

### ** Examples

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



cleanEx()
nameEx("cload")
### * cload

flush(stderr()); flush(stdout())

### Name: cload
### Title: cload
### Aliases: cload
### Keywords: methods file

### ** Examples

# see Dataclass and Evaluation for examples
## The function is currently defined as
function(filename){
  eval.parent(parse(text=paste("load(\"",filename,".comment\")", sep = "")))
  }



cleanEx()
nameEx("distrSimMASK")
### * distrSimMASK

flush(stderr()); flush(stdout())

### Name: distrSimMASK
### Title: Masking of/by other functions in package "distrSim"
### Aliases: distrSimMASK MASKING
### Keywords: programming distribution documentation

### ** Examples

distrSimMASK()



cleanEx()
nameEx("distrSimoptions")
### * distrSimoptions

flush(stderr()); flush(stdout())

### Name: distrSimoptions
### Title: functions to change the global variables of the package
###   'distrSim'
### Aliases: distrSimoptions getdistrSimOption MaxNumberofPlottedObsDims
###   MaxNumberofPlottedRuns MaxNumberofSummarizedObsDims
###   MaxNumberofSummarizedRuns
### Keywords: misc

### ** Examples

distrSimoptions()
distrSimoptions("MaxNumberofPlottedObsDims")
distrSimoptions("MaxNumberofPlottedObsDims" = 5)
# or
getdistrSimOption("MaxNumberofPlottedObsDims")



cleanEx()
nameEx("subsetting-methods")
### * subsetting-methods

flush(stderr()); flush(stdout())

### Name: Subsetting-methods
### Title: Subsetting/Indexing methods for SeqDataFrames objects in Package
###   'distrSim'
### Aliases: [-methods [<--methods [,SeqDataFrames-method
###   [<-,SeqDataFrames-method
### Keywords: methods

### ** Examples

s0 <- matrix(1:6,3,2)
d0 <- data.frame(s0)
d1 <- data.frame(s0 + 3)
SF <- SeqDataFrames(d0, d1)
SF[1,2,1]



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
