 ladeall <- function(DIR="distr", develDir = "C:/rtest/distr/pkg",pattern=""){
 od <- getwd()
 print(file.path(develDir,DIR, "R"))
 setwd(file.path(develDir,DIR, "R"))
 lapply(grep(paste(pattern,".(r|R)$",sep="",collapse=""),dir(),value=T),function(...) {print(...);source(...)})
 setwd(od)
}

#ladeall(DIR="distr", develDir = "C:/rtest/distr/branches/distr-2.1/pkg")
#ladeall(DIR="distrEx", develDir = "C:/rtest/distr/pkg")
ladeall(DIR="ROptEst", develDir = "C:/rtest/robast/pkg")

