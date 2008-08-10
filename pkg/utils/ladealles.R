 ladeall <- function(DIR="distr", develDir = "C:/rtest/distr/pkg",pattern=""){
 od <- getwd()
 print(file.path(develDir,DIR, "R"))
 setwd(file.path(develDir,DIR, "R"))
 lapply(grep(paste(pattern,".R$",sep="",collapse=""),dir(),value=T),function(...) {print(...);source(...)})
 setwd(od)
}

ladeall(DIR="distrMod", develDir = "C:/rtest/distr/branches/distr-2.0/pkg")

