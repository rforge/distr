 ladeall <- function(DIR="distr", develDir = "C:/rtest/distr/pkg",pattern="",
          withPrint = TRUE){
 od <- getwd()
 if(withPrint) print(file.path(develDir,DIR, "R"))
 setwd(file.path(develDir,DIR, "R"))
 lapply(grep(paste(pattern,".(r|R)$",sep="",collapse=""),dir(),value=T),
          function(...) {if(withPrint) print(...);source(...)})
 setwd(od)
}

#ladeall(DIR="distr", develDir = "C:/rtest/distr/branches/distr-2.1/pkg")
#ladeall(DIR="distrMod", develDir = "C:/rtest/distr/branches/distr-2.8/pkg")
#ladeall(DIR="distrMod", develDir = "C:/rtest/distr/branches/distr-2.4/pkg",withPrint=FALSE)
ladeall(DIR="RobExtremes", develDir = "C:/rtest/robast/branches/robast-1.2/pkg")

