finde <- function(x = "nchar", dir="C:/rtest/distr/pkg/distr/R", ext = "R"){
ow <- getwd()
setwd(dir)
cat("\n")
findL <- function(y){
zz=readLines(y)
lgr <- grep(x,zz)
if(length(lgr>0))
   cat(y," :: ",paste(lgr),"\n")
invisible()
}
DIR <- grep(paste("\\.", ext, sep=""), dir(), value=TRUE)
s = lapply(DIR,findL)
setwd(ow)
}
finde(x=".makeQNew", dir ="C:/rtest/distr/pkg/distr/R")

