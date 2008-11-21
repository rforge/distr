finde <- function(x = "nchar", dir="C:/rtest/distr/pkg/distr/R", ext = "R", rec = FALSE){
  ow <- getwd()
  infind <- function(dir0){
    setwd(dir0)
    cat("\n")
    findL <- function(y){
       zz <- readLines(y)
       lgr <- grep(x,zz)
       if(length(lgr>0)){
          if(rec)
             cat(paste(dir0,"/",y,sep="")," :: ",paste(lgr),"\n")
          else cat(y," :: ",paste(lgr),"\n")
       }
       invisible()
    }
  ext0 <- if(ext=="") "" else paste("\\.", ext, sep="")
  DIR <- grep(ext0, dir(, rec = rec), value=TRUE)
  s <- lapply(DIR,findL)
  }
infind(dir)   
setwd(ow)
}
finde(x=".makeQNew", dir ="C:/rtest/distr/pkg/distr/R")
finde(x=".makeQNew", dir ="C:/rtest/distr/pkg/distr", rec=TRUE)

