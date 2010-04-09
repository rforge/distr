finde <- function(x = "nchar", dir="C:/rtest/distr/pkg/distr/R", ext = "R", rec = FALSE){
  ow <- getwd()
  on.exit(setwd(ow))
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
}


finde(x=".makeQNew", dir ="C:/rtest/distr/pkg/distr/R")
finde(x="http://distr\\.r-forge\\.r-project\\.org/distr\\.pdf", dir ="C:/rtest/distr/", rec=TRUE)

ersetze <- function(x0 = "nchar", x1="nchar", dir="C:/rtest/distr/pkg/distr/R", ext = "R", rec = FALSE){
  ow <- getwd()
  on.exit(setwd(ow))
  infind <- function(dir0){
    setwd(dir0)
    cat("\n")
    findL <- function(y){
       zz <- readLines(y)
       lgr <- grep(x0,zz)
       writeLines(gsub(x0,x1,zz),y)
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
}
ersetze(x0="href=\"distr.pdf\"",x1="href=\"http://cran.r-project.org/web/packages/distrDoc/vignettes/distr.pdf\"", dir ="C:/rtest/distr/www", rec=TRUE,ext="html")
ersetze(x0="peter.ruckdeschel@uni-bayreuth.de",x1="peter.ruckdeschel@itwm.fraunhofer.de", dir ="C:/rtest/distr/www", rec=TRUE, ext="html")
