finde <- function(x = "nchar", dir="C:/rtest/distr/pkg/distr/R", ext = "R", excludeFilepattern="", excludeext="", withEmpty=FALSE, rec = FALSE){
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
  DIR <- dir(rec=rec)
  if(! ((ext=="")&&(withEmpty))){
     ext0 <- sapply(ext, function(ext1) if(ext=="") "" else paste("\\.", ext, sep=""))
     extL <- sapply(ext0, function(ers) grepl(ers,DIR))
     if(withEmpty){
        emptyL <- !grepl("\\.", DIR)
        extL <- cbind(extL,emptyL)
     }
     if(!is.null(dim(extL))) extL <- apply(extL, 1, any)
     DIR <- DIR[extL]
  }
  if(! ((length(excludeext)==0)||((length(excludeext)==1)&&(all(excludeext==""))))){
     excludeextL <- sapply(excludeext, function(ext1) grepl(ext1, DIR))
     if(!is.null(dim(excludeextL))) excludeextL <- apply(excludeextL, 1, any)
     DIR <- DIR[!excludeextL]
  }
  if(! ((length(excludeFilepattern)==0)||((length(excludeFilepattern)==1)&&(all(excludeFilepattern==""))))){
     excludepatternL <- sapply(excludeFilepattern, function(ext1) grepl(ext1, DIR))
     if(!is.null(dim(excludepatternL))) excludepatternL <- apply(excludepatternL, 1, any)
     DIR <- DIR[!excludepatternL]
  }
  s <- lapply(DIR,findL)
  }
infind(dir)   
}
finde(x="q\\(", dir ="C:/rtest/distr/branches/distr-2.7/pkg/", rec=TRUE)
finde(x="http\\:/", dir ="C:/rtest/distr/branches/distr-2.7/pkg/distr/", rec=TRUE)
#finde(x="Wow6432Node", dir ="C:/R/devel/src/gnuwin32", rec=TRUE, ext="")
finde(x="omega", dir ="C:/rtest/distr/branches/distr-2.7/pkg/distrMod/", rec=TRUE)

#finde(x="getPos", dir ="C:/rtest/distr/branches/distr-2.4/pkg/distrMod/", rec=TRUE)
finde(x="roptest\\(", dir ="C:/rtest/robast/branches/robast-0.9/pkg/ROptEst/", rec=TRUE)
finde(x="getSlots\\(", dir ="C:/rtest/distr/branches/distr-2.4/pkg/distrMod/", rec=TRUE)
finde(x="\"loc\"", dir ="C:/rtest/distr/branches/distr-2.4/pkg/distrEx/", rec=TRUE)
finde(x=".makeQNew", dir ="C:/rtest/distr/pkg/distr/R")
finde(x="http://distr\\.r-forge\\.r-project\\.org/distr\\.pdf", dir ="C:/rtest/distr/", rec=TRUE)
finde(x="cniper.+\\(", dir ="C:/rtest/robast/branches/robast-0.9/pkg", rec=TRUE)

ersetze <- function(x0 = "nchar", x1="nchar", dir="C:/rtest/distr/pkg/distr/R", ext = "R", excludeFilepattern="", excludeext="", rec = FALSE, withEmpty=FALSE, withoverwrite = FALSE){
  ow <- getwd()
  on.exit(setwd(ow))
  infind <- function(dir0){
    setwd(dir0)
    cat("\n")
    findL <- function(y){
       zz <- readLines(y)
       lgr <- grep(x0,zz)
       if(length(lgr>0)){
          if(withoverwrite) writeLines(gsub(x0,x1,zz),y)
          if(rec)
             cat(paste(dir0,"/",y,sep="")," :: ",paste(lgr),"\n")
          else cat(y," :: ",paste(lgr),"\n")
       }
       invisible()
    }
  DIR <- dir(rec=rec)
  if(! ((ext=="")&&(withEmpty))){
     ext0 <- sapply(ext, function(ext1) if(ext=="") "" else paste("\\.", ext, sep=""))
     extL <- sapply(ext0, function(ers) grepl(ers,DIR))
     if(withEmpty){
        emptyL <- !grepl("\\.", DIR)
        extL <- cbind(extL,emptyL)
     }
     if(!is.null(dim(extL))) extL <- apply(extL, 1, any)
     DIR <- DIR[extL]
  }
  if(! ((length(excludeext)==0)||((length(excludeext)==1)&&(all(excludeext==""))))){
     excludeextL <- sapply(excludeext, function(ext1) grepl(ext1, DIR))
     if(!is.null(dim(excludeextL))) excludeextL <- apply(excludeextL, 1, any)
     DIR <- DIR[!excludeextL]
  }
  if(! ((length(excludeFilepattern)==0)||((length(excludeFilepattern)==1)&&(all(excludeFilepattern==""))))){
     excludepatternL <- sapply(excludeFilepattern, function(ext1) grepl(ext1, DIR))
     if(!is.null(dim(excludepatternL))) excludepatternL <- apply(excludepatternL, 1, any)
     DIR <- DIR[!excludepatternL]
  }
  s <- lapply(DIR,findL)
  }
infind(dir)
}
ersetze(x0="https://distr.r-forge.r-project.org/",x1="http://distr.r-forge.r-project.org/", dir ="C:/rtest/distr/branches/distr-2.7/pkg", rec=TRUE,ext="Rd")

ersetze(x0="href=\"distr.pdf\"",x1="href=\"http://cran.r-project.org/web/packages/distrDoc/vignettes/distr.pdf\"", dir ="C:/rtest/distr/www", rec=TRUE,ext="html")
ersetze(x0="peter.ruckdeschel@uni-bayreuth.de",x1="peter.ruckdeschel@uni-oldenburg.de", dir ="C:/rtest/distr/www", rec=TRUE, ext="html")
ersetze(x0="@itwm.fraunhofer.de", x1="@uni-oldenburg.de", dir ="C:/rtest/robast/branches/robast-1.1", rec=TRUE, withEmpty=TRUE, ext="", excludeext=c("pdf","Rout\\.save","tar\\.gz", excludeFilepattern="Rcheck"))
