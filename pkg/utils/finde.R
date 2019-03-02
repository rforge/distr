if(FALSE) {
   ## use this to source this file / have finde() and ersetze() available

   ## change svncheckout suitably...
   svncheckout <- file.path("C:/rtest/distr")
   trunk <- TRUE
   ## if the version from devel
   branches <- dir(file.path(svncheckout,"branches"))
   branches <- grep("distr-",branches,value=TRUE)
   branches <- branches[!grepl("\\.[[:alpha:]]+",branches)]
   thisbranch <- max(branches)
   thisdir <- file.path(svncheckout,"branches", thisbranch)
   print(thisdir)
   ## or in trunk
   if(trunk) thisdir <- svncheckout #""
   print(file.path(thisdir,"pkg/utils/finde.R"))
   source(file.path(thisdir,"pkg/utils/finde.R"))
}
#

finde <- function(x = "nchar", dir = "C:/rtest/distr/pkg/distr/R",
                  ext = "R", restrictFilepattern = "", excludeFilepattern = "",
                  excludeext = "", withEmpty = FALSE, rec = FALSE){
  ersetze(x0=x, dir=dir, ext=ext, restrictFilepattern =restrictFilepattern,
          excludeFilepattern = excludeFilepattern, excludeext =excludeext,
          withEmpty = withEmpty, rec = rec, withoverwrite = FALSE)
}

if(FALSE){
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
}

selectfiles <- function(pat,DIR, negate=FALSE){
  if(! ((length(pat)==0)||((length(pat)==1)&&(all(pat==""))))){
     patL <- sapply(pat, function(ext1) grepl(ext1, DIR))
     if(!is.null(dim(patL))) patL <- apply(patL, 1, any)
     if(negate) patL <- !patL
     DIR <- DIR[patL]
  }
  return(DIR)
}

ersetze <- function(x0 = "nchar", x1 = "nchar", dir = "C:/rtest/distr/pkg/distr/R",
                    ext = "R", restrictFilepattern = "", excludeFilepattern = "",
                    excludeext = "", rec = FALSE, withEmpty = FALSE,
                    withoverwrite = FALSE){
  ow <- getwd()
  on.exit(setwd(ow))
  infind <- function(dir0){
    setwd(dir0)
    cat("\n")
    findL <- function(y){
       zz <- readLines(y, warn = FALSE)
       lgr <- grep(x0,zz)
       if(length(lgr>0)){
          if(withoverwrite) writeLines(gsub(x0,x1,zz),y)
          if(rec)
             cat(file.path(dir0,y)," :: ",paste(lgr),"\n")
          else cat(y," :: ",paste(lgr),"\n")
       }
       invisible()
    }
  DIR <- dir(rec=rec)
  if(! (all(ext=="")&&(!withEmpty))){
     ext0 <- sapply(ext, function(ext1) if(ext=="") "" else paste("\\.", ext, sep=""))
     extL <- sapply(ext0, function(ers) grepl(ers,DIR))
     if(withEmpty){
        emptyL <- !grepl("\\.", DIR)
        extL <- cbind(extL,emptyL)
     }
     if(!is.null(dim(extL))) extL <- apply(extL, 1, any)
     DIR <- DIR[extL]
  }
  DIR <- selectfiles(excludeext,DIR, negate=TRUE)
  DIR <- selectfiles(restrictFilepattern,DIR, negate=FALSE)
  DIR <- selectfiles(excludeFilepattern,DIR, negate=TRUE)
  s <- lapply(DIR,findL)
  }
infind(dir)
}

if(FALSE){
ersetze(x0="https://distr.r-forge.r-project.org/",x1="http://distr.r-forge.r-project.org/", dir ="C:/rtest/distr/branches/distr-2.7/pkg", rec=TRUE,ext="Rd")
ersetze(x0="href=\"distr.pdf\"",x1="href=\"http://cran.r-project.org/web/packages/distrDoc/vignettes/distr.pdf\"", dir ="C:/rtest/distr/www", rec=TRUE,ext="html")
ersetze(x0="peter.ruckdeschel@uni-bayreuth.de",x1="peter.ruckdeschel@uni-oldenburg.de", dir ="C:/rtest/distr/www", rec=TRUE, ext="html")
ersetze(x0="@itwm.fraunhofer.de", x1="@uni-oldenburg.de", dir ="C:/rtest/robast/branches/robast-1.1", rec=TRUE, withEmpty=TRUE, ext="", excludeext=c("pdf","Rout\\.save","tar\\.gz", excludeFilepattern="Rcheck"))
}
