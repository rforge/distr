### some utils for unified treatment of DESCRIPTION files from R

source("C:/rtest/distr/branches/distr-2.4/pkg/utils/getRevNr.R")

updatePackageHelp <- function(package){
  if(file.exists(file.path(package, "DESCRIPTION"))){
  DFF    <-  read.dcf(file = file.path(package, "DESCRIPTION"))
  mandir <-  dir(file.path(package, "man"))
  PFfileI <-  grep("-package", mandir, value = TRUE)
  if(length(PFfileI)){
  PFfile <- file.path(package, "man", PFfileI)
  PF     <-  readLines(con = PFfile)
  replaceField <- function(field, dfile){
     liS <- paste("(",field,":[[:blank:]]+\\\\tab).+(\\\\cr.*)",sep="")
     reS <- paste("\\1",DFF[1,field],"\\2")
     df0 <- gsub(liS, reS,dfile)
     return(df0)}
  PFc    <-  PF
  s <- sapply(c("Package","Version","Date","Depends","License","SVNRevision"),
              function(x){ PFca <- replaceField(field=x,dfile=PFc)
                           PFc <<- PFca
                           return(NA)})
  writeLines(PFc, con = PFfile)
  return(invisible())
  }}
}


changeDescription <- function(startDir, names, values, 
                              pkgs = NULL, 
                              withSVNread = TRUE,
                              withPackageHelpUpdate = TRUE,
                              pathRepo = NULL,
                              withDate = TRUE){
  oldDir <- getwd()
  on.exit(setwd(oldDir))
  setwd(startDir)
  if(is.null(pathRepo)) pathRepo <- gsub(".*/([^/]+)/*$","\\1", startDir)
  if(withSVNread){
      svnrev <- getRevNr(startDir, pathRepo)[[1]]
      print(svnrev)
      if("SVNRevision" %in% names){
         values[which(names=="SVNRevision"),] <- svnrev
      }else{
         nr <- nrow(values)
         names <- c(names,"SVNRevision")
         values <- rbind(values,rep(svnrev,ncol(values)))
         rownames(values)[nr+1] <- "SVNRevision"
      }
  }
  if(withDate){
     if(!"Date" %in% names){
         nr <- nrow(values)
         dat <- format(Sys.time(), format="%Y-%m-%d")
         names <- c(names,"Date")
         values <- rbind(values,rep(dat,ncol(values)))
         rownames(values)[nr+1] <- "Date"
     }
  }
#  print(names)
#  print(values)
  
  if(is.matrix(values) && is.null(colnames(values))) 
     colnames(values) <- rep(pkgs, length.out = ncol(values))
     
  if(is.null(pkgs)) {
     pkgs <-   pkgs <- dir("pkg/")
     idx <-  grep(".+\\.",pkgs)
     if(length(idx)) idx <- -idx else idx <- TRUE
     pkgs <- pkgs[idx]
  }
  if (length(pkgs) && length(names) && length(values)){
     pkgs <- pkgs[sapply(pkgs, function(x)
                 file.exists(paste("pkg/",x,"/DESCRIPTION",sep="")))]

     if(!is.matrix(values))
         values <- matrix(values, length(names), length(pkgs), 
                      dimnames = list(names, pkgs))
     else values <- values[,pkgs,drop=F]
    # get packages
     sapply(pkgs, function(x){
       FN <- file.path("pkg",x,"DESCRIPTION")
       xx <- read.dcf(FN)
#       print(xx)
#       print(values)
#       print(names)
#       print(values[names,x])
#       print(xx[,names])
       xx[,names] <- values[names,x]
       write.dcf(xx, file=FN,width=1.2*getOption("width"))
       if(withPackageHelpUpdate)
          updatePackageHelp(package=file.path("pkg",x))
     })
  }
  return(invisible())
}

getVersions <- function(startDir = "C:/rtest/robast/branches/robast-0.7",
                        pkgs){
return(sapply(pkgs,function(x){
   ff <- read.dcf(file.path(startDir,
                            "pkg",x,"DESCRIPTION"))
   ff[1,"Version"]}))}


##############################################################################
# EXAMPLES
##############################################################################
if(FALSE){## Example 1
Pkgs <- c("startupmsg", "SweaveListingUtils",
                      "distr", "distrEx", "distrDoc",
                      "distrMod", "distrTeach", "distrSim", "distrTEst")
Names <- c("Version", "License", "Date")
Values <- matrix(c("2.0.2","LGPL-3"),3,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",] <- c("0.5.2", "0.1.1", "2.0.3", "2.0.2", "2.0.3",
                         rep("2.0.2",4))
changeDescription(startDir = "C:/rtest/distr",names=Names,
                  pkgs=Pkgs, values=Values)
}

if(FALSE){## Example 2
Pkgs <- c("SweaveListingUtils", "distr", "distrEx",
                      "distrMod", "distrTeach", "distrSim", "distrTEst")
Names <- c("Date")
Values <- matrix((format(Sys.time(), format="%Y-%m-%d")),1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr",names="Date",
                  pkgs=Pkgs, values=format(Sys.time(), format="%Y-%m-%d"))
}

if(FALSE){### Version 2.4.1
Pkgs <- c("startupmsg", "SweaveListingUtils",
          "distr", "distrEx", "distrDoc",
          "distrMod", "distrTeach", "distrSim",
          "distrTEst", "distrEllipse", "distrRmetrics")
Names <- c("Version")
Values <- matrix(c("2.4.1",1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",,drop=FALSE] <- c("0.8.1", "0.6.1", rep("2.4.1",9))
changeDescription(startDir = "C:/rtest/distr",names=Names,
                  pkgs=Pkgs, values=Values)
}
if(FALSE){### Version 0.9.1
Pkgs <- c("RandVar", "ROptEstOld")
Names <- c("Version")
Values <- matrix("0.9.1",1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/robast",names=Names,
                  pkgs=Pkgs, values=Values)
}

if(FALSE){
Pkgs <- c("RandVar", "RobAStBase", "RobAStRDA", "ROptEst", "RobExtremes")
Names <- c("Version", "License")
Values <- matrix(c(c("0.9.2","LGPL-3"),rep(c("0.9","LGPL-3"),4)),2,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/robast/",names=Names,
                  pkgs=Pkgs, values=Values)
}

}
##############################################################################


copyDescription <- function(startDir){
  oldDir <- getwd()
  on.exit(setwd(oldDir))
  setwd(startDir)
    # get packages
  pkgs <- dir("pkg/")
  pkgs <- pkgs[-grep(".+\\.",pkgs)]
  pkgs <- pkgs[sapply(pkgs, function(x)
                 file.exists(paste("pkg/",x,"/DESCRIPTION",sep="")))]
  sapply(pkgs, function(x){
    FN <- paste("pkg/",x,"/DESCRIPTION",sep="")
    FN2 <- paste("branches/distr-2.1/pkg/",x,"/DESCRIPTION",sep="")
    file.copy(from=FN2, to =FN, over=TRUE)
  })
}
copyDescription(startDir = "C:/rtest/distr")

rmDescription2 <- function(startDir){
  oldDir <- getwd()
  on.exit(setwd(oldDir))
  setwd(startDir)
    # get packages
  pkgs <- dir("pkg/")
  pkgs <- pkgs[-grep(".+\\.",pkgs)]
  pkgs <- pkgs[sapply(pkgs, function(x)
                 file.exists(paste("pkg/",x,"/DESCRIPTION",sep="")))]
  sapply(pkgs, function(x){
    FN <- paste("pkg/",x,"/DESCRIPTION2",sep="")
    file.remove(FN)
  })
}

rmDescription2(startDir = "C:/rtest/distr")
