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
   print(file.path(thisdir,"pkg/utils/DESCRIPTIONutils.R"))
   source(file.path(thisdir,"pkg/utils/DESCRIPTIONutils.R"))
   source(file.path(thisdir,"pkg/utils/getRevNr.R"))
}
#

### some utils for unified treatment of DESCRIPTION files from R

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
  s <- sapply(c("Package","Version","Date","Depends","License","VCS/SVNRevision"),
              function(x){ PFca <- replaceField(field=x,dfile=PFc)
                           PFc <<- PFca
                           return(NA)})
  writeLines(PFc, con = PFfile)
  return(invisible())
  }}
}


replaceReqRversion <- function(text,version){
     if(!is.na(version)) gsub("^R[ ]*\\([^\\)]+\\)",version,text)
  }

## needs: getRevNr() in getRevNr.R in  utils/ e.g.
## source("C:/rtest/distr/branches/distr-2.7/pkg/utils/getRevNr.R")

  changeDescription <- function(startDir ## folder with pkgs to be updated,
                             ### e.g. "C:/rtest/distr/branches/distr-2.6"
             ,names ### names of the DESCRIPTION tags to be updated
             ,values ### values of the DESCRIPTION tags to be updated
                     ## (a matrix, columns = pkgs and row = tags see examples)
             ,pkgs = NULL ## pkgs to be updated; if NULL all pkgs in startfolder
             ,withSVNread = TRUE  ### should VCS/SVNRevision be updated
             ,withPackageHelpUpdate = TRUE ### should file <pkg>-package.Rd in man
                                 ## be updated
             ,pathRepo = NULL  ### path to svn repo; if NULL deduced from startDir
                               ### assuming r-forge
             ,withDate = TRUE, ### shall date be updated?
    inRforge = TRUE,    ### shall we use r-forge as repository
                        ## (otherwise need full URL as arg pathRepo
    withlogin = TRUE,   ### do we need option --login (yes in cygwin, don't know in Linux)
    ReqRVersion = NA, ## do we change required R-versions?
    PathToBash = "C:/cygwin64/bin/bash",  ## path to bash
    PathToreadsvnlog.sh="C:/rtest/distr/branches/distr-2.4/pkg/utils",
                    ### path to shell script readsvnlog.sh
    tmpfile = "C:/rtest/tmp-svnlog5.txt", ### some tmpfile to which we write the
                        ## results temporarily; is deleted afterwords
                                verbose = FALSE){
    oldDir <- getwd()
    on.exit(setwd(oldDir))
    setwd(startDir)
    if(withSVNread){
        startD <- gsub("/branches/[^/]+","",startDir)
        if(is.null(pathRepo)) pathRepo <- gsub(".*/([^/]+)/*$","\\1", startD)
        svnrev <- getRevNr(startD, pathRepo, inRforge, withlogin,
                           PathToBash, PathToreadsvnlog.sh)
        print(svnrev)

        if("VCS/SVNRevision" %in% names){
           values[which(names=="VCS/SVNRevision"),] <- c(svnrev[[1]])
        }else{
           nr <- nrow(values)
           names <- c(names,"VCS/SVNRevision")
           vlsvn <- rep(c(svnrev[[1]]),ncol(values))
           values <- base::rbind(values,vlsvn)
           rownames(values)[nr+1] <- "VCS/SVNRevision"
        }
    }
    if(withDate){
       if(!"Date" %in% names){
           nr <- nrow(values)
           dat <- format(Sys.time(), format="%Y-%m-%d")
           names <- c(names,"Date")
           values <- base::rbind(values,rep(dat,ncol(values)))
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
    print(values)
    if (length(pkgs) && length(names) && length(values)){
       pkgs <- pkgs[sapply(pkgs, function(x)
                   file.exists(file.path("pkg",x,"DESCRIPTION")))]
       print(pkgs)
       if(!is.matrix(values))
           values <- matrix(values, length(names), length(pkgs),
                        dimnames = list(names, pkgs))
       else values <- values[,pkgs,drop=F]
      # get packages
       sapply(pkgs, function(x){
         FN <- file.path("pkg",x,"DESCRIPTION")
         xx <- read.dcf(FN)
         if(verbose){
          cat("\nxx:\n---\n")
          print(xx)
          cat("\nvalues:\n-------\n")
          print(values)
          cat("\nnames:\n------\n")
          print(names)
          cat("\nvalues[names,x]:\n-----------------\n")
          print(values[names,x])
          cat("\nxx[names,x]:\n-------------\n")
          print(xx[,names])
         }
         xx[,names] <- values[names,x]
         if(!is.na(ReqRVersion[x])){
            xx[,"Depends"] <- replaceReqRversion(xx[,"Depends"],ReqRVersion[x])
         }
         print(xx[,names])

         write.dcf(xx, file=FN,width=1.2*getOption("width"))
         if(withPackageHelpUpdate)
            updatePackageHelp(package=file.path("pkg",x))
       })
    }
    return(invisible())
  }


### Examples see DESCRIPTIONutilsExamples.R in same folder

updateHTMLpages <- function(startDir = "C:/rtest/distr",
                            pkgNames = c("distr", "distrEx","distrSim",
							             "distrTEst", "distrEllipse",
										 "distr-Familie", "distrTeach",
										 "distrMod", "distrDoc", "RandVar"),
                             pkgVersions = c(rep("2.8.0",9),"1.2.0")
							 ){
   if(is.null(names(pkgVersions))) names(pkgVersions) <- pkgNames
   for(pkg in pkgNames){
       File <- file.path(startDir, "www", paste(pkg,".html",sep=""))
       xx <- suppressWarnings(readLines(con = File))
	   if(length(xx)){
	      xx <- gsub("Release Date: .+<br>", paste("Release Date:",
		            format(Sys.time(), format="%Y-%m-%d"), "<br>"),xx)
          xx <- gsub("> Version: .+<br>", paste("> Version:",
		            pkgVersions[pkg], "<br>"),xx)
          xx <- gsub("last updated on .+<br>", paste("last updated on ",
		            format(Sys.time(), format="%Y-%m-%d"), ". <br>",
					sep=""),xx)
          writeLines(xx, con = File)
       }
    }
    return(invisible())
}

if(FALSE){
   updateHTMLpages()
}



getVersions <- function(startDir = "C:/rtest/robast/branches/robast-0.7",
                        pkgs){
return(sapply(pkgs,function(x){
   ff <- read.dcf(file.path(startDir,
                            "pkg",x,"DESCRIPTION"))
   ff[1,"Version"]}))}




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
if(FALSE) copyDescription(startDir = "C:/rtest/distr")

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

if(FALSE) rmDescription2(startDir = "C:/rtest/distr")
