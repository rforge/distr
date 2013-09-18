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
  s <- sapply(c("Package","Version","Date","Depends","License","SVNRevision"),
              function(x){ PFca <- replaceField(field=x,dfile=PFc)
                           PFc <<- PFca
                           return(NA)})
  writeLines(PFc, con = PFfile)
  return(invisible())
  }}
}

## needs: getRevNr() in getRevNr.R in  utils/ e.g.
## source("C:/rtest/distr/branches/distr-2.4/pkg/utils/getRevNr.R")

  changeDescription <- function(startDir ## folder with pkgs to be updated,
                             ### e.g. "C:/rtest/distr/branches/distr-2.6"
             ,names ### names of the DESCRIPTION tags to be updated
             ,values ### values of the DESCRIPTION tags to be updated
                     ## (a matrix, columns = pkgs and row = tags see examples)
             ,pkgs = NULL ## pkgs to be updated; if NULL all pkgs in startfolder
             ,withSVNread = TRUE  ### should SVNRevision be updated
             ,withPackageHelpUpdate = TRUE ### should file <pkg>-package.Rd in man
                                 ## be updated
             ,pathRepo = NULL  ### path to svn repo; if NULL deduced from startDir
                               ### assuming r-forge
             ,withDate = TRUE, ### shall date be updated?
    inRforge = TRUE,    ### shall we use r-forge as repository
                        ## (otherwise need full URL as arg pathRepo
    withlogin = TRUE,   ### do we need option --login (yes in cygwin, don't know in Linux)
    PathToBash = "C:/cygwin/bin/bash",  ## path to bash
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
                           PathToBash, PathToreadsvnlog.sh)[[1]]
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
          print(xx)
          print(values)
          print(names)
          print(values[names,x])
          print(xx[,names])
         }
         xx[,names] <- values[names,x]
         print(xx[,names])
         write.dcf(xx, file=FN,width=1.2*getOption("width"))
         if(withPackageHelpUpdate)
            updatePackageHelp(package=file.path("pkg",x))
       })
    }
    return(invisible())
  }
### Examples see DESCRIPTIONutilsExamples.R in same folder

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
