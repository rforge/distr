getRevNr <- function(dir="C:/rtest/distr/",
    pathRepo = "distr",   ### the svn project name
    inRforge = TRUE,    ### shall we use r-forge as repository
                        ## (otherwise need full URL as arg pathRepo
    withlogin = TRUE,   ### do we need option --login (yes in cygwin, don't know in Linux)
    PathToBash = "C:/cygwin/bin/bash",  ## path to bash
    PathToUtils="C:/rtest/distr/branches/distr-2.4/pkg/utils",
                    ### path to shell script readsvnlog.sh
    tmpfile = "C:/rtest/tmp-svnlog5.txt", ### some tmpfile to which we write the
                        ## results temporarily; is deleted afterwords
    verbose=FALSE
    ){
  ow <- getwd()
  on.exit(setwd(ow))
  setwd(dir)
  toRev <- "HEAD"
  if(inRforge)
     pathRepo <- paste("svn://svn.r-forge.r-project.org/svnroot/",pathRepo,sep="")
  inQuotes <- function(x) paste("\"",x,"\"", sep="")
  comd <- paste(inQuotes(PathToBash), ifelse(withlogin,"--login",""),
                  inQuotes(paste(PathToUtils, "readsvnlog.sh", sep="/")),
                  inQuotes(pathRepo), 501, toRev, 10000,
                  inQuotes(tmpfile), 0)
    if(verbose) cat(comd,"\n")
    system(comd,intern=FALSE,ignore.stdout=TRUE,ignore.stderr=TRUE,wait=TRUE)
    zz<-readLines(tmpfile)
    zz<-gsub("(-{50}.*)", "",zz)
    zz<-gsub("(r[[:digit:]]{1,4}.*)",paste("\\1\n",sep=""),zz)
    zz <- zz[grep("^r[[:digit:]]{1,4}.*",zz)]
    zzn <- as.numeric(gsub("^r([[:digit:]]{1,4}).*","\\1",zz))
    ln <- which.max(zzn)
    zzn <- max(zzn)
    zznm <- gsub("^r[[:digit:]]{1,4} \\| ([^ ]+) \\|.*", "\\1", zz[ln])
    zzdt <- gsub("^r[[:digit:]]{1,4} \\| [^ ]+ \\| ([^ ]+).*", "\\1", zz[ln])
    return(list(vn=zzn,dat=zzdt,who=zznm))
}

getAllRevNr <- function(
         dir = "C:/rtest/distr/", ### top folder from which to recurse
         except = NULL,           ### some exceptions (regular expressions!)
         listOrMax = "list"       ### do we return a list or only the max?
                         ){
  oldDir <- getwd()
  on.exit(setwd(oldDir))
  setwd(dir)
  DD  <- dir(".",rec=T)
  DD1 <- DD[!DD%in%dir(".")]
  DIR <- sort(unique(sub("/[^/]+$","",DD1)))
  if(!is.null(except))
     DIR <- DIR[-grep(except,DIR)]
  li <- vector("list",length(DIR))
  j <- 1
  for(i in 1: length(DIR)){
          li0 <- getRevNr(DIR[i])
          if(!is.null(li0)) {li[[j]] <- li0; j <- j+1}
          }
  li<-li[1:j]
  if(is.null(li[[j]])) li <- li[1:(j-1)]
  print(length(li))
  jm <- 1
  for(j in 1: length(li))
     {#print(j); print(li[[j]]); print(li[[jm]]$dat)
      if(li[[j]]$vn>0)
         if(li[[j]]$dat > li[[jm]]$dat) jm <- j}
  if(listOrMax=="list")   
     return(li) else return(li[[jm]])
}

if(FALSE){
 ### some examples
getRevNr()
getAllRevNr()

}


