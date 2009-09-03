### utility to concatenate all files of same kind in one output
showAllFiles <- function(
         type = "TOBEDONE$", ### type of Files to be concatenated
                             ### typically something like
                             ### DESCRIPTION, NEWS, LICENSE, TOBEDONE, CITATION
         dir = "C:/rtest/distr/", ### top folder from which to recurse
         except = NULL,      ### some exceptions (regular expressions!)
         con = stdout()      ### where to write the results to
                         ){
  myline <- paste(rep("-",length.out=78),collapse="",sep="")
  oldDir <- getwd()
  on.exit(setwd(oldDir))
  printToBeDone <- function(dir0){
    setwd(dir0)
    cat("\n")
    printout <- function(y){
       #y
       zz <- readLines(y, warn = FALSE)
       writeLines(myline, con=con)
       writeLines(paste("File: ",y,collapse=""), con=con)
       writeLines(myline, con=con)
       writeLines(zz, con=con)
       writeLines("\n", con = con)
       invisible()
       }
    DIR <- grep(type, dir(".", rec = TRUE), value=TRUE)
    if(!is.null(except))
       DIR <- DIR[-grep(except,DIR)]
    s <- lapply(DIR,printout)
    invisible()
    }
  printToBeDone(dir)
  invisible()
}

if(FALSE){
 ### some examples
showAllFiles()
showAllFiles(Type="NEWS", except="www")
showAllFiles(dir="C:/rtest/robast")

}

if(FALSE){
### does not really work / Tortoise is to be used interactively...
showsvnlogWithTortoise <- function(pathRepo = "distr",
    inRforge = TRUE,
    PathToTortoise="C:/Program Files/TortoiseSVN",
    fromRev = 501, toRev = 502){
    if(inRforge)
       pathRepo <- paste("svn://svn.r-forge.r-project.org/svnroot/",pathRepo,sep="")
    inQuotes <- function(x) paste("\"",x,"\"", sep="")
    comd <- paste(inQuotes(paste(PathToTortoise,
                                "/bin/TortoiseProc.exe", sep ="")),
            "/command:log", paste("/path:", inQuotes(pathRepo), sep=""),
            paste("/startrev:",fromRev, sep=""),
            paste("/endrev:", toRev, sep=""), "/closeonend:0")
    system(comd,intern=FALSE,ignore=TRUE,wait=FALSE)
    }
}

showsvnlog <- function(
    pathRepo = "distr",   ### the svn project name
    inRforge = TRUE,    ### shall we use r-forge as repository
                        ## (otherwise need full URL as arg pathRepo
    withlogin = TRUE,   ### do we need option --login (yes in cygwin, don't know in Linux)
    PathToBash = "C:/cygwin/bin/bash",  ## path to bash
    PathToUtils="C:/rtest/distr/branches/distr-2.2/pkg/utils",
                    ### path to shell script readsvnlog.sh
    fromRev = 501,  ## arg for svn log --- first log-listed revision
    toRev = 502,    ## arg for svn log --- last log-listed revision;
                    ## may be Inf or <0 => then use HEAD revision
    limit = 100,    ### how many revisions will be shown atmost
    tmpfile = "C:/rtest/tmp-svnlog3.txt", ### some tmpfile to which we write the
                        ## results temporarily; is deleted afterwords
    con = stdout(),     ### results are written to con; by default screen, but
                        ### may also be a file
    withChPaths = FALSE ### shall changed files be reported?
    ){

    if(toRev==Inf || toRev<0) toRev <- "HEAD"
    myline <- paste(rep("-",length.out=72),collapse="",sep="")
    if(inRforge)
       pathRepo <- paste("svn://svn.r-forge.r-project.org/svnroot/",pathRepo,sep="")
    inQuotes <- function(x) paste("\"",x,"\"", sep="")

    comd <- paste(inQuotes(PathToBash), ifelse(withlogin,"--login",""),
                  inQuotes(paste(PathToUtils, "readsvnlog.sh", sep="/")),
                  inQuotes(pathRepo), fromRev, toRev, limit,
                  inQuotes(tmpfile), ifelse(withChPaths,1,0))
    cat(comd,"\n")
    system(comd,intern=FALSE,ignore=TRUE,wait=TRUE)
    zz<-readLines(tmpfile)
    zz<-gsub("(-{50}.*)", "\n\\1",zz)
    zz<-gsub("(r[[:digit:]]{1,4}.*)",paste("\\1\n",myline,sep=""),zz)
    if(!withChPaths){
      zz <- zz[-grep("Changed paths",zz)]
      zz <- zz[-grep("^   [A,M,D,R] ",zz)]
    }
    writeLines(zz,sep="\n", con= con)
    file.remove(tmpfile)
    return(invisible())
    }

if(FALSE){
### some examples
showsvnlog(from=500,to=520)
showsvnlog(from=300,to=Inf,limit=1000)
}
