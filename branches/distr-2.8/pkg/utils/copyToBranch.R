copyToBranch <- function(FILE, fromDir, toDir, fileInfo=NULL, recursive = FALSE, overwrite=TRUE){
   oldDir <- getwd()
   on.exit(setwd(oldDir))
   ### toCopyList
   toDo <- NULL
   if (file.exists(fromDir)){
       setwd(fromDir)
       toDo <- dir(recursive=recursive)
       if(!is.null(FILE))
           toDo <- toDo[grep(FILE,toDo)]
       if(!is.null(fileInfo)){
          fI <- sapply(toDo, file.info)
          toDo <- fileInfo(fI)
       }
   }
   cat("Files to copy:\n")
   print(toDo)
   if(!is.null(toDo)&&!is.null(toDir)) if(length(toDo))
      return(file.copy(toDo, file.path(toDir,toDo), overwrite= overwrite))
   else return(NULL)
}

copyToBranch(FILE="^pkg/[^/]*/inst/NEWS",fromDir="C:/rtest/distr",
             toDir="C:/rtest/distr/branches/distr-2.1",rec=TRUE)
             
copyToBranch(FILE="^pkg/utils/c.*",fromDir="C:/rtest/distr",
             toDir="C:/rtest/distr/branches/distr-2.1",rec=TRUE)

## should no longer be used:
##     use svn branch instead for a new branch
##     use svn merge to integrate a devel branch back into trunk
