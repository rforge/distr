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
