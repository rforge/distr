getRevNr <- function(dir="C:/rtest/distr/"){
  ow <- getwd()
  on.exit(setwd(ow))
  setwd(dir)
  if(! file.exists(".svn/entries")) return(NULL)
  tab <- read.table(".svn/entries")[,1]
  i<-1
  while(i<length(tab)){
     if(tab[i]=="dir") break
     i <- i+1
  }
  vn <- as.numeric(paste(tab[i+1]))
  while(i<length(tab)){
     if(length(grep("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}",tab[i],value=FALSE))>0) break
     i <- i+1
  }
  dat <- as.POSIXct(sub("(.+)T.+","\\1",grep("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}",tab[i],value=TRUE)))
  who <- paste(tab[i+2])
  return(list(vn=vn,dat=dat,who=who))
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


