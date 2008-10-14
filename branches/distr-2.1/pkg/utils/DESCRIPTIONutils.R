### some utils for unified treatment of DESCRIPTION files from R

changeDescription <- function(startDir, names, values){
  oldDir <- getwd()
  setwd(startDir)
    # get packages
  pkgs <- dir("pkg/")
  idx <-  grep(".+\\.",pkgs)
  if(length(idx)) idx <- -idx else idx <- TRUE
  pkgs <- pkgs[idx]
  pkgs <- pkgs[sapply(pkgs, function(x)
                 file.exists(paste("pkg/",x,"/DESCRIPTION",sep="")))]
  sapply(pkgs, function(x){
    FN <- paste("pkg/",x,"/DESCRIPTION",sep="")
    xx <- read.dcf(FN)
    xx[,names] <- values
    write.dcf(xx, file=FN)
  })
  setwd(oldDir)
}

changeDescription(startDir = "C:/rtest/distr",names=c("Version", "License", "Date"),
                   values=c("2.0.2","LGPL-3",paste(Sys.Date())))
                   
copyDescription <- function(startDir){
  oldDir <- getwd()
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
  setwd(oldDir)
}
copyDescription(startDir = "C:/rtest/distr")

rmDescription2 <- function(startDir){
  oldDir <- getwd()
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
  setwd(oldDir)
}

rmDescription2(startDir = "C:/rtest/distr")
