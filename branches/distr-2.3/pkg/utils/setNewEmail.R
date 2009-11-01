setNewEmail <- function(
              Email.old = "Peter.Ruckdeschel@web.de",
              Email.new = "Peter.Ruckdeschel@itwm.fraunhofer.de",
              dev.dir = "C:/rtest/",
              packs = c("startupmsg",
                        "distr",
                        "distrEx",
                        "distrDoc",
                        "distrSim", 
                        "distrTEst", 
                        "distrTeach", 
                        "distrMod", 
                        "", 
                        "Benchmark",
                        "RobAStBase",
                        "RandVar",
                        "ROptEst",
                        "RobLox",
                        "RobRex",
                        "ROptRegTS"), 
              packs.dir = paste(dev.dir, c(rep("distr/pkg",8),
                               "robKalman/pkg","",
                               rep("RobASt/pkg",6)), sep=""), 
              withHTML = TRUE, 
              packs.HTML.dir = c("", paste(dev.dir, 
                     c(rep("distr/www",7),"robKalman/www"),
                                      sep=""),rep("",7)), 
              rkurs = TRUE,
              rkursDir = "D:/Eigene Dateien/Arbeit/R-Kurs/",
              exts = c("\\.R$","\\.Rd","\\.Rnw","\\.htm$","\\.html$","\\.tex$")
                           )
{oD <- getwd()
# setwd(dev.dir)

names(packs.dir) <- packs
names(packs.HTML.dir) <- packs
### R-Kurs
if (rkurs)
   {xx <- readLines(paste(rkursDir,"rkurs.tex", sep = ""))
    if(length(grep(Email.old,xx, ignore.case=TRUE)))
       {xx <- gsub(Email.old,Email.new, xx, ignore.case=TRUE)
        writeLines(xx, con=paste(rkursDir,"rkurs.tex", sep = ""))}
   }

changeInList <- function(mylist){
    lapply(mylist, function(x) {
            print(x)
            xx <- readLines(x) 
            if(length(grep(Email.old, xx, ignore.case=TRUE)))
               {xx <- gsub(Email.old,Email.new, xx, ignore.case=TRUE)
               writeLines(xx, con=x)}
               })
            }

for(i in 1:length(packs))
   {
    setwd(packs.dir[i])
    print(gettextf("----: %s/%s",packs.dir[i],packs[i]))
    #DESCRIPTION file
    xx <-  if(packs[i]=="") readLines(paste(packs[i],"DESCRIPTION",sep=""))
           else readLines(paste(packs[i],"/DESCRIPTION",sep=""))
    if(length(grep(Email.old, xx, ignore.case=TRUE)))
         {xx <- gsub(Email.old,Email.new, xx, ignore.case=TRUE)
          if(packs[i]=="") 
             writeLines(xx, con=paste(packs[i],"DESCRIPTION",sep=""))
          else 
             writeLines(xx, con=paste(packs[i],"/DESCRIPTION",sep=""))}

    lapply(exts, function(y) changeInList(list.files(pattern = y, 
                 ignore.case=TRUE, recursive = TRUE)))

    ### Paket-Homepages
    if(withHTML && packs.HTML.dir[i]!="") 
       {setwd(packs.dir[i])

        lapply(exts, function(y) changeInList(list.files(pattern = y, 
                 ignore.case=TRUE, recursive = TRUE)))
       }
   }

   setwd(oD) 
}

setNewEmail()
setNewEmail(  Email.old = "Peter.Ruckdeschel@uni-bayreuth.de",
              Email.new = "Peter.Ruckdeschel@itwm.fraunhofer.de",
              dev.dir = "C:/rtest/",
              packs = c("", 
                        "RobAStBase",
                        "RandVar",
                        "ROptEst",
                        "RobLox",
                        "RobRex",
                        "ROptRegTS"), 
              packs.dir = c(paste(dev.dir, c("robKalman/pkg",
                               rep("RobASt/pkg",6)), sep="")), 
              withHTML = TRUE, 
              packs.HTML.dir = c(paste(dev.dir, 
                     c("robKalman/www"),sep=""),rep("",6)), 
              rkurs = FALSE,
              rkursDir = "",
              exts = c("\\.R$","\\.Rd","\\.Rnw","\\.htm$","\\.html$","\\.tex$")
                           )
