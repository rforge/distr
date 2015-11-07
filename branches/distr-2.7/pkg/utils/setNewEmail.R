setNewEmail <- function(
              Email.old = "peter.ruckdeschel@itwm.fraunhofer.de",
              Email.new = "peter.ruckdeschel@uni-oldenburg.de",
              dev.dir = "C:/rtest/",
              packs = c("startupmsg",
                        "SweaveListingUtils",
                        "distr",
                        "distrEx",
                        "distrDoc",
                        "distrSim", 
                        "distrTEst", 
                        "distrTeach", 
                        "distrEllipse", 
                        "distrRmetrics", 
                        "distrMod", 
                        "robKalman",
                        "RobAStBase",
                        "RobAStRDA",
                        "RandVar",
                        "ROptEstOld",
                        "ROptEst",
                        "RobLox",
                        "RobExtremes",
                        "RobLoxBioC",
                        "RobRex",
                        "ROptRegTS"), 
              packs.dir = paste(dev.dir, c(rep("distr/pkg",11),
                               "robKalman/pkg",
                               rep("RobASt/pkg",10)), sep=""), 
              withHTML = TRUE, 
              packs.HTML.dir = c("", paste(dev.dir, 
                     c(rep("distr/www",11),"robKalman/www"),
                                      sep=""),rep("",10)), 
              rkurs = FALSE,
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
mypacks = c("startupmsg",
                        "SweaveListingUtils",
                        "distr",
                        "distrEx",
                        "distrDoc",
                        "distrSim", 
                        "distrTEst", 
                        "distrTeach", 
                        "distrEllipse", 
                        "distrRmetrics", 
                        "distrMod", 
                        "robKalman",
                        "RobAStBase",
                        "RobAStRDA",
                        "RandVar",
                        "ROptEstOld",
                        "ROptEst",
                        "RobLox",
                        "RobExtremes",
                        "RobLoxBioC",
                        "RobRex",
                        "ROptRegTS")
                        
dev.dir = "C:/rtest/"
setNewEmail(packs=mypacks[-12], packs.dir = c(paste(dev.dir, c(rep("distr/branches/distr-2.6/pkg",11),
                               rep("RobASt/branches/robast-1.0/pkg",10)), sep=""))) 
setNewEmail(packs=mypacks[1:11], packs.dir = c(paste(dev.dir, rep("distr/branches/distr-2.7/pkg",11), sep=""))) 


setNewEmail()
setNewEmail(  Email.old = "Peter.Ruckdeschel@uni-bayreuth.de",
              Email.new = "peter.ruckdeschel@uni-oldenburg.de",
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
