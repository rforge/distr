source("C:/rtest/distr/branches/distr-2.3/pkg/utils/getRevNr.R")
#
setNewversion <- function(
              dev.dir = "C:/rtest/",
              packs = c("startupmsg",
                        "SweaveListingUtils",
                        "distr",
                        "distrEx",
                        "distrDoc",
                        "distrMod",
                        "distrSim", 
                        "distrTEst", 
                        "distrTeach", 
                        "distrEllipse",
                        "robKalman", 
#                        "Benchmark",
                        "RandVar",
                        "RobAStBase",
                        "ROptEst",
                        "RobLox",
                        "RobRex"
                        "RobLoxBioC",
                        "ROptEstOld",
                        "ROptRegTS",
                        ), 
              svnrepos =  paste(dev.dir, c(rep("distr",10),"robkalman",
                                rep(RobASt",8)), sep=""),
              packs.dir = paste(dev.dir, c(rep("distr/pkg",10),
                               "robKalman/pkg","",
                               rep("RobASt/pkg",8)), sep=""), 
              packs.ver = c("0.5", "0.8", rep("2.3",8),"0.4",rep("0.8",8),          
              packs.gtR = c("1.8.0","2.10.0","2.2.0",rep("2.6.0",9),"2.6.0",
              rep("2.7.0",4),"2.4.0","2.8.1","2.4.0","2.4.0"),
              withHTML = TRUE, 
              packs.HTML.dir = c("", "", paste(dev.dir, 
                     c(rep("distr/www",8),"robKalman/www"),
                                      sep=""),"",
                     paste(dev.dir, c(rep("RobASt/www",8)), sep="")),
              rkurs = TRUE,
              rkursDir = "D:/Eigene Dateien/Arbeit/R-Kurs/"
                           )

{oD <- getwd()
# setwd(dev.dir)

names(packs.dir) <- packs
names(packs.HTML.dir) <- packs
names(packs.ver) <- packs
names(packs.gtR) <- packs

mydate <- strsplit(paste(Sys.time())," ")[[1]][1]

x <- strsplit(date()," ")[[1]]

### R-Kurs
if (rkurs)
   {xx <- readLines(paste(rkursDir,"rkurs.tex", sep = ""))
    xx <- gsub("\\\\def \\\\DISTRV\\{.*\\}", 
         paste("\\\\def \\\\DISTRV\\{", packs.ver["distr"], "\\}", sep = ""), xx)
    writeLines(xx, con=paste(rkursDir,"rkurs.tex", sep = ""))
   }

for(i in 1:length(packs))
   {
    setwd(packs.dir[i])
    svn <- getRevAllNr(packs.dir[i],list="max")
    svnrev <- svn[[1]]
    
    #DESCRIPTION file
    xx <-  readLines(paste(packs[i],"/DESCRIPTION",sep=""))

    xx <- gsub("^Date: .*", paste("Date:", mydate), xx)
    
    pk <- packs.ver[i]
                    
    gtR <- packs.gtR[i]

    xx <- gsub("^Version: .*", paste("Version:", pk),xx)
    xx <- gsub("^SVNRevision: .*$", paste("SVNRevision: ", sverev),xx)

    writeLines(xx, con=paste(packs[i],"/DESCRIPTION",sep=""))

    #man-pages
    fdx <- grep("\\-package",dir(paste(packs[i],"/man",sep="")),value=TRUE)

    if (file.exists(paste(packs[i],"/man/",fdx,sep="")))                  
     { yy <-  readLines(paste(packs[i],"/man/",fdx,sep=""))
    
       yy <- gsub("Version: \\\\tab .*\\\\cr$", 
                    paste("Version: \\\\tab", pk, "\\\\cr"), yy)

       yy <- gsub("SVNRevision: \\\\tab .*\\\\cr$", 
                    paste("SVNRevision: \\\\tab", svnrev, "\\\\cr"), yy)

       yy <- gsub("Date: \\\\tab .*\\\\cr$", 
                    paste("Date: \\\\tab", mydate, "\\\\cr"), yy)
       
       writeLines(yy, con=paste(packs[i],"/man/",fdx,sep=""))
      } 


    ### Paket-Homepages
    if(withHTML && packs.HTML.dir[i]!="") #)(length(grep("distr",packsi)>0)||i=="robKalman"))
       {hhF <- paste(packs.HTML.dir[i],"/",packs[i],".html",sep="")
        hh <- readLines(hhF)
        searchV <- paste(".*<div style=\"text-align: justify;\">[[:space:]]",
                           "*Version:[[:space:]]*([[:digit:]]+\\.[[:digit:]]+",
                           "(-[[:digit:]]+)?)[[:space:]]*", sep = "")
        searchV1 <- paste(searchV, "(<br>|-.*)", 
                           sep = "") 
        searchV2 <- paste(searchV, "(<br>|---[[:space:]]+still in[[:space:]]*",
                           "(\n)development stage<br>)", 
                           sep = "")
        m1 <- grep(searchV1,hh)
        oldV <- gsub(searchV1, "\\1", hh[m1])
        
        hh <- gsub("Release Date:.*<br>",
             paste("Release Date: ", mydate, " <br>", sep=""),
             hh)

        LN <- grep("Required R-Version:",hh)[1]+1
        LNE <- length(hh)
#
# no longer needed on R-Forge....
#

#        DWN <- grep("<h2>Download</h2>",hh)[1]+2
#        DLN <- grep("<h3 style=\"text-align: justify; color: rgb\\(0, 0, 0\\);\">Linux</h3>",hh)[1]+1
        CHN <- grep("<h2 style=\"text-align: center; color: rgb\\(0, 0, 153\\);\">Our plans for",hh)[1]-2
        LUN <- grep("and last updated on ",hh)[1]-1
        
        hh[LUN+1] <- paste("and last updated on ",mydate,".<br>",sep="")
        
        print(c(pk,oldV))
        if(pk!=oldV)             
        {hh1 <- character(LNE+7)
         hh1[1:LN] <- hh[1:LN]
         m2 <- grep(searchV1,hh1)
         hh1[m2:(m2+1)] <- strsplit(gsub(searchV2, 
             paste("<div style=\"text-align: justify;\"> Version: ", pk, "<br>\\4 ", sep=""),
             paste(hh1[m2], hh1[m2+1], sep = "\n") ),"\n")[[1]][1:2]
         hh1[LN+1] <- paste("  <li>&gt;=",gtR," for version ",pk,",</li>",sep="")
         hh1[(LN+2):(CHN+1)] <- hh[(LN+1):(CHN)]
#
# no longer needed on R-Forge....
#
#         hh1[DWN+2] <- paste("  <li>as&nbsp;<a href=\"",
#                             ifelse(i=="robKalman","",
#                                    paste("v",pkgv,"/", sep = "")), 
#                             packs[i],"_",pk,
#                             ".zip\">Win-Zip-File for R &gt;=", 
#                             gtR,"</a>",sep="")        
#         hh1[DWN+3] <- paste("(Version ",pk,")</li>",sep="")        
#         hh1[(DWN+4):(DLN+3)] <- hh[(DWN+1):(DLN)]
#         hh1[DLN+4] <- paste("  <li>as <a href=\"",
#                             ifelse(i=="robKalman","",
#                                    paste("v",pkgv,"/", sep = "")),
#                             packs[i],"_",pk, 
#                             ".tar.gz\">Linux-tar-gz-File for R",sep="")
#         hh1[DLN+5] <- paste("&gt;=",gtR,"</a> (Version ",pk,")</li>",sep="")
#         hh1[(DLN+6):(CHN+5)] <- hh[(DLN+1):(CHN)]
         hh1[CHN+2] <- paste("<h3 style=\"text-align: left; color: rgb\\(0, 0, 0\\);\">Changes from ",
                            oldV," to", sep="")
         hh1[CHN+3] <- paste(pk,"<br>",sep="")
         hh1[CHN+4] <- "</h3>";  hh1[CHN+9] <- "<ul>" 
         hh1[CHN+6] <- "<li> TEMPLATE </li>"
         hh1[CHN+7] <- "</ul>"
         hh1[(CHN+8):(LUN+7)] <- hh[(CHN+1):LUN]
         hh1[LUN+8] <- paste("and last updated on ",mydate,".<br>",sep="")
         hh1[(LUN+9):(LNE+7)] <- hh[(LUN+2):LNE]
         hh <- hh1; rm(hh1)
         }
       writeLines(hh, con=hhF)
       }
   }

### changes in the project page

if(withHTML){  
   for( Hfile in c("distr-Familie.html", "index.php"))
     {

     hhF <- paste(packs.HTML.dir["distr"],"/",Hfile,sep="")
     hh <-  readLines(hhF)        
     hh <- gsub("Release Date:.*<br>",
             paste("Release Date: ", mydate, " <br>", sep=""),
             hh)

     LUN <- grep("and last updated on ",hh)[1]-1
        
     hh[LUN+1] <- paste("and last updated on ",mydate,".<br>",sep="")
        
     writeLines(hh, con=hhF)
     }
}


### in der Vignette


if("distrDoc" %in% packs)
{dDocF <- paste(packs.dir["distrDoc"],"/distrDoc/inst/doc/distr.Rnw",sep = "")
 xy <- readLines(dDocF)
 xy0 <- gsub("\\\\newcommand\\{\\\\pkgversion\\}\\{\\{\\\\tt .*\\}\\}",
          paste("\\\\newcommand\\{\\\\pkgversion\\}\\{\\{\\\\tt ",
                  packs.ver["distr"],"\\}\\}", sep = ""),xy)
 xy0 <- gsub("\\\\newcommand\\{\\\\pkgExversion\\}\\{\\{\\\\tt .*\\}\\}",
        paste("\\\\newcommand\\{\\\\pkgExversion\\}\\{\\{\\\\tt ",
                packs.ver["distr"],"\\}\\}", sep = ""),xy)
# xy[1:10]}
 writeLines(xy, con=dDocF)}

 setwd(oD) 
}

setNewversion()
