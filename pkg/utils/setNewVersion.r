setNewversion <- function(
              dev.dir = "C:/rtest/distr/pkg",
              pkgv = "2.0", 
              pkgExv = pkgv, 
              pkgStv = "0.5", 
              pkgrKv = "0.2", 
              pkgBv = "2.4",
              packs = c("startupmsg",
                        "distr",
                        "distrEx",
                        "distrDoc",
                        "distrSim", 
                        "distrTEst", 
                        "distrTeach", 
                        "robKalman", 
                        "Benchmark"), 
              rkurs = TRUE,
              rkursDir = "D:/Eigene Dateien/Arbeit/R-Kurs/",
              withHTML = TRUE, 
              HTMLDir = "C:/rtest/distr/www/",
              robKalmanDir = "C:/rtest/pkg/robKalman/www/",
              gtRv = "2.2.0", gtRKv = "2.3.0")

{oD <- getwd()
 setwd(dev.dir)

mydate <- strsplit(paste(Sys.time())," ")[[1]][1]

x <- strsplit(date()," ")[[1]]

### R-Kurs
if (rkurs)
   {xx <- readLines(paste(rkursDir,"rkurs.tex", sep = ""))
    xx <- gsub("\\\\def \\\\DISTRV\\{.*\\}", 
         paste("\\\\def \\\\DISTRV\\{", pkgv, "\\}", sep = ""), xx)
    writeLines(xx, con=paste(rkursDir,"rkurs.tex", sep = ""))
   }

for(i in packs)
   {
    #DESCRIPTION file
    xx <-  readLines(paste(i,"/DESCRIPTION",sep=""))

    xx <- gsub("^Date: .*", paste("Date:", mydate), xx)
    
    pk <- switch(i, "distrEx" = pkgExv,
                    "startupmsg" = pkgStv,
                    "robKalman" = pkgrKv,
                    "Benchmark" = pkgBv,
                    pkgv)
                    
    gtR <- switch(i, "robKalman" = gtRKv,
                    gtRv)

    xx <- gsub("^Version: .*", paste("Version:", pk),xx)

    writeLines(xx, con=paste(i,"/DESCRIPTION",sep=""))

    #man-pages
    fdx <- grep("\\-package",dir(paste(i,"/man",sep="")),value=TRUE)

    if (i != "startupmsg")
     { yy <-  readLines(paste(i,"/man/",fdx,sep=""))
    
       yy <- gsub("Version: \\\\tab .*\\\\cr", 
                    paste("Version: \\\\tab", pk, "\\\\cr"), yy)

       yy <- gsub("Date: \\\\tab .*\\\\cr", 
                    paste("Date: \\\\tab", mydate, "\\\\cr"), yy)
       
       writeLines(yy, con=paste(i,"/man/",fdx,sep=""))
      } 


    ### Paket-Homepages
    if(withHTML && (length(grep("distr",i)>0)||i=="robKalman"))
       {if (i=="robKalman")
            hh <- readLines(paste(robKalmanDir,i,".html",sep=""))
        else
            hh <- readLines(paste(HTMLDir,i,".html",sep=""))
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
        DWN <- grep("<h2>Download</h2>",hh)[1]+2
        DLN <- grep("<h3 style=\"text-align: justify; color: rgb\\(0, 0, 0\\);\">Linux</h3>",hh)[1]+1
        CHN <- grep("<h2 style=\"text-align: center; color: rgb\\(0, 0, 153\\);\">Our plans for",hh)[1]-2
        LUN <- grep("and last updated on ",hh)[1]-1
        
        hh[LUN+1] <- paste("and last updated on ",mydate,".<br>",sep="")
        
        if(pk!=oldV)             
        {hh1 <- character(LNE+11)
         hh1[1:LN] <- hh[1:LN]
         m2 <- grep(searchV1,hh1)
         hh1[m2:(m2+1)] <- strsplit(gsub(searchV2, 
             paste("<div style=\"text-align: justify;\"> Version: ", pk, "<br>\\4 ", sep=""),
             paste(hh1[m2], hh1[m2+1], sep = "\n") ),"\n")[[1]][1:2]
         hh1[LN+1] <- paste("  <li>&gt;=",gtR," for version ",pk,",</li>",sep="")
         hh1[(LN+2):(DWN+1)] <- hh[(LN+1):(DWN)]
         hh1[DWN+2] <- paste("  <li>as&nbsp;<a href=\"",
                             ifelse(i=="robKalman","",
                                    paste("v",pkgv,"/", sep = "")), 
                             i,"_",pk,
                             ".zip\">Win-Zip-File for R &gt;=", 
                             gtR,"</a>",sep="")        
         hh1[DWN+3] <- paste("(Version ",pk,")</li>",sep="")        
         hh1[(DWN+4):(DLN+3)] <- hh[(DWN+1):(DLN)]
         hh1[DLN+4] <- paste("  <li>as <a href=\"",
                             ifelse(i=="robKalman","",
                                    paste("v",pkgv,"/", sep = "")),
                             i,"_",pk, 
                             ".tar.gz\">Linux-tar-gz-File for R",sep="")
         hh1[DLN+5] <- paste("&gt;=",gtR,"</a> (Version ",pk,")</li>",sep="")
         hh1[(DLN+6):(CHN+5)] <- hh[(DLN+1):(CHN)]
         hh1[CHN+6] <- paste("<h3 style=\"text-align: left; color: rgb\\(0, 0, 0\\);\">Changes from ",
                            oldV," to", sep="")
         hh1[CHN+7] <- paste(pk,"<br>",sep="")
         hh1[CHN+8] <- "</h3>";  hh1[CHN+9] <- "<ul>" 
         hh1[CHN+10] <- "<li> TEMPLATE </li>"
         hh1[CHN+11] <- "</ul>"
         hh1[(CHN+12):(LUN+11)] <- hh[(CHN+1):LUN]
         hh1[LUN+12] <- paste("and last updated on ",mydate,".<br>",sep="")
         hh1[(LUN+13):(LNE+11)] <- hh[(LUN+2):LNE]
         hh <- hh1; rm(hh1)
         }
       if (i=="robKalman")
           writeLines(hh, con=paste(robKalmanDir,i,".html",sep=""))
       else
           writeLines(hh, con=paste(HTMLDir,i,".html",sep=""))    
       }
   }

### in der Vignette

if("distrDoc" %in% packs)
{xy <- readLines("distrDoc/inst/doc/distr.Rnw")
 xy0 <- gsub("\\\\newcommand\\{\\\\pkgversion\\}\\{\\{\\\\tt .*\\}\\}",
          paste("\\\\newcommand\\{\\\\pkgversion\\}\\{\\{\\\\tt ",pkgv,"\\}\\}",
                  sep = ""),xy)
 xy0 <- gsub("\\\\newcommand\\{\\\\pkgExversion\\}\\{\\{\\\\tt .*\\}\\}",
        paste("\\\\newcommand\\{\\\\pkgExversion\\}\\{\\{\\\\tt ",pkgv,"\\}\\}",
               sep = ""),xy0)
# xy[1:10]}
 writeLines(xy, con="distrDoc/inst/doc/distr.Rnw")}

 setwd(oD) 
}

setNewversion(pkgv="2.0")
