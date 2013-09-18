########## compactify all vignettes
compactVignettes <- function(startDir="C:/rtest",rforgeproject="distr",
                      qpdf = "C:/R/qpdf-4.0.0/bin/qpdf",
                      gs = "C:/Program Files (x86)/Ghostscript/gs9.00/bin/gswin32",
                      select = "distr-2.5", invert = FALSE){
     startD <- file.path(startDir,rforgeproject)
     dir0 <- paste(startD,dir(startD, recursive=TRUE),sep="/")
     dir1 <- grep("inst/doc/.+\\.pdf",dir0,value=TRUE)
     dir1 <- c(dir1,grep("vignettes/.+\\.pdf",dir0,value=TRUE))
     if(!is.null(select)){
        l <- length(select)
        for(i in 1:l) dir1 <- grep(select[i],dir1, value = TRUE, invert = invert[i])
     }
     print(dir1)
     tools::compactPDF(dir1, gs_quality = "ebook", qpdf=qpdf, gs_cmd=gs)
}

if(FALSE){
## Examples
compactVignettes()
compactVignettes(select="branches", invert=TRUE)
}