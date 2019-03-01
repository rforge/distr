getCitAndDesc <- function(dir){
  DESC <- try(read.dcf(file.path(dir,"DESCRIPTION")),silent=TRUE)
  if(is(DESC,"try-error")) DESC <- NULL
  if(!is.null(DESC)){
      nam <- colnames(DESC)
      aut <- if("Authors@R" %in% nam) "Authors@R" else{
             if("author" %in% nam) "author" else "Author"}
      DESC <- gsub("\\n"," ",DESC[1,aut])
#      DESC <- eval(parse(text=DESC))
  }
  CIT <- try(readCitationFile(file.path(dir,"inst/CITATION")),silent=TRUE)
  if(is(CIT,"try-error")) CIT <- NULL
  return(list(DESC=DESC,CIT=CIT))
}

getAllCitAndDesc <- function(dir){
  folders <- unique(gsub("\\.Rcheck","",list.dirs(path=file.path(dir), rec=FALSE)))
  folders <- gsub(".+/([^/]+)","\\1",folders)
  AllList <- vector("list", length(folders))
  for(i in 1:length(folders)){
      AllList[[i]] <- getCitAndDesc(file.path(dir,folders[i]))
  }
  names(AllList) <- folders
  return(AllList)
}

printErg <- function(erg){
   for(i in 1:length(erg)){
      if(!is.null(erg[[i]]$CIT) || !is.null(erg[[i]]$DESC) ){
      cat("\n------------------------------------\n")
      cat(names(erg)[i])
      cat("\n------------------------------------\n")
      cat("Autor(en) in DESCRIPTION\n")
      print(erg[[i]]$DESC)
      cat("\nAutor(en) in CITATION\n")
      print(erg[[i]]$CIT)}
   }
}
printErg(getAllCitAndDesc("C:/rtest/distr/pkg"))
printErg(getAllCitAndDesc("C:/rtest/robast/pkg"))
