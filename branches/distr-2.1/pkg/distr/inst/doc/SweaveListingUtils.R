###############################################################################
# some R helpers for Sweave / listings
# will be included into some package some day...
#
###############################################################################

### default settings

.Rset <- list("language"="R","escapechar"="`",
        "fancyvrb"="true","basicstyle"="\\color{Rcolor}\\footnotesize",
        "commentstyle"="\\color{Rcomment}\\ttfamily\\itshape",
        "literate"="{<-}{{$\\leftarrow$}}2",
        "morekeywords"="[2]{Norm,Pois,lambda,p,d,r,distroptions}")

.Rdset <- list("language"="TeX", "basicstyle"="\\color{black}\\tiny",
               "commentstyle"="\\ttfamily\\itshape")
.Rcolor   <- c(0,0.5,0.5)
.Rout     <- c(0.461,0.039,0.102)
.Rcomment <- c(0.101,0.043,0.432)
.pkv <- "2.0.2"
.pkg <- "distr"

.CacheLength <- 0
.CacheFiles <- NULL

print.taglist <- function(x, LineLength = 80, offset.start = 0,
                          withFinalLineBreak = TRUE){
   xc <- as.character(deparse(substitute(x)))
   ll <- length(x)
   LineL <- max(LineLength-2,0)
   LineBreak <- NULL
   if(ll){
      offS <- paste(rep(" ", offset.start), collapse = "")
      for(i in 1:ll){
          cat(LineBreak)
          trystr0 <- paste(names(x[i]),x[[i]],sep = "=")
          if(i==1){
             trystr  <- trystr0
             ntry <- nchar(trystr) + offset.start
             if(ntry > LineL) error(gettextf(
                      "First element of %s is already too long", xc))
          }else{
             trystr  <- paste(actstr, trystr0, sep = ",")
             ntry <- nchar(trystr)
          }
          if (ntry < LineL){
              actstr <- trystr
          }else{
              cat(actstr,",%",sep="")
              LineBreak <- "\n"
              actstr <- paste(offS, trystr0, sep = "")
          }
      }
      if(nzchar(actstr)) {
         cat("\n", actstr, sep="")
         if(withFinalLineBreak) cat("\n")
      }
   }
   return(invisible())
}

taglist <- function(..., list = NULL, defname = "V"){
  dots <-  c(list,match.call(call = sys.call(),
                       expand.dots = FALSE)$"...")
  ldots <- length(dots)
  if(ldots){
     defname <- unique(defname)
     defnames <- if(length(defname)<ldots){
            paste(rep(defname,length.out=ldots), 1:ldots, sep = "")} else defname
     nms <- names(dots)
     if(is.null(nms)) nms <- rep("", ldots)
     nms[ nms == "" ] <- defnames[nms == ""]
     names(dots) <- nms
     return(structure(as.list(dots), class = c("taglist","list")))
  }
  return(invisible())
}

lstset <- function(taglist, LineLength = 80){
   startS <- "\\lstset{"
   cat(startS)
   print(taglist, LineLength = LineLength, offset.start = nchar(startS),
         withFinalLineBreak = FALSE)
   cat("}%\n")
   return(invisible())
}

lstsetR <- function(Rset = .Rset, LineLength = 80){
   lstset(taglist(list=Rset), LineLength = LineLength)
   return(invisible())
}

lstsetRd <- function(Rdset = .Rdset, LineLength = 80){
   lstset(taglist(list=Rdset), LineLength = LineLength)
   return(invisible())
}


SweaveListingPreparations <- function(LineLength = 80,
   Rset = .Rset, Rdset = .Rdset, Rcolor = .Rcolor, Rout = .Rout,
   Rcomment = .Rcomment, pkg = .pkg, pkv = .pkv, lib.loc = NULL){

line <- paste("%",paste(rep("-",LineLength-2),collapse=""),"%\n", sep="")

cat(line,"%Preparations for Sweave and Listings\n",line,"%\n", sep = "")
cat("\\RequirePackage{color}\n")
cat("\\definecolor{Rcolor}{rgb}{",paste(Rcolor,collapse=", "),"}\n", sep = "")
cat("\\definecolor{Rout}{rgb}{",paste(Rout,collapse=", "),"}\n", sep = "")
cat("\\definecolor{Rcomment}{rgb}{",paste(Rcomment,collapse=", "),"}\n", sep = "")
cat(line)
cat("\\RequirePackage{listings}\n")
cat(line)
cat("\\global\\def\\Rlstset{%\n")
lstsetR(Rset=Rset, LineLength=LineLength)
cat("}\n")
cat("\\global\\def\\Rdlstset{%\n")
lstsetRd(Rdset=Rdset, LineLength=LineLength)
cat("}\n")
cat("\\Rlstset\n")
cat(line)
cat("\\DefineVerbatimEnvironment{Sinput}{Verbatim}")
cat("%\n  {formatcom=\\color{Rcolor}\\lstset{fancyvrb=true}}\n")
cat("\\DefineVerbatimEnvironment{Soutput}{Verbatim}")
cat("%\n  {formatcom=\\color{Rout}\\footnotesize\\lstset{fancyvrb=false}}\n")
cat("\\DefineVerbatimEnvironment{Scode}{Verbatim}")
cat("%\n  {fontshape=sl,formatcom=\\color{Rcolor}\\lstset{fancyvrb=true}}\n")
cat(line)
cat("\\ifthenelse{\\boolean{Sweave@gin}}{\\setkeys{Gin}{width=0.6\\textwidth}}{}%\n")
cat(line)
cat("\\newcommand{\\code}[1]{{\\tt\\color{Rcolor} #1}}\n")
cat("\\newcommand{\\file}[1]{{\\tt #1}}\n")
cat("\\newcommand{\\pkg}[1]{{\\tt \"#1\"}}\n")
if(missing(pkv)){
     if(nzchar(rpkv <- readPkgVersion(package = pkg, lib.loc=lib.loc)))
        pkv <- rpkv
   }
cat("\\newcommand{\\pkgversion}{{\\tt ",pkv,"}}\n", sep = "")
cat(line,"%\n%\n",sep="")
return(invisible())
}

readSourceFromRForge <- function(PKG, TYPE, FILENAME, PROJECT){
  RForgeURL <- paste("http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/",
                            "*checkout*/pkg/", PKG, "/", TYPE,"/", FILENAME,
                            "?root=", PROJECT, sep ="")
  if(is.null(.CacheFiles[[RForgeURL]])){
    .CacheLength <<- .CacheLength + 1
    RL <- readLines(url(RForgeURL))
    .CacheFiles[[RForgeURL]] <<- RL
  }
  .CacheFiles[[RForgeURL]]
}

copySourceFromRForge <- function(PKG, TYPE, FILENAME, PROJECT, from, to,
                                 offset.before = 0, offset.after = 0 ){
   RL <- readSourceFromRForge(PKG, TYPE, FILENAME, PROJECT)
   lR <- length(RL)
   from <- if(missing(from)) 1 else {if(is.numeric(from))
                                        max(from-offset.before,1)
                                     else {if(length(gr0 <- grep(from,RL)))
                                            max(gr0[1]-offset.before,1) else lR
                                          }
                                    }
   to <- if(missing(to)) lR else {if(is.numeric(to))
                                        min(to+offset.after,lR)
                                     else {if(length(gr1<-grep(to,RL[from:lR])))
                                            min(from+gr1[1]-1+offset.after,lR)
                                           else 0
                                           }
                                 }
   if(to>=from) return(list(text=RL[from:to], lines=c(from,to)))
   return(invisible())
}

lstinputSourceFromRForge <- function(PKG, TYPE, FILENAME, PROJECT, from, to,
                                 offset.before = 0, offset.after = 0,
                                 LineLength = 80,
                                 withLines = ifelse(TYPE=="R", TRUE, FALSE)){
   line <- paste("%",paste(rep("-",LineLength-2),collapse=""),"%\n", sep="")
   dots <- match.call(call = sys.call(sys.parent(1)),
                       expand.dots = FALSE)$"..."
   argL0 <- list(PKG, TYPE, FILENAME, PROJECT)
   totl <- 1
   if(!missing(from)) totl <- max(totl, length(from))
   if(!missing(to)) totl <- max(totl, length(to))
   if(totl>1){
      argL <- vector("list",totl)
      fromL <- NULL; toL <- NULL
      offs.from <- rep(offset.before, length.out = totl)
      offs.to <- rep(offset.after, length.out = totl)
      if(!missing(from))
          fromL <- rep(as.list(from),length.out = totl)
      if(!missing(to))
          toL <- rep(as.list(to),length.out = totl)
      for (j in 1 : totl){
          argL[[j]] <- argL0
          if(!missing(from))
              argL[[j]] <- c(argL[[j]], from = fromL[[j]])
          if(!missing(to))
              argL[[j]] <- c(argL[[j]],to = toL[[j]])
          argL[[j]] <- c(argL[[j]], offset.before = offs.from[j],
                         offset.after = offs.to[j])
      }
   }else {
      argL <- argL0
      if(!missing(from)) argL <- c(argL, from = from)
      if(!missing(to)) argL <- c(argL, to = to)
      argL <- list(c(argL, offset.before = offset.before,
                        offset.after = offset.after))
   }
   erg <- lapply(argL, function(x)  do.call(copySourceFromRForge, args = c(x)))
   RL <- lapply(erg, function(x) x$text)
   lineNr <- lapply(erg, function(x) x$lines)
   lR <- lapply(RL, length)
   lE <- length(erg)
   if(withLines){
      for(k in 1:lE){
        if( k == 1 ) {
             if( ( lineNr[[k]][1] < lineNr[[k]][2] ) || ( lE>1 ) )
                  cat("lines ")
             else cat("line ")
        }else{
             if( k < lE )
                  cat(", \n")
             else cat(", and\n")
             }
        if(lineNr[[k]][1] < lineNr[[k]][2])
           cat(lineNr[[k]][1], "--", lineNr[[k]][2], sep = "")
        else cat(lineNr[[k]][1])
      }
      cat("\n")
   }
   for(k in 1:length(erg)){
     if(lR[[k]]){
        todo <- NULL
        if(TYPE=="man"){
          ex.from <- if(length(gr <- grep("\\\\examples\\{",RL[[k]]))) gr[1] else lR[[k]]
          ex.to <- if(length(gr <- grep("\\}",RL[[k]][ex.from:lR[[k]]]))) ex.from+gr[1]-1 else 1
          cat(line,"\\Rdlstset\n",sep="")
          cat("\\begin{lstlisting}\n")
          if(ex.from<=ex.to){
             writeLines(RL[[k]][1:(ex.from)])
             cat("\\end{lstlisting}\\vspace{-2ex}\n")
             cat(line,"\\Rlstset\n",sep="")
             cat("\\begin{lstlisting}[basicstyle = \\color{Rcolor}")
             cat("\\scriptsize, xleftmargin = 2em]\n")
             writeLines(RL[[k]][(ex.from+1):(ex.to-1)])
             if(ex.to <lR){
                cat("\\end{lstlisting}\\vspace{-3ex}\n")
                cat(line,"\\Rdlstset\n",sep="")
                cat("\\begin{lstlisting}\n")
                writeLines(RL[[k]][(ex.to):lR[[k]]])
                }
          }else writeLines(RL[[k]])
        }else{
          cat(line,"\\Rlstset\n",sep="")
          cat("\\begin{lstlisting}\n")
          writeLines(RL[[k]])
        }
        cat("\\end{lstlisting}\n",line,"%\n\n",sep="")
        }
      }
   return(invisible())
}

readPkgVersion <- function(package, lib.loc = NULL){
       Dfile <- system.file("DESCRIPTION", package=package, lib.loc=lib.loc)
       return( if(nzchar(Dfile)) read.dcf(Dfile, fields="Version") else "")}