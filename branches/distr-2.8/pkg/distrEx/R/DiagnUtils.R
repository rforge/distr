############# print and other methods for DiagnosticClass

############################################################################
##########   internal helper functions and constants
############################################################################

.nmsToGather <- c("method", "time", "lower",
                  "upper", "rel.tol", "abs.tol", "stop.on.error",
                  "value", "abs.error", "subdivisions" ,"message")

.reorganizeDiagnosticList <- function(liste, .depth=1, names0, prenames = "",
          nmstoGather="", nmstoGatherNS="",  withprint=TRUE,
          .GatherList = NULL, .GatherListNS = NULL){
  if(missing(names0)||(all(names0=="")&&length(names0)==1))
     names0 <- .showallNamesDiagnosticList(liste)
  nms <- names(liste)
  if(is.null(.GatherList)&&is.null(.GatherListNS)){
    if(missing(nmstoGather)){
    #       if(is.null(match.call()$names0)||all(match.call()$names0==""))
          nmstoGather <- .nmsToGather  # else nmstoGather <- names0
    }
    #    if(all(nmstoGather=="")&&length(nmstoGather)==1)
    nmstoGather <- names0[names0%in%nmstoGather]
    if(length(nmstoGather)){
       .GatherList <- vector("list", length(nmstoGather))
       names(.GatherList) <- nmstoGather
    }
    if(!((missing(nmstoGatherNS)||nmstoGatherNS==""))){
       nmstoGatherNS0 <- nmstoGatherNS
    }else nmstoGatherNS0 <- NULL
    nmstoGatherNS <- names0[!names0%in%nmstoGather]
    if(!is.null(nmstoGatherNS0))
    nmstoGatherNS <- nmstoGatherNS[nmstoGatherNS %in% nmstoGatherNS0]
    if(length(nmstoGatherNS)){
       .GatherListNS <- vector("list", length(nmstoGatherNS))
       names(.GatherListNS) <- nmstoGatherNS
    }

  }
  if(is.null(nms)) nms <- paste("[",seq(liste),"]",sep="")
  for(i in seq(liste)){
     if(nms[i]=="") nms[i] <- paste("[",i,"]",sep="")
     longname <- paste(prenames,nms[i],sep="$")
     if(is(liste[[i]], "try-error")) liste[[i]] <- list("message"=as.list(liste[[i]])[[1]])
   	 if(nms[i]%in% names0){
        if(withprint) cat(rep(">", .depth)," ", nms[i],"\n",sep="")
     }
	   if(is.list(liste[[i]])){
        res <- .reorganizeDiagnosticList(liste[[i]], .depth=.depth+1, names0=names0,
            prenames=longname, nmstoGather=nmstoGather, nmstoGatherNS=nmstoGatherNS,
            withprint= withprint, .GatherList = .GatherList, .GatherListNS = .GatherListNS)
        .GatherList <- res$show
        .GatherListNS <- res$noshow
     }
	   if(!is.null(nms)){
	     	if(nms[i] %in% names0){
    		   if(withprint) cat(longname,":\n")
           if(withprint) print(liste[[i]])
       		 if(nms[i] %in% nmstoGather){
    		     vec0 <- NULL
    		     nvec0 <- NULL
		         if(length(.GatherList[[nms[i]]])) {
		            vec0 <- .GatherList[[nms[i]]]
			          nvec0 <- names(vec0)
		         }
             vecneu <- liste[[i]]
		         lvecneu <- length(vecneu)
		         vec0 <- c(vec0, vecneu)
             nmsC <- if(!is.call(liste[[i]]))
                         paste(longname,names(vecneu),sep=".") else longname
             nvec0 <- c(nvec0, nmsC)
             names(vec0) <- nvec0
		         .GatherList[[nms[i]]] <- vec0
		       }
       		 if(nms[i] %in% nmstoGatherNS){
    		     vec0 <- NULL
    		     nvec0 <- NULL
		         if(length(.GatherListNS[[nms[i]]])) {
		            vec0 <- .GatherListNS[[nms[i]]]
			          nvec0 <- names(vec0)
		         }
             vecneu <- liste[[i]]
		         vec0 <- c(vec0, vecneu)
             nmsC <- if(!is.call(liste[[i]]))
                         paste(longname,names(vecneu),sep=".") else longname
             nvec0 <- c(nvec0, nmsC)
             names(vec0) <- nvec0
		         .GatherListNS[[nms[i]]] <- vec0
		       }
	      }
    }
  }
  if(.depth==1 && "time" %in% c(names(.GatherList),names(.GatherListNS))){
      li <- if("time" %in% names(.GatherList)) .GatherList[["time"]] else .GatherListNS[["time"]]
      if(length(li)){
         linms <- names(li)
         mat <- t(matrix(li,5))
         colmat <- unique(gsub(".+\\$time\\.","",linms))
         rowmat <- unique(gsub("(.+)\\$time\\..+","\\1",linms))
         colnames(mat) <- colmat
         rownames(mat) <- rowmat
         if("time" %in% names(.GatherList))
            .GatherList[["time"]] <- mat
         if("time" %in% names(.GatherListNS))
            .GatherListNS[["time"]] <- mat
      }
  }
  return(invisible(list(show=.GatherList, noshow=.GatherListNS)))
}

.showallNamesDiagnosticList <- function(liste,.depth=1){
   nms <- names(liste)
   for(item in seq(liste)){
       nms.depthr <- NULL
	   if(is.list(liste[[item]]))
        nms.depthr <- .showallNamesDiagnosticList(liste[[item]],.depth=.depth+1)
	   nms<- unique(c(nms,nms.depthr))
   }
   return(nms)
}

############################################################################
##########   functions to operate on diagnostic information
############################################################################


print.DiagnosticClass <- function(x, what, withNonShows = FALSE, ...){
   if(missing(what)) what <- .showallNamesDiagnosticList(x)
   xn <- paste(deparse(substitute(x)))
   Diagtitle <- gettext("Diagnostic Information to Integrations in Object ")
   underl <- paste(rep("=",nchar(Diagtitle)+3+nchar(xn)),collapse="")
   cat("\n", underl,"\n", Diagtitle, "\"", xn,"\"\n", underl, "\n\n", sep="")
   cat(gettext("The diagnostic has information to the following names:\n\n"))
   nms <- .showallNamesDiagnosticList(x)
   print(nms, ...)
   cat("\n")
   res <- .reorganizeDiagnosticList(x, names0=what, withprint=FALSE)
   diaglistsShow <- res$show
   sel <- names(diaglistsShow) %in% what
   diaglistsShow <- diaglistsShow[sel]
   for(item in seq(diaglistsShow)){
      cat(gettext("Diagnostic information on item \""),
           names(diaglistsShow)[item],"\":\n\n", sep="")
      if(names(diaglistsShow)[item]=="call"){
         cat("Calls: \n")
         print(names(diaglistsShow[[item]]),...)
      }else print(diaglistsShow[[item]], ...)
      cat("\n")
   }
   if(withNonShows){
      diaglistsNoShow <- res$noshow
      sel <- names(diaglistsNoShow) %in% what
      diaglistsNoShow <- diaglistsNoShow[sel]
      for(item in seq(diaglistsNoShow)){
          cat(gettext("Diagnostic information on item \""),
               names(diaglistsNoShow)[item],"\":", sep="")
          if(names(diaglistsNoShow)[item]=="call"){
             cat("\n\n",  gettext("Calls"), ": \n", sep="")
             print(names(diaglistsNoShow[[item]]), ...)
          }else{
             if(names(diaglistsNoShow)[item]=="args"){
                cat("\n\n", gettext("args"), ": \n", sep="")
                print(names(diaglistsNoShow[[item]]), ...)
             }else cat(" ",gettext("skipped"), "\n", sep="")
          }
          cat("\n")
      }
   }
   cat(underl,"\n", gettext("-- end of diagnostic --\n"), underl,"\n\n",sep="")
   res <- c(res$show,res$noshow)
   res <- res[what]
   return(invisible(res))
}

showDiagnostic <- function(x, what, withNonShows = FALSE, ...){
   diagn <- attr(x,"diagnostic")
   diagnKStep <- attr(x,"kStepDiagnostic")
   if(!is.null(diagnKStep)){
      if(is.null(diagn)){
         diagn <- list(kStep=diagnKStep)
         class(diagn) <- "DiagnosticClass"
      }else{
         diagn <- c(diagn, kStep=diagnKStep)
         class(diagn) <- "DiagnosticClass"
      }
   }
   if(is.null(diagn)) return(invisible(NULL))
   if(missing(what)) what <- .showallNamesDiagnosticList(diagn)
   res <- print(diagn, what = what, withNonShows=withNonShows, ...)
   return(invisible(res))
}

getDiagnostic<- function(x, what, reorganized=TRUE){
   diagn <- attr(x,"diagnostic")
   diagnKStep <- attr(x,"kStepDiagnostic")
   if(!is.null(diagnKStep)){
      if(is.null(diagn)){
         diagn <- list(kStep=diagnKStep)
         class(diagn) <- "DiagnosticClass"
      }else{
         diagn <- c(diagn, kStep=diagnKStep)
         class(diagn) <- "DiagnosticClass"
      }
   }
   if(!reorganized) return(invisible(diagn))
   if(missing(what)){ what <- ""; toSel <- .nmsToGather
               }else{ toSel <- what }
   diagns <- .reorganizeDiagnosticList(diagn, names0=what, withprint=FALSE)
   diagns.s <- diagns$show[names(diagns$show) %in% toSel]
   diagns.ns <- diagns$noshow[names(diagns$noshow) %in% toSel]
   res <- c(diagns.s,diagns.ns)
   res <- res[what]
   return(invisible(res))
}
