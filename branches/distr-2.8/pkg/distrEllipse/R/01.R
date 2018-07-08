.onLoad <- function(lib, pkg){
#    require("methods", character = TRUE, quietly = TRUE) 
#    require("distrEx")
#    require("distrSim")
#    require(setRNG)
}


.distrEllipseoptions <- list(
                Nsim = 2000,
                withED = TRUE,
                lwd.Ed = 2,
                col.Ed = c(3,4),
                withMean = TRUE,
                cex.mean = 2,
                pch.mean = 20,
                col.mean = 2
                      )
  

.onAttach <- function(library, pkg)
{
  unlockBinding(".distrEllipseoptions", asNamespace("distrEllipse"))
    msga <- gettext(
    "Some functions from package 'stats' are intentionally masked ---see distrEllipseMASK().\n"
                   )
  buildStartupMessage(pkg="distrEllipse", msga, packageHelp=TRUE, library=library, 
               #     MANUAL="http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf",
  VIGNETTE = gettext("Package \"distrDoc\" provides a vignette to this package as well as to several related packages; try vignette(\"distr\")."))
###
  invisible()
}

distrEllipseMASK <- function(library = NULL) 
{
    infoShow(pkg = "distrEllipse", filename="MASKING", library = library)
}

#------------------------------------
#### utilities copied from package distr v.2.6  svn-rev 943
#------------------------------------
.isInteger  <- function(x, tol = .Machine$double.eps) abs(as.integer(x)-x)< tol
.isNatural  <- function(x, tol = .Machine$double.eps) .isInteger(x, tol) & (x>0)

.inArgs <- function(arg, fct)
          {as.character(arg) %in% names(formals(fct))}

.isEqual <- function(p0, p1, tol = min( getdistrOption("TruncQuantile")/2,
                                          .Machine$double.eps^.7
                                          ))
                abs(p0-p1)< tol

#------------------------------------------------------------------------------
# issue warnings in show / print as to Arith or print
#------------------------------------------------------------------------------
.IssueWarn <- function(Arith,Sim){
    msgA1 <- msgA2 <- msgS1 <- msgS2 <- NULL
    if(Arith && getdistrOption("WarningArith")){
      msgA1 <- gettext(
       "arithmetics on distributions are understood as operations on r.v.'s\n")
      msgA2 <- gettext(
       "see 'distrARITH()'; for switching off this warning see '?distroptions'")
       }
    if(Sim && getdistrOption("WarningSim")){
      msgS1 <- gettext(
       "slots d,p,q have been filled using simulations; ")
      msgS2 <- gettext(
       "for switching off this warning see '?distroptions'")
       }
    return(list(msgA=c(msgA1,msgA2), msgS = c(msgS1,msgS2)))
    }
