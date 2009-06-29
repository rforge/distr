.onLoad <- function(lib, pkg){
    require("methods", character = TRUE, quietly = TRUE) 
    require("distrEx")
    require("distrSim")
    require(setRNG)
}


.distrEllipseoptions <- list(
                ## nothing up to now...
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
