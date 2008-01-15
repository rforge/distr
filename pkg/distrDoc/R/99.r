.First.lib <- function(libname, pkgname)
{                                                                    
    if (.Platform$OS.type == "windows" && require("Biobase")
        && interactive() && .Platform$GUI == "Rgui") 
        addVigs2WinMenu("distrDoc")  

buildStartupMessage(pkg="distrDoc",  library=libname, 
                    packageHelp=TRUE, # MANUAL="http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf",
                    VIGNETTE=gettext("This package provides a vignette to package \"distr\"\nand to several related packages; try vignette(\"distr\")."))
  invisible()
} 
