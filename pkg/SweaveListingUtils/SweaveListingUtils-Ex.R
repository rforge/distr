pkgname <- "SweaveListingUtils"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('SweaveListingUtils')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("SweaveListingMASK")
### * SweaveListingMASK

flush(stderr()); flush(stdout())

### Name: SweaveListingMASK
### Title: Masking of/by other functions in package "SweaveListingUtils"
### Aliases: SweaveListingMASK MASKING
### Keywords: programming documentation

### ** Examples

SweaveListingMASK()



cleanEx()
nameEx("SweaveListingOptions")
### * SweaveListingOptions

flush(stderr()); flush(stdout())

### Name: SweaveListingOptions
### Title: Function to change the global options of the package
###   'SweaveListingUtils'
### Aliases: SweaveListingOptions SweaveListingoptions
###   getSweaveListingOption Rset Rdset Rin Rout Rcode Rcolor Rbcolor
###   Routcolor RRecomdcolor Rcommentcolor pkg pkv Keywordstyle
###   Recomd.Keywordstyle interm.Keywordstyle intermediate overwrite
###   inSweave fromRForge base.url addRset addRdset fileCommand pkgCommand
### Keywords: misc utilities

### ** Examples

SweaveListingOptions()
SweaveListingOptions("Rout")
SweaveListingOptions("Rout" = c(0,0,0))
# or
SweaveListingOptions(Rout = c(0,1,0))
getSweaveListingOption("Rout")



cleanEx()
nameEx("SweaveListingPreparations")
### * SweaveListingPreparations

flush(stderr()); flush(stdout())

### Name: SweaveListingPreparations
### Title: SweaveListingPreparations
### Aliases: SweaveListingPreparations
### Keywords: utilities

### ** Examples

SweaveListingPreparations()
SweaveListingPreparations(pkv="2.1")



cleanEx()
nameEx("changeKeywordstyles")
### * changeKeywordstyles

flush(stderr()); flush(stdout())

### Name: changeKeywordstyles
### Title: changeKeywordstyles
### Aliases: changeKeywordstyles
### Keywords: utilities

### ** Examples

require(MASS)
lstsetLanguage(pkgs = c("MASS","stats"),
               keywordstyles  = paste("\\bfseries\\color{",c("blue","red"),"}",
                         sep="", collapse=""))
changeKeywordstyles(pkgs = c("distr","distrEx"),
                    keywordstyles = paste("\\bfseries\\color{",c("green","blue"),"}",
                    collapse="", sep = ""))



cleanEx()
nameEx("copySourceFromRForge")
### * copySourceFromRForge

flush(stderr()); flush(stdout())

### Name: copySourceFromRForge
### Title: copySourceFromRForge
### Aliases: copySourceFromRForge
### Keywords: utilities

### ** Examples

copySourceFromRForge("distr","R","AllClasses.R","distr", from =2, to =3,
                     offset.after=2)
copySourceFromRForge("distr","R","AllClasses.R","distr", from ="setClass",
                      to ="\\}")



cleanEx()
nameEx("isBaseOrRecommended")
### * isBaseOrRecommended

flush(stderr()); flush(stdout())

### Name: isBaseOrRecommended
### Title: isBaseOrRecommended
### Aliases: isBaseOrRecommended
### Keywords: utilities internal

### ** Examples

isBaseOrRecommended(pkgs = c("SweaveListingUtils","Matrix","splines"))


cleanEx()
nameEx("library")
### * library

flush(stderr()); flush(stdout())

### Name: library
### Title: Loading Packages with registering symbols for TeX package
###   'listing'
### Aliases: library require
### Keywords: utilities

### ** Examples

require(survival)



cleanEx()
nameEx("lstinputSourceFromRForge")
### * lstinputSourceFromRForge

flush(stderr()); flush(stdout())

### Name: lstinputSourceFromRForge
### Title: lstinputSourceFromRForge
### Aliases: lstinputSourceFromRForge
### Keywords: utilities

### ** Examples

lstinputSourceFromRForge("distr","R","AllClasses.R","distr",
                     "## Class: BinomParameter", "#-")

lstinputSourceFromRForge("distr","R","AllClasses.R","distr",
                     from = "## Class: binomial distribution",
                     to = "contains = \"LatticeDistribution\"", offset.after = 1)
lstinputSourceFromRForge("distr","man","Binom-class.Rd","distr")

lstinputSourceFromRForge("distr","R","BinomialDistribution.R","distr",
                     from = c("## Access Methods", "## wrapped access methods"),
                     to = c("setReplaceMethod\\(\"prob\", \"BinomParameter\"",
                            "size = value\\)\\)") ,
                     offset.after = c(1,1))
lstinputSourceFromRForge("distr","R","BinomialDistribution.R","distr",
                     from = c(8,43,45), to = c(16,53,45))
lstinputSourceFromRForge("distr","R","BinomialDistribution.R","distr",
                     from = c("## Access Methods", "## wrapped access methods"),
                     to = c("setReplaceMethod\\(\"prob\", \"BinomParameter\"",
                            "size = value\\)\\)") ,
                     offset.after = c(1,1))



cleanEx()
nameEx("lstset")
### * lstset

flush(stderr()); flush(stdout())

### Name: lstset
### Title: lstset and friends
### Aliases: lstset lstsetRd lstsetR lstsetRin lstsetRout lstsetRcode
###   lstdefRstyle lstsetRall
### Keywords: utilities

### ** Examples

lstset(taglist(A="H", b=2, 3),30)
lstset(taglist(A="H", b=2, 3),30, startS = "\\lstdefinestyle{Rstyle}{")
lstsetR()
lstsetRd()



cleanEx()
nameEx("lstsetLanguage")
### * lstsetLanguage

flush(stderr()); flush(stdout())

### Name: lstsetLanguage
### Title: lstsetLanguage
### Aliases: lstsetLanguage
### Keywords: utilities

### ** Examples

require(MASS)
lstsetLanguage(pkgs = c("MASS","stats"),
               keywordstyles  = paste("\\bfseries\\color{",c("blue","red"),"}",
                         sep="", collapse=""))
### not to be used:
print(SweaveListingUtils:::.alreadyDefinedPkgs)
print(SweaveListingUtils:::.keywordsR)



cleanEx()
nameEx("readPackageVersion")
### * readPackageVersion

flush(stderr()); flush(stdout())

### Name: readPkgVersion
### Title: readPkgVersion
### Aliases: readPkgVersion
### Keywords: utilities

### ** Examples

readPkgVersion(package = "distr")



cleanEx()
nameEx("readSourceFromRForge")
### * readSourceFromRForge

flush(stderr()); flush(stdout())

### Name: readSourceFromRForge
### Title: readSourceFromRForge
### Aliases: readSourceFromRForge
### Keywords: utilities

### ** Examples

readSourceFromRForge("distr","R","AllClasses.R","distr")



cleanEx()
nameEx("setToBeDefinedPkgs")
### * setToBeDefinedPkgs

flush(stderr()); flush(stdout())

### Name: setToBeDefinedPkgs
### Title: setToBeDefinedPkgs
### Aliases: setToBeDefinedPkgs
### Keywords: utilities

### ** Examples

setToBeDefinedPkgs(pkgs = c("distr","distrEx"),
                   keywordstyles = paste("\\bfseries\\color{",c("blue","red"),"}",
                         sep="", collapse=""))
### not to be used:
print(SweaveListingUtils:::.tobeDefinedPkgs)



cleanEx()
nameEx("taglist")
### * taglist

flush(stderr()); flush(stdout())

### Name: taglist
### Title: S3 class taglist
### Aliases: print.taglist taglist
### Keywords: utilities

### ** Examples

 TL <- taglist("HA"=8,"JUI"=7,"butzi", list=list("HU"="AHAL","HA"="BETA","BUZ"))
 print(TL)
 print(TL, LineLength=10, first.print="myList=", offset.start=4,
           withFinalLineBreak = FALSE)
 


### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
