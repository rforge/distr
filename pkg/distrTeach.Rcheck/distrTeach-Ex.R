### * <HEADER>
###
attach(NULL, name = "CheckExEnv")
assign("nameEx", 
       local({
	   s <- "__{must remake R-ex/*.R}__"
           function(new) {
               if(!missing(new)) s <<- new else s
           }
       }),
       pos = "CheckExEnv")
## Add some hooks to label plot pages for base and grid graphics
assign("base_plot_hook",
       function() {
           pp <- par(c("mfg","mfcol","oma","mar"))
           if(all(pp$mfg[1:2] == c(1, pp$mfcol[2]))) {
               outer <- (oma4 <- pp$oma[4]) > 0; mar4 <- pp$mar[4]
               mtext(sprintf("help(\"%s\")", nameEx()), side = 4,
                     line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
              outer = outer, adj = 1, cex = .8, col = "orchid", las=3)
           }
       },
       pos = "CheckExEnv")
assign("grid_plot_hook",
       function() {
           pushViewport(viewport(width=unit(1, "npc") - unit(1, "lines"),
                                 x=0, just="left"))
           grid.text(sprintf("help(\"%s\")", nameEx()),
                     x=unit(1, "npc") + unit(0.5, "lines"),
                     y=unit(0.8, "npc"), rot=90,
                     gp=gpar(col="orchid"))
       },
       pos = "CheckExEnv")
setHook("plot.new",     get("base_plot_hook", pos = "CheckExEnv"))
setHook("persp",        get("base_plot_hook", pos = "CheckExEnv"))
setHook("grid.newpage", get("grid_plot_hook", pos = "CheckExEnv"))
assign("cleanEx",
       function(env = .GlobalEnv) {
	   rm(list = ls(envir = env, all.names = TRUE), envir = env)
           RNGkind("default", "default")
	   set.seed(1)
   	   options(warn = 1)
	   .CheckExEnv <- as.environment("CheckExEnv")
	   delayedAssign("T", stop("T used instead of TRUE"),
		  assign.env = .CheckExEnv)
	   delayedAssign("F", stop("F used instead of FALSE"),
		  assign.env = .CheckExEnv)
	   sch <- search()
	   newitems <- sch[! sch %in% .oldSearch]
	   for(item in rev(newitems))
               eval(substitute(detach(item), list(item=item)))
	   missitems <- .oldSearch[! .oldSearch %in% sch]
	   if(length(missitems))
	       warning("items ", paste(missitems, collapse=", "),
		       " have been removed from the search path")
       },
       pos = "CheckExEnv")
assign("ptime", proc.time(), pos = "CheckExEnv")
grDevices::postscript("distrTeach-Ex.ps")
assign("par.postscript", graphics::par(no.readonly = TRUE), pos = "CheckExEnv")
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"), pager="console")
options(warn = 1)    
library('distrTeach')

assign(".oldSearch", search(), pos = 'CheckExEnv')
assign(".oldNS", loadedNamespaces(), pos = 'CheckExEnv')
cleanEx(); nameEx("IllustCLT")
### * IllustCLT

flush(stderr()); flush(stdout())

### Name: illustrateCLT
### Title: Functions for Illustrating the CLT
### Aliases: illustrateCLT illustrateCLT.tcl
### Keywords: distribution methods dynamic

### ** Examples

distroptions("DefaultNrFFTGridPointsExponent" = 13)
illustrateCLT(Distr = Unif(), len = 20)
distroptions("DefaultNrFFTGridPointsExponent" = 12)
illustrateCLT(Distr = Pois(lambda = 2), len = 20)
distroptions("DefaultNrFFTGridPointsExponent" = 13)
illustrateCLT(Distr = Pois(lambda = 2)+Unif(), len = 20)
illustrateCLT.tcl(Distr = Unif(), k = 4, "Unif()")



cleanEx(); nameEx("IllustLLN")
### * IllustLLN

flush(stderr()); flush(stdout())

### Name: illustrateLLN
### Title: Functions for Illustrating the LLN
### Aliases: illustrateLLN
### Keywords: distribution methods dynamic

### ** Examples

illustrateLLN(Distr = Unif())
illustrateLLN(Distr = Pois(lambda = 2))
illustrateLLN(Distr = Pois(lambda = 2)+Unif())
illustrateLLN(Td(3), m = 50, col.Eline = "green", lwd = 2, cex = 0.6, main = 
 "My LLN %C%Q", sub = "generated %D")
illustrateLLN(Td(3), m = 50, CLTorCheb = "Chebyshev") 
illustrateLLN(Td(3), m = 50, CLTorCheb = "Chebyshev", coverage = 0.75) 



cleanEx(); nameEx("plotCLT")
### * plotCLT

flush(stderr()); flush(stdout())

### Name: plotCLT
### Title: Generic Plot Function for Illustrating the CLT
### Aliases: plotCLT plotCLT-methods plotCLT,AbscontDistribution-method
###   plotCLT,DiscreteDistribution-method
### Keywords: internal methods hplot distribution

### ** Examples

illustrateCLT(Distr = Unif(), len = 20)



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
