###############################################################################
## Determine estimates by minimizing a given criterion
###############################################################################
MCEstimator <- function(x, ParamFamily, criterion, crit.name, 
                        startPar = NULL, 
                        Infos, trafo = NULL, penalty = 0, validity.check = TRUE,
                        asvar.fct, ...){

    ## preparation: getting the matched call
    es.call <- match.call()
    dots <- match.call(expand.dots = FALSE)$"..."


    ## some checking
    if(!is.numeric(x))
      stop(gettext("'x' has to be a numeric vector"))   
    if(!is(ParamFamily, "ParamFamily"))
      stop(gettext("'ParamFamily' has to be of class 'ParamFamily'"))
    if(!is.function(criterion))
      stop(gettext("'criterion' has to be a function"))

    ## manipulation of the arg list to method mceCalc
    argList <- c(list(x = x, PFam = ParamFamily, criterion = criterion, 
                   startPar = startPar, penalty = penalty))
    if(missing(Infos))      Infos <- NULL
    argList <- c(argList, Infos = Infos)
    if(missing(crit.name)) crit.name <- ""               
    argList <- c(argList, crit.name = crit.name)               
    if(!is.null(dots))      argList <- c(argList, dots)

    ## call to mceCalc
    res0 <- do.call(mceCalc, argList)
    

    ## digesting the results of mceCalc
    res <- .process.meCalcRes(res0, PFam = ParamFamily, 
                              trafo = trafo, 
                              res.name = paste("Minimum", crit.name, 
                                               "estimate", sep=" ", collapse=""), 
                              call = es.call, 
                              asvar.fct = function(ParamFamily, param)
                                  solve(FisherInfo(ParamFamily, param = param)),
                              ...)

    return(res)
}
