## Determine estimator by minimizing a given criterion
MCEstimator <- function(x, ParamFamily, criterion, crit.name, interval, par, ...){
    if(!is.numeric(x))
      stop(gettext("'x' has to be a numeric vector"))
    if(!is(ParamFamily, "ParamFamily"))
      stop(gettext("'ParamFamily' has to be of class 'ParamFamily'"))
    if(!is.function(criterion))
      stop(gettext("'criterion' has to be a function"))

    fun <- function(theta, Data, ParamFamily, criterion, ...){
        criterion(Data, modifyParam(ParamFamily)(theta), ...)
    }

    if(dimension(param(ParamFamily)) == 1){
        res <- optimize(f = fun, interval = interval, Data = x, 
                      ParamFamily = ParamFamily, criterion = criterion, ...)
        theta <- res$minimum
        names(theta) <- names(main(ParamFamily))
        crit <- res$objectiv
    }else{
        if(missing(par)) par <- main(ParamFamily)
	if(inherits(par,"Estimate")) par <- par$estimate
        res <- optim(par = par, fn = fun, Data = x, ParamFamily = ParamFamily, 
                      criterion = criterion, ...)
        theta <- res$par
        names(theta) <- names(main(ParamFamily))
        crit <- res$value
        if(missing(crit.name))
          names(crit) <- as.character(match.call()$criterion)
        else
          names(crit) <- crit.name
    }

    structure(list("estimate" = theta, "criterion" = crit), class = c("MCEstimate", "Estimate"))
}

## print method for objects of class Estimate
print.Estimate <- function(x, digits = getOption("digits"), ...){
  print(x$estimate)
}
