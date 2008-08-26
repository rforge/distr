setAs("MCEstimate", "mle", def = function(from){
      to <- new("mle")
      to@call <- substitute(mle(minuslogl = crit.f, start = startPar), 
                            list(crit.f = from@criterion.fct,
                                 startPar = as.list(from@untransformed.estimate)))
      to@coef <- from@untransformed.estimate
      to@fullcoef <- from@untransformed.estimate
      to@vcov <- if(!is.null(from@untransformed.asvar)) 
                 from@untransformed.asvar/from@samplesize else matrix(NA,1,1)
      to@min <- from@criterion
      to@details <- as.list(c(from@Infos))
      to@method <- from@method
      to@minuslogl <- from@criterion.fct
to})

setMethod("profile", "MCEstimate",
          function (fitted, which = 1:p, maxsteps = 100,
                    alpha = 0.01, zmax = sqrt(qchisq(1 - alpha, 1L)),
                    del = zmax/5, trace = FALSE, ...){
m.mle <- as(fitted,"mle") 
profile(m.mle)
})
