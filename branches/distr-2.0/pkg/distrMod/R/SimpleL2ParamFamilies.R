##################################################################
## Binomial family
##################################################################
BinomFamily <- function(size = 1, prob = 0.5, trafo){ 
    name <- "Binomial family"
    distribution <- Binom(size = size, prob = prob)
    if(prob == 0.5)
        distrSymm <- SphericalSymmetry(SymmCenter = size*prob)
    else
        distrSymm <- NoSymmetry()
    param0 <- prob
    names(param0) <- "prob"
    param <- ParamFamParameter(name = "probability of success",  
                               main = param0, trafo = trafo)
    modifyParam <- function(theta){ Binom(size = size, prob = theta) }
    body(modifyParam) <- substitute({ Binom(size = size, prob = theta) }, list(size = size))
    props <- c("The Binomial family is symmetric with respect to prob = 0.5;", 
               "i.e., d(Binom(size, prob))(k)=d(Binom(size,1-prob))(size-k)")
    
    L2deriv.fct <- function(param){
                   prob <- main(param)
                   fct <- function(x){}
                   body(fct) <- substitute({ (x-size*prob)/(prob*(1-prob)) },
                                list(size = size, prob = prob))
                   return(fct)}
    L2derivSymm <- FunSymmList(OddSymmetric(SymmCenter = size*prob)) 
    L2derivDistr <- UnivarDistrList((distribution - size*prob)/(prob*(1-prob)))
    if(prob == 0.5)
        L2derivDistrSymm <- DistrSymmList(SphericalSymmetry(SymmCenter = 0))
    else
        L2derivDistrSymm <- DistrSymmList(NoSymmetry())
    FisherInfo.fct <- function(param){
                       prob <- main(param)
                       PosDefSymmMatrix(matrix(size/(prob*(1-prob))))}

    L2ParamFamily(name = name, distribution = distribution, 
        distrSymm = distrSymm, param = param, modifyParam = modifyParam,
        props = props, L2deriv.fct = L2deriv.fct, L2derivSymm = L2derivSymm,
        L2derivDistr = L2derivDistr, L2derivDistrSymm = L2derivDistrSymm,
        FisherInfo.fct = FisherInfo.fct)
}

##################################################################
## Poisson family
##################################################################
PoisFamily <- function(lambda = 1, trafo){ 
    name <- "Poisson family"
    distribution <- Pois(lambda = lambda)
    distrSymm <- NoSymmetry()
    param0 <- lambda
    names(param0) <- "lambda"
    param <- ParamFamParameter(name = "positive mean",
                               main = param0, trafo = trafo)
    modifyParam <- function(theta){ Pois(lambda = theta) }
    props <- character(0)
    L2deriv.fct <- function(param){
                   lambda <- main(param)
                   fct <- function(x){}
                   body(fct) <- substitute({ x/lambda-1 }, 
                                list(lambda = lambda))
                   return(fct)}
    L2derivSymm <- FunSymmList(OddSymmetric(SymmCenter = lambda))
    L2derivDistr <- UnivarDistrList(distribution/lambda - 1)
    L2derivDistrSymm <- DistrSymmList(NoSymmetry())
    FisherInfo.fct <- function(param){
                   lambda <- main(param)
                   PosDefSymmMatrix(matrix(1/lambda))}

    L2ParamFamily(name = name, distribution = distribution, 
        distrSymm = distrSymm, param = param, modifyParam = modifyParam,
        props = props, L2deriv.fct = L2deriv.fct, L2derivSymm = L2derivSymm,
        L2derivDistr = L2derivDistr, L2derivDistrSymm = L2derivDistrSymm,
        FisherInfo.fct = FisherInfo.fct)
}

##################################################################
## Gamma family
##################################################################
GammaFamily <- function(scale = 1, shape = 1, trafo){ 
    name <- "Gamma family"
    distribution <- Gammad(scale = scale, shape = shape)
    distrSymm <- NoSymmetry()
    param0 <- c(scale, shape)
    names(param0) <- c("scale", "shape")
    param <- ParamFamParameter(name = "scale and shape",  
                        main = param0, trafo = trafo)
    modifyParam <- function(theta){ Gammad(scale = theta[1], shape = theta[2]) }
    props <- c("The Gamma family is scale invariant via the parametrization",
               "'(nu,shape)=(log(scale),shape)'")
    L2deriv.fct <- function(param){
                   scale <- main(param)[1]
                   shape <- main(param)[2]
                   fct1 <- function(x){}
                   fct2 <- function(x){}
                   body(fct1) <- substitute({ (x/scale - shape)/scale },
                        list(scale = scale, shape = shape))
                   body(fct2) <- substitute({ (log(x/scale) - digamma(shape)) },
                        list(scale = scale, shape = shape))
                   return(list(fct1, fct2))}
    L2derivSymm <- FunSymmList(OddSymmetric(SymmCenter = scale*shape), NonSymmetric())
    L2derivDistr <- UnivarDistrList((Gammad(scale = 1, shape = shape) - shape)/scale, 
                                         (log(Gammad(scale = 1, shape = shape)) - digamma(shape)))
    L2derivDistrSymm <- DistrSymmList(NoSymmetry(), NoSymmetry())
    FisherInfo.fct <- function(param){
                   scale <- main(param)[1]
                   shape <- main(param)[2]
                   PosDefSymmMatrix(matrix(c(shape/scale^2, 1/scale, 
                                            1/scale, trigamma(shape)), ncol=2))}

    L2ParamFamily(name = name, distribution = distribution, 
        distrSymm = distrSymm, param = param, modifyParam = modifyParam,
        props = props, L2deriv.fct = L2deriv.fct, L2derivSymm = L2derivSymm,
        L2derivDistr = L2derivDistr, L2derivDistrSymm = L2derivDistrSymm,
        FisherInfo.fct = FisherInfo.fct)
}

if(FALSE){

##################################################################
## Normal location family
##################################################################
NormLocationFamily <- function(mean = 0, sd = 1, trafo){ 
    name <- "normal location family"
    distribution <- Norm(mean = mean, sd = sd)
    distrSymm <- SphericalSymmetry(SymmCenter = mean)
    param0 <- mean
    names(param0) <- "mean"
    param <- ParamFamParameter(name = "location", main = param0, trafo = trafo)
    modifyParam <- function(theta){ Norm(mean = theta, sd = sd) }
    body(modifyParam) <- substitute({ Norm(mean = theta, sd = sd) }, list(sd = sd))
    props <- c("The normal location family is invariant under",
               "the group of transformations 'g(x) = x + mean'",
               "with location parameter 'mean'")
    L2deriv.fct <- function(param){
                   mean <- main(param)
                   fct <- function(x){}
                   body(fct) <- substitute({ (x - mean)/sd^2 }, 
                                             list(mean = mean, sd = sd))
                   return(fct)}
    L2derivSymm <- FunSymmList(OddSymmetric(SymmCenter = mean))
    L2derivDistr <- UnivarDistrList(Norm(mean=0, sd=1/sd))
    L2derivDistrSymm <- DistrSymmList(SphericalSymmetry(SymmCenter = 0))
    FisherInfo.fct <- function(param){PosDefSymmMatrix(matrix(1/sd^2))}

    L2ParamFamily(name = name, distribution = distribution, 
        distrSymm = distrSymm, param = param, modifyParam = modifyParam,
        props = props, L2deriv.fct = L2deriv.fct, L2derivSymm = L2derivSymm,
        L2derivDistr = L2derivDistr, L2derivDistrSymm = L2derivDistrSymm,
        FisherInfo.fct = FisherInfo.fct)
}

##################################################################
## Gumbel location family
##################################################################
GumbelLocationFamily <- function(loc = 0, scale = 1, trafo){ 
    name <- "Gumbel location family"
    distribution <- Gumbel(loc = loc, scale = scale)
    distrSymm <- NoSymmetry()
    param0 <- loc
    names(param0) <- "loc"
    param <- ParamFamParameter(name = "location", main = param0, trafo = trafo)
    modifyParam <- function(theta){ Gumbel(loc = theta, scale = scale) }
    body(modifyParam) <- substitute({ Gumbel(loc = theta, scale = scale) }, list(scale = scale))
    props <- c("The Gumbel location family is invariant under",
               "the group of transformations 'g(x) = x + loc'",
               "with location parameter 'loc'")
    L2deriv.fct <- function(param){
                   loc <- main(param)
                   fct <- function(x){}
                   body(fct) <- substitute({ (1 - exp(-(x-loc)/scale))/scale }, 
                         list(loc = loc, scale = scale))
                   return(fct)}
    L2derivSymm <- FunSymmList(NonSymmetric())
    L2derivDistr <- UnivarDistrList((1-Exp(rate=1))/scale)
    L2derivDistrSymm <- DistrSymmList(NoSymmetry())
    FisherInfo.fct <- function(param){PosDefSymmMatrix(matrix(1/scale^2))}

    L2ParamFamily(name = name, distribution = distribution, 
        distrSymm = distrSymm, param = param, modifyParam = modifyParam,
        props = props, L2deriv.fct = L2deriv.fct, L2derivSymm = L2derivSymm,
        L2derivDistr = L2derivDistr, L2derivDistrSymm = L2derivDistrSymm,
        FisherInfo.fct = FisherInfo.fct)
}

##################################################################
## Normal scale family
##################################################################
NormScaleFamily <- function(sd = 1, mean = 0, trafo){ 
    name <- "normal scale family"
    distribution <- Norm(mean = mean, sd = sd)
    distrSymm <- SphericalSymmetry(SymmCenter = mean)
    param0 <- sd
    names(param0) <- "sd"
    param <- ParamFamParameter(name = "scale", main = param0, trafo = trafo)
    modifyParam <- function(theta){ Norm(mean = mean, sd = theta) }
    body(modifyParam) <- substitute({ Norm(mean = mean, sd = theta) }, list(mean = mean))
    props <- c("The normal scale family is invariant under",
               "the group of transformations 'g(y) = sd*y'",
               "with scale parameter 'sd'")
    L2deriv.fct <- function(param){
                   sd <- main(param)
                   fct <- function(x){}
                   body(fct) <- substitute({ (((x-mean)/sd)^2 - 1)/sd }, 
                                list(sd = sd, mean = mean))
                   return(fct)}
    L2derivSymm <- FunSymmList(EvenSymmetric(SymmCenter = mean))
    L2derivDistr <- UnivarDistrList((Chisq(df = 1, ncp = 0)-1)/sd)
    L2derivDistrSymm <- DistrSymmList(NoSymmetry())
    FisherInfo.fct <- function(param){
                   sd <- main(param)
                   PosDefSymmMatrix(matrix(2/sd^2))}

    L2ParamFamily(name = name, distribution = distribution, 
        distrSymm = distrSymm, param = param, modifyParam = modifyParam,
        props = props, L2deriv.fct = L2deriv.fct, L2derivSymm = L2derivSymm,
        L2derivDistr = L2derivDistr, L2derivDistrSymm = L2derivDistrSymm,
        FisherInfo.fct = FisherInfo.fct)
}

##################################################################
## Exponential scale family
##################################################################
ExpScaleFamily <- function(rate = 1, trafo){ 
    name <- "Exponential scale family"
    distribution <- Exp(rate = rate)
    distrSymm <- NoSymmetry()
    param0 <- 1/rate
    names(param0) <- "scale"
    param <- ParamFamParameter(name = "scale", main = param0, trafo = trafo)
    modifyParam <- function(theta){ Exp(rate = 1/theta) }
    props <- c("The Exponential scale family is invariant under",
               "the group of transformations 'g(y) = scale*y'",
               "with scale parameter 'scale = 1/rate'")
    L2deriv.fct <- function(param){
                   rate <- main(param)
                   fct <- function(x){}
                   body(fct) <- substitute({ (rate*x - 1)*rate }, 
                                list(rate = rate))
                   return(fct)}
    L2derivSymm <- FunSymmList(EvenSymmetric(SymmCenter = 1/rate))
    L2derivDistr <- UnivarDistrList((Exp(rate=1)-1)*rate)
    L2derivDistrSymm <- DistrSymmList(NoSymmetry())
    FisherInfo.fct <- function(param){
                   rate <- main(param)
                   PosDefSymmMatrix(matrix(rate^2))}

    L2ParamFamily(name = name, distribution = distribution, 
        distrSymm = distrSymm, param = param, modifyParam = modifyParam,
        props = props, L2deriv.fct = L2deriv.fct, L2derivSymm = L2derivSymm,
        L2derivDistr = L2derivDistr, L2derivDistrSymm = L2derivDistrSymm,
        FisherInfo.fct = FisherInfo.fct)
}

##################################################################
## Lognormal scale family
##################################################################
LnormScaleFamily <- function(meanlog = 0, sdlog = 1, trafo){ 
    name <- "lognormal scale family"
    distribution <- Lnorm(meanlog = meanlog, sdlog = sdlog)
    distrSymm <- NoSymmetry()
    param0 <- exp(meanlog)
    names(param0) <- "scale"
    param <- ParamFamParameter(name = "scale", main = param0, trafo = trafo)
    modifyParam <- function(theta){ Lnorm(meanlog = log(theta), sdlog = sdlog) }
    body(modifyParam) <- substitute({ Lnorm(meanlog = log(theta), sdlog = sdlog) }, list(sdlog = sdlog))
    props <- c("The Lognormal scale family is invariant under",
               "the group of transformations 'g(y) = scale*y'",
               "with scale parameter 'scale = exp(meanlog)'")
    L2deriv.fct <- function(param){
                   meanlog <- log(main(param))
                   fct <- function(x){}
                   body(fct) <- substitute({ exp(-meanlog)*(log(x) - meanlog)/sdlog^2 },
                       list(meanlog = meanlog, sdlog = sdlog))
                   return(fct)}
    L2derivSymm <- FunSymmList(NonSymmetric())
    L2derivDistr <- UnivarDistrList(Norm(mean=0, sd=exp(-meanlog)/sdlog^2))
    L2derivDistrSymm <- DistrSymmList(SphericalSymmetry(SymmCenter = 0))
    FisherInfo.fct <- function(param){
                   meanlog <- log(main(param))
                   PosDefSymmMatrix(matrix(exp(-meanlog)^2/sdlog^2))}

    L2ParamFamily(name = name, distribution = distribution, 
        distrSymm = distrSymm, param = param, modifyParam = modifyParam,
        props = props, L2deriv.fct = L2deriv.fct, L2derivSymm = L2derivSymm,
        L2derivDistr = L2derivDistr, L2derivDistrSymm = L2derivDistrSymm,
        FisherInfo.fct = FisherInfo.fct)
}


##################################################################
## Normal location and scale family
##################################################################
NormLocationScaleFamily <- function(mean = 0, sd = 1, trafo){ 
    name <- "normal location and scale family"
    distribution <- Norm(mean = mean, sd = sd)
    distrSymm <- SphericalSymmetry(SymmCenter = mean)
    param0 <- c(mean, sd)
    names(param0) <- c("mean", "sd")
    param <- ParamFamParameter(name = "location and scale", main = param0, trafo = trafo)
    modifyParam <- function(theta){ Norm(mean = theta[1], sd = theta[2]) }
    props <- c("The normal location and scale family is invariant under",
               "the group of transformations 'g(x) = sd*x + mean'",
               "with location parameter 'mean' and scale parameter 'sd'")
    L2deriv.fct <- function(param){
                   mean <- main(param)[1]
                   sd <-   main(param)[2]
                   fct1 <- function(x){}
                   fct2 <- function(x){}
                   body(fct1) <- substitute({ (x - mean)/sd^2 }, 
                                               list(mean = mean, sd = sd))
                   body(fct2) <- substitute({ (((x-mean)/sd)^2 - 1)/sd }, 
                                              list(sd = sd, mean = mean))
                   return(list(fct1, fct2))}

    L2derivSymm <- FunSymmList(OddSymmetric(SymmCenter = mean), EvenSymmetric(SymmCenter = mean))
    L2derivDistr <- UnivarDistrList(Norm(mean=0, sd=1/sd), (Chisq(df = 1, ncp = 0)-1)/sd)
    L2derivDistrSymm <- DistrSymmList(SphericalSymmetry(), NoSymmetry())
    FisherInfo.fct <- function(param){
                   mean <- main(param)[1]
                   sd <- main(param)[2]
                   PosDefSymmMatrix(matrix(c(1/sd^2, 0, 0, 2/sd^2), ncol=2))}

    L2ParamFamily(name = name, distribution = distribution, 
        distrSymm = distrSymm, param = param, modifyParam = modifyParam, 
        props = props, L2deriv.fct = L2deriv.fct, L2derivSymm = L2derivSymm, 
        L2derivDistr = L2derivDistr, L2derivDistrSymm = L2derivDistrSymm,
        FisherInfo.fct = FisherInfo.fct)
}

}
if(TRUE){
################################################################################
## Group Models with central distribution Norm(0,1)
################################################################################

##################################################################
## Normal location family
##################################################################
NormLocationFamily <- function(mean = 0, sd = 1, trafo){ 
    L2LocationFamily(loc = mean, scale = sd, name = "normal location family", 
                     L2derivDistr.0 = Norm(mean = 0, sd=1/sd),
                     FisherInfo.0 = 1, trafo = trafo)
}

##################################################################
## Normal scale family
##################################################################
NormScaleFamily <- function(sd = 1, mean = 0, trafo){ 
    L2ScaleFamily(loc = mean, scale = sd, name = "normal scale family", 
                  L2derivDistr.0 = (Chisq(df = 1, ncp = 0)-1)/sd,
                  FisherInfo.0 = 2, trafo = trafo)
}

##################################################################
## Normal location and scale family
##################################################################
NormLocationScaleFamily <- function(mean = 0, sd = 1, trafo){ 
    L2LocationScaleFamily(loc = mean, scale = sd, 
              name = "normal location and scale family", 
              L2derivDistr.0 = list( Norm(mean = 0, sd=1/sd), 
                                    (Chisq(df = 1, ncp = 0)-1)/sd),
              FisherInfo.0 = diag(c(1,2)), trafo = trafo)
}

###############################################################################
# other location and / or scale models
###############################################################################

##################################################################
## Exponential scale family
##################################################################
ExpScaleFamily <- function(rate = 1, trafo){ 
    L2ScaleFamily(loc = 0, scale = 1/rate, name = "Exponential scale family", 
                  centraldistribution = Exp(rate = 1),
                  LogDeriv = function(x)  x-1,  
                  L2derivDistr.0 = (Exp(rate = 1)-1)*rate,
                  FisherInfo.0 = 1, 
                  distrSymm = NoSymmetry(), 
                  L2derivSymm = FunSymmList(NonSymmetric()), 
                  L2derivDistrSymm = DistrSymmList(NoSymmetry()),
                  trafo = trafo)
}


##################################################################
## Lognormal scale family
##################################################################
LnormScaleFamily <- function(meanlog = 0, sdlog = 1, trafo){ 
    L2ScaleFamily(loc = 0, scale = exp(meanlog),  
                  name = "lognormal scale family", 
                  centraldistribution = Lnorm(meanlog = 0, sdlog = sdlog),
                  LogDeriv = function(x) log(x)/sdlog^2/x,  
                  L2derivDistr.0 = Norm(mean=0, sd=1/sdlog),
                  FisherInfo.0 = 1/sdlog^2, 
                  distrSymm = NoSymmetry(), 
                  L2derivSymm = FunSymmList(NonSymmetric()), 
                  L2derivDistrSymm <- DistrSymmList(SphericalSymmetry(SymmCenter = 0)),
                  trafo = trafo)
}


##################################################################
## Gumbel location family
##################################################################
GumbelLocationFamily <- function(loc = 0, scale = 1, trafo){ 
    L2LocationFamily(loc = loc, scale = scale, 
                     name = "Gumbel location family", 
                     centraldistribution = Gumbel(loc = 0, scale = scale),
                     LogDeriv = function(x)  (1-exp(-(x-loc)/scale))/scale,  
                     L2derivDistr.0 = (1 - Exp(rate = 1))/scale,
                     FisherInfo.0 = 1/scale^2, 
                     distrSymm = NoSymmetry(), 
                     L2derivSymm = FunSymmList(NonSymmetric()), 
                     L2derivDistrSymm = DistrSymmList(NoSymmetry()),
                     trafo = trafo)
}


##################################################################
## Cauchy location scale family
##################################################################
CauchyLocationScaleFamily <- function(loc = 0, scale = 1, trafo){ 
    L2LocationScaleFamily(loc = loc, scale = scale, 
                  name = "Cauchy Location and scale family", 
                  centraldistribution = Cauchy(),
                  LogDeriv = function(x)  2*x/(x^2+1),  
                  trafo = trafo)
}

}
