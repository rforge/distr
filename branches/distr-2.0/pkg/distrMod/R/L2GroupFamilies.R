##################################################################
## L2 location family
##################################################################
L2LocationFamily <- function(loc = 0, scale = 1, name, centraldistribution = Norm(),
                             LogDeriv = function(x)x, L2derivDistr.0,
                             FisherInfo.0, 
                             distrSymm, L2derivSymm, L2derivDistrSymm,
                             trafo, ...){
    if(missing(name))
       name <- "L2 location family"

    distribution <- scale*centraldistribution+loc
 
    if(missing(distrSymm))
         {distrSymm <- SphericalSymmetry(SymmCenter = loc)
    }else{
        if(!is(distrSymm, "NoSymmetry")){
            if(!is(distrSymm@SymmCenter, "numeric"))
                stop("slot 'SymmCenter' of 'distrSymm' has to be of class 'numeric'")
            if(length(distrSymm@SymmCenter) != 1)
                stop("slot 'SymmCenter' of 'distrSymm' has wrong dimension")
                    }
         }      
 
    param <- ParamFamParameter(name = "location", main = loc, trafo = trafo)
    modifyParam <- function(theta){}
    body(modifyParam) <- substitute({ scale*centraldistribution+theta },
                                      list(scale = scale))
    props <- c(paste("The", name, "is invariant under"),
               "the group of transformations 'g(x) = x + loc'",
               "with location parameter 'loc'")
    L2deriv.fct <- function(param){
                   loc <- main(param)
                   fct <- function(x){}
                   body(fct) <- substitute({ LogDeriv((x - loc)/scale)/scale },
                                             list(loc = loc, scale = scale))
                   return(fct)}
    L2deriv <- EuclRandVarList(RealRandVariable(list(L2deriv.fct(param)), Domain = Reals())) 
    
    if(missing (L2derivSymm)) 
         {L2derivSymm <- FunSymmList(OddSymmetric(SymmCenter = loc))
    }else{
          if(!length(L2derivSymm) == 1) 
              stop("wrong length of argument L2derivSymm")
         }      
       
    L2derivDistr <- if(missing(L2derivDistr.0))
        imageDistr(RandVar = L2deriv, distr = distribution) else 
        UnivarDistrList(L2derivDistr.0)

    if(missing (L2derivDistrSymm)) 
         {L2derivDistrSymm <- DistrSymmList(SphericalSymmetry(SymmCenter = 0))
    }else{
          if(!length(L2derivSymm) == 1) 
              stop("wrong length of argument L2derivSymm")
         }      
    
    FI0 <- if(missing(FisherInfo.0))
           E(centraldistribution, fun = function(x) LogDeriv(x)^2,
             useApply = FALSE, ...) else FisherInfo.0
             
    FisherInfo.fct <- function(param) PosDefSymmMatrix(FI0/scale^2)

    L2Fam <- new("L2LocationFamily")
    L2Fam@name <- name
    L2Fam@distribution <- distribution
    L2Fam@distrSymm <- distrSymm
    L2Fam@param <- param
    L2Fam@modifyParam <- modifyParam
    L2Fam@props <- props
    L2Fam@LogDeriv <- LogDeriv
    L2Fam@L2deriv.fct <- L2deriv.fct
    L2Fam@L2deriv <- L2deriv
    L2Fam@L2derivSymm <- L2derivSymm
    L2Fam@L2derivDistr <- L2derivDistr
    L2Fam@L2derivDistrSymm <- L2derivDistrSymm
    L2Fam@FisherInfo.fct <- FisherInfo.fct
    L2Fam@FisherInfo <- FisherInfo.fct(param)

    return(L2Fam)
}


##################################################################
## L2 scale family
##################################################################
L2ScaleFamily <- function(scale = 1, loc = 0, name, centraldistribution = Norm(),
                             LogDeriv = function(x)x ,L2derivDistr.0,
                             FisherInfo.0, 
                             distrSymm, L2derivSymm, L2derivDistrSymm,
                             trafo, ...){
    if(missing(name))
       name <- "L2 scale family"

    distribution <- scale*centraldistribution+loc
 
    if(missing(distrSymm))
         {distrSymm <- SphericalSymmetry(SymmCenter = loc)
    }else{
          if(!is(distrSymm, "NoSymmetry")){
            if(!is(distrSymm@SymmCenter, "numeric"))
                stop("slot 'SymmCenter' of 'distrSymm' has to be of class 'numeric'")
            if(length(distrSymm@SymmCenter) != 1)
                stop("slot 'SymmCenter' of 'distrSymm' has wrong dimension")
                    }
         }      
    param <- ParamFamParameter(name = "scale", main = scale, trafo = trafo)
    modifyParam <- function(theta){}
    body(modifyParam) <- substitute({ theta*centraldistribution+loc },
                                      list(loc = loc))
    props <- c(paste("The", name, "is invariant under"),
               "the group of transformations 'g(y) = scale*y'",
               "with scale parameter 'scale'")
    L2deriv.fct <- function(param){
                   scale <- main(param)
                   fct <- function(x){}
                   body(fct) <- substitute({ ((x - loc)/scale*LogDeriv((x - loc)/scale)-1)/scale },
                                             list(loc = loc, scale = scale))
                   return(fct)}
    L2deriv <- EuclRandVarList(RealRandVariable(list(L2deriv.fct(param)), Domain = Reals())) 

    if(missing (L2derivSymm)) 
         {L2derivSymm <- FunSymmList(EvenSymmetric(SymmCenter = mean))
    }else{
          if(!length(L2derivSymm) == 1) 
              stop("wrong length of argument L2derivSymm")
         }      

    L2derivDistr <- if(missing(L2derivDistr.0))
        imageDistr(RandVar = L2deriv, distr = distribution) else 
        UnivarDistrList(L2derivDistr.0)

    if(missing (L2derivDistrSymm)) 
         {L2derivDistrSymm <- DistrSymmList(NoSymmetry())
    }else{
          if(!length(L2derivSymm) == 1) 
              stop("wrong length of argument L2derivSymm")
         }      

    FI0 <- if(missing(FisherInfo.0)) 
           E(centraldistribution, fun = function(x) (x*LogDeriv(x)-1)^2,
             useApply = FALSE, ...) else FisherInfo.0
    FisherInfo.fct <- function(param){
                   scale <- main(param)
                   PosDefSymmMatrix(FI0/scale^2)}

    L2Fam <- new("L2ScaleFamily")
    L2Fam@name <- name
    L2Fam@distribution <- distribution
    L2Fam@distrSymm <- distrSymm
    L2Fam@param <- param
    L2Fam@modifyParam <- modifyParam
    L2Fam@props <- props
    L2Fam@LogDeriv <- LogDeriv
    L2Fam@L2deriv.fct <- L2deriv.fct
    L2Fam@L2deriv <- L2deriv
    L2Fam@L2derivSymm <- L2derivSymm
    L2Fam@L2derivDistr <- L2derivDistr
    L2Fam@L2derivDistrSymm <- L2derivDistrSymm
    L2Fam@FisherInfo.fct <- FisherInfo.fct
    L2Fam@FisherInfo <- FisherInfo.fct(param)

    return(L2Fam)
}


##################################################################
## L2 location and scale family
##################################################################
L2LocationScaleFamily <- function(loc = 0, scale = 1, name, 
                             centraldistribution = Norm(),
                             LogDeriv = function(x)x, L2derivDistr.0,
                             FisherInfo.0, 
                             distrSymm, L2derivSymm, L2derivDistrSymm,
                             trafo, ...){
    if(missing(name))
       name <- "L2 location and scale family"

    distribution <- scale*centraldistribution+loc
   
    if(missing(distrSymm))
         {distrSymm <- SphericalSymmetry(SymmCenter = loc)
    }else{
          if(!is(distrSymm, "NoSymmetry")){
            if(!is(distrSymm@SymmCenter, "numeric"))
                stop("slot 'SymmCenter' of 'distrSymm' has to be of class 'numeric'")
            if(length(distrSymm@SymmCenter) != 1)
                stop("slot 'SymmCenter' of 'distrSymm' has wrong dimension")
                    }
         }      
    param <- ParamFamParameter(name = "location and scale", main = c(loc, scale),
                               trafo = trafo)
    modifyParam <- function(theta){theta[2]*centraldistribution+theta[1] }
    props <- c(paste("The", name, "is invariant under"),
               "the group of transformations 'g(x) = scale*x + loc'",
               "with location parameter 'loc' and scale parameter 'scale'")

    L2deriv.fct <- function(param){
                   mean <- main(param)[1]
                   sd <-   main(param)[2]
                   fct1 <- function(x){}
                   fct2 <- function(x){}
                   body(fct1) <- substitute({ LogDeriv((x - loc)/scale)/scale },
                                             list(loc = loc, scale = scale))
                   body(fct2) <- substitute({ 
                        ((x - loc)/scale * LogDeriv((x - loc)/scale)-1)/scale },
                                             list(loc = loc, scale = scale))
                   return(list(fct1, fct2))}

    L2deriv <- EuclRandVarList(RealRandVariable(L2deriv.fct(param), 
                               Domain = Reals())) 
    
    if(missing (L2derivSymm)) 
         {L2derivSymm <- FunSymmList(OddSymmetric(SymmCenter = loc),
                               EvenSymmetric(SymmCenter = loc))
    }else{
          if(!length(L2derivSymm) == 2) 
              stop("wrong length of argument L2derivSymm")
         }      

    L2derivDistr <- if (missing(L2derivDistr.0))
         imageDistr(RandVar = L2deriv, distr = distribution) else
         UnivarDistrList(L2derivDistr.0[[1]],L2derivDistr.0[[2]])

    if(missing (L2derivDistrSymm)) 
         {L2derivDistrSymm <- DistrSymmList(SphericalSymmetry(), NoSymmetry())
    }else{
          if(!length(L2derivSymm) == 1) 
              stop("wrong length of argument L2derivSymm")
         }      

    if(missing(FisherInfo.0)) 
      { FI11 <- E(centraldistribution, fun = function(x) LogDeriv(x)^2,
               useApply = FALSE, ...)
        FI22 <- E(centraldistribution, fun = function(x) (x*LogDeriv(x)-1)^2,
               useApply = FALSE, ...)
        if( is(distrSymm, "SphericalSymmetry") )               
           { FI12 <- 0
           } else {
             FI12 <- E(centraldistribution, fun = function(x) x*LogDeriv(x)^2,
                       useApply = FALSE, ...)           
           }   
        FI0 <- matrix(c(FI11,FI12,FI12,FI22),2,2)
      } else{ 
        FI0 <- FisherInfo.0 
      }
             
    FisherInfo.fct <- function(param){
                   scale <- main(param)[2]
                   PosDefSymmMatrix(FI0/scale^2)}

    L2Fam <- new("L2LocationScaleFamily")
    L2Fam@name <- name
    L2Fam@distribution <- distribution
    L2Fam@distrSymm <- distrSymm
    L2Fam@param <- param
    L2Fam@modifyParam <- modifyParam
    L2Fam@props <- props
    L2Fam@LogDeriv <- LogDeriv
    L2Fam@L2deriv.fct <- L2deriv.fct
    L2Fam@L2deriv <- L2deriv
    L2Fam@L2derivSymm <- L2derivSymm
    L2Fam@L2derivDistr <- L2derivDistr
    L2Fam@L2derivDistrSymm <- L2derivDistrSymm
    L2Fam@FisherInfo.fct <- FisherInfo.fct
    L2Fam@FisherInfo <- FisherInfo.fct(param)

    return(L2Fam)
}
