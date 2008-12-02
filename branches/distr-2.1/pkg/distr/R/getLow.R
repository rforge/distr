#------------------------------------------------------------------------------
### getLow / getUp methods
#------------------------------------------------------------------------------

 setMethod("getLow", "AbscontDistribution",
            function(object, eps = getdistrOption("TruncQuantile")) {
                 q0 <- q(object)(0)
                 if (q0 > - Inf){
                   return(q0)
                }else{
                   qF <- q(object)
                   qe <- qF(eps)
                   if (!is.na(qe) && qe > -Inf)
                      return(qe)
                   else{
                      if(.inArgs("log.p",qF))
                         return(qF(p = .fm(x = .5, f = q(object)), 
                               log.p = TRUE))
                      else
                         return(qF(p = exp(.fm(x = .5, f = q(object)))))
                   }
                }  
            })
 setMethod("getUp", "AbscontDistribution",
            function(object, eps = getdistrOption("TruncQuantile")) {
                 q1 <- q(object)(1)
                 if (q1 < Inf){
                   return(q1)
                 }else{
                    qF <- q(object)
                    if (.inArgs("lower.tail", qF)){
                          qe <- qF(eps, lower.tail = FALSE)
                          if (!is.na(qe) && qe < Inf)
                               return(qe)
                          else{
                             if(.inArgs("log.p",qF))
                                return(qF(p = .fM2(x = .5, f = q(object), 
                                      log.p = TRUE)))
                             else
                                return(qF(p = exp(.fM2(x = .5, f = q(object)))))
                          }
                      }else{
                          qe <- q(object)(1-eps)
                          if (!is.na(qe) && qe < Inf)
                               return(qe)
                          else{
                             if(.inArgs("log.p",qF))
                               return(qF(p = .fM(x = .5, f = q(object)), 
                                      log.p = TRUE))
                             else
                               return(qF(p = exp(.fM(x = .5, f = q(object))))) 
                          }
                      }
                 }
            })
 setMethod("getLow", "DiscreteDistribution",
            function(object) min(support(object)) )
 setMethod("getUp", "DiscreteDistribution",
            function(object) max(support(object)) )

 setMethod("getLow", "LatticeDistribution",
            function(object){ 
                lattice <- lattice(object) 
                qF <- q(object)
                if(is.finite(Length(lattice)) || width(lattice)>0)
                   return(min(support(object)))
                if(.inArgs("log.p",qF))
                   return(qF(p = .fm(x = .5, f = q(object)), log.p = TRUE))
                else 
                   return(qF(p = exp(.fm(x = .5, f = q(object)))))
                })
 setMethod("getUp", "LatticeDistribution",
            function(object){
                lattice <- lattice(object) 
                if(is.finite(Length(lattice)) || width(lattice)<0)
                   return(max(support(object)))
                qF <- q(object)
                if (.inArgs("lower.tail", qF)){
                    if(.inArgs("log.p",qF))
                       return(qF(p = .fM(x = .5, f = q(object)), 
                                        log.p = TRUE))
                    else
                       return(qF(p = exp(.fM(x = .5, f = q(object)))))
                }
                if(.inArgs("log.p",qF))
                   return(qF(p = .fM2(x = .5, f = q(object)), 
                             lower.tail = FALSE, log.p = TRUE))               
                else
                   return(qF(p = exp(.fM2(x = .5, f = q(object))), 
                             lower.tail = FALSE))               
                
                })   
