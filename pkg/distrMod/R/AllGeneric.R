### ---------------------------
### "multiple-purpose generics" --- 
###   for  
###      + accessor method  
###      + stats/base function
###      + functional method
### ---------------------------

## access and replace methods
#if(!isGeneric("param<-")){ 
#    setGeneric("param<-", function(object, value) standardGeneric("param<-"))
#}
if(!isGeneric("type")){
    setGeneric("type", function(object) standardGeneric("type"))
}
if(!isGeneric("SymmCenter")){
    setGeneric("SymmCenter", function(object) standardGeneric("SymmCenter"))
}
if(!isGeneric("distrSymm")){
    setGeneric("distrSymm", function(object) standardGeneric("distrSymm"))
}
if(!isGeneric("distribution")){
    setGeneric("distribution", function(object) standardGeneric("distribution"))
}
if(!isGeneric("props")){
    setGeneric("props", function(object) standardGeneric("props"))
}
if(!isGeneric("props<-")){
    setGeneric("props<-", function(object, value) standardGeneric("props<-"))
}
if(!isGeneric("addProp<-")){
    setGeneric("addProp<-", function(object, value) standardGeneric("addProp<-"))
}
if(!isGeneric("main")){
    setGeneric("main", function(object) standardGeneric("main"))
}
if(!isGeneric("main<-")){
    setGeneric("main<-", function(object, value) standardGeneric("main<-"))
}
if(!isGeneric("nuisance")){
    setGeneric("nuisance", function(object) standardGeneric("nuisance"))
}
if(!isGeneric("nuisance<-")){
    setGeneric("nuisance<-", function(object, value) standardGeneric("nuisance<-"))
}
if(!isGeneric("trafo")){
    setGeneric("trafo", function(object) standardGeneric("trafo"))
}
if(!isGeneric("trafo<-")){
    setGeneric("trafo<-", function(object, value) standardGeneric("trafo<-"))
}
if(!isGeneric("modifyParam")){
    setGeneric("modifyParam", function(object) standardGeneric("modifyParam"))
}
if(!isGeneric("dimension")){
    setGeneric("dimension", function(object) standardGeneric("dimension"))
}
if(!isGeneric("L2deriv")){
    setGeneric("L2deriv", function(object) standardGeneric("L2deriv"))
}
if(!isGeneric("L2derivSymm")){
    setGeneric("L2derivSymm", function(object) standardGeneric("L2derivSymm"))
}
if(!isGeneric("L2derivDistr")){
    setGeneric("L2derivDistr", function(object) standardGeneric("L2derivDistr"))
}
if(!isGeneric("L2derivDistrSymm")){
    setGeneric("L2derivDistrSymm", function(object) standardGeneric("L2derivDistrSymm"))
}
if(!isGeneric("FisherInfo")){
    setGeneric("FisherInfo", function(object) standardGeneric("FisherInfo"))
}
if(!isGeneric("checkL2deriv")){
    setGeneric("checkL2deriv", function(L2Fam, ...) standardGeneric("checkL2deriv"))
}
if(!isGeneric("bound")){ 
    setGeneric("bound", function(object) standardGeneric("bound"))
}
if(!isGeneric("width")){ 
    setGeneric("width", function(object) standardGeneric("width"))
}

if(!isGeneric("sign")){
    setGeneric("sign", function(x) standardGeneric("sign"))
}
if(!isGeneric("nu")){
    setGeneric("nu", function(object) standardGeneric("nu"))
}

if(!isGeneric("sign<-")){
    setGeneric("sign<-", function(object,value) standardGeneric("sign<-"))
}
if(!isGeneric("nu<-")){
    setGeneric("nu<-", function(object,value) standardGeneric("nu<-"))
}

if(!isGeneric("biastype")){
    setGeneric("biastype", function(object) standardGeneric("biastype"))
}

if(!isGeneric("biastype<-")){
    setGeneric("biastype<-", function(object,value) standardGeneric("biastype<-"))
}

if(!isGeneric("solve")){
    setGeneric("solve", function(a,b,...) standardGeneric("solve"))
}

if(!isGeneric("modifyModel")){
    setGeneric("modifyModel", function(model, param, ...) standardGeneric("modifyModel"))
}
if(!isGeneric("existsPIC")){
    setGeneric("existsPIC", function(object,...) standardGeneric("existsPIC"))
}

if(!isGeneric("norm")){
    setGeneric("norm", function(x,...) standardGeneric("norm"))
}

if(!isGeneric("normtype")){
    setGeneric("normtype", function(object) standardGeneric("normtype"))
}

if(!isGeneric("normtype<-")){
    setGeneric("normtype<-", function(object,value) standardGeneric("normtype<-"))
}

if(!isGeneric("QuadForm")){
    setGeneric("QuadForm", function(object) standardGeneric("QuadForm"))
}
if(!isGeneric("QuadForm<-")){
    setGeneric("QuadForm<-", function(object,value) standardGeneric("QuadForm<-"))
}

if(!isGeneric("fct<-")){
    setGeneric("fct<-", function(object,value) standardGeneric("fct<-"))
}
if(!isGeneric("fct")){
    setGeneric("fct", function(object) standardGeneric("fct"))
}
if(!isGeneric("updateNorm")){
    setGeneric("updateNorm", function(normtype, ...) standardGeneric("updateNorm"))
}
