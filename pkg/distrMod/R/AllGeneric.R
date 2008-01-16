### ---------------------------
### "multiple-purpose generics" --- 
###   for  
###      + accessor method  
###      + stats/base function
###      + functional method
### ---------------------------

## access methods
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
