#########
## about Dataclass: slot Data
##
### changed from version 1.8 on:
## ith observation in ith line of datamatrix/array
## jth item/dimension of each observation in jth column of datamatrix/array
## kth run/time of each observation in kth slide of datamatrix/array

## ++old
## +ith run in ith line of datamatrix
## +jth samples of each run in jth column of datamatrix


################################
##
## Class: Dataclass
##
################################

## Access methods

setMethod("filename", "Dataclass", function(object) object@filename)
setMethod("Data", "Dataclass", function(object) object@Data)
setMethod("runs", "Dataclass", function(object) object@runs)
setMethod("samplesize", "Dataclass", function(object) object@samplesize)
setMethod("obsDim", "Dataclass", function(object) object@obsDim)### new v.1.8
setMethod("name", "Dataclass", function(object) object@name)### new v.1.8
setMethod("getVersion", "Dataclass", 
           function(object) object@version)### new v.1.8

setMethod("Dataclass","DataframeorSeqDataFrames", function(Data, filename = NULL, name = "Data-Set"){ 
 runs0 <- runs(Data)
 obsDim0 <- obsDim(Data)
 dimnames0 <- obsDimnames(Data)
 rnames <- names(Data)
 dnames <- obsDimnames(Data)
 samplesize0 <- samplesize(Data)
 new("Dataclass", filename = filename, Data = Data0, runs = runs0, 
     obsDim = obsDim0, samplesize = samplesize0, name = name)
})

setMethod("Dataclass","ArrayorNULLorVector", function(Data, filename = NULL, name = "Data-Set") 
{if(is.null(Data))
    stop("generating an object of class \"Dataclasss\" requires data of type \"array\" or \"vector\"")

 runs0 <- 1
 obsDim0 <- 1
 dimnames0 <- NULL
 rnames <- NULL
 dnames <- NULL
 snames <- NULL
 if(is(Data,"array") & !is.na(dim(Data)[3]))
    { runs0 <- dim(Data)[3]   
      if(!is.null(dimnames(Data))) 
          rnames <- dimnames(Data)[[3]]
    } 
 if(!is.null(dim(Data)))
    { obsDim0 <- dim(Data)[2]   
      samplesize0 <- dim(Data)[1] 
      if(!is.null(dimnames(Data))) 
          {dnames <- dimnames(Data)[[2]]
           snames <- dimnames(Data)[[1]]
           dimnames0 <- list(snames, dnames, rnames)
      } else {
      samplesize0 <- length(Data)
      if(!is.null(names(Data)))  
          {snames <- names(Data)
           dimnames0 <- list(snames, dnames, rnames)
           }
      }
    }
      
 Data0 <- array(data = Data, dim = c(samplesize0,obsDim0,runs0),
                dimnames = dimnames0)       
 new("Dataclass", filename = filename, Data = Data0, runs = runs0, 
     obsDim = obsDim0, samplesize = samplesize0, name = name)
})

 


## Replacement methods

setReplaceMethod("name", "Dataclass", 
                  function(object, value)
                          { object@name <- value; object}) ### new 1.8
setReplaceMethod("filename", "Dataclass", 
                  function(object, value){ object@filename <- value; object})

setReplaceMethod("Data", "Dataclass",
                 function(object, value){
                   datdim <- dim(value)
                   object <- new("Dataclass",
                                 filename = filename(object),
                                 Data = value, samplesize=datdim[1],
                                 obsDim = datdim[2], 
                                 runs = datdim[3])
                   object
                 })




