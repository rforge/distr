### new util compare to compare structured S4 objects:

compare <- function(obj1, obj2, level = 0) {
#   if(!isClass(class(obj1))||!isClass(class(obj2)))
#      stop("'isOldVersion()' only works for formal S4-Classes.")
   if(! class(obj1)[1]==class(obj2)[1]){
      cat(gettextf("Problem with classes: %s != %s\n",
           class(obj1)[1], class(obj2)[1]))
#      stop("Classes must be identical.")
      }
   slotNames <- slotNames(obj1)
   indent1 <- paste(rep("  ",max(level-1,0)),sep="",collapse="")
   indent <- paste(rep("  ",level),sep="",collapse="")
   cat(gettextf("%s-------------- Level %i -----------------\n",indent1,level))
   ers <- sapply(slotNames, function(x){
                if(!is.null(slot(obj1,x))&&!is.null(slot(obj2,x))){
                    cat(gettextf("%sComparing slot %s:\n", indent, x))
                    cat(gettextf("%s-------------- Level %i -----------------\n",indent, level+1))
                }
                cat(gettextf("%sSlot %s of object 1:\n", indent, x))
                print(slot(obj1,x))
                cat(gettextf("%sSlot %s of object 2:\n", indent, x))
                print(slot(obj2,x))
                if(is.list(slot(obj1,x))){
                   if(length(slot(obj1,x)))
                   sapply(1:length(slot(obj1,x)), function(i){
                          obj10 <- slot(obj1,x)[[i]]; obj20 <- slot(obj2,x)[[i]]
                          cat(gettextf("%sComparing element %i of slot list %s:\n", indent, i, x))
                          erg<- try(
                          compare(obj1=obj10,obj2=obj20, level = level + 1)
                          ,silent=TRUE)
                          if(is(erg,"try-error"))
                             cat(gettextf("!!!!! Classes of slot %s are not identical.\n",x))
                   }
                   )
                }
                else if(isClass(class(slot(obj1,x)))&&
                   class(slot(obj1,x))[1]!="numeric" &&
                   class(slot(obj1,x))[1]!="character" &&
                   class(slot(obj1,x))[1]!="matrix" &&
                   class(slot(obj1,x))[1]!="array" &&
                   class(slot(obj1,x))[1]!="call" &&
                   class(slot(obj1,x))[1]!="function"
                   ){
#                   cat(gettextf("now trying %s\n", class(slot(obj1,x))[1]))
                   obj10 <- slot(obj1,x); obj20 <- slot(obj2,x)
                   erg<- try(
                      compare(obj1=obj10,obj2=obj20, level = level + 1)
                      ,silent=TRUE)
                   if(is(erg,"try-error"))
                      cat(gettextf("!!!!! Classes of slot %s are not identical.\n",x))
                   }

                }
                )
   return(invisible(NULL))
}
