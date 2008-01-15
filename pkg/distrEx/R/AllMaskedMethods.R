############################################################################
### ------------------------------------------------
### In this comment substitute 'xxx' by
###  'var', 'median', 'IQR', 'mad',  respectively
### ------------------------------------------------
### We intentionally mask function 'xxx' from stats in order to add a formal
### argument "...".
### functionality of 'stats::xxx' is completely retained, however
### for help to the original 'stats::xxx' function write
###       'help("xxx", package="stats")'
### for code to the original 'stats::xxx' function write
###       'stats::xxx'
############################################################################


## ---- registering original functions as Methods: ----

setMethod("var", "ANY", function(x , ...)
       {dots <- list(...)
        if(hasArg(y)) y <- dots$"y"
        na.rm <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        if(!hasArg(use))
             use <- ifelse (na.rm, "complete.obs","all.obs")
        else use <- dots$"use"
        if(hasArg(y))
           stats::var(x = x, y = y, na.rm = na.rm, use)
        else
           stats::var(x = x, y = NULL, na.rm = na.rm, use)
        })

setMethod("median","ANY",function(x , ...)
       {dots <- list(...)
        na.rm <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        stats::median(x = x, na.rm = na.rm)}
)

setMethod("IQR","ANY",function(x , ...)
       {dots <- list(...)
        na.rm <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        stats::IQR(x = x, na.rm = na.rm)}
)

setMethod("mad","ANY",function(x , ...)
       {dots <- list(...)
        na.rm     <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        low       <-  ifelse(hasArg(low), dots$"low", FALSE)
        high      <-  ifelse(hasArg(high), dots$"high", FALSE)
        center    <-  ifelse(hasArg(center), dots$"center", median(x))
        constant  <-  ifelse(hasArg(constant), dots$"constant", 1.4826)
        stats::mad(x = x, center = center, constant = constant ,
                   na.rm = na.rm, low = low, high = high)})


############################################################################
### ------------------------------------------------
### In this comment substitute 'xxx' by
### 'kurtosis', 'skewness',  respectively
### ------------------------------------------------
### We intentionally /copy/ function 'xxx' from e1071 in order to avoid
### the necessity to add e1071 to the 'Depends' tag in the DESCRIPTION
### file of this package.
### functionality of 'e1071::xxx' is completely retained, however
###
### acknowledgement:
###     methods 'skewness' and 'kurtosis' for particular methods
###     have been provided by G. Jay Kerns, gkerns@ysu.edu
############################################################################

setMethod("skewness","ANY",function(x , ...)
       {dots <- list(...)
        na.rm     <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        ### definition taken from package e1071
        if (na.rm)
        x <- x[!is.na(x)]
        sum((x - mean(x))^3)/(length(x) * sd(x)^3)
        })

setMethod("kurtosis","ANY",function(x , ...)
       {dots <- list(...)
        na.rm     <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        ### definition taken from package e1071
        if (na.rm)
        x <- x[!is.na(x)]
        sum((x - mean(x))^4)/(length(x) * var(x)^2) - 3
        })
