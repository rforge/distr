### default settings
.CacheLength <- 0
.CacheFiles <- NULL

.SweaveListingOptions <- list(
Rset = list("language"="R","escapechar"="`",
        "fancyvrb"="true","basicstyle"="\\color{Rcolor}\\footnotesize",
        "commentstyle"="\\color{Rcomment}\\ttfamily\\itshape",
        "literate"="{<-}{{$\\leftarrow$}}2",
        "morekeywords"="[2]{Norm,Pois,lambda,p,d,r,distroptions}"),
Rdset = list("language"="TeX", "basicstyle"="\\color{black}\\tiny",
               "commentstyle"="\\ttfamily\\itshape"),
Rcolor  = c(0,0.5,0.5),
Rout     = c(0.461,0.039,0.102),
Rcomment = c(0.101,0.043,0.432),
pkv = "2.0.2",
pkg = "distr"
)

SweaveListingOptions <- function(...) {
    if (nargs() == 0) return(.SweaveListingOptions)
    current <- .SweaveListingOptions
    temp <- list(...)
    if (length(temp) == 1 && is.null(names(temp))) {
        arg <- temp[[1]]
        switch(mode(arg),
            list = temp <- arg,
            character = return(.SweaveListingOptions[arg]),
            stop("invalid argument: ", sQuote(arg)))
    }
    if (length(temp) == 0) return(current)
    n <- names(temp)
    if (is.null(n)) stop("options must be given by name")
    changed <- current[n]
    current[n] <- temp
    if (sys.parent() == 0)
        env <- asNamespace("SweaveListingUtils")
    else
        env <- parent.frame()
    assign(".SweaveListingOptions", current, envir = env)

    invisible(current)
}

getSweaveListingOption <- function(x) SweaveListingOptions(x)[[1]]
SweaveListingoptions <- SweaveListingOptions
