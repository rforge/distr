# generating function for class 'PosDefSymmMatrix'
PosSemDefSymmMatrix <- function(mat){ 
    if(!is.matrix(mat)) mat <- as.matrix(mat)
    new("PosSemDefSymmMatrix", mat)
}

PosDefSymmMatrix <- function(mat){ 
    if(!is.matrix(mat)) mat <- as.matrix(mat)
    new("PosDefSymmMatrix", mat)
}

