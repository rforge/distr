 ladeall <- function(DIR="distr", develDir = "C:/rtest/distr/pkg"){
 od <- getwd()
 print(file.path(develDir,DIR, "R"))
 setwd(file.path(develDir,DIR, "R"))
 lapply(grep(".R$",dir(),value=T),source)
 setwd(od)
}

ladeall()

