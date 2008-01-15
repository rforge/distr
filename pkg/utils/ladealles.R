 ladeall <- function(DIR="distr", develDir = "C:/rtest"){
 od <- getwd()
 print(file.path(develDir,DIR, "R"))
 setwd(file.path(develDir,DIR, "R"))
 lapply(grep(".R$",dir(),value=T),source)
 setwd(od)
}

ladeall()

