##############################################################################
# EXAMPLES
##############################################################################
if(FALSE){## Example 1
Pkgs <- c("startupmsg", "SweaveListingUtils",
                      "distr", "distrEx", "distrDoc",
                      "distrMod", "distrTeach", "distrSim", "distrTEst")
Names <- c("Version", "License", "Date")
Values <- matrix(c("2.0.2","LGPL-3"),3,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",] <- c("0.5.2", "0.1.1", "2.0.3", "2.0.2", "2.0.3",
                         rep("2.0.2",4))
changeDescription(startDir = "C:/rtest/distr",names=Names,
                  pkgs=Pkgs, values=Values)
}

if(FALSE){## Example 2
Pkgs <- c("SweaveListingUtils", "distr", "distrEx",
                      "distrMod", "distrTeach", "distrSim", "distrTEst")
Names <- c("Date")
Values <- matrix((format(Sys.time(), format="%Y-%m-%d")),1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr",names="Date",
                  pkgs=Pkgs, values=format(Sys.time(), format="%Y-%m-%d"))
}

if(FALSE){### Version 2.4.1
Pkgs <- c("startupmsg", "SweaveListingUtils",
          "distr", "distrEx", "distrDoc",
          "distrMod", "distrTeach", "distrSim",
          "distrTEst", "distrEllipse", "distrRmetrics")
Names <- c("Version")
Values <- matrix(c("2.4.1",1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",,drop=FALSE] <- c("0.8.1", "0.6.1", rep("2.4.1",9))
changeDescription(startDir = "C:/rtest/distr",names=Names,
                  pkgs=Pkgs, values=Values)
}
if(FALSE){### Version 0.9.1
Pkgs <- c("RobLox", "RobLoxBioC", "RobRex", "ROptRegTS")
Names <- c("Version","License")
Values <- matrix(c("0.9","LGPL-3"),2,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/robast",names=Names,
                  pkgs=Pkgs, values=Values)
}
if(FALSE){
Pkgs <- c("RandVar", "ROptEstOld")
Names <- c("Version","License")
Values <- matrix(c("0.9.1","LGPL-3"),2,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/robast",names=Names,
                  pkgs=Pkgs, values=Values)

Pkgs <- c("RandVar", "ROptEstOld", "RobAStBase", "RobAStRDA", "RobLox", "RobRex", "RobLoxBioC", "ROptEst", "RobExtremes", "ROptRegTS")
Names <- c("Version")
Values <- matrix(c("1.0"),1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/robast/branches/robast-1.0",names=Names,
                  pkgs=Pkgs, values=Values)
}

if(FALSE){### Version 2.5
Pkgs <- c("startupmsg", "SweaveListingUtils",
          "distrEx", "distrDoc",
          "distrTeach",
          "distrTEst", "distrEllipse", "distrRmetrics")
Names <- c("Version")
Values <- matrix(c("2.5"),1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",,drop=FALSE] <- c("0.9", "0.7", rep("2.5",6))
changeDescription(startDir = "C:/rtest/distr",names=Names,
                  pkgs=Pkgs, values=Values)

Pkgs <- c("distr", "distrSim","distrMod")
Names <- c("Version")
Values <- matrix(c("2.6"),1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr/branches/distr-2.6",names=Names,
                  pkgs=Pkgs, values=Values)

Pkgs <- c("startupmsg", "SweaveListingUtils",
          "distrEx", "distrDoc",
          "distrTeach",
          "distrTEst", "distrEllipse", "distrRmetrics")
Names <- c("Version")
Values <- matrix(c("2.6"),1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",,drop=FALSE] <- c("0.9.1", "0.7", rep("2.6",6))
changeDescription(startDir = "C:/rtest/distr/branches/distr-2.6",names=Names,
                  pkgs=Pkgs, values=Values)


Pkgs <- c("startupmsg", "distr", "distrEx", "distrDoc", "distrSim",
          "distrTeach", "distrMod",
          "distrTEst", "distrEllipse", "distrRmetrics")
Names <- c("Version")
Values <- matrix(c("2.8"),1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",] <- matrix(c("0.9.1", rep("2.7.0",9)),1,10)
changeDescription(startDir = "C:/rtest/distr",names=Names,
                  pkgs=Pkgs, values=Values, verbose=TRUE)
}
Pkgs <- c("RandVar", "RobAStBase", "ROptEst", "RobExtremes", "RobAStRDA",
          "ROptEstOld", "RobLox",
          "ROptRegTS", "RobRex", "RobLoxBioC")
Values <- matrix(c("2.8"),1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",] <- matrix(c(rep("1.1.0",10)),1,10)
changeDescription(startDir = "C:/rtest/robast/branches/robast-1.1",names=Names,
                  pkgs=Pkgs, values=Values, verbose=TRUE)

##############################################################################
