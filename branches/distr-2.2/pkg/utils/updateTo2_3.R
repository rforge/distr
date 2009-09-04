####distr:
### for uptodate:
Pkgs <- c("startupmsg", "SweaveListingUtils", "distr", "distrEx", "distrDoc",
                      "distrMod", "distrTeach", "distrSim", "distrTEst",
                      "distrEllipse")
Names <- c("Date")
Values <- matrix(c(format(Sys.time(), format="%Y-%m-%d")),1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values
#Values["Version",] <- c("0.8", "0.5", rep("2.3", 7), "0.2")
changeDescription(startDir = "C:/rtest/distr/branches/distr-2.2",names=Names,
                  pkgs=Pkgs, values=Values)

### for 2.3
Pkgs <- c("startupmsg", "SweaveListingUtils", "distr", "distrEx", "distrDoc",
                      "distrMod", "distrTeach", "distrSim", "distrTEst",
                      "distrEllipse")
Names <- c("Version",  "Date")
Values <- matrix(c("2.0.2", format(Sys.time(), format="%Y-%m-%d")),2,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",] <- c("0.8", "0.5", rep("2.3", 7), "0.2")
changeDescription(startDir = "C:/rtest/distr/branches/distr-2.3",names=Names,
                  pkgs=Pkgs, values=Values)

#### robast:
### for uptodate:
Pkgs <- c("RandVar", "RobAStBase", "ROptEst", "RobLox"
          ,"ROptEstOld", "ROptRegTS", "RobRex", "RobLoxBioC"
          )
Names <- c("Date")
Values <- matrix(c(format(Sys.time(), format="%Y-%m-%d")),1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values
#Values["Version",] <- c("0.8", "0.5", rep("2.3", 7), "0.2")
changeDescription(startDir = "C:/rtest/robast/branches/robast-0.7",names=Names,
                  pkgs=Pkgs, values=Values)

### for 0.8
Pkgs <- c("RandVar", "RobAStBase", "ROptEst", "ROptEstOld", "RobLox",
          "ROptRegTS", "RobRex", "RobLoxBioC")

Names <- c("Version",  "Date")
Values <- matrix(c("0.8", format(Sys.time(), format="%Y-%m-%d")),2,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",] <- c(rep("0.8",5), "0.6.1", "0.6.1", "0.5")
changeDescription(startDir = "C:/rtest/robast/branches/robast-0.8",names=Names,
                  pkgs=Pkgs, values=Values)
