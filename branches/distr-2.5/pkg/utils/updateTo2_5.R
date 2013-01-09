####distr:
### for uptodate:
Pkgs <- c("SweaveListingUtils", "distr", "distrEx", "distrDoc",
                      "distrMod", "distrTeach", "distrSim", "distrTEst",
                      "distrEllipse")
Names <- c("Version",  "Date", "SVNRevision")
Values <- matrix(c("2.0.2", format(Sys.time(), format="%Y-%m-%d"),1),3,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",] <- c("0.6", rep("2.4", 8))
changeDescription(startDir = "C:/rtest/distr/branches/distr-2.4",names=Names,
                  pkgs=Pkgs, values=Values)

### for 2.4
Pkgs <- c("distr", "distrEx", "distrDoc",
                      "distrMod", "distrTeach", "distrSim", "distrTEst",
                      "distrEllipse")
Names <- c("Version",  "Date", "SVNRevision")
Values <- matrix(c("2.0.2", format(Sys.time(), format="%Y-%m-%d"),1),3,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",] <- rep("2.5", 8)
changeDescription(startDir = "C:/rtest/distr/branches/distr-2.5",names=Names,
                  pkgs=Pkgs, values=Values)

#### robast:
### for uptodate:
Pkgs <- c("RandVar", "RobAStBase", "ROptEst", "RobLox"
          ,"ROptEstOld", "ROptRegTS", "RobRex", "RobLoxBioC"
          )
Names <- c("Version",  "Date", "SVNRevision")
Values <- matrix(c(0.8,format(Sys.time(), format="%Y-%m-%d"),1),3,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",] <- c(rep("0.8",8))
changeDescription(startDir = "C:/rtest/robast",names=Names,
                  pkgs=Pkgs, values=Values)

### for 0.9
Pkgs <- c("RandVar", "RobAStBase", "ROptEst", "ROptEstOld", "RobLox",
          "ROptRegTS", "RobRex", "RobLoxBioC")

Names <- c("Version","Date","SVNRevision")
Values <- matrix(c("0.8", format(Sys.time(), format="%Y-%m-%d"),1),3,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",] <- c(rep("0.9",8))
changeDescription(startDir = "C:/rtest/robast/branches/robast-0.9",names=Names,
                  pkgs=Pkgs, values=Values)
  