Pkgs <- c("startupmsg", "SweaveListingUtils",
                      "distr", "distrEx", "distrDoc",
                      "distrMod", "distrTeach", "distrSim", "distrTEst")
Names <- c("Version", "Date")
Values <- matrix(c("2.0.2", format(Sys.time(), format="%Y-%m-%d")),2,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",] <- c("0.5.3", "0.2", rep("2.1",7))

changeDescription(startDir = "C:/rtest/distr/branches/distr-2.1",names=Names,
                  pkgs=Pkgs, values=Values)


Pkgs <- c("RandVar", "RobAStBase",
                      "RobLox", "ROptEst", "RobRex",
                      "ROptRegTS")
Names <- c("Version", "Date")
Values <- matrix(c("0.7", format(Sys.time(), format="%Y-%m-%d")),2,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",2] <- "0.2"

changeDescription(startDir = "C:/rtest/RobASt/branches/robast-0.7",names=Names,
                  pkgs=Pkgs, values=Values)
