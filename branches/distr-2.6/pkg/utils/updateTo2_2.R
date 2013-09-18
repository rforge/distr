Pkgs <- c("startupmsg", "SweaveListingUtils", "distr", "distrEx", "distrDoc",
                      "distrMod", "distrTeach", "distrSim", "distrTEst")
Names <- c("Version",  "Date")
Values <- matrix(c("2.0.2", format(Sys.time(), format="%Y-%m-%d")),2,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",] <- c("0.7", "0.3", rep("2.2", 7))
changeDescription(startDir = "C:/rtest/distr/branches/distr-2.2",names=Names,
                  pkgs=Pkgs, values=Values)
