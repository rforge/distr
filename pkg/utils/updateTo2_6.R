Pkgs <- c("SweaveListingUtils", "distr", "distrEx", "distrDoc",
                      "distrMod", "distrTeach", "distrSim", "distrTEst",
                      "distrEllipse")
Names <- c("Version","Date","SVNRevision")
Values <- matrix(c("2.0.2", format(Sys.time(), format="%Y-%m-%d"),1),3,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
Values["Version",] <- c("0.5", rep("2.3", 8))
changeDescription(startDir = "C:/rtest/distr/",names=Names,
                  pkgs=Pkgs, values=Values)

Values <- matrix(c("0.9.2",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "startupmsg"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values)

Values <- matrix(c("0.7",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "SweaveListingUtils"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values)

Values <- matrix(c("2.6",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "distr"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values)

Values <- matrix(c("2.6",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "distrEx"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values)

Values <- matrix(c("2.6",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "distrSim"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values)

Values <- matrix(c("2.6",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "distrTEst"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values)

Values <- matrix(c("2.6",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "distrTeach"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values)

Values <- matrix(c("2.6",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "distrRmetrics"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values)

Values <- matrix(c("2.6",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "distrEllipse"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values)

Values <- matrix(c("2.6",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "distrDoc"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values)

Values <- matrix(c("0.9.4",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "RandVar"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/RobASt/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values, pathRepo="robast")

Values <- matrix(c("2.6",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "distrMod"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/distr/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values)

Values <- matrix(c("1.0",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "RandVar"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/RobASt/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values, pathRepo="robast")

Values <- matrix(c("1.0",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "RobAStRDA"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/RobASt/",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values, pathRepo="robast")

Values <- matrix(c("1.0",format(Sys.time(), format="%Y-%m-%d")),2,1)
Pkgs  <- "RobAStBase"
Names <- c("Version","Date")
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/RobASt/branches/robast-1.0",names=Names, verbose=TRUE,
                  pkgs=Pkgs, values=Values, pathRepo="robast")
