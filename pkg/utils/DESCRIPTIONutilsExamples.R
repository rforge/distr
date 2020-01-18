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

Pkgs <- c("RandVar", "ROptEstOld", "RobAStBase", "RobAStRDA", "RobLox", "RobRex", "RobLoxBioC", "ROptEst", "RobExtremes", "ROptRegTS")
Names <- c("Version")
Values <- matrix(c("1.1.0"),1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/robast/",names=Names,pkgs=Pkgs, values=Values)


Pkgs <- c("RandVar", "ROptEstOld", "RobAStBase", "RobAStRDA", "RobLox", "RobRex", "RobLoxBioC", "ROptEst", "RobExtremes", "ROptRegTS")
Names <- c("Version")
Values <- matrix(c("1.2.0"),1,length(Pkgs))
colnames(Values) <- Pkgs
rownames(Values) <- Names
changeDescription(startDir = "C:/rtest/robast/branches/robast-1.2",names=Names,pkgs=Pkgs, values=Values)
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
Values["Version",] <- matrix(c("0.9.1", rep("2.8.0",9)),1,10)
changeDescription(startDir = "C:/rtest/distr/branches/distr-2.8",names=Names,
                  pkgs=Pkgs, values=Values, verbose=TRUE)
}

##############################################################################

if(FALSE){## Version 2.8 in trunk
Pkgs <- c("startupmsg",
          "distr", "distrEx", "distrDoc", "distrEllipse", "distrRmetrics",
          "distrMod", "distrTeach", "distrSim", "distrTEst")
Names <- c("Version")
Values <- matrix(c("2.8.0"),1,length(Pkgs))
ReqRVersion0 <- c(NA,rep("R(>= 3.4)",length(Pkgs)-1))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs[-1]) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.0)",
            "RandVar"="RandVar(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0) <- Pkgs
rownames(Values)  <- Names
Values["Version",1] <- "0.9.6"
changeDescription(startDir = "C:/rtest/distr",names=Names,
                  pkgs=Pkgs, values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
updateHTMLpages(pkgVersions = c(rep("2.8.0",9),"1.2.0"))
}

if(FALSE){## Version 2.8 in trunk second shift 20190402
Pkgs <- c("distrEllipse", "distrMod")
Names <- c("Version")
Values <- matrix(c("2.8.0"),1,length(Pkgs))
ReqRVersion0 <- c(rep("R(>= 3.4)",length(Pkgs)))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.0)",
            "RandVar"="RandVar(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0) <- Pkgs
rownames(Values)  <- Names
changeDescription(startDir = "C:/rtest/distr",names=Names,
                  pkgs=Pkgs, values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
}

if(FALSE){## Version 2.8 in trunk third shift 20190407
Pkgs <- c("distrMod")
Names <- c("Version")
Values <- matrix(c("2.8.1"),1,length(Pkgs))
ReqRVersion0 <- c(rep("R(>= 3.4)",length(Pkgs)))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.0)",
            "RandVar"="RandVar(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0) <- Pkgs
rownames(Values)  <- Names
changeDescription(startDir = "C:/rtest/distr",names=Names,
                  pkgs=Pkgs, values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
}

if(FALSE){## Version 2.8 in trunk fourth shift 20190430
Pkgs <- c("distrMod")
Names <- c("Version")
Values <- matrix(c("2.8.2"),1,length(Pkgs))
ReqRVersion0 <- c(rep("R(>= 3.4)",length(Pkgs)))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.2)",
            "RandVar"="RandVar(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0) <- Pkgs
rownames(Values)  <- Names
changeDescription(startDir = "C:/rtest/distr",names=Names,
                  pkgs=Pkgs, values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
}

if(FALSE){## Version 2.8 in branch
Pkgs <- c("startupmsg",
          "distr", "distrEx", "distrDoc", "distrEllipse", "distrRmetrics",
          "distrMod", "distrTeach", "distrSim", "distrTEst")
Names <- c("Version")
Values <- matrix(c("2.8.0"),1,length(Pkgs))
ReqRVersion0 <- c(NA,rep("R(>= 3.4)",length(Pkgs)-1))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs[-1]) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.0)",
            "RandVar"="RandVar(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0) <- Pkgs
rownames(Values)  <- Names
Values["Version",1] <- "0.9.6"
changeDescription(startDir = "C:/rtest/distr/branches/distr-2.8",names=Names,
                  pkgs=Pkgs, values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
}

if(FALSE){## Version 2.9 in branch
Pkgs <- c("startupmsg",
          "distr", "distrEx", "distrDoc", "distrEllipse", "distrRmetrics",
          "distrMod", "distrTeach", "distrSim", "distrTEst")
Names <- c("Version")
Values <- matrix(c("2.9.0"),1,length(Pkgs))
ReqRVersion0 <- c(NA,rep("R(>= 3.4)",length(Pkgs)-1))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs[-1]) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.0)",
            "RandVar"="RandVar(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0) <- Pkgs
rownames(Values)  <- Names
Values["Version",1] <- "0.9.7"
changeDescription(startDir = "C:/rtest/distr/branches/distr-2.9",names=Names,
                  pkgs=Pkgs, values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
}

if(FALSE){## Version 1.2 in trunk
Pkgs <- c("RandVar", "ROptEstOld", "RobAStBase", "RobAStRDA", "RobLox", "RobRex", "RobLoxBioC", "ROptEst", "RobExtremes", "ROptRegTS")
Names <- c("Version")
Values <- matrix(c("1.2.0"),1,length(Pkgs))
ReqRVersion0 <- rep("R(>= 3.4)",length(Pkgs))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.0)",
            "RandVar"="RandVar(>= 1.2.0)", "RobAStBase"="RobAStBase(>= 1.2.0)",
            "ROptEst"="ROptEst(>= 1.2.0)", "ROptEstOld"="ROptEstOld(>= 1.2.0)",
            "RobAStRDA"="RobAStRDA(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0)<- Pkgs
rownames(Values)  <- Names

changeDescription(startDir = "C:/rtest/robast/",names=Names,pkgs=Pkgs,
                  values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
}

if(FALSE){## Version 1.2 in trunk second shift 20190402
Pkgs <- c("RandVar", "ROptEstOld", "RobAStBase", "RobLox", "RobRex", "RobLoxBioC", "ROptEst", "RobExtremes", "ROptRegTS")
Names <- c("Version")
Values <- matrix(c("1.2.0"),1,length(Pkgs))
ReqRVersion0 <- rep("R(>= 3.4)",length(Pkgs))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.0)",
            "RandVar"="RandVar(>= 1.2.0)", "RobAStBase"="RobAStBase(>= 1.2.0)",
            "ROptEst"="ROptEst(>= 1.2.0)", "ROptEstOld"="ROptEstOld(>= 1.2.0)",
            "RobAStRDA"="RobAStRDA(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0)<- Pkgs
rownames(Values)  <- Names

changeDescription(startDir = "C:/rtest/robast/",names=Names,pkgs=Pkgs,
                  values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
}

if(FALSE){## Version 1.2 in trunk third shift 20190407
Pkgs <- c("RobAStBase","ROptEst")
Names <- c("Version")
Values <- matrix(c("1.2.1"),1,length(Pkgs))
ReqRVersion0 <- rep("R(>= 3.4)",length(Pkgs))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.1)",
            "RandVar"="RandVar(>= 1.2.0)", "RobAStBase"="RobAStBase(>= 1.2.0)",
            "ROptEst"="ROptEst(>= 1.2.0)", "ROptEstOld"="ROptEstOld(>= 1.2.0)",
            "RobAStRDA"="RobAStRDA(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0)<- Pkgs
rownames(Values)  <- Names

changeDescription(startDir = "C:/rtest/robast/",names=Names,pkgs=Pkgs,
                  values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)

Pkgs <- c("RobExtremes")
Names <- c("Version")
Values <- matrix(c("1.2.0"),1,length(Pkgs))
ReqRVersion0 <- rep("R(>= 3.4)",length(Pkgs))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.1)",
            "RandVar"="RandVar(>= 1.2.0)", "RobAStBase"="RobAStBase(>= 1.2.0)",
            "ROptEst"="ROptEst(>= 1.2.0)", "ROptEstOld"="ROptEstOld(>= 1.2.0)",
            "RobAStRDA"="RobAStRDA(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0)<- Pkgs
rownames(Values)  <- Names

changeDescription(startDir = "C:/rtest/robast/",names=Names,pkgs=Pkgs,
                  values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
}

if(FALSE){## Version 1.2 in branch
Pkgs <- c("RandVar", "ROptEstOld", "RobAStBase", "RobAStRDA", "RobLox", "RobRex", "RobLoxBioC", "ROptEst", "RobExtremes", "ROptRegTS")
Names <- c("Version")
Values <- matrix(c("1.2.0"),1,length(Pkgs))
ReqRVersion0 <- rep("R(>= 3.4)",length(Pkgs))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
for(pk in Pkgs) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.0)",
            "RandVar"="RandVar(>= 1.2.0)", "RobAStBase"="RobAStBase(>= 1.2.0)",
            "ROptEst"="ROptEst(>= 1.2.0)", "ROptEstOld"="ROptEstOld(>= 1.2.0)",
            "RobAStRDA"="RobAStRDA(>= 1.2.0)", "ROptRegTS"="ROptRegTS(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0) <- Pkgs
rownames(Values)  <- Names
changeDescription(startDir = "C:/rtest/robast/branches/robast-1.2",names=Names,
                  pkgs=Pkgs, values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
}

if(FALSE){## Version 1.3 in branch
Pkgs <- c("RandVar", "ROptEstOld", "RobAStBase", "RobAStRDA", "RobLox", "RobRex", "RobLoxBioC", "ROptEst", "RobExtremes", "ROptRegTS")
Names <- c("Version")
Values <- matrix(c("1.3.0"),1,length(Pkgs))
ReqRVersion0 <- rep("R(>= 3.4)",length(Pkgs))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs[-1]) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.0)",
            "RandVar"="RandVar(>= 1.2.0)", "RobAStBase"="RobAStBase(>= 1.2.0)",
            "ROptEst"="ROptEst(>= 1.2.0)", "ROptEstOld"="ROptEstOld(>= 1.2.0)",
            "RobAStRDA"="RobAStRDA(>= 1.2.0)", "ROptRegTS"="ROptRegTS(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0)<- Pkgs
rownames(Values)  <- Names
changeDescription(startDir = "C:/rtest/robast/branches/robast-1.3",names=Names,
                  pkgs=Pkgs, values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
}

if(FALSE){## nur RandVar Version 1.2.1 in trunk
Pkgs <- c("RandVar")
Names <- c("Version")
Values <- matrix(c("1.2.1"),1,length(Pkgs))
ReqRVersion0 <- rep("R(>= 3.4)",length(Pkgs))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs[-1]) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.0)",
            "RandVar"="RandVar(>= 1.2.0)", "RobAStBase"="RobAStBase(>= 1.2.0)",
            "ROptEst"="ROptEst(>= 1.2.0)", "ROptEstOld"="ROptEstOld(>= 1.2.0)",
            "RobAStRDA"="RobAStRDA(>= 1.2.0)", "ROptRegTS"="ROptRegTS(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0)<- Pkgs
rownames(Values)  <- Names
changeDescription(startDir = "C:/rtest/robast/",names=Names,
                  pkgs=Pkgs, values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
}

if(FALSE){## nur RandVar Version 1.3.0 in branch
Pkgs <- c("RandVar")
Names <- c("Version")
Values <- matrix(c("1.3.0"),1,length(Pkgs))
ReqRVersion0 <- rep("R(>= 3.4)",length(Pkgs))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs[-1]) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.0)",
            "RandVar"="RandVar(>= 1.2.0)", "RobAStBase"="RobAStBase(>= 1.2.0)",
            "ROptEst"="ROptEst(>= 1.2.0)", "ROptEstOld"="ROptEstOld(>= 1.2.0)",
            "RobAStRDA"="RobAStRDA(>= 1.2.0)", "ROptRegTS"="ROptRegTS(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0)<- Pkgs
rownames(Values)  <- Names
changeDescription(startDir = "C:/rtest/robast/branches/robast-1.3",names=Names,
                  pkgs=Pkgs, values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
}

if(FALSE){## nur distrMod Version 2.8.3 in trunk
Pkgs <- c("distrMod")
Names <- c("Version")
Values <- matrix(c("2.8.3"),1,length(Pkgs))
ReqRVersion0 <- c(NA,rep("R(>= 3.4)",length(Pkgs)-1))
ReqDistrPkgVersion0 <- vector("list",length(Pkgs))
names(ReqDistrPkgVersion0) <- Pkgs
ReqDistrPkgVersion0[["RandVar"]] <- NA
for(pk in Pkgs[-1]) ReqDistrPkgVersion0[[pk]] <- c("distr"="distr(>= 2.8.0)",
            "distrEx"="distrEx(>= 2.8.0)","distrMod"="distrMod(>= 2.8.0)",
            "RandVar"="RandVar(>= 1.2.0)")
colnames(Values) <- names(ReqRVersion0) <- Pkgs
rownames(Values)  <- Names
changeDescription(startDir = "C:/rtest/distr",names=Names,
                  pkgs=Pkgs, values=Values,ReqRVersion =ReqRVersion0,
                  ReqDistrPkgVersion =ReqDistrPkgVersion0)
updateHTMLpages(pkgNames ="distrMod", pkgVersions = "2.8.3")
}

