## Extract model results of interest, write TAF output tables

library(icesTAF)

mkdir("output")

source("utilities_output.R")

fit.current <- read.fit("model/sam")

## Figure out a few basics
minAge <- min(fit.current$res[,3])
maxAge <- max(fit.current$res[,3])
noN <- maxAge - minAge + 1
conffile <- "model/model.cfg"
range.fbar <- fit.current$keys$fbarRange

## Summary table
R <- fit.current$R[,c(1,3,4)]
tsb <- exp(fit.current$logtsb[,c(1,3,4)])
ssb <- exp(fit.current$logssb[,c(1,3,4)])
fbar <- exp(fit.current$logfbar[,c(1,3,4)])
tab1 <- cbind(R, tsb, ssb, fbar)
tab1[nrow(tab1),-c(7:9)] <- NA
colnames(tab1) <- c("Recruits", "Low", "High", "TSB", "Low", "High", "SSB", "Low", "High",
                    paste("F",range.fbar[1],range.fbar[2],sep=""), "Low", "High")
rownames(tab1) <- fit.current$years

## N table
tab2 <- exp(fit.current$stateEst[,1:noN])
rownames(tab2) <- fit.current$years
colnames(tab2) <- paste(1:noN+minAge-1, c(rep("",noN-1),"+"), sep="")

## F table
tab3 <- exp(fit.current$stateEst[,-c(1:noN)])
tab3 <- tab3[-nrow(tab3),]
rownames(tab3) <- fit.current$years[1:nrow(tab3)]
colnames(tab3) <- paste(1:ncol(tab3)+minAge-1, c(rep("",ncol(tab3)-1),"+"), sep="")

## Scale table
tab4 <- exp(fit.current$logscale[,c(1,3,4)])
if(nrow(tab4) > 0) {
  idx1 <- grep("Years in which catch data are to be scaled", readLines(conffile))
  idx2 <- grep("Define Fbar range", readLines(conffile))
  num <- scan(conffile, skip=idx1, comment.char="#", quiet=TRUE, nlines=idx2-idx1)
  n <- num[1]
  y <- num[2:(n+1)]
  key <- matrix(num[-c(1:(n+1))], nrow=length(y), byrow=TRUE)
  if(!all(apply(key, 1, function(x)length(unique(x))==1))) {
    tab4 <- matrix(tab4[,1][key], nrow=length(y))
    colnames(tab4) <- paste(1:ncol(tab3)+minAge-1, c(rep("",ncol(tab3)-1),"+"), sep="")
    rownames(tab4) <- y
  } else { # important special case
    colnames(tab4) <- c("Catch multiplier", "Low", "High")
    rownames(tab4) <- y[key[,1]]
  }
}

## Catch table
yy <- fit.current$years[-length(fit.current$years)]
scale <- rep(1, length(yy))
## get catch scaling
phi <- 1 / rowMeans(exp(matrix(fit.current$logscale[fit.current$keys$keyParScaledYA],
                               nrow=fit.current$keys$noScaledYears)))
scaleYears <- fit.current$keys$keyScaledYears
scale[yy%in%scaleYears] <- phi
tab7 <- cbind(exp(fit.current$logLand[,1]) * scale,
              exp(fit.current$logDis[,1]) * scale,
              exp(fit.current$logCatch[,1]) * scale, exp(fit.current$logCatch[,1]))
rownames(tab7) <- yy
colnames(tab7) <- c("Landings", "Discards", "Catch", "Total Removal")

## Predicted catch at age
sub <- fit.current$res[fit.current$res[,2]==1,c(1,3,5)]
names(sub) <- c("year", "age", "logPred")
mat <- reshape(sub, idvar="year", timevar="age", direction="wide")
rownames(mat) <- mat[,1]
tab8 <- exp(mat[,-1])
colnames(tab8) <- paste(1:ncol(tab8)+minAge-1, c(rep("",ncol(tab8)-1),"+"), sep="")

## Prepare tables for export
summary <- xtab2taf(tab1)
natage <- xtab2taf(tab2)
fatage <- xtab2taf(tab3)
multiplier <- xtab2taf(tab4)
catch <- xtab2taf(tab7)
catage_fit <- xtab2taf(tab8)

## Write tables to output directory
write.taf(fatage, "output/fatage.csv") # 8
write.taf(natage, "output/natage.csv") # 9
write.taf(catage_fit, "output/catage_fit.csv") # 10
write.taf(summary, "output/summary.csv")       # 11a
write.taf(catch, "output/catch.csv")           # 11b
write.taf(multiplier, "output/multiplier.csv") # 11c
