## Preprocess data, write TAF data tables

## Before: cn.dat, cw.dat, dw.dat, lf.dat, lw.dat, mo_raw.dat, nm.dat, pf.dat,
##         pm.dat, survey.dat, sw.dat (bootstrap/data)
## After:  catage.csv, catage_full.csv, catch_sop.csv, datage.csv,
##         datage_full.csv, ibts1.csv, ibts3.csv, landfrac.csv, latage.csv,
##         latage_full.csv, maturity.csv, maturity_full.csv, natmort.csv,
##         propf.csv, propm.csv, sam.dat, surveytime.csv, wcatch.csv,
##         wcatch_full.csv, wdiscards.csv, wdiscards_full.csv, wlandings.csv,
##         wlandings_full.csv, wstock.csv, wstock_full.csv (data)

library(icesTAF)
suppressMessages(library(mgcv))
source("utilities_data.R")

mkdir("data")

## Get data
setwd("bootstrap/data")
catch.no <- read.ices("cn.dat")
catch.mean.weight <- read.ices("cw.dat")
dis.mean.weight <- read.ices("dw.dat")
land.mean.weight <- read.ices("lw.dat")
stock.mean.weight <- read.ices("sw.dat")
prop.mature <- read.ices("mo_raw.dat")
natural.mortality <- read.ices("nm.dat")
surveys <- read.surveys("survey.dat")
land.no <- read.ices("lf.dat")
dis.no <- catch.no - land.no
prop.f <- read.ices("pf.dat")
prop.m <- read.ices("pm.dat")
setwd("../..")
## full datasets, including all ages
catch.no.full <- catch.no
land.no.full <- land.no
land.mean.weight.full <- land.mean.weight
dis.mean.weight.full <- dis.mean.weight
catch.mean.weight.full <- catch.mean.weight
stock.mean.weight.full <- stock.mean.weight

## Smooth maturity
skipYears <- c(1:10, 54)
columnsToSmooth <- 1:5
mo <- prop.mature[-c(skipYears),]
for(cc in columnsToSmooth){
  ww <- mo[,cc]
  tt <- 1:length(ww)
  tmp <- gam(ww ~ s(tt))
  mo[,cc] <- predict(tmp)
}
prop.mature[-c(skipYears),] <- mo
prop.mature[54,] <- prop.mature[53,]
prop.mature.full <- prop.mature

## Modify to 6+ data
cutage <- 6
low <- 1
GE <- which(as.numeric(colnames(catch.no)) >= cutage)
E <- which(as.numeric(colnames(catch.no)) == cutage)
w <- catch.no[,GE] / rowSums(catch.no[,GE])
wex <- rbind(w, w[nrow(w),])
wD <- dis.no[,GE] / ifelse(rowSums(dis.no[,GE])>0, rowSums(dis.no[,GE]), 1)
wL <- land.no[,GE] / rowSums(land.no[,GE])
catch.no[,E] <- rowSums(catch.no[,GE])
catch.no <- catch.no[,low:E]
prop.mature[,E] <- rowSums(prop.mature[,GE] * wex)
prop.mature <- prop.mature[,low:E]
stock.mean.weight[,E] <- rowSums(stock.mean.weight[,GE] * wex)
stock.mean.weight <- stock.mean.weight[,low:E]
catch.mean.weight[,E] <- rowSums(catch.mean.weight[,GE] * w)
catch.mean.weight <- catch.mean.weight[,low:E]
dis.mean.weight[,E] <- rowSums(dis.mean.weight[,GE] * wD)
dis.mean.weight <- dis.mean.weight[,low:E]
land.mean.weight[,E] <- rowSums(land.mean.weight[,GE] * wL)
land.mean.weight <- land.mean.weight[,low:E]
natural.mortality[,E] <- rowSums(natural.mortality[,GE] * wex)
natural.mortality <- natural.mortality[,low:E]
land.no[,E] <- rowSums(land.no[,GE])
land.no <- land.no[,low:E]
land.frac <- ifelse(catch.no>0, land.no/catch.no, 1)
prop.f <- prop.f[,low:E]
prop.m <- prop.m[,low:E]

## Catch as sum of products
sop <- data.frame(Landings=rowSums(land.no * land.mean.weight),
                  Discards=rowSums((catch.no-land.no) * dis.mean.weight))
sop$Catch <- sop$Landings + sop$Discards
## sop$Catch2 <- rowSums(catch.no * catch.mean.weight)

## Prepare tables for export
latage <- xtab2taf(land.no)
datage <- xtab2taf(catch.no - land.no)
catage <- xtab2taf(catch.no)
wlandings <- xtab2taf(land.mean.weight)
wdiscards <- xtab2taf(dis.mean.weight)
wcatch <- xtab2taf(catch.mean.weight)
wstock <- xtab2taf(stock.mean.weight)
maturity <- xtab2taf(prop.mature)
natmort <- xtab2taf(natural.mortality)
ibts1 <- xtab2taf(surveys[[1]])
ibts3 <- xtab2taf(surveys[[2]])
surveytime <- as.data.frame(lapply(surveys, attr, "time"),
                            col.names=c("ibts1","ibts3"))
catch_sop <- xtab2taf(sop)
landfrac <- xtab2taf(land.frac)
propf <- xtab2taf(prop.f)
propm <- xtab2taf(prop.m)
## full datasets, including all ages
latage_full <- xtab2taf(land.no.full)
datage_full <- xtab2taf(catch.no.full - land.no.full)
catage_full <- xtab2taf(catch.no.full)
wlandings_full <- xtab2taf(land.mean.weight.full)
wdiscards_full <- xtab2taf(dis.mean.weight.full)
wcatch_full <- xtab2taf(catch.mean.weight.full)
wstock_full <- xtab2taf(stock.mean.weight.full)
maturity_full <- xtab2taf(prop.mature.full)

## Rename plus group
latage <- plus(latage)
datage <- plus(datage)
catage <- plus(catage)
wlandings <- plus(wlandings)
wdiscards <- plus(wdiscards)
wcatch <- plus(wcatch)
wstock <- plus(wstock)
landfrac <- plus(landfrac)
maturity <- plus(maturity)
natmort <- plus(natmort)
propf <- plus(propf)
propm <- plus(propm)

## Write tables to data directory
setwd("data")
write.taf(latage)     # 2a
write.taf(datage)     # 2b
write.taf(catage)     # 2c
write.taf(wlandings)  # 3a
write.taf(wdiscards)  # 3b
write.taf(wcatch)     # 3c
write.taf(wstock)
write.taf(catch_sop)  # 4
write.taf(landfrac)
write.taf(maturity)   # 5a
write.taf(natmort)    # 5b
write.taf(ibts1)      # 6a
write.taf(ibts3)      # 6b
write.taf(surveytime)
write.taf(propf)
write.taf(propm)
## full datasets, including all ages
write.taf(latage_full)
write.taf(datage_full)
write.taf(catage_full)
write.taf(wlandings_full)
write.taf(wdiscards_full)
write.taf(wcatch_full)
write.taf(wstock_full)
write.taf(maturity_full)
setwd("..")

## Prepare model input file
taf2sam <- function(x)
{
  y <- as.matrix(taf2xtab(x))
  colnames(y) <- sub("\\+", "", colnames(y))
  y
}
surveys <- list(ibts1=taf2sam(ibts1), ibts3=taf2sam(ibts3))
attr(surveys$ibts1, "time") <- surveytime$ibts1
attr(surveys$ibts3, "time") <- surveytime$ibts3
input <- write.records(surveys=surveys,
                       residual.fleet=taf2sam(catage),
                       prop.mature=taf2sam(maturity),
                       stock.mean.weight=taf2sam(wstock),
                       catch.mean.weight=taf2sam(wcatch),
                       dis.mean.weight=taf2sam(wdiscards),
                       land.mean.weight=taf2sam(wlandings),
                       prop.f=taf2sam(propf),
                       prop.m=taf2sam(propm),
                       natural.mortality=taf2sam(natmort),
                       land.frac=taf2sam(landfrac),
                       file="data/sam.dat")
